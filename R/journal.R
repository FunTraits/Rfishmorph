# =============================================================================
# journal.R -- couche de capture (journal append-only)
#
# Couche de STOCKAGE des landmarks FISHMORPH : journal append-only + consolidation.
#
# PROBLEME RESOLU
#   L'app de digitalisation ecrivait chaque specimen en REECRIVANT integralement
#   un classeur .xlsx de plusieurs Mo, dans un dossier synchronise (OneDrive).
#   Un crash de R, une coupure ou un verrou de synchronisation pendant cette
#   reecriture peut donc detruire TOUT le fichier -- pas seulement le dernier
#   specimen. Le volume n'est pas en cause (quelques milliers de specimens x ~25
#   points = quelques Mo) : c'est le MOTIF D'ECRITURE qui est fragile.
#
# PRINCIPE
#   Ne jamais reecrire ce qui est deja ecrit. La capture devient un flux de
#   fichiers immuables ; la base analysable est RECONSTRUITE a la demande.
#
#     photos --[app]--> journal append-only --[consolidation]--> csv / xlsx
#                       (jamais modifie)                          (export)
#
#   * Un journal par SESSION : "landmarks_<operateur>_<horodatage>.tsv".
#     Une session terminee = fichier fige -> OneDrive ne peut plus creer de copie
#     en conflit, et deux postes produisent deux fichiers qui fusionnent par
#     simple concatenation.
#   * Format LONG (une ligne = UN point) : ajouter un landmark demain n'est plus
#     une migration de schema, seulement des lignes en plus.
#   * Un crash n'abime au pire que la derniere ligne du journal courant, qui est
#     detectee et jetee a la lecture.
#   * La deduplication garde le dernier enregistrement par cle, ce qui donne
#     gratuitement l'HISTORIQUE des corrections (fm_journal_history()).
#
# DEPENDANCES : aucune pour le journal (base R). openxlsx uniquement pour les
#   exports .xlsx optionnels.
#
# Voir R/digitizer-app.R (launch_fishmorph_digitizer) pour l'ecriture ; exemples
# de relecture et de consolidation en fin de fichier.
#
# COUCHE SUPERIEURE : R/database.R construit, A PARTIR de ces journaux, une base
# DuckDB derivee (types, contraintes, vues, SQL) et les exports Parquet / CSV
# d'archivage. Cette base est jetable et reconstructible ; les journaux restent
# la seule source de verite.
# =============================================================================

# Colonnes du journal, dans l'ordre. Toute colonne ajoutee plus tard doit l'etre
# EN FIN de liste : fm_journal_read() tolere des journaux de largeurs differentes
# (anciens fichiers) en completant les colonnes absentes par NA.
.FM_JOURNAL_COLS <- c(
  "record_id",     # identifiant de l'ENREGISTREMENT (un clic sur "Enregistrer")
  "timestamp",     # ISO 8601 UTC, tri lexicographique = tri chronologique
  "operator",      # qui a digitalise
  "app_version",   # version de l'outil de saisie
  "mode",          # reconstruct | correct | new
  "target_sheet",  # feuille du classeur visee (tracabilite)
  "row_key",       # CLE de deduplication (espece, ou fichier photo en mode "new")
  "species",       # Genus species
  "photo_file",    # nom du fichier photo (basename)
  "img_w", "img_h",# dimensions de l'image en pixels : les X/Y sont en pixels IMAGE
  "ruler_mm",      # longueur reelle de la barre d'echelle 20-21 (mm), ou NA
  "mm_per_px",     # echelle deduite, ou NA
  "landmark",      # numero du point
  "x", "y",        # coordonnees en pixels image (Y vers le bas)
  "status"         # placed | seeded | derived | na  (voir ci-dessous)
)

# Signification de `status` -- c'est l'information que le format large du classeur
# ne peut pas porter, et elle est precieuse en controle qualite :
#   placed  : point pose ou deplace a la main (ou recharge d'une saisie anterieure)
#   seeded  : point encore a sa position de GRAINE, jamais verifie par l'operateur
#   derived : point calcule automatiquement (8, 9, 11, 15, 23)
#   na      : point explicitement marque NON MESURABLE
.FM_JOURNAL_STATUS <- c("placed", "seeded", "derived", "na")


# --- utilitaires -------------------------------------------------------------

# horodatage ISO 8601 en UTC, a la milliseconde. En UTC et avec ce format, l'ordre
# lexicographique des chaines EST l'ordre chronologique : la deduplication peut
# donc trier sans jamais reparser de date (et sans dependre du fuseau du poste).
.fm_iso_now <- function() format(as.POSIXct(Sys.time(), tz = "UTC"),
                                 "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

# neutralise ce qui casserait un TSV (tabulation, retour a la ligne, guillemets).
# Renvoie TOUJOURS au moins un element : une metadonnee absente (NULL, ou input
# Shiny pas encore initialise) donnerait sinon un vecteur de longueur 0, et
# data.frame() echouerait sur des longueurs incompatibles -- ce qui ferait perdre
# l'enregistrement au lieu de le degrader.
.fm_tsv_safe <- function(x) {
  x <- as.character(x)
  if (!length(x)) return("")
  x[is.na(x)] <- ""
  gsub("[\t\r\n\"]+", " ", x)
}

#' Ecriture ATOMIQUE d'un classeur openxlsx
#'
#' `saveWorkbook()` ecrase le fichier cible en place : pendant la reecriture (des
#' secondes pour un classeur de plusieurs Mo) le fichier est dans un etat
#' intermediaire, et une interruption le detruit. On ecrit donc dans un fichier
#' temporaire du MEME dossier -- condition necessaire pour que le renommage soit
#' atomique, un renommage inter-volumes etant en realite une copie -- puis on
#' bascule par renommage.
#'
#' L'ancien fichier n'est pas supprime mais deplace en "<nom>.prev.xlsx", ce qui
#' fournit une sauvegarde d'une generation pour un cout nul. En cas d'echec du
#' renommage final, l'ancien fichier est restaure.
#'
#' @param wb Objet openxlsx.
#' @param path Chemin cible.
#' @param keep_prev Conserver la generation precedente (defaut TRUE).
#' @return TRUE (invisible) si l'ecriture a abouti.
#' @export
fm_save_workbook_atomic <- function(wb, path, keep_prev = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Le package 'openxlsx' est requis.", call. = FALSE)
  tmp <- file.path(dirname(path),
                   sprintf(".%s.tmp%d", basename(path), Sys.getpid()))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  if (!file.exists(tmp)) stop("Ecriture temporaire echouee : ", tmp, call. = FALSE)

  prev <- sub("(\\.xlsx)?$", ".prev.xlsx", path)
  had  <- file.exists(path)
  # On ecarte l'ancien fichier AVANT de renommer : sous Windows, file.rename()
  # echoue si la destination existe deja.
  if (had) {
    if (file.exists(prev)) unlink(prev)
    if (!file.rename(path, prev))
      stop("Impossible d'ecarter l'ancien fichier (verrouille par Excel ?) : ",
           path, call. = FALSE)
  }
  if (!file.rename(tmp, path)) {
    if (had) file.rename(prev, path)              # restauration
    stop("Renommage final echoue : ", path, call. = FALSE)
  }
  if (had && !keep_prev) unlink(prev)
  invisible(TRUE)
}

# --- journal : ecriture ------------------------------------------------------

#' Ouvre un journal de session (append-only)
#'
#' Cree `journal_dir` au besoin et un fichier TSV propre a la session. Le fichier
#' n'est ecrit qu'en AJOUT : il n'est jamais relu ni reecrit par l'app, et devient
#' immuable des la fin de la session.
#'
#' @param journal_dir Dossier des journaux.
#' @param operator Identifiant de l'operateur (defaut : utilisateur systeme).
#' @param app_version Version de l'outil de saisie, tracee dans chaque ligne.
#' @return Une "poignee" de journal a passer a [fm_journal_append()].
#' @export
fm_journal_open <- function(journal_dir, operator = NULL, app_version = NA_character_) {
  if (is.null(operator) || !nzchar(operator))
    operator <- tryCatch(unname(Sys.info()[["user"]]), error = function(e) "unknown")
  operator <- gsub("[^A-Za-z0-9._-]+", "_", operator)
  if (!dir.exists(journal_dir))
    dir.create(journal_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(journal_dir))
    stop("Impossible de creer le dossier du journal : ", journal_dir, call. = FALSE)

  stamp <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y%m%dT%H%M%SZ", tz = "UTC")
  sid   <- paste0(operator, "_", stamp)
  path  <- file.path(journal_dir, paste0("landmarks_", sid, ".tsv"))
  # collision improbable (deux lancements dans la meme seconde) -> suffixe
  k <- 1L
  while (file.exists(path)) {
    k <- k + 1L
    path <- file.path(journal_dir, sprintf("landmarks_%s-%d.tsv", sid, k))
  }
  cat(paste(.FM_JOURNAL_COLS, collapse = "\t"), "\n", sep = "", file = path)
  message("Journal de session : ", path)
  structure(list(path = path, dir = journal_dir, operator = operator,
                 session_id = sid, app_version = as.character(app_version),
                 n = local({ e <- new.env(parent = emptyenv()); e$i <- 0L; e })),
            class = "fm_journal")
}

#' Ajoute un enregistrement (un specimen) au journal
#'
#' Un "enregistrement" = un clic sur "Enregistrer", soit une ligne par landmark
#' partageant le meme `record_id`. L'ecriture est un simple `cat(append = TRUE)`
#' d'un bloc de texte deja construit : le fichier existant n'est jamais relu ni
#' reecrit, donc une interruption ne peut tronquer que la derniere ligne (qui sera
#' ecartee a la lecture).
#'
#' @param jr Poignee renvoyee par [fm_journal_open()].
#' @param row_key Cle de deduplication (espece, ou fichier photo en mode "new").
#' @param coords Matrice a 2 colonnes (X, Y) indexee par numero de landmark.
#' @param points Numeros de landmarks a enregistrer.
#' @param status Vecteur nomme (nom = numero de point) de statuts ; defaut "placed".
#' @param species,photo_file,mode,target_sheet,img_w,img_h,ruler_mm,mm_per_px Metadonnees.
#' @return Le `record_id` ecrit (invisible), ou NULL si rien a ecrire.
#' @export
fm_journal_append <- function(jr, row_key, coords, points,
                              status = NULL, species = NA, photo_file = NA,
                              mode = NA, target_sheet = NA,
                              img_w = NA, img_h = NA,
                              ruler_mm = NA, mm_per_px = NA) {
  if (!inherits(jr, "fm_journal")) stop("`jr` n'est pas un journal.", call. = FALSE)
  points <- points[points >= 1 & points <= nrow(coords)]
  if (!length(points)) return(invisible(NULL))

  jr$n$i <- jr$n$i + 1L
  rid <- sprintf("%s-%05d", jr$session_id, jr$n$i)
  ts  <- .fm_iso_now()

  st <- rep("placed", length(points))
  if (!is.null(status)) {
    hit <- match(as.character(points), names(status))
    st[!is.na(hit)] <- as.character(status)[hit[!is.na(hit)]]
  }
  # sans coordonnee utilisable, aucun autre statut n'a de sens : on note "na"
  # plutot que de laisser croire a un point pose ou calcule.
  fin <- is.finite(coords[points, 1]) & is.finite(coords[points, 2])
  st[!fin] <- "na"

  # formatC(format = "f") et NON format() : ce dernier applique getOption("digits")
  # (7 chiffres significatifs par defaut) et arrondirait une abscisse a 5 chiffres
  # sur une grande photo (12345.678 -> "12345.68"). Ici la precision est fixee en
  # nombre de DECIMALES, jamais en chiffres significatifs.
  num <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (!length(v)) return("")          # meme garde que .fm_tsv_safe()
    out <- rep("", length(v))
    ok <- is.finite(v)
    if (any(ok))
      out[ok] <- formatC(v[ok], format = "f", digits = 6, drop0trailing = TRUE)
    out
  }
  rows <- data.frame(
    record_id = rid, timestamp = ts, operator = jr$operator,
    app_version = jr$app_version %||% "", mode = .fm_tsv_safe(mode),
    target_sheet = .fm_tsv_safe(target_sheet), row_key = .fm_tsv_safe(row_key),
    species = .fm_tsv_safe(species), photo_file = .fm_tsv_safe(photo_file),
    img_w = num(img_w), img_h = num(img_h),
    ruler_mm = num(ruler_mm), mm_per_px = num(mm_per_px),
    landmark = as.character(points),
    x = num(round(coords[points, 1], 3)), y = num(round(coords[points, 2], 3)),
    status = st, stringsAsFactors = FALSE)
  rows <- rows[, .FM_JOURNAL_COLS, drop = FALSE]

  txt <- paste(do.call(paste, c(unname(as.list(rows)), sep = "\t")), collapse = "\n")
  cat(txt, "\n", sep = "", file = jr$path, append = TRUE)
  invisible(rid)
}

# --- journal : lecture -------------------------------------------------------

.fm_journal_empty <- function() {
  d <- as.data.frame(matrix(character(0), nrow = 0, ncol = length(.FM_JOURNAL_COLS)),
                     stringsAsFactors = FALSE)
  names(d) <- .FM_JOURNAL_COLS
  d
}

#' Lit et concatene tous les journaux d'un dossier
#'
#' Tolerant par construction : une derniere ligne tronquee par un crash est
#' ecartee (colonnes obligatoires manquantes), et un journal ecrit par une version
#' anterieure (moins de colonnes) est complete par des NA.
#'
#' @param journal_dir Dossier des journaux (ou vecteur de dossiers).
#' @return Un data.frame LONG, une ligne par point et par enregistrement.
#' @export
fm_journal_read <- function(journal_dir) {
  fs <- unlist(lapply(journal_dir, function(d)
    list.files(d, pattern = "^landmarks_.*\\.tsv$", full.names = TRUE)), use.names = FALSE)
  if (!length(fs)) return(.fm_journal_empty())
  parts <- lapply(fs, function(f) {
    d <- try(utils::read.delim(f, sep = "\t", header = TRUE, quote = "",
                               comment.char = "", colClasses = "character",
                               fill = TRUE, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(d, "try-error") || is.null(d) || !nrow(d)) return(NULL)
    for (cc in setdiff(.FM_JOURNAL_COLS, names(d))) d[[cc]] <- rep(NA_character_, nrow(d))
    d <- d[, .FM_JOURNAL_COLS, drop = FALSE]
    # ligne tronquee (crash en cours d'ecriture) : sans record_id/landmark/row_key
    # elle est inexploitable -> on la jette silencieusement.
    ok <- !is.na(d$record_id) & nzchar(d$record_id) &
          !is.na(d$landmark)  & nzchar(d$landmark) &
          !is.na(d$row_key)
    d[ok, , drop = FALSE]
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(.fm_journal_empty())
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' Etat des journaux d'un dossier
#'
#' A appeler en PREMIER quand une consolidation renvoie un resultat vide : dit
#' immediatement si le dossier est le bon, quels fichiers s'y trouvent, et
#' combien d'enregistrements chacun contient. Un journal a 0 enregistrement est
#' le cas normal d'une session ouverte puis fermee sans avoir rien enregistre :
#' l'app cree le fichier au LANCEMENT, pas au premier "Enregistrer".
#'
#' @param journal_dir Dossier des journaux.
#' @return data.frame : fichier, octets, n_lignes, n_records, n_points, periode.
#' @export
fm_journal_status <- function(journal_dir) {
  ex <- dir.exists(journal_dir)
  message("Dossier : ", normalizePath(journal_dir, mustWork = FALSE),
          if (ex) "" else "   [INTROUVABLE]")
  if (!ex) return(invisible(NULL))
  fs <- list.files(journal_dir, pattern = "^landmarks_.*\\.tsv$", full.names = TRUE)
  other <- setdiff(list.files(journal_dir), basename(fs))
  if (!length(fs)) {
    message("Aucun fichier 'landmarks_*.tsv'.",
            if (length(other))
              paste0(" Le dossier contient pourtant : ",
                     paste(utils::head(other, 5), collapse = ", "),
                     " -- mauvais dossier, ou fichiers renommes ?") else
              " Dossier vide : l'app n'a jamais ete lancee avec ce journal_dir.")
    return(invisible(NULL))
  }
  J <- fm_journal_read(journal_dir)
  out <- do.call(rbind, lapply(fs, function(f) {
    n_l <- length(readLines(f, warn = FALSE))
    data.frame(fichier = basename(f), octets = file.size(f),
               n_lignes = max(0L, n_l - 1L), stringsAsFactors = FALSE)
  }))
  out$n_records <- NA_integer_; out$n_points <- NA_integer_
  out$debut <- NA_character_;   out$fin <- NA_character_
  if (nrow(J)) {
    # rattache chaque enregistrement a sa session via le prefixe du record_id
    sess <- sub("-\\d+$", "", J$record_id)
    for (i in seq_len(nrow(out))) {
      sid <- sub("^landmarks_(.*)\\.tsv$", "\\1", out$fichier[i])
      k <- sess == sid
      out$n_records[i] <- length(unique(J$record_id[k]))
      out$n_points[i]  <- sum(k)
      if (any(k)) { out$debut[i] <- min(J$timestamp[k]); out$fin[i] <- max(J$timestamp[k]) }
    }
  }
  out$n_records[is.na(out$n_records)] <- 0L
  out$n_points[is.na(out$n_points)]   <- 0L
  tot <- sum(out$n_records)
  message(sprintf("%d fichier(s), %d enregistrement(s), %d point(s) au total.",
                  nrow(out), tot, sum(out$n_points)))
  if (tot == 0L)
    message("-> Aucun 'Enregistrer & suivant' n'a encore ete effectue dans une ",
            "session utilisant ce journal. Le fichier est cree au LANCEMENT de ",
            "l'app ; il ne se remplit qu'au premier enregistrement.")
  out
}

#' Historique des enregistrements d'une cle
#'
#' Utile pour verifier qu'une correction a bien ete prise, ou pour comparer deux
#' passages sur le meme specimen.
#'
#' @param journal_dir Dossier des journaux, ou data.frame deja lu.
#' @param row_key Cle a inspecter. NULL -> resume de toutes les cles.
#' @export
fm_journal_history <- function(journal_dir, row_key = NULL) {
  J <- if (is.data.frame(journal_dir)) journal_dir else fm_journal_read(journal_dir)
  if (!nrow(J)) return(J)
  if (!is.null(row_key)) J <- J[J$row_key %in% row_key, , drop = FALSE]
  if (!nrow(J)) return(J)
  agg <- do.call(rbind, lapply(split(J, J$record_id), function(g) data.frame(
    record_id = g$record_id[1], timestamp = g$timestamp[1], operator = g$operator[1],
    mode = g$mode[1], row_key = g$row_key[1], species = g$species[1],
    photo_file = g$photo_file[1], n_points = nrow(g),
    n_placed = sum(g$status == "placed"), n_seeded = sum(g$status == "seeded"),
    n_na = sum(g$status == "na"), stringsAsFactors = FALSE)))
  agg <- agg[order(agg$row_key, agg$timestamp, agg$record_id), ]
  rownames(agg) <- NULL
  agg
}

# --- consolidation -----------------------------------------------------------

#' Reconstruit la base analysable a partir des journaux
#'
#' Pour chaque cle (`row_key`), seul le DERNIER enregistrement est retenu --
#' horodatage maximal, `record_id` maximal en cas d'egalite. Les enregistrements
#' anterieurs restent dans les journaux : ils constituent l'historique des
#' corrections, consultable via [fm_journal_history()], et ne sont jamais perdus.
#'
#' @param journal_dir Dossier des journaux, ou data.frame deja lu.
#' @param long TRUE -> renvoie le format long retenu (une ligne par point) au lieu
#'   du tableau large.
#' @param drop_na_points TRUE (defaut) -> les points marques "na" sortent en NA.
#'   FALSE -> leurs coordonnees eventuelles sont conservees.
#' @param out_csv,out_xlsx Chemins d'export optionnels (l'xlsx exige openxlsx).
#' @return data.frame large : une ligne par cle, colonnes `<n>_X` / `<n>_Y`.
#' @export
fishmorph_consolidate <- function(journal_dir, long = FALSE, drop_na_points = TRUE,
                                  out_csv = NULL, out_xlsx = NULL) {
  J <- if (is.data.frame(journal_dir)) journal_dir else fm_journal_read(journal_dir)
  if (!nrow(J)) {
    if (!is.data.frame(journal_dir)) fm_journal_status(journal_dir)
    warning("Aucun enregistrement dans le journal (voir le diagnostic ci-dessus).",
            call. = FALSE)
    return(.fm_journal_empty())
  }
  # dernier enregistrement par cle. On travaille sur la table des ENREGISTREMENTS
  # (et non des points) pour ne jamais melanger deux passages sur un meme
  # specimen : on garde un record_id entier, donc un jeu de points coherent.
  R <- unique(J[, c("row_key", "record_id", "timestamp")])
  R <- R[order(R$row_key, R$timestamp, R$record_id), , drop = FALSE]
  keep <- R$record_id[!duplicated(R$row_key, fromLast = TRUE)]
  K <- J[J$record_id %in% keep, , drop = FALSE]

  K$x <- suppressWarnings(as.numeric(K$x))
  K$y <- suppressWarnings(as.numeric(K$y))
  if (isTRUE(drop_na_points)) {
    bad <- K$status %in% "na"
    K$x[bad] <- NA_real_; K$y[bad] <- NA_real_
  }
  K$landmark <- suppressWarnings(as.integer(K$landmark))
  K <- K[!is.na(K$landmark), , drop = FALSE]
  if (isTRUE(long)) { rownames(K) <- NULL; return(K) }

  meta_cols <- c("row_key", "species", "photo_file", "operator", "timestamp",
                 "record_id", "mode", "target_sheet", "img_w", "img_h",
                 "ruler_mm", "mm_per_px", "app_version")
  wide <- K[!duplicated(K$record_id), meta_cols, drop = FALSE]
  wide <- wide[order(wide$row_key), , drop = FALSE]
  rownames(wide) <- NULL

  pts <- sort(unique(K$landmark))
  for (p in pts) {
    s <- K[K$landmark == p, , drop = FALSE]
    i <- match(wide$record_id, s$record_id)
    wide[[paste0(p, "_X")]] <- s$x[i]
    wide[[paste0(p, "_Y")]] <- s$y[i]
    wide[[paste0(p, "_status")]] <- s$status[i]
  }
  # colonnes de statut regroupees en fin de tableau (elles genent la lecture des
  # coordonnees, mais on ne les jette pas : c'est l'info de controle qualite)
  st <- grep("_status$", names(wide), value = TRUE)
  wide <- wide[, c(setdiff(names(wide), st), st), drop = FALSE]

  if (!is.null(out_csv)) {
    utils::write.csv(wide, out_csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    message("Export CSV : ", out_csv, " (", nrow(wide), " lignes)")
  }
  if (!is.null(out_xlsx)) {
    if (!requireNamespace("openxlsx", quietly = TRUE))
      warning("openxlsx absent : export .xlsx ignore.", call. = FALSE)
    else {
      openxlsx::write.xlsx(wide, out_xlsx, overwrite = TRUE)
      message("Export XLSX : ", out_xlsx, " (", nrow(wide), " lignes)")
    }
  }
  wide
}

#' Controle qualite rapide d'une consolidation
#'
#' Signale ce qu'un tableau de coordonnees ne montre pas : points jamais verifies
#' (restes a la graine), points declares non mesurables, specimens incomplets.
#'
#' @param journal_dir Dossier des journaux, ou data.frame deja lu.
#' @param expect Points attendus pour un specimen complet.
#' @export
fishmorph_journal_qc <- function(journal_dir, expect = c(1:19, 22L, 23L)) {
  K <- fishmorph_consolidate(journal_dir, long = TRUE, drop_na_points = FALSE)
  if (!nrow(K)) return(K)
  qc <- do.call(rbind, lapply(split(K, K$row_key), function(g) data.frame(
    row_key = g$row_key[1], species = g$species[1], photo_file = g$photo_file[1],
    timestamp = g$timestamp[1],
    n_manquants = sum(!expect %in% g$landmark),
    manquants = paste(setdiff(expect, g$landmark), collapse = ","),
    n_seeded = sum(g$status == "seeded" & g$landmark %in% expect),
    seeded = paste(g$landmark[g$status == "seeded" & g$landmark %in% expect],
                   collapse = ","),
    n_na = sum(g$status == "na" & g$landmark %in% expect),
    a_echelle = isTRUE(is.finite(suppressWarnings(as.numeric(g$mm_per_px[1])))),
    stringsAsFactors = FALSE)))
  qc <- qc[order(-qc$n_manquants, -qc$n_seeded, qc$row_key), , drop = FALSE]
  rownames(qc) <- NULL
  qc
}

# -----------------------------------------------------------------------------
# Utilisation type
# -----------------------------------------------------------------------------
# library(Rfishmorph)
# jdir <- "FishMORPH/landmark_journal"
#
# # (1) Tableau analysable (une ligne par cle) :
# base <- fishmorph_consolidate(jdir, out_csv = "landmarks_consolides.csv")
#
# # (2) Controle qualite : points jamais verifies, manquants, absence d'echelle :
# subset(fishmorph_journal_qc(jdir), n_seeded > 0 | n_manquants > 0)
#
# # (3) Historique des passages sur un specimen :
# fm_journal_history(jdir, "Coilia.nasus")
#
# # (4) Format long, pour geomorph / intraitR :
# lg <- fishmorph_consolidate(jdir, long = TRUE)
#
# # (5) FUSIONNER DEUX POSTES : copier les .tsv des deux dossiers dans un seul.
# #     Les noms de fichiers incluent l'operateur et l'horodatage, donc aucune
# #     collision n'est possible ; il n'y a rien d'autre a faire que la copie.
# -----------------------------------------------------------------------------
