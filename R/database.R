# =============================================================================
# database.R -- base DuckDB derivee + validation
#
# Base de donnees DERIVEE (DuckDB) des landmarks FISHMORPH.
#
# POSITION DANS LA CHAINE
#
#   journaux TSV  -->  base DuckDB  -->  Parquet / CSV
#   (append-only,      (contraintes,      (artefacts d'archivage,
#    SOURCE DE          types, vues,       citables, lisibles sans
#    VERITE)            requetes SQL)      aucun logiciel specifique)
#
# La base est DERIVEE et JETABLE : fishmorph_build_db() la reconstruit
# integralement depuis les journaux en quelques secondes. C'est ce qui rend
# acceptable de poser un moteur embarque dans un dossier synchronise -- une base
# corrompue par OneDrive n'est plus un incident de donnees, seulement une
# reconstruction. Les journaux, eux, ne sont jamais reecrits.
#
# COROLLAIRE : ne JAMAIS ecrire directement dans la base. Toute saisie passe par
# l'app (donc par le journal), sans quoi la prochaine reconstruction l'effacera.
#
# CE QUE LA BASE APPORTE, QUE LE TSV NE PEUT PAS
#   * types (une coordonnee est un DOUBLE, pas la chaine "500,5") ;
#   * contraintes : landmark dans 1..25, un seul point par (specimen, landmark),
#     statut dans un vocabulaire ferme, echelle strictement positive ;
#   * modele relationnel : session d'enregistrement / specimen / observation ;
#   * requetes ad hoc en SQL ou via dbplyr, sans tout charger en memoire.
#
# CE QU'ELLE N'APPORTE PAS, ET QU'IL FAUT CODER : la plausibilite MORPHOMETRIQUE.
# Un jeu de coordonnees peut satisfaire toutes les contraintes SQL et decrire un
# poisson impossible. fishmorph_validate() confronte donc chaque specimen a
# l'enveloppe empirique des 9556 especes du referentiel (voir .FM_RATIO_BOUNDS).
#
# ARCHIVAGE : un fichier .duckdb n'est pas un format de depot. Le format sur
# disque de DuckDB n'est garanti retrocompatible que depuis la version 1.0, et un
# depot type Zenodo attend du texte brut ou du Parquet. On exporte donc
# systematiquement, et ce sont ces exports qui sont citables.
#
# DEPENDANCES : duckdb, DBI. fishmorph_landmark_store.R doit etre charge.
# =============================================================================


# --- enveloppe empirique des proportions FISHMORPH ---------------------------
# Quantiles 0.1 % et 99.9 % du rapport segment/Bl, calcules sur FishMORPH_seg.csv
# (n = 6492 a 7706 especes selon le segment, valeurs strictement positives). Ce
# sont des bornes de PLAUSIBILITE, pas de validite : un ratio hors enveloppe
# signale un specimen a REGARDER, pas un specimen a rejeter -- une espece
# reellement atypique (anguilliforme, poisson-lune) peut legitimement en sortir.
# D'ou la severite "avertissement" et non "erreur".
.FM_RATIO_BOUNDS <- data.frame(
  segment = c("Bd", "Hd", "Eh2", "Mo2", "PFi2", "PFl", "Ed", "Jl", "CPd", "CFd"),
  a       = c( 3L,   5L,   7L,    1L,    10L,    10L,   13L,  1L,   16L,   18L),
  b       = c( 4L,   6L,   8L,    9L,    11L,    12L,   14L,  15L,  17L,   19L),
  lo      = c(0.0387, 0.0210, 0.0208, 0.0108, 0.0121, 0.0239, 0.0043, 0.0072,
              0.0062, 0.0199),
  med     = c(0.2480, 0.1382, 0.1372, 0.1152, 0.0745, 0.1829, 0.0589, 0.0559,
              0.1055, 0.2593),
  hi      = c(0.7487, 0.4197, 0.4296, 0.4238, 0.3498, 0.4168, 0.1386, 0.2263,
              0.2162, 0.5342),
  stringsAsFactors = FALSE
)

.FM_DDL <- c(
# `mode` et `ts` sont NULLABLES a dessein : un journal ecrit par une version
# anterieure peut ne pas les porter. Mieux vaut enregistrer "provenance inconnue"
# que refuser la donnee ou, pire, lui inventer une valeur plausible. Le CHECK
# reste en place pour interdire toute valeur HORS vocabulaire.
"CREATE TABLE record (
   record_id    VARCHAR PRIMARY KEY,
   ts           TIMESTAMP,
   operator     VARCHAR NOT NULL,
   app_version  VARCHAR,
   mode         VARCHAR CHECK (mode IS NULL OR
                               mode IN ('reconstruct','correct','new')),
   target_sheet VARCHAR
 )",
"CREATE TABLE specimen (
   specimen_id  VARCHAR PRIMARY KEY,
   species      VARCHAR NOT NULL,
   photo_file   VARCHAR,
   img_w        INTEGER CHECK (img_w  IS NULL OR img_w  > 0),
   img_h        INTEGER CHECK (img_h  IS NULL OR img_h  > 0),
   ruler_mm     DOUBLE  CHECK (ruler_mm  IS NULL OR ruler_mm  > 0),
   mm_per_px    DOUBLE  CHECK (mm_per_px IS NULL OR mm_per_px > 0),
   record_id    VARCHAR NOT NULL
 )",
# Le PRIMARY KEY composite est la contrainte qui compte : il rend structurellement
# impossible d'avoir deux fois le meme point pour un specimen, ce qu'aucun format
# tabulaire large ne peut garantir.
"CREATE TABLE landmark_obs (
   specimen_id  VARCHAR  NOT NULL,
   landmark     SMALLINT NOT NULL CHECK (landmark BETWEEN 1 AND 25),
   x            DOUBLE,
   y            DOUBLE,
   status       VARCHAR  NOT NULL
                CHECK (status IN ('placed','seeded','derived','na')),
   PRIMARY KEY (specimen_id, landmark)
 )"
)

.fm_sql_str <- function(x) paste0("'", gsub("'", "''", x), "'")

# --- construction ------------------------------------------------------------

#' Reconstruit la base DuckDB a partir des journaux
#'
#' Idempotent : deux appels successifs donnent la meme base. La base precedente
#' est ecrasee (c'est un artefact derive), jamais mise a jour en place.
#'
#' @param journal_dir Dossier des journaux (ou data.frame long deja lu).
#' @param db_path Chemin du fichier .duckdb. NULL -> pas de base sur disque, tout
#'   se fait en memoire (utile pour valider sans rien ecrire).
#' @param export_dir Dossier d'export Parquet + CSV. NULL -> pas d'export.
#' @param validate TRUE -> lance fishmorph_validate() et joint le rapport.
#' @param stop_on_error TRUE -> interrompt si des anomalies de severite "erreur"
#'   sont detectees, AVANT d'ecrire quoi que ce soit.
#' @return Liste invisible : `db_path`, `n_specimens`, `n_points`, `issues`.
#' @export
fishmorph_build_db <- function(journal_dir,
                               db_path    = NULL,
                               export_dir = NULL,
                               validate   = TRUE,
                               stop_on_error = FALSE) {
  for (p in c("DBI", "duckdb")) if (!requireNamespace(p, quietly = TRUE))
    stop("Le package '", p, "' est requis (install.packages(\"", p, "\")).",
         call. = FALSE)
  if (!exists("fishmorph_consolidate", mode = "function"))
    stop("fishmorph_landmark_store.R n'est pas charge.", call. = FALSE)

  K <- suppressWarnings(fishmorph_consolidate(journal_dir, long = TRUE,
                                              drop_na_points = FALSE))
  if (!nrow(K)) {
    if (!is.data.frame(journal_dir)) fm_journal_status(journal_dir)
    stop("Aucun enregistrement exploitable : rien a mettre en base.\n",
         "  Le journal est cree au LANCEMENT de l'app, mais ne se remplit qu'au ",
         "premier 'Enregistrer & suivant'.\n",
         "  Digitalise au moins un specimen, puis relance fishmorph_build_db().",
         call. = FALSE)
  }

  issues <- if (isTRUE(validate)) fishmorph_validate(K) else NULL
  n_err <- if (is.null(issues)) 0L else sum(issues$severite == "erreur")
  if (n_err > 0L) {
    msg <- sprintf("%d anomalie(s) de severite 'erreur' detectee(s).", n_err)
    if (isTRUE(stop_on_error))
      stop(msg, " Base non construite. Inspecte le rapport : ",
           "fishmorph_validate(journal_dir).", call. = FALSE)
    warning(msg, " La base est construite quand meme ; consulte $issues.",
            call. = FALSE)
  }

  num <- function(v) suppressWarnings(as.numeric(v))
  int <- function(v) suppressWarnings(as.integer(num(v)))
  # horodatage : le journal ecrit de l'ISO 8601 UTC suffixe 'Z', que strptime ne
  # sait pas lire tel quel -> on retire le Z et on force le fuseau.
  ts  <- as.POSIXct(strptime(sub("Z$", "", K$timestamp), "%Y-%m-%dT%H:%M:%OS",
                             tz = "UTC"), tz = "UTC")

  first_by <- function(df, key) df[!duplicated(df[[key]]), , drop = FALSE]
  rec <- first_by(data.frame(
    record_id = K$record_id, ts = ts, operator = K$operator,
    app_version = K$app_version, mode = K$mode, target_sheet = K$target_sheet,
    stringsAsFactors = FALSE), "record_id")
  spe <- first_by(data.frame(
    specimen_id = K$row_key, species = K$species, photo_file = K$photo_file,
    img_w = int(K$img_w), img_h = int(K$img_h),
    ruler_mm = num(K$ruler_mm), mm_per_px = num(K$mm_per_px),
    record_id = K$record_id, stringsAsFactors = FALSE), "specimen_id")
  spe$species[is.na(spe$species) | !nzchar(spe$species)] <- "(inconnu)"
  obs <- data.frame(
    specimen_id = K$row_key, landmark = int(K$landmark),
    x = num(K$x), y = num(K$y), status = K$status, stringsAsFactors = FALSE)
  obs <- obs[!is.na(obs$landmark), , drop = FALSE]
  obs <- obs[!duplicated(obs[, c("specimen_id", "landmark")]), , drop = FALSE]

  # Mise en conformite AVANT insertion : une contrainte violee ferait echouer tout
  # le lot avec un message peu parlant. On corrige donc explicitement, en le
  # signalant -- et sans jamais inventer de valeur : ce qui est inconnu devient
  # NULL, ce qui est hors vocabulaire est ramene au seul statut defendable.
  bad_mode <- !is.na(rec$mode) & !rec$mode %in% c("reconstruct", "correct", "new")
  if (any(bad_mode)) {
    warning(sum(bad_mode), " enregistrement(s) au mode inconnu -> NULL.", call. = FALSE)
    rec$mode[bad_mode] <- NA_character_
  }
  rec$mode[!nzchar(rec$mode %||% "") & !is.na(rec$mode)] <- NA_character_
  if (any(is.na(rec$ts)))
    warning(sum(is.na(rec$ts)), " enregistrement(s) sans horodatage lisible.",
            call. = FALSE)
  no_op <- is.na(rec$operator) | !nzchar(rec$operator)
  if (any(no_op)) rec$operator[no_op] <- "(inconnu)"

  bad_st <- !obs$status %in% .FM_JOURNAL_STATUS
  if (any(bad_st)) {
    warning(sum(bad_st), " observation(s) au statut inconnu -> 'na'.", call. = FALSE)
    obs$status[bad_st] <- "na"
  }

  # base ecrite dans un fichier TEMPORAIRE puis basculee : si la construction
  # echoue a mi-parcours, la base precedente reste intacte.
  final <- db_path
  if (!is.null(db_path)) {
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
    db_path <- file.path(dirname(db_path),
                         sprintf(".%s.building%d", basename(db_path), Sys.getpid()))
    if (file.exists(db_path)) unlink(db_path)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path %||% ":memory:",
                        read_only = FALSE)
  ok <- FALSE
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    if (!ok && !is.null(db_path) && file.exists(db_path)) unlink(db_path)
  }, add = TRUE)

  for (ddl in .FM_DDL) DBI::dbExecute(con, ddl)
  DBI::dbAppendTable(con, "record", rec)
  DBI::dbAppendTable(con, "specimen", spe)
  DBI::dbAppendTable(con, "landmark_obs", obs)
  if (!is.null(issues) && nrow(issues)) DBI::dbWriteTable(con, "qc_issue", issues)

  # DuckDB n'applique pas les cles etrangeres avec la rigueur de PostgreSQL : on
  # verifie donc l'integrite referentielle EXPLICITEMENT, plutot que de supposer
  # qu'une contrainte declaree suffit.
  orph <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM landmark_obs o
      WHERE NOT EXISTS (SELECT 1 FROM specimen s WHERE s.specimen_id = o.specimen_id)")$n
  if (orph > 0) warning(orph, " observation(s) sans specimen correspondant.",
                        call. = FALSE)

  .fm_create_views(con, sort(unique(obs$landmark)))
  if (!is.null(export_dir)) .fm_export(con, export_dir)

  n_sp <- nrow(spe); n_pt <- nrow(obs)
  ok <- TRUE
  DBI::dbDisconnect(con, shutdown = TRUE)
  on.exit(NULL)

  if (!is.null(final)) {
    prev <- paste0(final, ".prev")
    if (file.exists(final)) { if (file.exists(prev)) unlink(prev)
                              file.rename(final, prev) }
    if (!file.rename(db_path, final)) {
      if (file.exists(prev)) file.rename(prev, final)
      stop("Bascule de la base echouee : ", final, call. = FALSE)
    }
    message("Base construite : ", final, " (", n_sp, " specimens, ", n_pt, " points)")
  }
  invisible(list(db_path = final, n_specimens = n_sp, n_points = n_pt,
                 issues = issues))
}

# vues : tableau large + ratios morphometriques. Elles sont RECALCULEES a chaque
# requete, donc jamais desynchronisees des observations -- contrairement a une
# colonne "Bd" figee dans un classeur.
.fm_create_views <- function(con, pts) {
  sel <- paste(vapply(pts, function(p) sprintf(
    '  MAX(CASE WHEN o.landmark = %d THEN o.x END) AS "%d_X",
  MAX(CASE WHEN o.landmark = %d THEN o.y END) AS "%d_Y"', p, p, p, p),
    character(1)), collapse = ",\n")
  DBI::dbExecute(con, sprintf(
"CREATE OR REPLACE VIEW v_landmarks_wide AS
 SELECT s.specimen_id, s.species, s.photo_file, s.mm_per_px, s.record_id,
%s
 FROM specimen s JOIN landmark_obs o USING (specimen_id)
 GROUP BY s.specimen_id, s.species, s.photo_file, s.mm_per_px, s.record_id", sel))

  d <- function(a, b) sprintf('sqrt(pow("%d_X"-"%d_X",2)+pow("%d_Y"-"%d_Y",2))',
                              b, a, b, a)
  B <- .FM_RATIO_BOUNDS
  rat <- paste(sprintf("  %s / Bl_px AS %s", d(B$a, B$b), B$segment), collapse = ",\n")
  DBI::dbExecute(con, sprintf(
"CREATE OR REPLACE VIEW v_ratios AS
 SELECT specimen_id, species, photo_file, mm_per_px, Bl_px,
        Bl_px * mm_per_px AS Bl_mm,
%s
 FROM (SELECT *, %s AS Bl_px FROM v_landmarks_wide)
 WHERE Bl_px > 0", rat, d(1L, 2L)))

  # etat de saisie : combien de points restent a leur position de graine, donc
  # jamais verifies a l'oeil. Invisible dans un tableau de coordonnees.
  DBI::dbExecute(con,
"CREATE OR REPLACE VIEW v_specimen_qc AS
 SELECT s.specimen_id, s.species, s.photo_file,
        r.ts, r.operator, r.mode,
        COUNT(*) FILTER (WHERE o.status = 'placed')  AS n_placed,
        COUNT(*) FILTER (WHERE o.status = 'seeded')  AS n_seeded,
        COUNT(*) FILTER (WHERE o.status = 'derived') AS n_derived,
        COUNT(*) FILTER (WHERE o.status = 'na')      AS n_na,
        s.mm_per_px IS NOT NULL AS a_echelle
 FROM specimen s
 JOIN record r USING (record_id)
 JOIN landmark_obs o USING (specimen_id)
 GROUP BY s.specimen_id, s.species, s.photo_file, r.ts, r.operator, r.mode,
          s.mm_per_px")
  invisible(TRUE)
}

.fm_export <- function(con, export_dir) {
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  cp <- function(what, file, fmt)
    DBI::dbExecute(con, sprintf("COPY (SELECT * FROM %s) TO %s (FORMAT %s)",
                                what, .fm_sql_str(file.path(export_dir, file)), fmt))
  # Parquet : typé, compresse, colonnaire -> l'artefact de depot.
  cp("landmark_obs",     "landmark_obs.parquet",  "parquet")
  cp("specimen",         "specimen.parquet",      "parquet")
  cp("v_landmarks_wide", "landmarks_wide.parquet", "parquet")
  cp("v_ratios",         "ratios.parquet",        "parquet")
  # CSV : redondant avec le Parquet, mais lisible dans trente ans sans logiciel.
  cp("v_landmarks_wide", "landmarks_wide.csv", "csv, HEADER")
  cp("v_ratios",         "ratios.csv",         "csv, HEADER")
  message("Exports ecrits dans : ", export_dir)
  invisible(TRUE)
}

#' Ouvre la base (en lecture seule par defaut)
#'
#' La lecture seule est le mode normal : la base est derivee, on ne doit jamais y
#' ecrire a la main. Elle permet aussi a plusieurs processus R d'ouvrir le meme
#' fichier simultanement.
#' @export
fishmorph_db_connect <- function(db_path, read_only = TRUE) {
  for (p in c("DBI", "duckdb")) if (!requireNamespace(p, quietly = TRUE))
    stop("Le package '", p, "' est requis.", call. = FALSE)
  if (!file.exists(db_path)) stop("Base introuvable : ", db_path, call. = FALSE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

# --- validation --------------------------------------------------------------

#' Controle structurel ET morphometrique
#'
#' Les contraintes SQL garantissent la coherence du CONTENANT ; cette fonction
#' interroge la plausibilite du CONTENU. Un jeu de coordonnees peut satisfaire
#' toutes les contraintes et decrire un poisson impossible.
#'
#' Severites : "erreur" = incoherence certaine (point hors de l'image, points
#' confondus, axe degenere) ; "avertissement" = a regarder (proportion hors de
#' l'enveloppe des 9556 especes) ; "info" = tracabilite (point non verifie,
#' declare non mesurable, absence d'echelle).
#'
#' @param x Dossier de journaux, ou data.frame long (sortie de
#'   `fishmorph_consolidate(long = TRUE)`).
#' @param expect Points attendus pour un specimen complet.
#' @param bounds Enveloppe des ratios (defaut : [.FM_RATIO_BOUNDS]).
#' @return data.frame : specimen_id, species, photo_file, severite, probleme,
#'   landmark, detail.
#' @export
fishmorph_validate <- function(x, expect = c(1:19, 22L, 23L),
                               bounds = .FM_RATIO_BOUNDS) {
  K <- if (is.data.frame(x)) x else
    fishmorph_consolidate(x, long = TRUE, drop_na_points = FALSE)
  out <- list()
  add <- function(sp, sev, pb, lm = NA_integer_, detail = "") {
    if (!length(sp) || !nrow(sp)) return(invisible())
    out[[length(out) + 1L]] <<- data.frame(
      specimen_id = sp$row_key, species = sp$species, photo_file = sp$photo_file,
      severite = sev, probleme = pb, landmark = lm, detail = detail,
      stringsAsFactors = FALSE)
  }
  if (!nrow(K)) return(do.call(rbind, out) %||% .fm_issue_empty())

  K$landmark <- suppressWarnings(as.integer(K$landmark))
  K$x <- suppressWarnings(as.numeric(K$x))
  K$y <- suppressWarnings(as.numeric(K$y))
  K$img_w <- suppressWarnings(as.numeric(K$img_w))
  K$img_h <- suppressWarnings(as.numeric(K$img_h))

  # 1. coordonnee hors des bornes de l'image -> clic egare ou photo remplacee.
  #    Tolerance de 1 % : un point peut legitimement froler le bord.
  tol <- 0.01
  hors <- K[is.finite(K$x) & is.finite(K$y) & is.finite(K$img_w) & is.finite(K$img_h) &
            (K$x < -tol * K$img_w | K$x > (1 + tol) * K$img_w |
             K$y < -tol * K$img_h | K$y > (1 + tol) * K$img_h), , drop = FALSE]
  if (nrow(hors)) add(hors, "erreur", "point hors de l'image", hors$landmark,
                      sprintf("(%.0f, %.0f) pour une image %.0fx%.0f",
                              hors$x, hors$y, hors$img_w, hors$img_h))

  by_sp <- split(K, K$row_key)
  for (g in by_sp) {
    m <- g[1, , drop = FALSE]
    fin <- g[is.finite(g$x) & is.finite(g$y) & !(g$status %in% "na"), , drop = FALSE]

    # 2. deux landmarks distincts exactement au meme pixel = clic manque
    if (nrow(fin) > 1) {
      k <- paste(round(fin$x, 1), round(fin$y, 1))
      dup <- unique(k[duplicated(k)])
      for (kk in dup) {
        lm <- sort(fin$landmark[k == kk])
        add(m, "erreur", "points confondus", lm[1],
            paste("points", paste(lm, collapse = "+"), "au meme pixel"))
      }
    }
    # 3. points attendus absents
    miss <- setdiff(expect, g$landmark)
    if (length(miss)) add(m, "avertissement", "point absent", miss[1],
                          paste("manquants :", paste(miss, collapse = ",")))
    # 4. tracabilite de la saisie
    sd_ <- g$landmark[g$status %in% "seeded" & g$landmark %in% expect]
    if (length(sd_)) add(m, "info", "point jamais verifie", sd_[1],
                         paste("restes a la graine :", paste(sort(sd_), collapse = ",")))
    aj_ <- g$landmark[g$status %in% "adjusted" & g$landmark %in% expect]
    if (length(aj_)) add(m, "info", "point recale par convention", aj_[1],
                         paste("extremes 3/4 corriges :", paste(sort(aj_), collapse = ",")))
    na_ <- g$landmark[g$status %in% "na" & g$landmark %in% expect]
    if (length(na_)) add(m, "info", "point non mesurable", na_[1],
                         paste("declares NA :", paste(sort(na_), collapse = ",")))
    if (!any(is.finite(suppressWarnings(as.numeric(m$mm_per_px)))))
      add(m, "info", "pas de barre d'echelle", NA_integer_,
          "coordonnees en pixels uniquement")

    # 5. plausibilite morphometrique, referee a l'enveloppe FISHMORPH
    P <- matrix(NA_real_, 25, 2)
    ok <- g$landmark >= 1 & g$landmark <= 25 & !is.na(g$landmark)
    P[g$landmark[ok], 1] <- g$x[ok]; P[g$landmark[ok], 2] <- g$y[ok]
    dd <- function(a, b) if (all(is.finite(P[c(a, b), ])))
      sqrt(sum((P[b, ] - P[a, ])^2)) else NA_real_
    Bl <- dd(1L, 2L)
    if (!is.finite(Bl) || Bl <= 0) {
      add(m, "erreur", "axe du corps degenere", NA_integer_,
          "LM1 et LM2 confondus ou absents : aucune echelle relative possible")
      next
    }
    for (i in seq_len(nrow(bounds))) {
      r <- dd(bounds$a[i], bounds$b[i]) / Bl
      if (!is.finite(r)) next
      if (r < bounds$lo[i] || r > bounds$hi[i])
        add(m, "avertissement", "proportion hors enveloppe", bounds$a[i],
            sprintf("%s/Bl = %.3f hors [%.3f ; %.3f] (mediane %.3f)",
                    bounds$segment[i], r, bounds$lo[i], bounds$hi[i], bounds$med[i]))
    }
  }
  res <- if (length(out)) do.call(rbind, out) else .fm_issue_empty()
  sev <- factor(res$severite, levels = c("erreur", "avertissement", "info"))
  res <- res[order(sev, res$specimen_id), , drop = FALSE]
  rownames(res) <- NULL
  res
}

.fm_issue_empty <- function() data.frame(
  specimen_id = character(0), species = character(0), photo_file = character(0),
  severite = character(0), probleme = character(0), landmark = integer(0),
  detail = character(0), stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# Utilisation
# -----------------------------------------------------------------------------
# library(Rfishmorph)
#
# jdir <- "FishMORPH/landmark_journal"
#
# # (1) Reconstruire la base + les exports d'archivage. A relancer aussi souvent
# #     qu'on veut : c'est un artefact derive, jamais une mise a jour en place.
# res <- fishmorph_build_db(jdir,
#          db_path    = "FishMORPH/fishmorph.duckdb",
#          export_dir = "FishMORPH/exports")
# subset(res$issues, severite == "erreur")
#
# # (2) Valider SANS rien ecrire (avant de decider) :
# iss <- fishmorph_validate(jdir)
# table(iss$severite, iss$probleme)
#
# # (3) Interroger. Exemple : specimens dont l'oeil sort de l'enveloppe, ou dont
# #     plus de 3 points n'ont jamais ete verifies.
# con <- fishmorph_db_connect("FishMORPH/fishmorph.duckdb")
# DBI::dbGetQuery(con, "
#   SELECT r.specimen_id, r.species, r.Ed, q.n_seeded
#   FROM v_ratios r JOIN v_specimen_qc q USING (specimen_id)
#   WHERE r.Ed > 0.1386 OR q.n_seeded > 3
#   ORDER BY q.n_seeded DESC")
#
# # ... ou en dplyr, sans SQL :
# # dplyr::tbl(con, "v_ratios") |> dplyr::filter(Bd > 0.5) |> dplyr::collect()
# DBI::dbDisconnect(con, shutdown = TRUE)
#
# # (4) Repartir des exports sans aucun moteur (Parquet ou CSV) :
# # arrow::read_parquet("FishMORPH/exports/landmarks_wide.parquet")
# # read.csv("FishMORPH/exports/landmarks_wide.csv")
# -----------------------------------------------------------------------------
