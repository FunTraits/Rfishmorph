# =============================================================================
# fishmorph_reconstruct_app.R
#
# Outil interactif : placer les segments FISHMORPH sur la photo d'une espece,
# les transformer en 21 landmarks (points 1-19 + 22 + 23) et les enregistrer
# dans la feuille "Global_Landmark" d'une COPIE du classeur.
# 23 = point derive (auto) : intersection de la droite (1,9) et de la droite
#      passant par 6 parallele a l'axe (1,2) -> distance museau -> base tete.
#
# Entree  : FISHMORPH_PUBLI_9556sp.xlsx  (feuille 1 "Global_segments",
#           feuille 2 "Global_Landmark") + dossier "Photos utilisees".
# Sortie  : copie "..._reconstructed.xlsx", feuille 2 completee ligne par ligne.
#
# Trois modes (bouton "Mode", ou arg `mode=`) :
#   * "reconstruct" : file des especes SANS landmarks (a digitaliser).
#   * "correct"     : file des especes DEJA landmarkees (relecture/correction) ;
#                     les 21 points sont recharges du classeur et repositionnables,
#                     puis "Enregistrer & suivant" reecrit la ligne.
#   * "new"         : file des PHOTOS NOUVELLES (dossier `new_photo_dir`), non
#                     presentes dans le classeur. Aucun segment n'existe : les
#                     points sont amorces par les PROPORTIONS MEDIANES du jeu
#                     FISHMORPH (voir .FM_NEW_RATIOS) apres les clics museau (1)
#                     et base caudale (2), puis places a la main. Le nom d'espece
#                     est saisi dans un champ (pre-rempli depuis le nom de
#                     fichier). LM20/21 (barre d'echelle, optionnels) s'ajoutent a
#                     l'ordre de saisie et donnent mm_per_px. L'enregistrement se
#                     fait dans la feuille `new_sheet` ("new_specimens"), en
#                     ajoutant une ligne (ou en la reecrivant si la photo y est
#                     deja, cle = colonne photo_file).
#
# Flux (mode reconstruct) :
#   1. L'app construit la file des especes SANS landmarks, AVEC segments et
#      AVEC une photo correspondante.
#   2. Pour l'espece courante : cliquer le MUSEAU (LM1) puis la BASE CAUDALE
#      (LM2) -> axe, position et echelle (px/unite) deduits de Bl.
#   3. Les 20 points sont pre-places depuis les segments (longueurs
#      verrouillees) ; on affine avec les curseurs (parametres non identifies)
#      et, au besoin, en repositionnant un point precis au clic.
#   4. "Enregistrer & suivant" verifie la convention des extremes (3 = point le
#      plus dorsal, 4 = le plus ventral : voir .fm_extreme_violations), puis
#      ecrit les X/Y dans la copie et passe a la suivante.
#
# Calibration des segments (verifiee sur les lignes deja digitalisees) :
#   paires euclidiennes = Bl(1,2) Bd(3,4) Hd(5,6) Ed(13,14) Jl(1,15)
#   PFl(10,12) CPd(16,17) CFd(18,19) ; et pour les hauteurs, on utilise les
#   colonnes *2* : Eh2->(7,8)  Mo2->(1,9)  PFi2->(10,11)
#   (Eh/Mo/PFi bruts NE sont PAS des distances entre paires dans ce fichier).
#
# Convention de coordonnees : comme les lignes existantes, on enregistre en
# pixels IMAGE (Y vers le bas : le haut du corps a un Y plus petit).
#
# Statut : PROTOTYPE. Dependances : shiny, openxlsx, jpeg, png.
# =============================================================================

# points enregistres dans la feuille 2 (ordre des colonnes _X/_Y)
# 23 = point derive (auto) : intersection de la droite (1,9) et de la droite
# passant par 6 parallele a l'axe (1,2) -> segment 23-6 parallele a (1,2).
.FM_LM_PTS <- c(1:19, 22L, 23L)

# Version de l'outil de saisie, tracee dans CHAQUE ligne du journal : c'est elle
# qui permettra, dans deux ans, de savoir avec quelle logique geometrique un
# specimen donne a ete digitalise.
# FONCTION et non constante : packageVersion() echouerait a l'installation, quand
# ce fichier est evalue alors que le package n'est pas encore installe.
.fm_app_version <- function()
  tryCatch(as.character(utils::packageVersion("Rfishmorph")),
           error = function(e) "dev")

# La couche de capture (journal append-only) vit dans R/journal.R et fait partie
# du meme package : plus rien a charger a la main.

# ordre de saisie au clic : d'abord l'axe brise museau -> 22 -> 24 -> caudale,
# puis les points a poser a la main (avance automatique) ; .FM_DERIVED = derives.
.FM_CLICK_ORDER <- c(1L, 22L, 24L, 2L, 3L, 4L, 7L, 5L, 6L, 13L, 14L, 10L, 12L, 16L, 17L, 18L, 19L)
# derives (auto) : 8/9/11 = points du ventre ; 15 seme. 10/12 restent dans la
# boucle de saisie ; tant qu'ils ne sont pas cliques ils suivent 11 (PFi/PFl
# conserves), et une fois places/corriges ils restent ou tu les mets.
# 22 = CHARNIERE (n'est plus "derive") : affichee entre 1 et 2 et rendue active
# a l'ouverture d'une espece en mode correction (voir seed_from_existing).
.FM_DERIVED     <- c(8L, 9L, 11L, 15L, 23L)

# MODE "new" (photos nouvelles) : meme ordre + la barre d'echelle 20/21 a la fin.
# 20/21 sont OPTIONNELS (on peut les laisser non poses) et servent uniquement a
# calculer mm_per_px = ruler_mm / dist(20,21).
.FM_SCALE_PTS       <- c(20L, 21L)
.FM_CLICK_ORDER_NEW <- c(.FM_CLICK_ORDER, .FM_SCALE_PTS)
.FM_LM_PTS_NEW      <- c(.FM_LM_PTS, .FM_SCALE_PTS)

.fm_next <- function(cur, order = .FM_CLICK_ORDER) {
  i <- match(cur, order)
  if (is.na(i)) return(cur)
  if (i >= length(order)) return(3L)  # apres le dernier -> revient au 1er anatomique (3)
  order[i + 1L]
}

# --- proportions medianes FISHMORPH (mode "new", pas de segments) ------------
# Medianes de segment/Bl calculees sur FishMORPH_seg.csv (n = 6492 a 7706 especes
# selon le segment ; seules les valeurs > 0 sont retenues). Elles servent
# UNIQUEMENT de graine : apres les clics 1 (museau) et 2 (base caudale), chaque
# point est pre-place a la proportion mediane du corps, puis corrige au clic.
# Aucune longueur n'est donc "verrouillee" dans ce mode, contrairement a
# "reconstruct" ou les segments mesures contraignent les paires.
.FM_NEW_RATIOS <- c(Bd = 0.2480, Hd = 0.1382, Eh2 = 0.1372, Mo2 = 0.1152,
                    PFi2 = 0.0745, PFl = 0.1829, Ed = 0.0589, Jl = 0.0559,
                    CPd = 0.1055, CFd = 0.2593)

# pseudo-segments pour .fm_place() : Bl = 1 -> ppu = Blpx, donc chaque longueur
# vaut ratio * Blpx pixels. Exactement equivalent a passer des segments mesures.
.fm_new_segments <- function() c(list(Bl = 1), as.list(.FM_NEW_RATIOS))

# nom d'espece propose a partir du nom de fichier photo :
# "Abramis_brama_2.JPG" -> "Abramis brama" (genre capitalise, epithete minuscule)
.fm_name_from_file <- function(path) {
  x <- tools::file_path_sans_ext(basename(path))
  x <- sub("\\s*\\d+$", "", x)
  x <- gsub("_(profile|dessin|dessous|dessus|photo)$", "", x, ignore.case = TRUE)
  x <- trimws(gsub("[^A-Za-z]+", " ", x))
  w <- strsplit(x, "\\s+")[[1]]
  w <- w[nzchar(w)]
  if (!length(w)) return("")
  w <- tolower(w)
  w[1] <- paste0(toupper(substr(w[1], 1, 1)), substring(w[1], 2))
  paste(w, collapse = " ")
}

# points de CHARNIERE (axe brise). 22 est un landmark enregistre ; 24 et 25 sont
# des charnieres SUPPLEMENTAIRES (pas de colonnes dans le classeur -> non
# enregistrees, ce sont des aides de saisie pour quelques specimens tres
# courbes). Places au besoin, ils permettent jusqu'a 4 segments d'axe
# (1 -> ... -> 2). Non places, ils restent "en ligne" (aucun effet).
.FM_HINGES <- c(22L, 24L, 25L)

# chaine ordonnee de l'axe brise : 1, puis les charnieres POSEES triees selon
# leur position le long de la corde 1->2, puis 2.
.fm_axis_chain <- function(P) {
  fin <- function(i) i <= nrow(P) && all(is.finite(P[i, ]))
  hs <- .FM_HINGES[vapply(.FM_HINGES, fin, logical(1))]
  if (length(hs) > 1 && fin(1) && fin(2)) {
    uc <- P[2, ] - P[1, ]
    hs <- hs[order(vapply(hs, function(i) sum((P[i, ] - P[1, ]) * uc), numeric(1)))]
  }
  c(1L, hs, 2L)
}
# longueur (px) le long de l'axe brise (somme des segments de la chaine)
.fm_axis_len_px <- function(P) {
  ch <- .fm_axis_chain(P)
  if (length(ch) < 2) return(NA_real_)
  sum(vapply(seq_len(length(ch) - 1L),
             function(k) sqrt(sum((P[ch[k + 1L], ] - P[ch[k], ])^2)), numeric(1)))
}

# colonne de segment a utiliser pour chaque paire de landmarks
.FM_PAIR_SEG <- list(
  Bl  = list(seg = "Bl",   pair = c(1, 2)),
  Bd  = list(seg = "Bd",   pair = c(3, 4)),
  Hd  = list(seg = "Hd",   pair = c(5, 6)),
  Eh  = list(seg = "Eh2",  pair = c(7, 8)),
  Mo  = list(seg = "Mo2",  pair = c(1, 9)),
  PFi = list(seg = "PFi2", pair = c(10, 11)),
  PFl = list(seg = "PFl",  pair = c(10, 12)),
  Ed  = list(seg = "Ed",   pair = c(13, 14)),
  Jl  = list(seg = "Jl",   pair = c(1, 15)),
  CPd = list(seg = "CPd",  pair = c(16, 17)),
  CFd = list(seg = "CFd",  pair = c(18, 19))
)

# parametres libres (non identifies par les segments) et valeurs par defaut.
# Valeurs f (position axiale) et o (part dorsale) recalees sur les MEDIANES des
# especes deja digitalisees du fichier (17 especes) -- les anciennes valeurs
# (o_Hd=0.85, o_PF=0.90) placaient tete et pectorale trop haut. o_PF est negatif
# car l'insertion pectorale est sous la ligne mediane du corps.
.fm_defaults <- function() list(
  f_Bd = 0.47, o_Bd = 0.50, f_Hd = 0.10, o_Hd = 0.43,
  f_eye = 0.10, o_eye = 0.82, f_PF = 0.25, o_PF = -0.69,
  ang_PFl = 35, ang_Jl = 20, f_CP = 0.93, o_CP = 0.52,
  f_CF = 1.15, o_CF = 0.47
)

# --- indexation robuste des photos ------------------------------------------
# associe un nom normalise d'espece -> chemin de fichier
.fm_photo_index <- function(photo_dir) {
  files <- list.files(photo_dir, pattern = "\\.(jpg|jpeg|png|JPG|JPEG|PNG)$",
                      full.names = TRUE)
  norm <- function(x) {
    x <- tools::file_path_sans_ext(basename(x))
    x <- sub("\\s*\\d+$", "", x)                       # " 1", " 2" a la fin
    x <- gsub("_(profile|dessin|dessous|dessus|photo)$", "", x, ignore.case = TRUE)
    x <- gsub("[^A-Za-z]+", "_", x)                    # tout separateur -> _
    tolower(gsub("^_|_$", "", x))
  }
  keys <- vapply(files, norm, character(1))
  idx <- files[!duplicated(keys)]
  names(idx) <- keys[!duplicated(keys)]
  idx
}

.fm_species_key <- function(genus_species) {
  x <- gsub("[^A-Za-z]+", "_", genus_species)          # "Genus.species" -> genus_species
  tolower(gsub("^_|_$", "", x))
}

# --- point 23 (derive) ------------------------------------------------------
# Intersection de la droite (1,9) et de la droite passant par 6 et parallele a
# l'axe AVANT (1->22, la charniere ; corde 1->2 si 22 non defini). Le segment
# 23-6 est donc parallele a 1->22 et mesure la distance axiale museau (1) ->
# base de la tete (6). Repere-agnostique (pixels image OK).
.fm_point23 <- function(P) {
  if (!all(is.finite(P[c(1L, 2L, 6L, 9L), ]))) return(c(NA_real_, NA_real_))
  d1 <- P[9L, ] - P[1L, ]               # direction de la droite (1,9)
  # 23-6 parallele au segment TETE (1 -> 22 ; repli 24 puis 2 si non pose)
  fin2 <- function(i) i <= nrow(P) && all(is.finite(P[i, ]))
  htip <- if (fin2(22L)) P[22L, ] else if (fin2(24L)) P[24L, ] else P[2L, ]
  d2 <- htip - P[1L, ]                   # direction de l'axe TETE (1 -> 22)
  cr <- d1[1] * d2[2] - d1[2] * d2[1]
  if (!is.finite(cr) || abs(cr) < 1e-9) return(c(NA_real_, NA_real_))
  w <- P[6L, ] - P[1L, ]
  a <- (w[1] * d2[2] - w[2] * d2[1]) / cr
  as.numeric(P[1L, ] + a * d1)
}

# --- placement des 20 points depuis les segments ----------------------------
# segments : liste nommee (valeurs des colonnes Bl,Bd,Hd,Eh2,Mo2,PFi2,PFl,Ed,
#            Jl,CPd,CFd) ; A,B : clics museau/base caudale (px image) ;
# params : parametres libres. Retourne matrice 22x2 (lignes = points 1..22 ;
# seules .FM_LM_PTS sont renseignees), Y vers le bas.
.fm_place <- function(segments, A, B, params = list()) {
  p  <- utils::modifyList(.fm_defaults(), params)
  gv <- function(nm) as.numeric(segments[[nm]])
  Bl <- gv("Bl")
  Blpx <- sqrt(sum((B - A)^2))
  ppu  <- Blpx / Bl                 # pixels par unite-segment
  u    <- (B - A) / Blpx            # axe antero-posterieur
  up   <- c(u[2], -u[1])            # normale "dorsale" (vers le haut ecran, Y-)
  cm   <- function(v) v * ppu
  st   <- function(f) A + Bl * ppu * f * u

  # 25 lignes : 1..23 + charnieres supplementaires 24,25 (laissees NA ici)
  P <- matrix(NA_real_, nrow = 25, ncol = 2, dimnames = list(NULL, c("X", "Y")))
  set <- function(i, xy) P[i, ] <<- xy
  vseg <- function(f, L, o) { s <- st(f); list(top = s + cm(L) * o * up,
                                               bot = s - cm(L) * (1 - o) * up) }
  set(1, A); set(2, B)
  v <- vseg(p$f_Bd, gv("Bd"), p$o_Bd); set(3, v$top); set(4, v$bot)
  v <- vseg(p$f_Hd, gv("Hd"), p$o_Hd); set(5, v$top); set(6, v$bot)
  eye <- st(p$f_eye)
  set(8, eye - cm(gv("Hd")) * p$o_eye * up)      # bas du corps sous l'oeil
  set(7, P[8, ] + cm(gv("Eh")) * up)             # centre de l'oeil (Eh2)
  set(13, P[7, ] + cm(gv("Ed") / 2) * up)
  set(14, P[7, ] - cm(gv("Ed") / 2) * up)
  set(9, P[1, ] - cm(gv("Mo")) * up)             # bas du corps sous le museau (Mo2)
  v <- vseg(p$f_PF, gv("PFi"), p$o_PF); set(10, v$top); set(11, v$bot)  # PFi2
  d <- cos(-p$ang_PFl * pi / 180) * u + sin(-p$ang_PFl * pi / 180) * up
  set(12, P[10, ] + cm(gv("PFl")) * d)
  d <- cos(-p$ang_Jl * pi / 180) * u + sin(-p$ang_Jl * pi / 180) * up
  set(15, P[1, ] + cm(gv("Jl")) * d)
  v <- vseg(p$f_CP, gv("CPd"), p$o_CP); set(16, v$top); set(17, v$bot)
  v <- vseg(p$f_CF, gv("CFd"), p$o_CF); set(18, v$top); set(19, v$bot)
  set(22, st(0.5))                               # point de courbure sur l'axe
  set(23, .fm_point23(P))                         # derive : croisement (1,9) x (//axe par 6)
  P
}

# --- correction geometrique : conventions FISHMORPH -------------------------
# Applique, dans le repere de l'axe du corps (1-2) -- donc valable meme si la
# photo est inclinee -- les 5 conventions de correct_geometry_conventions() :
#   perpendiculaires a l'axe : 9 aligne sur 1, 4 sur 3, 11 sur 10 (meme
#     coordonnee AXIALE) ; groupe oeil {5,13,7,14,6,8} : meme coordonnee axiale
#     (mediane) -> verticale de l'oeil ; ventre {9,8,11,4} : meme coordonnee
#     NORMALE (mediane) -> ligne parallele a l'axe.
.fm_apply_conventions <- function(P) {
  A <- P[1, ]; B <- P[2, ]
  L <- sqrt(sum((B - A)^2)); if (!is.finite(L) || L == 0) return(P)
  u <- (B - A) / L; n <- c(u[2], -u[1])
  fin <- function(i) all(is.finite(P[i, ]))
  ax <- vapply(1:22, function(i) if (fin(i)) sum((P[i, ] - A) * u) else NA_real_, numeric(1))
  no <- vapply(1:22, function(i) if (fin(i)) sum((P[i, ] - A) * n) else NA_real_, numeric(1))
  # perpendiculaires (coordonnee axiale du point cale sur celle de l'ancre)
  if (fin(1) && fin(9))  ax[9]  <- ax[1]
  if (fin(3) && fin(4))  ax[4]  <- ax[3]
  if (fin(10) && fin(11)) ax[11] <- ax[10]
  # verticale de l'oeil : coordonnee axiale = mediane du groupe
  eg <- c(5, 13, 7, 14, 6, 8); if (any(is.finite(ax[eg]))) ax[eg] <- stats::median(ax[eg], na.rm = TRUE)
  # ligne du ventre : coordonnee normale = mediane du groupe
  hg <- c(9, 8, 11, 4);        if (any(is.finite(no[hg]))) no[hg] <- stats::median(no[hg], na.rm = TRUE)
  for (i in 1:22) if (is.finite(ax[i]) && is.finite(no[i])) P[i, ] <- A + ax[i] * u + no[i] * n
  if (nrow(P) >= 23) P[23, ] <- .fm_point23(P)   # 23 derive
  P
}

# --- correction via les fonctions canoniques du package ----------------------
# L'app travaille dans le repere arbitraire de la photo ; standardize_geometry()
# et correct_geometry_conventions() exigent l'axe 1-2 horizontal. On construit
# donc un objet landmarks (avec une barre d'echelle factice 20-21 le long de
# l'axe), on applique
#   standardize_geometry(orient = FALSE)  [rescale + rotation, une similitude]
#   -> correct_geometry_conventions()     [les 5 conventions canoniques]
# puis on inverse la similitude via les points 1-2, que les conventions ne
# deplacent jamais, pour revenir dans le repere de la photo.
#
# Equivalent a .fm_apply_conventions() a la precision machine. Cette version est
# conservee comme REFERENCE : si les deux divergent un jour, c'est celle-ci qui
# fait foi, puisqu'elle passe par le code canonique du package plutot que par la
# reimplementation locale utilisee en edition interactive (plus rapide).
#
# NOTE : depuis Rfishmorph 0.2.0 ces fonctions sont dans CE package. L'appel
# passait auparavant par intraitR::, ce qui creait une dependance externe pour du
# code deja porte ici.
.fm_correct_via_package <- function(P) {
  ax <- P[2, ] - P[1, ]; axlen <- sqrt(sum(ax^2))
  if (!is.finite(axlen) || axlen == 0) return(P)
  R <- P[1:22, , drop = FALSE]
  R[20, ] <- P[1, ]                       # barre d'echelle factice : origine
  R[21, ] <- P[1, ] + ax / axlen * axlen  # ... et extremite le long de l'axe (long. ~ Bl)
  arr <- array(NA_real_, c(22, 2, 1), dimnames = list(NULL, c("X", "Y"), "sp"))
  arr[, , 1] <- R
  fish <- structure(list(coords = arr, scale = NULL,
                         metadata = data.frame(specimen = "sp", row.names = "sp")),
                    class = c("fishmorph_landmarks", "intrait_landmarks"))
  res <- try(suppressWarnings(suppressMessages({
    fs <- standardize_geometry(fish, orient = FALSE)
    correct_geometry_conventions(fs, tolerance_coord = 1e-6)
  })), silent = TRUE)
  if (inherits(res, "try-error")) return(.fm_apply_conventions(P))
  C <- res$coords[, , 1]
  dC <- C[2, ] - C[1, ]; dR <- P[2, ] - P[1, ]
  s <- sqrt(sum(dR^2)) / sqrt(sum(dC^2))
  th <- atan2(dR[2], dR[1]) - atan2(dC[2], dC[1])
  Rot <- matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2, 2)
  Pout <- P
  for (i in .FM_LM_PTS) if (i <= nrow(C) && all(is.finite(C[i, ])))
    Pout[i, ] <- P[1, ] + s * as.numeric(Rot %*% (C[i, ] - C[1, ]))
  Pout[23, ] <- .fm_point23(Pout)   # 23 recalcule (n'existe pas dans le repere du package)
  Pout
}

# --- conventions EN EDITION CONTRAINTE --------------------------------------
# Conventions perpendiculaire/parallele appliquees en direct, chacune pilotee par
# un point editable (le dernier deplace du groupe, ou un pilote par defaut), de
# sorte qu'on peut bouger les points tout en gardant les regles.
#
# AXE BRISE a 3 segments a ancres FIXES (charnieres 22 et 24 ; 25 = courbure Bl
# seule, sans convention) :
#   TETE    = segment 1 -> 22   : Mo (1-9), verticale oeil/Hd {5,13,7,14,6,8}, 23-6
#   MILIEU  = segment 22 -> 24  : Bd (3-4), pectorale PFi (10-11) et PFl (10-12)
#   CAUDALE = segment 24 -> 2   : pedoncule (16-17), nageoire caudale (18-19)
# Si une charniere n'est pas posee, repli gracieux vers la corde 1->2 (poisson
# droit / correction) : comportement retro-compatible.
# --- CONVENTION DES EXTREMES (3 = dos, 4 = ventre) ---------------------------
# FISHMORPH definit Bd comme la profondeur MAXIMALE du corps : 3 doit donc etre
# le point le plus DORSAL et 4 le point le plus VENTRAL du contour du corps. Un
# 5 (haut de tete) au-dessus du 3, ou un 11 (ventre a la pectorale) sous le 4,
# est une erreur de saisie qui sous-estime Bd.
#
# Points EXCLUS de la comparaison :
#   8, 9, 11 : points ventraux DERIVES. Ils sont calcules DEPUIS le 4 (ligne du
#           ventre) : tester si 4 est le point le plus bas contre eux est
#           circulaire. Mesure faite sur les 1036 specimens T-26 digitalises :
#           en les incluant, 20,6 % du lot est signale, dont 198 des 213 alertes
#           portent sur 8, 9 ou 11, avec un depassement median de 0,5 % de Bl --
#           du bruit de ligne de ventre, pas une erreur de Bd. En les excluant,
#           1,5 % est signale (16 specimens), dont 12 fois le cas 5-au-dessus-de-3,
#           avec un depassement median de 7,8 % de Bl. Le taux est alors STABLE
#           de 0,003 a 0,02 Bl : ce qui reste est une erreur grossiere, nettement
#           separee du bruit, et non un artefact de seuil ;
#   16-19 : pedoncule et nageoire caudale -- hors du contour du corps par
#           definition (demande explicite), la caudale depassant souvent Bd ;
#   12, 15 : extremites de la pectorale et de la machoire -- appendices, qui
#           depassent legitimement le contour ;
#   20, 21 : barre d'echelle ; 23 : point derive ; 24, 25 : charnieres de saisie.
# Restent compares a 3/4 : 1, 2, 5, 6, 7, 10, 13, 14, 22 -- les landmarks qui
# sont des MESURES independantes du contour du corps.
.FM_EXTREME_EXCLUDE <- c(8L, 9L, 11L, 12L, 15L, 16L, 17L, 18L, 19L,
                         20L, 21L, 23L, 24L, 25L)

# tolerance par defaut, en FRACTION de la corde 1-2 : 0.3 % de Bl (soit ~6 px
# pour un poisson de 2000 px). En deca, l'ecart releve du bruit de clic.
.FM_EXTREME_TOL <- 0.003
# PLANCHER absolu, en pixels. Sur une petite image la tolerance relative tombe
# sous le bruit de clic (0.003 * 600 px = 1.8 px) et un depassement de 2 px
# suffirait a declencher l'alerte -- c'est du bruit, pas une erreur. Fixe a 5 px
# sur les donnees T-26 : les specimens conformes plafonnent a -0.4 px de
# depassement (p98) et le plus petit ecart REEL vaut 11.8 px. Entre 1 et 8 px de
# plancher, le nombre de specimens signales ne bouge pas (16) : la bande est
# vide, 5 px tombe au milieu et ne coute donc aucune detection.
.FM_EXTREME_FLOOR <- 5

# libelles des points, pour les messages de l'application
.FM_PT_LABELS <- c(
  "1" = "museau", "2" = "base de la caudale", "3" = "dos (Bd sup.)",
  "4" = "ventre (Bd inf.)", "5" = "haut de la tete (Hd sup.)",
  "6" = "bas de la tete (Hd inf.)", "7" = "centre de l'oeil",
  "8" = "ventre sous l'oeil", "9" = "ventre sous le museau",
  "10" = "insertion de la pectorale", "11" = "ventre a la pectorale",
  "12" = "extremite de la pectorale", "13" = "haut de l'oeil",
  "14" = "bas de l'oeil", "15" = "extremite de la machoire",
  "16" = "pedoncule sup.", "17" = "pedoncule inf.", "18" = "caudale sup.",
  "19" = "caudale inf.", "20" = "echelle (debut)", "21" = "echelle (fin)",
  "22" = "charniere du corps", "23" = "point derive 23")
.fm_pt_label <- function(i) {
  l <- unname(.FM_PT_LABELS[as.character(i)])
  ifelse(is.na(l), "", paste0(" (", l, ")"))
}

# Coordonnees dans le repere du CORPS : abscisse le long de l'axe 1-2, hauteur
# perpendiculaire a cet axe. On raisonne sur cette hauteur et non sur le Y brut
# de l'image, car une photo inclinee fausserait la comparaison ; quand le poisson
# est horizontal, les deux coincident exactement (au signe pres).
#
# `sgn` donne le cote DORSAL, deduit de la position relative de 3 et 4 et non
# d'une convention d'image : le test est donc valable tete a gauche ou a droite,
# photo retournee, ou case "Inverser dorsal/ventral" cochee.
.fm_body_frame <- function(P) {
  if (nrow(P) < 4L) return(NULL)
  A <- P[1, ]; B <- P[2, ]
  if (!all(is.finite(A)) || !all(is.finite(B))) return(NULL)
  L <- sqrt(sum((B - A)^2)); if (!is.finite(L) || L == 0) return(NULL)
  u <- (B - A) / L; n <- c(u[2], -u[1])
  ax <- no <- rep(NA_real_, nrow(P))
  for (i in seq_len(nrow(P))) if (all(is.finite(P[i, ]))) {
    ax[i] <- sum((P[i, ] - A) * u); no[i] <- sum((P[i, ] - A) * n)
  }
  if (!is.finite(no[3]) || !is.finite(no[4]) || no[3] == no[4]) return(NULL)
  list(A = A, u = u, n = n, L = L, ax = ax, no = no, sgn = sign(no[3] - no[4]))
}

# Violations de la convention des extremes. Renvoie NULL si tout est conforme,
# sinon un data.frame : `point` (3 ou 4), `culprit` (le point qui le depasse),
# `delta` (depassement en pixels).
.fm_extreme_violations <- function(P, tol_frac = .FM_EXTREME_TOL) {
  g <- .fm_body_frame(P); if (is.null(g)) return(NULL)
  tol  <- max(.FM_EXTREME_FLOOR, tol_frac * g$L)
  cand <- setdiff(seq_len(nrow(P)), c(3L, 4L, .FM_EXTREME_EXCLUDE))
  cand <- cand[is.finite(g$no[cand])]
  if (!length(cand)) return(NULL)
  out <- list()
  d <- g$sgn * (g$no[cand] - g$no[3])           # depassement du cote DORSAL
  k <- which.max(d)
  if (d[k] > tol) out[[length(out) + 1L]] <-
    data.frame(point = 3L, culprit = cand[k], delta = unname(d[k]))
  d <- g$sgn * (g$no[4] - g$no[cand])           # depassement du cote VENTRAL
  k <- which.max(d)
  if (d[k] > tol) out[[length(out) + 1L]] <-
    data.frame(point = 4L, culprit = cand[k], delta = unname(d[k]))
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# Correction automatique : 3 (resp. 4) prend la HAUTEUR du point qui le depasse,
# en conservant son abscisse le long de l'axe. La convention "3-4 perpendiculaire
# a l'axe" est donc preservee, et seul Bd change (il augmente).
.fm_fix_extremes <- function(P, viol) {
  g <- .fm_body_frame(P); if (is.null(g)) return(P)
  for (r in seq_len(nrow(viol))) {
    i <- viol$point[r]; j <- viol$culprit[r]
    if (!is.finite(g$ax[i]) || !is.finite(g$no[j])) next
    P[i, ] <- g$A + g$ax[i] * g$u + g$no[j] * g$n
  }
  P
}

.fm_constrain <- function(P, overridden = integer(0), pfl_px = NA_real_) {
  A <- P[1, ]; B <- P[2, ]; Lab <- sqrt(sum((B - A)^2))
  if (!is.finite(Lab) || Lab == 0) return(P)
  fin <- function(i) i <= nrow(P) && all(is.finite(P[i, ]))

  # repere local (origine o, axe unitaire o->tip, normale) -> ax / no / setp.
  # setp modifie P dans l'environnement de .fm_constrain via <<-.
  frame <- function(o, tip) {
    d <- tip - o; Ld <- sqrt(sum(d^2))
    d <- if (is.finite(Ld) && Ld > 0) d / Ld else (B - A) / Lab
    nn <- c(d[2], -d[1])
    list(ax   = function(i) sum((P[i, ] - o) * d),
         no   = function(i) sum((P[i, ] - o) * nn),
         setp = function(i, a, b) P[i, ] <<- o + a * d + b * nn)
  }
  # Trois segments d'axe a ANCRES FIXES (22 et 24 = charnieres ; 25 ne sert qu'a
  # courber Bl, aucune convention) :
  #   TETE    -> segment 1 -> 22   (Mo 1-9, oeil/Hd {5,13,7,14,6,8}, 23-6)
  #   MILIEU  -> segment 22 -> 24  (Bd 3-4, pectorale PFi 10-11 et PFl 10-12)
  #   CAUDALE -> segment 24 -> 2   (pedoncule 16-17, nageoire caudale 18-19)
  # Repli gracieux si une charniere n'est pas posee (poisson droit / correction) :
  p22 <- if (fin(22)) P[22, ] else NULL
  p24 <- if (fin(24)) P[24, ] else NULL
  head_tip <- if (!is.null(p22)) p22 else if (!is.null(p24)) p24 else B
  mid_org  <- if (!is.null(p22)) p22 else A
  mid_tip  <- if (!is.null(p24)) p24 else B
  tail_org <- if (!is.null(p24)) p24 else if (!is.null(p22)) p22 else A
  fr_head <- frame(A, head_tip)       # 1 -> 22
  fr_mid  <- frame(mid_org, mid_tip)  # 22 -> 24
  fr_tail <- frame(tail_org, B)       # 24 -> 2

  driver <- function(grp, default) {          # point pilote VALIDE (finite) du groupe
    o <- overridden[overridden %in% grp]; o <- o[vapply(o, fin, logical(1))]
    if (length(o)) return(o[length(o)])
    if (fin(default)) return(default)
    pres <- grp[vapply(grp, fin, logical(1))]
    if (length(pres)) pres[1] else NA_integer_
  }
  # segment perpendiculaire a l'axe `fr` : le pilote garde tout, l'autre garde sa
  # hauteur (no) et recale son abscisse (ax) sur le pilote.
  perp <- function(fr, a, b, default) {
    if (!(fin(a) && fin(b))) return(invisible())
    dr <- driver(c(a, b), default); ot <- if (dr == a) b else a
    fr$setp(ot, fr$ax(dr), fr$no(ot))
  }
  # --- TETE (segment 1->22) ---
  perp(fr_head, 1, 9, 1L)       # Mo  : museau (1) pilote, ventre (9) adapte
  eye <- c(5, 13, 7, 14, 6, 8); de <- driver(eye, 7L)   # verticale oeil/Hd
  if (!is.na(de)) { ae <- fr_head$ax(de)
    for (i in setdiff(eye, de)) if (fin(i)) fr_head$setp(i, ae, fr_head$no(i)) }
  if (fin(7) && fin(13) && fin(14)) {          # 13/14 symetriques, diametre Ed conserve
    h <- abs(fr_head$no(13) - fr_head$no(14)) / 2
    fr_head$setp(13, fr_head$ax(7), fr_head$no(7) + h)
    fr_head$setp(14, fr_head$ax(7), fr_head$no(7) - h)
  }
  # --- MILIEU (segment 22->24) : Bd + pectorale ---
  perp(fr_mid, 3, 4, 4L)        # Bd  : ventre (4) pilote, dos (3) adapte
  perp(fr_mid, 10, 11, 11L)     # PFi : ventre (11) pilote, insertion (10) adapte
  # --- CAUDALE (segment 24->2) ---
  perp(fr_tail, 16, 17, 16L)    # pedoncule caudal vertical
  perp(fr_tail, 18, 19, 18L)    # nageoire caudale verticale

  # --- LIGNE DU VENTRE, BRISEE au point 11 ---
  #   tete   : 9, 8, 11 alignes PARALLELE a 1->22
  #   milieu : 4, 11    alignes PARALLELE a 22->24
  # Pivot par defaut = 11 (jonction). On amene chaque point VENTRAL (9, 8, 4) sur
  # sa ligne en changeant UNIQUEMENT sa hauteur (on garde son abscisse -> reste
  # perpendiculaire a l'axe) ; le point DORSAL partenaire (1, 7, 3) NE bouge PAS.
  # Ainsi 1-9 = bouche->bas du corps, 7-8 = oeil->bas du corps, 3-4 = profondeur.
  # Un point deplace a la main (overridden) n'est pas bouge.
  # 4 = point PRECIS (maitre). La chaine derive de 4 :
  #   1) 11 se cale sur 4  -> ligne 11-4 PARALLELE a 22-24 (11 garde son abscisse)
  #   2) 8,9 se calent sur 11 -> ligne 9-8-11 PARALLELE a 1-22
  # L'ordre compte : on derive 11 depuis 4 AVANT de deriver 8,9 depuis 11.
  # Chaque point garde son abscisse -> reste perpendiculaire a son axe.
  belly_line <- function(fr, pivot, movers) {
    if (is.na(pivot) || !fin(pivot)) return(invisible())
    nb <- fr$no(pivot)
    for (m in movers) if (m != pivot && fin(m)) fr$setp(m, fr$ax(m), nb)
  }
  mid_piv  <- if (fin(4)) 4L else if (fin(11)) 11L else NA_integer_
  belly_line(fr_mid,  mid_piv,  c(11L))            # 11 <- 4  (11-4 // 22-24)
  head_piv <- if (fin(11)) 11L else if (fin(9)) 9L else NA_integer_
  belly_line(fr_head, head_piv, c(8L, 9L, 11L))    # 8,9 <- 11 (9-8-11 // 1-22)

  # PFl (10->12) EN DERNIER (apres que 10 ait sa position finale) : PARALLELE a
  # 22->24 et LONGUEUR FIXE = PFl si connue (12 = 10 + PFl*u_mid). Si TU as deplace
  # 12 a la main (overridden), il reste libre.
  if (fin(10) && fin(12) && !(12L %in% overridden)) {
    if (is.finite(pfl_px)) fr_mid$setp(12, fr_mid$ax(10) + pfl_px, fr_mid$no(10))
    else                   fr_mid$setp(12, fr_mid$ax(12), fr_mid$no(10))
  }
  P
}

# =============================================================================
# Application
# =============================================================================
#' Outil interactif de digitalisation des landmarks FISHMORPH
#'
#' Ouvre une application 'shiny' qui place les 21 landmarks FISHMORPH sur des
#' photographies de specimens, selon trois files de travail commutables a la
#' volee (voir `mode`). Chaque enregistrement part d'abord dans un journal
#' append-only ([fm_journal_open()]), puis dans le classeur ; en cas d'arret
#' brutal, [fishmorph_consolidate()] retrouve tout le travail.
#'
#' Les photographies restent EN LOCAL : elles ne sont jamais copiees dans le
#' package ni dans le classeur, seuls leur nom de fichier et leurs dimensions en
#' pixels sont enregistres. C'est `photo_dir` et `new_photo_dir` qui font le lien.
#'
#' @section Controle des extremes a l'enregistrement:
#' FISHMORPH definit `Bd` comme la profondeur MAXIMALE du corps : le point 3 doit
#' donc etre le plus dorsal et le point 4 le plus ventral. Quand la case
#' *"Verifier 3/4 (extremes)"* est cochee (defaut), "Enregistrer & suivant"
#' controle cette convention avant toute ecriture et, si elle est violee, propose
#' de **remesurer** (le point fautif devient actif et la vue s'y centre), de
#' **corriger automatiquement** (3, resp. 4, prend la hauteur du point qui le
#' depasse, en gardant sa position le long de l'axe : `Bd` augmente, la
#' perpendicularite 3-4 est preservee) ou d'**enregistrer sans corriger**.
#'
#' Les hauteurs sont mesurees perpendiculairement a l'axe du corps 1-2 -- une
#' photo inclinee ne fausse donc pas le test -- et le cote dorsal est deduit de
#' la position relative de 3 et 4, ce qui rend le controle valable quelle que
#' soit l'orientation (tete a gauche ou a droite, photo retournee, case
#' "Inverser dorsal/ventral" cochee). Sont EXCLUS de la comparaison le pedoncule
#' et la nageoire caudale (16-19), qui depassent le corps par definition, ainsi
#' que les extremites d'appendices (12 pectorale, 15 machoire) et les points
#' ventraux DERIVES (8, 9, 11), calcules depuis le 4 lui-meme -- les inclure
#' signalerait 20,6 pour cent des specimens T-26 pour du bruit de ligne de
#' ventre, contre 1,5 pour cent d'erreurs franches une fois exclus ; la barre
#' d'echelle (20, 21), le point derive (23) et les charnieres (24, 25) ne sont
#' pas des points de contour. La tolerance vaut 0,003 fois la longueur du corps
#' (soit 3 pixels pour un poisson de 1000 pixels), en deca de quoi l'ecart releve
#' du bruit de clic.
#' Les points recales portent le statut `"adjusted"` dans le journal, distinct de
#' `"placed"` : la correction automatique reste tracable specimen par specimen.
#'
#' @param xlsx_path Chemin du classeur maitre (2 feuilles).
#' @param photo_dir Dossier des photos (reste en local).
#' @param out_path  Chemin de la copie de sortie. NULL -> "<maitre>_reconstructed.xlsx"
#'   dans le meme dossier. La copie est creee si absente ; sinon la reprise se
#'   fait dessus (les especes deja enregistrees sont exclues de la file).
#' @param seg_sheet,lm_sheet Noms des feuilles.
#' @param new_sheet Feuille ou sont ajoutes les NOUVEAUX specimens (mode "new").
#'   Creee (avec les entetes de `lm_sheet`) si elle n'existe pas.
#' @param new_photo_dir Dossier des photos de nouveaux specimens (mode "new").
#'   Toutes les images du dossier forment la file. Peut ne pas exister : le mode
#'   "new" est alors simplement indisponible.
#' @param ruler_mm Longueur reelle (mm) de la barre d'echelle digitalisee par les
#'   points 20 et 21 en mode "new". Modifiable dans l'app, specimen par specimen.
#' @param journal_dir Dossier du JOURNAL append-only (voir [fm_journal_open()]).
#'   Chaque enregistrement y est ajoute AVANT toute ecriture du classeur : c'est
#'   la source de verite, et elle survit a un crash.
#'   [fishmorph_consolidate()] reconstruit la base a tout moment.
#' @param operator Identifiant de l'operateur, trace dans le journal et dans le
#'   nom du fichier de session. NULL -> utilisateur systeme.
#' @param xlsx_flush_every Nombre d'enregistrements entre deux ecritures du
#'   classeur. Le classeur pese plusieurs Mo et est REECRIT INTEGRALEMENT a chaque
#'   fois : le sortir de la boucle de saisie evite autant le risque que l'attente.
#'   Les modifications non ecrites restent en memoire (donc relisibles dans la
#'   session), sont ecrites en fin de session, par le bouton dedie, et sont de
#'   toute facon dans le journal. 1 = comportement historique (a chaque specimen).
#' @param mode File de depart : "reconstruct" (especes SANS landmarks, a
#'   digitaliser depuis les segments), "correct" (especes DEJA landmarkees, a
#'   relire/corriger : les 21 points sont recharges du classeur) ou "new"
#'   (photos nouvelles de `new_photo_dir`, ajoutees a `new_sheet`). Commutable a
#'   tout moment via le bouton "Mode" dans l'app. Si la file demandee est vide,
#'   l'app demarre sur une autre.
#' @return Invisiblement `NULL` ; appelee pour son effet de bord (lance l'app).
#' @seealso [fishmorph_consolidate()] pour relire le journal,
#'   [fishmorph_build_db()] pour en construire la base,
#'   [launch_fishmorph_space()] pour explorer l'espace morphologique.
#' @examples
#' \dontrun{
#' launch_fishmorph_digitizer(
#'   xlsx_path     = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
#'   photo_dir     = "FishMORPH/Photos utilisees",
#'   new_photo_dir = "FishMORPH/Photos nouvelles",
#'   operator      = "AT",
#'   mode          = "new")
#' }
#' @export
launch_fishmorph_digitizer <- function(
    xlsx_path,
    photo_dir = file.path(dirname(xlsx_path), "Photos utilisees"),
    out_path  = NULL,
    seg_sheet = "Global_segments",
    lm_sheet  = "Global_Landmark",
    new_sheet = "new_specimens",
    new_photo_dir = file.path(dirname(xlsx_path), "Photos nouvelles"),
    ruler_mm  = 10,
    journal_dir = file.path(dirname(xlsx_path), "landmark_journal"),
    operator  = NULL,
    xlsx_flush_every = 10L,
    mode      = c("reconstruct", "correct", "new")) {

  mode <- match.arg(mode)
  if (!exists("fm_journal_open", mode = "function"))
    stop("fishmorph_landmark_store.R n'est pas charge : source() ce fichier ",
         "d'abord (il porte le journal de securite et la consolidation).",
         call. = FALSE)
  xlsx_flush_every <- max(1L, as.integer(xlsx_flush_every))
  for (pkg in c("shiny", "openxlsx")) if (!requireNamespace(pkg, quietly = TRUE))
    stop("Le package '", pkg, "' est requis.", call. = FALSE)
  if (!file.exists(xlsx_path)) stop("Classeur introuvable : ", xlsx_path, call. = FALSE)
  if (!dir.exists(photo_dir)) stop("Dossier photos introuvable : ", photo_dir, call. = FALSE)
  if (is.null(out_path)) {
    # si on ouvre deja le fichier "_reconstructed", on reecrit DEDANS (pas de
    # nouvelle copie a chaque lancement) ; sinon on cree/complete la copie.
    out_path <- if (grepl("_reconstructed\\.xlsx$", xlsx_path)) xlsx_path
                else sub("\\.xlsx$", "_reconstructed.xlsx", xlsx_path)
  }
  if (!file.exists(out_path)) file.copy(xlsx_path, out_path)

  shiny <- asNamespace("shiny")

  # --- lecture des deux feuilles depuis la COPIE (reprise possible) ----------
  # on lit les entetes bruts separement : read.xlsx peut renommer les colonnes
  # commencant par un chiffre ("1_X" -> "X1_X"), ce qui casserait l'ecriture.
  read_sheet <- function(sheet) {
    hdr <- as.character(openxlsx::read.xlsx(out_path, sheet = sheet,
                                            colNames = FALSE, rows = 1))
    # on ne coupe QUE les entetes vides de FIN : supprimer un vide interieur
    # decalerait `cols = seq_along(hdr)` et desalignerait les noms de colonnes.
    ok <- !is.na(hdr) & nzchar(hdr)
    if (any(ok)) hdr <- hdr[seq_len(max(which(ok)))] else hdr <- character(0)
    # forcer la plage complete de colonnes : sinon read.xlsx supprime les
    # colonnes de fin entierement vides (ex. 22_X / 22_Y rarement digitalises),
    # ce qui desaligne les noms.
    df <- openxlsx::read.xlsx(out_path, sheet = sheet, colNames = FALSE,
                              startRow = 2, cols = seq_along(hdr))
    if (is.null(df) || !nrow(df))                    # feuille vide (entetes seuls)
      df <- as.data.frame(matrix(NA, nrow = 0, ncol = length(hdr)))
    else if (ncol(df) < length(hdr))                 # securite : re-padding
      df[, (ncol(df) + 1):length(hdr)] <- NA
    names(df) <- hdr
    list(df = df, hdr = hdr)
  }
  s1 <- read_sheet(seg_sheet); seg_df <- s1$df
  s2 <- read_sheet(lm_sheet);  lm_df  <- s2$df; lm_hdr <- s2$hdr
  col_of <- function(nm) match(nm, lm_hdr)   # utilise lm_hdr a jour (maj possible)
  wb <- openxlsx::loadWorkbook(out_path)   # charge une fois, ecrit en memoire

  # ajoute a une feuille les colonnes manquantes (entete ecrit a la suite), et
  # les cree aussi dans le data.frame en memoire. Retourne l'entete a jour.
  ensure_cols <- function(sheet, hdr, need) {
    miss <- need[!need %in% hdr]
    if (!length(miss)) return(hdr)
    for (j in seq_along(miss))
      openxlsx::writeData(wb, sheet, miss[j], startCol = length(hdr) + j,
                          startRow = 1, colNames = FALSE)
    fm_save_workbook_atomic(wb, out_path)                    # persiste les entetes
    message("Colonnes ajoutees a '", sheet, "' : ", paste(miss, collapse = ", "))
    c(hdr, miss)
  }

  # colonnes des charnieres supplementaires 24/25 : creees si absentes, pour que
  # ces points soient ENREGISTRES comme les autres (22/23 ont deja leurs colonnes).
  hinge_cols <- c("24_X", "24_Y", "25_X", "25_Y")
  new_lm_hdr <- ensure_cols(lm_sheet, lm_hdr, hinge_cols)
  # rep(...) et non NA seul : sur une feuille vide (0 ligne) df[[cc]] <- NA echoue
  for (cc in setdiff(new_lm_hdr, lm_hdr)) lm_df[[cc]] <- rep(NA_real_, nrow(lm_df))
  lm_hdr <- new_lm_hdr                             # entete a jour -> col_of les trouve
  # points enregistres/recharges : landmarks + charnieres 22/23/24/25
  save_pts <- c(.FM_LM_PTS, 24L, 25L)

  # --- feuille des NOUVEAUX specimens (mode "new") ---------------------------
  # Creee avec les entetes de lm_sheet si absente. On y garantit ensuite les
  # colonnes dont l'app a besoin : les 21 landmarks (deja presents si "memes
  # champs"), les charnieres 24/25, la barre d'echelle 20/21, et trois colonnes
  # de tracabilite : photo_file (CLE de la ligne), ruler_mm, mm_per_px.
  new_exists <- new_sheet %in% openxlsx::getSheetNames(out_path)
  if (!new_exists) openxlsx::addWorksheet(wb, new_sheet)
  # feuille absente OU presente mais sans ligne d'entete : on y ecrit celle de
  # lm_sheet. Une feuille existante avec ses entetes est laissee telle quelle.
  hdr0 <- if (new_exists) {
    h <- suppressWarnings(as.character(openxlsx::read.xlsx(
      out_path, sheet = new_sheet, colNames = FALSE, rows = 1)))
    h[!is.na(h) & nzchar(h)]
  } else character(0)
  if (!length(hdr0)) {
    openxlsx::writeData(wb, new_sheet, t(as.matrix(lm_hdr)), colNames = FALSE)
    message("Entetes ecrits dans '", new_sheet, "' (copie de '", lm_sheet, "').")
  }
  if (!new_exists || !length(hdr0)) fm_save_workbook_atomic(wb, out_path)
  ns <- read_sheet(new_sheet)
  new_hdr <- ns$hdr
  new_df  <- ns$df
  save_pts_new <- c(.FM_LM_PTS, .FM_SCALE_PTS, 24L, 25L)
  need_new <- c("Genus.species",
                as.vector(rbind(paste0(save_pts_new, "_X"), paste0(save_pts_new, "_Y"))),
                "photo_file", "ruler_mm", "mm_per_px")
  new_hdr2 <- ensure_cols(new_sheet, new_hdr, need_new)
  for (cc in setdiff(new_hdr2, new_hdr)) new_df[[cc]] <- rep(NA, nrow(new_df))
  new_hdr <- new_hdr2
  col_of_new <- function(nm) match(nm, new_hdr)
  # tout en caractere : new_df ne sert qu'a retrouver la ligne d'une photo et a
  # compter les lignes ; les valeurs reelles sont ecrites par writeData. Evite les
  # erreurs d'affectation (chaine dans une colonne logique d'une feuille vide).
  if (ncol(new_df)) new_df[] <- lapply(new_df, as.character)

  # index photos + cles espece
  photos <- .fm_photo_index(photo_dir)
  seg_df$.key <- .fm_species_key(seg_df$Genus.species)
  lm_df$.key  <- .fm_species_key(lm_df$Genus.species)

  seg_cols <- c("Bl", "Bd", "Hd", "Eh2", "Mo2", "PFi2", "PFl", "Ed", "Jl", "CPd", "CFd")
  has_seg  <- stats::complete.cases(seg_df[, "Bl", drop = FALSE]) &
              !is.na(suppressWarnings(as.numeric(seg_df$Bl)))
  xcols <- paste0(.FM_LM_PTS, "_X")
  lm_missing <- apply(lm_df[, xcols, drop = FALSE], 1, function(r) all(is.na(r)))
  has_photo  <- lm_df$.key %in% names(photos)

  seg_by_key <- seg_df[has_seg, ]
  seg_by_key <- seg_by_key[!duplicated(seg_by_key$.key), ]
  rownames(seg_by_key) <- seg_by_key$.key

  # DEUX files, choisies au lancement (arg `mode`) et commutables par le bouton
  # "Mode" dans l'app :
  #   * reconstruct : especes SANS landmarks, AVEC segments et photo (a digitaliser)
  #   * correct     : especes DEJA landmarkees, AVEC photo (a relire / corriger ;
  #                   les 21 points sont recharges depuis le classeur, PAS
  #                   reconstruits depuis les segments)
  q_recon <- which(lm_missing &
                     lm_df$.key %in% rownames(seg_by_key) & has_photo)
  q_corr  <- which(!lm_missing & has_photo & !is.na(lm_df$.key))

  # file "new" : toutes les images du dossier des nouvelles photos. Une entree =
  # UNE photo (et non une espece) : plusieurs specimens d'une meme espece sont
  # donc possibles, chacun sur sa propre ligne de `new_sheet`.
  new_photos <- if (dir.exists(new_photo_dir))
    sort(list.files(new_photo_dir, full.names = TRUE,
                    pattern = "\\.(jpe?g|png|gif|bmp|tiff?)$", ignore.case = TRUE))
  else character(0)
  q_new <- seq_along(new_photos)

  if (!length(q_recon) && !length(q_corr) && !length(q_new))
    stop("Aucune espece exploitable (ni a reconstruire, ni a corriger avec photo, ",
         "ni nouvelle photo dans '", new_photo_dir, "').", call. = FALSE)
  # si la file demandee est vide, on bascule sur la premiere file non vide
  qlen_of <- function(m) switch(m, reconstruct = length(q_recon),
                                correct = length(q_corr), new = length(q_new), 0L)
  if (!qlen_of(mode)) {
    alt <- c("reconstruct", "correct", "new")
    alt <- alt[vapply(alt, function(m) qlen_of(m) > 0, logical(1))][1]
    message("File '", mode, "' vide -> demarrage en mode '", alt, "'."); mode <- alt
  }
  message(sprintf("Files : %d espece(s) a reconstruire, %d a corriger, %d nouvelle(s) photo(s).",
                  length(q_recon), length(q_corr), length(q_new)))

  # --- journal append-only : la source de verite -----------------------------
  # Ouvert AVANT toute saisie. Chaque enregistrement y est ajoute en premier ; le
  # classeur n'est plus qu'un export, ecrit atomiquement et par lots.
  jr <- fm_journal_open(journal_dir, operator = operator,
                        app_version = .fm_app_version())
  pending <- 0L                          # enregistrements non ecrits dans le xlsx
  # ecrit le classeur si assez d'enregistrements se sont accumules (ou si force).
  # En cas d'echec on N'ECRASE RIEN et on previent : le journal, lui, est deja
  # ecrit, donc aucune donnee n'est perdue -- fishmorph_consolidate() la retrouve.
  flush_xlsx <- function(force = FALSE) {
    if (pending == 0L) return(invisible(FALSE))
    if (!force && pending < xlsx_flush_every) return(invisible(FALSE))
    ok <- tryCatch({ fm_save_workbook_atomic(wb, out_path); TRUE },
                   error = function(e) { warning("Ecriture du classeur echouee : ",
                     conditionMessage(e), " -- les donnees restent dans le journal ",
                     "(", jr$path, ").", call. = FALSE); FALSE })
    if (ok) pending <<- 0L
    invisible(ok)
  }

  # choix pour l'acces direct (valeur = position dans la file du mode courant)
  goto_of <- function(rows) stats::setNames(seq_along(rows), lm_df$Genus.species[rows])
  choices_recon <- goto_of(q_recon); choices_corr <- goto_of(q_corr)
  choices_new   <- stats::setNames(seq_along(new_photos), basename(new_photos))

  # Lecteur d'image ROBUSTE : l'extension ment souvent (~7% des .jpg sont en fait
  # des GIF/PNG/BMP), donc on detecte le VRAI format par les octets magiques et on
  # route vers le bon lecteur. JPEG/PNG via jpeg/png (rapide) ; GIF/BMP/TIFF ou
  # tout format non natif via magick (ImageMagick) converti en tableau [H,W,3].
  read_img <- function(path) {
    sig <- tryCatch(readBin(path, "raw", n = 8L), error = function(e) raw(0))
    is_jpeg <- length(sig) >= 2 && sig[1] == as.raw(0xFF) && sig[2] == as.raw(0xD8)
    is_png  <- length(sig) >= 8 &&
      all(sig[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))
    if (is_jpeg && requireNamespace("jpeg", quietly = TRUE)) return(jpeg::readJPEG(path))
    if (is_png  && requireNamespace("png",  quietly = TRUE)) return(png::readPNG(path))
    # tout le reste (GIF/BMP/TIFF, ou .jpg mal etiquete) -> magick, qui RE-ENCODE
    # en PNG temporaire lu ensuite par png::readPNG (evite tout reshape manuel du
    # tableau, source de l'image "en rayures" quand on se trompe d'ordre des dims).
    if (requireNamespace("magick", quietly = TRUE) &&
        requireNamespace("png", quietly = TRUE)) {
      im  <- magick::image_read(path)
      tmp <- tempfile(fileext = ".png"); on.exit(unlink(tmp), add = TRUE)
      magick::image_write(im, tmp, format = "png")
      return(png::readPNG(tmp))
    }
    # dernier recours : tenter jpeg puis png (peut echouer proprement)
    out <- tryCatch(jpeg::readJPEG(path), error = function(e)
             tryCatch(png::readPNG(path), error = function(e2) NULL))
    if (is.null(out))
      stop("Format d'image non lisible (", toupper(tools::file_ext(path)),
           " reel different de l'extension). Installez le package 'magick'.",
           call. = FALSE)
    out
  }

  # --- UI --------------------------------------------------------------------
  # clic droit maintenu sur la photo = deplacer la vue (envoie des deltas a Shiny)
  pan_js <- shiny::HTML(paste(
    "(function(){var dg=false,lx=0,ly=0,adx=0,ady=0,c=0,raf=null;",
    "function el(){return document.getElementById('plot');}",
    "function flush(){raf=null;if(adx===0&&ady===0)return;Shiny.setInputValue('pan',{dx:adx,dy:ady,n:++c},{priority:'event'});adx=0;ady=0;}",
    "document.addEventListener('contextmenu',function(e){var m=el();if(m&&m.contains(e.target))e.preventDefault();});",
    "document.addEventListener('mousedown',function(e){var m=el();if(m&&m.contains(e.target)&&e.button===2){dg=true;lx=e.clientX;ly=e.clientY;e.preventDefault();}});",
    "document.addEventListener('mousemove',function(e){if(!dg)return;var m=el();if(!m)return;var r=m.getBoundingClientRect();adx+=(e.clientX-lx)/r.width;ady+=(e.clientY-ly)/r.height;lx=e.clientX;ly=e.clientY;if(!raf)raf=requestAnimationFrame(flush);});",
    "document.addEventListener('mouseup',function(e){if(e.button===2){dg=false;if(!raf)raf=requestAnimationFrame(flush);}});",
    "})();", sep = "\n"))
  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$script(pan_js)),
    shiny::tags$style(".irs{margin-bottom:2px}"),
    shiny::titlePanel("FISHMORPH - segments -> landmarks (digitalisation guidee)"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::uiOutput("progress"),
        # --- mode "new" : identite du specimen + barre d'echelle --------------
        shiny::conditionalPanel(
          "input.mode == 'new'",
          shiny::hr(),
          shiny::h5("Nouveau specimen"),
          shiny::textInput("new_species", "Nom d'espece (Genre espece)", ""),
          shiny::helpText("Pre-rempli depuis le nom du fichier photo ; corrigez-le",
                          "si besoin. C'est cette valeur qui est ecrite dans la",
                          "colonne Genus.species de la feuille des nouveaux",
                          "specimens."),
          shiny::numericInput("ruler_mm", "Barre d'echelle 20-21 : longueur reelle (mm)",
                              value = ruler_mm, min = 0, step = 1),
          shiny::helpText("Optionnel. Posez les points 20 et 21 aux deux extremites",
                          "de la reference (regle, etiquette) : mm_per_px =",
                          "longueur reelle / distance 20-21 en pixels. Non poses,",
                          "mm_per_px reste NA et les coordonnees restent en pixels."),
          shiny::uiOutput("new_photo_lab")),
        shiny::hr(),
        shiny::div(
          shiny::actionButton("zoom_in", "Zoom +"),
          shiny::actionButton("zoom_out", "Zoom -"),
          shiny::actionButton("zoom_reset", "Vue entiere")),
        shiny::helpText("Zoom : boutons +/- ; clic droit maintenu = se deplacer sur",
                        "la photo ; double-clic = vue entiere."),
        shiny::radioButtons("flip_mode", "Retourner la photo (+ landmarks)",
          c("Aucun" = "none", "Horizontal" = "h", "Vertical" = "v", "180" = "hv"),
          selected = "none", inline = TRUE),
        shiny::radioButtons("flip_disp", "Retourner la photo SEULE (landmarks fixes)",
          c("Aucun" = "none", "Horizontal" = "h", "Vertical" = "v", "180" = "hv"),
          selected = "none", inline = TRUE),
        shiny::helpText("La 2e option ne retourne QUE l'affichage de la photo :",
                        "les landmarks (et l'enregistrement) ne bougent pas. Utile",
                        "quand les points charges sont en miroir de la photo. Reste",
                        "actif d'une espece a l'autre."),
        shiny::hr(),
        shiny::checkboxInput("flipdorsal", "Inverser dorsal/ventral", FALSE),
        shiny::checkboxInput("correct",
          "Respecter les conventions (edition contrainte)", FALSE),
        shiny::checkboxInput("checkextremes",
          "Verifier 3/4 (extremes) a l'enregistrement", TRUE),
        shiny::helpText("A l'enregistrement, verifie que 3 est le point le plus",
                        "DORSAL et 4 le plus VENTRAL (hauteurs mesurees",
                        "perpendiculairement a l'axe du corps). Exclus : caudale",
                        "(16-19), extremites d'appendices (12, 15) et points",
                        "ventraux derives (8, 9, 11). En cas",
                        "d'ecart, propose de remesurer ou de corriger",
                        "automatiquement."),
        shiny::checkboxInput("showlines", "Lignes de repere (contour/oeil/ventre)", TRUE),
        shiny::checkboxInput("fastdisp", "Affichage rapide (photo allegee)", TRUE),
        shiny::hr(),
        shiny::h5("Placement initial (graine, avant vos clics)"),
        shiny::helpText("Ces curseurs ne fixent que la POSITION DE DEPART des points que",
                        "les segments ne contraignent pas (position le long du corps,",
                        "partage haut/bas, angles des nageoires/machoire). Des que vous",
                        "cliquez un point, votre clic remplace la graine ; utiles surtout",
                        "pour degrossir avant de cliquer."),
        shiny::sliderInput("f_Bd", "Bd position", 0, 1, .47, .01),
        shiny::sliderInput("o_Bd", "Bd part dorsale", 0, 1, .50, .01),
        shiny::sliderInput("f_Hd", "Hd position", 0, 1, .10, .01),
        shiny::sliderInput("o_Hd", "Hd part dorsale", 0, 1, .43, .01),
        shiny::sliderInput("f_eye", "Oeil position", 0, 1, .10, .01),
        shiny::sliderInput("o_eye", "Oeil hauteur (bas du corps)", 0, 1.5, .82, .01),
        shiny::sliderInput("f_PF", "Pectorale position", 0, 1, .25, .01),
        shiny::sliderInput("o_PF", "Pectorale part dorsale", -1, 1, -.69, .01),
        shiny::sliderInput("f_CP", "Pedoncule position", .5, 1, .93, .01),
        shiny::sliderInput("ang_PFl", "PFl angle", 0, 90, 35, 1),
        shiny::sliderInput("ang_Jl", "Jl angle", -30, 90, 20, 1)
      ),
      shiny::mainPanel(
        width = 9,
        # barre d'actions au-dessus de la photo, avec les numeros de points
        shiny::div(style = "margin-bottom:6px;",
          shiny::div(style = "display:inline-block;vertical-align:middle;margin-right:14px;",
            shiny::radioButtons("mode", NULL,
              c("A reconstruire" = "reconstruct", "Corriger existants" = "correct",
                "Nouvelles photos" = "new"),
              selected = mode, inline = TRUE)),
          shiny::actionButton("prev", "< Precedent"),
          shiny::actionButton("nextsp", "Suivant >"),
          shiny::span(style = "display:inline-block;width:14px;"),
          shiny::actionButton("set_na", "Marquer NA"),
          shiny::actionButton("save", "Enregistrer & suivant", class = "btn-primary"),
          shiny::actionButton("skip", "Passer"),
          shiny::span(style = "display:inline-block;width:14px;"),
          shiny::actionButton("flush", "Ecrire le classeur"),
          shiny::span(style = "display:inline-block;width:14px;"),
          shiny::div(style = "display:inline-block;vertical-align:middle;min-width:280px;",
            shiny::selectizeInput("goto_species", NULL, choices = NULL,
              selected = NULL, width = "280px",
              options = list(placeholder = "Aller a une espece...")))),
        shiny::uiOutput("lm_buttons"),
        shiny::plotOutput("plot", height = "620px", click = "click",
          dblclick = "img_dblclick"),
        shiny::fluidRow(
          shiny::column(7,
            shiny::h5("Controle : segment cible vs. reconstruit (px)"),
            shiny::tableOutput("rt")),
          shiny::column(5, shiny::verbatimTextOutput("status"))
        )
      )
    )
  )

  # --- serveur ---------------------------------------------------------------
  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      qi = 1L, mode = mode, img = NULL, w = NULL, h = NULL,
      A = NULL, B = NULL, P = NULL, override = list(), saved = integer(0),
      sel = 1L, zoom = 1, cx = NULL, cy = NULL, hx = NULL, hy = NULL,
      arr = NULL, flip = "none", dispflip = "none", na = integer(0),
      newstamp = 0L,         # incremente a chaque ecriture dans new_sheet
      flushstamp = 0L,       # incremente a chaque ecriture du classeur
      edited = integer(0),   # points DEPLACES par l'utilisateur cette session
                             # (distincts des points simplement charges du classeur)
      adjusted = integer(0)) # points recales par la convention des extremes
                             # (statut "adjusted" dans le journal)

    # file et liste d'acces direct du mode courant. En mode "new" la file indexe
    # les PHOTOS de new_photo_dir (et non des lignes de lm_df).
    qrows    <- shiny::reactive(switch(rv$mode, correct = q_corr, new = q_new, q_recon))
    goto_now <- shiny::reactive(switch(rv$mode, correct = choices_corr,
                                       new = choices_new, choices_recon))
    is_new   <- shiny::reactive(identical(rv$mode, "new"))
    # ordre de saisie et points affiches : + barre d'echelle 20/21 en mode "new"
    click_order <- shiny::reactive(if (is_new()) .FM_CLICK_ORDER_NEW else .FM_CLICK_ORDER)
    lm_pts      <- shiny::reactive(if (is_new()) .FM_LM_PTS_NEW else .FM_LM_PTS)

    # peuplement cote serveur du champ d'acces direct : la liste n'est jamais
    # rendue entierement dans le navigateur (filtrage/pagination cote serveur).
    # NB : on utilise les choix du mode INITIAL (valeur `mode`), pas la reactive
    # goto_now(), car on est ici hors contexte reactif.
    shiny::updateSelectizeInput(session, "goto_species",
      choices = switch(mode, correct = choices_corr, new = choices_new, choices_recon),
      selected = 1L, server = TRUE)

    # retournement + affichage : on retourne le TABLEAU image (numerique) puis, si
    # l'affichage rapide est actif, on le sous-echantillonne pour le rendu. Les
    # COORDONNEES restent en pixels d'origine (rv$w/rv$h inchanges), donc clics et
    # enregistrement ne sont pas affectes -- seule la nettete a l'ecran change.
    flip_arr <- function(a, mode) {
      d <- dim(a); H <- d[1]; W <- d[2]
      if (length(d) == 3) {
        if (grepl("h", mode)) a <- a[, W:1, , drop = FALSE]
        if (grepl("v", mode)) a <- a[H:1, , , drop = FALSE]
      } else {
        if (grepl("h", mode)) a <- a[, W:1, drop = FALSE]
        if (grepl("v", mode)) a <- a[H:1, , drop = FALSE]
      }
      a
    }
    downscale <- function(a, maxdim = 1600L) {
      d <- dim(a); if (max(d[1], d[2]) <= maxdim) return(a)
      st <- ceiling(max(d[1], d[2]) / maxdim)
      ri <- seq(1L, d[1], by = st); ci <- seq(1L, d[2], by = st)
      if (length(d) == 3) a[ri, ci, , drop = FALSE] else a[ri, ci, drop = FALSE]
    }
    make_disp <- function() {
      if (is.null(rv$arr)) return(NULL)
      a <- flip_arr(rv$arr, rv$flip)          # retournement "photo + landmarks"
      a <- flip_arr(a, rv$dispflip)           # retournement PUREMENT visuel (points fixes)
      if (isTRUE(input$fastdisp)) a <- downscale(a)
      grDevices::as.raster(a)
    }
    flip_pt <- function(p, mode) {
      if (is.null(p)) return(p)
      if (grepl("h", mode)) p[1] <- rv$w - p[1]
      if (grepl("v", mode)) p[2] <- rv$h - p[2]
      p
    }
    remap <- function(p, oldm, newm) flip_pt(flip_pt(p, oldm), newm)

    # cur_idx = position dans la file (ligne de lm_df, ou index de photo si "new")
    cur_idx  <- shiny::reactive(qrows()[rv$qi])
    cur_row  <- shiny::reactive(if (is_new()) NA_integer_ else cur_idx())
    cur_key  <- shiny::reactive(if (is_new()) NA_character_ else lm_df$.key[cur_row()])
    # chemin de la photo courante : index des photos du classeur, ou fichier brut
    cur_photo <- shiny::reactive({
      i <- cur_idx(); if (length(i) != 1 || is.na(i)) return(NA_character_)
      if (is_new()) new_photos[i] else {
        k <- cur_key(); if (is.na(k) || !k %in% names(photos)) NA_character_ else photos[[k]]
      }
    })
    # ligne de new_sheet correspondant a un fichier photo (NA si absent). Version
    # NON reactive : `new_df` n'est pas un reactiveVal, donc la recherche doit
    # relire l'objet a jour au moment de l'enregistrement -- sinon un second clic
    # sur "Enregistrer" avant navigation ajouterait une ligne EN DOUBLE.
    new_row_of <- function(f) {
      if (length(f) != 1 || is.na(f) || !nrow(new_df) ||
          !"photo_file" %in% names(new_df)) return(NA_integer_)
      h <- which(!is.na(new_df$photo_file) & new_df$photo_file == f)
      if (length(h)) h[1] else NA_integer_
    }
    # version reactive pour l'affichage : invalidee par la navigation et par
    # rv$newstamp (incremente apres chaque ecriture dans new_sheet).
    cur_new_row <- shiny::reactive({
      if (!is_new()) return(NA_integer_)
      rv$newstamp
      new_row_of(basename(cur_photo()))
    })
    cur_name <- shiny::reactive({
      if (!is_new()) return(lm_df$Genus.species[cur_row()])
      # mode "new" : le champ de saisie fait foi ; a defaut, le nom deja
      # enregistre dans new_sheet, sinon celui deduit du nom de fichier.
      nm <- trimws(as.character(input$new_species %||% ""))
      if (nzchar(nm)) return(nm)
      basename(cur_photo())
    })
    cur_seg  <- shiny::reactive({
      # mode "new" : pas de segments mesures -> pseudo-segments = proportions
      # medianes FISHMORPH avec Bl = 1 (voir .FM_NEW_RATIOS). Le reste du code
      # (placement, echelle px/unite, table de controle) est inchange.
      if (is_new()) {
        s <- .fm_new_segments()
        return(stats::setNames(lapply(seg_cols, function(nm) as.numeric(s[[nm]])), seg_cols))
      }
      k <- cur_key()
      if (length(k) != 1 || is.na(k) || !k %in% rownames(seg_by_key))
        return(stats::setNames(as.list(rep(NA_real_, length(seg_cols))), seg_cols))
      s <- seg_by_key[k, seg_cols]
      stats::setNames(as.list(as.numeric(s)), seg_cols)
    })
    # scalaire numerique sur : input$... peut etre NULL (input pas encore cree) ou
    # "" -> as.numeric() rend numeric(0), et `if (is.finite(numeric(0)))` echoue.
    num1 <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (length(v) != 1 || !is.finite(v)) NA_real_ else v
    }
    # echelle mm/px depuis les points 20-21 et la longueur de regle saisie
    mmpp_of <- function(P) {
      mm <- num1(input$ruler_mm)
      if (!is.finite(mm) || mm <= 0) return(NA_real_)
      if (nrow(P) < 21 || !all(is.finite(P[c(20L, 21L), ]))) return(NA_real_)
      d <- sqrt(sum((P[21L, ] - P[20L, ])^2))
      if (!is.finite(d) || d <= 0) NA_real_ else mm / d
    }

    # mode "correct" : recharge les 21 landmarks deja enregistres du classeur
    # (LM1 -> A, LM2 -> B, les autres en overrides) pour les relire / deplacer.
    # Les points vides (NA dans la feuille) sont marques NA ; LM23 est derive et
    # n'est donc pas recharge (il est recalcule apres coup). L'axe et l'echelle
    # decoulent des LM1/LM2 charges, meme si les segments manquent.
    # `df`/`row` : par defaut la feuille des landmarks a la ligne courante ; en
    # mode "new" on passe new_df et la ligne deja enregistree pour cette photo.
    # `pts` : points a recharger (les 21 landmarks, + 20/21 en mode "new").
    seed_from_existing <- function(df = lm_df, row = cur_row(),
                                   pts = setdiff(.FM_LM_PTS, c(1L, 2L, 23L)),
                                   extra = c(24L, 25L)) {
      if (length(row) != 1 || is.na(row) || row > nrow(df)) return()
      getxy <- function(pt) {
        xc <- paste0(pt, "_X"); yc <- paste0(pt, "_Y")
        if (!all(c(xc, yc) %in% names(df))) return(c(NA_real_, NA_real_))
        c(suppressWarnings(as.numeric(df[row, xc])),
          suppressWarnings(as.numeric(df[row, yc])))
      }
      a <- getxy(1L); b <- getxy(2L)
      if (all(is.finite(a))) rv$A <- a
      if (all(is.finite(b))) rv$B <- b
      ov <- list(); na <- integer(0)
      for (pt in pts) {                            # 3..19, 22 (+ 20/21 en "new")
        xy <- getxy(pt)
        if (all(is.finite(xy))) ov[[as.character(pt)]] <- xy else na <- c(na, pt)
      }
      for (pt in extra) {                          # charnieres : chargees si presentes,
        xy <- getxy(pt)                            # sinon simplement non posees (pas NA)
        if (all(is.finite(xy))) ov[[as.character(pt)]] <- xy
      }
      rv$override <- ov; rv$na <- na; rv$sel <- 22L  # charniere active a l'ouverture
    }
    # mode "new" : recharge une photo deja enregistree dans new_sheet. La barre
    # d'echelle (20/21) est rechargee mais n'est jamais marquee NA -- elle est
    # optionnelle, absente = "non posee" et non "non mesurable".
    seed_from_new <- function() {
      r <- cur_new_row(); if (is.na(r)) return()
      seed_from_existing(df = new_df, row = r,
                         pts = setdiff(.FM_LM_PTS, c(1L, 2L, 23L)),
                         extra = c(24L, 25L, .FM_SCALE_PTS))
    }

    load_species <- function() {
      rv$A <- NULL; rv$B <- NULL; rv$P <- NULL; rv$override <- list(); rv$na <- integer(0)
      rv$edited <- integer(0); rv$adjusted <- integer(0)
      rv$sel <- 1L; rv$zoom <- 1; rv$cx <- NULL; rv$cy <- NULL
      if (!length(qrows())) { rv$img <- NULL; rv$arr <- NULL; return() }
      path <- cur_photo()
      img <- if (is.na(path)) NULL else tryCatch(read_img(path), error = function(e) NULL)
      if (is.null(img)) { rv$img <- NULL; rv$arr <- NULL; return() }
      rv$arr <- img; rv$h <- dim(img)[1]; rv$w <- dim(img)[2]
      rv$flip <- "none"; rv$img <- make_disp()
      shiny::updateRadioButtons(session, "flip_mode", selected = "none")
      if (is_new()) {
        # nom d'espece : celui deja enregistre pour cette photo, sinon deduit du
        # nom de fichier. Recharge aussi les points si la photo a deja ete faite.
        r <- cur_new_row()
        nm <- if (!is.na(r) && "Genus.species" %in% names(new_df))
                as.character(new_df[r, "Genus.species"]) else NA_character_
        if (is.na(nm) || !nzchar(nm)) nm <- .fm_name_from_file(path)
        shiny::updateTextInput(session, "new_species", value = nm)
        rm <- if (!is.na(r) && "ruler_mm" %in% names(new_df))
                num1(new_df[r, "ruler_mm"]) else NA_real_
        if (is.finite(rm)) shiny::updateNumericInput(session, "ruler_mm", value = rm)
        if (!is.na(r)) seed_from_new()
        return()
      }
      # recharge les landmarks enregistres si l'espece en a deja : toujours en mode
      # "correct", et aussi en mode "reconstruire" pour une espece deja sauvegardee
      # (sinon revenir dessus repartait des positions calculees -> 24/25 perdus).
      row0 <- cur_row()
      has_saved <- !is.na(row0) && "1_X" %in% names(lm_df) &&
        is.finite(suppressWarnings(as.numeric(lm_df[row0, "1_X"])))
      if (rv$mode == "correct" || has_saved) seed_from_existing()
    }
    shiny::observeEvent(rv$qi, load_species(), ignoreInit = FALSE)

    # bascule de mode : change de file, revient a la 1re espece, recharge la
    # liste d'acces direct et l'espece courante
    shiny::observeEvent(input$mode, {
      if (identical(input$mode, rv$mode)) return()
      rv$mode <- input$mode
      shiny::updateSelectizeInput(session, "goto_species",
        choices = goto_now(), selected = 1L, server = TRUE)
      if (rv$qi == 1L) load_species() else rv$qi <- 1L  # sinon l'observer de qi recharge
    }, ignoreInit = TRUE)

    params <- shiny::reactive({
      d <- .fm_defaults()
      d$f_Bd <- input$f_Bd; d$o_Bd <- input$o_Bd; d$f_Hd <- input$f_Hd; d$o_Hd <- input$o_Hd
      d$f_eye <- input$f_eye; d$o_eye <- input$o_eye
      d$f_PF <- input$f_PF; d$o_PF <- input$o_PF
      d$f_CP <- input$f_CP; d$ang_PFl <- input$ang_PFl; d$ang_Jl <- input$ang_Jl
      d
    })

    # reconstruction courante (matrice 22x2), avec overrides manuels appliques
    recon <- shiny::reactive({
      shiny::req(rv$A, rv$B)
      seg <- cur_seg()
      seg2 <- seg
      names(seg2)[match(c("Eh2","Mo2","PFi2"), names(seg2))] <- c("Eh","Mo","PFi")
      pr <- params(); if (isTRUE(input$flipdorsal)) {
        for (nm in c("o_Bd","o_Hd","o_eye","o_PF","o_CP","o_CF")) pr[[nm]] <- 1 - pr[[nm]]
        pr$ang_PFl <- -pr$ang_PFl; pr$ang_Jl <- -pr$ang_Jl
      }
      P <- .fm_place(seg2, rv$A, rv$B, pr)
      for (k in names(rv$override)) P[as.integer(k), ] <- rv$override[[k]]
      if (length(rv$na)) P[rv$na, ] <- NA_real_          # points marques non mesurables
      if (isTRUE(input$correct)) {
        # les conventions ne "protegent" que les points que TU as deplaces cette
        # session (rv$edited), pas les points simplement recharges du classeur en
        # mode correction -- sinon, tous etant des overrides, plus rien ne suivrait
        # (ex. bouger le 4 ne ramenait plus 8/9/11 sur la ligne du ventre).
        # longueur cible PFl en pixels (echelle = axe brise Bl) -> 12 = 10 + PFl*uf
        blpx <- .fm_axis_len_px(P)
        ppu <- blpx / as.numeric(seg$Bl)
        # en mode "new" PFl n'est PAS mesure (c'est une mediane de graine) : on ne
        # verrouille donc pas la longueur 10-12, on garde seulement le parallelisme.
        pfl_px <- if (is_new() || !is.finite(ppu)) NA_real_
                  else as.numeric(seg$PFl) * ppu
        P <- .fm_constrain(P, rv$edited, pfl_px = pfl_px)
      }
      P[23, ] <- .fm_point23(P)     # 23 toujours recalcule (auto) apres edition/conventions
      P
    })

    # centre le zoom sur le point actif (s'il a une position)
    zoom_to_sel <- function() {
      if (is.null(rv$A) || is.null(rv$B)) return()
      P <- try(recon(), silent = TRUE); if (inherits(P, "try-error")) return()
      if (rv$sel <= nrow(P) && all(is.finite(P[rv$sel, ]))) {
        rv$cx <- P[rv$sel, 1]; rv$cy <- P[rv$sel, 2] }
    }

    # clic sur la photo : pose le point ACTIF, puis avance automatiquement
    shiny::observeEvent(input$click, {
      if (is.null(rv$img)) return()
      pt <- c(input$click$x, input$click$y); s <- rv$sel
      if (s == 1L) rv$A <- pt
      else if (s == 2L) rv$B <- pt
      else {  # re-insere en fin de liste : le dernier point deplace pilote son groupe
        ov <- rv$override; k <- as.character(s)
        ov[[k]] <- NULL; ov[[k]] <- pt; rv$override <- ov
        if (s %in% rv$na) rv$na <- setdiff(rv$na, s)   # re-place -> n'est plus NA
        rv$edited <- union(rv$edited, s)               # point deplace a la main
        # un point recale par la convention puis repointe a la main redevient une
        # MESURE : il ne doit plus sortir "adjusted" dans le journal.
        rv$adjusted <- setdiff(rv$adjusted, s)
      }
      rv$sel <- .fm_next(s, click_order())
    })

    # barre de boutons : selectionne le point actif
    shiny::observeEvent(input$sel_btn, { rv$sel <- as.integer(input$sel_btn); zoom_to_sel() })

    # marquer le point actif comme NA (non mesurable) puis avancer
    shiny::observeEvent(input$set_na, {
      if (rv$sel %in% c(1L, 2L)) return()          # museau/caudale requis pour l'axe
      rv$na <- union(rv$na, rv$sel)
      rv$adjusted <- setdiff(rv$adjusted, rv$sel)
      ov <- rv$override; ov[[as.character(rv$sel)]] <- NULL; rv$override <- ov
      rv$sel <- .fm_next(rv$sel, click_order())
    })

    # --- zoom ---
    shiny::observeEvent(input$zoom_in,  { rv$zoom <- min(rv$zoom * 1.5, 12); zoom_to_sel() })
    shiny::observeEvent(input$zoom_out, { rv$zoom <- max(rv$zoom / 1.5, 1)
      if (rv$zoom == 1) { rv$cx <- NULL; rv$cy <- NULL } })
    shiny::observeEvent(input$zoom_reset, { rv$zoom <- 1; rv$cx <- NULL; rv$cy <- NULL })
    shiny::observeEvent(input$img_dblclick, { rv$zoom <- 1; rv$cx <- NULL; rv$cy <- NULL })
    # clic droit maintenu : deplacement (pan) de la vue
    shiny::observeEvent(input$pan, {
      if (is.null(rv$img) || rv$zoom <= 1) return()
      if (is.null(rv$cx)) rv$cx <- rv$w / 2
      if (is.null(rv$cy)) rv$cy <- rv$h / 2
      rv$cx <- rv$cx - input$pan$dx * (rv$w / rv$zoom)
      rv$cy <- rv$cy - input$pan$dy * (rv$h / rv$zoom)
    })

    # --- retourner la photo (transforme aussi les points deja poses) ---
    shiny::observeEvent(input$flip_mode, {
      if (is.null(rv$arr)) return()
      oldm <- rv$flip; newm <- input$flip_mode
      if (identical(oldm, newm)) return()
      if (!is.null(rv$A)) rv$A <- remap(rv$A, oldm, newm)
      if (!is.null(rv$B)) rv$B <- remap(rv$B, oldm, newm)
      if (length(rv$override))
        rv$override <- lapply(rv$override, remap, oldm = oldm, newm = newm)
      rv$flip <- newm
      rv$img <- make_disp()
      rv$zoom <- 1; rv$cx <- NULL; rv$cy <- NULL
    }, ignoreInit = TRUE)
    # retournement PUREMENT visuel : ne retourne que l'affichage de la photo,
    # les landmarks (et donc l'enregistrement) restent inchanges. Persiste d'une
    # espece a l'autre. Le repere des clics est celui des points, donc une
    # correction faite ici reste coherente avec les points deja charges.
    shiny::observeEvent(input$flip_disp, {
      if (is.null(rv$arr)) return()
      rv$dispflip <- input$flip_disp
      rv$img <- make_disp()
    }, ignoreInit = TRUE)
    # bascule affichage rapide (ne touche pas aux points)
    shiny::observeEvent(input$fastdisp, { if (!is.null(rv$arr)) rv$img <- make_disp() },
                        ignoreInit = TRUE)

    # barre de points au-dessus de la photo (vert = actif, bleu = pose, gris = derive)
    output$lm_buttons <- shiny::renderUI({
      # axe brise en tete de liste : 1, 22, 24, 2 ; puis anatomiques ; 25 a la FIN
      anat <- setdiff(click_order(), c(1L, 22L, 24L, 2L, .FM_SCALE_PTS))
      order_show <- c(1L, 22L, 24L, 2L, anat, .FM_DERIVED, 25L,
                      if (is_new()) .FM_SCALE_PTS)
      placed <- function(i) {
        if (i == 1L) return(!is.null(rv$A)); if (i == 2L) return(!is.null(rv$B))
        as.character(i) %in% names(rv$override)
      }
      btns <- lapply(order_show, function(i) {
        col <- if (i == rv$sel) "background:#28a745;color:#fff;font-weight:bold;"
               else if (i %in% rv$na) "background:#f8d7da;color:#a00;text-decoration:line-through;"
               else if (i %in% .FM_SCALE_PTS) "background:#d9f2e6;color:#065;font-weight:bold;"  # barre d'echelle
               else if (i %in% .FM_HINGES) "background:#ffd24d;color:#000;font-weight:bold;"  # charnieres 22/24/25
               else if (i %in% .FM_DERIVED) "background:#eee;color:#999;"
               else if (placed(i)) "background:#cfe8ff;"
               else "background:#f7f7f7;"
        shiny::tags$button(type = "button", i,
          onclick = sprintf("Shiny.setInputValue('sel_btn', %d, {priority:'event'});", i),
          style = paste0("margin:1px;padding:3px 8px;min-width:34px;border:1px solid #ccc;",
                         "border-radius:3px;cursor:pointer;", col))
      })
      shiny::div(style = "margin-bottom:6px;line-height:2.2;",
        shiny::tags$strong("Point actif (cliquez la photo pour le poser -> avance auto) : "),
        btns,
        shiny::tags$div(style = "font-size:11px;color:#666;",
          "Vert = actif ; bleu = pose ; rose barre = NA ; gris = derive (auto) ;",
          "jaune = CHARNIERES. Axe brise : 1 -> 22 -> 24 -> 2 (posez 22 puis 24 sur",
          "les coudes). Tete sur 1-22, Bd + pectorale (10,11,12) sur 22-24, caudale",
          "(16-17,18-19) sur 24-2. 25 (fin de liste) = courbure Bl entre 22 et 24,",
          "sans convention. 24/25 ne sont PAS enregistrees (aides de saisie).",
          if (is_new())
            " Vert pale = BARRE D'ECHELLE 20/21 (optionnelle, en fin de liste)."))
    })

    nav <- function(step) {
      ni <- rv$qi + step
      if (ni >= 1 && ni <= length(qrows())) rv$qi <- ni
    }
    shiny::observeEvent(input$nextsp, nav(1))
    shiny::observeEvent(input$prev,   nav(-1))
    shiny::observeEvent(input$skip,   nav(1))

    # acces direct : le champ (recherche par nom) saute a l'espece choisie
    shiny::observeEvent(input$goto_species, {
      ni <- suppressWarnings(as.integer(input$goto_species))
      if (!is.na(ni) && ni >= 1 && ni <= length(qrows()) && ni != rv$qi)
        rv$qi <- ni
    }, ignoreInit = TRUE)
    # garde le champ synchronise quand on navigue avec les boutons / enregistre
    shiny::observeEvent(rv$qi, {
      shiny::updateSelectizeInput(session, "goto_species", selected = rv$qi)
    }, ignoreInit = TRUE)

    # --- enregistrement d'un NOUVEAU specimen dans `new_sheet` -----------------
    # Cle de la ligne = photo_file (basename). Si la photo y figure deja, la ligne
    # est REECRITE ; sinon une ligne est AJOUTEE a la fin. Retourne TRUE si ecrit.
    save_new <- function(P) {
      nm <- trimws(as.character(input$new_species %||% ""))
      if (!nzchar(nm)) {
        shiny::showNotification("Renseignez le nom de l'espece avant d'enregistrer.",
                                type = "error")
        return(FALSE)
      }
      f <- basename(cur_photo())
      if (is.na(f)) return(FALSE)
      r <- new_row_of(f)                           # lecture directe (cf. new_row_of)
      if (is.na(r)) {                              # nouvelle ligne en fin de feuille
        r <- nrow(new_df) + 1L
        blank <- as.data.frame(matrix(NA_character_, nrow = 1, ncol = ncol(new_df)),
                               stringsAsFactors = FALSE)
        names(blank) <- names(new_df)
        new_df <<- rbind(new_df, blank)
      }
      r_excel <- r + 1L                            # +1 pour l'entete
      wr <- function(col, val) {
        j <- col_of_new(col); if (is.na(j)) return(invisible())
        openxlsx::writeData(wb, new_sheet, val, startCol = j, startRow = r_excel,
                            colNames = FALSE)
        new_df[r, col] <<- as.character(val)       # new_df est tout-caractere
      }
      wr("Genus.species", nm)
      wr("photo_file", f)
      for (pnum in save_pts_new) {                 # 1..19, 20, 21, 22, 23, 24, 25
        wr(paste0(pnum, "_X"), round(P[pnum, 1], 3))
        wr(paste0(pnum, "_Y"), round(P[pnum, 2], 3))
      }
      mm <- num1(input$ruler_mm)
      wr("ruler_mm", if (is.finite(mm)) mm else NA)
      mpp <- mmpp_of(P)
      wr("mm_per_px", if (is.finite(mpp)) round(mpp, 6) else NA)
      rv$newstamp <- rv$newstamp + 1L              # invalide cur_new_row()
      TRUE
    }

    # --- statut de chaque point, pour le journal -------------------------------
    # C'est l'information que le format large du classeur ne peut pas porter :
    #   placed  : pose / deplace a la main, ou recharge d'une saisie anterieure
    #   seeded  : ENCORE A SA POSITION DE GRAINE, donc jamais verifie -> a auditer
    #   adjusted: recale par la convention des extremes (3/4), pas pointe
    #   derived : calcule automatiquement (8, 9, 11, 15, 23)
    #   na      : declare non mesurable
    point_status <- function(points) {
      ov <- names(rv$override)
      # rv$edited passe AVANT .FM_DERIVED : un point derive que tu as explicitement
      # repositionne cette session n'est plus un point calcule, c'est une mesure.
      st <- vapply(points, function(p) {
        if (p %in% rv$na) "na"
        # "adjusted" AVANT "placed" : un point recale par la convention des
        # extremes n'a pas ete pointe par l'operateur, la distinction doit
        # survivre dans le journal (controle qualite a posteriori).
        else if (p %in% rv$adjusted) "adjusted"
        else if (p %in% rv$edited) "placed"
        else if (p %in% .FM_DERIVED) "derived"
        else if (p %in% c(1L, 2L) || as.character(p) %in% ov) "placed"
        else "seeded"
      }, character(1))
      stats::setNames(st, as.character(points))
    }

    # ecrit l'enregistrement courant dans le JOURNAL (avant tout xlsx)
    journal_write <- function(P, row_key, points, species) {
      mm <- num1(input$ruler_mm); mpp <- mmpp_of(P)
      fm_journal_append(jr, row_key = row_key, coords = P, points = points,
        status = point_status(points), species = species,
        photo_file = basename(cur_photo()), mode = rv$mode,
        target_sheet = if (is_new()) new_sheet else lm_sheet,
        img_w = rv$w, img_h = rv$h,
        ruler_mm = if (is_new() && is.finite(mm)) mm else NA,
        mm_per_px = if (is_new() && is.finite(mpp)) mpp else NA)
    }

    # --- enregistrement ---------------------------------------------------------
    # ORDRE IMPORTANT : le journal d'abord (ajout d'un bloc de lignes, immuable,
    # instantane), le classeur ensuite et par lots. Si R s'arrete entre les deux,
    # rien n'est perdu : fishmorph_consolidate(journal_dir) reconstruit la base.
    # --- convention des extremes : verification a l'enregistrement --------------
    # Le bouton "Enregistrer & suivant" ne declenche plus l'ecriture directement :
    # il passe d'abord par ce controle. Si 3 n'est pas le point le plus dorsal (ou
    # 4 le plus ventral), une fenetre propose de remesurer ou de corriger.
    conv_msg <- function(v) {
      shiny::tags$ul(lapply(seq_len(nrow(v)), function(r) {
        i <- v$point[r]; j <- v$culprit[r]
        shiny::tags$li(sprintf(
          "Le point %d%s doit etre le plus %s : le point %d%s le depasse de %.0f px.",
          i, .fm_pt_label(i), if (i == 3L) "DORSAL" else "VENTRAL",
          j, .fm_pt_label(j), v$delta[r]))
      }))
    }
    show_conv_modal <- function(v) {
      shiny::showModal(shiny::modalDialog(
        title = "Conventions FISHMORPH : Bd (3-4) n'est pas la profondeur maximale",
        conv_msg(v),
        shiny::tags$p(shiny::tags$em(
          "Hauteurs mesurees perpendiculairement a l'axe du corps. La caudale",
          "(16-19), les extremites d'appendices (12, 15) et les points ventraux",
          "derives (8, 9, 11) sont exclus du test.")),
        shiny::tags$p("Corriger automatiquement donne au point sa hauteur, en",
                      "gardant sa position le long de l'axe ; les points recales",
                      "sont notes 'adjusted' dans le journal."),
        footer = shiny::tagList(
          shiny::actionButton("conv_remeasure", "Remesurer", class = "btn-primary"),
          shiny::actionButton("conv_fix", "Corriger automatiquement et enregistrer"),
          shiny::actionButton("conv_asis", "Enregistrer sans corriger")),
        easyClose = FALSE, size = "l"))
    }
    # applique la correction, en repassant par recon() : les conventions d'edition
    # contrainte peuvent redeplacer des points (ligne du ventre notamment), donc on
    # itere jusqu'a stabilite -- 3 passes suffisent largement, la garde evite une
    # boucle infinie sur un cas pathologique.
    apply_conv_fix <- function() {
      for (it in 1:3) {
        P <- recon()
        v <- .fm_extreme_violations(P)
        if (is.null(v)) return(invisible(TRUE))
        Pf <- .fm_fix_extremes(P, v)
        ov <- rv$override
        for (i in v$point) {
          k <- as.character(i); ov[[k]] <- NULL; ov[[k]] <- Pf[i, ]
        }
        rv$override <- ov
        rv$na       <- setdiff(rv$na, v$point)
        rv$edited   <- union(rv$edited, v$point)
        rv$adjusted <- union(rv$adjusted, v$point)
      }
      invisible(is.null(.fm_extreme_violations(recon())))
    }

    shiny::observeEvent(input$save, {
      shiny::req(rv$A, rv$B)
      if (isTRUE(input$checkextremes)) {
        v <- .fm_extreme_violations(recon())
        if (!is.null(v)) { show_conv_modal(v); return() }
      }
      do_save()
    })

    # 1) remesurer : on ferme, on selectionne le point fautif et on y zoome
    shiny::observeEvent(input$conv_remeasure, {
      shiny::removeModal()
      v <- .fm_extreme_violations(recon())
      if (!is.null(v)) { rv$sel <- v$point[1]; zoom_to_sel() }
    })
    # 2) corriger automatiquement puis enregistrer
    shiny::observeEvent(input$conv_fix, {
      shiny::removeModal()
      ok <- apply_conv_fix()
      if (!isTRUE(ok))
        shiny::showNotification(
          "Convention 3/4 toujours non respectee apres correction : verifiez la saisie.",
          type = "warning", duration = 8)
      do_save()
    })
    # 3) enregistrer tel quel (l'ecart est reel et assume)
    shiny::observeEvent(input$conv_asis, { shiny::removeModal(); do_save() })

    do_save <- function() {
      shiny::req(rv$A, rv$B)
      P <- recon()
      if (is_new()) {                              # nouveaux specimens : autre feuille
        nm <- trimws(as.character(input$new_species %||% ""))
        f  <- basename(cur_photo())
        if (!nzchar(nm) || is.na(f)) {
          shiny::showNotification("Renseignez le nom de l'espece avant d'enregistrer.",
                                  type = "error")
          return()
        }
        journal_write(P, row_key = f, points = save_pts_new, species = nm)
        if (save_new(P)) {
          pending <<- pending + 1L
          flush_xlsx()
          rv$saved <- union(rv$saved, -cur_idx())  # cles negatives : file "new"
          shiny::showNotification(paste("Ajoute a", new_sheet, ":", cur_name()),
                                  type = "message")
          nav(1)
        }
        return()
      }
      journal_write(P, row_key = as.character(cur_name()), points = save_pts,
                    species = as.character(cur_name()))
      r_excel <- cur_row() + 1L                    # +1 pour l'entete
      for (pnum in save_pts) {                     # 1..19, 22, 23, 24, 25
        cx <- col_of(paste0(pnum, "_X")); cy <- col_of(paste0(pnum, "_Y"))
        if (is.na(cx) || is.na(cy)) next        # colonne absente de la feuille -> ignore
        openxlsx::writeData(wb, lm_sheet, round(P[pnum, 1], 3),
                            startCol = cx, startRow = r_excel, colNames = FALSE)
        openxlsx::writeData(wb, lm_sheet, round(P[pnum, 2], 3),
                            startCol = cy, startRow = r_excel, colNames = FALSE)
      }
      # met a jour lm_df EN MEMOIRE pour que revenir sur l'espece dans la meme
      # session recharge bien ce qu'on vient d'enregistrer (24/25 compris).
      rr <- cur_row()
      for (pnum in save_pts) {
        xc <- paste0(pnum, "_X"); yc <- paste0(pnum, "_Y")
        if (xc %in% names(lm_df)) lm_df[rr, xc] <<- round(P[pnum, 1], 3)
        if (yc %in% names(lm_df)) lm_df[rr, yc] <<- round(P[pnum, 2], 3)
      }
      pending <<- pending + 1L
      flush_xlsx()
      rv$saved <- union(rv$saved, cur_row())
      shiny::showNotification(paste("Enregistre :", cur_name()), type = "message")
      nav(1)
    }

    # ecriture manuelle du classeur (le journal, lui, est deja a jour)
    shiny::observeEvent(input$flush, {
      if (pending == 0L) {
        shiny::showNotification("Classeur deja a jour.", type = "message"); return()
      }
      n <- pending
      if (isTRUE(flush_xlsx(force = TRUE)))
        shiny::showNotification(sprintf("Classeur ecrit (%d enregistrement(s)).", n),
                                type = "message")
      else
        shiny::showNotification("Echec de l'ecriture : donnees conservees dans le journal.",
                                type = "error")
      rv$flushstamp <- rv$flushstamp + 1L
    })

    # fin de session (fermeture de l'onglet / arret de l'app) : dernier flush.
    # Filet de securite seulement -- si R est tue brutalement il ne s'execute pas,
    # et c'est precisement pour ce cas que le journal existe.
    session$onSessionEnded(function() {
      try(flush_xlsx(force = TRUE), silent = TRUE)
    })

    output$plot <- shiny::renderPlot({
      if (is.null(rv$img)) { graphics::plot.new()
        graphics::text(.5, .5, "Photo indisponible pour cette espece."); return() }
      op <- graphics::par(mar = c(0,0,0,0)); on.exit(graphics::par(op))
      cx <- if (is.null(rv$cx)) rv$w / 2 else rv$cx
      cy <- if (is.null(rv$cy)) rv$h / 2 else rv$cy
      hw <- (rv$w / 2) / rv$zoom; hh <- (rv$h / 2) / rv$zoom
      cx <- min(max(cx, hw), rv$w - hw); cy <- min(max(cy, hh), rv$h - hh)
      graphics::plot(NA, xlim = c(cx - hw, cx + hw), ylim = c(cy + hh, cy - hh), asp = 1,
                     xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
      graphics::rasterImage(rv$img, 0, rv$h, rv$w, 0)
      if (!is.null(rv$A)) graphics::points(rv$A[1], rv$A[2], pch = 3, col = "cyan", lwd = 3, cex = 2)
      if (!is.null(rv$B)) graphics::points(rv$B[1], rv$B[2], pch = 3, col = "orange", lwd = 3, cex = 2)
      if (!is.null(rv$A) && !is.null(rv$B)) {
        P <- recon()
        # lignes de repere (contour du corps, ventre, verticale de l'oeil, oeil)
        if (isTRUE(input$showlines)) {
          path <- function(pts, ...) { pts <- pts[is.finite(P[pts, 1])]
            if (length(pts) > 1) graphics::lines(P[pts, 1], P[pts, 2], ...) }
          path(c(1, 5, 3, 16, 18, 19, 17, 4, 6, 1), col = "grey30", lwd = 1)      # contour
          path(c(9, 8, 11, 4), col = "grey85", lty = 3, lwd = 1)                  # ventre
          if (all(is.finite(P[c(1, 9), ]))) path(c(1, 9), col = "grey60", lwd = 1) # droite museau (1-9)
          if (all(is.finite(P[c(6, 23), ])))                                       # segment 23-6 (// axe)
            graphics::segments(P[23,1], P[23,2], P[6,1], P[6,2], col = "magenta", lwd = 1.5)
          path(c(5, 13, 7, 14, 6, 8), col = "grey85", lty = 3, lwd = 1)           # verticale oeil
          if (all(is.finite(P[c(7, 13, 14), ]))) {                               # oeil (cercle)
            er <- sqrt(sum((P[13, ] - P[14, ])^2)) / 2; th <- seq(0, 2*pi, length.out = 60)
            graphics::lines(P[7,1] + er*cos(th), P[7,2] + er*sin(th), col = "grey85", lty = 3, lwd = 1)
          }
        }
        pairs <- lapply(.FM_PAIR_SEG, `[[`, "pair")
        cols <- grDevices::hcl.colors(length(pairs), "Dark3")
        for (k in seq_along(pairs)) { ab <- pairs[[k]]
          # Bl : trace le long de l'axe BRISE 1 -> charnieres posees -> 2
          if (names(pairs)[k] == "Bl") {
            ch <- .fm_axis_chain(P)
            graphics::lines(P[ch, 1], P[ch, 2], col = cols[k], lwd = 2)
          } else
            graphics::segments(P[ab[1],1],P[ab[1],2],P[ab[2],1],P[ab[2],2], col = cols[k], lwd = 2) }
        # barre d'echelle 20-21 (mode "new") : tracee en vert, hors du corps
        if (is_new() && all(is.finite(P[.FM_SCALE_PTS, ])))
          graphics::segments(P[20,1], P[20,2], P[21,1], P[21,2],
                             col = "#00a06a", lwd = 3)
        lp <- lm_pts()
        graphics::points(P[lp,1], P[lp,2], pch = 21, bg = "white", cex = 1.2)
        graphics::text(P[lp,1], P[lp,2], lp, pos = 3, cex = .7, col = "yellow")
        # charnieres supplementaires (24,25) posees : dessinees en or
        xh <- setdiff(.FM_HINGES, lp)
        xh <- xh[vapply(xh, function(i) i <= nrow(P) && all(is.finite(P[i, ])), logical(1))]
        if (length(xh)) {
          graphics::points(P[xh,1], P[xh,2], pch = 21, bg = "gold", cex = 1.3)
          graphics::text(P[xh,1], P[xh,2], xh, pos = 3, cex = .7, col = "orange")
        }
        # point actif en rouge (landmarks OU charnieres)
        if (rv$sel %in% c(lp, .FM_HINGES) && rv$sel <= nrow(P) &&
            all(is.finite(P[rv$sel, ])))
          graphics::points(P[rv$sel,1], P[rv$sel,2], pch = 21, bg = "red", cex = 1.7, lwd = 2)
        graphics::legend("topright", names(pairs), col = cols, lwd = 2, bg = "white", cex = .8, ncol = 2)
      }
    })

    # libelle de la photo courante + statut d'enregistrement (mode "new")
    output$new_photo_lab <- shiny::renderUI({
      if (!is_new()) return(NULL)
      f <- basename(cur_photo())
      already <- !is.na(cur_new_row())
      shiny::HTML(sprintf("Photo : <code>%s</code><br>%s",
        if (is.na(f)) "-" else f,
        if (already) "<span style='color:#0a0'>deja dans la feuille (sera reecrite)</span>"
        else "<span style='color:#666'>nouvelle ligne a l'enregistrement</span>"))
    })

    output$rt <- shiny::renderTable({
      shiny::req(rv$A, rv$B); P <- recon(); seg <- cur_seg()
      # Bl = longueur le long de l'axe BRISE 1->...->2 (curviligne, toutes les
      # charnieres posees) ; l'echelle px/unite en decoule et convertit les autres.
      Blpx <- .fm_axis_len_px(P)
      ppu  <- Blpx / seg$Bl
      dd <- function(nm, a, b) {
        px <- if (nm == "Bl") Blpx else sqrt(sum((P[a, ] - P[b, ])^2))
        px / ppu
      }
      cible <- vapply(.FM_PAIR_SEG, function(x) as.numeric(seg[[x$seg]]), numeric(1))
      rec   <- vapply(seq_along(.FM_PAIR_SEG), function(k)
        dd(names(.FM_PAIR_SEG)[k], .FM_PAIR_SEG[[k]]$pair[1], .FM_PAIR_SEG[[k]]$pair[2]),
        numeric(1))
      if (!is_new())
        return(data.frame(segment = names(.FM_PAIR_SEG), cible = cible,
                          reconstruit = rec, row.names = NULL))
      # mode "new" : pas de cible mesuree. `cible` vaut la MEDIANE FISHMORPH du
      # rapport segment/Bl et `mesure` le rapport effectivement digitalise ; la
      # colonne mm n'apparait que si la barre d'echelle 20-21 est posee.
      out <- data.frame(segment = names(.FM_PAIR_SEG),
                        mediane_ratio = cible, ratio_mesure = rec, row.names = NULL)
      mpp <- mmpp_of(P)
      if (is.finite(mpp)) out$mm <- rec * Blpx * mpp
      out
    }, digits = 3)

    output$progress <- shiny::renderUI({
      modelab <- switch(rv$mode,
        correct = "CORRIGER (deja landmarkes)",
        new     = "NOUVELLES PHOTOS (ajout au classeur)",
        "RECONSTRUIRE (sans landmarks)")
      Bl <- cur_seg()$Bl
      scal <- "-"
      if (!is.null(rv$A) && !is.null(rv$B) && is.finite(Bl)) {
        P <- try(recon(), silent = TRUE)
        blpx <- if (!inherits(P, "try-error")) .fm_axis_len_px(P)
                else sqrt(sum((rv$B - rv$A)^2))
        # en mode "new" Bl = 1 (pseudo-segment) : px/unite = longueur du corps en
        # pixels, donc on affiche plutot mm/px (barre 20-21) et Bl en mm si connu.
        scal <- if (is_new()) {
          mpp <- if (inherits(P, "try-error")) NA_real_ else mmpp_of(P)
          if (is.finite(mpp)) sprintf("%.4f mm/px (Bl = %.1f mm)", mpp, blpx * mpp)
          else "barre 20-21 non posee"
        } else sprintf("%.2f px/unite", blpx / Bl)    # echelle sur l'axe brise
      }
      # etat du tampon xlsx : rv$saved / rv$flushstamp servent de declencheurs
      # reactifs (`pending` est une simple variable, non reactive).
      rv$flushstamp
      buf <- if (pending == 0L) "classeur a jour"
             else sprintf("<span style='color:#b36b00'>%d en attente d'ecriture</span>",
                          pending)
      shiny::HTML(sprintf(
        paste0("Mode : <b>%s</b><br><b>%s</b><br>%s %d / %d<br>Enregistrees : %d",
               "<br>Echelle : %s<br>Journal : <b>OK</b> (%s)"),
        modelab, cur_name(), if (is_new()) "Photo" else "Espece",
        rv$qi, length(qrows()), length(rv$saved), scal, buf))
    })

    output$status <- shiny::renderText({
      if (is.null(rv$img))
        return(if (!length(qrows())) "Aucune espece dans ce mode." else "Photo indisponible.")
      lab <- if (rv$sel == 1L) "MUSEAU (LM1)" else if (rv$sel == 2L) "BASE CAUDALE (LM2)"
             else if (rv$sel == 20L) "BARRE D'ECHELLE, debut (LM20)"
             else if (rv$sel == 21L) "BARRE D'ECHELLE, fin (LM21)"
             else paste0("LM", rv$sel)
      intro <- if (rv$mode == "correct")
        paste0("Mode CORRECTION : les 21 landmarks sont recharges du classeur. ",
               "Selectionnez un point (bouton ou clic sur la photo) puis cliquez sa ",
               "nouvelle position ; 'Enregistrer & suivant' reecrit la ligne du classeur.\n")
      else if (rv$mode == "new")
        paste0("Mode NOUVELLES PHOTOS : cette photo n'est pas dans le classeur. ",
               "Aucun segment mesure n'existe -> apres les clics 1 (museau) et 2 ",
               "(base caudale), les points sont pre-places aux PROPORTIONS MEDIANES ",
               "FISHMORPH, a corriger un par un. Verifiez le nom d'espece (panneau ",
               "de gauche) ; 20/21 = barre d'echelle, optionnelle. ",
               "'Enregistrer & suivant' ajoute une ligne a la feuille '", new_sheet, "'.\n")
      else ""
      paste0(intro, "Point actif : ", lab,
             " -> cliquez sa position sur la photo (avance auto).\n",
             if (is.null(rv$A) || is.null(rv$B))
               "Posez d'abord museau (1) puis base caudale (2)."
             # NB : il n'y a PAS de zoom a la molette (aucun handler wheel n'est
             # pose) ; le zoom passe par les boutons +/- du panneau de gauche.
             else paste0("Zoom : boutons + / - (se centre sur le point actif) ; ",
                         "clic droit maintenu = deplacer la vue ; ",
                         "double-clic = vue entiere."))
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server))
}

# -----------------------------------------------------------------------------
# Utilisation
# -----------------------------------------------------------------------------
# library(Rfishmorph)
#
# # (0) SECURITE DES DONNEES. Chaque enregistrement part d'abord dans un journal
# #     append-only (dossier `journal_dir`, un fichier TSV par session, jamais
# #     reecrit) ; le classeur n'est plus qu'un export, ecrit atomiquement et par
# #     lots de `xlsx_flush_every`. Si R plante, rien n'est perdu :
# #        base <- fishmorph_consolidate("FishMORPH/landmark_journal",
# #                                      out_csv = "FishMORPH/landmarks.csv")
# #        qc   <- fishmorph_journal_qc("FishMORPH/landmark_journal")
# #     `qc` liste notamment les points restes a leur position de GRAINE, donc
# #     jamais verifies a l'oeil -- information absente du classeur.
#
# # (1) digitaliser les especes SANS landmarks (comportement historique) :
# launch_fishmorph_digitizer(
#   xlsx_path = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
#   photo_dir = "FishMORPH/Photos utilisées"
# )
#
# # (2) relire / corriger les especes DEJA landmarkees :
# launch_fishmorph_digitizer(
#   xlsx_path = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
#   photo_dir = "FishMORPH/Photos utilisées",
#   mode      = "correct"
# )
#
# # (3) AJOUTER de nouvelles photos (specimens absents du classeur) :
# launch_fishmorph_digitizer(
#   xlsx_path     = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
#   photo_dir     = "FishMORPH/Photos utilisées",
#   new_photo_dir = "FishMORPH/Photos nouvelles",   # <- dossier a digitaliser
#   new_sheet     = "new_specimens",                # <- feuille d'arrivee
#   ruler_mm      = 10,                             # <- longueur de la regle 20-21
#   mode          = "new"
# )
# Deposez les photos dans `new_photo_dir` AVANT de lancer l'app : chaque image du
# dossier devient une entree de la file (plusieurs specimens d'une meme espece
# sont donc possibles, une ligne chacun). Le nom d'espece est pre-rempli depuis le
# nom de fichier et modifiable dans le panneau de gauche. La cle d'une ligne est
# la colonne `photo_file` : revenir sur une photo deja faite recharge ses points
# et REECRIT la meme ligne au lieu d'en ajouter une seconde.
#
# Le bouton "Mode" en haut de l'app bascule entre les trois files a la volee.
# -> ecrit dans FishMORPH/FISHMORPH_PUBLI_9556sp_reconstructed.xlsx
