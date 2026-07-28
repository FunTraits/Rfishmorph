# =============================================================================
# digitizer-app.R
#
# Interactive tool: place the FISHMORPH segments on a species photograph, turn
# them into 21 landmarks (points 1-19 + 22 + 23) and record them in the
# "Global_Landmark" sheet of a COPY of the workbook.
# 23 = derived point (automatic): intersection of the line (1,9) with the line
#      through 6 parallel to the axis (1,2) -> snout -> head-base distance.
#
# Input  : FISHMORPH_PUBLI_9556sp.xlsx  (sheet 1 "Global_segments",
#           sheet 2 "Global_Landmark") + the "Photos utilisees" folder.
# Output : the copy "..._reconstructed.xlsx", sheet 2 filled row by row.
#
# Three modes (the "Queue" selector, or the `mode=` argument):
#   * "reconstruct" : queue of species WITHOUT landmarks (to be digitized).
#   * "correct"     : queue of species ALREADY landmarked (review/correction);
#                     the 21 points are reloaded from the workbook and can be
#                     moved, then "Save & next" rewrites the row.
#   * "new"         : queue of NEW PHOTOGRAPHS (folder `new_photo_dir`), absent
#                     from the workbook. No segment exists: the points are
#                     seeded from the MEDIAN PROPORTIONS of the FISHMORPH set
#                     (see .FM_NEW_RATIOS) after the snout (1) and caudal-base
#                     (2) clicks, then placed by hand. The species name is
#                     typed in a field (pre-filled from the file name). LM20/21
#                     (scale bar, optional) join the entry order and give
#                     mm_per_px. The record goes to the `new_sheet` sheet
#                     ("new_specimens"), appending a row (or rewriting it if
#                     the photograph is already there, key = the photo_file
#                     column).
#
# Flow (reconstruct mode):
#   1. The app builds the queue of species WITHOUT landmarks, WITH segments and
#      WITH a matching photograph.
#   2. For the current species: click the SNOUT (LM1) then the CAUDAL BASE
#      (LM2) -> axis, position and scale (px/unit) deduced from Bl.
#   3. The 20 points are pre-placed from the segments (lengths locked); they
#      are refined with the sliders (the parameters the segments leave free)
#      and, where needed, by repositioning a given point with a click.
#   4. "Save & next" checks the extreme-point convention (3 = the
#      most dorsal point, 4 = the most ventral: see .fm_extreme_violations),
#      then writes the X/Y into the copy and moves on to the next.
#
# Calibration of the segments (checked against the rows already digitized):
#   Euclidean pairs = Bl(1,2) Bd(3,4) Hd(5,6) Ed(13,14) Jl(1,15)
#   PFl(10,12) CPd(16,17) CFd(18,19); and for the heights, the *2* columns are
#   used: Eh2->(7,8)  Mo2->(1,9)  PFi2->(10,11)
#   (raw Eh/Mo/PFi are NOT distances between pairs in this file).
#
# Coordinate convention: like the existing rows, everything is recorded in
# IMAGE pixels (Y downwards: the top of the body has the smaller Y).
#
# Status: PROTOTYPE. Dependencies: shiny, openxlsx, jpeg, png.
# =============================================================================

# points recorded in sheet 2 (order of the _X/_Y columns)
# 23 = derived point (automatic): intersection of the line (1,9) with the line
# through 6 parallel to the axis (1,2) -> segment 23-6 parallel to (1,2).
.FM_LM_PTS <- c(1:19, 22L, 23L)

# Version of the digitizing tool, traced in EVERY journal row: it is what will
# make it possible, in two years' time, to know with which geometric logic a
# given specimen was digitized.
# A FUNCTION and not a constant: packageVersion() would fail at installation
# time, when this file is evaluated while the package is not yet installed.
.fm_app_version <- function()
  tryCatch(as.character(utils::packageVersion("Rfishmorph")),
           error = function(e) "dev")

# The capture layer (append-only journal) lives in R/journal.R and is part of
# the same package: there is nothing left to load by hand.

# click entry order: the broken axis first, snout -> 22 -> 24 -> caudal, then
# the points to place by hand (auto-advance); .FM_DERIVED = derived points.
.FM_CLICK_ORDER <- c(1L, 22L, 24L, 2L, 3L, 4L, 7L, 5L, 6L, 13L, 14L, 10L, 12L, 16L, 17L, 18L, 19L)
# derived (automatic): 8/9/11 = belly points; 15 is seeded. 10/12 stay in the
# entry loop; as long as they have not been clicked they follow 11 (PFi/PFl
# preserved), and once placed or corrected they stay where you put them.
# 22 = HINGE (no longer "derived"): shown between 1 and 2 and made active when
# a species is opened in correction mode (see seed_from_existing).
.FM_DERIVED     <- c(8L, 9L, 11L, 15L, 23L)

# MODE "new" (new photographs): same order + the scale bar 20/21 at the end.
# 20/21 are OPTIONAL (they may be left unplaced) and serve only to compute
# mm_per_px = ruler_mm / dist(20,21).
.FM_SCALE_PTS       <- c(20L, 21L)
.FM_CLICK_ORDER_NEW <- c(.FM_CLICK_ORDER, .FM_SCALE_PTS)
.FM_LM_PTS_NEW      <- c(.FM_LM_PTS, .FM_SCALE_PTS)

.fm_next <- function(cur, order = .FM_CLICK_ORDER) {
  i <- match(cur, order)
  if (is.na(i)) return(cur)
  if (i >= length(order)) return(3L)  # after the last -> back to the 1st anatomical (3)
  order[i + 1L]
}

# --- median FISHMORPH proportions ("new" mode, no segments) ------------------
# Medians of segment/Bl computed on FishMORPH_seg.csv (n = 6,492 to 7,706
# species depending on the segment; only values > 0 are kept). They serve ONLY
# as a seed: after the clicks on 1 (snout) and 2 (caudal base), each point is
# pre-placed at the median proportion of the body, then corrected with a click.
# No length is therefore "locked" in this mode, unlike "reconstruct" where the
# measured segments constrain the pairs.
.FM_NEW_RATIOS <- c(Bd = 0.2480, Hd = 0.1382, Eh2 = 0.1372, Mo2 = 0.1152,
                    PFi2 = 0.0745, PFl = 0.1829, Ed = 0.0589, Jl = 0.0559,
                    CPd = 0.1055, CFd = 0.2593)

# pseudo-segments for .fm_place(): Bl = 1 -> ppu = Blpx, so each length is
# ratio * Blpx pixels. Exactly equivalent to passing measured segments.
.fm_new_segments <- function() c(list(Bl = 1), as.list(.FM_NEW_RATIOS))

# species name proposed from the photograph file name:
# "Abramis_brama_2.JPG" -> "Abramis brama" (genus capitalised, epithet lower)
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

# HINGE points (broken axis). 22 is a recorded landmark; 24 and 25 are EXTRA
# hinges, entry aids for the few strongly curved specimens -- they are recorded
# too (their columns are created in the workbook at start-up), because they
# define the frames in which every convention was applied. Placed where needed,
# they allow up to 4 axis segments (1 -> ... -> 2). Left unplaced, they stay
# "in line" (no effect).
.FM_HINGES <- c(22L, 24L, 25L)

# ordered chain of the broken axis: 1, then the hinges ACTUALLY PLACED, sorted
# by their position along the chord 1->2, then 2.
.fm_axis_chain <- function(P) {
  fin <- function(i) i <= nrow(P) && all(is.finite(P[i, ]))
  hs <- .FM_HINGES[vapply(.FM_HINGES, fin, logical(1))]
  if (length(hs) > 1 && fin(1) && fin(2)) {
    uc <- P[2, ] - P[1, ]
    hs <- hs[order(vapply(hs, function(i) sum((P[i, ] - P[1, ]) * uc), numeric(1)))]
  }
  c(1L, hs, 2L)
}
# length (px) along the broken axis (sum of the segments of the chain)
.fm_axis_len_px <- function(P) {
  ch <- .fm_axis_chain(P)
  if (length(ch) < 2) return(NA_real_)
  sum(vapply(seq_len(length(ch) - 1L),
             function(k) sqrt(sum((P[ch[k + 1L], ] - P[ch[k], ])^2)), numeric(1)))
}

# segment column to use for each pair of landmarks
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

# Free parameters (those the segments do not identify) and their defaults.
# The f (axial position) and o (dorsal share) values were reset on the MEDIANS
# of the species already digitized in the file (17 species) -- the former
# values (o_Hd=0.85, o_PF=0.90) placed the head and the pectoral fin too high.
# o_PF is negative because the pectoral insertion lies below the body midline.
.fm_defaults <- function() list(
  f_Bd = 0.47, o_Bd = 0.50, f_Hd = 0.10, o_Hd = 0.43,
  f_eye = 0.10, o_eye = 0.82, f_PF = 0.25, o_PF = -0.69,
  ang_PFl = 35, ang_Jl = 20, f_CP = 0.93, o_CP = 0.52,
  f_CF = 1.15, o_CF = 0.47
)

# --- robust indexing of the photographs --------------------------------------
# maps a normalised species name -> file path
.fm_photo_index <- function(photo_dir) {
  files <- list.files(photo_dir, pattern = "\\.(jpg|jpeg|png|JPG|JPEG|PNG)$",
                      full.names = TRUE)
  norm <- function(x) {
    x <- tools::file_path_sans_ext(basename(x))
    x <- sub("\\s*\\d+$", "", x)                       # a trailing " 1", " 2"
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

# --- point 23 (derived) ------------------------------------------------------
# Intersection of the line (1,9) with the line through 6 parallel to the axis
# IN FRONT (1->22, the hinge; the chord 1->2 if 22 is undefined). The segment
# 23-6 is therefore parallel to 1->22 and measures the axial distance from the
# snout (1) to the base of the head (6). Frame-agnostic (image pixels are fine).
.fm_point23 <- function(P) {
  if (!all(is.finite(P[c(1L, 2L, 6L, 9L), ]))) return(c(NA_real_, NA_real_))
  d1 <- P[9L, ] - P[1L, ]               # direction of the line (1,9)
  # 23-6 parallel to the HEAD segment (1 -> 22; falls back to 24 then 2)
  fin2 <- function(i) i <= nrow(P) && all(is.finite(P[i, ]))
  htip <- if (fin2(22L)) P[22L, ] else if (fin2(24L)) P[24L, ] else P[2L, ]
  d2 <- htip - P[1L, ]                   # direction of the HEAD axis (1 -> 22)
  cr <- d1[1] * d2[2] - d1[2] * d2[1]
  if (!is.finite(cr) || abs(cr) < 1e-9) return(c(NA_real_, NA_real_))
  w <- P[6L, ] - P[1L, ]
  a <- (w[1] * d2[2] - w[2] * d2[1]) / cr
  as.numeric(P[1L, ] + a * d1)
}

# --- placing the 20 points from the segments ---------------------------------
# segments: named list (values of the columns Bl,Bd,Hd,Eh2,Mo2,PFi2,PFl,Ed,
#            Jl,CPd,CFd); A,B: the snout / caudal-base clicks (image px);
# params: the free parameters. Returns a 22x2 matrix (rows = points 1..22;
# only .FM_LM_PTS are filled in), Y downwards.
.fm_place <- function(segments, A, B, params = list()) {
  p  <- utils::modifyList(.fm_defaults(), params)
  gv <- function(nm) as.numeric(segments[[nm]])
  Bl <- gv("Bl")
  Blpx <- sqrt(sum((B - A)^2))
  ppu  <- Blpx / Bl                 # pixels per segment unit
  u    <- (B - A) / Blpx            # antero-posterior axis
  up   <- c(u[2], -u[1])            # "dorsal" normal (screen upwards, Y-)
  cm   <- function(v) v * ppu
  st   <- function(f) A + Bl * ppu * f * u

  # 25 rows: 1..23 + the extra hinges 24, 25 (left NA here)
  P <- matrix(NA_real_, nrow = 25, ncol = 2, dimnames = list(NULL, c("X", "Y")))
  set <- function(i, xy) P[i, ] <<- xy
  vseg <- function(f, L, o) { s <- st(f); list(top = s + cm(L) * o * up,
                                               bot = s - cm(L) * (1 - o) * up) }
  set(1, A); set(2, B)
  v <- vseg(p$f_Bd, gv("Bd"), p$o_Bd); set(3, v$top); set(4, v$bot)
  v <- vseg(p$f_Hd, gv("Hd"), p$o_Hd); set(5, v$top); set(6, v$bot)
  eye <- st(p$f_eye)
  set(8, eye - cm(gv("Hd")) * p$o_eye * up)      # body underside below the eye
  set(7, P[8, ] + cm(gv("Eh")) * up)             # centre of the eye (Eh2)
  set(13, P[7, ] + cm(gv("Ed") / 2) * up)
  set(14, P[7, ] - cm(gv("Ed") / 2) * up)
  set(9, P[1, ] - cm(gv("Mo")) * up)             # body underside below the snout (Mo2)
  v <- vseg(p$f_PF, gv("PFi"), p$o_PF); set(10, v$top); set(11, v$bot)  # PFi2
  d <- cos(-p$ang_PFl * pi / 180) * u + sin(-p$ang_PFl * pi / 180) * up
  set(12, P[10, ] + cm(gv("PFl")) * d)
  d <- cos(-p$ang_Jl * pi / 180) * u + sin(-p$ang_Jl * pi / 180) * up
  set(15, P[1, ] + cm(gv("Jl")) * d)
  v <- vseg(p$f_CP, gv("CPd"), p$o_CP); set(16, v$top); set(17, v$bot)
  v <- vseg(p$f_CF, gv("CFd"), p$o_CF); set(18, v$top); set(19, v$bot)
  set(22, st(0.5))                               # curvature point on the axis
  set(23, .fm_point23(P))                         # derived: (1,9) x (// axis through 6)
  P
}

# --- correction geometrique : conventions FISHMORPH -------------------------
# Applies, in the frame of the body axis (1-2) -- hence valid even if the
# photograph is tilted -- the 5 conventions of correct_geometry_conventions():
#   perpendicular to the axis: 9 aligned on 1, 4 on 3, 11 on 10 (same AXIAL
#     coordinate); eye group {5,13,7,14,6,8}: same axial coordinate (the
#     median) -> vertical of the eye; belly {9,8,11,4}: same NORMAL
#     coordinate (the median) -> a line parallel to the axis.
.fm_apply_conventions <- function(P) {
  A <- P[1, ]; B <- P[2, ]
  L <- sqrt(sum((B - A)^2)); if (!is.finite(L) || L == 0) return(P)
  u <- (B - A) / L; n <- c(u[2], -u[1])
  fin <- function(i) all(is.finite(P[i, ]))
  ax <- vapply(1:22, function(i) if (fin(i)) sum((P[i, ] - A) * u) else NA_real_, numeric(1))
  no <- vapply(1:22, function(i) if (fin(i)) sum((P[i, ] - A) * n) else NA_real_, numeric(1))
  # perpendiculars (axial coordinate of the point set on that of the anchor)
  if (fin(1) && fin(9))  ax[9]  <- ax[1]
  if (fin(3) && fin(4))  ax[4]  <- ax[3]
  if (fin(10) && fin(11)) ax[11] <- ax[10]
  # vertical of the eye: axial coordinate = median of the group
  eg <- c(5, 13, 7, 14, 6, 8); if (any(is.finite(ax[eg]))) ax[eg] <- stats::median(ax[eg], na.rm = TRUE)
  # belly line: normal coordinate = median of the group
  hg <- c(9, 8, 11, 4);        if (any(is.finite(no[hg]))) no[hg] <- stats::median(no[hg], na.rm = TRUE)
  for (i in 1:22) if (is.finite(ax[i]) && is.finite(no[i])) P[i, ] <- A + ax[i] * u + no[i] * n
  if (nrow(P) >= 23) P[23, ] <- .fm_point23(P)   # 23 is derived
  P
}

# --- correction through the package's canonical functions --------------------
# The app works in the arbitrary frame of the photograph; standardize_geometry()
# and correct_geometry_conventions() require the axis 1-2 to be horizontal. We
# therefore build a landmarks object (with a dummy scale bar 20-21 along the
# axis) and apply
#   standardize_geometry(orient = FALSE)  [rescale + rotation, a similarity]
#   -> correct_geometry_conventions()     [the 5 canonical conventions]
# then invert the similarity through points 1-2, which the conventions never
# move, to come back into the frame of the photograph.
#
# Equivalent to .fm_apply_conventions() to machine precision. This version is
# kept as the REFERENCE: should the two ever diverge, it is this one that
# prevails, since it goes through the package's canonical code rather than the
# local reimplementation used for interactive editing (which is faster).
#
# NOTE: since Rfishmorph 0.2.0 these functions live in THIS package. The call
# used to go through intraitR::, which created an external dependency for code
# already ported here.
.fm_correct_via_package <- function(P) {
  ax <- P[2, ] - P[1, ]; axlen <- sqrt(sum(ax^2))
  if (!is.finite(axlen) || axlen == 0) return(P)
  R <- P[1:22, , drop = FALSE]
  R[20, ] <- P[1, ]                       # dummy scale bar: origin
  R[21, ] <- P[1, ] + ax / axlen * axlen  # ... and end along the axis (length ~ Bl)
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
  Pout[23, ] <- .fm_point23(Pout)   # 23 recomputed (it does not exist in the package frame)
  Pout
}

# --- conventions under CONSTRAINED EDITING -----------------------------------
# Perpendicular/parallel conventions applied live, each driven by an editable
# point (the last one moved in the group, or a default driver), so that points
# can be moved while the rules still hold.
#
# BROKEN AXIS with 3 segments and FIXED anchors (hinges 22 and 24; 25 = Bl
# curvature only, with no convention):
#   HEAD    = segment 1 -> 22   : Mo (1-9), eye/Hd vertical {5,13,7,14,6,8}, 23-6
#   MIDDLE  = segment 22 -> 24 : Bd (3-4), pectoral PFi (10-11) and PFl (10-12)
#   CAUDALE = segment 24 -> 2   : pedoncule (16-17), nageoire caudale (18-19)
# If a hinge is not placed, graceful fallback to the chord 1->2 (a straight
# droit / correction) : comportement retro-compatible.
# --- THE EXTREME-POINT CONVENTION (3 = back, 4 = belly) ----------------------
# FISHMORPH defines Bd as the MAXIMUM body depth: 3 must therefore be the most
# DORSAL point and 4 the most VENTRAL point of the body outline. A 5 (top of
# the head) above 3, or an 11 (belly at the pectoral fin) below 4, is an entry
# error that under-estimates Bd.
#
# Points EXCLUDED from the comparison:
#   8, 9, 11 : DERIVED ventral points. They are computed FROM 4 (the belly
#           line): testing whether 4 is the lowest point against them is
#           circular. Measured on the 1,036 digitized T-26 specimens: including
#           them flags 20.6 % of the batch, 198 of the 213 alerts bearing on
#           8, 9 or 11, with a median overshoot of 0.5 % of Bl -- belly-line
#           noise, not a Bd error. Excluding them, 1.5 % is flagged (16
#           specimens), 12 of which are the 5-above-3 case, with a median
#           overshoot of 7.8 % of Bl. The rate is then STABLE from 0.003 to
#           0.02 Bl: what remains is gross error, cleanly separated from the
#           noise, and not an artefact of the threshold;
#   16-19 : caudal peduncle and caudal fin -- outside the body outline by
#           definition (an explicit requirement), the caudal fin often
#           exceeding Bd;
#   12, 15 : tips of the pectoral fin and of the jaw -- appendages, which
#           legitimately exceed the outline;
#   20, 21 : scale bar; 23: derived point; 24, 25: entry hinges.
# Still compared with 3/4: 1, 2, 5, 6, 7, 10, 13, 14, 22 -- the landmarks that
# are MEASUREMENTS independent of the body outline.
.FM_EXTREME_EXCLUDE <- c(8L, 9L, 11L, 12L, 15L, 16L, 17L, 18L, 19L,
                         20L, 21L, 23L, 24L, 25L)

# Default tolerance, as a FRACTION of the chord 1-2: 0.3 % of Bl (about 6 px
# for a fish of 2,000 px). Below that, the discrepancy is click noise.
.FM_EXTREME_TOL <- 0.003

# Absolute FLOOR, in pixels. On a small image the relative tolerance falls
# below click noise (0.003 * 600 px = 1.8 px) and a 2 px overshoot would be
# enough to raise the alert -- that is noise, not an error. Set to 5 px on the
# T-26 data: compliant specimens top out at -0.4 px of overshoot (p98) and the
# smallest REAL discrepancy is 11.8 px. Anywhere between a 1 px and an 8 px
# floor the number of flagged specimens is unchanged (16): the band is empty,
# so 5 px sits in the middle of it and costs no detection.
.FM_EXTREME_FLOOR <- 5

# --- THE EYE VERTICAL, IN ORDER ----------------------------------------------
# The six points 5, 13, 7, 14, 6, 8 are placed on ONE vertical by convention
# (.fm_apply_conventions aligns their axial coordinate). That convention says
# nothing about their ORDER along that vertical -- and the order is anatomy, not
# a choice: top of the head, top of the eye, centre of the eye, bottom of the
# eye, bottom of the head, body underside. Reading them from the back downwards
# gives 5 > 13 > 7 > 14 > 6 > 8.
#
# The two failures this catches are invisible in the coordinate table and
# survive every other check, because each pair stays internally consistent:
#   * 5 no longer the most dorsal of the group -> Hd (5-6) is under-measured
#     exactly as Bd is when 3 is not the most dorsal point;
#   * two points swapped -- typically 13 and 14 (the eye clicked bottom-first),
#     or 7 outside the 13-14 pair -- which leaves Ed (13-14) unchanged in
#     LENGTH while Eh (7-8), the eye HEIGHT above the belly, silently refers to
#     the wrong edge of the eye.
# Point 8 is derived from the belly line, so the 6/8 pair tests a measured point
# against a computed one: a 6 below the belly line is a genuine entry error.
#
# Settled on the data rather than by argument, as the Bd rule was. Over the
# 4,151 species already digitized in the workbook (median Bl = 485 px, same
# tolerance), the expected order holds for 99.5 % of them, and each inversion is
# rare enough to be an error rather than noise:
#   8 above 6  : 22 specimens (0.53 %)
#   13 above 5 : 10 (0.24 %) -- the same 10 as "5 does not top the group"
#   6 above 14 :  2 (0.05 %)
#   14 above 7 :  1 (0.02 %)
# A convention that 99.5 % of a hand-digitized corpus already satisfies is a
# convention, not a preference; the residue is worth looking at one by one.
.FM_EYE_ORDER <- c(5L, 13L, 7L, 14L, 6L, 8L)   # dorsal -> ventral

# --- COINCIDENT POINTS: a measurement of zero ---------------------------------
# Some segments are legitimately ZERO on some species, and a zero is a
# measurement like any other -- neither a missing value nor a placement error.
# The FISHMORPH heights read from the ventral profile vanish when the structure
# sits ON that profile (`OGp = 0` for a mouth at the bottom, `PFv = 0` for a
# pectoral fin inserted on the belly); the bottom of the head can be exactly the
# body underside; an eye reaching the top of the head puts 5 on 13.
#
# NOTHING IS DELETED. Both points keep a position, both are drawn on the
# photograph and both are written to the workbook -- one simply takes the
# coordinates of the other, so the segment between them measures zero. A
# coincidence is a measurement; an absence is NA, and the two must not be
# confused downstream.
#
# In "reconstruct" mode a zero already comes from the workbook: .fm_place()
# reads Mo2 = 0 and puts 9 on 1 by construction. These rules are for the two
# other queues -- "new", where the points are seeded from medians, and
# "correct", where a specimen is being repaired -- and they are RE-APPLIED at
# the end of recon(), after .fm_constrain() and after point 23 is recomputed,
# because the constrained editing re-derives the ventral points on the belly
# line and would undo them at the next click.
#
# Which point moves is a protocol decision, not an aesthetic one, and it is not
# the same for every rule. For the mouth the fixed point is 1, the snout, an
# anatomical landmark that must not move -- and 23, built on the line (1, 9),
# is undefined once 9 sits on 1, so it follows 1 rather than becoming NA. For
# the two ventral rules it is the BELLY LINE that holds: 8 and 11 are its
# intersections with the eye and the pectoral verticals, so the head bottom (6)
# and the fin insertion (10) come onto them rather than the reverse -- the
# ventral profile is a global fit, steadier than a single click. For the eye at
# the top of the head, 5 (the head outline) comes onto 13, since moving 13 would
# change Ed, a measurement in its own right.
# A rule is a list of MOVES, each `c(from, to)`: one rule can have to move more
# than one point, and not necessarily onto the same partner. `6 = 8` is the case
# that forces this. Point 23 is the intersection of the line (1, 9) with the
# line through 6 parallel to the head axis; the belly line {9, 8, 11} is itself
# parallel to that axis by convention. So the moment 6 sits ON the belly line,
# the parallel through 6 IS the belly line, and its intersection with (1, 9) is
# 9 itself: 23 = 9 follows, it is not an extra convention. Checked numerically,
# including on a tilted photograph, where the derived 23 lands exactly on 9.
.FM_COLLAPSE <- list(
  Mo  = list(moves = list(c(9L, 1L), c(23L, 1L)),
             label = "Mo = 0 (mouth on the belly)",
             tip = "9 and 23 take the coordinates of 1: mouth height nil, OGp = 0"),
  Hd6 = list(moves = list(c(6L, 8L), c(23L, 9L)),
             label = "6 = 8 (head bottom on the belly)",
             tip = paste("6 takes the coordinates of 8: the head ends on the",
                         "ventral profile -- and 23 follows 9, since the",
                         "parallel through 6 is then the belly line itself")),
  PFi = list(moves = list(c(10L, 11L)),
             label = "PFi = 0 (pectoral on the belly)",
             tip = "10 takes the coordinates of 11: insertion on the ventral profile, PFv = 0"),
  EyeTop = list(moves = list(c(5L, 13L)),
                label = "5 = 13 (eye at the head top)",
                tip = "5 takes the coordinates of 13: the eye reaches the dorsal profile")
)

# Apply the active rules, move by move and in order. A move whose reference is
# not placed is skipped: a zero is only meaningful once the point it is measured
# from exists. Order matters when several rules are on -- with both `Mo` and
# `6 = 8`, 9 goes onto 1 first, so 23 then follows 9 to the same place.
.fm_apply_collapse <- function(P, active) {
  if (is.null(P) || !length(active)) return(P)
  for (nm in intersect(active, names(.FM_COLLAPSE)))
    for (mv in .FM_COLLAPSE[[nm]]$moves) {
      if (mv[1] > nrow(P) || mv[2] > nrow(P)) next
      if (all(is.finite(P[mv[2], ]))) P[mv[1], ] <- P[mv[2], ]
    }
  P
}
.fm_collapse_points <- function(active) {
  if (!length(active)) return(integer(0))
  unlist(lapply(.FM_COLLAPSE[intersect(active, names(.FM_COLLAPSE))],
                function(r) vapply(r$moves, function(m) m[1], integer(1))),
         use.names = FALSE)
}

# labels of the points, for the application's messages
.FM_PT_LABELS <- c(
  "1" = "snout", "2" = "caudal-fin base", "3" = "back (Bd upper)",
  "4" = "belly (Bd lower)", "5" = "top of the head (Hd upper)",
  "6" = "bottom of the head (Hd lower)", "7" = "centre of the eye",
  "8" = "belly below the eye", "9" = "belly below the snout",
  "10" = "pectoral-fin insertion", "11" = "belly at the pectoral fin",
  "12" = "pectoral-fin tip", "13" = "top of the eye",
  "14" = "bottom of the eye", "15" = "jaw tip",
  "16" = "peduncle upper", "17" = "peduncle lower", "18" = "caudal upper",
  "19" = "caudal lower", "20" = "scale bar (start)", "21" = "scale bar (end)",
  "22" = "body hinge", "23" = "derived point 23")
.fm_pt_label <- function(i) {
  l <- unname(.FM_PT_LABELS[as.character(i)])
  ifelse(is.na(l), "", paste0(" (", l, ")"))
}

# Coordinates in the frame of the BODY: abscissa along the axis 1-2, height
# perpendicular to that axis. We reason on that height and not on the raw image
# Y, because a tilted photograph would distort the comparison; when the fish is
# horizontal the two coincide exactly (up to the sign).
#
# `sgn` gives the DORSAL side, deduced from the relative position of 3 and 4 and
# not from an image convention: the test therefore holds head left or head
# right, photograph flipped, or the "Flip dorsal/ventral" box ticked.
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

# Violations of the extreme-point convention. Returns NULL when everything is
# compliant, otherwise a data.frame: `point` (3 or 4), `culprit` (the point
# overshooting it), `delta` (the overshoot in pixels) and `kind` ("extreme").
.fm_extreme_violations <- function(P, tol_frac = .FM_EXTREME_TOL) {
  g <- .fm_body_frame(P); if (is.null(g)) return(NULL)
  tol  <- max(.FM_EXTREME_FLOOR, tol_frac * g$L)
  cand <- setdiff(seq_len(nrow(P)), c(3L, 4L, .FM_EXTREME_EXCLUDE))
  cand <- cand[is.finite(g$no[cand])]
  if (!length(cand)) return(NULL)
  out <- list()
  d <- g$sgn * (g$no[cand] - g$no[3])           # overshoot on the DORSAL side
  k <- which.max(d)
  if (d[k] > tol) out[[length(out) + 1L]] <-
    data.frame(point = 3L, culprit = cand[k], delta = unname(d[k]),
               kind = "extreme", stringsAsFactors = FALSE)
  d <- g$sgn * (g$no[4] - g$no[cand])           # overshoot on the VENTRAL side
  k <- which.max(d)
  if (d[k] > tol) out[[length(out) + 1L]] <-
    data.frame(point = 4L, culprit = cand[k], delta = unname(d[k]),
               kind = "extreme", stringsAsFactors = FALSE)
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# Violations of the order along the eye vertical (see .FM_EYE_ORDER). Two things
# are tested, and they are not the same statement:
#   1. point 5 is the most DORSAL of the whole group -- the Hd analogue of the
#      3/4 rule, reported against whichever point overshoots it most;
#   2. every CONSECUTIVE pair is in order, which catches a local swap (13/14
#      inverted, 7 outside the eye) that (1) cannot see.
# Returns NULL, or the same schema as .fm_extreme_violations() with
# `kind = "order"`: `point` is the one that should sit ABOVE, `culprit` the one
# that is found above it, `delta` the inversion in pixels.
.fm_eye_order_violations <- function(P, tol_frac = .FM_EXTREME_TOL) {
  g <- .fm_body_frame(P); if (is.null(g)) return(NULL)
  tol <- max(.FM_EXTREME_FLOOR, tol_frac * g$L)
  pts <- .FM_EYE_ORDER[.FM_EYE_ORDER <= nrow(P)]
  h   <- g$sgn * g$no[pts]                      # height, dorsal side positive
  ok  <- is.finite(h)
  if (sum(ok) < 2L) return(NULL)
  out <- list()

  # 1. the top of the head must top the group
  if (ok[1]) {
    d <- h[-1] - h[1]                           # positive = above point 5
    d[!is.finite(d)] <- -Inf
    k <- which.max(d)
    if (is.finite(d[k]) && d[k] > tol) out[[length(out) + 1L]] <-
      data.frame(point = pts[1], culprit = pts[-1][k], delta = unname(d[k]),
                 kind = "order", stringsAsFactors = FALSE)
  }
  # 2. consecutive pairs, on the points actually placed: a missing landmark
  #    must not break the chain, it must be stepped over.
  seq_ok <- pts[ok]; h_ok <- h[ok]
  for (i in seq_len(length(seq_ok) - 1L)) {
    d <- h_ok[i + 1L] - h_ok[i]                 # positive = the lower one is above
    if (d > tol) out[[length(out) + 1L]] <-
      data.frame(point = seq_ok[i], culprit = seq_ok[i + 1L], delta = unname(d),
                 kind = "order", stringsAsFactors = FALSE)
  }
  if (!length(out)) return(NULL)
  out <- do.call(rbind, out)
  # the "5 tops the group" rule and the first consecutive pair can name the same
  # inversion twice; one line per (point, culprit) is enough.
  out[!duplicated(out[, c("point", "culprit")]), , drop = FALSE]
}

# Every convention checked on save, in one table. The order is deliberate: the
# extremes come first, because they are the ones the automatic correction can
# repair -- an inverted pair cannot be repaired by moving a point, only by
# measuring it again.
.fm_convention_violations <- function(P, tol_frac = .FM_EXTREME_TOL) {
  v <- rbind(.fm_extreme_violations(P, tol_frac),
             .fm_eye_order_violations(P, tol_frac))
  if (is.null(v) || !nrow(v)) NULL else v
}

# Automatic correction: 3 (resp. 4) takes the HEIGHT of the point overshooting
# it, keeping its abscissa along the axis. The convention "3-4 perpendicular to
# the axis" is therefore preserved, and only Bd changes (it grows).
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
  # setp modifies P in the environment of .fm_constrain, through <<-.
  frame <- function(o, tip) {
    d <- tip - o; Ld <- sqrt(sum(d^2))
    d <- if (is.finite(Ld) && Ld > 0) d / Ld else (B - A) / Lab
    nn <- c(d[2], -d[1])
    list(ax   = function(i) sum((P[i, ] - o) * d),
         no   = function(i) sum((P[i, ] - o) * nn),
         setp = function(i, a, b) P[i, ] <<- o + a * d + b * nn)
  }
  # Three axis segments with FIXED ANCHORS (22 and 24 = hinges; 25 only serves
  # to curve Bl, with no convention):
  #   HEAD    -> segment 1 -> 22   (Mo 1-9, eye/Hd {5,13,7,14,6,8}, 23-6)
  #   MILIEU  -> segment 22 -> 24  (Bd 3-4, pectorale PFi 10-11 et PFl 10-12)
  #   CAUDALE -> segment 24 -> 2   (pedoncule 16-17, nageoire caudale 18-19)
  # Graceful fallback when a hinge is not placed (straight fish / correction):
  p22 <- if (fin(22)) P[22, ] else NULL
  p24 <- if (fin(24)) P[24, ] else NULL
  head_tip <- if (!is.null(p22)) p22 else if (!is.null(p24)) p24 else B
  mid_org  <- if (!is.null(p22)) p22 else A
  mid_tip  <- if (!is.null(p24)) p24 else B
  tail_org <- if (!is.null(p24)) p24 else if (!is.null(p22)) p22 else A
  fr_head <- frame(A, head_tip)       # 1 -> 22
  fr_mid  <- frame(mid_org, mid_tip)  # 22 -> 24
  fr_tail <- frame(tail_org, B)       # 24 -> 2

  driver <- function(grp, default) {          # the VALID (finite) driver of the group
    o <- overridden[overridden %in% grp]; o <- o[vapply(o, fin, logical(1))]
    if (length(o)) return(o[length(o)])
    if (fin(default)) return(default)
    pres <- grp[vapply(grp, fin, logical(1))]
    if (length(pres)) pres[1] else NA_integer_
  }
  # segment perpendicular to the axis `fr`: the driver keeps everything, the
  # other keeps its height (no) and resets its abscissa (ax) on the driver.
  perp <- function(fr, a, b, default) {
    if (!(fin(a) && fin(b))) return(invisible())
    dr <- driver(c(a, b), default); ot <- if (dr == a) b else a
    fr$setp(ot, fr$ax(dr), fr$no(ot))
  }
  # --- TETE (segment 1->22) ---
  perp(fr_head, 1, 9, 1L)       # Mo  : snout (1) drives, belly (9) follows
  eye <- c(5, 13, 7, 14, 6, 8); de <- driver(eye, 7L)   # eye/Hd vertical
  if (!is.na(de)) { ae <- fr_head$ax(de)
    for (i in setdiff(eye, de)) if (fin(i)) fr_head$setp(i, ae, fr_head$no(i)) }
  if (fin(7) && fin(13) && fin(14)) {          # 13/14 symetriques, diametre Ed conserve
    h <- abs(fr_head$no(13) - fr_head$no(14)) / 2
    fr_head$setp(13, fr_head$ax(7), fr_head$no(7) + h)
    fr_head$setp(14, fr_head$ax(7), fr_head$no(7) - h)
  }
  # --- MILIEU (segment 22->24) : Bd + pectorale ---
  perp(fr_mid, 3, 4, 4L)        # Bd  : belly (4) drives, back (3) follows
  perp(fr_mid, 10, 11, 11L)     # PFi : belly (11) drives, insertion (10) follows
  # --- CAUDALE (segment 24->2) ---
  perp(fr_tail, 16, 17, 16L)    # pedoncule caudal vertical
  perp(fr_tail, 18, 19, 18L)    # nageoire caudale verticale

  # --- BELLY LINE, BROKEN at point 11 ---
  #   head   : 9, 8, 11 aligned PARALLEL to 1->22
  #   milieu : 4, 11    alignes PARALLELE a 22->24
  # Default pivot = 11 (the junction). Each VENTRAL point (9, 8, 4) is brought
  # its line by changing ONLY its height (its abscissa is kept -> it stays
  # perpendicular to the axis); the DORSAL partner point (1, 7, 3) does NOT move.
  # So 1-9 = mouth->body underside, 7-8 = eye->body underside, 3-4 = depth.
  # A point moved by hand (overridden) is not moved.
  # 4 = the PRECISE point (the master). The chain derives from 4:
  #   1) 11 aligns on 4  -> line 11-4 PARALLEL to 22-24 (11 keeps its abscissa)
  #   2) 8,9 align on 11 -> line 9-8-11 PARALLEL to 1-22
  # The order matters: 11 is derived from 4 BEFORE 8,9 are derived from 11.
  # Each point keeps its abscissa -> it stays perpendicular to its axis.
  belly_line <- function(fr, pivot, movers) {
    if (is.na(pivot) || !fin(pivot)) return(invisible())
    nb <- fr$no(pivot)
    for (m in movers) if (m != pivot && fin(m)) fr$setp(m, fr$ax(m), nb)
  }
  mid_piv  <- if (fin(4)) 4L else if (fin(11)) 11L else NA_integer_
  belly_line(fr_mid,  mid_piv,  c(11L))            # 11 <- 4  (11-4 // 22-24)
  head_piv <- if (fin(11)) 11L else if (fin(9)) 9L else NA_integer_
  belly_line(fr_head, head_piv, c(8L, 9L, 11L))    # 8,9 <- 11 (9-8-11 // 1-22)

  # PFl (10->12) LAST (once 10 has its final position): PARALLEL to
  # 22->24 et LONGUEUR FIXE = PFl si connue (12 = 10 + PFl*u_mid). Si TU as deplace
  # 12 by hand (overridden), it stays free.
  if (fin(10) && fin(12) && !(12L %in% overridden)) {
    if (is.finite(pfl_px)) fr_mid$setp(12, fr_mid$ax(10) + pfl_px, fr_mid$no(10))
    else                   fr_mid$setp(12, fr_mid$ax(12), fr_mid$no(10))
  }
  P
}

# =============================================================================
# Application
# =============================================================================
#' Interactive tool for digitizing the FISHMORPH landmarks
#'
#' Opens a 'shiny' application that places the 21 FISHMORPH landmarks on
#' specimen photographs, along three working queues that can be switched on the
#' fly (see `mode`). Every record goes first to an append-only journal
#' ([fm_journal_open()]), then to the workbook; after a brutal interruption,
#' [fishmorph_consolidate()] recovers all the work.
#'
#' The photographs stay LOCAL: they are never copied into the package nor into
#' the workbook, only their file name and their size in pixels are recorded. It
#' is `photo_dir` and `new_photo_dir` that make the link.
#'
#' @section Extreme-point check on save:
#' FISHMORPH defines `Bd` as the MAXIMUM body depth: point 3 must therefore be
#' the most dorsal and point 4 the most ventral. When the box
#' *"Check 3/4 (extremes)"* is ticked (the default), "Save & next"
#' checks that convention before anything is written and, if it is breached,
#' offers to **measure again** (the offending point becomes active and the view
#' centres on it), to **correct automatically** (3, resp. 4, takes the height of
#' the point overshooting it, keeping its position along the axis: `Bd` grows,
#' the 3-4 perpendicularity is preserved) or to **save without correcting**.
#'
#' Heights are measured perpendicular to the body axis 1-2 -- a tilted
#' photograph therefore does not distort the test -- and the dorsal side is
#' deduced from the relative position of 3 and 4, which makes the check valid
#' whatever the orientation (head left or right, photograph flipped, the
#' "Flip dorsal/ventral" box ticked). EXCLUDED from the comparison are the
#' caudal peduncle and fin (16-19), which exceed the body by definition, the
#' appendage tips (12 pectoral, 15 jaw) and the DERIVED ventral points
#' (8, 9, 11), computed from 4 itself -- including them would flag 20.6 per
#' cent of the T-26 specimens for belly-line noise, against 1.5 per cent of
#' outright errors once they are excluded; the scale bar (20, 21), the derived
#' point (23) and the hinges (24, 25) are not outline points. The tolerance is
#' 0.003 times the body length (that is 3 pixels for a fish of 1,000 pixels),
#' below which the discrepancy is click noise.
#'
#' Points that were snapped carry the status `"adjusted"` in the journal,
#' distinct from `"placed"`: the automatic correction stays traceable specimen
#' by specimen.
#'
#' @section Coincident points, a measurement of zero:
#' A bar under the photograph declares the segments that are ZERO on the species
#' in view. A zero is a measurement like any other -- neither a missing value nor
#' a placement error -- and the FISHMORPH ratios are defined to take it:
#' `OGp = 0` for a mouth opening on the ventral profile, `PFv = 0` for a
#' pectoral fin inserted on the belly. Four rules are offered: `Mo = 0` (9, and
#' 23, take the coordinates of 1), `6 = 8` (the bottom of the head is the body
#' underside, and 23 follows 9), `PFi = 0` (10 takes the coordinates of 11) and
#' `5 = 13` (an eye reaching the top of the head).
#'
#' That 23 follows 9 under `6 = 8` is a consequence, not an extra convention.
#' Point 23 is the intersection of the line (1, 9) with the line through 6
#' parallel to the head axis, and the belly line {9, 8, 11} is itself parallel
#' to that axis. The moment 6 sits ON the belly line, that parallel IS the belly
#' line, and its intersection with (1, 9) is 9.
#'
#' Nothing is deleted. Both points keep a position, both are drawn on the
#' photograph and both are written to the workbook; one simply takes the
#' coordinates of the other, so the segment between them measures zero. A
#' coincidence is a measurement, an absence is `NA`, and the two must not be
#' confused downstream.
#'
#' In `"reconstruct"` mode a zero already comes from the workbook, since the
#' points are laid out from the measured segments. The rules are for `"new"`,
#' where the points are seeded from medians, and `"correct"`, where a specimen
#' is being repaired. They are applied at the very end of the reconstruction,
#' after the constrained editing and after point 23 is rebuilt, which would
#' otherwise re-derive the ventral points on the belly line at the next click.
#'
#' Which point moves is a protocol decision and is not the same for every rule.
#' For the mouth the fixed point is 1, the snout, an anatomical landmark that
#' must not move -- and 23, built on the line (1, 9), is undefined once 9 sits
#' on 1, so it follows 1 rather than becoming `NA`. For the two ventral rules
#' the belly line holds: 8 and 11 are its intersections with the eye and the
#' pectoral verticals, so the head bottom and the fin insertion come onto them
#' rather than the reverse. For the eye at the top of the head, 5 comes onto 13,
#' since moving 13 would change `Ed`, a measurement in its own right. Points
#' moved by a rule take the `"adjusted"` status in the journal, and the
#' declarations are reset for every species.
#'
#' The same box also checks the ORDER of the eye vertical. The six points 5, 13,
#' 7, 14, 6, 8 are placed on one vertical by the FISHMORPH conventions, and
#' anatomy fixes their order along it, from the back downwards: top of the head,
#' top of the eye, centre of the eye, bottom of the eye, bottom of the head,
#' body underside. Two failures follow from that and from nothing else -- 5 no
#' longer topping the group, which under-measures `Hd` exactly as a misplaced 3
#' under-measures `Bd`; and a local swap, typically 13 and 14 when the eye is
#' clicked bottom-first, or 7 outside the 13-14 pair. Neither is visible in a
#' coordinate table: each pair stays internally consistent, `Ed` (13-14) keeps
#' its length, while `Eh` (7-8) silently refers to the wrong edge of the eye.
#' An inversion is reported but **never corrected automatically**: moving a
#' point to satisfy the order would invent a measurement rather than repair one.
#'
#' @param xlsx_path Path of the master workbook (2 sheets).
#' @param photo_dir Photograph folder (stays local).
#' @param out_path  Path of the output copy. NULL -> "<master>_reconstructed.xlsx"
#'   in the same folder. The copy is created if absent; otherwise work resumes
#'   on it (species already recorded are excluded from the queue).
#' @param seg_sheet,lm_sheet Sheet names.
#' @param new_sheet Sheet the NEW specimens are appended to ("new" mode).
#'   Created (with the headers of `lm_sheet`) if it does not exist.
#' @param new_photo_dir Folder of the new specimens' photographs ("new" mode).
#'   Every image in the folder forms the queue. It may not exist: the "new"
#'   mode is then simply unavailable.
#' @param ruler_mm Real length (mm) of the scale bar digitized by points 20 and
#'   21 in "new" mode. Can be changed in the app, specimen by specimen.
#' @param journal_dir Folder of the append-only JOURNAL (see [fm_journal_open()]).
#'   Every record is appended to it BEFORE any workbook write: it is the source
#'   of truth, and it survives a crash.
#'   [fishmorph_consolidate()] rebuilds the database at any time.
#' @param operator Operator identifier, traced in the journal and in the session
#'   file name. NULL -> the system user.
#' @param xlsx_flush_every Number of records between two writes of the
#'   workbook. The workbook weighs several Mb and is REWRITTEN IN FULL every
#'   time: taking it out of the digitizing loop removes both the risk and the
#'   wait. Unwritten changes stay in memory (and are therefore readable within
#'   the session), are written at the end of the session, by the dedicated
#'   button, and are in the journal in any case. 1 = the historical behaviour
#' @param mode Starting queue: "reconstruct" (species WITHOUT landmarks, to be
#'   digitized from the segments), "correct" (species ALREADY landmarked, to be
#'   reviewed/corrected: the 21 points are reloaded from the workbook) or
#'   "new" (new photographs from `new_photo_dir`, appended to `new_sheet`).
#'   Switchable at any moment through the "Queue" selector in the app. If the
#'   requested queue is empty, the app starts on another one.
#' @return Invisibly `NULL`; called for its side effect (it launches the app).
#' @seealso [fishmorph_consolidate()] to read the journal back,
#'   [fishmorph_build_db()] to build the database from it,
#'   [launch_fishmorph_space()] to explore the morphological space.
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
    stop("fishmorph_landmark_store.R is not loaded: source() that file first ",
         "(it carries the safety journal and the consolidation).",
         call. = FALSE)
  xlsx_flush_every <- max(1L, as.integer(xlsx_flush_every))
  for (pkg in c("shiny", "openxlsx")) if (!requireNamespace(pkg, quietly = TRUE))
    stop("Package '", pkg, "' is required.", call. = FALSE)
  if (!file.exists(xlsx_path)) stop("Classeur introuvable : ", xlsx_path, call. = FALSE)
  if (!dir.exists(photo_dir)) stop("Photograph folder not found: ", photo_dir, call. = FALSE)
  if (is.null(out_path)) {
    # if the "_reconstructed" file is the one being opened, we rewrite INSIDE
    # it (no new copy at every launch); otherwise the copy is created/completed.
    out_path <- if (grepl("_reconstructed\\.xlsx$", xlsx_path)) xlsx_path
                else sub("\\.xlsx$", "_reconstructed.xlsx", xlsx_path)
  }
  if (!file.exists(out_path)) file.copy(xlsx_path, out_path)

  shiny <- asNamespace("shiny")

  # --- reading the two sheets from the COPY (so work can be resumed) ---------
  # the raw headers are read separately: read.xlsx may rename the columns that
  # start with a digit ("1_X" -> "X1_X"), which would break the writing.
  read_sheet <- function(sheet) {
    hdr <- as.character(openxlsx::read.xlsx(out_path, sheet = sheet,
                                            colNames = FALSE, rows = 1))
    # ONLY the trailing empty headers are cut: removing an inner empty one would
    # shift `cols = seq_along(hdr)` and misalign the column names.
    ok <- !is.na(hdr) & nzchar(hdr)
    if (any(ok)) hdr <- hdr[seq_len(max(which(ok)))] else hdr <- character(0)
    # the full column range is forced: otherwise read.xlsx drops the entirely
    # empty trailing columns (e.g. 22_X / 22_Y, rarely digitized), which
    # misaligns the names.
    df <- openxlsx::read.xlsx(out_path, sheet = sheet, colNames = FALSE,
                              startRow = 2, cols = seq_along(hdr))
    if (is.null(df) || !nrow(df))                    # empty sheet (headers only)
      df <- as.data.frame(matrix(NA, nrow = 0, ncol = length(hdr)))
    else if (ncol(df) < length(hdr))                 # securite : re-padding
      df[, (ncol(df) + 1):length(hdr)] <- NA
    names(df) <- hdr
    list(df = df, hdr = hdr)
  }
  s1 <- read_sheet(seg_sheet); seg_df <- s1$df
  s2 <- read_sheet(lm_sheet);  lm_df  <- s2$df; lm_hdr <- s2$hdr
  col_of <- function(nm) match(nm, lm_hdr)   # uses the up-to-date lm_hdr
  wb <- openxlsx::loadWorkbook(out_path)   # loaded once, written in memory

  # adds the missing columns to a sheet (header written at the end), and creates
  # them in the in-memory data.frame too. Returns the updated header.
  ensure_cols <- function(sheet, hdr, need) {
    miss <- need[!need %in% hdr]
    if (!length(miss)) return(hdr)
    for (j in seq_along(miss))
      openxlsx::writeData(wb, sheet, miss[j], startCol = length(hdr) + j,
                          startRow = 1, colNames = FALSE)
    fm_save_workbook_atomic(wb, out_path)                    # persist the headers
    message("Colonnes ajoutees a '", sheet, "' : ", paste(miss, collapse = ", "))
    c(hdr, miss)
  }

  # columns of the extra hinges 24/25: created if absent, so that these points
  # are RECORDED like the others (22/23 already have their columns).
  hinge_cols <- c("24_X", "24_Y", "25_X", "25_Y")
  new_lm_hdr <- ensure_cols(lm_sheet, lm_hdr, hinge_cols)
  # rep(...) and not a bare NA: on an empty sheet (0 rows) df[[cc]] <- NA fails
  for (cc in setdiff(new_lm_hdr, lm_hdr)) lm_df[[cc]] <- rep(NA_real_, nrow(lm_df))
  lm_hdr <- new_lm_hdr                             # updated header -> col_of finds them
  # points recorded / reloaded: the landmarks + the hinges 22/23/24/25
  save_pts <- c(.FM_LM_PTS, 24L, 25L)

  # --- sheet of the NEW specimens ("new" mode) -------------------------------
  # Created with the headers of lm_sheet if absent. The columns the app needs
  # are then guaranteed: the 21 landmarks (already there if the fields match),
  # the hinges 24/25, the scale bar 20/21, and three traceability columns:
  # photo_file (the row KEY), ruler_mm, mm_per_px.
  new_exists <- new_sheet %in% openxlsx::getSheetNames(out_path)
  if (!new_exists) openxlsx::addWorksheet(wb, new_sheet)
  # sheet absent OR present but without a header row: the header of lm_sheet is
  # written into it. An existing sheet with its headers is left alone.
  hdr0 <- if (new_exists) {
    h <- suppressWarnings(as.character(openxlsx::read.xlsx(
      out_path, sheet = new_sheet, colNames = FALSE, rows = 1)))
    h[!is.na(h) & nzchar(h)]
  } else character(0)
  if (!length(hdr0)) {
    openxlsx::writeData(wb, new_sheet, t(as.matrix(lm_hdr)), colNames = FALSE)
    message("Headers written into '", new_sheet, "' (a copy of '", lm_sheet, "').")
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
  # everything as character: new_df only serves to find the row of a photograph
  # and to count the rows; the real values are written by writeData. Avoids
  # assignment errors (a string into a logical column of an empty sheet).
  if (ncol(new_df)) new_df[] <- lapply(new_df, as.character)

  # photograph index + species keys
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

  # TWO queues, chosen at launch (the `mode` argument) and switchable with the
  # "Queue" selector in the app:
  #   * reconstruct : species WITHOUT landmarks, WITH segments and a photograph
  #   * correct     : species ALREADY landmarked, WITH a photograph (to review /
  #                   correct; the 21 points are reloaded from the workbook, NOT
  #                   reconstructed from the segments)
  q_recon <- which(lm_missing &
                     lm_df$.key %in% rownames(seg_by_key) & has_photo)
  q_corr  <- which(!lm_missing & has_photo & !is.na(lm_df$.key))

  # the "new" queue: every image in the new-photographs folder. One entry = ONE
  # photograph (and not one species): several specimens of the same species are
  # therefore possible, each on its own row of `new_sheet`.
  new_photos <- if (dir.exists(new_photo_dir))
    sort(list.files(new_photo_dir, full.names = TRUE,
                    pattern = "\\.(jpe?g|png|gif|bmp|tiff?)$", ignore.case = TRUE))
  else character(0)
  q_new <- seq_along(new_photos)

  if (!length(q_recon) && !length(q_corr) && !length(q_new))
    stop("No usable species (none to reconstruct, none to correct with a photograph, ",
         "no new photograph in '", new_photo_dir, "').", call. = FALSE)
  # if the requested queue is empty, we fall back to the first non-empty one
  qlen_of <- function(m) switch(m, reconstruct = length(q_recon),
                                correct = length(q_corr), new = length(q_new), 0L)
  if (!qlen_of(mode)) {
    alt <- c("reconstruct", "correct", "new")
    alt <- alt[vapply(alt, function(m) qlen_of(m) > 0, logical(1))][1]
    message("File '", mode, "' vide -> demarrage en mode '", alt, "'."); mode <- alt
  }
  message(sprintf("Queues: %d species to reconstruct, %d to correct, %d new photograph(s).",
                  length(q_recon), length(q_corr), length(q_new)))

  # --- append-only journal: the source of truth ------------------------------
  # Opened BEFORE any entry. Every record goes there first; the workbook is now
  # only an export, written atomically and in batches.
  jr <- fm_journal_open(journal_dir, operator = operator,
                        app_version = .fm_app_version())
  pending <- 0L                          # records not yet written to the xlsx
  # writes the workbook once enough records have accumulated (or when forced).
  # On failure NOTHING is overwritten and the operator is told: the journal is
  # already written, so no data is lost -- fishmorph_consolidate() finds it.
  flush_xlsx <- function(force = FALSE) {
    if (pending == 0L) return(invisible(FALSE))
    if (!force && pending < xlsx_flush_every) return(invisible(FALSE))
    ok <- tryCatch({ fm_save_workbook_atomic(wb, out_path); TRUE },
                   error = function(e) { warning("Writing the workbook failed: ",
                     conditionMessage(e), " -- the data stays in the journal ",
                     "(", jr$path, ").", call. = FALSE); FALSE })
    if (ok) pending <<- 0L
    invisible(ok)
  }

  # choices for the direct-access field (value = position in the current queue)
  goto_of <- function(rows) stats::setNames(seq_along(rows), lm_df$Genus.species[rows])
  choices_recon <- goto_of(q_recon); choices_corr <- goto_of(q_corr)
  choices_new   <- stats::setNames(seq_along(new_photos), basename(new_photos))

  # ROBUST image reader: the extension lies often (~7% of the .jpg files are in
  # fact GIF/PNG/BMP), so the REAL format is detected from the magic bytes and
  # routed to the right reader. JPEG/PNG through jpeg/png (fast); GIF/BMP/TIFF
  # tout format non natif via magick (ImageMagick) converti en tableau [H,W,3].
  read_img <- function(path) {
    sig <- tryCatch(readBin(path, "raw", n = 8L), error = function(e) raw(0))
    is_jpeg <- length(sig) >= 2 && sig[1] == as.raw(0xFF) && sig[2] == as.raw(0xD8)
    is_png  <- length(sig) >= 8 &&
      all(sig[1:8] == as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A)))
    if (is_jpeg && requireNamespace("jpeg", quietly = TRUE)) return(jpeg::readJPEG(path))
    if (is_png  && requireNamespace("png",  quietly = TRUE)) return(png::readPNG(path))
    # everything else (GIF/BMP/TIFF, or a mislabelled .jpg) -> magick, which
    # RE-ENCODES to a temporary PNG then read by png::readPNG (this avoids any
    # manual reshape of the array, the source of the "striped" image).
    if (requireNamespace("magick", quietly = TRUE) &&
        requireNamespace("png", quietly = TRUE)) {
      im  <- magick::image_read(path)
      tmp <- tempfile(fileext = ".png"); on.exit(unlink(tmp), add = TRUE)
      magick::image_write(im, tmp, format = "png")
      return(png::readPNG(tmp))
    }
    # last resort: try jpeg then png (it may fail cleanly)
    out <- tryCatch(jpeg::readJPEG(path), error = function(e)
             tryCatch(png::readPNG(path), error = function(e2) NULL))
    if (is.null(out))
      stop("Format d'image non lisible (", toupper(tools::file_ext(path)),
           " differs from its extension). Install the 'magick' package.",
           call. = FALSE)
    out
  }

  # --- UI --------------------------------------------------------------------
  # right-button drag on the photograph pans the view (deltas sent to Shiny)
  pan_js <- shiny::HTML(paste(
    "(function(){var dg=false,lx=0,ly=0,adx=0,ady=0,c=0,raf=null;",
    "function el(){return document.getElementById('plot');}",
    "function flush(){raf=null;if(adx===0&&ady===0)return;Shiny.setInputValue('pan',{dx:adx,dy:ady,n:++c},{priority:'event'});adx=0;ady=0;}",
    "document.addEventListener('contextmenu',function(e){var m=el();if(m&&m.contains(e.target))e.preventDefault();});",
    "document.addEventListener('mousedown',function(e){var m=el();if(m&&m.contains(e.target)&&e.button===2){dg=true;lx=e.clientX;ly=e.clientY;e.preventDefault();}});",
    "document.addEventListener('mousemove',function(e){if(!dg)return;var m=el();if(!m)return;var r=m.getBoundingClientRect();adx+=(e.clientX-lx)/r.width;ady+=(e.clientY-ly)/r.height;lx=e.clientX;ly=e.clientY;if(!raf)raf=requestAnimationFrame(flush);});",
    "document.addEventListener('mouseup',function(e){if(e.button===2){dg=false;if(!raf)raf=requestAnimationFrame(flush);}});",
    "})();", sep = "\n"))
  # bslib is only an appearance layer: without it the application is identical,
  # just plainer. No feature depends on it.
  has_bslib <- requireNamespace("bslib", quietly = TRUE) &&
    utils::packageVersion("bslib") >= "0.5.0"

  app_css <- paste0(
    ".irs{margin-bottom:2px}",
    # ---- toolbars ----------------------------------------------------------
    # One button style for the whole app, defined here rather than borrowed from
    # whatever Bootstrap happens to be loaded: the app must look the same with
    # and without bslib. Buttons carry their ROLE in their weight -- one filled
    # primary (Save), one amber (Mark NA, which destroys a measurement), the
    # rest quiet outlines -- because a row where everything shouts reads as a
    # row where nothing is important.
    ".toolbar{display:flex;flex-wrap:wrap;align-items:center;gap:6px;",
    "margin-bottom:8px;}",
    ".toolbar .form-group{margin-bottom:0;}",
    ".toolbar .selectize-control{margin-bottom:0;}",
    ".toolbar .btn{height:32px;padding:0 12px;font-size:13px;line-height:30px;",
    "border:1px solid #d1d5db;background:#fff;color:#374151;border-radius:7px;",
    "box-shadow:none;transition:background .12s,border-color .12s;}",
    ".toolbar .btn:hover{background:#f3f4f6;border-color:#9ca3af;}",
    ".toolbar .btn:active,.toolbar .btn:focus{outline:none;",
    "box-shadow:0 0 0 3px rgba(37,99,235,.15);}",
    ".toolbar .btn-primary{background:#2563eb;border-color:#2563eb;color:#fff;",
    "font-weight:600;}",
    ".toolbar .btn-primary:hover{background:#1d4ed8;border-color:#1d4ed8;}",
    ".toolbar .btn-warning{background:#f59e0b;border-color:#f59e0b;color:#fff;",
    "font-weight:600;}",
    ".toolbar .btn-warning:hover{background:#d97706;border-color:#d97706;}",
    # Related actions touch, so the eye reads them as one control.
    ".tbgroup{display:inline-flex;}",
    ".tbgroup .btn{border-radius:0;margin:0;border-right-width:0;}",
    ".tbgroup .btn:first-child{border-top-left-radius:7px;",
    "border-bottom-left-radius:7px;}",
    ".tbgroup .btn:last-child{border-top-right-radius:7px;",
    "border-bottom-right-radius:7px;border-right-width:1px;}",
    ".tbsep{width:1px;height:22px;background:#e5e7eb;margin:0 3px;flex:0 0 auto;}",
    ".tbhint{font-size:11.5px;color:#9ca3af;line-height:1.3;max-width:340px;}",
    ".toolbar .selectize-input{min-height:32px;height:32px;padding:4px 10px;",
    "border-radius:7px;border-color:#d1d5db;}",
    # a denser side panel: the tabs already separate the groups, so the vertical
    # rhythm inside a tab can be tighter
    ".sidetabs .tab-content{padding-top:10px;}",
    ".sidetabs .form-group{margin-bottom:10px;}",
    ".sidetabs .shiny-input-container{width:100% !important;}",
    ".sidetabs .help-block{font-size:11.5px;line-height:1.35;color:#6b7280;}",
    ".sidetabs .nav-link{padding:5px 9px;font-size:12.5px;}",
    # the queue selector at the head of the panel: boxed, it reads as the state
    # of the session rather than as one more control
    ".modebar{background:#f8fafc;border:1px solid #e5e7eb;border-radius:8px;",
    "padding:6px 10px 0 10px;margin-bottom:10px;}",
    ".modebar .form-group{margin-bottom:4px;}",
    ".modebar .control-label{font-size:12px;color:#6b7280;margin-bottom:2px;}",
    ".progressbox{background:#f8fafc;border:1px solid #e5e7eb;border-radius:8px;",
    "padding:8px 10px;font-size:13px;line-height:1.5;}",
    ".sessionbar{font-size:12px;color:#6b7280;padding:2px 0 8px 0;",
    "border-bottom:1px solid #e5e7eb;margin-bottom:10px;}",
    ".sessionbar code{font-size:11.5px;color:#374151;background:#f3f4f6;",
    "padding:1px 5px;border-radius:4px;}",
    # The landmark bar is the real navigation of the application. One row, never
    # wrapped: the buttons share the width (flex:1 1 0), so a point keeps its
    # so a point keeps its place on screen whatever the window size. A wrap
    # would turn the glance into a search.
    ".lmrow{display:flex;flex-wrap:nowrap;gap:3px;align-items:stretch;",
    "overflow-x:auto;margin-bottom:6px;padding-bottom:2px;}",
    ".lmrow .lmbtn{flex:1 1 0;min-width:30px;padding:7px 0;font-size:14px;",
    "line-height:1.1;text-align:center;border:1px solid #d1d5db;",
    "border-radius:7px;cursor:pointer;transition:filter .12s;}",
    ".lmrow .lmbtn:hover{filter:brightness(0.94);}",
    # A gap between the sections of the bar -- axis, anatomical run, derived and
    # hinges, scale bar. Twenty-odd identical buttons in a row is a list; four
    # groups is a map, and a point is found in the group it belongs to.
    ".lmgap{flex:0 0 14px;}",
    # floor under the photograph: a narrower device draws nothing useful
    "#plot{min-width:360px;min-height:360px;}",
    ".collapsebar{margin:8px 0 10px 0;padding:6px 10px;background:#fffbeb;",
    "border:1px solid #fde68a;border-radius:8px;font-size:13px;}",
    ".collapsebar .form-group{margin-bottom:0;}",
    ".collapsebar .checkbox-inline{margin-right:14px;font-size:12.5px;}",
    "#set_na{font-weight:600;}",
    ".app-title{font-family:'Inter','SF Pro Display','Segoe UI Variable',",
    "'Helvetica Neue',system-ui,-apple-system,'Segoe UI',Roboto,sans-serif;",
    "letter-spacing:-0.015em;}",
    ".app-title-name{font-weight:800;}",
    ".app-title-sub{font-weight:600;opacity:0.62;}")

  head_tags <- shiny::tags$head(shiny::tags$script(pan_js),
                                shiny::tags$style(shiny::HTML(app_css)))

  app_title <- shiny::tags$span(
    class = "app-title",
    shiny::tags$span(class = "app-title-name", "FishMORPH"),
    shiny::tags$span(class = "app-title-sub",
                     " — segments vers landmarks, digitalisation guidee"))

  # a card when bslib is there, a bordered div otherwise. `fill = FALSE` on
  # purpose: a filling card negotiates its height with its siblings, and the
  # photograph above loses the argument -- that is how a 620 px plot ends up in
  # peripherique plus petit que ses propres marges.
  card_box <- function(title, ...) {
    if (has_bslib)
      bslib::card(bslib::card_header(title), bslib::card_body(..., gap = "6px"),
                  fill = FALSE)
    else
      shiny::div(class = "well", style = "padding:10px;",
                 shiny::tags$strong(title), ...)
  }

  # --- panneau lateral : un onglet par rythme d'usage --------------------------
  # The settings are not touched at the same rhythm -- once per specimen
  # (identity, scale), once per photograph (flips), once per session (seeding
  # sliders, checks) -- and stacking them in one column put the most used ones
  # below the least used ones.
  side_tabs <- shiny::tabsetPanel(
    id = "sidetab", type = if (has_bslib) "pills" else "tabs",

    shiny::tabPanel(
      "Specimen",
      shiny::conditionalPanel(
        "input.mode == 'new'",
        shiny::textInput("new_species", "Species name (Genus species)", ""),
        shiny::helpText("Pre-filled from the photograph file name; correct it if",
                        "needed. It is this value that is written into the",
                        "Genus.species column of the sheet of new",
                        "specimens."),
        shiny::numericInput("ruler_mm", "Scale bar 20-21: real length (mm)",
                            value = ruler_mm, min = 0, step = 1),
        shiny::helpText("Optional. Place points 20 and 21 at the two ends of the",
                        "reference (a ruler, a label): mm_per_px = real length /",
                        "their distance in pixels. Left unplaced, mm_per_px stays",
                        "NA and the coordinates stay in pixels."),
        shiny::uiOutput("new_photo_lab")),
      shiny::conditionalPanel(
        "input.mode != 'new'",
        shiny::helpText("Les champs d'identite ne servent qu'en mode",
                        "\"New photographs\" mode: elsewhere, the species is the",
                        "one of the workbook row.")),
      shiny::hr(),
      shiny::strong("Edition"),
      shiny::checkboxInput("flipdorsal", "Flip dorsal/ventral", FALSE),
      shiny::checkboxInput("correct",
        "Enforce the conventions (constrained editing)", FALSE),
      shiny::helpText("Moving a point then propagates the FISHMORPH conventions",
                      "to the points that depend on it: segment 3-4",
                      "perpendicular to the axis, eye group on one vertical,",
                      "belly line aligned. Unticked, each point moves alone.")),

    shiny::tabPanel(
      "Display",
      shiny::checkboxInput("showlines", "Reference lines (outline/eye/belly)", TRUE),
      shiny::checkboxInput("fastdisp", "Fast display (lightened photograph)", TRUE),
      shiny::radioButtons("flip_mode", "Flip the photograph (+ landmarks)",
        c("Aucun" = "none", "Horizontal" = "h", "Vertical" = "v", "180" = "hv"),
        selected = "none", inline = TRUE),
      shiny::radioButtons("flip_disp", "Flip the photograph ONLY (landmarks fixed)",
        c("Aucun" = "none", "Horizontal" = "h", "Vertical" = "v", "180" = "hv"),
        selected = "none", inline = TRUE),
      shiny::helpText("The second option flips ONLY the display of the",
                      "photograph: the landmarks (and the record) do not move.",
                      "Useful when the loaded points are mirrored relative to the",
                      "photograph. It persists from one species to the next.")),

    shiny::tabPanel(
      "Checks",
      shiny::checkboxInput("checkextremes",
        "Check the conventions on save (3/4 extremes, eye vertical)", TRUE),
      shiny::helpText("On save, checks that 3 is the most DORSAL point and 4 the",
                      "most VENTRAL one (heights measured perpendicular to the",
                      "body axis). Excluded: the caudal fin (16-19), the",
                      "appendage tips (12, 15) and the derived ventral points",
                      "(8, 9, 11). On a breach, offers to measure again or to",
                      "correct automatically."),
      shiny::helpText("Also checks the ORDER of the eye vertical -- 5, 13, 7,",
                      "14, 6, 8 from the back downwards -- and that 5 tops the",
                      "group. An inversion (the eye clicked bottom-first, 7",
                      "outside 13-14, 5 below 13) leaves every pair internally",
                      "consistent, so Ed keeps its length while Hd or Eh refers",
                      "to the wrong point. It is never corrected automatically:",
                      "moving a point to satisfy the order would invent a",
                      "measurement."),
      shiny::hr(),
      shiny::strong("Workbook and journal"),
      shiny::uiOutput("io_info"),
      shiny::actionButton("flush", "Write the workbook", class = "btn-primary"),
      shiny::helpText("The workbook is rewritten every", xlsx_flush_every,
                      "record(s), atomically. The journal is written at EVERY",
                      "record and is the source of truth:",
                      "fishmorph_consolidate() reconstruit tout a partir de lui.")),

    shiny::tabPanel(
      "Seed",
      shiny::helpText("These sliders only set the STARTING POSITION of the points",
                      "the segments do not constrain (position along the body,",
                      "top/bottom share, fin and jaw angles). As soon as you",
                      "click a point, your click replaces the seed; they are",
                      "useful mostly to rough things out before clicking."),
      shiny::sliderInput("f_Bd", "Bd position", 0, 1, .47, .01),
      shiny::sliderInput("o_Bd", "Bd part dorsale", 0, 1, .50, .01),
      shiny::sliderInput("f_Hd", "Hd position", 0, 1, .10, .01),
      shiny::sliderInput("o_Hd", "Hd part dorsale", 0, 1, .43, .01),
      shiny::sliderInput("f_eye", "Oeil position", 0, 1, .10, .01),
      shiny::sliderInput("o_eye", "Eye height (from the body underside)", 0, 1.5, .82, .01),
      shiny::sliderInput("f_PF", "Pectorale position", 0, 1, .25, .01),
      shiny::sliderInput("o_PF", "Pectorale part dorsale", -1, 1, -.69, .01),
      shiny::sliderInput("f_CP", "Pedoncule position", .5, 1, .93, .01),
      shiny::sliderInput("ang_PFl", "PFl angle", 0, 90, 35, 1),
      shiny::sliderInput("ang_Jl", "Jl angle", -30, 90, 20, 1)))

  # The queue selector at the head of the side panel: it decides what the whole
  # session is doing -- which species are offered and what
  # "Save & next" means -- so it belongs to the state of the session,
  # rangee d'actions par specimen ou il etait a un bouton de "Save".
  side_panel <- shiny::div(
    class = "sidetabs",
    shiny::div(class = "modebar",
      shiny::radioButtons("mode", "Queue",
        c("To reconstruct" = "reconstruct", "Correct existing" = "correct",
          "New photographs" = "new"), selected = mode, inline = FALSE)),
    shiny::uiOutput("progress"), shiny::br(), side_tabs)

  main_panel <- shiny::tagList(
    # what the session IS, on one line: the paths are declared at the console,
    # they are therefore displayed and not editable.
    shiny::div(class = "sessionbar", shiny::uiOutput("session_info")),
    # --- action bar, right above the photograph -------------------------------
    # Everything done once per specimen on one row, where the eye already is:
    # the queue, the saving. Nothing here forces a trip back down to the side
    # panel in the middle of an entry.
    shiny::div(
      class = "toolbar",
      shiny::div(class = "tbgroup",
        shiny::actionButton("prev", "\u2039 Previous"),
        shiny::actionButton("nextsp", "Next \u203a")),
      shiny::div(class = "tbsep"),
      shiny::actionButton("save", "Save & next", class = "btn-primary"),
      shiny::actionButton("skip", "Skip"),
      shiny::div(class = "tbsep"),
      shiny::div(style = "min-width:260px;",
        shiny::selectizeInput("goto_species", NULL, choices = NULL,
          selected = NULL, width = "260px",
          options = list(placeholder = "Jump to a species...")))),
    # --- active-point bar -----------------------------------------------------
    # "Mark NA" acts on the point under the cursor: its place is against the
    # landmark bar, not against "Save & next" where a slip of one
    # button saved the specimen.
    shiny::div(
      class = "toolbar",
      shiny::actionButton("set_na", "Mark NA", class = "btn-warning"),
      shiny::div(class = "tbsep"),
      shiny::div(class = "tbgroup",
        shiny::actionButton("zoom_in", "Zoom +"),
        shiny::actionButton("zoom_out", "Zoom \u2212"),
        shiny::actionButton("zoom_reset", "Whole view")),
      shiny::div(class = "tbhint",
        "Right-click and drag to pan \u00b7 double-click for the whole view \u00b7",
        "the zoom centres on the active point")),
    shiny::uiOutput("lm_buttons"),
    shiny::plotOutput("plot", height = "620px", click = "click",
      dblclick = "img_dblclick"),
    # --- coincident points, immediately under the photograph ------------------
    # A zero is a measurement, and it is decided while looking at the fish -- so
    # the switch belongs under the photograph and not in a settings tab. It is
    # reset for every specimen: a mouth on the belly is a statement about THIS
    # species. Neither point is removed: one takes the coordinates of the other,
    # both stay on the photograph and both are written to the workbook.
    shiny::div(
      class = "collapsebar",
      shiny::div(style = "display:inline-block;vertical-align:middle;margin-right:10px;",
                 shiny::tags$strong("Coincident points:")),
      shiny::div(style = "display:inline-block;vertical-align:middle;",
                 shiny::checkboxGroupInput(
                   "collapse", NULL, inline = TRUE,
                   choiceNames = unname(vapply(.FM_COLLAPSE, function(r) r$label,
                                               character(1))),
                   choiceValues = names(.FM_COLLAPSE))),
      shiny::uiOutput("collapse_help")),
    shiny::fluidRow(
      shiny::column(7, card_box("Control: target vs. reconstructed segment (px)",
                                shiny::tableOutput("rt"))),
      shiny::column(5, card_box("Status", shiny::verbatimTextOutput("status")))),
    # --- reference, en pied de page -------------------------------------------
    # The entry order, the conventions and the colour code are read on the first
    # specimen and never again. Above the photograph they cost three lines of
    # scroll on each of the following thousands.
    card_box("Entry order, conventions and colour code",
             shiny::uiOutput("lm_legend"))
  )

  ui <- if (has_bslib) {
    # `fillable = FALSE`: this page is a document that scrolls, not a dashboard.
    # In a filling page every child negotiates a share of the height, and the
    # photograph -- which asks for 620 px -- is squeezed by the bars above and
    # the panels below.
    bslib::page_sidebar(
      title = app_title,
      theme = bslib::bs_theme(version = 5, primary = "#2563eb",
                              "border-radius" = "0.5rem"),
      fillable = FALSE,
      sidebar = bslib::sidebar(width = 360, open = "desktop", side_panel),
      head_tags, main_panel)
  } else {
    shiny::fluidPage(
      head_tags,
      shiny::titlePanel(app_title, windowTitle = "FishMORPH digitizer"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3, side_panel),
        shiny::mainPanel(width = 9, main_panel)))
  }

  # --- serveur ---------------------------------------------------------------
  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(
      qi = 1L, mode = mode, img = NULL, w = NULL, h = NULL,
      A = NULL, B = NULL, P = NULL, override = list(), saved = integer(0),
      sel = 1L, zoom = 1, cx = NULL, cy = NULL, hx = NULL, hy = NULL,
      arr = NULL, flip = "none", dispflip = "none", na = integer(0),
      newstamp = 0L,         # incremented at every write into new_sheet
      flushstamp = 0L,       # incremented at every write of the workbook
      edited = integer(0),   # points MOVED by the user during this session
                             # (as opposed to points merely loaded from the workbook)
      adjusted = integer(0), # points snapped by the extreme-point convention
                             # (status "adjusted" in the journal)
      collapse = character(0))  # segments declared zero on THIS specimen

    # queue and direct-access list of the current mode. In "new" mode the queue
    # indexes the PHOTOGRAPHS of new_photo_dir (and not rows of lm_df).
    qrows    <- shiny::reactive(switch(rv$mode, correct = q_corr, new = q_new, q_recon))
    goto_now <- shiny::reactive(switch(rv$mode, correct = choices_corr,
                                       new = choices_new, choices_recon))
    is_new   <- shiny::reactive(identical(rv$mode, "new"))
    # entry order and points displayed: + the scale bar 20/21 in "new" mode
    click_order <- shiny::reactive(if (is_new()) .FM_CLICK_ORDER_NEW else .FM_CLICK_ORDER)
    lm_pts      <- shiny::reactive(if (is_new()) .FM_LM_PTS_NEW else .FM_LM_PTS)

    # server-side population of the direct-access field: the list is never
    # rendered whole in the browser (server-side filtering/pagination).
    # NB: the choices of the INITIAL mode (the `mode` value) are used, not the
    # goto_now() reactive, because we are outside a reactive context here.
    shiny::updateSelectizeInput(session, "goto_species",
      choices = switch(mode, correct = choices_corr, new = choices_new, choices_recon),
      selected = 1L, server = TRUE)

    # flipping + display: the image ARRAY (numeric) is flipped, then, if the
    # fast display is on, sub-sampled for rendering. The COORDINATES stay in
    # original pixels (rv$w/rv$h unchanged), so clicks and records are not
    # affected -- only the sharpness on screen changes.
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
      a <- flip_arr(rv$arr, rv$flip)          # "photo + landmarks" flip
      a <- flip_arr(a, rv$dispflip)           # PURELY visual flip (points fixed)
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

    # cur_idx = position in the queue (row of lm_df, or photo index if "new")
    cur_idx  <- shiny::reactive(qrows()[rv$qi])
    cur_row  <- shiny::reactive(if (is_new()) NA_integer_ else cur_idx())
    cur_key  <- shiny::reactive(if (is_new()) NA_character_ else lm_df$.key[cur_row()])
    # path of the current photograph: the workbook photo index, or the raw file
    cur_photo <- shiny::reactive({
      i <- cur_idx(); if (length(i) != 1 || is.na(i)) return(NA_character_)
      if (is_new()) new_photos[i] else {
        k <- cur_key(); if (is.na(k) || !k %in% names(photos)) NA_character_ else photos[[k]]
      }
    })
    # row of new_sheet matching a photograph file (NA if absent). A NON reactive
    # version: `new_df` is not a reactiveVal, so the search must re-read the
    # up-to-date object at save time -- otherwise a second click on "Save"
    # before navigating would append a DUPLICATE row.
    new_row_of <- function(f) {
      if (length(f) != 1 || is.na(f) || !nrow(new_df) ||
          !"photo_file" %in% names(new_df)) return(NA_integer_)
      h <- which(!is.na(new_df$photo_file) & new_df$photo_file == f)
      if (length(h)) h[1] else NA_integer_
    }
    # reactive version for display: invalidated by navigation and by rv$newstamp
    # (incremented after every write into new_sheet).
    cur_new_row <- shiny::reactive({
      if (!is_new()) return(NA_integer_)
      rv$newstamp
      new_row_of(basename(cur_photo()))
    })
    cur_name <- shiny::reactive({
      if (!is_new()) return(lm_df$Genus.species[cur_row()])
      # "new" mode: the input field prevails; failing that, the name already
      # recorded in new_sheet, otherwise the one deduced from the file name.
      nm <- trimws(as.character(input$new_species %||% ""))
      if (nzchar(nm)) return(nm)
      basename(cur_photo())
    })
    cur_seg  <- shiny::reactive({
      # "new" mode: no measured segments -> pseudo-segments = the median
      # FISHMORPH proportions with Bl = 1 (see .FM_NEW_RATIOS). The rest of the
      # code (placement, px/unit scale, control table) is unchanged.
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
    # a safe numeric scalar: input$... may be NULL (input not yet created) or
    # "" -> as.numeric() rend numeric(0), et `if (is.finite(numeric(0)))` echoue.
    num1 <- function(x) {
      v <- suppressWarnings(as.numeric(x))
      if (length(v) != 1 || !is.finite(v)) NA_real_ else v
    }
    # mm/px scale from points 20-21 and the ruler length typed in
    mmpp_of <- function(P) {
      mm <- num1(input$ruler_mm)
      if (!is.finite(mm) || mm <= 0) return(NA_real_)
      if (nrow(P) < 21 || !all(is.finite(P[c(20L, 21L), ]))) return(NA_real_)
      d <- sqrt(sum((P[21L, ] - P[20L, ])^2))
      if (!is.finite(d) || d <= 0) NA_real_ else mm / d
    }

    # "correct" mode: reloads the 21 landmarks already recorded in the workbook
    # (LM1 -> A, LM2 -> B, the others as overrides) to review / move them.
    # Empty points (NA in the sheet) are marked NA; LM23 is derived and is
    # therefore not reloaded (it is recomputed afterwards). The axis and the
    # scale follow from the LM1/LM2 loaded, even when the segments are missing.
    # `df`/`row`: by default the landmark sheet at the current row; in "new"
    # mode new_df and the row already recorded for this photograph are passed.
    # `pts`: points to reload (the 21 landmarks, + 20/21 in "new" mode).
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
      for (pt in extra) {                          # hinges: loaded if present,
        xy <- getxy(pt)                            # otherwise simply not placed (not NA)
        if (all(is.finite(xy))) ov[[as.character(pt)]] <- xy
      }
      rv$override <- ov; rv$na <- na; rv$sel <- 22L  # hinge active on opening
    }
    # "new" mode: reloads a photograph already recorded in new_sheet. The scale
    # bar (20/21) is reloaded but is never marked NA -- it is
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
      # A declared zero is a statement about ONE species: it never carries over.
      rv$collapse <- character(0)
      shiny::updateCheckboxGroupInput(session, "collapse", selected = character(0))
      rv$sel <- 1L; rv$zoom <- 1; rv$cx <- NULL; rv$cy <- NULL
      if (!length(qrows())) { rv$img <- NULL; rv$arr <- NULL; return() }
      path <- cur_photo()
      img <- if (is.na(path)) NULL else tryCatch(read_img(path), error = function(e) NULL)
      if (is.null(img)) { rv$img <- NULL; rv$arr <- NULL; return() }
      rv$arr <- img; rv$h <- dim(img)[1]; rv$w <- dim(img)[2]
      rv$flip <- "none"; rv$img <- make_disp()
      shiny::updateRadioButtons(session, "flip_mode", selected = "none")
      if (is_new()) {
        # species name: the one already recorded for this photograph, otherwise
        # the one deduced from the file name. Also reloads the points if the
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
      # reloads the recorded landmarks if the species already has some: always in
      # "correct" mode, and also in "reconstruct" mode for a species already
      # saved (otherwise coming back to it restarted from computed positions
      row0 <- cur_row()
      has_saved <- !is.na(row0) && "1_X" %in% names(lm_df) &&
        is.finite(suppressWarnings(as.numeric(lm_df[row0, "1_X"])))
      if (rv$mode == "correct" || has_saved) seed_from_existing()
    }
    shiny::observeEvent(rv$qi, load_species(), ignoreInit = FALSE)

    # mode switch: changes queue, goes back to the 1st species, reloads the
    # direct-access list and the current species
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

    # current reconstruction (22x2 matrix), with the manual overrides applied
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
      if (length(rv$na)) P[rv$na, ] <- NA_real_          # points marked non-measurable
      if (isTRUE(input$correct)) {
        # the conventions only "protect" the points YOU moved during this session
        # (rv$edited), not the points merely reloaded from the workbook in
        # correction mode -- otherwise, all being overrides, nothing would follow
        # anything (e.g. moving 4 no longer brought 8/9/11 back onto the belly line).
        # target PFl length in pixels (scale = the broken axis Bl) -> 12 = 10 + PFl*uf
        blpx <- .fm_axis_len_px(P)
        ppu <- blpx / as.numeric(seg$Bl)
        # in "new" mode PFl is NOT measured (it is a seeding median): the length
        # 10-12 is therefore not locked, only the parallelism is kept.
        pfl_px <- if (is_new() || !is.finite(ppu)) NA_real_
                  else as.numeric(seg$PFl) * ppu
        P <- .fm_constrain(P, rv$edited, pfl_px = pfl_px)
      }
      P[23, ] <- .fm_point23(P)     # 23 always recomputed (auto) after editing/conventions
      # LAST, after the conventions AND after 23 is rebuilt: the conventions
      # re-derive the ventral points on the belly line, and 23 -- built on the
      # line (1, 9) -- is undefined once 9 sits on 1. Applying the rules here is
      # what puts 23 on 1 instead of leaving it NA.
      .fm_apply_collapse(P, rv$collapse)
    })

    # The declared zeros. recon() applies them, so the only state needed here is
    # the list itself; the points a rule moves are marked "adjusted" -- placed by
    # a rule the operator invoked, neither pointed at nor left at their seed.
    shiny::observeEvent(input$collapse, {
      rv$collapse <- input$collapse %||% character(0)
      moved <- .fm_collapse_points(rv$collapse)
      released <- setdiff(.fm_collapse_points(names(.FM_COLLAPSE)), moved)
      rv$adjusted <- union(setdiff(rv$adjusted, released), moved)
      rv$edited   <- setdiff(rv$edited, moved)
      rv$override[as.character(moved)] <- NULL   # the rule drives them now
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$collapse_help <- shiny::renderUI({
      act <- rv$collapse
      txt <- if (!length(act))
        paste("A segment that is genuinely zero on this species -- a mouth on",
              "the ventral profile, a head ending on it. One point takes the",
              "coordinates of the other: both stay on the photograph and in the",
              "workbook. Re-applied after every click, reset for each species.")
      else paste("Active:", paste(vapply(.FM_COLLAPSE[act], function(r) r$tip,
                                         character(1)), collapse = " | "))
      shiny::div(style = "font-size:11.5px;color:#92400e;margin-top:2px;", txt)
    })

    # centres the zoom on the active point (when it has a position)
    zoom_to_sel <- function() {
      if (is.null(rv$A) || is.null(rv$B)) return()
      P <- try(recon(), silent = TRUE); if (inherits(P, "try-error")) return()
      if (rv$sel <= nrow(P) && all(is.finite(P[rv$sel, ]))) {
        rv$cx <- P[rv$sel, 1]; rv$cy <- P[rv$sel, 2] }
    }

    # click on the photograph: places the ACTIVE point, then auto-advances
    shiny::observeEvent(input$click, {
      if (is.null(rv$img)) return()
      pt <- c(input$click$x, input$click$y); s <- rv$sel
      if (s == 1L) rv$A <- pt
      else if (s == 2L) rv$B <- pt
      else {  # re-inserted at the end of the list: the last point moved drives its group
        ov <- rv$override; k <- as.character(s)
        ov[[k]] <- NULL; ov[[k]] <- pt; rv$override <- ov
        if (s %in% rv$na) rv$na <- setdiff(rv$na, s)   # re-placed -> no longer NA
        rv$edited <- union(rv$edited, s)               # point moved by hand
        # a point snapped by the convention then pointed at again by hand becomes
        # a MEASUREMENT again: it must no longer come out as "adjusted".
        rv$adjusted <- setdiff(rv$adjusted, s)
      }
      rv$sel <- .fm_next(s, click_order())
    })

    # button bar: selects the active point
    shiny::observeEvent(input$sel_btn, { rv$sel <- as.integer(input$sel_btn); zoom_to_sel() })

    # marks the active point as NA (non-measurable) then advances
    shiny::observeEvent(input$set_na, {
      if (rv$sel %in% c(1L, 2L)) return()          # snout/caudal required for the axis
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
    # right button held down: panning the view
    shiny::observeEvent(input$pan, {
      if (is.null(rv$img) || rv$zoom <= 1) return()
      if (is.null(rv$cx)) rv$cx <- rv$w / 2
      if (is.null(rv$cy)) rv$cy <- rv$h / 2
      rv$cx <- rv$cx - input$pan$dx * (rv$w / rv$zoom)
      rv$cy <- rv$cy - input$pan$dy * (rv$h / rv$zoom)
    })

    # --- flip the photograph (transforms the points already placed too) ---
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
    # PURELY visual flip: it only flips the display of the photograph, the
    # landmarks (and hence the record) stay unchanged. Persists from one species
    # to the next. The frame of the clicks is that of the points, so a correction
    # made here stays coherent with the points already loaded.
    shiny::observeEvent(input$flip_disp, {
      if (is.null(rv$arr)) return()
      rv$dispflip <- input$flip_disp
      rv$img <- make_disp()
    }, ignoreInit = TRUE)
    # fast-display toggle (does not touch the points)
    shiny::observeEvent(input$fastdisp, { if (!is.null(rv$arr)) rv$img <- make_disp() },
                        ignoreInit = TRUE)

    # point bar above the photograph (green = active, blue = placed, grey = derived)
    output$lm_buttons <- shiny::renderUI({
      # Four sections, separated by a gap: the broken axis (1, 22, 24, 2), the
      # anatomical run, the points never clicked (derived, spare hinge 25), and
      # the scale bar in "new" mode. A single run of identical buttons is a
      # list; four groups is a map, and a point is found in the group it
      # belongs to.
      anat <- setdiff(click_order(), c(1L, 22L, 24L, 2L, .FM_SCALE_PTS))
      sections <- list(c(1L, 22L, 24L, 2L), anat, c(.FM_DERIVED, 25L),
                       if (is_new()) .FM_SCALE_PTS)
      sections <- Filter(length, sections)
      order_show <- unlist(sections, use.names = FALSE)
      placed <- function(i) {
        if (i == 1L) return(!is.null(rv$A)); if (i == 2L) return(!is.null(rv$B))
        as.character(i) %in% names(rv$override)
      }
      btns <- lapply(order_show, function(i) {
        col <- if (i == rv$sel) "background:#28a745;color:#fff;font-weight:bold;"
               else if (i %in% rv$na) "background:#f8d7da;color:#a00;text-decoration:line-through;"
               else if (i %in% .FM_SCALE_PTS) "background:#d9f2e6;color:#065;font-weight:bold;"  # scale bar
               else if (i %in% .FM_HINGES) "background:#ffd24d;color:#000;font-weight:bold;"  # hinges 22/24/25
               else if (i %in% .FM_DERIVED) "background:#eee;color:#999;"
               else if (placed(i)) "background:#cfe8ff;"
               else "background:#f7f7f7;"
        shiny::tags$button(type = "button", i, class = "lmbtn",
          onclick = sprintf("Shiny.setInputValue('sel_btn', %d, {priority:'event'});", i),
          style = col)
      })
      # Nothing but the buttons, and all of them on ONE line: the bar is a map of
      # the specimen, read at a glance dozens of times per fish, and the colour
      # code is stated once at the foot of the page. A legend repeated above
      # every specimen would only push the photograph further down.
      kids <- list(); k0 <- 0L
      for (k in seq_along(sections)) {
        if (k > 1L) kids[[length(kids) + 1L]] <- shiny::div(class = "lmgap")
        kids <- c(kids, btns[k0 + seq_along(sections[[k]])])
        k0 <- k0 + length(sections[[k]])
      }
      shiny::div(class = "lmrow", kids)
    })

    # The colour code and the conventions, at the foot of the page: read once.
    output$lm_legend <- shiny::renderUI({
      shiny::tags$div(style = "font-size:11.5px;color:#6b7280;line-height:1.6;",
        shiny::tags$b("Landmark bar: "),
        "green = active; blue = placed; pink struck through = NA; grey = derived;",
        "yellow = HINGES; pale green = scale bar 20/21 (mode",
        "\"New photographs\" mode only, optional, at the end of the list).",
        shiny::tags$br(),
        shiny::tags$b("Broken axis: "), "1 -> 22 -> 24 -> 2 (place 22 then 24 on",
        "the bends of a curved specimen). Head on 1-22, Bd and pectoral fin",
        "(10, 11, 12) on 22-24, caudal (16-17, 18-19) on 24-2. 25, at the end of",
        "the list, adds a fourth axis segment, with no convention.",
        shiny::tags$br(),
        shiny::tags$b("What is recorded: "),
        "the 21 landmarks (1-19, 22, 23) AND the hinges 24 and 25, in their own",
        "columns, plus 20/21 in \"New photographs\" mode. The hinges are not",
        "landmarks and have no place in any shape analysis, but they define",
        "the frames in which every convention was applied: without them,",
        "a species",
        "reopened for correction comes back with a straight axis and its",
        "geometry silently stops matching the one it was digitized under.")
    })

    nav <- function(step) {
      ni <- rv$qi + step
      if (ni >= 1 && ni <= length(qrows())) rv$qi <- ni
    }
    shiny::observeEvent(input$nextsp, nav(1))
    shiny::observeEvent(input$prev,   nav(-1))
    shiny::observeEvent(input$skip,   nav(1))

    # direct access: the field (search by name) jumps to the chosen species
    shiny::observeEvent(input$goto_species, {
      ni <- suppressWarnings(as.integer(input$goto_species))
      if (!is.na(ni) && ni >= 1 && ni <= length(qrows()) && ni != rv$qi)
        rv$qi <- ni
    }, ignoreInit = TRUE)
    # keeps the field in sync when navigating with the buttons / saving
    shiny::observeEvent(rv$qi, {
      shiny::updateSelectizeInput(session, "goto_species", selected = rv$qi)
    }, ignoreInit = TRUE)

    # --- recording a NEW specimen into `new_sheet` -----------------------------
    # Row key = photo_file (basename). If the photograph is already there, the
    # row is REWRITTEN; otherwise a row is APPENDED. Returns TRUE if written.
    save_new <- function(P) {
      nm <- trimws(as.character(input$new_species %||% ""))
      if (!nzchar(nm)) {
        shiny::showNotification("Fill in the species name before saving.",
                                type = "error")
        return(FALSE)
      }
      f <- basename(cur_photo())
      if (is.na(f)) return(FALSE)
      r <- new_row_of(f)                           # lecture directe (cf. new_row_of)
      if (is.na(r)) {                              # new row at the end of the sheet
        r <- nrow(new_df) + 1L
        blank <- as.data.frame(matrix(NA_character_, nrow = 1, ncol = ncol(new_df)),
                               stringsAsFactors = FALSE)
        names(blank) <- names(new_df)
        new_df <<- rbind(new_df, blank)
      }
      r_excel <- r + 1L                            # +1 for the header
      wr <- function(col, val) {
        j <- col_of_new(col); if (is.na(j)) return(invisible())
        openxlsx::writeData(wb, new_sheet, val, startCol = j, startRow = r_excel,
                            colNames = FALSE)
        new_df[r, col] <<- as.character(val)       # new_df is all-character
      }
      wr("Genus.species", nm)
      wr("photo_file", f)
      dropped <- integer(0)
      for (pnum in save_pts_new) {                 # 1..19, 20, 21, 22, 23, 24, 25
        if (is.na(col_of_new(paste0(pnum, "_X"))) ||
            is.na(col_of_new(paste0(pnum, "_Y")))) {
          if (all(is.finite(P[pnum, ]))) dropped <- c(dropped, pnum)
          next
        }
        wr(paste0(pnum, "_X"), round(P[pnum, 1], 3))
        wr(paste0(pnum, "_Y"), round(P[pnum, 2], 3))
      }
      if (length(dropped))
        shiny::showNotification(
          sprintf(paste("Columns absent from '%s': point(s) %s are NOT written",
                        "to the workbook (the journal, however, has them)."),
                  new_sheet, paste(dropped, collapse = ", ")),
          type = "error", duration = NULL)
      mm <- num1(input$ruler_mm)
      wr("ruler_mm", if (is.finite(mm)) mm else NA)
      mpp <- mmpp_of(P)
      wr("mm_per_px", if (is.finite(mpp)) round(mpp, 6) else NA)
      rv$newstamp <- rv$newstamp + 1L              # invalide cur_new_row()
      TRUE
    }

    # --- status of each point, for the journal ---------------------------------
    # This is the information the wide workbook layout cannot carry:
    #   placed  : placed / moved by hand, or reloaded from an earlier entry
    #   seeded  : STILL AT ITS SEED POSITION, hence never checked -> to be audited
    #   adjusted: snapped by the extreme-point convention (3/4), not pointed at
    #   derived : calcule automatiquement (8, 9, 11, 15, 23)
    #   na      : declare non mesurable
    point_status <- function(points) {
      ov <- names(rv$override)
      # rv$edited comes BEFORE .FM_DERIVED: a derived point that you explicitly
      # repositioned this session is no longer a computed point, it is a measurement.
      st <- vapply(points, function(p) {
        if (p %in% rv$na) "na"
        # "adjusted" BEFORE "placed": a point snapped by the extreme-point
        # convention was not pointed at by the operator, and the distinction must
        # survive into the journal (quality control after the fact).
        else if (p %in% rv$adjusted) "adjusted"
        else if (p %in% rv$edited) "placed"
        else if (p %in% .FM_DERIVED) "derived"
        else if (p %in% c(1L, 2L) || as.character(p) %in% ov) "placed"
        else "seeded"
      }, character(1))
      stats::setNames(st, as.character(points))
    }

    # writes the current record to the JOURNAL (before any xlsx)
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

    # --- saving -----------------------------------------------------------------
    # ORDER MATTERS: the journal first (appending a block of lines, immutable,
    # instantaneous), the workbook afterwards and in batches. If R stops between
    # nothing is lost: fishmorph_consolidate(journal_dir) rebuilds the database.
    # --- the conventions: checked on save ---------------------------------------
    # The "Save & next" button no longer triggers the write directly: it goes
    # through this check first. If 3 is not the most dorsal point (or 4 the most
    # ventral), or if the eye vertical is out of order, a dialog offers to
    # measure again or to correct.
    conv_msg <- function(v) {
      shiny::tags$ul(lapply(seq_len(nrow(v)), function(r) {
        i <- v$point[r]; j <- v$culprit[r]
        shiny::tags$li(if (identical(v$kind[r], "extreme")) sprintf(
          "Point %d%s must be the most %s: point %d%s overshoots it by %.0f px.",
          i, .fm_pt_label(i), if (i == 3L) "DORSAL" else "VENTRAL",
          j, .fm_pt_label(j), v$delta[r])
        else sprintf(
          "Eye vertical out of order: point %d%s must sit ABOVE point %d%s, and is %.0f px below it.",
          i, .fm_pt_label(i), j, .fm_pt_label(j), v$delta[r]))
      }))
    }
    show_conv_modal <- function(v) {
      has_ext <- any(v$kind == "extreme")
      has_ord <- any(v$kind == "order")
      shiny::showModal(shiny::modalDialog(
        title = if (has_ext && has_ord)
          "FISHMORPH conventions: Bd (3-4) and the eye vertical (5, 13, 7, 14, 6, 8)"
        else if (has_ord)
          "FISHMORPH conventions: the eye vertical (5, 13, 7, 14, 6, 8) is out of order"
        else "FISHMORPH conventions: Bd (3-4) is not the maximum body depth",
        conv_msg(v),
        shiny::tags$p(shiny::tags$em(
          "Heights measured perpendicular to the body axis. The caudal fin",
          "(16-19), the appendage tips (12, 15) and the derived ventral points",
          "(8, 9, 11) are excluded from the Bd test.")),
        if (has_ord) shiny::tags$p(shiny::tags$em(
          "The six points 5, 13, 7, 14, 6, 8 lie on one vertical, in that order",
          "from the back downwards: top of the head, top of the eye, centre,",
          "bottom of the eye, bottom of the head, body underside. An inversion",
          "leaves each pair internally consistent -- Ed keeps its length -- while",
          "Hd or Eh silently refers to the wrong point.")),
        shiny::tags$p(
          if (has_ext) shiny::tags$span(
            "Correcting automatically gives point 3 or 4 the height of the point",
            "overshooting it, keeping its position along the axis; the snapped",
            "points are recorded as 'adjusted' in the journal.")
          else NULL,
          if (has_ord) shiny::tags$span(
            shiny::tags$b(" An inversion is not corrected automatically:"),
            "moving a point to satisfy the order would invent a measurement.",
            "Measure the points again, or save as it is if the order is real.")
          else NULL),
        footer = shiny::tagList(
          shiny::actionButton("conv_remeasure", "Measure again", class = "btn-primary"),
          if (has_ext) shiny::actionButton("conv_fix", "Correct automatically and save"),
          shiny::actionButton("conv_asis", "Save without correcting")),
        easyClose = FALSE, size = "l"))
    }
    # applies the correction, going back through recon(): the constrained-editing
    # conventions may move points again (the belly line in particular), so we
    # iterate to stability -- 3 passes are ample, and the bound rules out an
    # infinite loop on a pathological case.
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
        v <- .fm_convention_violations(recon())
        if (!is.null(v)) { show_conv_modal(v); return() }
      }
      do_save()
    })

    # 1) measure again: close, select the offending point and zoom onto it. For
    #    an inversion the point to re-measure is the CULPRIT -- the one found on
    #    the wrong side -- not the reference it was compared with.
    shiny::observeEvent(input$conv_remeasure, {
      shiny::removeModal()
      v <- .fm_convention_violations(recon())
      if (!is.null(v)) {
        rv$sel <- if (identical(v$kind[1], "order")) v$culprit[1] else v$point[1]
        zoom_to_sel()
      }
    })
    # 2) correct automatically then save. Only the extremes are corrected; an
    #    inversion of the eye vertical is reported again on the way out, because
    #    saving it silently would hide the one error the check exists for.
    shiny::observeEvent(input$conv_fix, {
      shiny::removeModal()
      ok <- apply_conv_fix()
      if (!isTRUE(ok))
        shiny::showNotification(
          "Convention 3/4 still breached after correction: check the placement.",
          type = "warning", duration = 8)
      ord <- .fm_eye_order_violations(recon())
      if (!is.null(ord))
        shiny::showNotification(
          sprintf(paste("Eye vertical still out of order (%s): not corrected",
                        "automatically, saved as it is."),
                  paste(sprintf("%d/%d", ord$point, ord$culprit), collapse = ", ")),
          type = "warning", duration = 10)
      do_save()
    })
    # 3) save as it is (the discrepancy is real and assumed)
    shiny::observeEvent(input$conv_asis, { shiny::removeModal(); do_save() })

    do_save <- function() {
      shiny::req(rv$A, rv$B)
      P <- recon()
      if (is_new()) {                              # new specimens: another sheet
        nm <- trimws(as.character(input$new_species %||% ""))
        f  <- basename(cur_photo())
        if (!nzchar(nm) || is.na(f)) {
          shiny::showNotification("Fill in the species name before saving.",
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
      r_excel <- cur_row() + 1L                    # +1 for the header
      # A missing column loses the point: ensure_cols() creates them all at
      # start-up (22/23 already exist, 24/25 are added), so this case should
      # never arise -- but if it does (a sheet replaced by hand), the point must
      # disappear LOUDLY and not in silence.
      dropped <- integer(0)
      for (pnum in save_pts) {                     # 1..19, 22, 23, 24, 25
        cx <- col_of(paste0(pnum, "_X")); cy <- col_of(paste0(pnum, "_Y"))
        if (is.na(cx) || is.na(cy)) {
          if (all(is.finite(P[pnum, ]))) dropped <- c(dropped, pnum)
          next
        }
        openxlsx::writeData(wb, lm_sheet, round(P[pnum, 1], 3),
                            startCol = cx, startRow = r_excel, colNames = FALSE)
        openxlsx::writeData(wb, lm_sheet, round(P[pnum, 2], 3),
                            startCol = cy, startRow = r_excel, colNames = FALSE)
      }
      if (length(dropped))
        shiny::showNotification(
          sprintf(paste("Columns absent from '%s': point(s) %s are NOT written",
                        "to the workbook. They are in the journal;",
                        "fishmorph_consolidate() will find them again."),
                  lm_sheet, paste(dropped, collapse = ", ")),
          type = "error", duration = NULL)
      # updates lm_df IN MEMORY so that coming back to the species within the
      # session reloads exactly what has just been saved (24/25 included).
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

    # manual write of the workbook (the journal is already up to date)
    shiny::observeEvent(input$flush, {
      if (pending == 0L) {
        shiny::showNotification("Workbook already up to date.", type = "message"); return()
      }
      n <- pending
      if (isTRUE(flush_xlsx(force = TRUE)))
        shiny::showNotification(sprintf("Workbook written (%d record(s)).", n),
                                type = "message")
      else
        shiny::showNotification("Write failed: the data is kept in the journal.",
                                type = "error")
      rv$flushstamp <- rv$flushstamp + 1L
    })

    # end of session (tab closed / app stopped): a last flush. A safety net only
    # -- if R is killed outright it does not run, and it is precisely for that
    # case that the journal exists.
    session$onSessionEnded(function() {
      try(flush_xlsx(force = TRUE), silent = TRUE)
    })

    output$plot <- shiny::renderPlot({
      if (is.null(rv$img)) { graphics::plot.new()
        graphics::text(.5, .5, "No photograph available for this species."); return() }
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
        # reference lines (body outline, belly, vertical of the eye, eye)
        if (isTRUE(input$showlines)) {
          path <- function(pts, ...) { pts <- pts[is.finite(P[pts, 1])]
            if (length(pts) > 1) graphics::lines(P[pts, 1], P[pts, 2], ...) }
          path(c(1, 5, 3, 16, 18, 19, 17, 4, 6, 1), col = "grey30", lwd = 1)      # contour
          path(c(9, 8, 11, 4), col = "grey85", lty = 3, lwd = 1)                  # belly
          if (all(is.finite(P[c(1, 9), ]))) path(c(1, 9), col = "grey60", lwd = 1) # snout line (1-9)
          if (all(is.finite(P[c(6, 23), ])))                                       # segment 23-6 (// axe)
            graphics::segments(P[23,1], P[23,2], P[6,1], P[6,2], col = "magenta", lwd = 1.5)
          path(c(5, 13, 7, 14, 6, 8), col = "grey85", lty = 3, lwd = 1)           # vertical of the eye
          if (all(is.finite(P[c(7, 13, 14), ]))) {                               # eye (circle)
            er <- sqrt(sum((P[13, ] - P[14, ])^2)) / 2; th <- seq(0, 2*pi, length.out = 60)
            graphics::lines(P[7,1] + er*cos(th), P[7,2] + er*sin(th), col = "grey85", lty = 3, lwd = 1)
          }
        }
        pairs <- lapply(.FM_PAIR_SEG, `[[`, "pair")
        cols <- grDevices::hcl.colors(length(pairs), "Dark3")
        for (k in seq_along(pairs)) { ab <- pairs[[k]]
          # Bl: drawn along the BROKEN axis 1 -> hinges placed -> 2
          if (names(pairs)[k] == "Bl") {
            ch <- .fm_axis_chain(P)
            graphics::lines(P[ch, 1], P[ch, 2], col = cols[k], lwd = 2)
          } else
            graphics::segments(P[ab[1],1],P[ab[1],2],P[ab[2],1],P[ab[2],2], col = cols[k], lwd = 2) }
        # scale bar 20-21 ("new" mode): drawn in green, outside the body
        if (is_new() && all(is.finite(P[.FM_SCALE_PTS, ])))
          graphics::segments(P[20,1], P[20,2], P[21,1], P[21,2],
                             col = "#00a06a", lwd = 3)
        lp <- lm_pts()
        graphics::points(P[lp,1], P[lp,2], pch = 21, bg = "white", cex = 1.2)
        # COINCIDENT POINTS. Two points at the same pixel draw one circle on top
        # of the other and one label over the other, so a legitimate zero LOOKS
        # like a lost point. Both are here; the drawing has to say so. A group
        # gets a wider ring and ONE label carrying every number it holds
        # ("10+11"), instead of stacking illegible labels.
        lpf <- lp[vapply(lp, function(i) i <= nrow(P) && all(is.finite(P[i, ])),
                         logical(1))]
        if (length(lpf)) {
          grp <- split(lpf, paste(round(P[lpf,1], 1), round(P[lpf,2], 1)))
          single <- unlist(grp[lengths(grp) == 1L], use.names = FALSE)
          if (length(single))
            graphics::text(P[single,1], P[single,2], single, pos = 3, cex = .7,
                           col = "yellow")
          for (g in grp[lengths(grp) > 1L]) {
            xy <- P[g[1], ]
            graphics::points(xy[1], xy[2], pch = 1, col = "yellow", cex = 2.4, lwd = 2)
            graphics::points(xy[1], xy[2], pch = 1, col = "black",  cex = 3.0, lwd = 1)
            graphics::text(xy[1], xy[2], paste(sort(g), collapse = "+"), pos = 3,
                           cex = .75, font = 2, col = "yellow")
          }
        }
        # extra hinges (24, 25) placed: drawn in gold
        xh <- setdiff(.FM_HINGES, lp)
        xh <- xh[vapply(xh, function(i) i <= nrow(P) && all(is.finite(P[i, ])), logical(1))]
        if (length(xh)) {
          graphics::points(P[xh,1], P[xh,2], pch = 21, bg = "gold", cex = 1.3)
          graphics::text(P[xh,1], P[xh,2], xh, pos = 3, cex = .7, col = "orange")
        }
        # active point in red (landmarks OR hinges)
        if (rv$sel %in% c(lp, .FM_HINGES) && rv$sel <= nrow(P) &&
            all(is.finite(P[rv$sel, ])))
          graphics::points(P[rv$sel,1], P[rv$sel,2], pch = 21, bg = "red", cex = 1.7, lwd = 2)
        graphics::legend("topright", names(pairs), col = cols, lwd = 2, bg = "white", cex = .8, ncol = 2)
      }
    })

    # label of the current photograph + record status ("new" mode)
    output$new_photo_lab <- shiny::renderUI({
      if (!is_new()) return(NULL)
      f <- basename(cur_photo())
      already <- !is.na(cur_new_row())
      shiny::HTML(sprintf("Photo : <code>%s</code><br>%s",
        if (is.na(f)) "-" else f,
        if (already) "<span style='color:#0a0'>already in the sheet (will be rewritten)</span>"
        else "<span style='color:#666'>new row on saving</span>"))
    })

    output$rt <- shiny::renderTable({
      shiny::req(rv$A, rv$B); P <- recon(); seg <- cur_seg()
      # Bl = length along the BROKEN axis 1->...->2 (curvilinear, every hinge
      # placed); the px/unit scale follows from it and converts the others.
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
      # "new" mode: no measured target. `target` is the FISHMORPH MEDIAN of the
      # segment/Bl ratio and `measured` the ratio actually digitized; the mm
      # column only appears when the scale bar 20-21 is placed.
      out <- data.frame(segment = names(.FM_PAIR_SEG),
                        mediane_ratio = cible, ratio_mesure = rec, row.names = NULL)
      mpp <- mmpp_of(P)
      if (is.finite(mpp)) out$mm <- rec * Blpx * mpp
      out
    }, digits = 3)

    output$progress <- shiny::renderUI({
      modelab <- switch(rv$mode,
        correct = "CORRECT (already landmarked)",
        new     = "NEW PHOTOGRAPHS (appended to the workbook)",
        "RECONSTRUCT (without landmarks)")
      Bl <- cur_seg()$Bl
      scal <- "-"
      if (!is.null(rv$A) && !is.null(rv$B) && is.finite(Bl)) {
        P <- try(recon(), silent = TRUE)
        blpx <- if (!inherits(P, "try-error")) .fm_axis_len_px(P)
                else sqrt(sum((rv$B - rv$A)^2))
        # in "new" mode Bl = 1 (a pseudo-segment): px/unit = the body length in
        # pixels, so mm/px (the bar 20-21) is shown instead, and Bl in mm if known.
        scal <- if (is_new()) {
          mpp <- if (inherits(P, "try-error")) NA_real_ else mmpp_of(P)
          if (is.finite(mpp)) sprintf("%.4f mm/px (Bl = %.1f mm)", mpp, blpx * mpp)
          else "bar 20-21 not placed"
        } else sprintf("%.2f px/unit", blpx / Bl)    # scale on the broken axis
      }
      # state of the xlsx buffer: rv$saved / rv$flushstamp act as reactive
      # triggers (`pending` is a plain variable, not reactive).
      rv$flushstamp
      buf <- if (pending == 0L) "workbook up to date"
             else sprintf("<span style='color:#b36b00'>%d en attente d'ecriture</span>",
                          pending)
      shiny::div(class = "progressbox", shiny::HTML(sprintf(
        paste0("Mode : <b>%s</b><br><b>%s</b><br>%s %d / %d<br>Enregistrees : %d",
               "<br>Echelle : %s<br>Journal : <b>OK</b> (%s)"),
        modelab, cur_name(), if (is_new()) "Photo" else "Espece",
        rv$qi, length(qrows()), length(rv$saved), scal, buf)))
    })

    # What the session IS, on one line. These paths are arguments of
    # launch_fishmorph_digitizer(): they are displayed, they are not edited.
    output$session_info <- shiny::renderUI({
      shiny::HTML(sprintf(
        paste("Classeur <code>%s</code> &nbsp;&middot;&nbsp;",
              "photos <code>%s</code> &nbsp;&middot;&nbsp;",
              "journal <code>%s</code> &nbsp;&middot;&nbsp;",
              "operateur <code>%s</code>"),
        basename(out_path), basename(photo_dir), basename(jr$path), jr$operator))
    })

    # State of the two writing layers, in the "Checks" tab.
    output$io_info <- shiny::renderUI({
      rv$flushstamp; rv$saved                      # declencheurs reactifs
      shiny::div(class = "progressbox", shiny::HTML(sprintf(
        paste("Classeur : <code>%s</code><br>%s<br>",
              "Journal: <code>%s</code><br>written at every record"),
        out_path,
        if (pending == 0L) "a jour"
        else sprintf("<b style='color:#b36b00'>%d record(s) pending</b>",
                     pending),
        jr$path)))
    })

    output$status <- shiny::renderText({
      if (is.null(rv$img))
        return(if (!length(qrows())) "No species in this mode." else "Photograph unavailable.")
      lab <- if (rv$sel == 1L) "MUSEAU (LM1)" else if (rv$sel == 2L) "BASE CAUDALE (LM2)"
             else if (rv$sel == 20L) "BARRE D'ECHELLE, debut (LM20)"
             else if (rv$sel == 21L) "BARRE D'ECHELLE, fin (LM21)"
             else paste0("LM", rv$sel)
      intro <- if (rv$mode == "correct")
        paste0("CORRECTION mode: the 21 landmarks are reloaded from the workbook. ",
               "Select a point (a button, or a click on the photograph) then click ",
               "its new position; 'Save & next' rewrites the workbook row.\n")
      else if (rv$mode == "new")
        paste0("NEW PHOTOGRAPHS mode: this photograph is not in the workbook. ",
               "No measured segment exists -> after the clicks on 1 (snout) and 2 ",
               "(caudal base), the points are pre-placed at the FISHMORPH MEDIAN ",
               "PROPORTIONS, to be corrected one by one. Check the species name ",
               "(left panel); 20/21 = scale bar, optional. ",
               "'Save & next' appends a row to the sheet '", new_sheet, "'.\n")
      else ""
      paste0(intro, "Point actif : ", lab,
             " -> click its position on the photograph (auto-advance).\n",
             if (is.null(rv$A) || is.null(rv$B))
               "Place the snout (1) first, then the caudal base (2)."
             # NB: there is NO wheel zoom (no wheel handler is installed); the
             # zoom goes through the +/- buttons of the left panel.
             else paste0("Zoom: + / - buttons (centred on the active point); ",
                         "right-click and drag to pan; ",
                         "double-click for the whole view."))
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server))
}

# -----------------------------------------------------------------------------
# Utilisation
# -----------------------------------------------------------------------------
# library(Rfishmorph)
#
# # (0) DATA SAFETY. Every record goes first to an append-only journal (the
# #     `journal_dir` folder, one TSV file per session, never rewritten); the
# #     workbook is now only an export, written atomically and in batches of
# #     `xlsx_flush_every`. If R crashes, nothing is lost:
# #        base <- fishmorph_consolidate("FishMORPH/landmark_journal",
# #                                      out_csv = "FishMORPH/landmarks.csv")
# #        qc   <- fishmorph_journal_qc("FishMORPH/landmark_journal")
# #     `qc` lists in particular the points left at their SEED position, hence
# #     never checked by eye -- information absent from the workbook.
#
# # (1) digitize the species WITHOUT landmarks (the historical behaviour):
# launch_fishmorph_digitizer(
#   xlsx_path = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
#   photo_dir = "FishMORPH/Photos utilisées"
# )
#
# # (2) review / correct the species ALREADY landmarked:
# launch_fishmorph_digitizer(
#   xlsx_path = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
#   photo_dir = "FishMORPH/Photos utilisées",
#   mode      = "correct"
# )
#
# # (3) ADD new photographs (specimens absent from the workbook):
# launch_fishmorph_digitizer(
#   xlsx_path     = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
#   photo_dir     = "FishMORPH/Photos utilisées",
#   new_photo_dir = "FishMORPH/Photos nouvelles",   # <- folder to digitize
#   new_sheet     = "new_specimens",                # <- destination sheet
#   ruler_mm      = 10,                             # <- length of the ruler 20-21
#   mode          = "new"
# )
# Drop the photographs into `new_photo_dir` BEFORE launching the app: every
# image in the folder becomes an entry of the queue (several specimens of one
# species are therefore possible, one row each). The species name is pre-filled
# from the file name and can be changed in the left panel. The key of a row is
# the `photo_file` column: coming back to a photograph already done reloads its
# points and REWRITES the same row instead of appending a second one.
#
# The "Queue" selector at the top of the app switches between the three queues.
# -> writes into FishMORPH/FISHMORPH_PUBLI_9556sp_reconstructed.xlsx
