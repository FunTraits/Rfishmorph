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
.FM_CLICK_ORDER <- c(1L, 22L, 24L, 2L, 3L, 4L, 7L, 5L, 6L, 15L, 10L, 12L, 16L, 17L, 18L, 19L)
# The anatomical run is the one intraitR::digitize_landmarks() uses (ANAT_ORDER
# there), minus the points this app derives: the two applications measure the
# same protocol, and an operator moving between them must not have to relearn
# the path his eye takes over the specimen.
#
# derived (automatic): 8/9/11 = belly points; 13/14 are seeded symmetrically
# about 7 from the Ed segment (.fm_place); 23 is computed. 10/12 stay in the
# entry loop; as long as they have not been clicked they follow 11 (PFi/PFl
# preserved), and once placed or corrected they stay where you put them.
# 15 (jaw tip, Jl) is NOT derived: in "new" mode there is no measured Jl, so
# leaving it on its median seed would report an inter-specific median as a
# measurement. It is clicked, between 6 and 10, as in intraitR.
# 22 = HINGE (no longer "derived"): shown between 1 and 2 and made active when
# a species is opened in correction mode (see seed_from_existing).
.FM_DERIVED     <- c(8L, 9L, 11L, 13L, 14L, 23L)

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

# --- writing a photograph back to disk ---------------------------------------
# Used by the "bake the flip into the file" button. Three precautions, none of
# them optional:
#
#   * The ORIGINAL is copied aside first, ONCE. A digitizing session is allowed
#     to change how a photograph is stored, not to destroy the only copy of it;
#     and the backup is written only if it does not already exist, so a second
#     flip cannot overwrite the pristine file with an already-flipped one.
#   * The write goes to a temporary file IN THE SAME DIRECTORY and is renamed
#     over the target. A rename within one filesystem is atomic: an interrupted
#     write leaves the old photograph intact rather than a truncated one.
#     tempdir() would not do -- it is often another device, where rename falls
#     back on a copy and the atomicity is lost.
#   * The output format follows the REAL bytes of the original, not its
#     extension. In this collection about 7 % of the ".jpg" files are in fact
#     PNG, GIF or BMP (see read_img()), and re-encoding one of them as JPEG
#     because of its name would add lossy compression to a lossless file.
.fm_write_img <- function(a, path, quality = 0.97) {
  sig <- tryCatch(readBin(path, "raw", n = 2L), error = function(e) raw(0))
  is_jpeg <- length(sig) >= 2 && sig[1] == as.raw(0xFF) && sig[2] == as.raw(0xD8)
  a[!is.finite(a)] <- 0
  a[] <- pmin(pmax(a, 0), 1)
  # JPEG has no alpha channel: an RGBA array would be refused outright.
  if (is_jpeg && length(dim(a)) == 3L && dim(a)[3] == 4L)
    a <- a[, , seq_len(3), drop = FALSE]

  bak_dir <- file.path(dirname(path), "_originaux")
  bak <- file.path(bak_dir, basename(path))
  if (!file.exists(bak)) {
    dir.create(bak_dir, showWarnings = FALSE, recursive = TRUE)
    file.copy(path, bak, overwrite = FALSE)
  }

  tmp <- paste0(path, ".tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  if (is_jpeg) {
    if (!requireNamespace("jpeg", quietly = TRUE))
      stop("Writing a JPEG needs the 'jpeg' package.", call. = FALSE)
    jpeg::writeJPEG(a, tmp, quality = quality)
  } else {
    if (!requireNamespace("png", quietly = TRUE))
      stop("Writing a PNG needs the 'png' package.", call. = FALSE)
    png::writePNG(a, tmp)
  }
  if (!file.rename(tmp, path))
    stop("Could not replace ", path, call. = FALSE)
  list(path = path, backup = bak, jpeg = is_jpeg)
}

.fm_species_key <- function(genus_species) {
  x <- gsub("[^A-Za-z]+", "_", genus_species)          # "Genus.species" -> genus_species
  tolower(gsub("^_|_$", "", x))
}

# --- intake of a new photograph ----------------------------------------------
# Crop, rotate and flip are applied to the raw ARRAY and baked into the file
# BEFORE the photograph enters the queue. That order is not a convenience: the
# digitizer records coordinates in the pixels OF THE FILE, so re-framing a
# picture that already carries landmarks would silently invalidate every one of
# them. The intake page is the only place where the geometry of the image may
# still change, and it is upstream of the first click by construction.

# rotation by a multiple of 90 degrees, counter-clockwise on screen. Written on
# the array rather than delegated to magick: a quarter turn is a transposition
# and a row reversal, exact and lossless, where a re-encode through an external
# library would resample the picture for nothing.
.fm_rot90 <- function(a, k = 1L) {
  k <- as.integer(k) %% 4L
  if (is.na(k) || !k) return(a)
  rot1 <- function(x) {
    d <- dim(x)
    if (length(d) == 3L) {
      out <- array(x[1], dim = c(d[2], d[1], d[3]))
      for (ch in seq_len(d[3])) out[, , ch] <- t(x[, , ch])[d[2]:1, , drop = FALSE]
      out
    } else t(x)[d[2]:1, , drop = FALSE]
  }
  for (i in seq_len(k)) a <- rot1(a)
  a
}

# mirror: "h" left/right, "v" top/bottom, "hv" both.
.fm_mirror <- function(a, mode = "h") {
  d <- dim(a); H <- d[1]; W <- d[2]
  if (length(d) == 3L) {
    if (grepl("h", mode)) a <- a[, W:1, , drop = FALSE]
    if (grepl("v", mode)) a <- a[H:1, , , drop = FALSE]
  } else {
    if (grepl("h", mode)) a <- a[, W:1, drop = FALSE]
    if (grepl("v", mode)) a <- a[H:1, , drop = FALSE]
  }
  a
}

# crop to a rectangle given in DISPLAY pixels (x to the right, y downwards from
# the top-left corner), the frame the brush of the preview returns. The
# rectangle is clamped to the picture and refused below 8 px a side: a stray
# click must not turn a specimen into a two-pixel smear.
.fm_crop <- function(a, x0, x1, y0, y1, min_px = 8L) {
  d <- dim(a); H <- d[1]; W <- d[2]
  cx <- sort(c(x0, x1)); cy <- sort(c(y0, y1))
  c0 <- max(1L, floor(cx[1]) + 1L); c1 <- min(W, ceiling(cx[2]))
  r0 <- max(1L, floor(cy[1]) + 1L); r1 <- min(H, ceiling(cy[2]))
  if (!is.finite(c0) || !is.finite(c1) || !is.finite(r0) || !is.finite(r1) ||
      c1 - c0 + 1L < min_px || r1 - r0 + 1L < min_px) return(NULL)
  if (length(d) == 3L) a[r0:r1, c0:c1, , drop = FALSE] else a[r0:r1, c0:c1, drop = FALSE]
}

# TRUE when the file really is a PNG, whatever its extension says (about 7 % of
# the ".jpg" of this collection are not JPEG -- see read_img()).
.fm_is_png_file <- function(path) {
  sig <- tryCatch(readBin(path, "raw", n = 8L), error = function(e) raw(0))
  length(sig) >= 8L &&
    all(sig[1:8] == as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)))
}

# writes an array to a NEW file, the format taken from the target extension and
# the write made atomic by a rename inside the same directory (see
# .fm_write_img for why tempdir() would not do). No backup here: the target
# does not exist yet -- .fm_new_photo_path() guarantees it.
.fm_write_img_as <- function(a, path, quality = 0.97) {
  a[!is.finite(a)] <- 0
  a[] <- pmin(pmax(a, 0), 1)
  is_png <- grepl("\\.png$", path, ignore.case = TRUE)
  if (!is_png && length(dim(a)) == 3L && dim(a)[3] == 4L)
    a <- a[, , seq_len(3), drop = FALSE]           # JPEG carries no alpha channel
  tmp <- paste0(path, ".tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  if (is_png) {
    if (!requireNamespace("png", quietly = TRUE))
      stop("Writing a PNG needs the 'png' package.", call. = FALSE)
    png::writePNG(a, tmp)
  } else {
    if (!requireNamespace("jpeg", quietly = TRUE))
      stop("Writing a JPEG needs the 'jpeg' package.", call. = FALSE)
    jpeg::writeJPEG(a, tmp, quality = quality)
  }
  if (!file.rename(tmp, path))
    stop("Could not write ", path, call. = FALSE)
  path
}

# file name of a new photograph: "Genus_species.<ext>", suffixed 2, 3... when
# the folder already holds that species. The suffix is a SPECIMEN counter, not
# a duplicate marker -- the "new" queue is one entry per PHOTOGRAPH, several
# specimens of one species are legitimate, and both .fm_name_from_file() and
# .fm_photo_index() strip a trailing number, so all of them read back to the
# same binomial. The name is the only identity a raw image has, which is why it
# is normalised here rather than left to whatever the camera produced.
.fm_new_photo_path <- function(dir, genus_species, ext = "jpg") {
  base <- gsub("[^A-Za-z]+", "_", trimws(genus_species))
  base <- gsub("^_+|_+$", "", base)
  if (!nzchar(base)) return(NA_character_)
  ext <- sub("^\\.", "", tolower(ext))
  cand <- file.path(dir, paste0(base, ".", ext))
  k <- 1L
  while (file.exists(cand) && k < 999L) {
    k <- k + 1L
    cand <- file.path(dir, paste0(base, "_", k, ".", ext))
  }
  cand
}

# a binomial, checked on its FORM alone: two words, genus capitalised, epithet
# in lower case. Returns the normalised name, or NA with the reason attached.
# FishBase is a separate, optional and non-blocking step: a name absent from
# FishBase may still be a specimen worth digitizing, whereas "abramis  Brama"
# is a typing accident in every case.
.fm_check_binomial <- function(x) {
  x <- trimws(gsub("[_.]+", " ", as.character(x %||% "")))
  x <- gsub("\\s+", " ", x)
  w <- strsplit(x, " ")[[1]]
  w <- w[nzchar(w)]
  if (length(w) < 2L)
    return(list(ok = FALSE, name = x,
                msg = "Two words are needed: Genus species."))
  if (length(w) > 3L)
    return(list(ok = FALSE, name = x,
                msg = "Too many words for a binomial (three at most, sp. included)."))
  if (grepl("[^A-Za-z]", paste(w, collapse = "")))
    return(list(ok = FALSE, name = x,
                msg = "Letters only (no digit, no accent, no hyphen)."))
  w[1] <- paste0(toupper(substr(w[1], 1, 1)), tolower(substring(w[1], 2)))
  w[-1] <- tolower(w[-1])
  list(ok = TRUE, name = paste(w, collapse = " "), msg = "")
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
                tip = "5 takes the coordinates of 13: the eye reaches the dorsal profile"),
  # A second KIND of rule: not one point onto another, but one point onto a
  # LINE. The statement is the same in nature -- a distance declared zero -- but
  # the partner is the mid axis 22-24 rather than a landmark, so it is expressed
  # as a PROJECTION and not as a copy of coordinates. 4 is the master of the
  # belly line: putting it on the axis carries 11, then 8 and 9, with it.
  Bd4 = list(moves = list(), project = 4L,
             label = "4 on 22-24 (belly on the mid axis)",
             tip = paste("4 is projected perpendicularly onto the line (22, 24):",
                         "it keeps its abscissa along the axis, its height",
                         "becomes zero, and 11 then 8/9 follow it"))
)

# ---- the mid axis, as the conventions themselves see it ---------------------
# Frame of the MIDDLE segment, 22 -> 24, resolved EXACTLY as .fm_constrain()
# resolves it -- 22 falling back to 1, 24 falling back to 2 when a hinge is not
# placed. Reusing the same fallback is what guarantees that a point projected
# here sits at height zero in the very frame the conventions then work in;
# resolving it any other way would let the belly line be rebuilt in a frame
# where 4 is no longer on the axis. NULL when the direction is degenerate.
.fm_mid_frame <- function(P) {
  fin <- function(i) i <= nrow(P) && all(is.finite(P[i, ]))
  o   <- if (fin(22L)) P[22L, ] else if (fin(1L)) P[1L, ] else return(NULL)
  tip <- if (fin(24L)) P[24L, ] else if (fin(2L)) P[2L, ] else return(NULL)
  d <- tip - o; L <- sqrt(sum(d^2))
  if (!is.finite(L) || L == 0) return(NULL)
  d <- d / L
  list(o = o, u = d, n = c(d[2], -d[1]))
}
# ORTHOGONAL projection onto that axis: the point keeps its abscissa and its
# height is set to zero. The line is NOT bounded by 22 and 24 -- the foot of the
# perpendicular may fall on the prolongation of the segment, exactly as
# .fm_constrain() lets a point live outside the segment it is framed by.
.fm_project_mid <- function(P, pt) {
  if (pt > nrow(P) || !all(is.finite(P[pt, ]))) return(P)
  fr <- .fm_mid_frame(P); if (is.null(fr)) return(P)
  P[pt, ] <- fr$o + sum((P[pt, ] - fr$o) * fr$u) * fr$u
  P
}
# Distance from a point to the mid axis, in pixels (NA when undefined): what a
# projection rule reads back off the coordinates of a specimen saved earlier.
.fm_dist_mid <- function(P, pt) {
  if (pt > nrow(P) || !all(is.finite(P[pt, ]))) return(NA_real_)
  fr <- .fm_mid_frame(P); if (is.null(fr)) return(NA_real_)
  abs(sum((P[pt, ] - fr$o) * fr$n))
}
# Below this distance a point IS on the axis: the coordinates written to the
# workbook are the projection itself, so only rounding separates them from it.
.FM_PROJ_TOL <- 0.5

# Apply the active rules, move by move and in order. A move whose reference is
# not placed is skipped: a zero is only meaningful once the point it is measured
# from exists. Order matters when several rules are on -- with both `Mo` and
# `6 = 8`, 9 goes onto 1 first, so 23 then follows 9 to the same place.
# `kinds` selects which half of a rule is applied: the PROJECTIONS have to act
# BEFORE the conventions (4 drives the belly line, so 11, 8 and 9 must be
# re-derived from the projected 4), the point-to-point moves after them (see the
# comment at the end of recon()).
.fm_apply_collapse <- function(P, active, kinds = c("move", "project")) {
  if (is.null(P) || !length(active)) return(P)
  for (nm in intersect(active, names(.FM_COLLAPSE))) {
    r <- .FM_COLLAPSE[[nm]]
    if ("project" %in% kinds && !is.null(r$project))
      for (pt in r$project) P <- .fm_project_mid(P, pt)
    if ("move" %in% kinds)
      for (mv in r$moves) {
        if (mv[1] > nrow(P) || mv[2] > nrow(P)) next
        if (all(is.finite(P[mv[2], ]))) P[mv[1], ] <- P[mv[2], ]
      }
  }
  P
}
# The points a set of rules places. `kinds` separates the two families, and the
# distinction is not cosmetic: a point COPIED onto another owes it everything,
# so the rule takes over its position (its override is dropped); a PROJECTED
# point keeps the abscissa the operator clicked and only surrenders its height,
# so its override must survive -- dropping it would silently send 4 back to its
# seeded position along the body.
.fm_collapse_points <- function(active, kinds = c("move", "project")) {
  if (!length(active)) return(integer(0))
  pts <- unlist(lapply(.FM_COLLAPSE[intersect(active, names(.FM_COLLAPSE))],
                       function(r) c(
                         if ("move" %in% kinds)
                           vapply(r$moves, function(m) m[1], integer(1))
                         else integer(0),
                         if ("project" %in% kinds && !is.null(r$project))
                           as.integer(r$project) else integer(0))),
                use.names = FALSE)
  if (is.null(pts)) integer(0) else pts
}
# The extreme-point convention a rule deliberately SUSPENDS. Declaring 4 on the
# mid axis states that the ventral profile is not what 4 reads on this specimen;
# the ventral half of the 3/4 test would then flag every point below the axis
# (6, 10, 14 ...) on every save -- the rule working, not an error. The dorsal
# half, on 3, is untouched and still holds.
.fm_collapse_skip_extreme <- function(active) {
  if ("Bd4" %in% active) 4L else integer(0)
}
# Read the rules back off the COORDINATES: a specimen reopened must show the
# statement it was saved with. Two readings, one per kind of rule --
#   * projection : the point sits on the mid axis, so the distance to that axis
#                  is what identifies it;
#   * copy       : the two points of the pair are at the same place.
# The declaration is also recorded explicitly in the workbook and the journal
# (column `collapse_rules`), and that record is the one that counts for anything
# saved by this version; this function is what makes the thousands of specimens
# entered BEFORE it -- and any table coming from elsewhere -- still speak. The
# two are unioned on reload.
#
# Only the FIRST pair of a copy rule is tested: it is the statement itself
# ("9 on 1" = the mouth opens on the ventral profile), the others being its
# consequences (23 follows 1, and so on). A consequence left NA by an older
# entry would otherwise hide the statement that produced it.
.fm_collapse_detect <- function(P, tol = .FM_PROJ_TOL) {
  if (is.null(P)) return(character(0))
  nm <- names(.FM_COLLAPSE)
  nm[vapply(.FM_COLLAPSE, function(r) {
    if (!is.null(r$project)) {
      d <- vapply(r$project, function(pt) .fm_dist_mid(P, pt), numeric(1))
      return(all(is.finite(d)) && all(d <= tol))
    }
    if (!length(r$moves)) return(FALSE)
    m <- r$moves[[1]]
    if (max(m) > nrow(P) || !all(is.finite(P[m, ]))) return(FALSE)
    sqrt(sum((P[m[1], ] - P[m[2], ])^2)) <= tol
  }, logical(1))]
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
# `skip` suspends the test on one side: a coincidence rule that puts 4 on the
# mid axis makes the ventral half of the convention meaningless, and reporting
# it would turn a declared statement into an alert on every save.
.fm_extreme_violations <- function(P, tol_frac = .FM_EXTREME_TOL,
                                   skip = integer(0)) {
  g <- .fm_body_frame(P); if (is.null(g)) return(NULL)
  tol  <- max(.FM_EXTREME_FLOOR, tol_frac * g$L)
  cand <- setdiff(seq_len(nrow(P)), c(3L, 4L, .FM_EXTREME_EXCLUDE))
  cand <- cand[is.finite(g$no[cand])]
  if (!length(cand)) return(NULL)
  out <- list()
  if (!(3L %in% skip)) {
    d <- g$sgn * (g$no[cand] - g$no[3])         # overshoot on the DORSAL side
    k <- which.max(d)
    if (d[k] > tol) out[[length(out) + 1L]] <-
      data.frame(point = 3L, culprit = cand[k], delta = unname(d[k]),
                 kind = "extreme", stringsAsFactors = FALSE)
  }
  if (!(4L %in% skip)) {
    d <- g$sgn * (g$no[4] - g$no[cand])         # overshoot on the VENTRAL side
    k <- which.max(d)
    if (d[k] > tol) out[[length(out) + 1L]] <-
      data.frame(point = 4L, culprit = cand[k], delta = unname(d[k]),
                 kind = "extreme", stringsAsFactors = FALSE)
  }
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
.fm_convention_violations <- function(P, tol_frac = .FM_EXTREME_TOL,
                                      skip = integer(0)) {
  v <- rbind(.fm_extreme_violations(P, tol_frac, skip = skip),
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
#' pectoral fin inserted on the belly. Five rules are offered: `Mo = 0` (9, and
#' 23, take the coordinates of 1), `6 = 8` (the bottom of the head is the body
#' underside, and 23 follows 9), `PFi = 0` (10 takes the coordinates of 11),
#' `5 = 13` (an eye reaching the top of the head) and `4 on 22-24` (the belly
#' point of the body depth lies on the mid axis).
#'
#' The last one is of a different KIND: the partner is not a landmark but a
#' LINE, so 4 is not copied onto anything, it is PROJECTED perpendicularly onto
#' the segment 22-24 -- it keeps the abscissa the operator clicked along the
#' axis and its height becomes zero. The line is not bounded by its two hinges:
#' the foot of the perpendicular may fall on their prolongation, as everywhere
#' else in the constrained editing. Because 4 is the master of the belly line,
#' this rule is applied BEFORE the conventions rather than after them, so that
#' 11, then 8 and 9, are re-derived from the projected 4; it is replayed at the
#' end, where it is idempotent. Declaring it also suspends the VENTRAL half of
#' the extreme-point check on save: 4 no longer claims to be the most ventral
#' point, so reporting 6, 10 or 14 below it would flag the rule itself. The
#' dorsal half, on 3, is untouched.
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
#' A declaration is SAVED with the specimen, in the column `collapse_rules` of
#' the target sheet and in the journal, as the list of rule identifiers
#' (`"Mo;Hd6"`). Reopening the specimen puts the boxes back. It has to be
#' written down as such: a copy rule leaves nothing in the coordinates that
#' distinguishes it from a chance coincidence, and an unticked box cannot be
#' told from a box that was never ticked. Before this column existed the
#' statement lived only in the geometry it produced, so a reopened specimen came
#' back with its points collapsed but its boxes empty -- and the first click,
#' with the rule no longer applied, quietly undid the zero. For everything
#' entered then, and for any table coming from elsewhere, the rules are still
#' read back off the coordinates -- a pair of coincident points, a point on the
#' mid axis -- and the two readings are unioned.
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
#' @section Quality and review of an entry:
#' The `"Quality"` tab of the side panel carries two fields the coordinates
#' cannot express: a SCORE from 1 (unusable, landmarks largely guessed) to 5
#' (excellent -- whole fish, strictly lateral, every landmark unambiguous), and
#' a TICK declaring the specimen checked. They answer two different questions,
#' hence two fields: how good the entry is, and whether anyone has actually
#' looked at it. A specimen can perfectly well be checked AND poor -- that is
#' the state a re-photographing list is built from.
#'
#' `"Not scored"` and an empty cell are the same statement, and neither is a
#' score of zero. Both fields are reloaded with the specimen and rewritten at
#' every save, so returning to a species and saving it again preserves the
#' review it already carries; the author and the date are stamped only when
#' something is actually declared, otherwise "nobody has looked at it" would be
#' indistinguishable from "somebody looked and said nothing".
#'
#' They are written to four columns of the target sheet -- `quality_score`,
#' `reviewed`, `reviewed_by`, `review_date`, created on the fly like the hinge
#' columns, next to the `collapse_rules` of the declared coincidences -- and, at
#' record level, to the journal, from which
#' [fishmorph_consolidate()] brings them back. The journal already said how each
#' POINT was obtained (`placed`, `seeded`, `derived`...); this says what the
#' ENTRY as a whole is worth, which only the operator looking at the photograph
#' can decide.
#'
#' @section Bringing a new photograph in ("New species" page):
#' Until now a photograph entered a session only by being dropped into
#' `new_photo_dir` from a file manager, before launch, under whatever name the
#' camera had given it. That is not a detail of housekeeping: the file name is
#' the ONLY identity an image has before it is measured -- the species is read
#' off it (`.fm_name_from_file()`) and the workbook rows are matched on it -- so
#' the step belongs to the protocol and it belongs in the application.
#'
#' The second tab of the main panel does it in four moves, in the only order
#' that is safe. **Browse** for the file (JPEG, PNG, GIF, BMP or TIFF; the real
#' format is read from the magic bytes, not from the extension, since about
#' 7 per cent of the `.jpg` of this collection are not JPEG). **Name** the
#' specimen `Genus species`, pre-filled from the file name and checkable against
#' FishBase. **Frame** it -- quarter turns, mirrors, and a crop drawn with the
#' mouse on the preview. **Commit**, which writes the picture and opens it
#' straight away in the `"new"` queue.
#'
#' The framing comes before the first click and must never come after it. The
#' digitizer records coordinates in the pixels OF THE FILE, so cropping or
#' rotating a photograph that already carries landmarks would move every one of
#' them without touching a single recorded number. This page is the one place
#' where the geometry of an image may still change, and it is upstream of the
#' first click by construction. Quarter turns and crops are exact array
#' operations, lossless by nature; the output format follows the real bytes of
#' the source, so a PNG stays a PNG rather than acquiring JPEG artefacts on the
#' very pixels the landmarks are read on.
#'
#' The copy is named `Genus_species.<ext>`, suffixed `_2`, `_3`... when the
#' folder already holds that species -- a SPECIMEN counter, not a duplicate
#' marker: the `"new"` queue is one entry per PHOTOGRAPH, several specimens of
#' one species are legitimate, and both readers of the file name strip a
#' trailing number, so all of them come back to the same binomial. The file the
#' operator selected is left untouched where it was, and a copy of it is kept
#' under `_originaux/` beside the queue: what enters the queue has been cropped
#' and turned, and those pixels are gone.
#'
#' NOTHING is written to the workbook here. A row of `new_sheet` is the record
#' of a MEASUREMENT, and a photograph nobody has digitized has no measurement to
#' declare; the row is created by "Save & next", keyed on `photo_file`, exactly
#' as for a photograph dropped in the folder by hand. An empty row written at
#' intake would be indistinguishable from a specimen whose landmarks all came
#' out `NA`.
#'
#' The queue is rebuilt on the spot -- the alternative being to close the
#' application and lose the journal position for the sake of one photograph.
#'
#' @section Order of the queues:
#' The `"reconstruct"` and `"correct"` queues run in ALPHABETICAL order of the
#' species, not in the order of the workbook rows -- which is an accident of how
#' the sheet was assembled. The order of the queue is the order of the work:
#' "Save & next" hands over the next NAME, so a session walks the
#' classification instead of jumping from one unrelated fish to another.
#' Congeners then arrive together -- the same eye, the same fin, the same
#' ambiguities -- and correcting one *Barbus* puts the whole genus in the
#' operator's hand while the criteria are still fresh. Sorting is done in the C
#' locale, so the order is identical on every workstation. The `"new"` queue
#' keeps the alphabetical order of the photograph FILE names, the only identity
#' those images have before they are named.
#'
#' The field to the right of the toolbar reaches any species of the current
#' queue by name; its list is the queue itself, hence alphabetical, and the
#' search results keep that order instead of being ranked by match score.
#'
#' @param xlsx_path Path of the master workbook (2 sheets).
#' @param photo_dir Photograph folder (stays local).
#' @param out_path  Path of the output copy. NULL -> "<master>_reconstructed.xlsx"
#'   in the same folder. The copy is created if absent; otherwise work resumes
#'   on it (species already recorded are excluded from the queue).
#' @param seg_sheet,lm_sheet Sheet names.
#' @param new_sheet Sheet the NEW specimens are appended to ("new" mode).
#'   Created (with the headers of `lm_sheet`) if it does not exist. Its rows
#'   ALSO feed the "correct" queue, so a species entered here -- through the
#'   "new" queue or through the "Absent from FISHMORPH" panel of FishInTrait --
#'   can be reopened and corrected instead of staying invisible until someone
#'   promotes it to `lm_sheet`. Corrections go back to the sheet the row came
#'   from. NAME IT AS THE WORKBOOK DOES: the match is exact, and a workbook
#'   carrying `New_specimen` opened with the default `"new_specimens"` gets a
#'   SECOND, empty sheet rather than the one it already has.
#' @param new_photo_dir Folder of the new specimens' photographs ("new" mode).
#'   Every image in the folder forms the queue. It may not exist: the "new"
#'   mode is then simply unavailable until the "New species" page puts a
#'   photograph in it, which creates the folder.
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
#'   digitized from the segments), "correct" (specimens ALREADY landmarked, on
#'   `lm_sheet` OR on `new_sheet`, to be reviewed/corrected: the 21 points are
#'   reloaded from the workbook) or
#'   "new" (new photographs from `new_photo_dir`, appended to `new_sheet`).
#'   Switchable at any moment through the "Queue" selector in the app. If the
#'   requested queue is empty, the app starts on another one.
#' @param launch.browser Where the application opens. `TRUE` (default) or
#'   `"browser"` forces the system browser, past the RStudio Viewer pane --
#'   which is a few hundred pixels wide and the one place an application built
#'   for clicking nineteen points on a photograph must not open. `"viewer"`
#'   restores the pane, `FALSE` opens nothing and prints the URL, and a
#'   function is used as given.
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
    mode      = c("reconstruct", "correct", "new"),
    launch.browser = TRUE) {

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
  # --- what an entry says about itself ---------------------------------------
  # An entry is not only a set of coordinates: it is also worth something, and
  # that judgement belongs BESIDE the coordinates it qualifies. Four columns,
  # created on the fly like the hinges: the score (1-5), the checked flag, and
  # WHO declared it WHEN -- a review with no author and no date is an opinion
  # that cannot be audited.
  # `collapse_rules` joins them: the coincidences DECLARED on the specimen, as
  # the list of rule identifiers ("Mo;Hd6"). Until now the declaration only
  # existed in the geometry it produced, and the copy rules left nothing a
  # reader could tell from an ordinary coincidence -- reopening a specimen
  # brought its points back but not the statement that had put them there, so
  # the boxes came back empty and the next click undid the zero in silence.
  entry_cols <- c("quality_score", "reviewed", "reviewed_by", "review_date",
                 "collapse_rules")
  new_lm_hdr <- ensure_cols(lm_sheet, lm_hdr, c(hinge_cols, entry_cols))
  # rep(...) and not a bare NA: on an empty sheet (0 rows) df[[cc]] <- NA fails.
  # The review columns are created as CHARACTER: an author and a date are text,
  # and a logical column would refuse the string written back into it.
  for (cc in setdiff(new_lm_hdr, lm_hdr))
    lm_df[[cc]] <- if (cc %in% entry_cols) rep(NA_character_, nrow(lm_df))
                   else rep(NA_real_, nrow(lm_df))
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
  # `IUCN` is created here although the digitizer never writes it: a species
  # absent from the segment table has no threat status anywhere, and the sheet
  # it lives on is the only place one can be recorded. The column exists so
  # that the FishInTrait panel has somewhere to put what the operator types,
  # and so that build_fishmorph_landmark_table() finds it there.
  need_new <- c("Genus.species",
                as.vector(rbind(paste0(save_pts_new, "_X"), paste0(save_pts_new, "_Y"))),
                "photo_file", "ruler_mm", "mm_per_px", "IUCN", entry_cols)
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

  # --- where a row of `lm_df` actually LIVES ---------------------------------
  # `lm_df` used to be a view of ONE sheet, so a queue position was a row number
  # and the destination of a save was implicit. It now holds rows coming from
  # two sheets, and the destination has to travel WITH the row: `.sheet` names
  # the sheet, `.srow` the row inside it. Nothing else in the application needs
  # to know -- the display, the reloading of the points and the photograph
  # index all work on `lm_df` and are unchanged.
  lm_df$.sheet <- rep(lm_sheet, nrow(lm_df))
  lm_df$.srow  <- seq_len(nrow(lm_df))
  # `photo_file` is a column of the new-specimen sheet only; carried here under
  # a dot name so that it travels with the row without being mistaken for a
  # column of `lm_sheet` that the save path would try to write back.
  lm_df$.photo_file <- rep(NA_character_, nrow(lm_df))

  # --- the new specimens join the "correct" queue ----------------------------
  # A species entered by the "Absent from FISHMORPH" panel of FishInTrait, or
  # digitized through the "new" queue, lands on `new_sheet` and NOT on
  # `lm_sheet`. It was then invisible in every queue: `reconstruct` and
  # `correct` are built on the rows of `lm_sheet`, and `new` lists the FILES of
  # `new_photo_dir`, where its photograph is not -- FishInTrait files it with
  # the others, under `photo_dir`. A specimen already carrying its twenty-one
  # points does not need the intake queue; it needs to be re-openable and
  # correctable like any other, which is exactly what `correct` is for.
  #
  # A species already held by `lm_sheet` is NOT taken from `new_sheet`: the
  # staging row has been promoted, and the published row is the one to correct.
  #
  # The queue is built ONCE, at start-up, like the other two: a photograph
  # brought in through the "New species" page during the session appears in the
  # "new" queue, not in "correct", and only the next launch moves it over. That
  # is deliberate -- rebuilding the queue under the operator would renumber the
  # positions they are navigating with.
  if (nrow(new_df)) {
    anchor_new <- suppressWarnings(as.numeric(new_df[["1_X"]]))
    cand <- which(!is.na(anchor_new))
    if (length(cand)) {
      # the type of each column is taken from `lm_df`: `new_df` is entirely
      # character (see above), and rbind()ing it as it stands would silently
      # turn the coordinate columns of the whole table into text.
      like <- function(target, v)
        if (is.numeric(target)) suppressWarnings(as.numeric(v))
        else if (is.logical(target)) as.logical(v)
        else as.character(v)
      add <- as.data.frame(
        lapply(lm_df, function(cc) rep(cc[NA_integer_], length(cand))),
        stringsAsFactors = FALSE)
      names(add) <- names(lm_df)
      for (nm in intersect(names(lm_df), names(new_df)))
        add[[nm]] <- like(lm_df[[nm]], new_df[[nm]][cand])
      add$.sheet <- rep(new_sheet, length(cand))
      add$.srow  <- cand
      add$.key   <- .fm_species_key(add$Genus.species)
      add$.photo_file <- if ("photo_file" %in% names(new_df))
        trimws(as.character(new_df$photo_file[cand])) else NA_character_
      seen <- (add$.key %in% lm_df$.key) | is.na(add$.key)
      if (any(seen))
        message(sprintf(paste("%d row(s) of '%s' already held by '%s'",
                              "(or unnamed): left out of the queue."),
                        sum(seen), new_sheet, lm_sheet))
      add <- add[!seen, , drop = FALSE]
      if (nrow(add)) {
        lm_df <- rbind(lm_df, add)
        message(sprintf("%d new specimen(s) from '%s' added to the 'correct' queue.",
                        nrow(add), new_sheet))
      }
    }
  }

  seg_cols <- c("Bl", "Bd", "Hd", "Eh2", "Mo2", "PFi2", "PFl", "Ed", "Jl", "CPd", "CFd")
  has_seg  <- stats::complete.cases(seg_df[, "Bl", drop = FALSE]) &
              !is.na(suppressWarnings(as.numeric(seg_df$Bl)))
  xcols <- paste0(.FM_LM_PTS, "_X")
  lm_missing <- apply(lm_df[, xcols, drop = FALSE], 1, function(r) all(is.na(r)))

  # --- the photograph of a row, resolved ONCE --------------------------------
  # The species key is the right link for `lm_sheet`, where one row is one
  # species. It is the WRONG one for `new_sheet`, where one row is one
  # PHOTOGRAPH: the plate mode puts several specimens of the same species on
  # their own rows, and keying on the species would show all of them the same
  # picture. `photo_file`, which the digitizer records precisely for that, is
  # therefore preferred whenever it is filled and the file can be found -- in
  # `photo_dir` or in `new_photo_dir`, the specimen having passed through both.
  # The species index remains the fallback: a row written by the FishInTrait
  # panel carries no `photo_file`, only a name.
  photo_of_file <- function(f) {
    if (is.na(f) || !nzchar(f)) return(NA_character_)
    for (d in c(photo_dir, new_photo_dir)) {
      p <- file.path(d, f)
      if (file.exists(p)) return(p)
    }
    NA_character_
  }
  lm_photo <- unname(photos[lm_df$.key])          # NA for an unknown key
  for (i in which(!is.na(lm_df$.photo_file))) {
    p <- photo_of_file(lm_df$.photo_file[i])
    if (!is.na(p)) lm_photo[i] <- p
  }
  lm_df$.photo <- lm_photo
  has_photo  <- !is.na(lm_df$.photo)

  seg_by_key <- seg_df[has_seg, ]
  seg_by_key <- seg_by_key[!duplicated(seg_by_key$.key), ]
  rownames(seg_by_key) <- seg_by_key$.key

  # TWO queues, chosen at launch (the `mode` argument) and switchable with the
  # "Queue" selector in the app:
  #   * reconstruct : species WITHOUT landmarks, WITH segments and a photograph
  #   * correct     : specimens ALREADY landmarked, WITH a photograph (to review
  #                   / correct; the 21 points are reloaded from the workbook,
  #                   NOT reconstructed from the segments). Rows of `lm_sheet`
  #                   AND of `new_sheet`: a species added since the publication
  #                   is corrected like any other, and each goes back to the
  #                   sheet it came from (`.sheet` / `.srow`).
  q_recon <- which(lm_missing &
                     lm_df$.key %in% rownames(seg_by_key) & has_photo)
  q_corr  <- which(!lm_missing & has_photo & !is.na(lm_df$.key))
  # Both queues run in ALPHABETICAL order of the species and not in the order of
  # the workbook rows, which is an accident of how the sheet was assembled. The
  # order of the queue IS the order of the work: "Save & next" hands over the
  # next name, so a session walks the classification instead of jumping from one
  # unrelated fish to another. Congeners then arrive together -- the same eye,
  # the same fin, the same ambiguities -- and correcting one Barbus puts the
  # whole genus in the operator's hand while the criteria are still fresh.
  # method = "radix": collation in the C locale, hence the SAME order on every
  # workstation, where the locale order would depend on the machine.
  alpha_order <- function(rows) rows[order(lm_df$Genus.species[rows],
                                           method = "radix")]
  q_recon <- alpha_order(q_recon)
  q_corr  <- alpha_order(q_corr)

  # the "new" queue: every image in the new-photographs folder. One entry = ONE
  # photograph (and not one species): several specimens of the same species are
  # therefore possible, each on its own row of `new_sheet`.
  new_photos <- if (dir.exists(new_photo_dir))
    sort(list.files(new_photo_dir, full.names = TRUE,
                    pattern = "\\.(jpe?g|png|gif|bmp|tiff?)$", ignore.case = TRUE))
  else character(0)
  q_new <- seq_along(new_photos)

  # Three empty queues is no longer a fatal condition. It used to be, because
  # nothing could then be done in the application; the "New species" page is
  # precisely the answer to "there is nothing to digitize yet" -- browse a
  # photograph, name it, and the "new" queue exists. Refusing to open would
  # send the operator back to the file manager for the one case the page was
  # written for.
  if (!length(q_recon) && !length(q_corr) && !length(q_new))
    message("No usable species yet (nothing to reconstruct, nothing to correct ",
            "with a photograph, no photograph in '", new_photo_dir, "'). ",
            "Use the \"New species\" page to bring one in.")
  # if the requested queue is empty, we fall back to the first non-empty one
  qlen_of <- function(m) switch(m, reconstruct = length(q_recon),
                                correct = length(q_corr), new = length(q_new), 0L)
  if (!qlen_of(mode)) {
    alt <- c("reconstruct", "correct", "new")
    alt <- alt[vapply(alt, function(m) qlen_of(m) > 0, logical(1))][1]
    # every queue empty: we start on "new", the only one the intake page fills.
    if (is.na(alt)) alt <- "new"
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

  # choices for the direct-access field (value = position in the current queue).
  # No sorting here: the queues are ALREADY in alphabetical order (see
  # alpha_order above), so the list of the field and the order in which
  # "Save & next" hands the species over are one and the same thing -- a second
  # sort would only let them drift apart.
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

    # --- quality and review, once per specimen --------------------------------
    # The journal already says how each POINT was obtained (placed, seeded,
    # derived...); nothing said how good the ENTRY was, and that judgement --
    # a blurred photograph, a folded fin, a fish seen from three-quarters --
    # only the operator looking at the picture can make. Two fields, because
    # they answer two different questions: how good is it (1-5), and has anyone
    # actually looked at it (the tick). A specimen can be checked AND poor.
    shiny::tabPanel(
      "Quality",
      shiny::radioButtons(
        "quality", "Quality score of the entry",
        choices = c("Not scored" = "0", "1" = "1", "2" = "2", "3" = "3",
                    "4" = "4", "5" = "5"),
        selected = "0", inline = TRUE),
      shiny::helpText("1 = unusable (specimen unreadable, landmarks largely",
                      "guessed), 2 = doubtful, 3 = acceptable, 4 = good,",
                      "5 = excellent (whole fish, strictly lateral, every",
                      "landmark unambiguous). \"Not scored\" leaves the cell",
                      "empty: an absent score and a bad score are not the same",
                      "statement."),
      shiny::checkboxInput("reviewed", "Species checked (reviewed)", FALSE),
      shiny::helpText("Tick it once the landmarks have been looked at one by",
                      "one on this photograph. Saved with the operator's name",
                      "and the date, so a second pass knows what has already",
                      "been examined and by whom."),
      shiny::hr(),
      shiny::uiOutput("review_info"),
      shiny::helpText("Both fields are RELOADED with the specimen and rewritten",
                      "at every save: coming back to a species and saving it",
                      "again does not erase the review it already carries. They",
                      "go to the workbook (columns quality_score, reviewed,",
                      "reviewed_by, review_date) and to the journal, like the",
                      "coordinates.")),

    shiny::tabPanel(
      "Display",
      shiny::checkboxInput("showlines", "Reference lines (outline/eye/belly)", TRUE),
      shiny::checkboxInput("fastdisp", "Fast display (lightened photograph)", TRUE),
      shiny::radioButtons("flip_mode", "Flip the photograph (+ landmarks)",
        c("None" = "none", "Horizontal" = "h", "Vertical" = "v", "180" = "hv"),
        selected = "none", inline = TRUE),
      shiny::radioButtons("flip_disp", "Flip the photograph ONLY (landmarks fixed)",
        c("None" = "none", "Horizontal" = "h", "Vertical" = "v", "180" = "hv"),
        selected = "none", inline = TRUE),
      shiny::helpText("The second option flips ONLY the display of the",
                      "photograph: the landmarks (and the record) do not move.",
                      "Useful when the loaded points are mirrored relative to the",
                      "photograph. It persists from one species to the next."),
      shiny::hr(),
      shiny::actionButton("flip_write", "Write the flip into the file",
                          class = "btn-warning btn-sm"),
      shiny::helpText("Writes the photograph AS DISPLAYED back to disk, so that",
                      "the file and the recorded landmarks agree for every other",
                      "reader -- launch_fishmorph_basins() draws the points on",
                      "the file, and only the file. The original is copied into",
                      "a '_originaux/' subfolder first, and both flip selectors",
                      "return to None: the flip is now IN the picture and",
                      "applying it again would undo it.")),

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
  # The whole panel is the state of a DIGITIZING session -- which queue, which
  # specimen, which conventions -- and none of it applies while a photograph is
  # merely being brought in. It is therefore hidden on the intake page rather
  # than left there greyed out: a control that cannot act on what is on screen
  # is an invitation to a mistake, not a reminder.
  side_panel <- shiny::conditionalPanel(
    "input.page != 'add'",
    shiny::div(
      class = "sidetabs",
      shiny::div(class = "modebar",
        shiny::radioButtons("mode", "Queue",
          c("To reconstruct" = "reconstruct", "Correct existing" = "correct",
            "New photographs" = "new"), selected = mode, inline = FALSE)),
      shiny::uiOutput("progress"), shiny::br(), side_tabs))

  digit_panel <- shiny::tagList(
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
          # sortField on the TEXT and not on selectize's default `$score`: a
          # search for "Barbus" must return the genus in alphabetical order,
          # not ranked by how well each name matches, otherwise the congeners
          # come back shuffled and the neighbours of a species tell you
          # nothing. Ties are broken by the (already alphabetical) server-side
          # order.
          options = list(placeholder = "Jump to a species...",
                         sortField = list(list(field = "text",
                                               direction = "asc")))))),
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

  # --- page "New species": bringing a photograph INTO the queue ---------------
  # Until now a photograph entered the session only by being dropped into
  # `new_photo_dir` from a file manager, before launch, under whatever name the
  # camera had given it -- and the file name is the ONLY identity an image has
  # before it is measured (.fm_name_from_file() reads the species off it, and
  # .fm_photo_index() matches the workbook rows on it). That step is therefore
  # part of the protocol, and it belongs in the application.
  #
  # Four moves, in the only order that is safe: choose the file, name the
  # specimen, FRAME it, then commit. The framing must come before the first
  # click and never after it: the digitizer records coordinates in the pixels of
  # the file, so cropping or rotating a photograph that already carries
  # landmarks would move every one of them without touching a single recorded
  # number. Once committed, the picture is frozen and the queue is rebuilt on
  # the spot -- the new specimen is at the end of the "New photographs" queue,
  # ready to be measured, without restarting the session.
  #
  # NOTHING is written to the workbook here. A row in `new_sheet` is the record
  # of a MEASUREMENT, and a photograph that has not been digitized has no
  # measurement to declare; the row is created by "Save & next", keyed on
  # `photo_file`. Writing an empty row at intake would put specimens in the
  # sheet that no one has looked at, indistinguishable from specimens whose
  # landmarks all came out NA.
  add_panel <- shiny::tagList(
    shiny::div(class = "sessionbar",
      "The photograph is COPIED into the new-photographs folder under a name ",
      "derived from the species; the file you pick is left untouched. ",
      shiny::tags$code(new_photo_dir)),
    shiny::fluidRow(
      shiny::column(5,
        card_box("1. Photograph",
          shiny::fileInput("add_file", NULL, multiple = FALSE, width = "100%",
            accept = c("image/jpeg", "image/png", "image/gif", "image/bmp",
                       "image/tiff", ".jpg", ".jpeg", ".png", ".gif", ".bmp",
                       ".tif", ".tiff"),
            buttonLabel = "Browse...", placeholder = "No file selected"),
          shiny::helpText("JPEG, PNG, GIF, BMP or TIFF. The picture is read by",
                          "its real bytes, not by its extension.")),
        card_box("2. Species name",
          shiny::textInput("add_species", NULL, "", width = "100%",
                           placeholder = "Genus species"),
          shiny::helpText("Pre-filled from the file name when it can be read.",
                          "It becomes the file name of the copy",
                          "(Genus_species.jpg) and, at the first save, the",
                          "Genus.species of the new-specimens sheet."),
          shiny::div(class = "tbgroup",
            shiny::actionButton("add_check", "Check against FishBase"),
            shiny::actionButton("add_accept", "Use the accepted name")),
          shiny::uiOutput("add_name_info")),
        card_box("4. Add to the queue",
          shiny::actionButton("add_commit", "Add and go to the specimen",
                              class = "btn-primary", width = "100%"),
          shiny::helpText("Writes the framed picture into the new-photographs",
                          "folder, rebuilds the queue and opens the specimen in",
                          "the \"New photographs\" queue. The workbook is",
                          "untouched until the first \"Save & next\"."),
          shiny::uiOutput("add_log"))),
      shiny::column(7,
        card_box("3. Framing",
          shiny::div(class = "toolbar",
            shiny::div(class = "tbgroup",
              shiny::actionButton("add_rotl", "\u21ba 90\u00b0"),
              shiny::actionButton("add_rotr", "90\u00b0 \u21bb")),
            shiny::div(class = "tbsep"),
            shiny::div(class = "tbgroup",
              shiny::actionButton("add_fliph", "Mirror \u2194"),
              shiny::actionButton("add_flipv", "Mirror \u2195")),
            shiny::div(class = "tbsep"),
            shiny::div(class = "tbgroup",
              shiny::actionButton("add_crop", "Crop to selection"),
              shiny::actionButton("add_reset", "Reset")),
            shiny::div(class = "tbhint",
              "Drag a rectangle on the picture, then \"Crop to selection\". ",
              "Every operation is applied to the COPY and baked into the file: ",
              "after the first landmark, the framing must not change.")),
          shiny::plotOutput("add_plot", height = "520px",
            brush = shiny::brushOpts("add_brush", resetOnNew = TRUE,
                                     opacity = 0.25, fill = "#2563eb",
                                     stroke = "#1d4ed8")),
          shiny::uiOutput("add_info"),
          shiny::helpText("A lateral view, head to the LEFT and dorsal side UP,",
                          "is what the seeded landmarks assume; any other",
                          "orientation is digitizable but starts further from",
                          "the fish. Crop close to the specimen -- the pixels",
                          "around it carry no measurement and only cost zoom.",
                          "Include the scale bar if there is one: points 20 and",
                          "21 are placed on it and give mm_per_px.")))))

  main_panel <- shiny::tabsetPanel(
    id = "page", type = if (has_bslib) "pills" else "tabs",
    shiny::tabPanel("Digitizing", value = "digit",
                    shiny::div(style = "padding-top:10px;", digit_panel)),
    shiny::tabPanel("New species", value = "add",
                    shiny::div(style = "padding-top:10px;", add_panel)))

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
      photostamp = 0L,       # incremented when the new-photographs FOLDER changes
                             # (a photograph brought in by the intake page):
                             # invalidates the "new" queue and its goto list
      flushstamp = 0L,       # incremented at every write of the workbook
      revstamp = 0L,         # incremented at every write of the review columns
                             # (lm_df is a plain variable, not a reactive one:
                             # without this counter the panel showing what the
                             # FILE says would never notice a second save of
                             # the same row)
      edited = integer(0),   # points MOVED by the user during this session
                             # (as opposed to points merely loaded from the workbook)
      adjusted = integer(0), # points snapped by the extreme-point convention
                             # (status "adjusted" in the journal)
      collapse = character(0))  # segments declared zero on THIS specimen

    # queue and direct-access list of the current mode. In "new" mode the queue
    # indexes the PHOTOGRAPHS of new_photo_dir (and not rows of lm_df).
    # `rv$photostamp` is read on purpose and its value thrown away: the "new"
    # queue is a plain variable rebuilt by the intake page (new_photos, q_new,
    # choices_new are reassigned with <<-), and without this dependency a
    # photograph added during the session would sit in the folder while the
    # queue kept its length from launch time.
    qrows    <- shiny::reactive({
      rv$photostamp
      switch(rv$mode, correct = q_corr, new = q_new, q_recon)
    })
    goto_now <- shiny::reactive({
      rv$photostamp
      switch(rv$mode, correct = choices_corr, new = choices_new, choices_recon)
    })
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
      # `.photo` was resolved row by row at start-up (photo_file first, species
      # key as fallback), so this no longer has to know which sheet the row
      # comes from.
      if (is_new()) new_photos[i] else lm_df$.photo[i]
    })
    # sheet and row the current record must be written back to. A row of the
    # published sheet and a row of the new-specimen sheet are corrected in the
    # same queue but do NOT go to the same place.
    cur_sheet <- shiny::reactive(
      if (is_new()) new_sheet else lm_df$.sheet[cur_row()])
    cur_srow  <- shiny::reactive(
      if (is_new()) NA_integer_ else lm_df$.srow[cur_row()])
    # column index of a name IN THE SHEET the row lives in: the two sheets do
    # not carry their columns in the same order, and writing at the position
    # read from the other one would scatter the coordinates across the row.
    col_of_sheet <- function(sh, nm)
      if (identical(sh, lm_sheet)) match(nm, lm_hdr) else match(nm, new_hdr)
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
    # --- quality and review of the entry --------------------------------------
    # A score is a number from 1 to 5 or nothing at all: the "Not scored" button
    # (value 0) and an empty cell are the SAME statement and both come out NA --
    # a score of 0 would be a sixth grade nobody defined.
    qual_now <- function() {
      v <- num1(input$quality)
      if (!is.finite(v) || v < 1 || v > 5) NA_real_ else round(v)
    }
    rev_now <- function() isTRUE(input$reviewed)
    # a flag read back from a cell: openxlsx returns TRUE/FALSE, an older file or
    # a hand edit returns "TRUE" / "VRAI" / "1" / "oui" -- all the same statement.
    as_flag <- function(x) {
      s <- tolower(trimws(as.character(x)))
      length(s) == 1L && !is.na(s) && s %in%
        c("true", "vrai", "1", "yes", "oui", "y", "x", "ok")
    }
    # puts the two fields back in the state recorded for a row. Called for every
    # specimen: without it, saving a species after visiting a reviewed one would
    # carry that review over to it.
    load_review <- function(df, row) {
      q <- NA_real_; fl <- FALSE
      if (length(row) == 1L && !is.na(row) && row <= nrow(df)) {
        if ("quality_score" %in% names(df)) q <- num1(df[row, "quality_score"])
        if ("reviewed" %in% names(df))      fl <- as_flag(df[row, "reviewed"])
      }
      shiny::updateRadioButtons(session, "quality",
        selected = if (is.finite(q) && q >= 1 && q <= 5) as.character(round(q)) else "0")
      shiny::updateCheckboxInput(session, "reviewed", value = fl)
    }

    # the declaration of the specimen on screen, in the order of .FM_COLLAPSE so
    # that two identical statements are written identically -- a column read by
    # a machine must not depend on the order the boxes were ticked in.
    collapse_now <- function() {
      act <- intersect(names(.FM_COLLAPSE), rv$collapse)
      if (!length(act)) "" else paste(act, collapse = ";")
    }

    # the coincidences DECLARED on a recorded row, read back from
    # `collapse_rules`. Unknown identifiers are dropped rather than trusted: the
    # column is text in a workbook anyone can edit.
    collapse_declared <- function(df, row) {
      if (length(row) != 1L || is.na(row) || row > nrow(df) ||
          !"collapse_rules" %in% names(df)) return(character(0))
      v <- as.character(df[row, "collapse_rules"])
      if (length(v) != 1L || is.na(v) || !nzchar(trimws(v))) return(character(0))
      intersect(trimws(strsplit(v, "[;,[:space:]]+")[[1]]), names(.FM_COLLAPSE))
    }

    # writes the entry-level columns through the writer of the target sheet.
    # Written at EVERY save, empty included: the fields were reloaded with the
    # specimen, so rewriting them PRESERVES them, and clearing one becomes an
    # explicit act of the operator rather than an accident of navigation.
    write_entry_meta <- function(wr) {
      q <- qual_now(); fl <- rev_now()
      touched <- fl || is.finite(q)
      wr("quality_score", if (is.finite(q)) q else NA)
      wr("reviewed", fl)
      # author and date only when something IS declared: stamping a name on an
      # empty review would make "nobody has looked at it" indistinguishable from
      # "somebody looked and said nothing".
      wr("reviewed_by", if (touched) jr$operator else NA)
      wr("review_date", if (touched) .fm_iso_now() else NA)
      # The declared coincidences, in full, every time -- including the empty
      # string, which is how a rule is REMOVED from a specimen. Deducing them
      # from the geometry alone was never enough: a copy rule leaves nothing to
      # tell it apart from a chance coincidence, and an unticked box could not
      # be distinguished from a box that had never been ticked.
      wr("collapse_rules", collapse_now())
      rv$revstamp <- rv$revstamp + 1L
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
      # The declared coincidences are restored here, or reopening a specimen
      # would silently drop a statement the operator made about it -- and the
      # next click, with the rule no longer applied, would quietly undo the
      # zero. No point is touched: they are already where the rule put them,
      # only the tick boxes and the "adjusted" status are being restored.
      P0 <- matrix(NA_real_, 25L, 2L)
      if (!is.null(rv$A)) P0[1L, ] <- rv$A
      if (!is.null(rv$B)) P0[2L, ] <- rv$B
      for (k in names(ov)) {
        i <- suppressWarnings(as.integer(k))
        if (!is.na(i) && i >= 1L && i <= 25L) P0[i, ] <- ov[[k]]
      }
      # The DECLARATION recorded on the row first -- it is the operator's
      # statement, and it survives even a rule the geometry can no longer show.
      # The geometry second, for everything entered before the column existed.
      act <- union(collapse_declared(df, row), .fm_collapse_detect(P0))
      if (length(act)) {
        rv$collapse <- act
        shiny::updateCheckboxGroupInput(session, "collapse", selected = act)
        rv$adjusted <- union(rv$adjusted, .fm_collapse_points(act))
      }
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
      # score and tick: taken from the row of the specimen being opened, or
      # cleared if it has none. They qualify ONE entry, so they must never
      # trail behind the operator from one specimen to the next.
      load_review(if (is_new()) new_df else lm_df,
                  if (is_new()) cur_new_row() else cur_row())
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

    # ==========================================================================
    # Page "New species": a photograph enters the queue
    # ==========================================================================
    # State of the intake. `orig` is the picture as it was read and is never
    # touched again -- "Reset" has to be able to undo a crop, and a crop deletes
    # pixels. `work` is what is on screen and what will be written. Rotations,
    # mirrors and crops are applied to `work` in the order the operator asks
    # for them, so the preview IS the file: no transform is deferred to the
    # write, where it could no longer be seen and checked.
    add_rv <- shiny::reactiveValues(
      orig = NULL,     # array as read from the uploaded file
      work = NULL,     # array as displayed = array to be written
      src  = NULL,     # path of the uploaded temporary file
      srcname = NULL,  # its name on the operator's machine
      png  = FALSE,    # the source really is a PNG (magic bytes)
      fbase = NULL,    # result of the FishBase check, NULL until asked
      log  = NULL)     # what the last commit did

    # reading the uploaded file. read_img() detects the REAL format from the
    # magic bytes: about 7 % of the ".jpg" of this collection are not JPEG, and
    # trusting the extension is how a picture comes back striped.
    shiny::observeEvent(input$add_file, {
      f <- input$add_file
      if (is.null(f) || !nrow(f)) return()
      a <- tryCatch(read_img(f$datapath[1]), error = function(e) NULL)
      if (is.null(a)) {
        add_rv$orig <- NULL; add_rv$work <- NULL
        shiny::showNotification(
          paste0("Unreadable image: ", f$name[1],
                 ". A GIF/BMP/TIFF needs the 'magick' package."),
          type = "error", duration = NULL)
        return()
      }
      add_rv$orig <- a; add_rv$work <- a
      add_rv$src <- f$datapath[1]; add_rv$srcname <- f$name[1]
      add_rv$png <- .fm_is_png_file(f$datapath[1])
      add_rv$fbase <- NULL; add_rv$log <- NULL
      # the file name is the only identity the picture has at this point: it is
      # proposed, not imposed -- the operator corrects it if the camera or the
      # collection numbered its files rather than named them.
      nm <- .fm_name_from_file(f$name[1])
      if (nzchar(nm)) shiny::updateTextInput(session, "add_species", value = nm)
    })

    # geometry. Each button rewrites `work`; the brush is cleared with it,
    # since a selection drawn before a quarter turn no longer designates the
    # region the operator meant.
    add_apply <- function(f) {
      if (is.null(add_rv$work)) return()
      add_rv$work <- f(add_rv$work)
      session$resetBrush("add_brush")
    }
    shiny::observeEvent(input$add_rotl,  add_apply(function(a) .fm_rot90(a, 1L)))
    shiny::observeEvent(input$add_rotr,  add_apply(function(a) .fm_rot90(a, 3L)))
    shiny::observeEvent(input$add_fliph, add_apply(function(a) .fm_mirror(a, "h")))
    shiny::observeEvent(input$add_flipv, add_apply(function(a) .fm_mirror(a, "v")))
    shiny::observeEvent(input$add_reset, {
      add_rv$work <- add_rv$orig
      session$resetBrush("add_brush")
    })
    shiny::observeEvent(input$add_crop, {
      if (is.null(add_rv$work)) return()
      b <- input$add_brush
      if (is.null(b)) {
        shiny::showNotification("Drag a rectangle on the picture first.",
                                type = "warning")
        return()
      }
      a <- .fm_crop(add_rv$work, b$xmin, b$xmax, b$ymin, b$ymax)
      if (is.null(a)) {
        shiny::showNotification("Selection too small (8 px minimum a side).",
                                type = "error")
        return()
      }
      add_rv$work <- a
      session$resetBrush("add_brush")
    })

    # preview. The user space is the PIXEL grid of the working array, y running
    # downwards from the top-left corner: the brush then returns coordinates
    # .fm_crop() can use as they are, with no conversion to get wrong. The
    # picture is sub-sampled for drawing only (downscale), the array itself is
    # untouched -- what is written to disk keeps its full resolution.
    output$add_plot <- shiny::renderPlot({
      a <- add_rv$work
      if (is.null(a)) {
        op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op))
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Choose a photograph to see it here.",
                       col = "#9ca3af", cex = 1.1)
        return(invisible(NULL))
      }
      d <- dim(a); H <- d[1]; W <- d[2]
      op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op))
      graphics::plot(NA, xlim = c(0, W), ylim = c(H, 0), asp = 1,
                     xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
      graphics::rasterImage(grDevices::as.raster(downscale(a)), 0, H, W, 0,
                            interpolate = FALSE)
    })

    output$add_info <- shiny::renderUI({
      a <- add_rv$work
      if (is.null(a)) return(NULL)
      d0 <- dim(add_rv$orig); d1 <- dim(a)
      shiny::div(
        class = "tbhint", style = "margin-top:6px;",
        sprintf("%d \u00d7 %d px", d1[2], d1[1]),
        if (!identical(d0[1:2], d1[1:2]))
          sprintf(" (original %d \u00d7 %d px)", d0[2], d0[1]),
        sprintf(" \u00b7 source: %s", add_rv$srcname %||% ""),
        sprintf(" \u00b7 will be written as .%s", if (add_rv$png) "png" else "jpg"))
    })

    # --- optional FishBase check ---------------------------------------------
    # Non-blocking on purpose. A name absent from FishBase may still be a
    # specimen worth digitizing -- an undescribed form, a local checklist, a
    # taxonomy more recent than the copy of the database at hand -- so the
    # check REPORTS and never refuses. Only the form of the binomial is
    # enforced at commit, because "abramis  Brama" is a typing accident in
    # every case.
    shiny::observeEvent(input$add_check, {
      nm <- .fm_check_binomial(input$add_species)
      if (!nm$ok) {
        add_rv$fbase <- list(status = "form", msg = nm$msg, accepted = NA_character_)
        return()
      }
      if (!requireNamespace("rfishbase", quietly = TRUE)) {
        add_rv$fbase <- list(status = "unavailable", accepted = NA_character_,
                             msg = "The 'rfishbase' package is not installed.")
        return()
      }
      res <- tryCatch(validate_species_names(nm$name, verbose = FALSE),
                      error = function(e) NULL)
      if (is.null(res) || !nrow(res)) {
        add_rv$fbase <- list(status = "unavailable", accepted = NA_character_,
                             msg = "FishBase did not answer (network, or server down).")
        return()
      }
      add_rv$fbase <- list(status = as.character(res$status[1]),
                           accepted = as.character(res$accepted[1]),
                           msg = "")
    })

    # replaces the field with the name FishBase accepts. A separate button, not
    # an automatic rewrite: which name goes into the database is the operator's
    # call, and a synonym silently corrected is a decision nobody recorded.
    shiny::observeEvent(input$add_accept, {
      acc <- add_rv$fbase$accepted %||% NA_character_
      if (is.na(acc) || !nzchar(acc)) {
        shiny::showNotification("No accepted name to apply: run the check first.",
                                type = "warning")
        return()
      }
      shiny::updateTextInput(session, "add_species", value = acc)
    })

    output$add_name_info <- shiny::renderUI({
      nm <- .fm_check_binomial(input$add_species)
      box <- function(col, ...) shiny::div(
        class = "tbhint",
        style = paste0("margin-top:6px;color:", col, ";"), ...)
      if (!nzchar(trimws(as.character(input$add_species %||% ""))))
        return(box("#9ca3af", "No name yet."))
      if (!nm$ok) return(box("#b45309", nm$msg))
      fb <- add_rv$fbase
      base <- box("#6b7280", "Form accepted: ", shiny::tags$code(nm$name),
                  " \u00b7 file: ",
                  shiny::tags$code(basename(.fm_new_photo_path(
                    new_photo_dir, nm$name, if (add_rv$png) "png" else "jpg"))))
      if (is.null(fb)) return(base)
      msg <- switch(
        fb$status,
        accepted   = box("#15803d", "FishBase: name accepted."),
        synonym    = box("#b45309", "FishBase: SYNONYM of ",
                         shiny::tags$code(fb$accepted %||% "?"),
                         " -- use it, or keep this one deliberately."),
        unresolved = box("#b45309", "FishBase: name not resolved. ",
                         "Digitizing is still possible; the name is recorded as typed."),
        box("#6b7280", "FishBase: ", fb$msg %||% fb$status))
      shiny::tagList(base, msg)
    })

    # --- rebuilding the "new" queue ------------------------------------------
    # new_photos / q_new / choices_new are plain variables of the enclosing
    # function, read at launch. Reassigning them with <<- and bumping
    # rv$photostamp is what makes the queue notice a file that did not exist
    # when the session started -- the alternative being to close the
    # application and lose the journal position for one photograph.
    refresh_new_photos <- function() {
      new_photos <<- if (dir.exists(new_photo_dir))
        sort(list.files(new_photo_dir, full.names = TRUE,
                        pattern = "\\.(jpe?g|png|gif|bmp|tiff?)$",
                        ignore.case = TRUE))
      else character(0)
      q_new <<- seq_along(new_photos)
      choices_new <<- stats::setNames(seq_along(new_photos), basename(new_photos))
      rv$photostamp <- rv$photostamp + 1L
      invisible(new_photos)
    }

    # --- commit: the picture becomes a specimen of the queue -------------------
    shiny::observeEvent(input$add_commit, {
      a <- add_rv$work
      if (is.null(a)) {
        shiny::showNotification("Choose a photograph first.", type = "error")
        return()
      }
      nm <- .fm_check_binomial(input$add_species)
      if (!nm$ok) {
        shiny::showNotification(paste("Species name:", nm$msg), type = "error")
        return()
      }
      if (!dir.exists(new_photo_dir)) {
        ok <- dir.create(new_photo_dir, recursive = TRUE, showWarnings = FALSE)
        if (!ok && !dir.exists(new_photo_dir)) {
          shiny::showNotification(
            paste0("Could not create the folder '", new_photo_dir, "'."),
            type = "error", duration = NULL)
          return()
        }
      }
      # The output format follows the REAL bytes of the source: a PNG stays a
      # PNG. Re-encoding a lossless file as JPEG to satisfy an extension would
      # add compression artefacts to the very pixels the landmarks are read on.
      ext <- if (add_rv$png) "png" else "jpg"
      path <- .fm_new_photo_path(new_photo_dir, nm$name, ext)
      if (is.na(path)) {
        shiny::showNotification("Unusable species name.", type = "error")
        return()
      }
      wrote <- tryCatch({ .fm_write_img_as(a, path); TRUE },
                        error = function(e) {
                          shiny::showNotification(
                            paste("Writing failed:", conditionMessage(e)),
                            type = "error", duration = NULL)
                          FALSE })
      if (!wrote) return()

      # The FILE AS SUPPLIED is kept beside the queue, under its own name. What
      # goes into the queue has been cropped and turned, and those pixels are
      # gone: the backup is the only way back to the framing the photographer
      # chose, and to the resolution a later re-crop would need. Same
      # convention as the "bake the flip into the file" button -- one
      # '_originaux/' folder, and a backup that never overwrites an earlier one.
      bak <- NA_character_
      bak_dir <- file.path(new_photo_dir, "_originaux")
      if (!is.null(add_rv$src) && file.exists(add_rv$src)) {
        dir.create(bak_dir, showWarnings = FALSE, recursive = TRUE)
        b <- file.path(bak_dir, paste0(tools::file_path_sans_ext(basename(path)),
                                       "__", basename(add_rv$srcname %||% "source")))
        if (!file.exists(b) && file.copy(add_rv$src, b, overwrite = FALSE)) bak <- b
      }

      refresh_new_photos()
      i <- match(path, new_photos)
      if (is.na(i)) i <- match(basename(path), basename(new_photos))
      add_rv$log <- list(path = path, backup = bak, i = i)

      if (is.na(i)) {
        shiny::showNotification(
          paste0("Written to ", path, ", but the queue did not pick it up. ",
                 "Restart the application."), type = "warning", duration = NULL)
        return()
      }
      # Straight to the specimen: the photograph was brought in to be measured,
      # and stopping at "it has been added" would ask the operator to find it
      # again in a list. rv$mode is set BEFORE the radio button so that the
      # mode observer, which resets the queue to its first entry, sees no
      # change when the browser echoes the new value back.
      rv$mode <- "new"
      shiny::updateRadioButtons(session, "mode", selected = "new")
      shiny::updateSelectizeInput(session, "goto_species", choices = choices_new,
                                  selected = i, server = TRUE)
      if (identical(rv$qi, i)) load_species() else rv$qi <- i
      shiny::updateTabsetPanel(session, "page", selected = "digit")
      # the fields are cleared: the next photograph is a different specimen, and
      # a name left over from the previous one is how two fish end up sharing it.
      add_rv$orig <- NULL; add_rv$work <- NULL; add_rv$src <- NULL
      add_rv$srcname <- NULL; add_rv$fbase <- NULL
      shiny::updateTextInput(session, "add_species", value = "")
      shiny::showNotification(
        paste0("Added: ", basename(path), " (", length(new_photos),
               " photograph(s) in the queue)."), type = "message", duration = 8)
    })

    output$add_log <- shiny::renderUI({
      lg <- add_rv$log
      if (is.null(lg)) return(NULL)
      shiny::div(
        class = "tbhint", style = "margin-top:8px;color:#15803d;",
        "Written: ", shiny::tags$code(basename(lg$path)),
        if (!is.na(lg$backup))
          shiny::tagList(" \u00b7 original kept in ",
                         shiny::tags$code("_originaux/")),
        " \u00b7 no workbook row until the first \"Save & next\".")
    })

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
        # PROJECTIONS FIRST: `Bd4` moves 4, which is the MASTER of the belly
        # line. Applied after the conventions it would leave 11, 8 and 9 derived
        # from the position 4 held before the projection -- a belly line that no
        # longer passes through its own pivot. Applied here, .fm_constrain()
        # re-derives the whole ventral chain from the projected 4, and leaves it
        # on the axis: nothing in the conventions changes its height in the mid
        # frame (belly_line takes 4 as pivot, perp only ever resets its abscissa).
        P <- .fm_apply_collapse(P, rv$collapse, kinds = "project")
        P <- .fm_constrain(P, rv$edited, pfl_px = pfl_px)
      }
      P[23, ] <- .fm_point23(P)     # 23 always recomputed (auto) after editing/conventions
      # LAST, after the conventions AND after 23 is rebuilt: the conventions
      # re-derive the ventral points on the belly line, and 23 -- built on the
      # line (1, 9) -- is undefined once 9 sits on 1. Applying the rules here is
      # what puts 23 on 1 instead of leaving it NA. The projections are replayed
      # too -- idempotent when they have already acted, and this is the only pass
      # there is when the constrained editing is switched off.
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
      # ONLY the copied points are taken over by the rule. A projected point (4
      # on the axis) keeps its click: the rule sets its height, not its abscissa,
      # so dropping its override would move it along the body as well.
      copied <- .fm_collapse_points(rv$collapse, kinds = "move")
      rv$edited <- setdiff(rv$edited, copied)
      if (length(copied)) rv$override[as.character(copied)] <- NULL
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    output$collapse_help <- shiny::renderUI({
      act <- rv$collapse
      txt <- if (!length(act))
        paste("A segment that is genuinely zero on this species -- a mouth on",
              "the ventral profile, a head ending on it. One point takes the",
              "coordinates of the other, or is projected onto the mid axis:",
              "both stay on the photograph and in the workbook. Re-applied",
              "after every click, reset for each species.")
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
    # --- bake the flip into the FILE ------------------------------------------
    # The rule is "write what you see", and it is the right one for BOTH flips,
    # which is why they are composed here rather than handled separately:
    #
    #   * "photo + landmarks" moved the points with the image, so the points on
    #     screen sit on the flipped fish and are RECORDED in that frame. Saving
    #     the flipped image makes the file agree with the record.
    #   * "photo ONLY" exists for the opposite case -- points loaded mirrored
    #     relative to their photograph -- and flipping the display is what makes
    #     them land on the fish again. The recorded points were therefore always
    #     in the flipped frame, and saving the flipped image makes the file
    #     agree with them too.
    #
    # In both cases the displayed composition is the one the coordinates belong
    # to. What must NOT be baked in is the fast-display downscale: it is a
    # rendering shortcut, and writing it would silently shrink the photograph
    # under coordinates expressed in full-resolution pixels.
    shiny::observeEvent(input$flip_write, {
      if (is.null(rv$arr)) {
        shiny::showNotification("No photograph loaded.", type = "error"); return()
      }
      if (identical(rv$flip, "none") && identical(rv$dispflip, "none")) {
        shiny::showNotification("No flip to write: both selectors are on None.",
                                type = "message"); return()
      }
      f <- cur_photo()
      if (length(f) != 1 || is.na(f) || !file.exists(f)) {
        shiny::showNotification("Photograph file not found.", type = "error"); return()
      }
      a <- flip_arr(flip_arr(rv$arr, rv$flip), rv$dispflip)
      res <- tryCatch(.fm_write_img(a, f), error = function(e) e)
      if (inherits(res, "error")) {
        shiny::showNotification(paste("Write failed:", conditionMessage(res)),
                                type = "error", duration = 8)
        return()
      }
      # The session now has to match the disk: the array in memory becomes the
      # flipped one and both selectors go back to None. Leaving either of them
      # set would flip an already-flipped picture on the next redraw.
      rv$arr <- a
      rv$flip <- "none"; rv$dispflip <- "none"
      shiny::updateRadioButtons(session, "flip_mode", selected = "none")
      shiny::updateRadioButtons(session, "flip_disp", selected = "none")
      rv$img <- make_disp()
      rv$zoom <- 1; rv$cx <- NULL; rv$cy <- NULL
      shiny::showNotification(
        paste0(basename(f), " written",
               if (isTRUE(res$jpeg)) " (JPEG re-encoded at quality 0.97)" else "",
               ". Original kept in _originaux/.",
               " Remember to save the specimen as well if its points changed:",
               " this button writes the IMAGE, not the record."),
        type = "warning", duration = 10)
    })

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
      write_entry_meta(wr)
      rv$newstamp <- rv$newstamp + 1L              # invalide cur_new_row()
      TRUE
    }

    # --- status of each point, for the journal ---------------------------------
    # This is the information the wide workbook layout cannot carry:
    #   placed  : placed / moved by hand, or reloaded from an earlier entry
    #   seeded  : STILL AT ITS SEED POSITION, hence never checked -> to be audited
    #   adjusted: snapped by the extreme-point convention (3/4), not pointed at
    #   derived : calcule automatiquement (8, 9, 11, 13, 14, 23)
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
        target_sheet = if (is_new()) new_sheet else cur_sheet(),
        img_w = rv$w, img_h = rv$h,
        ruler_mm = if (is_new() && is.finite(mm)) mm else NA,
        mm_per_px = if (is_new() && is.finite(mpp)) mpp else NA,
        quality_score = qual_now(), reviewed = rev_now(),
        collapse = collapse_now())
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
    # the checks a declared coincidence suspends -- recomputed at every call, a
    # rule being ticked and unticked while the specimen is on screen
    conv_skip <- function() .fm_collapse_skip_extreme(rv$collapse)

    apply_conv_fix <- function() {
      for (it in 1:3) {
        P <- recon()
        v <- .fm_extreme_violations(P, skip = conv_skip())
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
      invisible(is.null(.fm_extreme_violations(recon(), skip = conv_skip())))
    }

    shiny::observeEvent(input$save, {
      shiny::req(rv$A, rv$B)
      if (isTRUE(input$checkextremes)) {
        v <- .fm_convention_violations(recon(), skip = conv_skip())
        if (!is.null(v)) { show_conv_modal(v); return() }
      }
      do_save()
    })

    # 1) measure again: close, select the offending point and zoom onto it. For
    #    an inversion the point to re-measure is the CULPRIT -- the one found on
    #    the wrong side -- not the reference it was compared with.
    shiny::observeEvent(input$conv_remeasure, {
      shiny::removeModal()
      v <- .fm_convention_violations(recon(), skip = conv_skip())
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
      sh      <- cur_sheet()
      col_of  <- function(nm) col_of_sheet(sh, nm)
      r_excel <- cur_srow() + 1L                   # +1 for the header
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
        openxlsx::writeData(wb, sh, round(P[pnum, 1], 3),
                            startCol = cx, startRow = r_excel, colNames = FALSE)
        openxlsx::writeData(wb, sh, round(P[pnum, 2], 3),
                            startCol = cy, startRow = r_excel, colNames = FALSE)
      }
      if (length(dropped))
        shiny::showNotification(
          sprintf(paste("Columns absent from '%s': point(s) %s are NOT written",
                        "to the workbook. They are in the journal;",
                        "fishmorph_consolidate() will find them again."),
                  sh, paste(dropped, collapse = ", ")),
          type = "error", duration = NULL)
      # updates lm_df IN MEMORY so that coming back to the species within the
      # session reloads exactly what has just been saved (24/25 included).
      rr <- cur_row()
      for (pnum in save_pts) {
        xc <- paste0(pnum, "_X"); yc <- paste0(pnum, "_Y")
        if (xc %in% names(lm_df)) lm_df[rr, xc] <<- round(P[pnum, 1], 3)
        if (yc %in% names(lm_df)) lm_df[rr, yc] <<- round(P[pnum, 2], 3)
      }
      # score and review, same row, same rule: workbook AND in-memory copy, so
      # that coming back to the species within the session finds them again.
      write_entry_meta(function(col, val) {
        j <- col_of(col); if (is.na(j)) return(invisible())
        openxlsx::writeData(wb, sh, val, startCol = j, startRow = r_excel,
                            colNames = FALSE)
        if (col %in% names(lm_df)) lm_df[rr, col] <<- val
      })
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

    # What the FILE says about this specimen, as opposed to what the two fields
    # above currently show: after a change and before a save the two differ, and
    # that difference is exactly what the operator needs to see.
    output$review_info <- shiny::renderUI({
      rv$qi; rv$revstamp; rv$newstamp                # declencheurs reactifs
      df  <- if (is_new()) new_df else lm_df
      row <- if (is_new()) cur_new_row() else cur_row()
      if (length(row) != 1L || is.na(row) || row > nrow(df))
        return(shiny::div(class = "progressbox",
                          "Specimen not recorded yet: no review on file."))
      g <- function(cc) {
        if (!cc %in% names(df)) return(NA_character_)
        v <- as.character(df[row, cc])
        if (length(v) != 1L || is.na(v) || !nzchar(trimws(v))) NA_character_ else v
      }
      q <- num1(g("quality_score"))
      shiny::div(class = "progressbox", shiny::HTML(sprintf(
        "In the file: score <b>%s</b> &middot; checked <b>%s</b><br>by <code>%s</code>, %s",
        if (is.finite(q)) sprintf("%d/5", as.integer(q)) else "-",
        if (as_flag(g("reviewed"))) "yes" else "no",
        if (is.na(g("reviewed_by"))) "-" else g("reviewed_by"),
        if (is.na(g("review_date"))) "-" else g("review_date"))))
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

  # The intake page uploads a photograph through the browser, and shiny refuses
  # anything above 5 Mb by default -- a limit a specimen picture passes without
  # trying. Raised for the lifetime of the application and restored on exit, so
  # the session that launched it keeps its own setting.
  old_maxreq <- getOption("shiny.maxRequestSize")
  options(shiny.maxRequestSize = 512 * 1024^2)
  on.exit(options(shiny.maxRequestSize = old_maxreq), add = TRUE)

  # Digitizing means clicking nineteen points on a photograph: the RStudio
  # Viewer pane, a few hundred pixels wide, is the one place this application
  # must not open. `.fm_browser()` forces the system browser past it.
  shiny::runApp(shiny::shinyApp(ui, server),
                launch.browser = .fm_browser(launch.browser))
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
# Photographs can be dropped into `new_photo_dir` before launching the app, or
# brought in from the "New species" page while it runs -- browse, name, crop and
# rotate, commit; the queue is rebuilt on the spot. Either way, every image in
# the folder becomes an entry of the queue (several specimens of one
# species are therefore possible, one row each). The species name is pre-filled
# from the file name and can be changed in the left panel. The key of a row is
# the `photo_file` column: coming back to a photograph already done reloads its
# points and REWRITES the same row instead of appending a second one.
#
# The "Queue" selector at the top of the app switches between the three queues.
# -> writes into FishMORPH/FISHMORPH_PUBLI_9556sp_reconstructed.xlsx
