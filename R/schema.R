# =============================================================================
# schema.R -- single source of truth for the FISHMORPH digitizing scheme.
#
# Landmark / segment / ratio definitions follow Brosse et al. (2021) and the
# original intraitR implementation (fishmorph_segments() / project_fishmorph()).
# Every other function in the package reads these constants; editing the scheme
# here propagates everywhere.
# =============================================================================

# ---- Landmark labels (21 anatomical + 1 optional body-curvature point) -------
# 1  snout tip                     12 pectoral fin ray tip
# 2  caudal-fin base (mid)         13 eye, dorsal edge
# 3  body, dorsal (max depth)      14 eye, ventral edge
# 4  body, ventral (max depth)     15 mouth corner (jaw)
# 5  head, dorsal profile          16 caudal peduncle, dorsal
# 6  head, ventral profile         17 caudal peduncle, ventral
# 7  eye centre                    18 caudal fin, dorsal
# 8  body ventral below eye        19 caudal fin, ventral
# 9  body ventral below snout      20 scale bar, point 1
# 10 pectoral fin insertion        21 scale bar, point 2
# 11 body ventral below pectoral   22 (optional) body-length curvature point
.FM_LANDMARK_LABELS <- c(
  "snout_tip", "caudal_base", "body_dorsal", "body_ventral",
  "head_dorsal", "head_ventral", "eye_centre", "body_ventral_eye",
  "body_ventral_snout", "pectoral_insertion", "body_ventral_pectoral",
  "pectoral_tip", "eye_dorsal", "eye_ventral", "mouth_corner",
  "peduncle_dorsal", "peduncle_ventral", "caudalfin_dorsal",
  "caudalfin_ventral", "scalebar_1", "scalebar_2", "bl_curvature"
)

# ---- The 11 segments and their landmark pairs --------------------------------
# Bl is handled specially: 1-22-2 broken line when landmark 22 is present and
# non-zero, else the straight 1-2 distance.
.FM_SEGMENT_PAIRS <- list(
  Bl  = c(1, 2),
  Bd  = c(3, 4),
  Hd  = c(5, 6),
  Eh  = c(7, 8),
  Mo  = c(1, 9),
  PFi = c(10, 11),
  PFl = c(10, 12),
  Ed  = c(13, 14),
  Jl  = c(1, 15),
  CPd = c(16, 17),
  CFd = c(18, 19)
)

.FM_SEGMENTS <- names(.FM_SEGMENT_PAIRS)

# ---- The 9 dimensionless ratios ---------------------------------------------
# numerator / denominator over segment names (Brosse et al. 2021, Table 1).
.FM_RATIO_DEF <- list(
  BEl = c("Bl",  "Bd"),   # Body elongation
  VEp = c("Eh",  "Bd"),   # Vertical eye position
  REs = c("Ed",  "Hd"),   # Relative eye size
  OGp = c("Mo",  "Bd"),   # Oral gape position
  RMl = c("Jl",  "Hd"),   # Relative maxillary length
  BLs = c("Hd",  "Bd"),   # Body lateral shape
  PFv = c("PFi", "Bd"),   # Pectoral fin vertical position
  PFs = c("PFl", "Bl"),   # Pectoral fin size
  CPt = c("CFd", "CPd")   # Caudal peduncle throttling
)

.FM_RATIOS <- names(.FM_RATIO_DEF)

# Human-readable trait names (for plots / tables).
.FM_RATIO_LONG <- c(
  BEl = "Body elongation",
  VEp = "Vertical eye position",
  REs = "Relative eye size",
  OGp = "Oral gape position",
  RMl = "Relative maxillary length",
  BLs = "Body lateral shape",
  PFv = "Pectoral fin vertical position",
  PFs = "Pectoral fin size",
  CPt = "Caudal peduncle throttling"
)

# Function each trait relates to (ecological interpretation, Brosse et al. 2021).
.FM_RATIO_FUNCTION <- c(
  BEl = "Hydrodynamism / position in the water column",
  VEp = "Vertical position of prey",
  REs = "Visual acuity",
  OGp = "Feeding position in the water column",
  RMl = "Size of prey / mouth",
  BLs = "Position in the water column",
  PFv = "Pectoral use for manoeuvrability",
  PFs = "Pectoral use for propulsion / manoeuvrability",
  CPt = "Caudal propulsion efficiency"
)

# Landmark points that are DERIVED (computed) rather than digitized directly.
.FM_DERIVED_POINTS <- c(8L, 9L, 11L, 15L, 22L)

#' FISHMORPH digitizing scheme
#'
#' Returns the full scheme used throughout the package: landmark labels, the 11
#' segment/landmark-pair map, the 9 ratio definitions and their ecological
#' meaning. All numeric routines derive from these constants.
#'
#' @return A named list with elements `landmark_labels`, `segment_pairs`,
#'   `segments`, `ratio_def`, `ratios`, `ratio_long`, `ratio_function` and
#'   `derived_points`.
#' @references Brosse et al. (2021) *Global Ecology and Biogeography* 30:2330-2336.
#' @examples
#' str(fishmorph_schema())
#' @export
fishmorph_schema <- function() {
  list(
    landmark_labels = .FM_LANDMARK_LABELS,
    segment_pairs   = .FM_SEGMENT_PAIRS,
    segments        = .FM_SEGMENTS,
    ratio_def       = .FM_RATIO_DEF,
    ratios          = .FM_RATIOS,
    ratio_long      = .FM_RATIO_LONG,
    ratio_function  = .FM_RATIO_FUNCTION,
    derived_points  = .FM_DERIVED_POINTS
  )
}

#' Names of the 11 FISHMORPH segments
#' @return Character vector.
#' @export
fishmorph_segment_names <- function() .FM_SEGMENTS

#' Names of the 9 FISHMORPH ratios
#' @return Character vector.
#' @export
fishmorph_ratio_names <- function() .FM_RATIOS

#' Segment -> landmark-pair map
#' @return Named list of length-2 integer vectors.
#' @export
fishmorph_landmark_pairs <- function() .FM_SEGMENT_PAIRS

# small internal null-coalescing helper
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ensure `df` carries an identifier column named `id_col`; if the column is
# absent but the row names are usable (not just 1..n), promote them to that
# column. Used so functions accept tables where the species id ended up in the
# row names (e.g. the output of fishmorph_segments()).
.ensure_id_column <- function(df, id_col, what = "data") {
  if (id_col %in% names(df)) return(df)
  rn <- rownames(df)
  if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(df)))) &&
      any(nzchar(rn))) {
    df[[id_col]] <- rn
    return(df)
  }
  stop("`id_col` = '", id_col, "' is not a column of `", what,
       "`, and its row names are not usable identifiers. Add the column, e.g. ",
       "`", what, "$", id_col, " <- rownames(", what, ")`.", call. = FALSE)
}
