# =============================================================================
# segments.R -- the 11 FISHMORPH linear measurements from landmark coordinates.
# =============================================================================

# Euclidean distance between landmarks a and b for a single 2D matrix P.
.lm_dist <- function(P, a, b) {
  if (a > nrow(P) || b > nrow(P)) return(NA_real_)
  sqrt(sum((P[a, ] - P[b, ])^2))
}

# Body length Bl as the arc length along the BROKEN body axis
# 1 -> (22) -> (24) -> 2, using whichever hinge landmarks (22, 24, 25) are
# present and non-zero, ordered along the 1-2 chord. With no hinge this reduces
# to the straight distance |1-2|. Returns NA if landmark 1 or 2 is missing.
.fm_bl_broken <- function(P) {
  npt <- nrow(P)
  if (!all(is.finite(P[1, ])) || !all(is.finite(P[2, ]))) return(NA_real_)
  uc <- P[2, ] - P[1, ]; Lc <- sqrt(sum(uc^2))
  if (!is.finite(Lc) || Lc == 0) return(NA_real_)
  placed <- function(i) i <= npt && all(is.finite(P[i, ])) &&
    (abs(P[i, 1]) + abs(P[i, 2]) > 0)
  hs <- .FM_AXIS_HINGES; hs <- hs[hs <= npt]
  hs <- hs[vapply(hs, placed, logical(1))]
  if (length(hs))
    hs <- hs[order(vapply(hs, function(i) sum((P[i, ] - P[1, ]) * uc), numeric(1)))]
  chain <- c(1L, hs, 2L)
  sum(vapply(seq_len(length(chain) - 1L),
             function(k) .lm_dist(P, chain[k], chain[k + 1L]), numeric(1)))
}

#' Compute the 11 FISHMORPH segments from landmarks
#'
#' Measures the eleven body segments (`Bl, Bd, Hd, Eh, Mo, PFi, PFl, Ed, Jl,
#' CPd, CFd`) from digitized landmark configurations. Body length `Bl` follows
#' the 1-22-2 broken line when the optional curvature landmark 22 is present and
#' non-zero, otherwise the straight 1-2 distance (matching intraitR).
#'
#' Absolute lengths require a scale. The scale is taken, in order, from
#' `scale_cm` combined with the scale-bar landmarks 20-21, then from the
#' object's `scale` slot, and otherwise the segments are returned in the input
#' (pixel) units with a warning-free `attr(, "unit") = "raw"`.
#'
#' @param x A `fishmorph_landmarks` (or `intrait_landmarks`) object.
#' @param scale_cm Real length (cm) of the scale-bar segment (landmarks 20-21).
#'   Ignored when landmarks 20-21 are absent.
#' @param na.rm Passed through to keep only rows with usable coordinates.
#' @return A data frame, one row per specimen, with column `specimen` and the 11
#'   segment columns. `attr(, "unit")` records `"cm"` or `"raw"`.
#' @seealso [fishmorph_ratios()], [fishmorph_traits()]
#' @examples
#' lm <- reconstruct_fishmorph_landmarks(
#'   list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
#'        PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0))
#' fishmorph_segments(lm, scale_cm = 1)
#' @export
fishmorph_segments <- function(x, scale_cm = NULL, na.rm = TRUE) {
  co <- .fm_coords(x); sp <- .fm_specimens(x)
  n <- length(sp)
  out <- data.frame(specimen = sp, stringsAsFactors = FALSE)
  for (nm in .FM_SEGMENTS) out[[nm]] <- NA_real_
  unit <- "raw"
  for (i in seq_len(n)) {
    P <- co[, , i]
    npt <- nrow(P)
    # per-specimen scale factor (units per pixel)
    sf <- 1
    if (!is.null(scale_cm) && npt >= 21) {
      d2021 <- .lm_dist(P, 20, 21)
      if (is.finite(d2021) && d2021 > 0) { sf <- scale_cm / d2021; unit <- "cm" }
    } else if (!is.null(x$scale)) {
      sc <- if (length(x$scale) == n) x$scale[i] else x$scale[1]
      if (is.finite(sc) && sc > 0) { sf <- sc; unit <- "cm" }
    }
    # Bl: arc length along the broken body axis 1 -> (22) -> (24) -> 2 using the
    # hinge landmarks 22/24/25 that are present (non-zero) ; straight 1-2 otherwise.
    bl <- .fm_bl_broken(P)
    if (!is.finite(bl) || bl <= 0) bl <- .lm_dist(P, 1, 2)
    out$Bl[i] <- bl * sf
    for (nm in setdiff(.FM_SEGMENTS, "Bl")) {
      pr <- .FM_SEGMENT_PAIRS[[nm]]
      out[[nm]][i] <- .lm_dist(P, pr[1], pr[2]) * sf
    }
  }
  if (na.rm) {
    keep <- rowSums(!is.na(out[.FM_SEGMENTS])) > 0 &
      is.finite(out$Bl) & out$Bl > 0
    out <- out[keep, , drop = FALSE]
  }
  attr(out, "unit") <- unit
  rownames(out) <- NULL
  out
}

#' Compute the 9 FISHMORPH ratios
#'
#' Derives the nine dimensionless morphological ratios from a table of segments.
#' Because each ratio is a quotient of two segments of the same specimen, the
#' unknown pixel-to-cm factor cancels: ratios are scale-invariant and can be
#' computed from raw (pixel) segments.
#'
#' @param x Either a `fishmorph_landmarks` object or a data frame of segments
#'   (as returned by [fishmorph_segments()]).
#' @param scale_cm Passed to [fishmorph_segments()] when `x` is a landmark object
#'   (irrelevant to the ratio values themselves).
#' @return A data frame with column `specimen` and the 9 ratio columns.
#'   Non-finite or zero denominators yield `NA`.
#' @examples
#' seg <- data.frame(specimen = "a", Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1,
#'                   Mo = 1.6, PFi = 1.9, PFl = 2.1, Ed = 0.6, Jl = 1.3,
#'                   CPd = 1.0, CFd = 3.0)
#' fishmorph_ratios(seg)
#' @export
fishmorph_ratios <- function(x, scale_cm = NULL) {
  seg <- if (is_fishmorph_landmarks(x)) fishmorph_segments(x, scale_cm = scale_cm) else x
  if (!is.data.frame(seg))
    stop("`x` must be a fishmorph_landmarks object or a segment data frame.",
         call. = FALSE)
  id <- if ("specimen" %in% names(seg)) seg$specimen else
    rownames(seg) %||% seq_len(nrow(seg))
  r <- data.frame(specimen = id, stringsAsFactors = FALSE)
  for (rn in .FM_RATIOS) {
    num <- .FM_RATIO_DEF[[rn]][1]; den <- .FM_RATIO_DEF[[rn]][2]
    # coerce defensively: readxl/read.csv may return segment columns as text,
    # which would silently yield NA ratios instead of the expected values.
    xnum <- suppressWarnings(as.numeric(seg[[num]]))
    xden <- suppressWarnings(as.numeric(seg[[den]]))
    r[[rn]] <- ifelse(is.finite(xnum) & is.finite(xden) & xden != 0,
                      xnum / xden, NA_real_)
  }
  rownames(r) <- NULL
  r
}

#' Full FISHMORPH trait table (segments + ratios)
#'
#' Convenience wrapper returning the 11 segments and the 9 ratios in one table.
#'
#' @param x A `fishmorph_landmarks` object.
#' @param scale_cm Scale-bar length in cm (see [fishmorph_segments()]).
#' @return A data frame with `specimen`, 11 segment and 9 ratio columns.
#' @export
fishmorph_traits <- function(x, scale_cm = NULL) {
  seg <- fishmorph_segments(x, scale_cm = scale_cm)
  rat <- fishmorph_ratios(seg)
  out <- merge(seg, rat, by = "specimen", sort = FALSE)
  attr(out, "unit") <- attr(seg, "unit")
  out
}
