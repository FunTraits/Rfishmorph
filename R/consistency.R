# =============================================================================
# consistency.R -- detect and fix impossible vertical orderings of the FISHMORPH
# landmarks (e.g. an eye segment deeper than the body -> VEp > 1). Works in raw
# image coordinates (Y down) or Cartesian (Y up): the dorsal-ventral direction
# is detected per specimen from landmarks 3 (dorsal) and 4 (ventral).
# =============================================================================

# Ordered dorsal -> ventral chains whose Y must be monotonic. Landmark 3 and 4
# (body depth) bound the eye/head chain, which directly forbids VEp > 1
# (|7-8| can no longer exceed |3-4|).
.FM_ORDER_CHAINS <- list(
  eye_body  = c(3, 5, 13, 7, 14, 6, 8, 4),  # body/head/eye vertical
  pectoral  = c(10, 11),                     # PFi: insertion above ventral
  peduncle  = c(16, 17),                     # caudal peduncle depth
  caudalfin = c(18, 19),                     # caudal fin depth
  snout     = c(1, 9)                        # mouth: snout above ventral
)

# dorsal -> ventral sign: +1 if Y increases downward (image), -1 if upward.
.dv_direction <- function(P, npt) {
  if (npt >= 4 && all(is.finite(P[3, ])) && all(is.finite(P[4, ])) &&
      P[4, 2] != P[3, 2]) return(sign(P[4, 2] - P[3, 2]))
  if (npt >= 8 && all(is.finite(P[3, ])) && all(is.finite(P[8, ])) &&
      P[8, 2] != P[3, 2]) return(sign(P[8, 2] - P[3, 2]))
  NA_real_
}

# clamp each chain to be monotonic dorsal->ventral; return corrected P + count.
.enforce_order_specimen <- function(P, chains, dir) {
  npt <- nrow(P); changed <- 0L
  for (ch in chains) {
    ch <- ch[ch <= npt]
    prev <- NA_real_
    for (i in ch) {
      if (!all(is.finite(P[i, ]))) next
      y <- P[i, 2]
      if (is.na(prev)) { prev <- y; next }
      if (dir * (y - prev) < 0) {      # crossed the previous point -> clamp
        P[i, 2] <- prev; changed <- changed + 1L
      } else prev <- y
    }
  }
  list(P = P, changed = changed)
}

#' Correct impossible vertical landmark orderings
#'
#' Enforces the dorsal-to-ventral Y ordering of the FISHMORPH landmarks so that
#' geometrically impossible configurations (which produce out-of-range ratios
#' such as `VEp > 1`) are removed. Along each ordered chain, any landmark whose
#' Y crosses its dorsal neighbour is snapped onto that neighbour. The
#' dorsal-ventral direction is detected per specimen (landmarks 3 vs 4), so the
#' function is correct in both image (Y-down) and Cartesian (Y-up) coordinates.
#'
#' Chains enforced (dorsal -> ventral): the body/head/eye column
#' `3, 5, 13, 7, 14, 6, 8, 4` (bounding the eye between the body-depth landmarks,
#' so `VEp <= 1`), the pectoral `10, 11`, the caudal peduncle `16, 17`, the
#' caudal fin `18, 19`, and the snout/mouth `1, 9`.
#'
#' @param landmarks A `fishmorph_landmarks` object (e.g. straight after
#'   `fishmorph_landmarks(lm_df)`).
#' @param chains Named list of ordered landmark chains to enforce. Defaults to
#'   the FISHMORPH set above; override to add/remove chains.
#' @param report Print a one-line summary (default `TRUE`).
#' @return The corrected `fishmorph_landmarks` object. `attr(, "order_report")`
#'   is a data frame (`specimen`, `n_corrected`, `flagged`).
#' @seealso [check_landmark_order()], [correct_geometry_conventions()]
#' @examples
#' lm <- read_landmarks_csv(
#'   system.file("extdata", "example_landmarks.csv", package = "Rfishmorph"))
#' lm2 <- correct_landmark_order(lm)
#' @export
correct_landmark_order <- function(landmarks, chains = .FM_ORDER_CHAINS,
                                   report = TRUE) {
  if (!is_fishmorph_landmarks(landmarks))
    stop("`landmarks` must be a fishmorph_landmarks object.", call. = FALSE)
  co <- .fm_coords(landmarks); sp <- .fm_specimens(landmarks); npt <- dim(co)[1]
  n_fixed <- integer(length(sp))
  for (i in seq_along(sp)) {
    P <- co[, , i]
    dir <- .dv_direction(P, npt)
    if (is.na(dir) || dir == 0) next
    r <- .enforce_order_specimen(P, chains, dir)
    co[, , i] <- r$P; n_fixed[i] <- r$changed
  }
  out <- fishmorph_landmarks(co, metadata = landmarks$metadata,
                             scale = landmarks$scale, pad_to = NULL)
  attr(out$coords, "imputed") <- attr(.fm_coords(landmarks), "imputed")
  rep_df <- data.frame(specimen = sp, n_corrected = n_fixed,
                       flagged = n_fixed > 0L, stringsAsFactors = FALSE)
  attr(out, "order_report") <- rep_df
  if (report)
    message(sprintf("correct_landmark_order(): adjusted %d landmark(s) in %d of %d specimen(s).",
                    sum(n_fixed), sum(n_fixed > 0L), length(sp)))
  out
}

#' Report vertical-ordering inconsistencies in landmarks (no correction)
#'
#' Dry-run companion to [correct_landmark_order()]: returns which specimens have
#' impossible dorsal-ventral landmark orderings and how many landmarks would be
#' moved, without altering the data.
#'
#' @param landmarks A `fishmorph_landmarks` object.
#' @param chains Chains to check (see [correct_landmark_order()]).
#' @return A data frame (`specimen`, `n_corrected`, `flagged`), the rows with
#'   `flagged = TRUE` being the inconsistent specimens.
#' @export
check_landmark_order <- function(landmarks, chains = .FM_ORDER_CHAINS) {
  attr(correct_landmark_order(landmarks, chains, report = FALSE),
       "order_report")
}
