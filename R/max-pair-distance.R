# =============================================================================
# max-pair-distance.R -- diameter of a planar point cloud.
# Moved from intraitR (R/max_pair_distance.R) in Rfishmorph 0.17.0: Rfishmorph owns the
# visualisation of the FISHMORPH functional space, intraitR re-exports it.
# It is the denominator plotly_fishmorph() uses to express tooltip distances as
# a share of the reference cloud.
# =============================================================================

#' Diameter of a point cloud: the distance between its two most distant points
#'
#' The largest Euclidean distance between any two points of a planar cloud.
#' Its purpose in this package is to be a **denominator**: a distance read off
#' an ordination -- a specimen to its species centroid, a centroid to a
#' reference point -- is in score units, which are arbitrary in magnitude and
#' depend on the axis pair displayed, so the number is uninterpretable on its
#' own. Divided by the diameter of the cloud it is read against, it becomes a
#' share of the total spread of that cloud: "this individual sits 7% of the
#' FISHMORPH morphospace away from its species' mean" is a statement a reader
#' can weigh, and it is comparable between axis pairs and between campaigns.
#'
#' @section Why the convex hull:
#' The two most distant points of a cloud are necessarily vertices of its
#' convex hull -- a point strictly inside the hull is closer to every other
#' point than some vertex is -- so the search can be restricted to the hull
#' without approximation. This matters: the FISHMORPH reference holds ~9,500
#' species, whose full pairwise distance matrix is ~45 million values (several
#' hundred megabytes), while its hull has a few dozen vertices. The result is
#' exact, not a sample.
#'
#' @param x Either a numeric vector of first coordinates, or a two-column
#'   matrix / data.frame of coordinates (in which case `y` is left `NULL`).
#' @param y Numeric vector of second coordinates, the same length as `x`, when
#'   `x` is a vector.
#'
#' @return A single numeric value, the maximum distance between two points.
#'   `NA_real_` when fewer than two points are finite, so that a caller
#'   dividing by it produces `NA` rather than a spurious number.
#'
#' @seealso [plotly_fishmorph()] and [project_fishmorph()], which use it to
#'   express tooltip distances as a percentage of the reference cloud's
#'   diameter. Re-exported by intraitR.
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(500); y <- rnorm(500)
#' max_pair_distance(x, y)
#' max_pair_distance(cbind(x, y))
#'
#' # a distance expressed as a share of the cloud's total spread
#' 100 * sqrt((x[1] - mean(x))^2 + (y[1] - mean(y))^2) / max_pair_distance(x, y)
#'
#' @export
max_pair_distance <- function(x, y = NULL) {
  if (is.null(y)) {
    m <- as.matrix(x)
    if (ncol(m) < 2)
      stop("`x` must have two columns when `y` is not given.", call. = FALSE)
    y <- as.numeric(m[, 2])
    x <- as.numeric(m[, 1])
  }
  x <- as.numeric(x); y <- as.numeric(y)
  if (length(x) != length(y))
    stop("`x` and `y` must have the same length.", call. = FALSE)

  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  n <- length(x)
  if (n < 2) return(NA_real_)

  # The hull is a pure optimisation here, and it is skipped for small clouds
  # (where it would cost more than it saves) and whenever chull() declines --
  # a fully collinear or duplicated cloud is degenerate for a hull but has a
  # perfectly well-defined diameter.
  idx <- seq_len(n)
  if (n > 32) {
    h <- try(grDevices::chull(x, y), silent = TRUE)
    if (!inherits(h, "try-error") && length(h) >= 2) idx <- h
  }
  d <- stats::dist(cbind(x[idx], y[idx]))
  if (!length(d)) return(NA_real_)
  max(d)
}
