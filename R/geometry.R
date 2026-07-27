# =============================================================================
# geometry.R -- geometric standardization and the FISHMORPH digitizing
# conventions. Ported from intraitR (standardize_geometry(),
# correct_geometry_conventions()) and the reconstruct app's .fm_apply_conventions().
# =============================================================================

#' Standardize landmark geometry (rescale, translate, optionally orient)
#'
#' Places every specimen in a common frame: landmark 1 (snout) at the origin and
#' the body axis 1-2 along the positive X direction. This is a similarity
#' transform (rotation + translation + optional isotropic rescale); it does not
#' alter any segment ratio.
#'
#' @param x A `fishmorph_landmarks` object.
#' @param orient If `TRUE` (default), rotate so the body axis 1-2 is horizontal
#'   with the snout on the left.
#' @param rescale If `TRUE`, rescale each specimen so `Bl = |1-2| = 1`. Useful to
#'   overlay specimens of different sizes; leaves ratios unchanged.
#' @param dorsal_up If `TRUE`, canonically orient each specimen dorsal-side up by
#'   reflecting it across the body axis whenever the dorsal landmark (3) lies
#'   below the ventral landmark (4). This removes the up/down mirror difference
#'   between configurations digitized in image coordinates (Y increasing
#'   downward) and those in Cartesian coordinates (e.g.
#'   [reconstruct_fishmorph_landmarks()] output), so measured and reconstructed
#'   fish overlay in the same orientation. A reflection preserves all segments
#'   and ratios. Default `FALSE`.
#' @return A `fishmorph_landmarks` object in the standardized frame.
#' @export
standardize_geometry <- function(x, orient = TRUE, rescale = FALSE,
                                 dorsal_up = FALSE) {
  co <- .fm_coords(x); sp <- .fm_specimens(x); npt <- dim(co)[1]
  for (i in seq_along(sp)) {
    P <- co[, , i]
    if (!all(is.finite(P[1, ])) || !all(is.finite(P[2, ]))) next
    A <- P[1, ]; B <- P[2, ]
    P <- sweep(P, 2, A)                       # translate snout to origin
    if (orient) {
      d <- B - A; L <- sqrt(sum(d^2))
      if (is.finite(L) && L > 0) {
        th <- atan2(d[2], d[1])
        # rotate by -th so the axis lies on +X
        Rm <- matrix(c(cos(-th), sin(-th), -sin(-th), cos(-th)), 2, 2)
        P <- t(Rm %*% t(P))
      }
    }
    # canonical dorsal-up orientation: reflect across the body axis (1-2) when
    # the dorsal landmark 3 sits below the ventral landmark 4. Works in the axis
    # frame, so it is valid whether or not `orient` was applied.
    if (dorsal_up && npt >= 4 && all(is.finite(P[3, ])) && all(is.finite(P[4, ]))) {
      A2 <- P[1, ]; d2 <- P[2, ] - A2; L2 <- sqrt(sum(d2^2))
      if (is.finite(L2) && L2 > 0) {
        u <- d2 / L2; n <- c(-u[2], u[1])     # dorsal-up normal (Cartesian)
        no3 <- sum((P[3, ] - A2) * n); no4 <- sum((P[4, ] - A2) * n)
        if (is.finite(no3) && is.finite(no4) && no3 < no4) {
          # reflect every point across the axis line: keep axial coord, flip normal
          for (j in seq_len(npt)) if (all(is.finite(P[j, ]))) {
            ax <- sum((P[j, ] - A2) * u); no <- sum((P[j, ] - A2) * n)
            P[j, ] <- A2 + ax * u - no * n
          }
        }
      }
    }
    if (rescale) {
      L <- sqrt(sum((P[2, ] - P[1, ])^2))
      if (is.finite(L) && L > 0) P <- P / L
    }
    co[, , i] <- P
  }
  fishmorph_landmarks(co, metadata = x$metadata, scale = x$scale)
}

# internal: apply the 5 conventions to a single 2D matrix, working in the frame
# of the body axis 1-2 (valid even if the specimen is tilted).
.apply_conventions_matrix <- function(P, tolerance_coord = 1e-6) {
  npt <- nrow(P)
  A <- P[1, ]; B <- P[2, ]
  L <- sqrt(sum((B - A)^2))
  if (!is.finite(L) || L == 0) return(P)
  u <- (B - A) / L                 # antero-posterior axis (unit)
  n <- c(-u[2], u[1])              # dorsal normal (rotate +90)
  fin <- function(i) i <= npt && all(is.finite(P[i, ]))
  ax <- rep(NA_real_, npt); no <- rep(NA_real_, npt)
  for (i in seq_len(npt)) if (fin(i)) {
    ax[i] <- sum((P[i, ] - A) * u); no[i] <- sum((P[i, ] - A) * n)
  }
  # (1) perpendiculars to the axis: dependent point shares the driver's AXIAL
  #     coordinate -> segment perpendicular to the body axis.
  if (fin(1) && fin(9))   ax[9]  <- ax[1]
  if (fin(3) && fin(4))   ax[4]  <- ax[3]
  if (fin(10) && fin(11)) ax[11] <- ax[10]
  # (2) eye vertical: shared axial coordinate = group median.
  eg <- c(5, 13, 7, 14, 6, 8); eg <- eg[eg <= npt]
  if (any(is.finite(ax[eg]))) ax[eg] <- stats::median(ax[eg], na.rm = TRUE)
  # (3) belly line: shared normal coordinate = group median.
  hg <- c(9, 8, 11, 4); hg <- hg[hg <= npt]
  if (any(is.finite(no[hg]))) no[hg] <- stats::median(no[hg], na.rm = TRUE)
  for (i in seq_len(npt)) if (is.finite(ax[i]) && is.finite(no[i]))
    P[i, ] <- A + ax[i] * u + no[i] * n
  P
}

# internal: "straighten" (unbend) a bent specimen along its broken body axis.
# The axis is the polyline 1 -> (22) -> (24) -> 2 built from whichever hinge
# landmarks (22, 24, 25) are present, ordered along the 1-2 chord. Each body
# segment is rotated rigidly about its (straightened) proximal end so the whole
# chain becomes a single straight line in the direction of the first segment
# (1 -> first hinge); arc length is preserved (rotation + translation, no
# scaling). Every landmark is carried by the segment whose axial interval
# contains it. With no hinge present the specimen is returned unchanged, so
# straight fish (and data without landmark 22/24) are untouched.
.unbend_matrix <- function(P) {
  npt <- nrow(P)
  fin <- function(i) i <= npt && all(is.finite(P[i, ]))
  if (!(fin(1) && fin(2))) return(P)
  hinges <- c(22L, 24L, 25L); hinges <- hinges[hinges <= npt]
  hs <- hinges[vapply(hinges, fin, logical(1))]
  if (!length(hs)) return(P)
  uc <- P[2, ] - P[1, ]; Lc <- sqrt(sum(uc^2))
  if (!is.finite(Lc) || Lc == 0) return(P)
  uc <- uc / Lc
  axof <- function(i) sum((P[i, ] - P[1, ]) * uc)     # abscissa along the 1-2 chord

  # A hinge can only be an INTERMEDIATE point of the axis if it projects
  # STRICTLY between the snout (abscissa 0) and the caudal base (Lc). A hinge
  # left at a default position, inherited from another specimen, or placed
  # askew on a strongly curved fish may project OUTSIDE that interval. Keeping
  # it made `axc` non-monotonic, and findInterval() failed with "'vec' must be
  # sorted non-decreasingly and not contain NAs". It is therefore discarded
  # rather than forced back onto the axis: an aberrant hinge is not information
  # to correct, it is information to ignore. The specimen is then straightened
  # with the remaining hinges, or left as it is if none remain.
  axh <- vapply(hs, axof, numeric(1))
  keep <- is.finite(axh) & axh > 0 & axh < Lc
  n_drop <- sum(!keep)
  hs <- hs[keep]; axh <- axh[keep]
  if (!length(hs)) return(structure(P, hinges_ignored = n_drop))

  o <- order(axh); hs <- hs[o]; axh <- axh[o]
  chain <- c(1L, hs, 2L)
  u1 <- P[chain[2L], ] - P[1, ]; L1 <- sqrt(sum(u1^2))
  if (!is.finite(L1) || L1 == 0) return(structure(P, hinges_ignored = n_drop))
  u1 <- u1 / L1
  # straightened positions of the chain points (along u1, arc length preserved)
  S <- matrix(NA_real_, length(chain), 2L); S[1, ] <- P[1, ]
  for (k in 2:length(chain))
    S[k, ] <- S[k - 1L, ] + sqrt(sum((P[chain[k], ] - P[chain[k - 1L], ])^2)) * u1
  # Built from the values already filtered and sorted, not recomputed: axof(1)
  # is 0 and axof(2) is Lc by definition of uc. cummax() further absorbs
  # equalities to within an epsilon, so that `axc` is non-decreasing BY
  # CONSTRUCTION -- findInterval's precondition can no longer be violated.
  axc <- cummax(c(0, axh, Lc))
  Pout <- P
  for (j in seq_len(npt)) {
    if (!fin(j)) next
    k <- findInterval(axof(j), axc, rightmost.closed = TRUE)
    k <- max(1L, min(k, length(chain) - 1L))
    a0 <- P[chain[k], ]; d0 <- P[chain[k + 1L], ] - a0
    if (sqrt(sum(d0^2)) == 0) next
    d1 <- S[k + 1L, ] - S[k, ]
    th <- atan2(d1[2], d1[1]) - atan2(d0[2], d0[1])
    R  <- matrix(c(cos(th), sin(th), -sin(th), cos(th)), 2L, 2L)
    Pout[j, ] <- S[k, ] + as.numeric(R %*% (P[j, ] - a0))
  }
  structure(Pout, hinges_ignored = n_drop)
}

#' Apply the "zero-ratio" conventions to landmarks
#'
#' Some FISHMORPH position ratios are set to exactly 0 by convention for certain
#' species -- e.g. a terminal mouth has `OGp = 0`, a ventral pectoral insertion
#' has `PFv = 0`. A ratio `R = num / den` is 0 when its numerator segment has
#' zero length, i.e. when the two landmarks defining that segment coincide. This
#' function enforces that on the landmark configuration: for every specimen whose
#' reference value of a chosen ratio is 0, it collapses the relevant landmark
#' onto its partner so the landmark-derived ratio also becomes exactly 0
#' (matching the convention seen as the vertical stripe at `published = 0` in the
#' segment-vs-landmark scatter).
#'
#' The collapses applied (numerator segment -> landmarks moved):
#' `OGp` (`Mo = |1-9|`) moves 9 onto 1; `PFv` (`PFi = |10-11|`) moves 11 onto 10;
#' `VEp` (`Eh = |7-8|`) moves 8 onto 7. Any ratio whose numerator is a segment
#' can be added via `ratios`.
#'
#' @param landmarks A `fishmorph_landmarks` object.
#' @param reference A data frame carrying the reference ratio values (the
#'   convention), with an identifier column matching the specimen names and a
#'   column for each ratio in `ratios` (e.g. the published FISHMORPH ratio
#'   table). If the ratio columns are absent but segment columns are present,
#'   the ratios are computed with [fishmorph_ratios()].
#' @param id_col Name of the identifier column in `reference` (default tries
#'   `Genus.species`, `specimen`, `species`, `id`).
#' @param ratios Which zero-conventions to enforce (default `c("OGp", "PFv")`).
#' @param tol Reference values `<= tol` are treated as zero (default 0).
#' @return The corrected `fishmorph_landmarks` object. `attr(, "n_collapsed")`
#'   reports how many specimens were adjusted per ratio.
#' @seealso [correct_geometry_conventions()], [compare_segments_landmarks()]
#' @examples
#' ref <- load_fishmorph_reference()          # has OGp, PFv columns
#' lm  <- reconstruct_fishmorph_landmarks(
#'   list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
#'        PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0),
#'   specimen = ref$Species[1])
#' correct_zero_ratio_landmarks(lm, ref, id_col = "Species")
#' @export
correct_zero_ratio_landmarks <- function(landmarks, reference, id_col = NULL,
                                         ratios = c("OGp", "PFv"), tol = 0) {
  if (!is_fishmorph_landmarks(landmarks))
    stop("`landmarks` must be a fishmorph_landmarks object.", call. = FALSE)
  reference <- as.data.frame(reference)
  id_col <- id_col %||% intersect(c("Genus.species", "specimen", "species",
                                    "id"), names(reference))[1]
  if (is.na(id_col))
    stop("Could not find an id column in `reference`; set `id_col`.",
         call. = FALSE)
  reference <- .ensure_id_column(reference, id_col, "reference")
  ratios <- intersect(ratios, fishmorph_ratio_names())
  if (!length(ratios)) stop("No valid ratio in `ratios`.", call. = FALSE)
  # make sure the ratio values are available (compute from segments if needed)
  need <- setdiff(ratios, names(reference))
  if (length(need)) {
    rr <- fishmorph_ratios(reference)
    for (r in need) if (r %in% names(rr)) reference[[r]] <- rr[[r]]
  }

  co <- .fm_coords(landmarks); sp <- as.character(.fm_specimens(landmarks))
  npt <- dim(co)[1]
  ids <- as.character(reference[[id_col]])
  n_matched <- sum(!is.na(match(sp, ids)))
  if (n_matched == 0)
    warning("None of the ", length(sp), " specimen name(s) match `reference$",
            id_col, "`; nothing corrected. Check `id_col` / name formatting ",
            "(dots vs spaces).", call. = FALSE)
  n_collapsed <- stats::setNames(integer(length(ratios)), ratios)

  for (r in ratios) {
    if (!r %in% names(reference)) next
    num_seg <- .FM_RATIO_DEF[[r]][1]            # numerator segment name
    pr <- .FM_SEGMENT_PAIRS[[num_seg]]          # c(a, b): move b onto a
    a <- pr[1]; b <- pr[2]
    if (a > npt || b > npt) next
    refnum <- suppressWarnings(as.numeric(reference[[r]]))
    idx <- match(sp, ids)                       # reference row for each specimen
    for (i in seq_along(sp)) {
      j <- idx[i]
      if (is.na(j)) next
      v <- refnum[j]
      if (is.finite(v) && v <= tol && all(is.finite(co[a, , i]))) {
        co[b, , i] <- co[a, , i]                # collapse b onto a -> segment = 0
        n_collapsed[r] <- n_collapsed[r] + 1L
      }
    }
  }
  out <- fishmorph_landmarks(co, metadata = landmarks$metadata,
                             scale = landmarks$scale, pad_to = NULL)
  attr(out$coords, "imputed") <- attr(.fm_coords(landmarks), "imputed")
  attr(out, "n_collapsed") <- n_collapsed
  message(sprintf("correct_zero_ratio_landmarks(): collapsed %s.",
                  paste(sprintf("%s in %d specimen(s)", names(n_collapsed),
                                n_collapsed), collapse = ", ")))
  out
}

#' Enforce the FISHMORPH digitizing conventions (optionally straightening)
#'
#' First, when `straighten = TRUE` (default), each bent specimen is "unbent"
#' along its broken body axis: the polyline 1 -> 22 -> 24 -> 2 (using whichever
#' of the hinge landmarks 22/24/25 are present, ordered along the 1-2 chord) is
#' laid out as a single straight line by rotating each body segment rigidly about
#' its proximal hinge (arc length preserved). Specimens without any hinge
#' landmark -- i.e. straight fish, or data digitized without landmark 22 -- are
#' left unchanged, so the behaviour is backward compatible.
#'
#' Then the five geometric conventions that a correctly digitized FISHMORPH
#' configuration must satisfy are applied, in the frame of the (now straight)
#' body axis 1-2:
#' \enumerate{
#'   \item landmark 9 shares the axial position of the snout (1);
#'   \item landmark 4 shares the axial position of 3 (`Bd` vertical);
#'   \item landmark 11 shares the axial position of 10 (`PFi` vertical);
#'   \item the eye group `{5,13,7,14,6,8}` shares one axial position (eye vertical);
#'   \item the belly group `{9,8,11,4}` shares one normal position (belly line).
#' }
#' Perpendicularity (3-4, 10-11 vertical) and the belly line are thus enforced
#' relative to the straightened axis. The transform snaps to group medians and
#' leaves landmark 1 fixed.
#'
#' @param x A `fishmorph_landmarks` object.
#' @param tolerance_coord Coordinate tolerance (kept for interface parity).
#' @param straighten If `TRUE` (default), unbend the broken axis 1-22-24-2 before
#'   applying the conventions. Set `FALSE` to keep the historical straight-axis
#'   behaviour (single axis 1-2, no unbending).
#' @return A corrected `fishmorph_landmarks` object.
#' @export
correct_geometry_conventions <- function(x, tolerance_coord = 1e-6,
                                         straighten = TRUE) {
  co <- .fm_coords(x); sp <- .fm_specimens(x)
  dropped <- integer(0)
  for (i in seq_along(sp)) {
    P <- co[, , i]
    if (isTRUE(straighten)) {
      P <- .unbend_matrix(P)                       # redresse l'axe brise 1-22-24-2
      nd <- attr(P, "hinges_ignored")
      if (!is.null(nd) && nd > 0L) dropped <- c(dropped, i)
    }
    co[, , i] <- .apply_conventions_matrix(P, tolerance_coord)
  }
  # An AGGREGATED warning, and not one per specimen: over a database of several
  # thousand individuals, one alert per row would be ignored. These specimens
  # were processed, simply with fewer hinges than expected.
  if (length(dropped))
    warning(length(dropped), " specimen(s) with at least one hinge (22/24/25) ",
            "projecting outside the snout -> caudal-base segment: it was ",
            "ignored when straightening. First affected: ",
            paste(utils::head(sp[dropped], 5), collapse = ", "),
            ". Check those points in launch_fishmorph_digitizer(mode = \"correct\").",
            call. = FALSE)
  fishmorph_landmarks(co, metadata = x$metadata, scale = x$scale)
}
