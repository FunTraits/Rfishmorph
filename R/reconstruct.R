# =============================================================================
# reconstruct.R -- rebuild the FISHMORPH landmarks from segment lengths.
#
# This is the (partial) inverse of fishmorph_segments(): given the 11 segment
# lengths and a 2-point anchor (snout + caudal base), it places all 21 landmarks
# so that the round-trip segments -> landmarks -> segments returns the input
# segments exactly. The free parameters (axial positions, dorso-ventral split,
# free-segment angles) affect only PLACEMENT, never lengths -- they are the
# information a photo re-injects. Ported from prototype_reconstruct_fishmorph.R.
# =============================================================================

.FM_RECON_SEGMENTS <- c("Bl", "Bd", "Hd", "Eh", "Mo", "PFi", "PFl", "Ed",
                        "Jl", "CPd", "CFd")

#' Default free parameters for landmark reconstruction
#'
#' The parameters NOT identified by the segments alone: `f_*` = fraction along
#' the body axis (0 = snout, 1 = caudal base); `o_*` = dorsal share of a depth
#' (0..1); `ang_*` = angle (degrees) of a free segment relative to the body axis.
#'
#' @param preset Which default set to return. `"template"` is the generic
#'   FISHMORPH gabarit; `"app"` is the set recalibrated on the medians of the
#'   already-digitized species in the FISHMORPH workbook, as used by
#'   [launch_fishmorph_digitizer()] (head and pectoral placed more
#'   realistically; `o_PF` is negative because the pectoral insertion sits below
#'   the body midline).
#' @return A named list of default parameter values.
#' @export
fishmorph_reconstruction_defaults <- function(preset = c("template", "app")) {
  preset <- match.arg(preset)
  if (preset == "app") {
    # Position offsets recalibrated on the workbook medians (as in the app).
    # Angle signs are expressed in this package's Cartesian engine and coincide
    # with the template values (the app stores them in the opposite,
    # image-Y-down convention).
    return(list(
      f_Bd  = 0.47, o_Bd  = 0.50,
      f_Hd  = 0.10, o_Hd  = 0.43,
      f_eye = 0.10, o_eye = 0.82,
      f_PF  = 0.25, o_PF  = -0.69,
      ang_PFl = -35,
      ang_Jl  = -20,
      f_CP  = 0.93, o_CP  = 0.52,
      f_CF  = 1.15, o_CF  = 0.47
    ))
  }
  list(
    f_Bd  = 0.38, o_Bd  = 0.55,
    f_Hd  = 0.12, o_Hd  = 0.85,
    f_eye = 0.10, o_eye = 0.10,
    f_PF  = 0.16, o_PF  = 0.90,
    ang_PFl = -35,
    ang_Jl  = -20,
    f_CP  = 0.93, o_CP  = 0.60,
    f_CF  = 1.06, o_CF  = 0.60
  )
}

#' Reconstruct the 21 FISHMORPH landmarks from segments
#'
#' @param segments A data frame (one row) or named list holding the 11 segments
#'   `Bl, Bd, Hd, Eh, Mo, PFi, PFl, Ed, Jl, CPd, CFd` (in cm).
#' @param anchor_snout,anchor_caudal Optional `c(x, y)` pixel positions of the
#'   snout (landmark 1) and caudal base (landmark 2). When both are given, the
#'   scale is deduced from `|2-1| / Bl`. When `NULL`, a synthetic horizontal
#'   anchor is generated using `px_per_cm`.
#' @param px_per_cm Scale used only for the synthetic anchor.
#' @param params Named list of free parameters (see
#'   [fishmorph_reconstruction_defaults()]); missing entries take the value from
#'   `preset`. Explicit `params` always override the preset.
#' @param scale_cm Real length (cm) of the calibration segment (landmarks 20-21).
#' @param specimen Specimen name.
#' @param preset Default placement rules: `"template"` (generic gabarit) or
#'   `"app"` to reproduce [launch_fishmorph_digitizer()] /
#'   `fishmorph_reconstruct_app` (offsets recalibrated on the workbook medians).
#'   Both keep the segment lengths **locked** to the input, so the round-trip
#'   stays exact regardless of preset.
#' @param add_curvature Place the optional body-length curvature landmark 22 on
#'   the axis midpoint (as the app does). It lies on the straight 1-2 line, so
#'   `Bl` is unchanged and the round-trip remains exact. Adds a 22nd landmark.
#' @return A `fishmorph_landmarks` object with 21 (or 22) landmarks in the
#'   Cartesian convention (Y up), compatible with [fishmorph_segments()].
#' @examples
#' seg <- list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
#'             PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0)
#' lm  <- reconstruct_fishmorph_landmarks(seg)                 # template
#' lm2 <- reconstruct_fishmorph_landmarks(seg, preset = "app") # app rules
#' fishmorph_segments(lm2, scale_cm = 1)[, c("Bl", "Bd", "Hd")]
#' @export
reconstruct_fishmorph_landmarks <- function(segments,
                                            anchor_snout = NULL,
                                            anchor_caudal = NULL,
                                            px_per_cm = 50,
                                            params = list(),
                                            scale_cm = 1,
                                            specimen = "reconstructed",
                                            preset = c("template", "app"),
                                            add_curvature = FALSE) {
  preset <- match.arg(preset)
  if (is.data.frame(segments)) {
    if (nrow(segments) < 1L) stop("`segments` is empty.", call. = FALSE)
    s <- as.list(segments[1L, , drop = FALSE])
  } else s <- as.list(segments)
  miss <- setdiff(.FM_RECON_SEGMENTS, names(s))
  if (length(miss))
    stop("Missing segment(s): ", paste(miss, collapse = ", "), call. = FALSE)
  s <- lapply(s[.FM_RECON_SEGMENTS], function(x) as.numeric(x)[1L])

  p <- utils::modifyList(fishmorph_reconstruction_defaults(preset), params)

  if (is.null(anchor_snout) || is.null(anchor_caudal)) {
    A <- c(0, 0); B <- c(s$Bl * px_per_cm, 0)
  } else {
    A <- as.numeric(anchor_snout); B <- as.numeric(anchor_caudal)
    px_per_cm <- sqrt(sum((B - A)^2)) / s$Bl
  }
  u   <- (B - A) / sqrt(sum((B - A)^2))
  nrm <- c(-u[2], u[1])
  cm  <- function(v) v * px_per_cm
  station <- function(f) A + cm(s$Bl) * f * u

  npt <- if (add_curvature) 22L else 21L
  P <- matrix(NA_real_, nrow = npt, ncol = 2, dimnames = list(NULL, c("X", "Y")))
  put  <- function(i, xy) P[i, ] <<- xy
  vseg <- function(f, L, o) {
    st <- station(f)
    list(top = st + cm(L) * o * nrm, bot = st - cm(L) * (1 - o) * nrm)
  }

  put(1, A); put(2, B)
  v <- vseg(p$f_Bd, s$Bd, p$o_Bd); put(3, v$top); put(4, v$bot)
  v <- vseg(p$f_Hd, s$Hd, p$o_Hd); put(5, v$top); put(6, v$bot)
  eye_st <- station(p$f_eye)
  put(8, eye_st - cm(s$Hd) * p$o_eye * nrm)
  put(7, P[8, ] + cm(s$Eh) * nrm)
  put(13, P[7, ] + cm(s$Ed / 2) * nrm)
  put(14, P[7, ] - cm(s$Ed / 2) * nrm)
  put(9, P[1, ] - cm(s$Mo) * nrm)
  v <- vseg(p$f_PF, s$PFi, p$o_PF); put(10, v$top); put(11, v$bot)
  dir_pf <- cos(p$ang_PFl * pi / 180) * u + sin(p$ang_PFl * pi / 180) * nrm
  put(12, P[10, ] + cm(s$PFl) * dir_pf)
  dir_jl <- cos(p$ang_Jl * pi / 180) * u + sin(p$ang_Jl * pi / 180) * nrm
  put(15, P[1, ] + cm(s$Jl) * dir_jl)
  v <- vseg(p$f_CP, s$CPd, p$o_CP); put(16, v$top); put(17, v$bot)
  v <- vseg(p$f_CF, s$CFd, p$o_CF); put(18, v$top); put(19, v$bot)

  # scale bar 20-21 with known pixel length = scale_cm * px_per_cm
  base <- A - nrm * cm(max(unlist(s)) * 0.9)
  put(20, base)
  put(21, base + u * (px_per_cm * scale_cm))

  # optional body-length curvature point on the straight axis (keeps Bl exact)
  if (add_curvature) put(22, station(0.5))

  fishmorph_landmarks(P, specimen = specimen)
}

#' Check the reconstruction round-trip
#'
#' Verifies that reconstructing landmarks from segments and re-measuring them
#' returns the input segments to machine precision, whatever the free
#' parameters. A large error signals a geometry bug, not a placement choice.
#'
#' @param segments Named list / data frame of the 11 segments (cm). If `NULL`, a
#'   built-in test specimen is used.
#' @param tol Tolerance for the OK/FAIL verdict.
#' @param ... Passed to [reconstruct_fishmorph_landmarks()].
#' @return Invisibly, a data frame with `input`, `recovered` and `abs_error`.
#' @examples
#' check_reconstruction_roundtrip()
#' @export
check_reconstruction_roundtrip <- function(segments = NULL, tol = 1e-6, ...) {
  if (is.null(segments)) {
    segments <- list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6,
                     PFi = 1.9, PFl = 2.1, Ed = 0.6, Jl = 1.3,
                     CPd = 1.0, CFd = 3.0)
  }
  lm <- reconstruct_fishmorph_landmarks(segments, ...)
  rec <- fishmorph_segments(lm, scale_cm = 1)
  rec <- rec[, .FM_RECON_SEGMENTS, drop = FALSE]
  inp <- unlist(segments[.FM_RECON_SEGMENTS])
  out <- data.frame(input = inp, recovered = as.numeric(rec[1, ]),
                    abs_error = abs(inp - as.numeric(rec[1, ])))
  max_err <- max(out$abs_error)
  message(sprintf("Round-trip: max error = %.2e cm (%s)",
                  max_err, if (max_err < tol) "OK" else "FAIL"))
  invisible(out)
}
