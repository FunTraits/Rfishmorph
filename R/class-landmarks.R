# =============================================================================
# class-landmarks.R -- the fishmorph_landmarks container and its I/O.
#
# A fishmorph_landmarks object stores digitized landmark configurations for one
# or more specimens:
#   coords   : numeric array [n_landmarks, 2, n_specimens], dimnames X/Y and
#              specimen names on the 3rd margin. Cartesian convention (Y up).
#   scale    : optional numeric vector (units per pixel) per specimen, or NULL.
#   metadata : data.frame, one row per specimen (row names = specimen names).
#
# The layout is deliberately identical to intraitR's "intrait_landmarks" so the
# two are interoperable; helpers below accept either class.
# =============================================================================

#' Create a fishmorph_landmarks object
#'
#' @param coords Numeric array `[n_landmarks, 2, n_specimens]` (X, Y on the 2nd
#'   margin), a `[n_landmarks, 2]` matrix (single specimen), or a data frame of
#'   landmark coordinates (see [read_landmarks_csv()] for the accepted layout).
#' @param metadata Optional data frame, one per specimen.
#' @param scale Optional numeric vector of scale factors (units per pixel).
#' @param specimen Optional specimen name(s) used when `coords` has none.
#' @param pad_to Minimum number of landmarks the object must carry. When the
#'   input has fewer (e.g. only 20 points, without the scale-bar landmarks
#'   20-21), the missing landmarks are added as `NA` rows so the configuration
#'   still follows the FISHMORPH scheme and is accepted by downstream functions
#'   (`fishmorph_segments()`, `plot_fishmorph_points()`, ...). Default `21L`
#'   (the 19 anatomical + 2 scale-bar landmarks). Set `NULL` to disable padding.
#' @return An object of class `fishmorph_landmarks`.
#' @examples
#' P <- matrix(rnorm(44), 22, 2, dimnames = list(NULL, c("X", "Y")))
#' fishmorph_landmarks(P, specimen = "demo")
#' # a 20-landmark config is padded to 21 (scale bar as NA):
#' dim(fishmorph_landmarks(matrix(rnorm(40), 20, 2))$coords)
#' @export
fishmorph_landmarks <- function(coords, metadata = NULL, scale = NULL,
                                specimen = NULL, pad_to = 21L) {
  if (is.data.frame(coords)) {
    return(.landmarks_from_long(coords, metadata = metadata, scale = scale))
  }
  if (is.matrix(coords)) {
    nm <- specimen %||% "specimen_1"
    arr <- array(NA_real_, dim = c(nrow(coords), 2L, 1L),
                 dimnames = list(NULL, c("X", "Y"), nm))
    arr[, , 1] <- coords[, 1:2]
    coords <- arr
  }
  if (!is.array(coords) || length(dim(coords)) != 3L)
    stop("`coords` must be a 3D array [landmarks, 2, specimens], a matrix or a data frame.",
         call. = FALSE)
  if (dim(coords)[2] != 2L)
    stop("The second dimension of `coords` must be 2 (X, Y).", call. = FALSE)
  dn <- dimnames(coords)
  if (is.null(dn)) dn <- vector("list", 3L)
  if (is.null(dn[[2]])) dn[[2]] <- c("X", "Y")
  sp <- dn[[3]] %||% (specimen %||% paste0("specimen_", seq_len(dim(coords)[3])))
  dn[[3]] <- sp
  dimnames(coords) <- dn
  # pad the landmark dimension with NA rows up to `pad_to`, so a configuration
  # missing e.g. the scale-bar landmarks 20-21 still follows the FISHMORPH
  # scheme and is accepted by downstream functions.
  if (!is.null(pad_to) && dim(coords)[1] < pad_to) {
    padded <- array(NA_real_, dim = c(pad_to, 2L, dim(coords)[3]),
                    dimnames = list(NULL, dn[[2]], sp))
    padded[seq_len(dim(coords)[1]), , ] <- coords
    coords <- padded
  }
  if (is.null(metadata)) {
    metadata <- data.frame(specimen = sp, stringsAsFactors = FALSE,
                           row.names = sp)
  }
  structure(list(coords = coords, scale = scale, metadata = metadata),
            class = "fishmorph_landmarks")
}

# internal: build coords array from a long/wide data frame
.landmarks_from_long <- function(df, id_col = NULL, metadata = NULL,
                                 scale = NULL) {
  nm <- names(df)
  # Wide layout: columns "<k>_X" / "<k>_Y".
  wide <- grepl("^[0-9]+_(X|Y)$", nm)
  if (any(wide)) {
    id_col <- id_col %||% intersect(c("specimen", "Genus.species", "id",
                                      "species"), nm)[1]
    ks <- sort(unique(as.integer(sub("_(X|Y)$", "", nm[wide]))))
    n_sp <- nrow(df)
    # Index rows by LANDMARK NUMBER, not by position: a sheet with landmarks
    # 1-19 and 22 (no scale bar 20-21) yields a 22-row array where landmark k
    # sits at row k and the absent landmarks (20, 21) are NA rows -- so the
    # FISHMORPH landmark indices used downstream stay correct.
    max_k <- max(ks)
    arr <- array(NA_real_, dim = c(max_k, 2L, n_sp),
                 dimnames = list(NULL, c("X", "Y"),
                                 if (!is.na(id_col)) as.character(df[[id_col]])
                                 else paste0("specimen_", seq_len(n_sp))))
    for (kk in ks) {
      xc <- paste0(kk, "_X"); yc <- paste0(kk, "_Y")
      if (xc %in% nm) arr[kk, 1, ] <- suppressWarnings(as.numeric(df[[xc]]))
      if (yc %in% nm) arr[kk, 2, ] <- suppressWarnings(as.numeric(df[[yc]]))
    }
    md <- if (!is.na(id_col))
      data.frame(specimen = as.character(df[[id_col]]),
                 row.names = as.character(df[[id_col]]),
                 stringsAsFactors = FALSE) else NULL
    return(fishmorph_landmarks(arr, metadata = metadata %||% md, scale = scale))
  }
  # Long layout: columns specimen, landmark, X, Y.
  need <- c("landmark", "X", "Y")
  if (!all(need %in% nm))
    stop("Data frame must have either '<k>_X'/'<k>_Y' columns or ",
         "'landmark', 'X', 'Y' (+ optional 'specimen').", call. = FALSE)
  id_col <- id_col %||% intersect(c("specimen", "Genus.species", "id",
                                    "species"), nm)[1]
  if (is.na(id_col)) { df$specimen <- "specimen_1"; id_col <- "specimen" }
  sp <- unique(as.character(df[[id_col]]))
  ks <- sort(unique(as.integer(df$landmark)))
  max_k <- max(ks)                         # rows indexed by landmark number
  arr <- array(NA_real_, dim = c(max_k, 2L, length(sp)),
               dimnames = list(NULL, c("X", "Y"), sp))
  for (s in sp) {
    sub <- df[as.character(df[[id_col]]) == s, , drop = FALSE]
    ix <- as.integer(sub$landmark)
    arr[ix, 1, s] <- suppressWarnings(as.numeric(sub$X))
    arr[ix, 2, s] <- suppressWarnings(as.numeric(sub$Y))
  }
  md <- data.frame(specimen = sp, row.names = sp, stringsAsFactors = FALSE)
  fishmorph_landmarks(arr, metadata = metadata %||% md, scale = scale)
}

#' Test whether an object is a landmark configuration
#'
#' @param x Any object. `TRUE` for `fishmorph_landmarks` or the interoperable
#'   `intrait_landmarks` class.
#' @return Logical scalar.
#' @export
is_fishmorph_landmarks <- function(x) {
  inherits(x, c("fishmorph_landmarks", "intrait_landmarks"))
}

#' Coerce to fishmorph_landmarks
#' @param x A `fishmorph_landmarks`, `intrait_landmarks`, matrix, array or data frame.
#' @param ... Passed to [fishmorph_landmarks()].
#' @return A `fishmorph_landmarks` object.
#' @export
as_fishmorph_landmarks <- function(x, ...) {
  if (inherits(x, "fishmorph_landmarks")) return(x)
  if (inherits(x, "intrait_landmarks")) {
    obj <- structure(list(coords = x$coords, scale = x$scale,
                          metadata = x$metadata),
                     class = "fishmorph_landmarks")
    return(obj)
  }
  fishmorph_landmarks(x, ...)
}

#' Coerce to an intraitR `intrait_landmarks` object
#'
#' Returns the same landmark configuration relabelled as `intrait_landmarks`, so
#' Rfishmorph outputs can be passed to `intraitR` functions such as
#' `plot_fishmorph_points()`, `gpa_fish()` or `fishmorph_segments()`. The
#' internal layout (`coords`, `scale`, `metadata`) is identical between the two
#' classes; only the class attribute changes.
#'
#' @param x A `fishmorph_landmarks` (or interoperable) object.
#' @return An object of class `intrait_landmarks`.
#' @examples
#' lm <- reconstruct_fishmorph_landmarks(
#'   list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
#'        PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0))
#' x <- as_intrait_landmarks(lm)
#' class(x)
#' # intraitR::plot_fishmorph_points(x, specimen = "reconstructed")
#' @export
as_intrait_landmarks <- function(x) {
  x <- as_fishmorph_landmarks(x)
  structure(list(coords = x$coords, scale = x$scale, metadata = x$metadata),
            class = "intrait_landmarks")
}

# internal accessor: coords array regardless of the (interoperable) class
.fm_coords <- function(x) {
  if (is_fishmorph_landmarks(x)) return(x$coords)
  stop("Expected a fishmorph_landmarks object.", call. = FALSE)
}

.fm_specimens <- function(x) dimnames(.fm_coords(x))[[3]]

#' @export
print.fishmorph_landmarks <- function(x, ...) {
  d <- dim(x$coords)
  cat("<fishmorph_landmarks>\n")
  cat(sprintf("  specimens : %d\n", d[3]))
  cat(sprintf("  landmarks : %d (2D)\n", d[1]))
  sp <- .fm_specimens(x)
  cat("  names     : ", paste(utils::head(sp, 4), collapse = ", "),
      if (length(sp) > 4) sprintf(", ... (+%d)", length(sp) - 4) else "", "\n",
      sep = "")
  if (!is.null(x$scale)) cat("  scale     : provided\n")
  invisible(x)
}

#' Read digitized landmarks from a CSV file
#'
#' Accepts two layouts. **Wide**: one row per specimen with columns
#' `1_X, 1_Y, 2_X, 2_Y, ...` plus an identifier column (`specimen`,
#' `Genus.species`, `id` or `species`). **Long**: columns `specimen`,
#' `landmark`, `X`, `Y`.
#'
#' @param file Path to the CSV file.
#' @param sep Field separator (default `","`; the FISHMORPH exports use `";"`).
#' @param dec Decimal mark.
#' @param id_col Optional name of the identifier column.
#' @return A `fishmorph_landmarks` object.
#' @seealso [write_landmarks_csv()]
#' @export
read_landmarks_csv <- function(file, sep = ",", dec = ".", id_col = NULL) {
  df <- utils::read.csv(file, sep = sep, dec = dec, check.names = FALSE,
                        stringsAsFactors = FALSE)
  .landmarks_from_long(df, id_col = id_col)
}

#' Write landmarks to a CSV file (long layout)
#'
#' @param x A `fishmorph_landmarks` object.
#' @param file Output path.
#' @param cartesian If `TRUE` (default) keep the Cartesian convention (Y up).
#'   Set `FALSE` to flip Y for image (Y-down) exports.
#' @param image_height Image height in pixels, required when `cartesian = FALSE`.
#' @return Invisibly, the written data frame.
#' @export
write_landmarks_csv <- function(x, file, cartesian = TRUE,
                                image_height = NULL) {
  co <- .fm_coords(x); sp <- .fm_specimens(x)
  rows <- list()
  for (s in seq_along(sp)) {
    P <- co[, , s]
    Y <- if (cartesian) P[, 2] else {
      if (is.null(image_height))
        stop("`image_height` is required when cartesian = FALSE.", call. = FALSE)
      image_height - P[, 2]
    }
    rows[[s]] <- data.frame(specimen = sp[s], landmark = seq_len(nrow(P)),
                            X = P[, 1], Y = Y, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  utils::write.csv(out, file, row.names = FALSE)
  invisible(out)
}
