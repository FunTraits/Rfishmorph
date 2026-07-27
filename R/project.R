# =============================================================================
# project.R -- the FISHMORPH functional trait space (frozen PCA) and projection
# of new specimens onto it. Ported from intraitR::trait_space() /
# project_fishmorph().
# =============================================================================

#' Load the FISHMORPH reference database
#'
#' Reads the published FISHMORPH trait table (9 ratios + maximum body length and
#' width + taxonomy + IUCN status). Since Rfishmorph 0.2.0 the default is the
#' **full table** (`fishmorph_data.csv`, 8,970 species with all 9 ratios
#' complete), which is also what [launch_fishmorph_space()] explores. The
#' 400-species sample is still bundled and reachable with `file = "sample"` for
#' fast examples and tests.
#'
#' @section Trait scale:
#' The bundled tables are **already** `log10(x + 1)` transformed, both the full
#' one and the sample. Do not transform them again: calling
#' [fishmorph_trait_space()] with `log = TRUE` on this output would take the
#' logarithm twice and silently distort the ordination. Ratios recomputed from
#' landmarks with [fishmorph_ratios()] are on the raw scale and *do* need it.
#'
#' @param file Path to a FISHMORPH CSV (`;`-separated) or XLSX file. `NULL`
#'   (default) loads the full bundled table; `"sample"` loads the 400-species
#'   sample; `"full"` is an explicit synonym of `NULL`.
#' @param sheet Sheet name when `file` is an XLSX (default `"Global_ratios"` then
#'   the first sheet).
#' @return A data frame with columns `Species, Family, Order, Genus`, the 9 ratio
#'   columns, `MBl`, `MBw` and `IUCN` when available.
#' @seealso [fishmorph_space_data()] for the path of the bundled full table,
#'   [launch_fishmorph_space()] to explore it interactively.
#' @examples
#' ref <- load_fishmorph_reference()          # 8,970 species
#' nrow(ref)
#' small <- load_fishmorph_reference("sample")  # 400 species
#' @export
load_fishmorph_reference <- function(file = NULL, sheet = NULL) {
  # Symbolic shortcuts: they spare the caller a system.file() call for a file
  # the package bundles anyway.
  if (is.null(file) || identical(file, "full")) file <- "fishmorph_data.csv"
  else if (identical(file, "sample")) file <- "fishmorph_reference_sample.csv"

  if (!file.exists(file) && !grepl("[/\\\\]", file)) {
    bundled <- system.file("extdata", file, package = "Rfishmorph")
    if (nzchar(bundled)) file <- bundled
  }
  if (!nzchar(file) || !file.exists(file))
    stop("Reference table not found: ", file,
         "\n  Pass an explicit path, or reinstall 'Rfishmorph'.", call. = FALSE)
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("Package 'readxl' is required to read XLSX files.", call. = FALSE)
    sheets <- readxl::excel_sheets(file)
    sh <- sheet %||% intersect(c("Global_ratios", "Global_segments"), sheets)[1]
    if (is.na(sh)) sh <- sheets[1]
    df <- as.data.frame(readxl::read_excel(file, sheet = sh,
                                           guess_max = 1048576))
  } else {
    df <- utils::read.csv(file, sep = ";", dec = ".", check.names = FALSE,
                          stringsAsFactors = FALSE)
    if (ncol(df) == 1L)   # fall back to comma separator
      df <- utils::read.csv(file, sep = ",", dec = ".", check.names = FALSE,
                            stringsAsFactors = FALSE)
  }
  df
}

#' Build the FISHMORPH functional trait space
#'
#' Fits a principal component analysis on a set of (optionally log-transformed
#' and scaled) morphological traits. This is the ordination used to describe the
#' FISHMORPH functional space; once fitted it is frozen and reused to project new
#' specimens with [project_fishmorph()], so every projection lives in the same
#' coordinate system.
#'
#' @param data A data frame of traits, or a `fishmorph_landmarks` object (its
#'   ratios are computed first).
#' @param traits Character vector of trait columns. Defaults to the 9 FISHMORPH
#'   ratios.
#' @param groups Optional grouping vector (e.g. species) used by the plot method.
#' @param log Apply a `log10(x + 1)` transform to traits before ordination
#'   (default `FALSE`), matching [project_fishmorph()] and the FISHMORPH scale.
#'   Several
#'   FISHMORPH ratios are position ratios that legitimately reach 0 (e.g. `OGp`,
#'   `VEp`, `PFv`); log-transforming them would set those specimens to `NA`.
#'   Enable it only for size-like, strictly positive trait sets.
#' @param scale Scale traits to unit variance (default `TRUE`).
#' @param na_action How to handle specimens with missing traits. Same options as
#'   `intraitR::trait_space()`, so the two packages stay concordant:
#'   `"omit"` (default, drop incomplete rows), `"fail"`, `"impute_mean"`,
#'   `"impute_group_mean"` (needs `groups`), `"missforest"` (random-forest
#'   imputation via \pkg{missForest}), or `"missforest_phylo"` (missForest
#'   augmented with phylogenetic PCoA axes; see [phylo_pcoa()]). Imputation is
#'   performed on the raw trait scale (before any log transform).
#' @param missforest_ntree,missforest_maxiter Passed to [missForest::missForest()]
#'   for the missForest `na_action`s.
#' @param tree Used by `na_action = "missforest_phylo"`: a `"phylo"` object, or
#'   `NULL` (default) to use [load_fishmorph_phylogeny()].
#' @param missforest_phylo_k Max number of phylogenetic PCoA axes to add
#'   (default 10, the number available in the precomputed table).
#' @param phylo_axes Used by `"missforest_phylo"`. `NULL` (default) uses the
#'   **precomputed** axes of [load_fishmorph_phylo_axes()], so that every call
#'   shares one and the same phylogenetic coordinate system. Supply a data frame
#'   (a `species` column plus one column per axis) to use your own.
#' @param species Species identifier for **each row**, used only to look up the
#'   phylogenetic axes of `"missforest_phylo"`. `NULL` (default) auto-detects a
#'   `Genus.species` / `Species` / `species` column. This is deliberately
#'   separate from `groups`: the phylogeny needs to know which species a row
#'   belongs to, not a categorical predictor for the forest.
#' @return An object of class `fishmorph_trait_space` with elements `pca`
#'   (the `prcomp` object), `scores`, `X` (the trait matrix used), `traits`,
#'   `groups`, `log`, `scale`, `ids`, `na_action` and `imputed` (number of
#'   filled cells).
#' @examples
#' ref <- load_fishmorph_reference()
#' ts <- fishmorph_trait_space(ref, groups = ref$Family)
#' ts
#' \donttest{
#' # keep incomplete specimens by imputing with phylogenetically-informed
#' # random forests (as in intraitR), using species as groups:
#' ts2 <- fishmorph_trait_space(ref, groups = ref$Species,
#'                              na_action = "missforest_phylo")
#' }
#' @export
fishmorph_trait_space <- function(data, traits = fishmorph_ratio_names(),
                                  groups = NULL, log = FALSE, scale = TRUE,
                                  na_action = c("omit", "fail", "impute_mean",
                                                "impute_group_mean", "missforest",
                                                "missforest_phylo"),
                                  missforest_ntree = 100,
                                  missforest_maxiter = 10, tree = NULL,
                                  missforest_phylo_k = 10, phylo_axes = NULL,
                                  species = NULL) {
  na_action <- match.arg(na_action)
  if (is_fishmorph_landmarks(data)) data <- fishmorph_ratios(data)
  miss <- setdiff(traits, names(data))
  if (length(miss))
    stop("Trait column(s) not found: ", paste(miss, collapse = ", "),
         call. = FALSE)
  # `species` (the key to the phylogenetic axes) is detected automatically;
  # `groups` (a categorical predictor / colouring) no longer is: filling it with
  # the species amounted to handing randomForest thousands of levels.
  if (is.null(species)) {
    sc <- intersect(c("Genus.species", "Species", "species"), names(data))[1]
    if (!is.na(sc)) species <- data[[sc]]
  }
  if (is.null(groups) && na_action == "impute_group_mean") groups <- species
  ids <- if ("Species" %in% names(data)) data$Species else
    if ("specimen" %in% names(data)) data$specimen else
      seq_len(nrow(data))
  X <- as.matrix(data[, traits, drop = FALSE])
  storage.mode(X) <- "double"

  # non-finite values (Inf from a zero-length denominator) are NOT ordinary
  # missing data and are not handled by na_action -- reject them (as intraitR).
  non_finite <- !is.na(X) & !is.finite(X)
  if (any(non_finite)) {
    bad_cols <- unique(colnames(X)[which(non_finite, arr.ind = TRUE)[, "col"]])
    stop("`traits` contains non-finite value(s) (Inf/-Inf, not NA) in column(s): ",
         paste(bad_cols, collapse = ", "),
         ". This usually indicates a zero-length denominator segment; correct ",
         "the measurement(s) or set the entries to NA first.", call. = FALSE)
  }

  grp_factor <- if (!is.null(groups)) factor(groups) else NULL
  n_na_before <- sum(is.na(X))
  res <- .apply_na_action(X, grp_factor, na_action,
                          missforest_ntree = missforest_ntree,
                          missforest_maxiter = missforest_maxiter,
                          context = "traits", tree = tree,
                          missforest_phylo_k = missforest_phylo_k,
                          phylo_axes = phylo_axes, species = species)
  X <- res$X; keep <- res$keep
  ids <- ids[keep]
  groups <- if (!is.null(groups)) groups[keep] else NULL

  if (log) { X[X <= 0] <- NA_real_; X <- log10(X + 1) }

  # drop any residual NA (e.g. impute_group_mean with an unresolved group,
  # or a non-positive value removed by log); prcomp needs a complete matrix.
  ok <- stats::complete.cases(X)
  if (!all(ok)) {
    if (na_action == "fail")
      stop("Missing/invalid trait values remain after na_action.", call. = FALSE)
    if (sum(!ok) > 0 && na_action != "omit")
      message(sprintf("Dropping %d row(s) still incomplete after na_action = \"%s\".",
                      sum(!ok), na_action))
    X <- X[ok, , drop = FALSE]; ids <- ids[ok]
    groups <- if (!is.null(groups)) groups[ok] else NULL
  }

  n_imputed <- n_na_before - sum(is.na(res$X))
  grp <- if (!is.null(groups)) groups else NULL
  pca <- stats::prcomp(X, center = TRUE, scale. = scale)
  scores <- as.data.frame(pca$x)
  structure(list(pca = pca, scores = scores, X = X, traits = traits,
                 groups = grp, log = log, scale = scale, ids = ids,
                 na_action = na_action, imputed = n_imputed),
            class = "fishmorph_trait_space")
}

# Internal: project new specimens onto an existing fishmorph_trait_space (frozen
# PCA). Used by plot_functional_space(). The public projection function against
# the FISHMORPH reference database is project_fishmorph() (see
# project-fishmorph.R), a faithful port of intraitR.
.project_onto_space <- function(space, newdata, groups = NULL) {
  if (!inherits(space, "fishmorph_trait_space"))
    stop("`space` must be a fishmorph_trait_space object.", call. = FALSE)
  if (is_fishmorph_landmarks(newdata)) newdata <- fishmorph_ratios(newdata)
  traits <- space$traits
  miss <- setdiff(traits, names(newdata))
  if (length(miss))
    stop("Trait column(s) not found in newdata: ", paste(miss, collapse = ", "),
         call. = FALSE)
  ids <- if ("Species" %in% names(newdata)) newdata$Species else
    if ("specimen" %in% names(newdata)) newdata$specimen else
      seq_len(nrow(newdata))
  X <- as.matrix(newdata[, traits, drop = FALSE])
  storage.mode(X) <- "double"
  if (space$log) { X[X <= 0] <- NA_real_; X <- log10(X) }
  ok <- stats::complete.cases(X)
  X <- X[ok, , drop = FALSE]; ids <- ids[ok]
  sc <- as.data.frame(stats::predict(space$pca, newdata = X))
  attr(sc, "id") <- ids
  if (!is.null(groups)) attr(sc, "group") <- groups[ok]
  sc$id <- ids
  if (!is.null(groups)) sc$group <- groups[ok]
  sc
}

#' @export
print.fishmorph_trait_space <- function(x, ...) {
  ve <- summary(x$pca)$importance[2, ]
  cat("<fishmorph_trait_space>\n")
  cat(sprintf("  specimens : %d\n", nrow(x$X)))
  cat(sprintf("  traits    : %s\n", paste(x$traits, collapse = ", ")))
  cat(sprintf("  transform : %slog10, %sscaled\n",
              if (x$log) "" else "no ", if (x$scale) "" else "un"))
  cat(sprintf("  variance  : PC1 %.1f%%, PC2 %.1f%% (cum %.1f%%)\n",
              100 * ve[1], 100 * ve[2], 100 * sum(ve[1:2])))
  if (!is.null(x$na_action))
    cat(sprintf("  na_action : %s%s\n", x$na_action,
                if (!is.null(x$imputed) && x$imputed > 0)
                  sprintf(" (%d cell(s) imputed)", x$imputed) else ""))
  invisible(x)
}
