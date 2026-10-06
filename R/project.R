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
#' @section Segment- vs landmark-derived traits:
#' Two measurement campaigns describe the same species pool. `source =
#' "segment"` reads the published table, whose ratios come from the eleven
#' segments measured on the plates (Brosse et al. 2021). `source = "landmark"`
#' reads `fishmorph_data_landmarks.csv`, whose ratios are recomputed from the
#' landmark re-digitization through [fishmorph_segments()], and which therefore
#' covers **only the species already digitized** -- fewer rows, reported on
#' load. The two tables share their column names, separator and `log10(x + 1)`
#' scale, so they are interchangeable wherever a reference is expected. The
#' default is read from `getOption("fishmorph.source")` and can be set once per
#' session with [set_fishmorph_source()]. See
#' [build_fishmorph_landmark_table()] for how the landmark table is produced.
#'
#' @param file Path to a FISHMORPH CSV (`;`-separated) or XLSX file. `NULL`
#'   (default) loads the full bundled table for the active `source`; `"sample"`
#'   loads the 400-species segment sample; `"full"` is an explicit synonym of
#'   `NULL`.
#' @param sheet Sheet name when `file` is an XLSX (default `"Global_ratios"` then
#'   the first sheet).
#' @param source Which measurement campaign to read: `"segment"` (the published
#'   table) or `"landmark"` (the re-digitized one). Defaults to
#'   `getOption("fishmorph.source", "segment")`. Ignored when `file` is an
#'   explicit path.
#' @param quiet Suppress the one-line message reporting which table was loaded
#'   and how many species it holds (default `FALSE`). That message exists so an
#'   analysis can never silently run on the partial landmark pool.
#' @return A data frame with columns `Species, Family, Order, Genus`, the 9 ratio
#'   columns, `MBl`, `MBw` and `IUCN` when available.
#' @seealso [fishmorph_space_data()] for the path of the bundled full table,
#'   [set_fishmorph_source()], [build_fishmorph_landmark_table()],
#'   [launch_fishmorph_space()] to explore it interactively.
#' @examples
#' ref <- load_fishmorph_reference()          # 8,970 species (segments)
#' nrow(ref)
#' small <- load_fishmorph_reference("sample")  # 400 species
#' \dontrun{
#' lmk <- load_fishmorph_reference(source = "landmark")  # re-digitized subset
#' }
#' @export
load_fishmorph_reference <- function(file = NULL, sheet = NULL,
                                     source = NULL, quiet = FALSE) {
  source <- .fm_resolve_source(source)
  # Symbolic shortcuts: they spare the caller a system.file() call for a file
  # the package bundles anyway. An explicit path is neither campaign, and is
  # labelled as such rather than inheriting the active one -- a table read from
  # disk must not come back claiming to be the bundled segment table.
  bundled <- is.null(file) || identical(file, "full")
  if (bundled) file <- .fm_source_file(source)
  else if (identical(file, "sample")) {
    file <- "fishmorph_reference_sample.csv"
    source <- "segment"
    bundled <- TRUE
  }

  bundled_name <- file
  if (!file.exists(file) && !grepl("[/\\\\]", file)) {
    from_pkg <- system.file("extdata", file, package = "Rfishmorph")
    if (nzchar(from_pkg)) file <- from_pkg
  }
  if (!nzchar(file) || !file.exists(file)) {
    hint <- if (identical(bundled_name, .fm_source_file("landmark")))
      paste0("\n  The landmark table is a snapshot of an ongoing re-measurement:",
             "\n  regenerate it with build_fishmorph_landmark_table().")
    else ""
    stop("Reference table not found: ", file,
         "\n  Pass an explicit path, or reinstall 'Rfishmorph'.", hint,
         call. = FALSE)
  }
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
  # Provenance is stated out loud rather than inferred from the object: the two
  # campaigns have identical columns but different species pools, so a silent
  # load is exactly how one would end up comparing incomparable spaces.
  if (!quiet) {
    lbl <- if (!bundled) paste0("user file (", basename(file), ")")
    else switch(source, landmark = "landmark (re-digitized)",
                "segment (published)")
    extra <- if ("n_imputed" %in% names(df))
      sprintf(", %d with >=1 imputed ratio", sum(df$n_imputed > 0, na.rm = TRUE))
    else ""
    message(sprintf("FISHMORPH reference: %s -- %d species%s.",
                    lbl, nrow(df), extra))
  }
  attr(df, "fishmorph_source") <- if (bundled) source else NA_character_
  df
}

# ---- source resolution ------------------------------------------------------

.FM_SOURCE_FILES <- c(segment  = "fishmorph_data.csv",
                      landmark = "fishmorph_data_landmarks.csv")

.fm_source_file <- function(source) unname(.FM_SOURCE_FILES[[source]])

# Resolve NULL to the session default. Kept in one place so that Rfishmorph,
# intraitR and FishInTrait all answer the same question the same way.
.fm_resolve_source <- function(source = NULL) {
  if (is.null(source)) source <- getOption("fishmorph.source", "segment")
  source <- match.arg(as.character(source)[1], names(.FM_SOURCE_FILES))
  source
}

#' Choose the FISHMORPH measurement campaign for the session
#'
#' Sets `options(fishmorph.source = )`, the default consulted by
#' [load_fishmorph_reference()], [fishmorph_space_data()],
#' [project_fishmorph()] and the shiny explorers whenever their own `source`
#' argument is left at `NULL`. Every function keeps an explicit `source`
#' argument, which always wins: use the option to switch a whole script, the
#' argument when a single call must be pinned regardless of the session state.
#'
#' @param source `"segment"` (published segment measurements, the package
#'   default) or `"landmark"` (traits recomputed from the landmark
#'   re-digitization).
#' @return Invisibly, the previous value.
#' @seealso [load_fishmorph_reference()], [get_fishmorph_source()],
#'   [build_fishmorph_landmark_table()]
#' @examples
#' old <- set_fishmorph_source("segment")
#' get_fishmorph_source()
#' set_fishmorph_source(old)
#' @export
set_fishmorph_source <- function(source = c("segment", "landmark")) {
  source <- match.arg(source)
  old <- getOption("fishmorph.source", "segment")
  options(fishmorph.source = source)
  invisible(old)
}

#' @rdname set_fishmorph_source
#' @export
get_fishmorph_source <- function() .fm_resolve_source(NULL)

#' Build the FISHMORPH functional trait space
#'
#' Fits a principal component analysis on a set of (optionally log-transformed
#' and scaled) morphological traits. This is the ordination used to describe the
#' FISHMORPH functional space; once fitted it is frozen and reused to project new
#' specimens with [project_fishmorph()], so every projection lives in the same
#' coordinate system.
#'
#' @param data A data frame of traits, or a `fishmorph_landmarks` object (its
#'   ratios are computed first). `NULL` (default) fits the space on the bundled
#'   reference of the campaign named by `source`.
#' @param source Which bundled table to fit when `data` is `NULL`: `"segment"`
#'   or `"landmark"`. `NULL` (default) follows
#'   `getOption("fishmorph.source", "segment")`, see [set_fishmorph_source()].
#'   The PCA is refitted on the chosen table, so the two campaigns define two
#'   distinct coordinate systems: axis order and sign may differ, and scores are
#'   not comparable across campaigns without a Procrustes alignment.
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
fishmorph_trait_space <- function(data = NULL, source = NULL,
                                  traits = fishmorph_ratio_names(),
                                  groups = NULL, log = FALSE, scale = TRUE,
                                  na_action = c("omit", "fail", "impute_mean",
                                                "impute_group_mean", "missforest",
                                                "missforest_phylo"),
                                  missforest_ntree = 100,
                                  missforest_maxiter = 10, tree = NULL,
                                  missforest_phylo_k = 10, phylo_axes = NULL,
                                  species = NULL) {
  na_action <- match.arg(na_action)
  # No data: fit the space on the bundled table of the active campaign. The
  # ordination is always refitted on whatever is supplied, so a "landmark"
  # space is a genuinely new PCA, not a reprojection of the segment one.
  if (is.null(data)) data <- load_fishmorph_reference(source = source)
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
