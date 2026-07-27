# =============================================================================
# impute.R -- missing-data imputation and phylogeny helpers.
#
# Faithful port of the intraitR routines so the two packages stay CONCORDANT:
#   load_fishmorph_phylogeny(), phylo_pcoa(), impute_landmarks(),
#   and the internal .apply_na_action() / .phylo_axes_for_species().
# The only intentional differences are the package name (bundled phylogeny path)
# and the S3 class of phylo_pcoa()'s result ("fishmorph_phylopcoa" instead of
# "intrait_phylopcoa"), so that print methods do not clash when both packages
# are loaded together. The numerical behaviour is identical.
# =============================================================================

# canonical species-name matching (spaces / underscores / dots interchangeable)
.canon_species_name <- function(x) gsub("[ ._]+", "_", trimws(as.character(x)))

#' Bundled global fish phylogeny
#'
#' Loads the FISHMORPH phylogenetic tree bundled with the package, for use with
#' [phylo_pcoa()] and the `"missforest_phylo"` imputation option of
#' [fishmorph_trait_space()] and [impute_landmarks()]. This is the same
#' `FishMORPH_Phylogeny.rds` object used by intraitR.
#'
#' @return An object of class `"phylo"` (tip labels formatted `"Genus.species"`).
#' @seealso [phylo_pcoa()], [impute_landmarks()], [fishmorph_trait_space()]
#' @export
load_fishmorph_phylogeny <- function() {
  path <- system.file("extdata", "Phylogeny", "FishMORPH_Phylogeny.rds",
                      package = "Rfishmorph")
  if (!nzchar(path))
    stop("Could not find 'FishMORPH_Phylogeny.rds' under inst/extdata/Phylogeny/.",
         call. = FALSE)
  tree <- readRDS(path)
  if (!inherits(tree, "phylo"))
    stop("The bundled phylogeny file did not contain a \"phylo\" object.",
         call. = FALSE)
  tree
}

# Session cache: the table of phylogenetic axes is read once per R session.
# An empty parent environment, so that nothing else is captured.
.fm_cache <- new.env(parent = emptyenv())

#' Precomputed phylogenetic PCoA axes for the FISHMORPH species pool
#'
#' Loads `pcoaPhylogenyFish.rds`, the **precomputed** principal-coordinate axes
#' of the global fish phylogeny for the 8,970 FISHMORPH species (10 axes,
#' `Eigen.1` to `Eigen.10`, ordered by decreasing eigenvalue).
#'
#' @section Why precomputed:
#' The alternative, [phylo_pcoa()], eigendecomposes the patristic distance matrix
#' of the tree. That matrix is *n by n*: for 8,970 species it is roughly 80
#' million entries, and the decomposition is cubic in *n*. Recomputing it on
#' every imputation is both slow and, more importantly, **not reproducible
#' across calls** -- the axes depend on which subset of species happens to be
#' present in the data at hand, so two analyses on different subsets end up in
#' different phylogenetic coordinate systems and are not comparable.
#'
#' Reading a fixed table instead makes the axes a *property of the phylogeny*
#' rather than of the current dataset. Every imputation, whatever the species
#' subset, then lives in one and the same phylogenetic space.
#'
#' @section File format:
#' The bundled table is a compressed `.rds` (about 540 kB against 1.8 MB for the
#' original whitespace-separated text). Both formats are accepted and dispatched
#' on the extension, so `file` can point at either. To regenerate the `.rds` from
#' the text source:
#' ```r
#' txt <- read.table("pcoaPhylogenyFish.txt", header = TRUE)
#' ax  <- data.frame(species = gsub("[ ._]+", "_", rownames(txt)), txt,
#'                   row.names = NULL)
#' names(ax)[-1] <- paste0("phylo_", seq_len(ncol(txt)))
#' saveRDS(ax, "pcoaPhylogenyFish.rds", compress = "xz")
#' ```
#'
#' @param file Optional path to an alternative axis table: either an `.rds`
#'   holding a data frame, or a whitespace-separated text file with species as
#'   row names and one column per axis. `NULL` uses the bundled file.
#' @param k Number of axes to return (first `k` columns). `NULL` returns all.
#' @param refresh Force a re-read instead of using the session cache.
#' @return A data frame with a `species` column (`Genus_species`) followed by the
#'   axis columns, named `phylo_1`, `phylo_2`, ...
#' @seealso [phylo_pcoa()] to recompute axes from a tree,
#'   [load_fishmorph_phylogeny()] for the tree itself.
#' @examples
#' ax <- load_fishmorph_phylo_axes(k = 3)
#' dim(ax)
#' head(ax, 3)
#' @export
load_fishmorph_phylo_axes <- function(file = NULL, k = NULL, refresh = FALSE) {
  key <- if (is.null(file)) "__bundled__" else normalizePath(file, mustWork = FALSE)
  if (!isTRUE(refresh) && !is.null(.fm_cache[[key]])) {
    ax <- .fm_cache[[key]]
  } else {
    if (is.null(file)) {
      # .rds first (compressed), .txt next: the second makes it possible to
      # start again from the raw source if the binary is unreadable on a given
      for (nm in c("pcoaPhylogenyFish.rds", "pcoaPhylogenyFish.txt")) {
        cand <- system.file("extdata", "Phylogeny", nm, package = "Rfishmorph")
        if (nzchar(cand) && file.exists(cand)) { file <- cand; break }
      }
      if (is.null(file))
        stop("Could not find 'pcoaPhylogenyFish.rds' (or .txt) under ",
             "inst/extdata/Phylogeny/. Reinstall 'Rfishmorph', or recompute the ",
             "axes with phylo_pcoa(load_fishmorph_phylogeny()).", call. = FALSE)
    }
    if (!file.exists(file))
      stop("Phylogenetic axis table not found: ", file, call. = FALSE)

    if (identical(tolower(tools::file_ext(file)), "rds")) {
      raw <- readRDS(file)
      if (!is.data.frame(raw))
        stop("The .rds file did not contain a data frame: ", file, call. = FALSE)
    } else {
      # platform. A header with one column FEWER than the data rows -> read.table
      # automatically promotes the 1st column to row names (the format written
      # by write.table()). `row.names` is therefore not forced, which would
      # break a file written with a named species column.
      raw <- utils::read.table(file, header = TRUE, check.names = FALSE,
                               stringsAsFactors = FALSE)
    }
    num <- vapply(raw, is.numeric, logical(1))
    if (!any(num))
      stop("No numeric axis column found in ", file, call. = FALSE)
    sp <- if (all(num)) rownames(raw) else as.character(raw[[which(!num)[1]]])
    ax <- data.frame(species = .canon_species_name(sp),
                     raw[, num, drop = FALSE], stringsAsFactors = FALSE)
    names(ax)[-1] <- paste0("phylo_", seq_len(sum(num)))
    ax <- ax[!duplicated(ax$species), , drop = FALSE]
    rownames(ax) <- NULL
    .fm_cache[[key]] <- ax
  }
  if (!is.null(k)) {
    k <- min(as.integer(k), ncol(ax) - 1L)
    ax <- ax[, c(1L, seq_len(k) + 1L), drop = FALSE]
  }
  ax
}

#' Phylogenetic Principal Coordinates Analysis
#'
#' Derives quantitative phylogenetic axes from a tree by PCoA of its patristic
#' (cophenetic) distances among species. Faithful port of `intraitR::phylo_pcoa()`.
#'
#' For the FISHMORPH species pool you normally want
#' [load_fishmorph_phylo_axes()] instead: it returns the same kind of axes,
#' precomputed once over all 8,970 species, so that successive analyses share a
#' single phylogenetic coordinate system. Use `phylo_pcoa()` when you work with a
#' different tree, or want a correction (`"cailliez"`, `"lingoes"`).
#'
#' @param tree An object of class `"phylo"` (e.g. [load_fishmorph_phylogeny()]).
#' @param species Optional character vector of species to retain. Names are
#'   matched after collapsing spaces/underscores/dots.
#' @param k Number of phylogenetic axes to keep (default: all positive-eigenvalue
#'   axes).
#' @param correction `"none"` (default), `"cailliez"` or `"lingoes"` (see
#'   [ape::pcoa()]).
#' @param ultrametric Coerce the tree to ultrametric before computing distances
#'   (default `TRUE`; uses \pkg{phytools}).
#' @param ultrametric_method `"nnls"` (default) or `"extend"`.
#' @return An object of class `"fishmorph_phylopcoa"` with `traits` (species +
#'   `PCoA1..PCoAk`), `var_explained`, `k`, `correction`, `tree` and
#'   `dropped_species`.
#' @seealso [load_fishmorph_phylogeny()], [fishmorph_trait_space()]
#' @export
phylo_pcoa <- function(tree, species = NULL, k = NULL,
                       correction = c("none", "cailliez", "lingoes"),
                       ultrametric = TRUE,
                       ultrametric_method = c("nnls", "extend")) {
  correction <- match.arg(correction)
  ultrametric_method <- match.arg(ultrametric_method)
  if (!inherits(tree, "phylo"))
    stop("`tree` must be an object of class \"phylo\".", call. = FALSE)
  if (!requireNamespace("ape", quietly = TRUE))
    stop("phylo_pcoa() requires the \"ape\" package.", call. = FALSE)

  tip_labels <- .canon_species_name(tree$tip.label)
  if (is.null(species)) species <- tree$tip.label
  species <- .canon_species_name(species)
  species_u <- unique(species)

  missing_sp <- setdiff(species_u, tip_labels)
  if (length(missing_sp) > 0)
    warning(sprintf("%d species not found in `tree$tip.label` and dropped: %s%s",
                    length(missing_sp),
                    paste(utils::head(missing_sp, 10), collapse = ", "),
                    if (length(missing_sp) > 10) ", ..." else ""), call. = FALSE)
  keep <- intersect(species_u, tip_labels)
  if (length(keep) < 3)
    stop("At least 3 species with a matching tip label are required; found ",
         length(keep), ".", call. = FALSE)

  pruned <- tree
  pruned$tip.label <- tip_labels
  pruned <- ape::drop.tip(pruned, setdiff(tip_labels, keep))

  if (isTRUE(ultrametric) && !ape::is.ultrametric(pruned)) {
    if (!requireNamespace("phytools", quietly = TRUE))
      stop("phylo_pcoa() requires \"phytools\" to coerce `tree` to ultrametric, ",
           "or set ultrametric = FALSE.", call. = FALSE)
    pruned <- phytools::force.ultrametric(pruned, method = ultrametric_method)
    message(sprintf(paste0("phylo_pcoa(): `tree` was not exactly ultrametric; ",
                           "coerced using phytools::force.ultrametric(method = \"%s\")."),
                    ultrametric_method))
  }

  phylo_dist <- stats::as.dist(ape::cophenetic.phylo(pruned))
  pco <- ape::pcoa(phylo_dist, correction = correction)

  vectors <- pco$vectors
  eig_col <- "Relative_eig"
  if (correction != "none" && !is.null(pco$vectors.cor) &&
      ncol(pco$vectors.cor) > 0) {
    vectors <- pco$vectors.cor
    eig_col <- "Rel_corr_eig"
  }
  n_axes_avail <- ncol(vectors)
  if (n_axes_avail == 0)
    stop("No positive-eigenvalue axis remains after PCoA; try correction = ",
         "\"cailliez\" or \"lingoes\".", call. = FALSE)
  if (is.null(k)) k <- n_axes_avail
  if (k > n_axes_avail)
    stop(sprintf("`k` = %d requests more axes than the %d available.", k,
                 n_axes_avail), call. = FALSE)

  vectors <- vectors[, seq_len(k), drop = FALSE]
  colnames(vectors) <- paste0("PCoA", seq_len(k))
  var_explained <- unname(pco$values[[eig_col]][seq_len(k)]) * 100
  traits <- data.frame(species = rownames(vectors), vectors,
                       row.names = NULL, stringsAsFactors = FALSE,
                       check.names = FALSE)
  structure(list(traits = traits, var_explained = var_explained, k = k,
                 correction = correction, tree = pruned,
                 dropped_species = missing_sp),
            class = "fishmorph_phylopcoa")
}

#' @export
print.fishmorph_phylopcoa <- function(x, ...) {
  cat("<fishmorph_phylopcoa>\n")
  cat(sprintf("  %d species, %d PCoA axis/axes (correction = \"%s\")\n",
              nrow(x$traits), x$k, x$correction))
  cat(sprintf("  Variance explained: %s\n",
              paste(sprintf("%s = %.1f%%", colnames(x$traits)[-1],
                            x$var_explained), collapse = ", ")))
  if (length(x$dropped_species) > 0)
    cat(sprintf("  %d requested species not found in the tree\n",
                length(x$dropped_species)))
  invisible(x)
}

# Phylogenetic axes broadcast over a `groups` vector.
#
# Default source: the table PRECOMPUTED on the 8,970 species
# (load_fishmorph_phylo_axes()). It is preferred to computing from the tree for
# two reasons, of which the second matters most:
#   1. cost -- the eigendecomposition of an 8,970 x 8,970 patristic matrix is
#      cubic, and would be redone at every call;
#   2. COMPARABILITY -- axes recomputed on the subset of species present in the
#      data define a DIFFERENT frame at every analysis. Two imputations on two
#      subsets would not live in the same phylogenetic space. The fixed table
#      makes the axes a property of the phylogeny, not of the data set at hand.
#
#
# Un `tree` explicitement fourni signifie que l'appelant veut SON arbre : on
# recomputed through phylo_pcoa(), as before. The table also serves as the
# reverse fallback when the tree cannot be found.
.phylo_axes_for_species <- function(species, tree = NULL, k_phylo = 10,
                                    axes = NULL) {
  if (is.null(species))
    return(list(axes = NULL, reason = "no `species` supplied", n_matched = 0L,
                k_used = 0L, source = NA_character_))
  groups <- species                       # the historical name in the body below
  canon_groups <- .canon_species_name(as.character(species))
  n_sp <- length(unique(canon_groups[!is.na(canon_groups)]))

  broadcast <- function(ax, src) {
    k_use <- min(k_phylo, ncol(ax) - 1L)
    cols <- names(ax)[seq_len(k_use) + 1L]
    m <- match(canon_groups, ax$species)
    if (all(is.na(m)))
      return(list(axes = NULL, reason = paste0(
        "none of the ", n_sp, " species could be matched to the ", src),
        n_matched = 0L, k_used = 0L, source = src))
    out <- ax[m, cols, drop = FALSE]
    names(out) <- paste0("phylo_", seq_len(k_use))
    rownames(out) <- NULL
    list(axes = out, reason = NULL,
         n_matched = length(unique(canon_groups[!is.na(m)])),
         k_used = k_use, source = src)
  }

  # 1. table fournie par l'appelant
  if (!is.null(axes)) {
    if (!is.data.frame(axes) || !"species" %in% names(axes))
      return(list(axes = NULL, reason = "`phylo_axes` must be a data frame with a `species` column",
                  n_matched = 0L, k_used = 0L, source = "user table"))
    axes$species <- .canon_species_name(axes$species)
    return(broadcast(axes, "supplied axis table"))
  }

  # 2. arbre explicite -> recalcul (comportement historique)
  if (!is.null(tree)) {
    sp_pool <- unique(as.character(groups)[!is.na(groups)])
    pp <- tryCatch(phylo_pcoa(tree, species = sp_pool, k = NULL,
                              ultrametric = FALSE), error = function(e) e)
    if (inherits(pp, "error"))
      return(list(axes = NULL, reason = conditionMessage(pp), n_matched = 0L,
                  k_used = 0L, source = "supplied tree"))
    k_use <- min(k_phylo, pp$k)
    ax <- pp$traits[, c("species", paste0("PCoA", seq_len(k_use))), drop = FALSE]
    ax$species <- .canon_species_name(ax$species)
    return(broadcast(ax, "supplied tree"))
  }

  # 3. default: the precomputed table; fallback on the bundled tree
  ax <- tryCatch(load_fishmorph_phylo_axes(), error = function(e) e)
  if (!inherits(ax, "error")) {
    res <- broadcast(ax, "precomputed axis table")
    if (!is.null(res$axes)) return(res)
    first_reason <- res$reason
  } else {
    first_reason <- conditionMessage(ax)
  }
  tr <- tryCatch(load_fishmorph_phylogeny(), error = function(e) e)
  if (inherits(tr, "error"))
    return(list(axes = NULL, k_used = 0L, n_matched = 0L,
                source = "none", reason = paste0(
                  first_reason, "; and the bundled phylogeny could not be ",
                  "loaded either: ", conditionMessage(tr))))
  Recall(species, tree = tr, k_phylo = k_phylo)
}

# trait-matrix NA handling (port of intraitR:::.apply_na_action). Returns the
# (possibly imputed / row-subset) matrix and a `keep` mask.
# `groups`  : predicteur CATEGORIEL optionnel (et cle de "impute_group_mean").
# `species`: the species identifier per ROW, used only to look the axes up.
#             axes phylogenetiques. Les deux etaient confondus : "missforest_phylo"
#             then required a `groups`, whereas the phylogeny needs no grouping
#             factor feeding the forest, only to know which species each row
#             corresponds to.
.apply_na_action <- function(X, groups, na_action, missforest_ntree = 100,
                             missforest_maxiter = 10, context = "traits",
                             tree = NULL, missforest_phylo_k = 10,
                             phylo_axes = NULL, species = NULL) {
  n <- nrow(X)
  if (!anyNA(X) || na_action == "keep") return(list(X = X, keep = rep(TRUE, n)))
  if (na_action == "fail")
    stop("`", context, "` contains missing values; set `na_action` to \"omit\", ",
         "\"impute_mean\", \"impute_group_mean\", \"missforest\", or ",
         "\"missforest_phylo\".", call. = FALSE)
  if (na_action == "impute_group_mean" && is.null(groups))
    stop("`na_action = \"impute_group_mean\"` requires `groups`.", call. = FALSE)

  keep <- rep(TRUE, n)
  if (na_action == "omit") {
    keep <- stats::complete.cases(X)
    message(sprintf("na_action = \"omit\": removing %d row(s) out of %d with missing values.",
                    sum(!keep), n))
    X <- X[keep, , drop = FALSE]
  } else if (na_action == "impute_mean") {
    n_na <- sum(is.na(X))
    for (j in seq_len(ncol(X))) {
      col_na <- is.na(X[, j]); if (any(col_na)) X[col_na, j] <- mean(X[, j], na.rm = TRUE)
    }
    message(sprintf("na_action = \"impute_mean\": imputed %d missing value(s) using column means.", n_na))
  } else if (na_action == "impute_group_mean") {
    X_before <- is.na(X)
    group_na <- is.na(groups)
    if (any(group_na))
      warning(sum(group_na), " row(s) have a missing/unresolved `groups` value and ",
              "cannot be imputed by within-group mean; left as NA.", call. = FALSE)
    for (j in seq_len(ncol(X))) {
      col <- X[, j]; col_na <- is.na(col); if (!any(col_na)) next
      for (g in levels(groups)) {
        in_group <- !group_na & groups == g
        idx <- which(in_group & col_na); if (length(idx) == 0) next
        g_mean <- mean(col[in_group], na.rm = TRUE)
        if (is.nan(g_mean)) {
          warning("Group \"", g, "\" has no non-missing values for at least one ",
                  context, " column; falling back to the overall column mean.",
                  call. = FALSE)
          g_mean <- mean(col, na.rm = TRUE)
        }
        col[idx] <- g_mean
      }
      X[, j] <- col
    }
    message(sprintf("na_action = \"impute_group_mean\": imputed %d missing value(s) using within-group means.",
                    sum(X_before & !is.na(X))))
  } else if (na_action %in% c("missforest", "missforest_phylo")) {
    if (!requireNamespace("missForest", quietly = TRUE))
      stop("na_action = \"", na_action, "\" requires the \"missForest\" package.",
           call. = FALSE)
    n_na <- sum(is.na(X))
    df_for_rf <- as.data.frame(X)
    grp_note <- ""
    if (!is.null(groups)) {
      # randomForest refuse un facteur de plus de 53 modalites. Un `groups` egal
      # to the species (thousands of levels) would therefore make missForest
      # fail -- one more reason NOT to pour the species into it automatically.
      g <- factor(groups)
      if (nlevels(g) > 53L) {
        warning("`groups` has ", nlevels(g), " levels; randomForest cannot use ",
                "more than 53 categories, so it is dropped from the predictors. ",
                "Species identity reaches the model through the phylogenetic ",
                "axes instead.", call. = FALSE)
      } else {
        df_for_rf$.group <- g
        grp_note <- ", using `groups` as an auxiliary predictor"
      }
    }
    phylo_note <- ""
    if (na_action == "missforest_phylo") {
      pax <- .phylo_axes_for_species(species, tree = tree,
                                     k_phylo = missforest_phylo_k,
                                     axes = phylo_axes)
      if (is.null(pax$axes)) {
        warning("na_action = \"missforest_phylo\": phylogenetic axes could not be used (",
                pax$reason, "); falling back to plain \"missforest\".", call. = FALSE)
      } else {
        df_for_rf <- cbind(df_for_rf, pax$axes)
        # the SOURCE of the axes is traced: two imputations are only comparable
        # if they rest on the same phylogenetic frame.
        phylo_note <- sprintf(
          ", augmented with %d phylogenetic PCoA axis/axes from the %s (%d species matched)",
          pax$k_used, pax$source, pax$n_matched)
      }
    }
    imp <- missForest::missForest(df_for_rf, ntree = missforest_ntree,
                                  maxiter = missforest_maxiter, verbose = FALSE)
    X <- as.matrix(imp$ximp[, colnames(X), drop = FALSE]); storage.mode(X) <- "double"
    nrmse <- if ("NRMSE" %in% names(imp$OOBerror)) imp$OOBerror[["NRMSE"]] else NA_real_
    message(sprintf("na_action = \"%s\": imputed %d missing value(s) using random-forest imputation (missForest)%s%s%s.",
                    na_action, n_na, grp_note, phylo_note,
                    if (!is.na(nrmse)) sprintf(" (out-of-bag NRMSE = %.3f)", nrmse) else ""))
  }
  list(X = X, keep = keep)
}

#' Impute missing values in a segment / ratio table
#'
#' Fills `NA`s in the chosen numeric columns (by default the 11 FISHMORPH
#' segments) using the same imputation methods as intraitR, so results stay
#' concordant: column mean, within-group mean, random forest
#' (`missForest`), or random forest augmented with phylogenetic PCoA axes
#' (`"missforest_phylo"`, see [phylo_pcoa()]). Use it instead of dropping
#' incomplete rows (e.g. your `complete.cases()` filter).
#'
#' @param data A data frame (e.g. the `Global_segments` sheet) with the columns
#'   named in `cols`.
#' @param cols Columns to impute. Default the 11 segments
#'   ([fishmorph_segment_names()]); pass [fishmorph_ratio_names()] to impute the
#'   9 ratios instead.
#' @param method `"missforest_phylo"` (default), `"missforest"`,
#'   `"impute_group_mean"` or `"impute_mean"`.
#' @param groups Optional grouping vector (one per row), used by
#'   `"impute_group_mean"` (required) and `"missforest"`/`"missforest_phylo"` (as
#'   an auxiliary predictor; species labels for the phylogeny). If `NULL`, a
#'   `Genus.species`/`species`/`Species` column of `data` is used when present.
#' @param tree,missforest_phylo_k,missforest_ntree,missforest_maxiter Passed to
#'   the missForest methods (see [impute_landmarks()]).
#' @param phylo_axes Used by `"missforest_phylo"`. `NULL` (default) uses the
#'   **precomputed** axes of [load_fishmorph_phylo_axes()], so that every call
#'   shares one and the same phylogenetic coordinate system. Supply a data frame
#'   (a `species` column plus one column per axis) to use your own.
#' @param species Species identifier for **each row**, used only to look up the
#'   phylogenetic axes of `"missforest_phylo"`. `NULL` (default) auto-detects a
#'   `Genus.species` / `Species` / `species` column. This is deliberately
#'   separate from `groups`: the phylogeny needs to know which species a row
#'   belongs to, not a categorical predictor for the forest.
#' @return `data` with the `cols` imputed. `attr(, "n_imputed")` gives the number
#'   of filled cells.
#' @seealso [fishmorph_trait_space()], [impute_landmarks()]
#' @examples
#' \donttest{
#' # impute the 11 segments of a Global_segments table, by species + phylogeny
#' seg_imp <- impute_traits(pub_seg, groups = pub_seg$Genus.species)
#' }
#' @export
impute_traits <- function(data, cols = fishmorph_segment_names(),
                          method = c("missforest_phylo", "missforest",
                                     "impute_group_mean", "impute_mean"),
                          groups = NULL, species = NULL,
                          tree = NULL, missforest_phylo_k = 10,
                          phylo_axes = NULL,
                          missforest_ntree = 100, missforest_maxiter = 10) {
  method <- match.arg(method)
  data <- as.data.frame(data)
  cols <- intersect(cols, names(data))
  if (!length(cols)) stop("None of `cols` found in `data`.", call. = FALSE)
  # `species` is detected automatically; `groups` is NOT. Filling both with the
  # species column amounted to handing randomForest a factor of several thousand
  # levels, and it refuses more than 53.
  if (is.null(species)) {
    sc <- intersect(c("Genus.species", "species", "Species"), names(data))[1]
    if (!is.na(sc)) species <- data[[sc]]
  }
  # for the group means, the species IS the group: an explicit fallback.
  if (is.null(groups) && method == "impute_group_mean") groups <- species
  grp_f <- if (!is.null(groups)) factor(groups) else NULL
  X <- as.matrix(data[cols]); storage.mode(X) <- "double"
  n_before <- sum(is.na(X))
  res <- .apply_na_action(X, grp_f, method,
                          missforest_ntree = missforest_ntree,
                          missforest_maxiter = missforest_maxiter,
                          context = "traits", tree = tree,
                          missforest_phylo_k = missforest_phylo_k,
                          phylo_axes = phylo_axes, species = species)
  data[cols] <- res$X
  attr(data, "n_imputed") <- n_before - sum(is.na(as.matrix(data[cols])))
  data
}

# coords getter accepting fishmorph_landmarks / intrait_landmarks / raw array
.imp_get_coords <- function(x) {
  if (is_fishmorph_landmarks(x)) A <- x$coords
  else if (is.array(x) && length(dim(x)) == 3) A <- x
  else stop("`landmarks` must be a fishmorph_landmarks object or a raw p x k x n array.",
            call. = FALSE)
  if (is.null(dimnames(A)) || is.null(dimnames(A)[[3]]))
    dimnames(A)[[3]] <- paste0("specimen_", seq_len(dim(A)[3]))
  A
}

#' Impute missing (NA) landmark coordinates
#'
#' Faithful port of `intraitR::impute_landmarks()`. Estimates missing 2D
#' coordinates directly in the landmark array. `"tps"`/`"regression"` use
#' [geomorph::estimate.missing()] (thin-plate spline / multivariate regression on
#' the geometric covariation among landmarks); `"impute_mean"`,
#' `"impute_group_mean"`, `"missforest"` and `"missforest_phylo"` treat each
#' coordinate as a numeric variable and impute it statistically, mirroring
#' [fishmorph_trait_space()]'s `na_action`. `"missforest_phylo"` augments the
#' random-forest predictors with phylogenetic PCoA axes (see [phylo_pcoa()]).
#'
#' Only anatomical landmarks 1-19 are imputed; the scale bar (20-21) and the
#' optional curvature point (22) are left untouched. The returned `coords` carry
#' an `"imputed"` attribute (a p x n logical matrix) marking estimated points.
#'
#' @param landmarks A `fishmorph_landmarks` (or `intrait_landmarks`) object, or a
#'   raw p x k x n array, with at least one `NA` coordinate.
#' @param method One of `"tps"` (default), `"regression"`, `"impute_mean"`,
#'   `"impute_group_mean"`, `"missforest"`, `"missforest_phylo"`.
#' @param groups Optional factor/character, one value per specimen (species
#'   labels). Auto-detected from `metadata$species` when available. Required by
#'   `"impute_group_mean"`, and by `"missforest_phylo"` for phylogenetic matching.
#' @param missforest_ntree,missforest_maxiter Passed to
#'   [missForest::missForest()] for the missForest methods.
#' @param tree Used by `"missforest_phylo"`: a `"phylo"` object, or `NULL`
#'   (default) to use [load_fishmorph_phylogeny()].
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
#' @return An object of the same class as `landmarks`, with landmarks 1-19
#'   completed.
#' @references Stekhoven & Buhlmann (2012) *Bioinformatics* 28:112-118.
#' @seealso [phylo_pcoa()], [load_fishmorph_phylogeny()], [fishmorph_trait_space()]
#' @export
impute_landmarks <- function(landmarks,
                             method = c("tps", "regression", "impute_mean",
                                        "impute_group_mean", "missforest",
                                        "missforest_phylo"),
                             groups = NULL, species = NULL,
                             missforest_ntree = 100, missforest_maxiter = 10,
                             tree = NULL, missforest_phylo_k = 10,
                             phylo_axes = NULL) {
  method <- match.arg(method)
  A <- .imp_get_coords(landmarks)
  p <- dim(A)[1]; k <- dim(A)[2]; n <- dim(A)[3]
  if (k != 2)
    stop("impute_landmarks() requires two-dimensional landmark configurations.",
         call. = FALSE)
  if (p < 21)
    stop("`landmarks` must contain at least 21 landmarks (FISHMORPH scheme); found ",
         p, ".", call. = FALSE)

  # `species`: the `species` column of the metadata, otherwise the SPECIMEN
  # NAMES (3rd dimension of the coordinate array), which are already species
  # modes reconstruct/correct. Detecte independamment de `groups`.
  if (is.null(species)) {
    if (is_fishmorph_landmarks(landmarks) && !is.null(landmarks$metadata) &&
        "species" %in% names(landmarks$metadata))
      species <- landmarks$metadata$species
    else if (!is.null(dimnames(A)[[3]]))
      species <- dimnames(A)[[3]]
  }
  if (!is.null(species) && length(species) != n)
    stop("`species` must have one entry per specimen (", n, "); got ",
         length(species), ".", call. = FALSE)
  if (is.null(groups) && method == "impute_group_mean") groups <- species
  if (method == "impute_group_mean" && is.null(groups))
    stop("method = \"impute_group_mean\" requires `groups` or `species`.",
         call. = FALSE)
  if (!is.null(groups)) {
    if (length(groups) != n)
      stop("`groups` must have one entry per specimen (", n, "); got ",
           length(groups), ".", call. = FALSE)
    groups <- factor(groups)
  }

  scale_na <- apply(A[20:21, , , drop = FALSE], 3, anyNA)
  if (any(scale_na))
    warning(sum(scale_na), " specimen(s) have a missing scale bar landmark (20/21); ",
            "these cannot be estimated and are left as NA.", call. = FALSE)

  shape_idx <- seq_len(min(19, p)); n_shape <- length(shape_idx)
  shape_A <- A[shape_idx, , , drop = FALSE]
  imputed_shape_mask <- apply(is.na(shape_A), c(1, 3), any)
  n_missing_pts <- sum(imputed_shape_mask)

  imputed_full <- matrix(FALSE, nrow = p, ncol = n,
                         dimnames = list(NULL, dimnames(A)[[3]]))
  prior_imputed <- attr(A, "imputed")
  if (!is.null(prior_imputed) && all(dim(prior_imputed) == dim(imputed_full)))
    imputed_full <- prior_imputed

  if (n_missing_pts == 0) {
    message("impute_landmarks(): no missing anatomical landmark (1-19) found; nothing to impute.")
    return(landmarks)
  }

  if (method %in% c("tps", "regression")) {
    if (!requireNamespace("geomorph", quietly = TRUE))
      stop("method = \"", method, "\" requires the \"geomorph\" package.",
           call. = FALSE)
    geomorph_method <- if (method == "tps") "TPS" else "Reg"
    imputed_shape <- tryCatch(
      geomorph::estimate.missing(shape_A, method = geomorph_method),
      error = function(e) stop(
        "geomorph::estimate.missing() failed (method = \"", geomorph_method,
        "\"): ", conditionMessage(e),
        ". Too few complete specimens; try na_action = \"omit\" or a statistical method.",
        call. = FALSE))
    A[shape_idx, , ] <- imputed_shape
    message(sprintf("impute_landmarks(): estimated %d missing anatomical landmark coordinate(s) using method = \"%s\".",
                    n_missing_pts, method))
  } else {
    dim_labels <- c("x", "y")
    n_coord_cols <- n_shape * k
    M <- matrix(NA_real_, nrow = n, ncol = n_coord_cols)
    col_names <- character(n_coord_cols); col_i <- 0L
    for (pt in seq_len(n_shape)) for (dd in seq_len(k)) {
      col_i <- col_i + 1L
      M[, col_i] <- shape_A[pt, dd, ]
      col_names[col_i] <- paste0("lm", shape_idx[pt], "_", dim_labels[dd])
    }
    colnames(M) <- col_names

    res <- .apply_na_action(M, groups, na_action = method,
                            missforest_ntree = missforest_ntree,
                            missforest_maxiter = missforest_maxiter,
                            context = "landmark coordinates", tree = tree,
                            missforest_phylo_k = missforest_phylo_k,
                            phylo_axes = phylo_axes, species = species)
    M <- res$X
    col_i <- 0L
    for (pt in seq_len(n_shape)) for (dd in seq_len(k)) {
      col_i <- col_i + 1L
      shape_A[pt, dd, ] <- M[, col_i]
    }
    A[shape_idx, , ] <- shape_A
  }

  imputed_full[shape_idx, ] <- imputed_full[shape_idx, ] | imputed_shape_mask
  attr(A, "imputed") <- imputed_full
  if (is_fishmorph_landmarks(landmarks)) { landmarks$coords <- A; return(landmarks) }
  A
}
