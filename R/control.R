# =============================================================================
# control.R -- quality-control tools to verify FISHMORPH measurements, in
# particular the agreement between published SEGMENT measurements and the values
# recomputed from digitized LANDMARKS. Ported from
# compare_segments_ratios_fishmorph.R and diagnostic_infinite_ratios.R.
# =============================================================================

#' Agreement metrics between two paired numeric vectors
#'
#' @param x,y Numeric vectors of equal length (`x` = landmark-derived,
#'   `y` = published, by convention, so `bias = mean(x - y)`).
#' @return A named numeric vector: `n`, `pearson`, `spearman`, `bias`, `rmse`,
#'   `mae`.
#' @export
agreement_metrics <- function(x, y) {
  k <- is.finite(x) & is.finite(y)
  n <- sum(k)
  if (n < 3) return(c(n = n, pearson = NA, spearman = NA, bias = NA,
                      rmse = NA, mae = NA))
  xx <- x[k]; yy <- y[k]
  c(n = n,
    pearson  = suppressWarnings(stats::cor(xx, yy, method = "pearson")),
    spearman = suppressWarnings(stats::cor(xx, yy, method = "spearman")),
    bias     = mean(xx - yy),
    rmse     = sqrt(mean((xx - yy)^2)),
    mae      = mean(abs(xx - yy)))
}

# normalize segments by Bl (dimensionless -> removes the unknown pixel scale)
.norm_by_bl <- function(df, id_col) {
  segs <- intersect(fishmorph_segment_names(), names(df))
  z <- df[c(id_col, segs)]
  for (s in setdiff(segs, "Bl")) z[[s]] <- df[[s]] / df$Bl
  z$Bl <- NULL
  z
}

#' Compare landmark-derived and published FISHMORPH measurements
#'
#' Verifies the internal consistency of a FISHMORPH workbook by comparing, for
#' each specimen that carries both, the morphology measured from digitized
#' landmarks with the published segment measurements. Two scale-invariant
#' quantities are compared: the 9 ratios (each a quotient of segments, so the
#' pixel-to-cm factor cancels) and the segments expressed as a fraction of body
#' length. A large disagreement flags a digitizing or transcription error.
#'
#' @param landmarks A `fishmorph_landmarks` object, OR a data frame of
#'   landmark-derived segments already carrying an id column.
#' @param published A data frame of published segments (or ratios) with an id
#'   column matching the landmarks.
#' @param id_col Name of the identifier column in `published` (default tries
#'   `specimen`, `Genus.species`, `species`, `id`).
#' @param variant For the published side, use the `"standard"` segments or the
#'   calibrated vertical-position variants (`"calibrated"`: `Bd2, Eh2, Mo2,
#'   PFi2` when present).
#' @param space If `TRUE`, also build a shared FISHMORPH functional space: a
#'   frozen PCA fitted on the segment- (published) ratios of the matched species,
#'   onto which BOTH the segment- and the landmark-derived ratios are projected.
#'   The two clouds then live in one coordinate system and are directly
#'   comparable (as in `compare_functional_spaces_fishmorph.R`). Default `FALSE`.
#' @param group_col Optional name of a column in `published` (e.g. `"Order"` or
#'   `"Family"`) used to colour the species in the functional-space plot.
#' @param log Passed to [fishmorph_trait_space()] when `space = TRUE`
#'   (default `FALSE`).
#' @return An object of class `fishmorph_comparison`: a list with `metrics`
#'   (one row per quantity x trait), `ratios` and `segments` (merged wide tables
#'   used for plotting), `id_col`, and -- when `space = TRUE` -- `space` (the
#'   frozen [fishmorph_trait_space()]), `space_scores` (projected PC scores with
#'   a `source` column, `"segment"` vs `"landmark"`), and `space_shift`: a data
#'   frame, one row per species, sorted by **decreasing** displacement between
#'   the two methods, with `distance` (Euclidean shift over all shared PCs),
#'   `dist_2d` (shift in the two plotted axes), `dPC1`/`dPC2` and `group`. The
#'   top rows are the species whose morphospace position changes most between
#'   the segment- and landmark-based measurements. When \pkg{vegan} is available,
#'   `procrustes` holds a Procrustes test ([vegan::protest()]) of the concordance
#'   between the two configurations (`correlation`, `significance`, `ss`, `n`).
#' @param na_action For `space = TRUE`: how to handle species with missing
#'   ratios when building/projecting the shared space. `"omit"` (default) drops
#'   them; `"impute_mean"`, `"impute_group_mean"`, `"missforest"` or
#'   `"missforest_phylo"` impute both the segment- and landmark-ratio matrices so
#'   no species is lost (same options as [fishmorph_trait_space()]).
#' @param missforest_ntree,missforest_maxiter,tree,missforest_phylo_k,phylo_axes
#'   Passed to the missForest imputation when `na_action` is a missForest method.
#'   Note that `"missforest_phylo"` takes its species key from `id_col`, not from
#'   `group_col`: the former identifies the species of each row, the latter is a
#'   coarser grouping (`"Order"`, `"Family"`) used as an auxiliary predictor and
#'   for colouring.
#' @seealso [plot_segment_landmark_agreement()], [summary.fishmorph_comparison()]
#' @export
compare_segments_landmarks <- function(landmarks, published, id_col = NULL,
                                       variant = c("standard", "calibrated"),
                                       space = FALSE, group_col = NULL,
                                       log = FALSE,
                                       na_action = c("omit", "fail",
                                                     "impute_mean",
                                                     "impute_group_mean",
                                                     "missforest",
                                                     "missforest_phylo"),
                                       missforest_ntree = 100,
                                       missforest_maxiter = 10, tree = NULL,
                                       missforest_phylo_k = 10,
                                       phylo_axes = NULL) {
  na_action <- match.arg(na_action)
  variant <- match.arg(variant)
  id_col <- id_col %||% intersect(c("specimen", "Genus.species", "species",
                                    "id"), names(published))[1]
  if (is.na(id_col))
    stop("Could not find an id column in `published`; set `id_col`.",
         call. = FALSE)
  published <- .ensure_id_column(published, id_col, "published")

  # landmark-derived segments (raw/pixel units are fine: comparisons are
  # scale-invariant) and ratios.
  if (is_fishmorph_landmarks(landmarks)) {
    lseg <- fishmorph_segments(landmarks)
    names(lseg)[names(lseg) == "specimen"] <- id_col
  } else {
    lseg <- landmarks
    if (!id_col %in% names(lseg) && "specimen" %in% names(lseg))
      names(lseg)[names(lseg) == "specimen"] <- id_col
  }
  lrat <- fishmorph_ratios(lseg)
  names(lrat)[names(lrat) == "specimen"] <- id_col
  lrat[[id_col]] <- lseg[[id_col]]

  # published segments: choose standard or calibrated vertical variants
  pick <- function(primary, fallback = NULL) {
    if (!is.null(fallback) && fallback %in% names(published))
      suppressWarnings(as.numeric(published[[fallback]]))
    else suppressWarnings(as.numeric(published[[primary]]))
  }
  pub_seg <- data.frame(id = published[[id_col]], stringsAsFactors = FALSE)
  names(pub_seg)[1] <- id_col
  base_cols <- c("Bl", "Hd", "Ed", "Jl", "PFl", "CPd", "CFd")
  for (nm in base_cols) if (nm %in% names(published)) pub_seg[[nm]] <- pick(nm)
  if (variant == "calibrated") {
    pub_seg$Bd  <- pick("Bd",  "Bd2");  pub_seg$Eh  <- pick("Eh",  "Eh2")
    pub_seg$Mo  <- pick("Mo",  "Mo2");  pub_seg$PFi <- pick("PFi", "PFi2")
  } else {
    pub_seg$Bd  <- pick("Bd");  pub_seg$Eh  <- pick("Eh")
    pub_seg$Mo  <- pick("Mo");  pub_seg$PFi <- pick("PFi")
  }
  # published ratios: use existing ratio columns if present, else compute
  if (all(fishmorph_ratio_names() %in% names(published))) {
    prat <- published[c(id_col, fishmorph_ratio_names())]
  } else {
    prat <- fishmorph_ratios(pub_seg)
    names(prat)[names(prat) == "specimen"] <- id_col
    prat[[id_col]] <- pub_seg[[id_col]]
  }

  metrics <- list()

  # --- RATIO agreement ---
  mr <- merge(lrat, prat, by = id_col, suffixes = c(".lm", ".pub"))
  for (rn in fishmorph_ratio_names()) {
    a <- mr[[paste0(rn, ".lm")]]; b <- mr[[paste0(rn, ".pub")]]
    if (is.null(a) || is.null(b)) next
    st <- agreement_metrics(a, b)
    metrics[[length(metrics) + 1L]] <- data.frame(
      quantity = "ratio", variant = variant, trait = rn,
      t(st), row.names = NULL, check.names = FALSE)
  }

  # --- SEGMENT/Bl agreement ---
  segs_present <- intersect(fishmorph_segment_names(), names(lseg))
  ms <- NULL
  if (length(segs_present) > 1 && "Bl" %in% names(pub_seg)) {
    lseg_n <- .norm_by_bl(lseg, id_col)
    pub_n  <- .norm_by_bl(pub_seg, id_col)
    ms <- merge(lseg_n, pub_n, by = id_col, suffixes = c(".lm", ".pub"))
    for (s in setdiff(intersect(names(lseg_n), names(pub_n)), id_col)) {
      st <- agreement_metrics(ms[[paste0(s, ".lm")]], ms[[paste0(s, ".pub")]])
      metrics[[length(metrics) + 1L]] <- data.frame(
        quantity = "segment_over_Bl", variant = "standard", trait = s,
        t(st), row.names = NULL, check.names = FALSE)
    }
  }

  md <- do.call(rbind, metrics)
  if (!is.null(md))
    md[] <- lapply(md, function(c) if (is.numeric(c)) round(c, 4) else c)

  # --- shared functional space (segment- vs landmark-based) ------------------
  fm_space <- NULL; space_scores <- NULL; space_shift <- NULL; procrustes <- NULL
  rn <- fishmorph_ratio_names()
  if (isTRUE(space) &&
      all(c(paste0(rn, ".pub"), paste0(rn, ".lm")) %in% names(mr))) {
    ids <- as.character(mr[[id_col]])
    grp <- NULL
    if (!is.null(group_col) && group_col %in% names(published)) {
      gmap <- stats::setNames(as.character(published[[group_col]]),
                              as.character(published[[id_col]]))
      grp <- unname(gmap[ids])
    }
    seg_df <- stats::setNames(as.data.frame(mr[paste0(rn, ".pub")]), rn)
    lm_df  <- stats::setNames(as.data.frame(mr[paste0(rn, ".lm")]), rn)
    seg_df$Species <- ids; lm_df$Species <- ids
    # optionally impute BOTH ratio matrices so no species is dropped, then fit
    # the ordination on the (complete) segment ratios and project both.
    if (na_action %in% c("impute_mean", "impute_group_mean", "missforest",
                         "missforest_phylo")) {
      grp_f <- if (!is.null(grp)) factor(grp) else NULL
      imp_ratios <- function(df) {
        X <- as.matrix(df[rn]); storage.mode(X) <- "double"
        # `ids` carries the values of `id_col`, hence the species: that is the
        # key "missforest_phylo" needs to find its axes. `grp_f` comes from
        # `group_col` (Order, Family, ...) and is only a predictor.
        res <- .apply_na_action(X, grp_f, na_action,
                                missforest_ntree = missforest_ntree,
                                missforest_maxiter = missforest_maxiter,
                                context = "ratios", tree = tree,
                                missforest_phylo_k = missforest_phylo_k,
                                phylo_axes = phylo_axes, species = ids)
        df[rn] <- res$X
        df
      }
      seg_df <- imp_ratios(seg_df); lm_df <- imp_ratios(lm_df)
      fit_na <- "omit"                 # no NA left after imputation
    } else {
      fit_na <- na_action
    }
    # frozen ordination on the published (segment) ratios of matched species
    fm_space <- fishmorph_trait_space(seg_df, groups = grp, log = log,
                                      scale = TRUE, na_action = fit_na)
    ps <- .project_onto_space(fm_space, seg_df, groups = grp)
    pl <- .project_onto_space(fm_space, lm_df,  groups = grp)
    ac <- names(fm_space$scores)[1:2]
    mk <- function(p, src) {
      d <- data.frame(species = p$id, PC1 = p[[ac[1]]], PC2 = p[[ac[2]]],
                      source = src, stringsAsFactors = FALSE)
      if (!is.null(p$group)) d$group <- p$group
      d
    }
    space_scores <- rbind(mk(ps, "segment"), mk(pl, "landmark"))

    # per-species displacement between the two methods in the shared ordination.
    # Distance is Euclidean over ALL shared principal components (the full
    # morphospace shift); the two plotted axes are also reported for convenience.
    pc_cols <- intersect(names(ps), colnames(fm_space$pca$x))
    common <- intersect(as.character(ps$id), as.character(pl$id))
    if (length(common) > 0 && length(pc_cols) > 0) {
      Ps <- ps[match(common, as.character(ps$id)), pc_cols, drop = FALSE]
      Pl <- pl[match(common, as.character(pl$id)), pc_cols, drop = FALSE]
      dist_full <- sqrt(rowSums((as.matrix(Pl) - as.matrix(Ps))^2))
      d2 <- sqrt((Pl[[ac[1]]] - Ps[[ac[1]]])^2 + (Pl[[ac[2]]] - Ps[[ac[2]]])^2)
      gmap2 <- if (!is.null(ps$group))
        stats::setNames(as.character(ps$group), as.character(ps$id)) else NULL
      space_shift <- data.frame(
        species  = common,
        distance = dist_full,           # full-space displacement
        dist_2d  = d2,                   # in the 2 plotted axes (PC1, PC2)
        dPC1 = Pl[[ac[1]]] - Ps[[ac[1]]],
        dPC2 = Pl[[ac[2]]] - Ps[[ac[2]]],
        stringsAsFactors = FALSE)
      if (!is.null(gmap2)) space_shift$group <- unname(gmap2[common])
      space_shift <- space_shift[order(-space_shift$distance), , drop = FALSE]
      rownames(space_shift) <- NULL

      # Procrustes test between the two configurations (segment vs landmark) on
      # the shared ordination axes -- the concordance of the two morphospaces,
      # as in compare_functional_spaces_fishmorph.R.
      if (nrow(Ps) >= 4 && requireNamespace("vegan", quietly = TRUE)) {
        pt <- tryCatch(vegan::protest(as.matrix(Ps), as.matrix(Pl),
                                      permutations = 999), error = function(e) NULL)
        if (!is.null(pt))
          procrustes <- list(correlation = unname(pt$t0),
                             ss = unname(pt$ss),
                             significance = pt$signif,
                             permutations = pt$permutations,
                             n = nrow(Ps), object = pt)
      } else if (nrow(Ps) >= 4) {
        warning("Package 'vegan' is required for the Procrustes test; ",
                "install it to populate `$procrustes`.", call. = FALSE)
      }
    }
  }

  structure(list(metrics = md, ratios = mr, segments = ms, id_col = id_col,
                 variant = variant, space = fm_space,
                 space_scores = space_scores, space_shift = space_shift,
                 procrustes = procrustes),
            class = "fishmorph_comparison")
}

#' Summary of a segment-versus-landmark comparison
#'
#' Prints what a table of metrics does not say at a glance: the ratio agreement
#' (n, Pearson r, bias, RMSE) trait by trait, the WEAKEST ratio -- the one that
#' decides whether the two measurement routes can be pooled at all -- and, when
#' the comparison was run with `space = TRUE`, the species whose position in the
#' functional space moves most between the two routes, plus the Procrustes test
#' of the two spaces.
#'
#' @param object A `fishmorph_comparison` object returned by
#'   [compare_segments_landmarks()].
#' @param ... Ignored, present for compatibility with the generic.
#' @return The metrics `data.frame` of `object`, invisibly (NULL if no specimen
#'   was comparable).
#' @seealso [compare_segments_landmarks()],
#'   [plot_segment_landmark_agreement()]
#' @export
summary.fishmorph_comparison <- function(object, ...) {
  m <- object$metrics
  if (is.null(m)) { cat("No comparable specimens.\n"); return(invisible(m)) }
  cat("<fishmorph_comparison>\n")
  cat(sprintf("  variant: %s | matched id column: %s\n",
              object$variant, object$id_col))
  r <- m[m$quantity == "ratio", c("trait", "n", "pearson", "bias", "rmse")]
  cat("\nRatio agreement (landmark vs published):\n")
  print(r, row.names = FALSE)
  worst <- r[which.min(r$pearson), , drop = FALSE]
  if (nrow(worst))
    cat(sprintf("\n  weakest ratio: %s (r = %.2f)\n", worst$trait, worst$pearson))
  if (!is.null(object$space_shift)) {
    ss <- object$space_shift
    cat(sprintf("\nLargest position shift between methods (top %d of %d species):\n",
                min(10L, nrow(ss)), nrow(ss)))
    top <- utils::head(ss[, intersect(c("species", "group", "distance", "dist_2d"),
                                      names(ss))], 10)
    print(top, row.names = FALSE)
  }
  if (!is.null(object$procrustes)) {
    p <- object$procrustes
    cat(sprintf(paste0("\nProcrustes test (segment vs landmark space, n = %d): ",
                       "correlation = %.3f, p = %.3f (%d permutations)\n"),
                p$n, p$correlation, p$significance, p$permutations))
  }
  invisible(m)
}

#' Plot landmark-vs-published agreement
#'
#' @param x A `fishmorph_comparison` object from [compare_segments_landmarks()].
#' @param type `"scatter"` (published vs landmark, one panel per ratio),
#'   `"bar"` (Pearson r per ratio), or `"space"` (the two FISHMORPH functional
#'   spaces -- segment-based and landmark-based -- side by side on the shared
#'   frozen ordination; requires `compare_segments_landmarks(..., space = TRUE)`).
#' @param style For `type = "space"`: per-group geometry, `"hull"` (default),
#'   `"spider"`, `"density"` or `"none"` (see [plot.fishmorph_projection()]).
#' @param reference_density For `type = "space"`: draw the shared-space density
#'   heatmap behind each panel (default `TRUE`).
#' @param arrows For `type = "space"`: overlay the trait loadings as biplot
#'   arrows on each panel (default `FALSE`).
#' @param arrow_scale,arrow_col Loading-arrow length fraction (in `(0, 1]`) and
#'   colour, when `arrows = TRUE`.
#' @param engine `"ggplot2"` (default) or `"base"`. `type = "space"` always uses
#'   base graphics (the intraitR ordination style).
#' @param ... Passed through (e.g. to [graphics::plot()] for `type = "space"`).
#' @return A ggplot object (invisibly for base / `"space"`).
#' @export
plot_segment_landmark_agreement <- function(x, type = c("scatter", "bar", "space"),
                                            style = c("hull", "spider", "density",
                                                      "none"),
                                            reference_density = TRUE,
                                            arrows = FALSE, arrow_scale = 0.8,
                                            arrow_col = "grey20",
                                            engine = c("ggplot2", "base"), ...) {
  type <- match.arg(type); style <- match.arg(style); engine <- match.arg(engine)
  if (!inherits(x, "fishmorph_comparison"))
    stop("`x` must be a fishmorph_comparison object.", call. = FALSE)

  if (type == "space") {
    if (is.null(x$space) || is.null(x$space_scores))
      stop("No functional space: recompute with ",
           "compare_segments_landmarks(..., space = TRUE).", call. = FALSE)
    ss <- x$space_scores
    ve <- (x$space$pca$sdev^2 / sum(x$space$pca$sdev^2)) * 100
    xlab <- sprintf("PC1 (%.1f%%)", ve[1]); ylab <- sprintf("PC2 (%.1f%%)", ve[2])
    xr <- range(ss$PC1, na.rm = TRUE); yr <- range(ss$PC2, na.rm = TRUE)
    bg <- as.matrix(x$space$scores[, 1:2])
    has_grp <- "group" %in% names(ss) && !all(is.na(ss$group))
    op <- graphics::par(mfrow = c(1, 2)); on.exit(graphics::par(op))
    for (src in c("segment", "landmark")) {
      d <- ss[ss$source == src, , drop = FALSE]
      grp <- if (has_grp) d$group else NULL
      .plot_ordination(d[, c("PC1", "PC2")], grp, xlab, ylab, style = style,
                       legend = has_grp, legend_position = "outside",
                       legend_title = "group", space_name = src,
                       background = bg, background_density = reference_density,
                       xlim = xr, ylim = yr, ...)
      if (isTRUE(arrows))
        .fm_biplot_arrows(x$space$pca$rotation[, 1:2, drop = FALSE],
                          arrow_scale = arrow_scale, arrow_col = arrow_col)
    }
    return(invisible(x))
  }

  rn <- fishmorph_ratio_names()

  if (type == "scatter") {
    m <- x$ratios
    long <- do.call(rbind, lapply(rn, function(r) {
      pc <- paste0(r, ".pub"); lc <- paste0(r, ".lm")
      if (!all(c(pc, lc) %in% names(m))) return(NULL)
      data.frame(trait = r, published = m[[pc]], landmark = m[[lc]])
    }))
    long <- long[is.finite(long$published) & is.finite(long$landmark), ]
    if (engine == "ggplot2" && .has_ggplot()) {
      ggplot2::ggplot(long, ggplot2::aes(x = .data$published, y = .data$landmark)) +
        ggplot2::geom_abline(slope = 1, intercept = 0, colour = "grey50",
                             linetype = 2) +
        ggplot2::geom_point(colour = "#3366aa", size = 0.7, alpha = 0.6) +
        ggplot2::facet_wrap(~trait, scales = "free") +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "published (segments)", y = "from landmarks")
    } else {
      op <- graphics::par(mfrow = c(3, 3), mar = c(4, 4, 2, 1))
      on.exit(graphics::par(op))
      for (r in rn) {
        d <- long[long$trait == r, ]
        if (!nrow(d)) { graphics::plot.new(); next }
        rng <- range(c(d$published, d$landmark))
        graphics::plot(d$published, d$landmark, pch = 19, cex = 0.5,
                       col = "#3366aa", xlim = rng, ylim = rng,
                       xlab = "published", ylab = "landmark",
                       main = sprintf("%s (r=%.2f)", r,
                                      suppressWarnings(stats::cor(d$published,
                                                                  d$landmark))))
        graphics::abline(0, 1, col = "grey40", lty = 2)
      }
      invisible(NULL)
    }
  } else {
    m <- x$metrics[x$metrics$quantity == "ratio", c("trait", "pearson")]
    m <- m[match(rn, m$trait), ]
    if (engine == "ggplot2" && .has_ggplot()) {
      m$trait <- factor(m$trait, levels = rn)
      ggplot2::ggplot(m, ggplot2::aes(x = .data$trait, y = .data$pearson)) +
        ggplot2::geom_col(fill = "#88bbdd") +
        ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
        ggplot2::ylim(min(0, min(m$pearson, na.rm = TRUE)), 1) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = NULL, y = "Pearson r (landmark vs published)")
    } else {
      graphics::barplot(m$pearson, names.arg = m$trait, ylim = c(min(0,
                        min(m$pearson, na.rm = TRUE)), 1), col = "#88bbdd",
                        ylab = "Pearson r")
      invisible(NULL)
    }
  }
}

#' Diagnose non-finite or extreme FISHMORPH ratios
#'
#' Flags specimens whose ratios are infinite, `NaN` or extreme outliers, usually
#' caused by a zero/near-zero denominator segment (a digitizing error). Ported
#' from diagnostic_infinite_ratios.R.
#'
#' @param data A data frame of ratios or segments, or a `fishmorph_landmarks`
#'   object.
#' @param z_thresh Absolute robust z-score (median/MAD) above which a finite
#'   value is also reported as an outlier. Default 8.
#' @return A data frame of flagged `id`, `trait`, `value` and `reason`
#'   (`"infinite"`, `"nan"`, `"outlier"`). Zero rows means the data are clean.
#' @export
check_infinite_ratios <- function(data, z_thresh = 8) {
  if (is_fishmorph_landmarks(data)) data <- fishmorph_ratios(data)
  rn <- intersect(fishmorph_ratio_names(), names(data))
  if (!length(rn))
    stop("No ratio columns found; supply ratios or landmarks.", call. = FALSE)
  id <- if ("Species" %in% names(data)) data$Species else
    if ("specimen" %in% names(data)) data$specimen else seq_len(nrow(data))
  flags <- list()
  for (r in rn) {
    v <- suppressWarnings(as.numeric(data[[r]]))
    inf <- which(is.infinite(v)); nan <- which(is.nan(v))
    fin <- v[is.finite(v)]
    out <- integer(0)
    if (length(fin) > 5) {
      md <- stats::median(fin); ma <- stats::mad(fin)
      if (ma > 0) out <- which(is.finite(v) & abs(v - md) / ma > z_thresh)
    }
    for (i in inf) flags[[length(flags) + 1L]] <-
      data.frame(id = id[i], trait = r, value = v[i], reason = "infinite")
    for (i in nan) flags[[length(flags) + 1L]] <-
      data.frame(id = id[i], trait = r, value = NA_real_, reason = "nan")
    for (i in out) flags[[length(flags) + 1L]] <-
      data.frame(id = id[i], trait = r, value = v[i], reason = "outlier")
  }
  if (!length(flags))
    return(data.frame(id = character(0), trait = character(0),
                      value = numeric(0), reason = character(0)))
  do.call(rbind, flags)
}

#' Check the FISHMORPH digitizing conventions
#'
#' Measures, per specimen, how far a configuration is from satisfying the five
#' geometric conventions (see [correct_geometry_conventions()]) by comparing it
#' with its corrected version. Large residuals point to a mis-placed landmark.
#'
#' @param x A `fishmorph_landmarks` object.
#' @param tolerance Residual (in the specimen's own units) above which a specimen
#'   is flagged. Default 0 returns the residual for every specimen.
#' @return A data frame with `specimen`, `max_shift`, `mean_shift` and, for each
#'   convention group, the residual; plus a logical `flag`.
#' @export
check_geometry_conventions <- function(x, tolerance = 0) {
  co <- .fm_coords(x); sp <- .fm_specimens(x)
  corr <- correct_geometry_conventions(x)
  cc <- .fm_coords(corr)
  rows <- lapply(seq_along(sp), function(i) {
    P <- co[, , i]; Q <- cc[, , i]
    d <- sqrt(rowSums((P - Q)^2))
    d[!is.finite(d)] <- NA
    data.frame(specimen = sp[i],
               max_shift = suppressWarnings(max(d, na.rm = TRUE)),
               mean_shift = suppressWarnings(mean(d, na.rm = TRUE)),
               eye_vertical = suppressWarnings(max(d[c(5, 13, 7, 14, 6, 8)],
                                                   na.rm = TRUE)),
               belly_line = suppressWarnings(max(d[c(9, 8, 11, 4)],
                                                 na.rm = TRUE)),
               perpendiculars = suppressWarnings(max(d[c(9, 4, 11)],
                                                     na.rm = TRUE)),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(v) { v[!is.finite(v)] <- NA; v })
  out$flag <- is.finite(out$max_shift) & out$max_shift > tolerance
  rownames(out) <- NULL
  out
}
