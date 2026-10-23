# =============================================================================
# plot-fishmorph-density.R -- 1-D density curves of the FISHMORPH space (axes and ratios).
# Moved from intraitR (R/plot_fishmorph_density.R) in Rfishmorph 0.17.0: Rfishmorph owns the
# visualisation of the FISHMORPH functional space, intraitR re-exports it.
# Works on any "fishmorph_projection", including the "intrait_fishmorph_projection"
# returned by intraitR::project_fishmorph() (a subclass), whose ITV results
# ($itv_proportion) are shown when present.
# =============================================================================

#' Density curves of the FISHMORPH space, per functional axis and per ratio
#'
#' For a projection built by [project_fishmorph()], draws a panel of kernel
#' density curves for every requested variable -- each functional (PCA) axis
#' and each morphological ratio -- comparing the whole FISHMORPH reference
#' database (a filled grey curve) with each focal species (a coloured curve).
#' This is the one-dimensional, marginal complement to the two-dimensional
#' ordination drawn by [plot.fishmorph_projection()]: rather than
#' showing where a species' intraspecific trait variation (ITV) sits in the
#' morphospace as a whole, it shows, variable by variable, how the spread of
#' the projected individuals compares with the global distribution along that
#' single axis or ratio -- e.g. whether a species occupies the centre or a
#' tail of the reference, and how wide its ITV is relative to the global
#' range.
#'
#' Each curve is drawn on a shared percentage axis: its density is rescaled to
#' a percentage of its *own* maximum (peak = 100%). Raw kernel densities
#' integrate to 1, so a species with a narrow, tightly-clustered ITV would
#' otherwise appear as a tall spike and a widely-spread one as a low, flat
#' curve, making the two hard to compare on the same axis. Normalising each to
#' its own peak puts every species and the reference on the same 0-100% height,
#' so the eye compares *where* each distribution sits and *how wide* it is
#' rather than a height that merely reflects its spread.
#'
#' Axis panels use the projected scores: the reference curve is the reference
#' species' own scores on that component (`x$global_scores_all`) and each
#' focal species' curve is its projected specimens' scores
#' (`x$scores_all`). Ratio panels use the trait values on the analysis scale
#' (the `log10(x + 1)` scale when [project_fishmorph()] was called with
#' `log_transform = TRUE`): the reference curve is `x$reference_traits` and
#' each focal species' curve is its specimens' traits (`x$specimen_traits`),
#' so the two are always on the same, directly comparable scale. Species
#' colours are the same session-stable colours used by
#' [plot.fishmorph_projection()], so a species keeps one colour
#' across both figures.
#'
#' @param x An object of class `"fishmorph_projection"` (or the
#'   `"intrait_fishmorph_projection"` subclass made by intraitR), from
#'   [project_fishmorph()].
#' @param what Character, which variables to draw: `"both"` (default) draws
#'   the requested axes followed by the requested ratios, `"axes"` only the
#'   functional axes, `"ratios"` only the morphological ratios.
#' @param axes Integer vector, which principal-component axes to draw density
#'   panels for (ignored when `what = "ratios"`). `NULL` (default) uses the
#'   two axes retained for plotting by the projection (`x$axes`); pass e.g.
#'   `1:4` to inspect further components. Each must be within the number of
#'   components available.
#' @param traits Character vector, which ratios to draw density panels for
#'   (ignored when `what = "axes"`). `NULL` (default) uses every trait in the
#'   space (`x$traits`); pass a subset to restrict the figure. Unknown trait
#'   names are reported with an error.
#' @param select_species Optional character vector: draw only these focal
#'   species' curves (matched against the projection's groups). The grey
#'   reference curve is always the full reference database, unaffected by this
#'   filter. `NULL` (default) draws every projected species.
#' @param select_specimens Optional character vector: restrict the focal
#'   curves to these specimen identifiers (matched against the score row
#'   names). Combined with `select_species` by intersection. `NULL` (default)
#'   keeps them all.
#' @param reference_col Colour of the FISHMORPH reference curve (and its fill).
#'   Defaults to `"grey55"`.
#' @param reference_fill Logical, shade the area under the reference curve (in
#'   a translucent `reference_col`) as well as drawing its outline, so the
#'   global distribution reads as a background band beneath the coloured
#'   species curves. Defaults to `TRUE`.
#' @param fill_alpha Numeric in `[0, 1]`, opacity of the reference fill (when
#'   `reference_fill = TRUE`). Defaults to `0.35`.
#' @param species_fill Logical, shade the area under each focal species' curve
#'   in a translucent version of that species' colour (as well as drawing the
#'   curve outline), so the species distributions read as filled bands rather
#'   than bare lines. The transparency lets overlapping species remain
#'   distinguishable where their distributions cross. Defaults to `TRUE`.
#' @param species_fill_alpha Numeric in `[0, 1]`, opacity of the species fills
#'   (when `species_fill = TRUE`). Kept lighter than `fill_alpha` by default
#'   because several species fills can overlap in one panel. Defaults to `0.2`.
#' @param rug Logical, add a rug of the individual focal specimens' values
#'   along the axis of each panel, coloured by species, so small samples (for
#'   which a density curve is only indicative, or is omitted entirely) still
#'   show where their specimens fall. Defaults to `TRUE`.
#' @param lwd Numeric, line width of the species curves (the reference curve is
#'   drawn one step heavier). Defaults to `1.8`.
#' @param legend Logical, draw a shared species legend in its own panel after
#'   the variable panels. Defaults to `TRUE`.
#' @param legend_italic Logical, italicise the species names in the legend (as
#'   binomials). Defaults to `TRUE`.
#' @param abbreviate_species Logical, abbreviate the genus in the legend
#'   (e.g. `"Squalius cephalus"` becomes `"S. cephalus"`). Defaults to `TRUE`.
#' @param mfrow Optional integer vector `c(nrow, ncol)` giving the panel grid.
#'   `NULL` (default) chooses a near-square layout that fits every variable
#'   panel plus the legend panel.
#' @param ... Further graphical parameters passed to each panel's
#'   [graphics::plot()] call (e.g. `cex.main`).
#'
#' @return Invisibly returns a list with one element per drawn variable, each
#'   itself a list of the [stats::density()] objects actually computed
#'   (`reference`, and one per species that had enough points), so the
#'   densities can be reused or redrawn. Called for its side effect of drawing
#'   the figure.
#'
#' @seealso [project_fishmorph()],
#'   [plot.fishmorph_projection()] (the two-dimensional ordination)
#'
#' @examples
#' \donttest{
#' ref <- load_fishmorph_reference()
#' sp <- ref[1:60, ]; sp$species <- sp$Family
#' proj <- project_fishmorph(sp, reference = ref, specimens_prelogged = TRUE)
#'
#' # every axis + every ratio, reference in grey and each group in colour
#' plot_fishmorph_density(proj)
#'
#' # only the two functional axes
#' plot_fishmorph_density(proj, what = "axes")
#'
#' # only a couple of ratios, for one group
#' plot_fishmorph_density(proj, what = "ratios", traits = c("BEl", "REs"),
#'                        select_species = levels(proj$groups)[1])
#' }
#'
#' @export
plot_fishmorph_density <- function(x,
                                   what = c("both", "axes", "ratios"),
                                   axes = NULL, traits = NULL,
                                   select_species = NULL, select_specimens = NULL,
                                   reference_col = "grey55",
                                   reference_fill = TRUE, fill_alpha = 0.35,
                                   species_fill = TRUE, species_fill_alpha = 0.2,
                                   rug = TRUE, lwd = 1.8,
                                   legend = TRUE, legend_italic = TRUE,
                                   abbreviate_species = TRUE,
                                   mfrow = NULL, ...) {
  if (!inherits(x, "fishmorph_projection")) {
    stop("`x` must be a \"fishmorph_projection\" object from project_fishmorph().",
         call. = FALSE)
  }
  what <- match.arg(what)

  ## -- resolve which axes / ratios to draw --------------------------------
  n_comp <- ncol(x$scores_all)
  if (is.null(axes)) axes <- x$axes
  axes <- as.integer(axes)
  if (what != "ratios") {
    if (any(!is.finite(axes)) || any(axes < 1) || any(axes > n_comp)) {
      stop("`axes` must index components 1:", n_comp, " (the axes available).",
           call. = FALSE)
    }
  }
  if (is.null(traits)) traits <- x$traits
  if (what != "axes") {
    unknown <- setdiff(traits, x$traits)
    if (length(unknown) > 0) {
      stop("`traits` not in this space: ", paste(unknown, collapse = ", "),
           ". Available: ", paste(x$traits, collapse = ", "), call. = FALSE)
    }
  }

  ## -- reference trait matrix (analysis scale): use the stored copy, else
  ##    reconstruct it exactly from the frozen PCA (for projections built
  ##    before `reference_traits` was stored). prcomp keeps a full orthonormal
  ##    rotation here (many more reference rows than traits), so
  ##    scores_all %*% t(loadings) inverts the rotation exactly; undo the
  ##    PCA's centring/scaling to return to trait units.
  ref_traits <- x$reference_traits
  if (is.null(ref_traits)) {
    M <- x$global_scores_all %*% t(x$loadings)
    pca <- x$pca
    if (!is.null(pca) && is.numeric(pca$scale)) M <- sweep(M, 2, pca$scale, "*")
    if (!is.null(pca) && is.numeric(pca$center)) M <- sweep(M, 2, pca$center, "+")
    colnames(M) <- rownames(x$loadings)
    ref_traits <- M
  }

  ## -- resolve the focal species / specimen selection ---------------------
  gr <- x$groups
  keep <- rep(TRUE, length(gr))
  if (!is.null(select_species)) {
    unmatched <- setdiff(select_species, levels(gr))
    if (length(unmatched) > 0) {
      warning("`select_species` not among projected species: ",
              paste(unmatched, collapse = ", "), call. = FALSE)
    }
    keep <- keep & as.character(gr) %in% select_species
  }
  if (!is.null(select_specimens)) {
    keep <- keep & rownames(x$scores_all) %in% select_specimens
  }
  if (!any(keep)) stop("No focal specimens match the selection.", call. = FALSE)
  gr <- droplevels(gr[keep])
  species <- levels(gr)
  sp_cols <- .stable_group_colors(species)

  ## -- assemble the list of panels ----------------------------------------
  # Per-axis variance explained recomputed from the frozen PCA, so it is
  # available for *any* requested component, not only the two that
  # `x$var_explained` covers.
  pc_var <- (x$pca$sdev^2 / sum(x$pca$sdev^2)) * 100
  panels <- list()
  if (what != "ratios") {
    for (a in axes) {
      panels[[length(panels) + 1]] <- list(
        kind = "axis",
        main = sprintf("PC%d (%.1f%%)", a, pc_var[a]),
        xlab = sprintf("PC%d score", a),
        ref  = x$global_scores_all[, a],
        sp   = lapply(species, function(s) x$scores_all[keep, , drop = FALSE][gr == s, a])
      )
    }
  }
  if (what != "axes") {
    for (tr in traits) {
      panels[[length(panels) + 1]] <- list(
        kind = "ratio",
        main = tr,
        xlab = paste0(tr, " (analysis scale)"),
        ref  = ref_traits[, tr],
        sp   = lapply(species, function(s) x$specimen_traits[keep, , drop = FALSE][gr == s, tr])
      )
    }
  }
  if (length(panels) == 0) stop("Nothing to draw for what = \"", what, "\".", call. = FALSE)

  ## -- layout: variable panels + one legend panel -------------------------
  n_panel <- length(panels) + as.integer(isTRUE(legend))
  if (is.null(mfrow)) {
    nc <- ceiling(sqrt(n_panel))
    nr <- ceiling(n_panel / nc)
    mfrow <- c(nr, nc)
  }
  old_par <- graphics::par(mfrow = mfrow, mar = c(4, 4, 2.5, 1),
                           tcl = 0.3, mgp = c(2.2, 0.5, 0), las = 1)
  on.exit(graphics::par(old_par), add = TRUE)

  ## -- one density panel --------------------------------------------------
  # Each curve's density is rescaled to a percentage of its *own* maximum
  # (peak = 100%), drawn on a shared 0-100% axis. Raw kernel densities
  # integrate to 1, so a species with narrow, tightly-clustered ITV produces
  # a tall spike while a widely-spread one produces a low, flat curve; on the
  # same axis the spike dominates and the two are hard to compare. Normalising
  # each to its own peak puts every species (and the reference) on the same
  # 0-100% height, so what the eye compares is *where* each distribution sits
  # and *how wide* it is -- not a height that merely reflects its spread. The
  # `stats::density()` objects returned (invisibly) keep their original,
  # un-normalised `y`, so the raw densities remain available for reuse.
  dots <- list(...)
  to_pct <- function(d) {
    if (is.null(d)) return(NULL)
    m <- max(d$y, na.rm = TRUE)
    if (!is.finite(m) || m <= 0) m <- 1
    d$y * 100 / m
  }
  draw_panel <- function(p) {
    ref_vals <- p$ref[is.finite(p$ref)]
    sp_vals  <- lapply(p$sp, function(v) v[is.finite(v)])
    d_ref <- if (length(ref_vals) >= 2) stats::density(ref_vals) else NULL
    d_sp  <- lapply(sp_vals, function(v) if (length(v) >= 2) stats::density(v) else NULL)

    all_x <- c(ref_vals, unlist(sp_vals))
    xr <- if (length(all_x)) range(all_x) else c(0, 1)

    base_args <- utils::modifyList(
      list(x = NA, xlim = xr, ylim = c(0, 105),
           xlab = p$xlab, ylab = "Density (% of max)", main = p$main, type = "n"),
      dots
    )
    do.call(graphics::plot, base_args)

    if (!is.null(d_ref)) {
      y_ref <- to_pct(d_ref)
      if (isTRUE(reference_fill)) {
        graphics::polygon(c(d_ref$x[1], d_ref$x, d_ref$x[length(d_ref$x)]),
                          c(0, y_ref, 0),
                          col = grDevices::adjustcolor(reference_col, alpha.f = fill_alpha),
                          border = NA)
      }
      graphics::lines(d_ref$x, y_ref, col = reference_col, lwd = lwd + 0.6)
    }
    for (i in seq_along(d_sp)) {
      if (is.null(d_sp[[i]])) next
      y_sp <- to_pct(d_sp[[i]])
      if (isTRUE(species_fill)) {
        graphics::polygon(c(d_sp[[i]]$x[1], d_sp[[i]]$x, d_sp[[i]]$x[length(d_sp[[i]]$x)]),
                          c(0, y_sp, 0),
                          col = grDevices::adjustcolor(sp_cols[i], alpha.f = species_fill_alpha),
                          border = NA)
      }
      graphics::lines(d_sp[[i]]$x, y_sp, col = sp_cols[i], lwd = lwd)
    }
    if (isTRUE(rug)) {
      for (i in seq_along(sp_vals)) {
        if (length(sp_vals[[i]])) {
          graphics::rug(sp_vals[[i]], col = sp_cols[i], lwd = 1, ticksize = 0.03)
        }
      }
    }
    stats::setNames(c(list(reference = d_ref), d_sp), c("reference", species))
  }

  out <- lapply(panels, draw_panel)
  names(out) <- vapply(panels, function(p) p$main, character(1))

  ## -- shared legend panel ------------------------------------------------
  if (isTRUE(legend)) {
    graphics::plot.new()
    labs <- species
    if (isTRUE(abbreviate_species)) labs <- .abbreviate_species_name(labs)
    ref_lab <- "FISHMORPH reference"
    legend_text <- if (isTRUE(legend_italic)) {
      # Italic species binomials plus a plain-text reference entry, kept as a
      # single expression vector (rather than concatenating an expression with
      # a bare string, which legend() renders inconsistently).
      items <- c(lapply(labs, function(l) bquote(italic(.(l)))),
                 list(bquote(.(ref_lab))))
      as.expression(items)
    } else {
      c(labs, ref_lab)
    }
    graphics::legend("center",
                     legend = legend_text,
                     col = c(unname(sp_cols), reference_col),
                     lwd = c(rep(lwd, length(species)), lwd + 0.6),
                     bty = "n", cex = 1, title = "Species")
  }

  invisible(out)
}
