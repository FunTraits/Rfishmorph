# =============================================================================
# plotly-fishmorph.R -- interactive (plotly) FISHMORPH projection figure.
# Moved from intraitR (R/plotly_fishmorph.R) in Rfishmorph 0.17.0: Rfishmorph owns the
# visualisation of the FISHMORPH functional space, intraitR re-exports it.
# Works on any "fishmorph_projection", including the "intrait_fishmorph_projection"
# returned by intraitR::project_fishmorph() (a subclass), whose ITV results
# ($itv_proportion) are shown when present.
# =============================================================================

#' Colour with an alpha channel, in the CSS notation plotly expects
#'
#' plotly's JSON schema takes colours as CSS strings and has no separate
#' opacity field for a fill (`fillcolor`) or a marker outline, so an R colour
#' name / hex code plus an alpha has to be flattened into a single
#' `"rgba(r, g, b, a)"` string. [grDevices::adjustcolor()] would instead
#' return an 8-digit hex (`"#RRGGBBAA"`), which older plotly.js builds ignore
#' silently -- the alpha is dropped and translucent hulls come out opaque, so
#' the conversion is done explicitly here.
#'
#' @param col A single R colour (name or hex).
#' @param alpha Numeric in `[0, 1]`, the opacity.
#' @return A length-1 character string, e.g. `"rgba(78, 121, 167, 0.2)"`.
#' @noRd
.rgba <- function(col, alpha = 1) {
  rgb <- grDevices::col2rgb(col)[, 1]
  sprintf("rgba(%d, %d, %d, %s)", rgb[[1]], rgb[[2]], rgb[[3]],
          format(alpha, digits = 3))
}

#' plotly colorscale from a vector of colours (low to high)
#'
#' plotly expects a colorscale as a list of `list(fraction, colour)` pairs
#' with fractions spanning `[0, 1]`, rather than the plain colour vector
#' [graphics::image()] takes as `col`. This converts the package's
#' density-heatmap ramp (see `.plot_ordination()`) into that form so the
#' static and interactive figures share one palette.
#'
#' @param cols Character vector of colours, low density first.
#' @return A list of two-element lists, ready for a plotly `colorscale`.
#' @noRd
.plotly_colorscale <- function(cols) {
  n <- length(cols)
  if (n == 1) return(list(list(0, cols[1]), list(1, cols[1])))
  lapply(seq_len(n), function(i) list((i - 1) / (n - 1), cols[i]))
}

#' Diameter of a planar point cloud (largest distance between two points)
#'
#' The scale a distance read off an ordination is expressed against. A distance
#' in score units is uninterpretable on its own -- it depends on the traits,
#' on the standardisation and on which components are displayed -- whereas the
#' same distance as a fraction of the largest distance the cloud contains is a
#' dimensionless share of the occupied space, comparable between axis pairs and
#' between figures.
#'
#' The diameter of a planar set is attained by two vertices of its convex hull
#' (any interior or edge point can be pushed outwards along the segment joining
#' the pair), so the quadratic search is run on the hull only. For the ~9,500
#' FISHMORPH species the hull holds a few dozen vertices, which turns a 45
#' million pair computation into a few hundred.
#'
#' @param x,y Numeric vectors of coordinates. Non-finite pairs are dropped.
#' @return A single number, the largest Euclidean distance between two points,
#'   or `NA_real_` when fewer than two distinct points remain (a scale of zero
#'   is not a usable denominator either).
#' @noRd
.cloud_diameter <- function(x, y) {
  # max_pair_distance() is exact (hull-restricted); a zero diameter is not a
  # usable denominator, hence NA here.
  d <- max_pair_distance(x, y)
  if (!is.finite(d) || d <= 0) return(NA_real_)
  d
}

#' Interactive FISHMORPH projection plot (plotly)
#'
#' The interactive counterpart of [plot.fishmorph_projection()]:
#' the same figure -- the reference database as a kernel-density heatmap
#' and/or point cloud, with the projected specimens on top of it, grouped by
#' species -- rendered as a **plotly** htmlwidget instead of a base-graphics
#' device. What the static plot cannot do, and what this is for, is
#' *identifying* points: a ~9,000-species morphospace with a few hundred
#' projected individuals in it has no room for labels, so hovering is the
#' only practical way to ask which specimen (and which trait values) sits at
#' a given position. The legend is clickable, so single species can be
#' isolated from an overplotted cloud without recomputing anything, and the
#' plot can be zoomed into a dense region.
#'
#' @section What is drawn, and in which order:
#' Traces are added back to front: the reference density heatmap and its
#' highest-density-region contour lines, the reference point cloud, the
#' per-species geometry (`style`), the focal species' own reference-database
#' points (`itv_reference`), then the projected specimens. Every trace
#' belonging to one species shares a plotly `legendgroup`, so a click on that
#' species in the legend hides or shows its points *and* its hull/ellipse
#' together (double-click isolates it). The reference layers are outside any
#' legend group and stay visible throughout, since they are the frame of
#' reference the selection is read against.
#'
#' @section Hover information:
#' Each projected point reports its specimen identifier (the row name of the
#' projection's `scores`), its species and its coordinates on the two
#' displayed axes. Adding `hover_traits = TRUE` appends its nine FISHMORPH
#' ratios *on the analysis scale* (i.e. `log10(x + 1)` when
#' [project_fishmorph()] was called with `log_transform = TRUE`), taken from
#' `x$specimen_traits`, so the numbers in the tooltip are directly comparable
#' with the reference values that define the space rather than with the raw
#' ratios; they are off by default, and wrapped three per line when on,
#' because plotly does not wrap hover text and a nine-ratio line covers the
#' morphospace the point is being located in. A hull or
#' ellipse reports its species, its number of specimens and, when the
#' projection carries an `intraitR::itv_proportion()` result, that species' share of
#' the global functional volume. Reference points report the reference
#' species' name when the projection carries labels (`x$global_species`).
#'
#' `hover_distances` (on by default) adds two numbers an ordination cannot be
#' read off by eye: how far a specimen sits from the centroid of its own
#' species -- its contribution to the intraspecific spread -- and how far it
#' sits from the single FISHMORPH morphotype of that species, i.e. the
#' individual the global database represents the whole species by. The second
#' is only shown when a reference point layer is actually drawn
#' (`reference_points` or `itv_reference`), since it is a distance to a point
#' the reader would otherwise not see; the `"spider"` centroid marker reports
#' that same distance for the species mean.
#'
#' Both are reported as a **percentage of the FISHMORPH span** -- the largest
#' distance between two reference species on the same two axes, i.e. the
#' diameter of the global morphospace -- with the value in score units in
#' brackets. A distance in score units is uninterpretable on its own, since it
#' depends on the traits, on the standardisation and on which components are
#' displayed; the same distance as a share of the whole occupied range is
#' dimensionless and can be compared between axis pairs, species and
#' campaigns. They remain plain Euclidean distances **on the two displayed
#' axes**: they are the distances the figure shows (`equal_aspect = TRUE` is
#' what makes them readable), they change when `axes` changes, and they are not
#' distances in the full nine-trait space.
#'
#' @param x An object of class `"fishmorph_projection"` (or the
#'   `"intrait_fishmorph_projection"` subclass made by intraitR), from
#'   [project_fishmorph()].
#' @param style Character, the per-species (foreground) geometry: `"hull"`
#'   (default; convex hull = the ITV footprint), `"spider"` (dispersion
#'   ellipse plus spokes to the centroid), `"density"` (kernel-density
#'   contour of the projected specimens), or `"points"` (no per-group
#'   geometry). As in the static method, this is independent of
#'   `reference_density`, which controls the *background* layer.
#' @param axes Integer vector of length 2, which principal components to
#'   display. `NULL` (default) uses the two axes the projection was built to
#'   plot (`x$axes`); any other pair is taken from the all-component scores
#'   (`x$scores_all`, `x$global_scores_all`) with no refitting, so exploring
#'   further components costs nothing and cannot move the space.
#' @param reference_density Logical, draw the reference database as a filled
#'   kernel-density heatmap (the same white-to-dark-red ramp as the static
#'   plot) with a few nested highest-density-region contour lines. Defaults
#'   to `TRUE`.
#' @param reference_points Logical, draw the reference species as a light
#'   background point cloud. Can be combined with `reference_density`.
#'   Defaults to `FALSE`.
#' @param itv_reference Logical, mark the focal species' own entries in the
#'   reference database, coloured to match their species, so the single
#'   database morphotype can be compared with the spread of the projected
#'   individuals. Matching is case-insensitive and treats spaces and
#'   underscores as equivalent; species with no reference row are skipped
#'   with a warning. Defaults to `FALSE`.
#' @param arrows Logical, overlay the trait loadings (`x$loadings`) as biplot
#'   arrows from the origin, with the trait name at each tip, turning the
#'   score plot into a PCA biplot. Drawn as plotly annotations, so they are
#'   part of the figure but carry no hover information. Defaults to `FALSE`.
#' @param arrow_scale Numeric in `(0, 1]`, how far the longest loading arrow
#'   reaches, as a fraction of the distance from the origin to the nearest
#'   edge of the displayed region; all arrows share that one factor, so their
#'   relative lengths and directions are preserved. Loadings are unit-scaled
#'   vectors with no natural size in score units, so this is a display choice
#'   only. Defaults to `0.8`.
#' @param arrow_col Colour of the loading arrows and their labels. Defaults
#'   to `"grey20"`.
#' @param background Logical master switch: when `FALSE`, no reference layer
#'   of any kind is drawn (overrides `reference_density`,
#'   `reference_points`, `itv_reference`). Defaults to `TRUE`.
#' @param background_col Colour of the reference point cloud. Defaults to
#'   `"grey75"`.
#' @param density_probs Numeric vector of coverage probabilities for the
#'   heatmap's highest-density-region contour lines. Defaults to
#'   `c(0.25, 0.5, 0.99)`.
#' @param density_palette Optional character vector of colours for the
#'   heatmap gradient (low to high density). `NULL` (default) uses the
#'   package's white-to-dark-red ramp.
#' @param select_species,select_specimens Optional character vectors
#'   restricting *this figure* to a subset of the already-projected species
#'   or specimen identifiers, without recomputing the space (the reference
#'   background is unaffected). Combined by intersection. `NULL` (default)
#'   shows everything projected.
#' @param ellipse_level,density_level Coverage probabilities for the
#'   `"spider"` ellipse and the `"density"` contour respectively. Default
#'   `0.95`.
#' @param hover_traits Logical, add each specimen's trait values (on the
#'   analysis scale) to its tooltip. Defaults to `FALSE`: nine ratios on one
#'   line produce a tooltip wider than the plotting region, which hides the
#'   very morphospace the point is being located in. When `TRUE` the values
#'   are wrapped three per line to keep the box narrow.
#' @param hover_distances Logical, add to each specimen's tooltip its
#'   Euclidean distance (on the two displayed axes, as a percentage of the
#'   largest distance between two reference species and in score units) to the
#'   centroid of its species and, when a reference point layer is drawn
#'   (`reference_points` or `itv_reference`), to its species' own FISHMORPH
#'   database point; the `"spider"` centroid marker then also reports its own
#'   distance to that point. The species is matched to the database with the
#'   rule `itv_reference` uses (case-insensitive, spaces and underscores
#'   equivalent) and the distance is omitted for a species the database does
#'   not contain. The centroid is that of the specimens *displayed*, so a
#'   `select_species` / `select_specimens` selection moves it. Defaults to
#'   `TRUE`.
#' @param hover_font_size Numeric, font size (in pixels) of the tooltip text.
#'   Defaults to `10`, smaller than plotly's own default, since a tooltip here
#'   carries an identifier and a species name rather than a single number.
#' @param point_size Numeric, marker size (in pixels) of the projected
#'   specimens. Defaults to `7`.
#' @param fill_alpha Numeric in `[0, 1]`, opacity of the per-species hull /
#'   ellipse / contour fill. Defaults to `0.2`, low enough that overlapping
#'   ITV footprints stay readable. Set to `0` for outlines only.
#' @param equal_aspect Logical, force one score unit on the horizontal axis to
#'   occupy the same number of pixels as one score unit on the vertical axis
#'   (plotly's `scaleanchor`). The two displayed components are in the same
#'   units, so an unequal aspect ratio silently distorts every distance, hull
#'   shape and loading direction read off the figure, and rescales them again
#'   at every zoom; the cost is that a low-variance PC2 then occupies only a
#'   thin horizontal band, as it should. Set to `FALSE` to reproduce the
#'   base-graphics method's behaviour, where each axis is stretched
#'   independently to fill the panel. Defaults to `TRUE`.
#' @param legend Logical, show the (clickable) species legend. Defaults to
#'   `TRUE`.
#' @param legend_title Title above the legend. Defaults to `"Species"`.
#' @param legend_italic Logical, italicise the species names in the legend
#'   and tooltips (as binomials), using the HTML markup plotly renders.
#'   Defaults to `TRUE`.
#' @param abbreviate_species Logical, abbreviate the genus in the legend
#'   (e.g. `"Squalius cephalus"` becomes `"S. cephalus"`). Tooltips always
#'   carry the full name. Defaults to `TRUE`.
#' @param webgl Logical, render the reference point cloud with WebGL
#'   (`"scattergl"`) rather than SVG. A ~9,000-point SVG trace makes the
#'   widget sluggish in a browser and slow to save; WebGL draws it on the GPU
#'   in one pass. Only the reference cloud is affected -- the specimens stay
#'   SVG so their markers and hover boxes render identically to a small
#'   figure. Defaults to `TRUE`.
#' @param title Optional plot title. `NULL` (default) builds one naming the
#'   space and the style, as the static method does.
#' @param width,height Optional widget dimensions in pixels, passed to
#'   [plotly::plot_ly()]. `NULL` (default) lets the container decide.
#'
#' @return A `plotly` htmlwidget (class `c("plotly", "htmlwidget")`). Printed
#'   at the console it opens in the RStudio Viewer or a browser; it can be
#'   embedded in an R Markdown / Quarto document, or written to a
#'   self-contained HTML file with `htmlwidgets::saveWidget()`. The widget's
#'   mode-bar camera button is configured to export SVG rather than PNG, so
#'   the figure taken out of an exploratory session is a vector graphic.
#'
#' @section Requirements:
#' Needs the **plotly** package (in `Suggests`): install it with
#' `install.packages("plotly")`. Only this function requires it; the rest of
#' Rfishmorph is unaffected.
#'
#' @references
#' Brosse, S., Charpin, N., Su, G., Toussaint, A., Herrera-R, G. A.,
#' Tedesco, P. A., & Villeger, S. (2021). FISHMORPH: A global database on
#' morphological traits of freshwater fishes. Global Ecology and
#' Biogeography, 30(11), 2330-2336.
#'
#' Sievert, C. (2020). Interactive Web-Based Data Visualization with R,
#' plotly, and shiny. Chapman and Hall/CRC.
#'
#' @seealso [project_fishmorph()],
#'   [plot.fishmorph_projection()] (the static figure),
#'   [plot_fishmorph_density()] (the one-dimensional marginals),
#'   `intraitR::itv_proportion()`
#'
#' @examples
#' \donttest{
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   ref <- load_fishmorph_reference()
#'   # demo: a few reference rows (already on the log scale) as "specimens";
#'   # field ratios measured with intraitR are projected the same way
#'   sp <- ref[1:40, ]; sp$species <- sp$Family
#'   proj <- project_fishmorph(sp, reference = ref, specimens_prelogged = TRUE)
#'
#'   # footprints over the reference heatmap, hover a point for its traits
#'   plotly_fishmorph(proj)
#'
#'   # biplot arrows + each group's own database point, on axes 1 and 3
#'   plotly_fishmorph(proj, axes = c(1, 3), arrows = TRUE, itv_reference = TRUE)
#'
#'   # raw reference cloud instead of the heatmap, dispersion ellipses
#'   plotly_fishmorph(proj, style = "spider", reference_density = FALSE,
#'                    reference_points = TRUE)
#'
#'   # save as a standalone HTML file
#'   # htmlwidgets::saveWidget(plotly_fishmorph(proj), "fishmorph.html")
#' }
#' }
#'
#' @export
plotly_fishmorph <- function(x,
                             style = c("hull", "spider", "density", "points"),
                             axes = NULL,
                             reference_density = TRUE,
                             reference_points = FALSE,
                             itv_reference = FALSE,
                             arrows = FALSE,
                             arrow_scale = 0.8,
                             arrow_col = "grey20",
                             background = TRUE,
                             background_col = "grey75",
                             density_probs = c(0.25, 0.5, 0.99),
                             density_palette = NULL,
                             select_species = NULL,
                             select_specimens = NULL,
                             ellipse_level = 0.95,
                             density_level = 0.95,
                             hover_traits = FALSE,
                             hover_distances = TRUE,
                             hover_font_size = 10,
                             point_size = 7,
                             fill_alpha = 0.2,
                             equal_aspect = TRUE,
                             legend = TRUE,
                             legend_title = "Species",
                             legend_italic = TRUE,
                             abbreviate_species = TRUE,
                             webgl = TRUE,
                             title = NULL,
                             width = NULL, height = NULL) {
  if (!inherits(x, "fishmorph_projection")) {
    stop("`x` must be a \"fishmorph_projection\" object from project_fishmorph().",
         call. = FALSE)
  }
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly_fishmorph() needs the 'plotly' package: install.packages(\"plotly\").",
         call. = FALSE)
  }
  style <- match.arg(style)
  if (!is.numeric(fill_alpha) || length(fill_alpha) != 1 ||
      fill_alpha < 0 || fill_alpha > 1) {
    stop("`fill_alpha` must be a single number in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(hover_font_size) || length(hover_font_size) != 1 ||
      hover_font_size <= 0) {
    stop("`hover_font_size` must be a single positive number.", call. = FALSE)
  }
  if (isTRUE(arrows) &&
      (!is.numeric(arrow_scale) || length(arrow_scale) != 1 ||
       arrow_scale <= 0 || arrow_scale > 1)) {
    stop("`arrow_scale` must be a single number in (0, 1].", call. = FALSE)
  }

  ## -- which components to display ----------------------------------------
  # Any pair of components can be shown without refitting: the projection
  # stores the specimen and reference scores on *all* components, and the
  # ordination itself is frozen (see project_fishmorph()).
  if (is.null(axes)) axes <- x$axes
  if (length(axes) != 2 || !is.numeric(axes) || any(!is.finite(axes))) {
    stop("`axes` must be a length-2 integer vector.", call. = FALSE)
  }
  axes <- as.integer(axes)
  same_axes <- identical(axes, as.integer(x$axes))
  if (same_axes) {
    sc_all <- x$scores
    gsc_all <- x$global_scores
    var_exp <- unname(x$var_explained)
  } else {
    if (is.null(x$scores_all) || is.null(x$global_scores_all)) {
      stop("Showing other `axes` needs the all-component scores; recompute the ",
           "projection with project_fishmorph() (>= 1.6.0).", call. = FALSE)
    }
    n_comp <- ncol(x$scores_all)
    if (any(axes < 1) || any(axes > n_comp)) {
      stop("`axes` requests a component beyond the ", n_comp, " available.",
           call. = FALSE)
    }
    sc_all <- as.data.frame(x$scores_all[, axes, drop = FALSE])
    gsc_all <- as.data.frame(x$global_scores_all[, axes, drop = FALSE])
    names(sc_all) <- names(gsc_all) <- paste0("PC", axes)
    rownames(sc_all) <- rownames(x$scores_all)
    var_exp <- (x$pca$sdev^2 / sum(x$pca$sdev^2))[axes] * 100
  }
  ax_names <- names(sc_all)

  ## -- selection filters (this figure only; the space is untouched) --------
  gr <- x$groups
  keep <- rep(TRUE, nrow(sc_all))
  if (!is.null(select_species))   keep <- keep & as.character(gr) %in% select_species
  if (!is.null(select_specimens)) keep <- keep & rownames(sc_all) %in% select_specimens
  if (!any(keep)) stop("No projected specimens match the plot selection.", call. = FALSE)
  sc <- sc_all[keep, , drop = FALSE]
  gr <- droplevels(gr[keep])
  ids <- rownames(sc)
  if (is.null(ids)) ids <- paste0("specimen", which(keep))

  ## -- colours: the session-stable per-species palette ---------------------
  # Same lookup as .plot_ordination(), so a species keeps one colour across
  # the static plot, the density panels and this widget.
  sp_levels <- levels(gr)
  # Positional, not by-name, lookup throughout: .stable_group_colors() returns
  # its colours in the order of `unique(levels(gr))`, i.e. exactly
  # `sp_levels`, and `[[` on a *name* cannot retrieve an element named `""`
  # (see ?Extract), which a blank/unresolved species identification stored as
  # an empty string would otherwise trigger.
  label_colors <- unname(.stable_group_colors(sp_levels))
  legend_labels <- sp_levels
  if (isTRUE(abbreviate_species)) {
    legend_labels <- .abbreviate_species_name(legend_labels)
  }
  if (isTRUE(legend_italic)) {
    legend_labels <- paste0("<i>", legend_labels, "</i>")
  }

  ## -- which reference layers -----------------------------------------------
  show_ref <- isTRUE(background)
  draw_density <- show_ref && isTRUE(reference_density)
  draw_points  <- show_ref && isTRUE(reference_points)

  ## -- the two distances carried by the tooltips ---------------------------
  # Species name normalisation, shared with the `itv_reference` layer below:
  # the campaign writes "Squalius_cephalus", the database "Squalius cephalus",
  # and the two must resolve to one species or the distance would be missing
  # for purely typographic reasons.
  norm_sp <- function(s) tolower(gsub("[ _]+", " ", trimws(as.character(s))))

  # Per-species centroid of the DISPLAYED specimens (`sc`, i.e. after
  # select_species / select_specimens), so the number in a tooltip describes
  # the cloud on screen and coincides with the diamond the "spider" style
  # draws. Computed once here rather than inside the style branch, because
  # every style now needs it while only "spider" plots it.
  cent <- vapply(sp_levels, function(g) {
    idx <- which(as.character(gr) == g)
    if (length(idx) == 0) return(c(NA_real_, NA_real_))
    c(mean(sc[[1]][idx], na.rm = TRUE), mean(sc[[2]][idx], na.rm = TRUE))
  }, numeric(2))

  # The species' own row in the reference database. match() takes the first
  # row of a species and returns NA for a species the database does not hold,
  # which is what makes the distance absent rather than wrong.
  ref_xy <- matrix(NA_real_, nrow = 2, ncol = length(sp_levels))
  if (isTRUE(hover_distances) && !is.null(x$global_species)) {
    mi <- match(norm_sp(sp_levels), norm_sp(x$global_species))
    ref_xy[1, ] <- gsc_all[[1]][mi]
    ref_xy[2, ] <- gsc_all[[2]][mi]
  }
  # A distance to a point that is not drawn invites the reader to check it
  # against a figure it cannot be seen on, so the reference distance follows
  # the reference point layers.
  show_refdist <- isTRUE(hover_distances) && show_ref &&
    (draw_points || isTRUE(itv_reference))

  # The yardstick: the largest distance between two FISHMORPH species on the
  # displayed axes, i.e. the span of the global morphospace itself. A distance
  # in score units means nothing to a reader (it depends on the traits, the
  # standardisation and the axis pair); the same distance as a percentage of
  # that span is a share of the world's morphological range, and it can be
  # compared between axis pairs, species and campaigns. The absolute value is
  # kept in brackets, since a percentage of an unstated quantity is not a
  # measurement.
  ref_diam <- if (isTRUE(hover_distances))
    .cloud_diameter(gsc_all[[1]], gsc_all[[2]]) else NA_real_
  has_scale <- is.finite(ref_diam)
  fmt_d <- function(d) {
    if (has_scale) sprintf("%.1f%% (%.3f)", 100 * d / ref_diam, d)
    else sprintf("%.3f", d)
  }
  dist_head <- if (has_scale)
    "distances, in % of the FISHMORPH span:" else "distances, in score units:"

  xlab <- sprintf("%s (%.1f%%)", ax_names[1], var_exp[1])
  ylab <- sprintf("%s (%.1f%%)", ax_names[2], var_exp[2])
  style_label <- switch(style, spider = "spider", hull = "convex hull",
                        density = "density", points = "points")
  if (is.null(title)) {
    title <- sprintf("FISHMORPH space (%s) -- %d specimens, %d species over %d reference species",
                     style_label, nrow(sc), nlevels(gr), x$n_reference)
  }

  p <- plotly::plot_ly(width = width, height = height)

  ## ---------------------------------------------------------------------
  ## 1. reference density heatmap + HDR contour lines
  ## ---------------------------------------------------------------------
  density_cols <- if (is.null(density_palette)) {
    grDevices::colorRampPalette(
      c("#FFFFFF", "#FFEDA0", "#FEB24C", "#FC8D59",
        "#EF6548", "#D7301F", "#990000", "#7F0000")
    )(64)
  } else {
    density_palette
  }
  if (draw_density) {
    fld <- .density_field(gsc_all[[1]], gsc_all[[2]], probs = density_probs)
    if (!is.null(fld)) {
      # .density_field()/.kde2d() return z[i, j] = density at (x[i], y[j]),
      # the orientation graphics::image() wants; plotly's heatmap indexes
      # z[row, col] = (y, x), hence the transpose.
      p <- plotly::add_trace(
        p, type = "heatmap", x = fld$x, y = fld$y, z = t(fld$z),
        colorscale = .plotly_colorscale(density_cols), zsmooth = "best",
        showscale = FALSE, hoverinfo = "skip", showlegend = FALSE,
        name = "Reference density"
      )
      # HDR contour lines are drawn as ordinary polylines rather than a
      # plotly contour trace: the levels come from the enclosed probability
      # mass (see .density_field()) and are irregularly spaced, which
      # plotly's start/end/size contour specification cannot express.
      if (length(fld$levels) > 0) {
        for (lev in fld$levels) {
          lines <- grDevices::contourLines(fld$x, fld$y, fld$z, levels = lev)
          for (ln in lines) {
            p <- plotly::add_trace(
              p, type = "scatter", mode = "lines", x = ln$x, y = ln$y,
              line = list(color = "#3a0000", width = 0.8),
              hoverinfo = "skip", showlegend = FALSE, name = "HDR contour"
            )
          }
        }
      }
    }
  }

  ## ---------------------------------------------------------------------
  ## 2. reference point cloud
  ## ---------------------------------------------------------------------
  if (draw_points) {
    ref_txt <- if (!is.null(x$global_species)) {
      paste0("Reference: <i>", x$global_species, "</i>")
    } else {
      rep("Reference species", nrow(gsc_all))
    }
    p <- plotly::add_trace(
      p, type = if (isTRUE(webgl)) "scattergl" else "scatter", mode = "markers",
      x = gsc_all[[1]], y = gsc_all[[2]],
      marker = list(size = max(point_size - 4, 2),
                    color = .rgba(background_col, 0.5)),
      text = ref_txt, hoverinfo = "text",
      name = "FISHMORPH reference", showlegend = isTRUE(legend)
    )
  }

  ## ---------------------------------------------------------------------
  ## 3. per-species geometry (the ITV footprint)
  ## ---------------------------------------------------------------------
  # Per-species share of the global functional volume, for the hull/ellipse
  # tooltip, when the projection carries an itv_proportion() result.
  vol_prop <- NULL
  if (!is.null(x$itv_proportion) && !is.null(x$itv_proportion$volume)) {
    v <- x$itv_proportion$volume
    vol_prop <- stats::setNames(v$proportion, v$group)
  }
  geom_x <- numeric(0)
  geom_y <- numeric(0)

  for (li in seq_along(sp_levels)) {
    g <- sp_levels[li]
    idx <- which(as.character(gr) == g)
    if (length(idx) == 0) next
    gx <- sc[[1]][idx]
    gy <- sc[[2]][idx]
    col_g <- label_colors[li]
    lab_g <- legend_labels[li]

    prop_txt <- ""
    if (!is.null(vol_prop)) {
      mi <- match(g, names(vol_prop))
      if (!is.na(mi) && is.finite(vol_prop[mi])) {
        prop_txt <- sprintf("<br>ITV / global volume: %.2f%%", 100 * vol_prop[mi])
      }
    }
    hover_g <- sprintf("<i>%s</i><br>n = %d%s", g, length(idx), prop_txt)

    polys <- list()
    if (style == "hull" && length(idx) >= 3) {
      hp <- grDevices::chull(gx, gy)
      polys <- list(list(x = gx[hp], y = gy[hp]))
    } else if (style == "spider") {
      ell <- .covariance_ellipse(gx, gy, level = ellipse_level)
      if (!is.null(ell)) polys <- list(list(x = ell[, 1], y = ell[, 2]))
    } else if (style == "density") {
      ct <- .density_contour(gx, gy, level = density_level)
      if (!is.null(ct)) polys <- lapply(ct, function(pl) list(x = pl$x, y = pl$y))
    }

    for (poly in polys) {
      # Close the ring explicitly: chull()/contourLines() return the vertices
      # only, and plotly's "toself" fill would otherwise close it with a
      # straight chord drawn *through* the cloud.
      px <- c(poly$x, poly$x[1])
      py <- c(poly$y, poly$y[1])
      geom_x <- c(geom_x, px)
      geom_y <- c(geom_y, py)
      p <- plotly::add_trace(
        p, type = "scatter", mode = "lines", x = px, y = py,
        fill = if (fill_alpha > 0) "toself" else "none",
        fillcolor = .rgba(col_g, fill_alpha),
        line = list(color = col_g, width = 1.6),
        text = hover_g, hoverinfo = "text",
        hoveron = if (fill_alpha > 0) "points+fills" else "points",
        legendgroup = g, showlegend = FALSE, name = lab_g
      )
    }

    # "spider" additionally draws spokes from every specimen to the group
    # centroid. All spokes go into a single trace, segments separated by NA,
    # so a species costs one trace rather than one per specimen.
    if (style == "spider" && length(idx) >= 2) {
      cx <- cent[1, li]; cy <- cent[2, li]
      seg_x <- as.vector(rbind(gx, rep(cx, length(gx)), rep(NA_real_, length(gx))))
      seg_y <- as.vector(rbind(gy, rep(cy, length(gy)), rep(NA_real_, length(gy))))
      p <- plotly::add_trace(
        p, type = "scatter", mode = "lines", x = seg_x, y = seg_y,
        line = list(color = .rgba(col_g, 0.5), width = 0.8),
        hoverinfo = "skip", legendgroup = g, showlegend = FALSE,
        name = paste0(lab_g, " (spokes)")
      )
      # The centroid is the species' mean morphology as this campaign measured
      # it; the database point is the single individual FISHMORPH represents
      # the same species by. The distance between the two is therefore the
      # figure's reading of how far the sample sits from the global record,
      # and it belongs on the marker rather than in a separate table.
      cent_txt <- sprintf("%s<br>centroid", hover_g)
      if (show_refdist && is.finite(ref_xy[1, li])) {
        cent_txt <- paste0(
          cent_txt, "<br>", dist_head, "<br>to its FISHMORPH point: ",
          fmt_d(sqrt((cx - ref_xy[1, li])^2 + (cy - ref_xy[2, li])^2)))
      }
      p <- plotly::add_trace(
        p, type = "scatter", mode = "markers", x = cx, y = cy,
        marker = list(size = point_size + 3, color = col_g, symbol = "diamond",
                      line = list(color = "black", width = 1)),
        text = cent_txt, hoverinfo = "text",
        legendgroup = g, showlegend = FALSE,
        name = paste0(lab_g, " (centroid)")
      )
    }
  }

  ## ---------------------------------------------------------------------
  ## 4. focal species' own reference-database points
  ## ---------------------------------------------------------------------
  if (show_ref && isTRUE(itv_reference)) {
    gs <- x$global_species
    if (is.null(gs)) {
      warning("`itv_reference = TRUE` needs reference species labels; recompute ",
              "the projection with project_fishmorph() (>= 1.6.0).", call. = FALSE)
    } else {
      norm <- norm_sp
      focal <- sp_levels
      gs_norm <- norm(gs)
      match_idx <- which(gs_norm %in% norm(focal))
      unmatched <- setdiff(norm(focal), gs_norm)
      if (length(unmatched) > 0) {
        warning("`itv_reference`: no reference row for species: ",
                paste(focal[norm(focal) %in% unmatched], collapse = ", "),
                call. = FALSE)
      }
      focal_norm <- norm(focal)
      for (k in match_idx) {
        li <- match(gs_norm[k], focal_norm)
        g <- if (is.na(li)) "" else focal[li]
        col_g <- if (is.na(li)) "black" else label_colors[li]
        lab_g <- if (is.na(li)) "reference" else legend_labels[li]
        p <- plotly::add_trace(
          p, type = "scatter", mode = "markers",
          x = gsc_all[[1]][k], y = gsc_all[[2]][k],
          marker = list(size = point_size + 4, color = col_g, symbol = "circle",
                        line = list(color = "black", width = 1.4)),
          text = sprintf("<i>%s</i><br>FISHMORPH database point<br>%s: %.3f<br>%s: %.3f",
                         gs[k], ax_names[1], gsc_all[[1]][k],
                         ax_names[2], gsc_all[[2]][k]),
          hoverinfo = "text", legendgroup = g, showlegend = FALSE,
          name = paste0(lab_g, " (reference)")
        )
      }
    }
  }

  ## ---------------------------------------------------------------------
  ## 5. the projected specimens (one trace per species = clickable legend)
  ## ---------------------------------------------------------------------
  tr_mat <- if (isTRUE(hover_traits)) x$specimen_traits else NULL
  for (li in seq_along(sp_levels)) {
    g <- sp_levels[li]
    idx <- which(as.character(gr) == g)
    if (length(idx) == 0) next
    col_g <- label_colors[li]
    txt <- sprintf("<b>%s</b><br><i>%s</i><br>%s: %.3f<br>%s: %.3f",
                   ids[idx], g,
                   ax_names[1], sc[[1]][idx], ax_names[2], sc[[2]][idx])
    # Where this individual sits relative to the two points the figure is read
    # against: its own species' mean, and its species' entry in the global
    # database. Placed before the trait values, so the two numbers stay on the
    # first lines of the tooltip when `hover_traits` is on.
    if (isTRUE(hover_distances)) {
      txt <- paste0(
        txt, "<br>", dist_head, "<br>to the centroid: ",
        fmt_d(sqrt((sc[[1]][idx] - cent[1, li])^2 +
                     (sc[[2]][idx] - cent[2, li])^2)))
      if (show_refdist && is.finite(ref_xy[1, li])) {
        txt <- paste0(
          txt, "<br>to its FISHMORPH point: ",
          fmt_d(sqrt((sc[[1]][idx] - ref_xy[1, li])^2 +
                       (sc[[2]][idx] - ref_xy[2, li])^2)))
      }
    }
    if (!is.null(tr_mat)) {
      # Trait values on the analysis scale, matched by specimen identifier
      # (specimen_traits is aligned to the *unfiltered* projection, so it is
      # indexed by name rather than by this figure's positions).
      rows <- match(ids[idx], rownames(tr_mat))
      if (all(!is.na(rows))) {
        trait_txt <- apply(tr_mat[rows, , drop = FALSE], 1, function(v) {
          items <- sprintf("%s = %.3f", colnames(tr_mat), v)
          # Wrapped three per line: nine ratios on a single line make the
          # tooltip wider than the plotting region (plotly does not wrap
          # hover text itself), which hides the morphospace the point is
          # being located in.
          lines <- split(items, ceiling(seq_along(items) / 3))
          paste(vapply(lines, paste, character(1), collapse = ", "),
                collapse = "<br>")
        })
        txt <- paste0(txt, "<br>", trait_txt)
      }
    }
    p <- plotly::add_trace(
      p, type = "scatter", mode = "markers", x = sc[[1]][idx], y = sc[[2]][idx],
      marker = list(size = point_size, color = .rgba(col_g, 0.8),
                    line = list(color = col_g, width = 0.8)),
      text = txt, hoverinfo = "text",
      legendgroup = g, showlegend = isTRUE(legend), name = legend_labels[li]
    )
  }

  ## ---------------------------------------------------------------------
  ## 6. layout: axes through the origin, clickable legend, biplot arrows
  ## ---------------------------------------------------------------------
  # Displayed extent, needed both to keep the reference cloud in frame and to
  # scale the loading arrows (which have no natural length in score units).
  all_x <- c(sc[[1]], geom_x,
             if (draw_density || draw_points) gsc_all[[1]] else numeric(0))
  all_y <- c(sc[[2]], geom_y,
             if (draw_density || draw_points) gsc_all[[2]] else numeric(0))
  xr <- range(all_x[is.finite(all_x)])
  yr <- range(all_y[is.finite(all_y)])
  xpad <- diff(xr) * 0.06
  ypad <- diff(yr) * 0.06
  if (!is.finite(xpad) || xpad == 0) xpad <- 1
  if (!is.finite(ypad) || ypad == 0) ypad <- 1
  xlim <- xr + c(-xpad, xpad)
  ylim <- yr + c(-ypad, ypad)

  annotations <- list()
  if (isTRUE(arrows)) {
    load <- x$loadings[, axes, drop = FALSE]
    lens <- sqrt(rowSums(load^2))
    max_len <- max(lens, na.rm = TRUE)
    edge <- min(abs(xlim), abs(ylim))
    if (is.finite(max_len) && max_len > 0 && is.finite(edge) && edge > 0) {
      fac <- arrow_scale * edge / max_len
      annotations <- lapply(seq_len(nrow(load)), function(i) {
        list(x = load[i, 1] * fac, y = load[i, 2] * fac,
             ax = 0, ay = 0, axref = "x", ayref = "y",
             xref = "x", yref = "y",
             text = rownames(load)[i], showarrow = TRUE,
             arrowhead = 2, arrowsize = 1, arrowwidth = 1.5,
             arrowcolor = arrow_col,
             font = list(color = arrow_col, size = 11, family = "serif"),
             standoff = 2)
      })
    }
  }

  # Isometric axes (`equal_aspect`): the two components are in the same score
  # units, so an unequal aspect ratio silently distorts every distance, hull
  # shape and arrow direction read off the figure. Set to FALSE to let a
  # low-variance PC2 fill the panel height instead, as the base-graphics
  # method does. Built by assignment rather than inline, because
  # `list(scaleanchor = NULL)` would still create a NULL element and send
  # `"scaleanchor": null` to plotly.js.
  yaxis_spec <- list(title = ylab, range = ylim, zeroline = TRUE,
                     zerolinecolor = "grey60", zerolinewidth = 1,
                     ticks = "inside")
  if (isTRUE(equal_aspect)) {
    yaxis_spec$scaleanchor <- "x"
    yaxis_spec$scaleratio <- 1
  }

  p <- plotly::layout(
    p,
    # The title sits above the plot area, where plotly also draws the mode bar:
    # a reserved top margin keeps the two from overlapping (the mode bar is
    # drawn over the paper, not laid out with it).
    title = list(text = title, font = list(size = 12), y = 0.98),
    margin = list(t = 60),
    # A tooltip here carries an identifier and a species name, not a single
    # number, so it is set smaller than plotly's default; `namelength = -1`
    # stops the trace name from being truncated to 15 characters and `align`
    # keeps multi-line text left-aligned rather than centred.
    hoverlabel = list(font = list(size = hover_font_size), align = "left",
                      namelength = -1),
    xaxis = list(title = xlab, range = xlim, zeroline = TRUE,
                 zerolinecolor = "grey60", zerolinewidth = 1, ticks = "inside"),
    yaxis = yaxis_spec,
    hovermode = "closest",
    showlegend = isTRUE(legend),
    # itemclick "toggle" hides one species, itemdoubleclick "toggleothers"
    # isolates it -- the two gestures that make an overplotted cloud
    # readable without recomputing the projection.
    legend = list(title = list(text = paste0("<b>", legend_title, "</b>"),
                               font = list(size = 11)),
                  font = list(size = 10),
                  itemclick = "toggle", itemdoubleclick = "toggleothers",
                  bgcolor = "rgba(255, 255, 255, 0.6)"),
    annotations = annotations,
    plot_bgcolor = "white", paper_bgcolor = "white"
  )

  # The mode bar's camera button is the practical route from an exploratory
  # widget to a figure: default it to SVG (vector, so the export is
  # publication-usable rather than a screenshot of the browser's raster).
  plotly::config(
    p, displaylogo = FALSE,
    toImageButtonOptions = list(format = "svg", filename = "fishmorph_space")
  )
}
