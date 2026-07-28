# =============================================================================
# project-fishmorph.R -- port of intraitR::project_fishmorph() and its plot
# method, which produce the FISHMORPH functional-space figure (reference density
# heatmap + per-species hulls/points + species legend).
#
# Faithful to intraitR except: (1) the result has class "fishmorph_projection"
# (to avoid S3 clashes when both packages are loaded); (2) the ITV-proportion
# tail (itv_proportion()) is not computed here -- it is a separate analysis, not
# needed for the figure. Everything about the ordination and the plot is
# identical to intraitR.
# =============================================================================

#' Project specimens into the global FISHMORPH functional space
#'
#' Builds a fixed functional trait space by PCA of a reference database of
#' FISHMORPH ratios (typically the full FISHMORPH database, ~9,000 species), then
#' places new specimens into that same, frozen space with [stats::predict()],
#' without re-estimating the ordination. Faithful port of
#' `intraitR::project_fishmorph()`.
#'
#' @param specimens A data frame (e.g. from [fishmorph_ratios()]) with one row
#'   per specimen and at least the `traits` columns, plus a `species` column or a
#'   `groups` argument for colouring.
#' @param reference The reference trait table: a data frame/matrix with the
#'   `traits` columns, or a path to a delimited file (`;`-separated read with
#'   [utils::read.csv2()], `,`-separated with [utils::read.csv()]). If `NULL`
#'   (default), the bundled reference ([load_fishmorph_reference()]) is used,
#'   for the campaign named by `source`.
#' @param source Which bundled reference to use when `reference` is `NULL`:
#'   `"segment"` or `"landmark"`. `NULL` (default) follows
#'   `getOption("fishmorph.source", "segment")`, see [set_fishmorph_source()].
#'   The ordination is always fitted on whichever table is supplied, so
#'   switching `source` refits the space rather than reprojecting into the
#'   other one -- scores from the two campaigns are not comparable term by term.
#' @param traits Character vector of trait columns present on both sides.
#'   Defaults to the nine FISHMORPH ratios.
#' @param groups Optional grouping vector (one per specimen); defaults to a
#'   `species` column of `specimens`.
#' @param select_species,select_specimens Optional filters on the projected
#'   specimens.
#' @param reference_prelogged,specimens_prelogged Whether each input is already
#'   `log10(x + 1)`-transformed. The FISHMORPH database is distributed
#'   pre-logged (`reference_prelogged = TRUE`); [fishmorph_ratios()] returns raw
#'   ratios (`specimens_prelogged = FALSE`).
#' @param log_transform Apply `log10(x + 1)` to bring raw inputs onto the
#'   reference scale (default `TRUE`).
#' @param scale Standardise traits to unit variance in the PCA (default `TRUE`).
#' @param axes Length-2 integer vector of ordination axes (default `c(1, 2)`).
#' @param na_action `"omit"` (default) drops specimens with missing traits;
#'   `"fail"` errors.
#' @return An object of class `"fishmorph_projection"` with `scores`,
#'   `global_scores`, `global_species`, `groups`, `var_explained`, `loadings`,
#'   `axes`, `traits`, `n_reference` and `pca`. Has [print()] and [plot()]
#'   methods.
#' @references Brosse et al. (2021) *Global Ecology and Biogeography* 30:2330-2336.
#' @seealso [plot.fishmorph_projection()], [fishmorph_ratios()],
#'   [load_fishmorph_reference()]
#' @examples
#' \donttest{
#' ref <- load_fishmorph_reference()
#' # a handful of focal specimens (here, reuse some reference rows as demo)
#' sp <- ref[1:40, ]; sp$species <- sp$Family
#' proj <- project_fishmorph(sp, reference = ref, specimens_prelogged = TRUE)
#' plot(proj, style = "hull")
#' }
#' @export
project_fishmorph <- function(specimens, reference = NULL,
                              source = NULL,
                              traits = fishmorph_ratio_names(),
                              groups = NULL,
                              select_species = NULL, select_specimens = NULL,
                              reference_prelogged = TRUE,
                              specimens_prelogged = FALSE,
                              log_transform = TRUE, scale = TRUE,
                              axes = c(1, 2),
                              na_action = c("omit", "fail")) {
  na_action <- match.arg(na_action)
  if (length(axes) != 2 || !is.numeric(axes))
    stop("`axes` must be a length-2 integer vector.", call. = FALSE)

  if (is.null(reference)) reference <- load_fishmorph_reference(source = source)
  if (is.character(reference) && length(reference) == 1) {
    if (!file.exists(reference))
      stop("`reference` file does not exist: ", reference, call. = FALSE)
    sep_semicolon <- grepl(";", readLines(reference, n = 1))
    reference <- if (sep_semicolon)
      utils::read.csv2(reference, dec = ".", stringsAsFactors = FALSE, check.names = TRUE)
    else utils::read.csv(reference, stringsAsFactors = FALSE, check.names = TRUE)
  }
  if (!is.data.frame(reference) && !is.matrix(reference))
    stop("`reference` must be a data.frame, a matrix, or a file path.", call. = FALSE)
  reference <- as.data.frame(reference)

  miss_ref <- setdiff(traits, names(reference))
  if (length(miss_ref) > 0)
    stop("`reference` is missing trait column(s): ", paste(miss_ref, collapse = ", "),
         call. = FALSE)
  specimens_df <- as.data.frame(specimens)
  miss_sp <- setdiff(traits, names(specimens_df))
  if (length(miss_sp) > 0)
    stop("`specimens` is missing trait column(s): ", paste(miss_sp, collapse = ", "),
         ". Compute them with fishmorph_ratios().", call. = FALSE)

  Xref <- data.matrix(reference[traits])
  ref_species <- if ("Species" %in% names(reference))
    as.character(reference[["Species"]]) else rownames(reference)
  ok_ref <- stats::complete.cases(Xref)
  if (any(!ok_ref)) {
    message(sprintf("Dropping %d reference row(s) with missing trait values.", sum(!ok_ref)))
    Xref <- Xref[ok_ref, , drop = FALSE]
    if (!is.null(ref_species)) ref_species <- ref_species[ok_ref]
  }
  if (nrow(Xref) < 3) stop("Fewer than 3 complete reference rows.", call. = FALSE)
  if (any(!is.finite(Xref))) stop("Non-finite value(s) in the reference trait matrix.", call. = FALSE)
  if (log_transform && !reference_prelogged) {
    if (any(Xref < 0)) stop("`log_transform = TRUE` needs non-negative reference values.", call. = FALSE)
    Xref <- log10(Xref + 1)
  }
  pca <- stats::prcomp(Xref, center = TRUE, scale. = scale)
  if (max(axes) > ncol(pca$rotation))
    stop("`axes` requests a component beyond the ", ncol(pca$rotation), " available.", call. = FALSE)
  var_explained <- (pca$sdev^2 / sum(pca$sdev^2))[axes] * 100
  global_scores <- as.data.frame(pca$x[, axes, drop = FALSE])
  names(global_scores) <- paste0("PC", axes)

  if (is.null(groups) && "species" %in% names(specimens_df)) groups <- specimens_df$species
  if (is.null(groups))
    stop("No `groups` supplied and `specimens` has no `species` column.", call. = FALSE)
  if (length(groups) != nrow(specimens_df))
    stop("`groups` must have one entry per row of `specimens`.", call. = FALSE)
  groups <- as.character(groups)

  keep <- rep(TRUE, nrow(specimens_df))
  if (!is.null(select_species)) {
    unmatched <- setdiff(select_species, unique(groups))
    if (length(unmatched) > 0)
      warning("`select_species` not found among specimens: ",
              paste(unmatched, collapse = ", "), call. = FALSE)
    keep <- keep & groups %in% select_species
  }
  if (!is.null(select_specimens)) {
    ids <- rownames(specimens_df)
    unmatched <- setdiff(select_specimens, ids)
    if (length(unmatched) > 0)
      warning("`select_specimens` not found: ",
              paste(utils::head(unmatched, 5), collapse = ", "),
              if (length(unmatched) > 5) ", ..." else "", call. = FALSE)
    keep <- keep & ids %in% select_specimens
  }
  if (!any(keep)) stop("No specimens left after the selection filter(s).", call. = FALSE)
  specimens_df <- specimens_df[keep, , drop = FALSE]; groups <- groups[keep]

  Xsp <- data.matrix(specimens_df[traits])
  incomplete <- !stats::complete.cases(Xsp)
  if (any(incomplete)) {
    if (na_action == "fail")
      stop(sum(incomplete), " specimen(s) have missing trait value(s); set na_action = \"omit\".",
           call. = FALSE)
    message(sprintf("na_action = \"omit\": dropping %d specimen(s) with missing trait values.",
                    sum(incomplete)))
    Xsp <- Xsp[!incomplete, , drop = FALSE]
    specimens_df <- specimens_df[!incomplete, , drop = FALSE]; groups <- groups[!incomplete]
  }
  if (nrow(Xsp) == 0) stop("No specimens with complete trait values to project.", call. = FALSE)
  if (any(!is.finite(Xsp))) stop("Non-finite value(s) in the specimen trait matrix.", call. = FALSE)
  if (log_transform && !specimens_prelogged) {
    if (any(Xsp < 0)) stop("`log_transform = TRUE` needs non-negative specimen values.", call. = FALSE)
    Xsp <- log10(Xsp + 1)
  }
  proj_all <- stats::predict(pca, newdata = Xsp)
  rownames(proj_all) <- rownames(specimens_df)
  scores <- as.data.frame(proj_all[, axes, drop = FALSE])
  names(scores) <- paste0("PC", axes); rownames(scores) <- rownames(specimens_df)

  structure(list(
    scores = scores, scores_all = proj_all,
    global_scores = global_scores, global_scores_all = pca$x,
    global_species = ref_species, groups = factor(groups),
    var_explained = stats::setNames(var_explained, names(scores)),
    loadings = pca$rotation, axes = axes, traits = traits,
    n_reference = nrow(Xref), pca = pca),
    class = "fishmorph_projection")
}

#' @export
print.fishmorph_projection <- function(x, ...) {
  cat("<fishmorph_projection>\n")
  cat(sprintf("  Reference space: %d species, %d traits (%s)\n",
              x$n_reference, length(x$traits), paste(x$traits, collapse = ", ")))
  cat(sprintf("  Axes %s/%s, variance explained: %.1f%% / %.1f%%\n",
              names(x$scores)[1], names(x$scores)[2],
              x$var_explained[1], x$var_explained[2]))
  cat(sprintf("  %d specimen(s) projected across %d species\n",
              nrow(x$scores), nlevels(x$groups)))
  tb <- table(x$groups)
  for (g in names(tb)) cat(sprintf("    %-28s n = %d\n", g, tb[[g]]))
  invisible(x)
}

#' Plot specimens projected into the global FISHMORPH space
#'
#' Draws the reference database as a kernel-density heatmap with the projected
#' specimens on top, coloured by species, with per-species convex hulls (or
#' spider/density/points). Faithful port of
#' `intraitR:::plot.intrait_fishmorph_projection()`.
#'
#' @param x A `fishmorph_projection` object.
#' @param style `"hull"` (default), `"spider"`, `"density"` or `"points"`.
#' @param reference_density Draw the reference density heatmap (default `TRUE`).
#' @param reference_points Draw the reference as a light point cloud (default
#'   `FALSE`).
#' @param itv_reference Mark the focal species' own reference-database points
#'   (default `FALSE`).
#' @param arrows Overlay trait loading arrows (biplot; default `FALSE`).
#' @param arrow_scale,arrow_col Loading-arrow length fraction and colour.
#' @param background Master switch for all reference layers (default `TRUE`).
#' @param background_col Reference point-cloud colour.
#' @param density_probs,density_palette Heatmap contour probabilities / palette.
#' @param select_species,select_specimens Restrict the plot to a subset.
#' @param ellipse_level,density_level Coverage for `"spider"`/`"density"`.
#' @param legend,legend_position,legend_title,legend_italic,abbreviate_species
#'   Legend controls (defaults tuned for species names).
#' @param ... Passed to [graphics::plot()].
#' @return Invisibly `x`.
#' @export
plot.fishmorph_projection <- function(x,
                                      style = c("hull", "spider", "density", "points"),
                                      reference_density = TRUE,
                                      reference_points = FALSE,
                                      itv_reference = FALSE,
                                      arrows = FALSE, arrow_scale = 0.8,
                                      arrow_col = "grey20",
                                      background = TRUE, background_col = "grey75",
                                      density_probs = c(0.25, 0.5, 0.99),
                                      density_palette = NULL,
                                      select_species = NULL, select_specimens = NULL,
                                      ellipse_level = 0.95, density_level = 0.95,
                                      legend = TRUE, legend_position = "outside",
                                      legend_title = "Species", legend_italic = TRUE,
                                      abbreviate_species = TRUE, ...) {
  style <- match.arg(style)
  internal_style <- if (identical(style, "points")) "none" else style
  if (isTRUE(arrows) && (!is.numeric(arrow_scale) || length(arrow_scale) != 1 ||
                         arrow_scale <= 0 || arrow_scale > 1))
    stop("`arrow_scale` must be a single number in (0, 1].", call. = FALSE)

  sc <- x$scores; gr <- x$groups; keep <- rep(TRUE, nrow(sc))
  if (!is.null(select_species))   keep <- keep & as.character(gr) %in% select_species
  if (!is.null(select_specimens)) keep <- keep & rownames(sc) %in% select_specimens
  if (!any(keep)) stop("No projected specimens match the plot selection.", call. = FALSE)
  sc <- sc[keep, , drop = FALSE]; gr <- droplevels(gr[keep])

  xlab <- sprintf("%s (%.1f%%)", names(x$scores)[1], x$var_explained[1])
  ylab <- sprintf("%s (%.1f%%)", names(x$scores)[2], x$var_explained[2])

  show_ref <- isTRUE(background)
  draw_density <- show_ref && isTRUE(reference_density)
  draw_points  <- show_ref && isTRUE(reference_points)
  bg <- if (draw_density || draw_points) x$global_scores else NULL

  hl <- NULL; hl_col <- "black"
  if (show_ref && isTRUE(itv_reference)) {
    gs <- x$global_species
    if (is.null(gs)) {
      warning("`itv_reference = TRUE` needs reference species labels.", call. = FALSE)
    } else {
      norm <- function(s) tolower(gsub("[ _]+", " ", trimws(as.character(s))))
      focal <- levels(gr); gs_norm <- norm(gs)
      match_idx <- which(gs_norm %in% norm(focal))
      unmatched <- setdiff(norm(focal), gs_norm)
      if (length(unmatched) > 0)
        warning("`itv_reference`: no reference row for species: ",
                paste(focal[norm(focal) %in% unmatched], collapse = ", "), call. = FALSE)
      if (length(match_idx) > 0) {
        hl <- x$global_scores[match_idx, , drop = FALSE]
        label_colors <- .stable_group_colors(levels(gr))
        focal_norm <- norm(names(label_colors))
        hl_col <- unname(label_colors[match(gs_norm[match_idx], focal_norm)])
        hl_col[is.na(hl_col)] <- "black"
      }
    }
  }

  .plot_ordination(sc, gr, xlab, ylab, style = internal_style,
                   ellipse_level = ellipse_level, density_level = density_level,
                   legend = legend, legend_position = legend_position,
                   legend_title = legend_title, legend_italic = legend_italic,
                   abbreviate_species = abbreviate_species,
                   space_name = "FISHMORPH space",
                   background = bg, background_col = background_col,
                   background_density = draw_density, background_points = draw_points,
                   density_probs = density_probs, density_palette = density_palette,
                   highlight = hl, highlight_col = hl_col, ...)

  if (isTRUE(arrows)) {
    load <- x$loadings[, x$axes, drop = FALSE]
    trait_names <- rownames(load); lens <- sqrt(rowSums(load^2))
    max_len <- max(lens, na.rm = TRUE); usr <- graphics::par("usr")
    edge <- min(abs(usr[1]), abs(usr[2]), abs(usr[3]), abs(usr[4]))
    if (is.finite(max_len) && max_len > 0 && is.finite(edge) && edge > 0) {
      fac <- arrow_scale * edge / max_len
      ax <- load[, 1] * fac; ay <- load[, 2] * fac
      graphics::arrows(0, 0, ax, ay, length = 0.07, angle = 20,
                       col = arrow_col, lwd = 1.5)
      graphics::text(ax * 1.08, ay * 1.08, labels = trait_names,
                     col = arrow_col, cex = 0.75, font = 3, xpd = NA)
    }
  }
  invisible(x)
}
