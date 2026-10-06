# =============================================================================
# visualize.R -- visualization tools for FISHMORPH landmarks, traits and the
# functional space. Primary output is ggplot2 when installed; a base-graphics
# fallback keeps every function dependency-free.
# =============================================================================

.has_ggplot <- function() requireNamespace("ggplot2", quietly = TRUE)

# landmark pairs to draw as segments (name -> two point indices)
.FM_DRAW_PAIRS <- list(
  Bl = c(1, 2), Bd = c(3, 4), Hd = c(5, 6), Eh = c(7, 8), Mo = c(1, 9),
  PFi = c(10, 11), PFl = c(10, 12), Ed = c(13, 14), Jl = c(1, 15),
  CPd = c(16, 17), CFd = c(18, 19)
)

#' Plot a digitized / reconstructed fish
#'
#' Draws the landmarks and the 11 segments of one specimen. With \pkg{ggplot2}
#' available it returns a ggplot object; otherwise it draws with base graphics.
#'
#' @param x A `fishmorph_landmarks` object.
#' @param specimen Which specimen (name or index) to draw. Default first.
#' @param label_points Annotate landmark numbers (default `TRUE`).
#' @param segments Draw the 11 segments (default `TRUE`).
#' @param engine `"ggplot2"` (default when available) or `"base"`.
#' @param ... Ignored.
#' @return A ggplot object (invisibly for base).
#' @export
plot_landmarks <- function(x, specimen = 1, label_points = TRUE,
                           segments = TRUE, engine = c("ggplot2", "base"), ...) {
  engine <- match.arg(engine)
  co <- .fm_coords(x); sp <- .fm_specimens(x)
  idx <- if (is.character(specimen)) match(specimen, sp) else specimen
  if (is.na(idx)) stop("Specimen not found.", call. = FALSE)
  P <- co[, , idx]
  npt <- nrow(P)
  seg_df <- do.call(rbind, lapply(names(.FM_DRAW_PAIRS), function(nm) {
    ab <- .FM_DRAW_PAIRS[[nm]]
    if (any(ab > npt) || !all(is.finite(P[ab, ]))) return(NULL)
    data.frame(segment = nm, x = P[ab[1], 1], y = P[ab[1], 2],
               xend = P[ab[2], 1], yend = P[ab[2], 2])
  }))
  pt_df <- data.frame(id = seq_len(npt), x = P[, 1], y = P[, 2])
  pt_df <- pt_df[is.finite(pt_df$x) & is.finite(pt_df$y), ]

  if (engine == "ggplot2" && .has_ggplot()) {
    g <- ggplot2::ggplot()
    if (segments && !is.null(seg_df))
      g <- g + ggplot2::geom_segment(
        data = seg_df,
        ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend,
                     yend = .data$yend, colour = .data$segment),
        linewidth = 1)
    g <- g + ggplot2::geom_point(
      data = pt_df, ggplot2::aes(x = .data$x, y = .data$y),
      shape = 21, fill = "white", size = 2.6)
    if (label_points)
      g <- g + ggplot2::geom_text(
        data = pt_df, ggplot2::aes(x = .data$x, y = .data$y,
                                   label = .data$id),
        vjust = -0.9, size = 3)
    g + ggplot2::coord_equal() +
      ggplot2::labs(title = sp[idx], colour = "Segment",
                    x = NULL, y = NULL) +
      ggplot2::theme_minimal()
  } else {
    op <- graphics::par(mar = c(2, 2, 3, 1)); on.exit(graphics::par(op))
    rng_x <- range(pt_df$x); rng_y <- range(pt_df$y)
    graphics::plot(NA, xlim = rng_x, ylim = rng_y, asp = 1,
                   xlab = "", ylab = "", main = sp[idx])
    if (segments && !is.null(seg_df)) {
      cols <- grDevices::rainbow(nrow(seg_df))
      for (k in seq_len(nrow(seg_df)))
        graphics::segments(seg_df$x[k], seg_df$y[k], seg_df$xend[k],
                           seg_df$yend[k], col = cols[k], lwd = 2)
    }
    graphics::points(pt_df$x, pt_df$y, pch = 21, bg = "white", cex = 1.1)
    if (label_points)
      graphics::text(pt_df$x, pt_df$y, labels = pt_df$id, pos = 3, cex = 0.7)
    invisible(NULL)
  }
}

#' @export
plot.fishmorph_landmarks <- function(x, ...) plot_landmarks(x, ...)

#' Plot the FISHMORPH functional trait space (intraitR style)
#'
#' Draws a `fishmorph_trait_space` with the same base-graphics engine as
#' `intraitR::plot.intrait_traitspace()`/[plot.fishmorph_projection()]: per-group
#' geometry (convex hull, spider/ellipse, or density contour), stable per-species
#' colours and an outside legend. Optionally overlays the whole-space
#' kernel-density heatmap (the "global base" density) and the trait loading
#' arrows (biplot).
#'
#' @param x A `fishmorph_trait_space` object.
#' @param style `"hull"` (default), `"spider"`, `"density"` or `"none"` (points
#'   only) -- the per-group geometry, as in intraitR.
#' @param axes Length-2 principal components to display (default `c(1, 2)`).
#' @param reference_density Draw the whole space's kernel-density heatmap
#'   (white-to-red gradient + HDR contours) behind the points -- the density of
#'   the global base. Default `FALSE`.
#' @param reference_points Draw all points again as a light background cloud.
#'   Default `FALSE`.
#' @param density_probs Coverage probabilities for the heatmap contour lines.
#' @param arrows Overlay the trait loadings as biplot arrows (default `FALSE`).
#' @param arrow_scale,arrow_col Arrow length fraction and colour.
#' @param ellipse_level,density_level Coverage for `"spider"`/`"density"`.
#' @param legend,legend_position,legend_title,legend_italic,abbreviate_species
#'   Legend controls (single legend, outside by default).
#' @param ... Passed to [graphics::plot()].
#' @return Invisibly `x`.
#' @seealso [plot.fishmorph_projection()]
#' @examples
#' ref <- load_fishmorph_reference()
#' ts  <- fishmorph_trait_space(ref, groups = ref$Order)
#' plot(ts, style = "hull")                              # per-Order hulls
#' plot(ts, style = "hull", reference_density = TRUE)   # + global density heatmap
#' plot(ts, style = "hull", arrows = TRUE)              # + trait loading arrows
#' @export
plot_trait_space <- function(x, style = c("hull", "spider", "density", "none"),
                             axes = c(1, 2),
                             reference_density = FALSE, reference_points = FALSE,
                             density_probs = c(0.25, 0.5, 0.99),
                             arrows = FALSE, arrow_scale = 0.8,
                             arrow_col = "grey20",
                             ellipse_level = 0.95, density_level = 0.95,
                             legend = !is.null(x$groups),
                             legend_position = "outside", legend_title = "Group",
                             legend_italic = FALSE, abbreviate_species = FALSE,
                             ...) {
  style <- match.arg(style)
  if (!inherits(x, "fishmorph_trait_space"))
    stop("`x` must be a fishmorph_trait_space object.", call. = FALSE)
  ac <- if (is.numeric(axes)) names(x$scores)[axes] else axes
  sc <- x$scores[, ac, drop = FALSE]
  ve <- (x$pca$sdev^2 / sum(x$pca$sdev^2)) * 100
  vi <- if (is.numeric(axes)) axes else match(ac, colnames(x$pca$x))
  xlab <- sprintf("%s (%.1f%%)", ac[1], ve[vi[1]])
  ylab <- sprintf("%s (%.1f%%)", ac[2], ve[vi[2]])

  bg <- if (reference_density || reference_points) as.matrix(sc) else NULL

  .plot_ordination(sc, x$groups, xlab, ylab, style = style,
                   ellipse_level = ellipse_level, density_level = density_level,
                   legend = legend, legend_position = legend_position,
                   legend_title = legend_title, legend_italic = legend_italic,
                   abbreviate_species = abbreviate_species,
                   space_name = "FISHMORPH space",
                   background = bg, background_density = reference_density,
                   background_points = reference_points,
                   density_probs = density_probs, ...)

  if (isTRUE(arrows)) {
    load <- x$pca$rotation[, vi, drop = FALSE]
    lens <- sqrt(rowSums(load^2)); max_len <- max(lens, na.rm = TRUE)
    usr <- graphics::par("usr")
    edge <- min(abs(usr[1]), abs(usr[2]), abs(usr[3]), abs(usr[4]))
    if (is.finite(max_len) && max_len > 0 && is.finite(edge) && edge > 0) {
      fac <- arrow_scale * edge / max_len
      axx <- load[, 1] * fac; ayy <- load[, 2] * fac
      graphics::arrows(0, 0, axx, ayy, length = 0.07, angle = 20,
                       col = arrow_col, lwd = 1.5)
      graphics::text(axx * 1.08, ayy * 1.08, labels = rownames(load),
                     col = arrow_col, cex = 0.75, font = 3, xpd = NA)
    }
  }
  invisible(x)
}

#' @export
plot.fishmorph_trait_space <- function(x, ...) plot_trait_space(x, ...)

#' Distributions of the FISHMORPH traits
#'
#' Faceted density/violin summaries of the 9 ratios (or any chosen traits),
#' useful to inspect a new species against the reference distribution.
#'
#' @param data A data frame of traits or a `fishmorph_landmarks` object.
#' @param traits Trait columns (default the 9 ratios).
#' @param highlight Optional named numeric vector (trait = value) or one-row data
#'   frame drawn as a vertical rule on each panel (e.g. a new species).
#' @param log Log10 x-axis (default `FALSE`).
#' @param engine `"ggplot2"` (default) or `"base"`.
#' @param ... Ignored.
#' @return A ggplot object (invisibly for base).
#' @export
plot_trait_distributions <- function(data, traits = fishmorph_ratio_names(),
                                     highlight = NULL, log = FALSE,
                                     engine = c("ggplot2", "base"), ...) {
  engine <- match.arg(engine)
  if (is_fishmorph_landmarks(data)) data <- fishmorph_ratios(data)
  traits <- intersect(traits, names(data))
  long <- do.call(rbind, lapply(traits, function(t)
    data.frame(trait = t, value = suppressWarnings(as.numeric(data[[t]])))))
  long <- long[is.finite(long$value), ]
  if (log) long$value[long$value <= 0] <- NA
  hl <- NULL
  if (!is.null(highlight)) {
    if (is.data.frame(highlight)) highlight <- unlist(highlight[1, traits])
    hl <- data.frame(trait = names(highlight), value = as.numeric(highlight))
    hl <- hl[hl$trait %in% traits & is.finite(hl$value), ]
  }
  if (engine == "ggplot2" && .has_ggplot()) {
    g <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value)) +
      ggplot2::geom_density(fill = "#88bbdd", colour = "#3366aa",
                            alpha = 0.6) +
      ggplot2::facet_wrap(~trait, scales = "free") +
      ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = "density")
    if (log) g <- g + ggplot2::scale_x_log10()
    if (!is.null(hl))
      g <- g + ggplot2::geom_vline(
        data = hl, ggplot2::aes(xintercept = .data$value),
        colour = "red", linewidth = 0.7)
    g
  } else {
    op <- graphics::par(mfrow = grDevices::n2mfrow(length(traits)),
                        mar = c(3, 3, 2, 1)); on.exit(graphics::par(op))
    for (t in traits) {
      v <- long$value[long$trait == t]
      graphics::plot(stats::density(v, na.rm = TRUE), main = t,
                     xlab = "", ylab = "", col = "#3366aa")
      if (!is.null(hl) && t %in% hl$trait)
        graphics::abline(v = hl$value[hl$trait == t], col = "red", lwd = 2)
    }
    invisible(NULL)
  }
}

# internal: derive the 9 ratios from a measurement source
.ratios_from_source <- function(x, source, scale_cm = NULL) {
  if (source == "landmark") {
    if (!is_fishmorph_landmarks(x))
      stop("source = 'landmark' expects a fishmorph_landmarks object.",
           call. = FALSE)
    return(fishmorph_ratios(x, scale_cm = scale_cm))
  }
  # source == "segment": accept a ratio table directly, else compute from segments
  if (is_fishmorph_landmarks(x))
    stop("source = 'segment' expects a data frame of segments or ratios.",
         call. = FALSE)
  if (all(fishmorph_ratio_names() %in% names(x))) {
    id <- if ("Species" %in% names(x)) x$Species else
      if ("specimen" %in% names(x)) x$specimen else
        if ("Genus.species" %in% names(x)) x$Genus.species else seq_len(nrow(x))
    return(cbind(specimen = id, x[fishmorph_ratio_names()]))
  }
  fishmorph_ratios(x)
}

#' Plot species in the FISHMORPH functional space (landmark- or segment-based)
#'
#' Ports the `intraitR` functional-space plot (`trait_space()` /
#' `project_fishmorph()` plotting) into Rfishmorph. Species are placed in the
#' FISHMORPH morphospace and coloured, with per-group convex hulls and trait
#' loading arrows. The `source` argument selects whether the morphological
#' traits come from digitized **landmarks** or from measured **segments**, so the
#' same species can be compared across measurement methods on one ordination
#' (as in `compare_functional_spaces_fishmorph.R`).
#'
#' The ordination is either supplied via `reference` (a frozen
#' [fishmorph_trait_space()] backbone, e.g. the full FISHMORPH database, onto
#' which the data are projected) or, when `reference = NULL`, fitted on the data
#' themselves. Projecting onto a shared frozen backbone is the correct way to
#' compare landmark- and segment-based positions, because both then live in one
#' coordinate system.
#'
#' @param x The data to place: a `fishmorph_landmarks` object when
#'   `source = "landmark"`, or a data frame of segments/ratios when
#'   `source = "segment"`.
#' @param source `"segment"` (default) or `"landmark"`: how the 9 ratios are
#'   obtained from `x`.
#' @param groups Optional grouping vector (e.g. species) used for colour and
#'   hulls. Defaults to specimen/`Species` identifiers found in `x`.
#' @param reference Optional frozen `fishmorph_trait_space` backbone to project
#'   onto. When `NULL`, the ordination is fitted on `x`.
#' @param axes Length-2 principal components to display (default `c(1, 2)`).
#' @param hull Draw convex hulls (per group, or one global hull when
#'   `global = TRUE`). Default `TRUE`.
#' @param density Overlay a 2D kernel density (per-group contour lines, or a
#'   filled global surface when `global = TRUE`). Default `FALSE`. ggplot2 only.
#' @param global Pool all specimens and ignore `groups`: a single colour, one
#'   overall hull/density (the whole functional space at once). Default `FALSE`.
#' @param points Draw the specimen points (default `TRUE`); set `FALSE` to show
#'   only hulls/density.
#' @param loadings Overlay trait loading arrows (default `TRUE`).
#' @param backbone When a `reference` is given, draw its cloud as light-grey
#'   context points (default `TRUE`).
#' @param legend Show the group colour legend (default `TRUE`). Set `FALSE` to
#'   drop it (useful when there are many groups, e.g. species).
#' @param scale_cm Optional scale for landmark-derived measurements (irrelevant
#'   to ratios; kept for interface parity).
#' @param engine `"ggplot2"` (default) or `"base"`.
#' @param ... Ignored.
#' @return A ggplot object (invisibly for base).
#' @seealso [fishmorph_trait_space()], [project_fishmorph()], [plot_trait_space()]
#' @examples
#' ref <- load_fishmorph_reference()
#' backbone <- fishmorph_trait_space(ref)
#' # segment-based species positions projected on the frozen backbone
#' plot_functional_space(ref, source = "segment", groups = ref$Order,
#'                       reference = backbone)
#' # landmark-based positions for digitized specimens
#' lm <- read_landmarks_csv(
#'   system.file("extdata", "example_landmarks.csv", package = "Rfishmorph"))
#' plot_functional_space(lm, source = "landmark", reference = backbone)
#' @export
plot_functional_space <- function(x, source = c("segment", "landmark"),
                                  groups = NULL, reference = NULL,
                                  axes = c(1, 2), hull = TRUE, density = FALSE,
                                  global = FALSE, points = TRUE, loadings = TRUE,
                                  backbone = TRUE, legend = TRUE, scale_cm = NULL,
                                  engine = c("ggplot2", "base"), ...) {
  source <- match.arg(source)
  engine <- match.arg(engine)

  rat <- .ratios_from_source(x, source, scale_cm = scale_cm)

  # default grouping: explicit groups, else identifiers carried by the data.
  # `global = TRUE` pools everything (no grouping).
  if (global) {
    groups <- NULL
  } else if (is.null(groups)) {
    if (source == "landmark") groups <- .fm_specimens(x)
    else if ("Species" %in% names(rat)) groups <- rat$Species
    else groups <- rat$specimen
  }

  # ordination: project onto a frozen backbone, or fit on the data
  if (!is.null(reference)) {
    if (!inherits(reference, "fishmorph_trait_space"))
      stop("`reference` must be a fishmorph_trait_space object.", call. = FALSE)
    space <- reference
    pr <- .project_onto_space(space, rat, groups = groups)
  } else {
    space <- fishmorph_trait_space(rat, groups = groups)
    sc <- space$scores
    pr <- sc; pr$group <- space$groups
  }
  ac <- if (is.numeric(axes)) names(space$scores)[axes] else axes
  ve <- summary(space$pca)$importance[2, ]
  lab <- function(a) sprintf("%s (%.1f%%)", a, 100 * ve[a])

  df <- data.frame(x = pr[[ac[1]]], y = pr[[ac[2]]],
                   group = if (!is.null(pr$group)) as.factor(pr$group) else NA)
  bb <- if (!is.null(reference) && backbone)
    data.frame(x = space$scores[[ac[1]]], y = space$scores[[ac[2]]]) else NULL

  load_df <- NULL
  if (loadings) {
    rot <- space$pca$rotation[, ac, drop = FALSE]
    s <- 0.9 * max(abs(unlist(space$scores[ac]))) / max(abs(rot))
    load_df <- data.frame(trait = rownames(rot), x = rot[, 1] * s,
                          y = rot[, 2] * s)
  }
  ttl <- sprintf("FISHMORPH functional space (%s-based)", source)

  if (engine == "ggplot2" && .has_ggplot()) {
    g <- ggplot2::ggplot()
    if (!is.null(bb))
      g <- g + ggplot2::geom_point(
        data = bb, ggplot2::aes(x = .data$x, y = .data$y),
        colour = "grey80", size = 0.6)
    has_grp <- !all(is.na(df$group))
    # 2D kernel density (drawn first, underneath)
    if (density) {
      if (has_grp)
        g <- g + ggplot2::geom_density_2d(
          data = df, ggplot2::aes(x = .data$x, y = .data$y,
                                  colour = .data$group), linewidth = 0.3)
      else
        g <- g + ggplot2::stat_density_2d(
          data = df, geom = "polygon", colour = NA, alpha = 0.5,
          ggplot2::aes(x = .data$x, y = .data$y,
                       fill = ggplot2::after_stat(level)))
    }
    # convex hulls: per group, or one global hull
    if (hull) {
      if (has_grp) {
        hulls <- do.call(rbind, lapply(split(df, df$group), function(d) {
          if (nrow(d) < 3) return(NULL); d[grDevices::chull(d$x, d$y), ]
        }))
        if (!is.null(hulls))
          g <- g + ggplot2::geom_polygon(
            data = hulls,
            ggplot2::aes(x = .data$x, y = .data$y, fill = .data$group,
                         colour = .data$group), alpha = 0.12, linewidth = 0.3)
      } else if (nrow(df) >= 3) {
        h <- df[grDevices::chull(df$x, df$y), ]
        g <- g + ggplot2::geom_polygon(
          data = h, ggplot2::aes(x = .data$x, y = .data$y),
          fill = "#3366aa", colour = "#3366aa", alpha = 0.12, linewidth = 0.3)
      }
    }
    if (points) {
      if (has_grp)
        g <- g + ggplot2::geom_point(
          data = df, ggplot2::aes(x = .data$x, y = .data$y,
                                  colour = .data$group), size = 1.3, alpha = 0.8)
      else
        g <- g + ggplot2::geom_point(
          data = df, ggplot2::aes(x = .data$x, y = .data$y),
          colour = "#3366aa", size = 1.3, alpha = 0.8)
    }
    if (!is.null(load_df))
      g <- g +
        ggplot2::geom_segment(
          data = load_df, inherit.aes = FALSE,
          ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
          arrow = ggplot2::arrow(length = ggplot2::unit(0.15, "cm")),
          colour = "grey20") +
        ggplot2::geom_text(
          data = load_df, inherit.aes = FALSE,
          ggplot2::aes(x = .data$x, y = .data$y, label = .data$trait),
          colour = "grey20", size = 3, vjust = -0.4)
    g <- g + ggplot2::labs(title = ttl, x = lab(ac[1]), y = lab(ac[2]),
                           colour = "group", fill = "group") +
      ggplot2::theme_minimal()
    if (!legend) g <- g + ggplot2::theme(legend.position = "none")
    g
  } else {
    op <- graphics::par(mar = c(4, 4, 2, 1)); on.exit(graphics::par(op))
    has_grp <- !all(is.na(df$group))
    xr <- range(c(df$x, bb$x), na.rm = TRUE); yr <- range(c(df$y, bb$y),
                                                          na.rm = TRUE)
    graphics::plot(NA, xlim = xr, ylim = yr, xlab = lab(ac[1]),
                   ylab = lab(ac[2]), main = ttl)
    if (density)
      message("`density = TRUE` is only supported by engine = 'ggplot2'; ",
              "ignored in base graphics.")
    if (!is.null(bb))
      graphics::points(bb$x, bb$y, pch = 19, cex = 0.4, col = "grey80")
    col <- if (has_grp) as.integer(df$group) else "#3366aa"
    if (hull) {
      if (has_grp) for (gi in levels(df$group)) {
        d <- df[df$group == gi, ]
        if (nrow(d) >= 3) {
          h <- grDevices::chull(d$x, d$y)
          graphics::polygon(d$x[h], d$y[h],
                            border = which(levels(df$group) == gi), col = NA)
        }
      } else if (nrow(df) >= 3) {
        h <- grDevices::chull(df$x, df$y)
        graphics::polygon(df$x[h], df$y[h], border = "#3366aa", col = NA)
      }
    }
    if (points) graphics::points(df$x, df$y, pch = 19, cex = 0.7, col = col)
    if (!is.null(load_df)) {
      graphics::segments(0, 0, load_df$x, load_df$y, col = "grey20")
      graphics::text(load_df$x, load_df$y, load_df$trait, cex = 0.7)
    }
    invisible(NULL)
  }
}
