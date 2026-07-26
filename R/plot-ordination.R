# =============================================================================
# plot-ordination.R -- the shared base-graphics ordination plotter and its
# helpers, ported verbatim from intraitR (utils-internal.R + group_colors.R) so
# the FISHMORPH functional-space figures are IDENTICAL to intraitR's:
#   .plot_ordination(), .density_field()/.kde2d(), .covariance_ellipse(),
#   .density_contour(), .stable_group_colors()/.ordination_palette(),
#   .abbreviate_species_name(), plus the exported group_colors()/
#   reset_group_colors(). Only the cache environment name differs.
# =============================================================================

# session-persistent species-colour cache (so a species keeps its colour across
# calls). Named to avoid clashing with intraitR's own cache.
.fm_color_cache <- new.env(parent = emptyenv())

.ordination_palette <- function(n) {
  base_pal <- c("#4E79A7", "#F28E2B", "#59A14F", "#E15759", "#B07AA1",
                "#76B7B2", "#EDC948", "#FF9DA7", "#9C755F", "#BAB0AC")
  if (n <= length(base_pal)) return(base_pal[seq_len(n)])
  extra_n <- n - length(base_pal)
  extra_pal <- grDevices::hcl.colors(max(extra_n, 40L), palette = "Dark 3")
  c(base_pal, extra_pal[seq_len(extra_n)])
}

.stable_group_colors <- function(labels) {
  labels <- as.character(labels)
  uniq <- unique(labels)
  cache <- if (exists("map", envir = .fm_color_cache, inherits = FALSE))
    get("map", envir = .fm_color_cache, inherits = FALSE) else character(0)
  new_labels <- setdiff(uniq, names(cache))
  if (length(new_labels) > 0) {
    pal <- .ordination_palette(length(cache) + length(new_labels))
    new_cols <- pal[seq(length(cache) + 1, length(cache) + length(new_labels))]
    names(new_cols) <- new_labels
    cache <- c(cache, new_cols)
    assign("map", cache, envir = .fm_color_cache)
  }
  stats::setNames(unname(cache[match(uniq, names(cache))]), uniq)
}

.abbreviate_species_name <- function(x) {
  vapply(x, function(lbl) {
    parts <- strsplit(lbl, " ", fixed = TRUE)[[1]]
    if (length(parts) < 2 || nchar(parts[1]) < 2) return(lbl)
    paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
  }, character(1), USE.NAMES = FALSE)
}

# lightweight 2D kernel density on a grid (no external dependency)
.kde2d <- function(x, y, n = 60, expand = 0.2) {
  rx <- range(x); ry <- range(y)
  padx <- diff(rx) * expand; pady <- diff(ry) * expand
  if (padx <= 0) padx <- 1; if (pady <= 0) pady <- 1
  gx <- seq(rx[1] - padx, rx[2] + padx, length.out = n)
  gy <- seq(ry[1] - pady, ry[2] + pady, length.out = n)
  hx <- stats::bw.nrd0(x); hy <- stats::bw.nrd0(y)
  if (!is.finite(hx) || hx <= 0) hx <- max(diff(rx), 1e-6) / 4
  if (!is.finite(hy) || hy <= 0) hy <- max(diff(ry), 1e-6) / 4
  ax <- outer(gx, x, function(g, xi) stats::dnorm((g - xi) / hx)) / hx
  ay <- outer(gy, y, function(g, yi) stats::dnorm((g - yi) / hy)) / hy
  z <- (ax %*% t(ay)) / length(x)
  list(x = gx, y = gy, z = z)
}

.density_field <- function(x, y, n = 120, expand = 0.05,
                           probs = c(0.25, 0.5, 0.99)) {
  ok <- is.finite(x) & is.finite(y); x <- x[ok]; y <- y[ok]
  if (length(x) < 5) return(NULL)
  kd <- tryCatch(.kde2d(x, y, n = n, expand = expand), error = function(e) NULL)
  if (is.null(kd) || any(!is.finite(kd$z))) return(NULL)
  z_sorted <- sort(as.vector(kd$z), decreasing = TRUE)
  total_mass <- sum(z_sorted)
  if (!is.finite(total_mass) || total_mass <= 0) return(NULL)
  cum_mass <- cumsum(z_sorted) / total_mass
  levels <- vapply(probs, function(p) {
    idx <- which(cum_mass >= p)[1]
    if (is.na(idx)) min(z_sorted) else z_sorted[idx]
  }, numeric(1))
  levels <- sort(unique(levels[is.finite(levels) & levels > 0]))
  list(x = kd$x, y = kd$y, z = kd$z, levels = levels)
}

.covariance_ellipse <- function(x, y, level = 0.95, n_points = 100) {
  if (length(x) < 3) return(NULL)
  S <- stats::cov(cbind(x, y))
  if (any(!is.finite(S)) || any(diag(S) <= 0)) return(NULL)
  centre <- c(mean(x), mean(y)); eig <- eigen(S)
  scale_factor <- sqrt(stats::qchisq(level, df = 2))
  theta <- seq(0, 2 * pi, length.out = n_points)
  circle <- rbind(cos(theta), sin(theta))
  axes <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow = 2)
  ellipse_pts <- t(axes %*% circle) * scale_factor
  sweep(ellipse_pts, 2, centre, "+")
}

.density_contour <- function(x, y, level = 0.95, n = 60) {
  if (length(x) < 5) return(NULL)
  kd <- tryCatch(.kde2d(x, y, n = n), error = function(e) NULL)
  if (is.null(kd) || any(!is.finite(kd$z))) return(NULL)
  z_sorted <- sort(as.vector(kd$z), decreasing = TRUE)
  total_mass <- sum(z_sorted)
  if (!is.finite(total_mass) || total_mass <= 0) return(NULL)
  cum_mass <- cumsum(z_sorted) / total_mass
  idx <- which(cum_mass >= level)[1]
  if (is.na(idx)) return(NULL)
  hdr_level <- z_sorted[idx]
  lines <- grDevices::contourLines(kd$x, kd$y, kd$z, levels = hdr_level)
  if (length(lines) == 0) return(NULL)
  lines
}

#' Reset the session-level species colour cache
#'
#' The ordination plot methods draw each species in a colour taken from a
#' session-persistent cache, so a species keeps the same colour across figures.
#' `reset_group_colors()` clears that cache. Ported from intraitR.
#' @return Invisibly `NULL`.
#' @seealso [group_colors()]
#' @export
reset_group_colors <- function() {
  rm(list = ls(envir = .fm_color_cache), envir = .fm_color_cache)
  invisible(NULL)
}

#' Look up the species colours used by the ordination plots
#'
#' @param x An object with a `$groups` element, or a factor/character vector of
#'   group labels.
#' @return A data frame with columns `group` and `color`.
#' @seealso [reset_group_colors()]
#' @export
group_colors <- function(x) {
  labels <- if (is.list(x)) x$groups else x
  if (is.null(labels))
    stop("`x` has no `groups` element and is not a vector of labels.",
         call. = FALSE)
  groups_f <- droplevels(as.factor(labels)); lv <- levels(groups_f)
  data.frame(group = lv, color = unname(.stable_group_colors(lv)[lv]),
             stringsAsFactors = FALSE)
}

# Overlay PCA trait loadings as biplot arrows on the current ordination plot.
.fm_biplot_arrows <- function(load, arrow_scale = 0.8, arrow_col = "grey20") {
  if (is.null(load) || ncol(load) < 2) return(invisible(NULL))
  lens <- sqrt(rowSums(load^2)); max_len <- max(lens, na.rm = TRUE)
  usr <- graphics::par("usr")
  edge <- min(abs(usr[1]), abs(usr[2]), abs(usr[3]), abs(usr[4]))
  if (is.finite(max_len) && max_len > 0 && is.finite(edge) && edge > 0) {
    fac <- arrow_scale * edge / max_len
    ax <- load[, 1] * fac; ay <- load[, 2] * fac
    graphics::arrows(0, 0, ax, ay, length = 0.07, angle = 20,
                     col = arrow_col, lwd = 1.5)
    graphics::text(ax * 1.08, ay * 1.08, labels = rownames(load),
                   col = arrow_col, cex = 0.75, font = 3, xpd = NA)
  }
  invisible(NULL)
}

# The shared ordination plotter (verbatim port of intraitR:::.plot_ordination).
.plot_ordination <- function(scores, groups, xlab, ylab, style = "spider",
                             ellipse_level = 0.95, density_level = 0.95,
                             legend = TRUE, legend_position = "outside",
                             legend_title = "Group", legend_italic = FALSE,
                             abbreviate_species = FALSE, space_name = "Ordination",
                             background = NULL, background_col = "grey75",
                             background_cex = 0.35, background_alpha = 0.5,
                             background_pch = 16,
                             background_density = FALSE, background_points = TRUE,
                             density_probs = c(0.25, 0.5, 0.99),
                             density_palette = NULL,
                             highlight = NULL, highlight_col = "black",
                             highlight_pch = 21, highlight_cex = 1.5, ...) {
  x <- scores[, 1]; y <- scores[, 2]; dots <- list(...)
  hl <- if (!is.null(highlight)) as.matrix(highlight) else NULL
  if (!is.null(hl) && nrow(hl) == 0) hl <- NULL
  bg <- if (!is.null(background)) as.matrix(background) else NULL
  density_cols <- if (is.null(density_palette))
    grDevices::colorRampPalette(c("#FFFFFF", "#FFEDA0", "#FEB24C", "#FC8D59",
                                  "#EF6548", "#D7301F", "#990000", "#7F0000"))(64)
    else density_palette
  draw_reference_bg <- function() {
    if (is.null(bg)) return(invisible(NULL))
    if (isTRUE(background_density)) {
      fld <- .density_field(bg[, 1], bg[, 2], probs = density_probs)
      if (!is.null(fld)) {
        graphics::image(fld$x, fld$y, fld$z, col = density_cols, add = TRUE,
                        useRaster = TRUE)
        if (length(fld$levels) > 0)
          graphics::contour(fld$x, fld$y, fld$z, levels = fld$levels,
                            drawlabels = FALSE, col = "#3a0000", lwd = 0.6,
                            add = TRUE)
      }
    }
    if (isTRUE(background_points))
      graphics::points(bg[, 1], bg[, 2], pch = background_pch, cex = background_cex,
                       col = grDevices::adjustcolor(background_col,
                                                    alpha.f = background_alpha))
    invisible(NULL)
  }
  draw_highlight <- function() {
    if (is.null(hl)) return(invisible(NULL))
    if (any(highlight_pch %in% 21:25))
      graphics::points(hl[, 1], hl[, 2], pch = highlight_pch, cex = highlight_cex,
                       bg = highlight_col, col = "black", lwd = 1.2)
    else
      graphics::points(hl[, 1], hl[, 2], pch = highlight_pch, cex = highlight_cex,
                       col = highlight_col, lwd = 1.2)
    invisible(NULL)
  }
  old_par <- graphics::par(tcl = 0.3, mgp = c(2.2, 0.5, 0), las = 1)
  on.exit(graphics::par(old_par), add = TRUE)
  pt_cex <- 0.7; pt_alpha <- 0.75
  style_label <- switch(style, spider = "spider", hull = "convex hull",
                        density = "density", NULL)
  main_title <- if (!is.null(style_label))
    paste0(space_name, " (", style_label, ")") else space_name

  if (is.null(groups)) {
    if (!is.null(bg) || !is.null(hl)) {
      rx <- range(c(x, if (!is.null(bg)) bg[, 1], if (!is.null(hl)) hl[, 1]))
      ry <- range(c(y, if (!is.null(bg)) bg[, 2], if (!is.null(hl)) hl[, 2]))
      base_args <- utils::modifyList(
        list(x = x, y = y, xlab = xlab, ylab = ylab, main = main_title,
             type = "n", xlim = rx, ylim = ry), dots)
      do.call(graphics::plot, base_args); draw_reference_bg()
      graphics::points(x, y, pch = 19, cex = pt_cex,
                       col = grDevices::adjustcolor("black", alpha.f = pt_alpha))
      draw_highlight()
    } else {
      plot_args <- utils::modifyList(
        list(x = x, y = y, xlab = xlab, ylab = ylab, main = main_title,
             pch = 19, cex = pt_cex,
             col = grDevices::adjustcolor("black", alpha.f = pt_alpha)), dots)
      do.call(graphics::plot, plot_args)
    }
    graphics::abline(h = 0, v = 0, lty = 3, col = "grey60")
    return(invisible(NULL))
  }

  groups <- droplevels(as.factor(groups))
  label_colors <- .stable_group_colors(levels(groups))
  pal <- unname(label_colors[levels(groups)])
  cols <- grDevices::adjustcolor(pal[as.integer(groups)], alpha.f = pt_alpha)

  ellipses <- vector("list", nlevels(groups))
  hulls <- vector("list", nlevels(groups))
  contours <- vector("list", nlevels(groups))
  extra_x <- numeric(0); extra_y <- numeric(0)
  for (i in seq_len(nlevels(groups))) {
    idx <- which(as.integer(groups) == i); if (length(idx) == 0) next
    gx <- x[idx]; gy <- y[idx]
    if (style == "spider") {
      ell <- .covariance_ellipse(gx, gy, level = ellipse_level)
      if (!is.null(ell)) { ellipses[[i]] <- ell
        extra_x <- c(extra_x, ell[, 1]); extra_y <- c(extra_y, ell[, 2]) }
    } else if (style == "hull") {
      if (length(idx) >= 3) {
        hpts <- grDevices::chull(gx, gy)
        hulls[[i]] <- list(x = gx[hpts], y = gy[hpts])
        extra_x <- c(extra_x, gx[hpts]); extra_y <- c(extra_y, gy[hpts])
      }
    } else if (style == "density") {
      ct <- .density_contour(gx, gy, level = density_level)
      if (!is.null(ct)) { contours[[i]] <- ct
        for (poly in ct) { extra_x <- c(extra_x, poly$x); extra_y <- c(extra_y, poly$y) } }
    }
  }
  xr <- range(c(x, extra_x, if (!is.null(bg)) bg[, 1], if (!is.null(hl)) hl[, 1]))
  yr <- range(c(y, extra_y, if (!is.null(bg)) bg[, 2], if (!is.null(hl)) hl[, 2]))
  xpad <- diff(xr) * 0.06; ypad <- diff(yr) * 0.06
  if (!is.finite(xpad) || xpad == 0) xpad <- 1
  if (!is.finite(ypad) || ypad == 0) ypad <- 1
  auto_xlim <- xr + c(-xpad, xpad); auto_ylim <- yr + c(-ypad, ypad)

  draw_legend <- isTRUE(legend) && nlevels(groups) > 0
  if (draw_legend && identical(legend_position, "outside")) {
    old_mar <- graphics::par(mar = graphics::par("mar") + c(0, 0, 0, 9))
    on.exit(graphics::par(old_mar), add = TRUE)
  }
  plot_args <- utils::modifyList(
    list(x = x, y = y, xlab = xlab, ylab = ylab, main = main_title, pch = 19,
         cex = pt_cex, col = cols, xlim = auto_xlim, ylim = auto_ylim), dots)
  if (!is.null(bg) || !is.null(hl)) {
    do.call(graphics::plot, utils::modifyList(plot_args, list(type = "n")))
    draw_reference_bg()
    graphics::points(x, y, pch = 19, cex = pt_cex, col = cols)
  } else {
    do.call(graphics::plot, plot_args)
  }
  graphics::abline(h = 0, v = 0, lty = 3, col = "grey60")

  if (style == "spider") {
    for (i in seq_len(nlevels(groups))) {
      idx <- which(as.integer(groups) == i); if (length(idx) == 0) next
      gx <- x[idx]; gy <- y[idx]; centre <- c(mean(gx), mean(gy))
      graphics::segments(centre[1], centre[2], gx, gy, col = pal[i], lty = 2, lwd = 0.8)
      if (!is.null(ellipses[[i]])) graphics::lines(ellipses[[i]], col = pal[i], lwd = 1.5)
      graphics::points(centre[1], centre[2], pch = 8, cex = 1.5, col = pal[i], lwd = 2)
    }
  } else if (style == "hull") {
    for (i in seq_len(nlevels(groups)))
      if (!is.null(hulls[[i]]))
        graphics::polygon(hulls[[i]]$x, hulls[[i]]$y, border = pal[i],
                          col = grDevices::adjustcolor(pal[i], alpha.f = 0.15))
  } else if (style == "density") {
    for (i in seq_len(nlevels(groups))) {
      idx <- which(as.integer(groups) == i); if (length(idx) == 0) next
      gx <- x[idx]; gy <- y[idx]
      graphics::points(mean(gx), mean(gy), pch = 8, cex = 1.5, col = pal[i], lwd = 2)
      if (!is.null(contours[[i]]))
        for (poly in contours[[i]]) graphics::lines(poly$x, poly$y, col = pal[i], lwd = 1.5)
    }
  }
  draw_highlight()

  if (draw_legend) {
    labels <- levels(groups)
    if (isTRUE(abbreviate_species)) labels <- .abbreviate_species_name(labels)
    legend_text <- if (isTRUE(legend_italic))
      as.expression(lapply(labels, function(l) bquote(italic(.(l))))) else labels
    if (identical(legend_position, "outside"))
      graphics::legend(x = graphics::par("usr")[2], y = graphics::par("usr")[4],
                       xpd = TRUE, legend = legend_text, col = pal, pch = 19,
                       bty = "n", cex = 0.8, xjust = 0, yjust = 1,
                       title = legend_title)
    else
      graphics::legend(legend_position, legend = legend_text, col = pal, pch = 19,
                       bty = "n", cex = 0.8, title = legend_title)
  }
  invisible(NULL)
}
