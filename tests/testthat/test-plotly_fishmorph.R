# Moved from intraitR (tests/testthat/test-plotly_fishmorph.R) in Rfishmorph 0.17.0, with the code it tests.

traits9 <- c("REs", "VEp", "RMl", "OGp", "BEl", "BLs", "PFv", "PFs", "CPt")

make_reference <- function(n = 80, seed = 11) {
  set.seed(seed)
  m <- matrix(runif(n * length(traits9), 0.05, 0.9), nrow = n)
  colnames(m) <- traits9
  df <- as.data.frame(m)
  # A "Species" column, as in the real FISHMORPH csv, so itv_reference has
  # reference labels to match the focal species against.
  df$Species <- paste("Genus", c("alpha", "beta", paste0("sp", seq_len(n - 2))))
  rownames(df) <- paste0("ref", seq_len(n))
  df
}

make_projection <- function() {
  ref <- make_reference()
  sp <- ref[1:16, traits9]
  rownames(sp) <- paste0("spec", seq_len(16))
  sp$species <- rep(c("Genus alpha", "Genus beta"), each = 8)
  project_fishmorph(sp, reference = ref,
                    reference_prelogged = TRUE, specimens_prelogged = TRUE)
}

# plot_ly()/add_trace()/layout() only record their arguments (in `x$attrs` and
# `x$layoutAttrs`); the traces and the resolved layout that plotly.js actually
# receives exist only after plotly_build(). Every assertion below therefore
# inspects the *built* widget.
bld <- function(p) plotly::plotly_build(p)$x

trace_types <- function(b) {
  vapply(b$data, function(tr) if (is.null(tr$type)) "scatter" else tr$type,
         character(1))
}
# Traces with a visible legend entry, i.e. the clickable species entries
# (plus the reference cloud when it is drawn).
n_legend_traces <- function(b) {
  sum(vapply(b$data, function(tr) isTRUE(tr$showlegend), logical(1)))
}
axis_title <- function(ax) if (is.list(ax$title)) ax$title$text else ax$title

test_that("plotly_fishmorph() returns a plotly htmlwidget", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  p <- plotly_fishmorph(proj)
  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
  b <- bld(p)
  expect_true("heatmap" %in% trace_types(b))   # reference density background
  expect_equal(n_legend_traces(b), 2L)         # one clickable entry per species
  expect_match(axis_title(b$layout$xaxis), "^PC1")
  expect_match(axis_title(b$layout$yaxis), "^PC2")
})

test_that("every style builds, and adds its own geometry", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  for (st in c("hull", "spider", "density", "points")) {
    b <- bld(plotly_fishmorph(proj, style = st))
    expect_equal(n_legend_traces(b), 2L)
  }
  # "points" draws no per-species geometry, so it uses strictly fewer traces
  # than "hull" on the same projection
  expect_lt(length(bld(plotly_fishmorph(proj, style = "points"))$data),
            length(bld(plotly_fishmorph(proj, style = "hull"))$data))
})

test_that("the species traces are grouped so the legend toggles both layers", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  b <- bld(plotly_fishmorph(proj, style = "hull"))
  groups <- unlist(lapply(b$data, function(tr) tr$legendgroup))
  expect_setequal(unique(groups), c("Genus alpha", "Genus beta"))
  # each species contributes at least two grouped traces (hull + points)
  expect_true(all(table(groups) >= 2))
})

test_that("reference layers are switchable", {
  skip_if_not_installed("plotly")
  proj <- make_projection()

  expect_false("heatmap" %in% trace_types(bld(plotly_fishmorph(proj, background = FALSE))))

  b_pts <- bld(plotly_fishmorph(proj, reference_density = FALSE,
                                reference_points = TRUE))
  expect_false("heatmap" %in% trace_types(b_pts))
  expect_true("scattergl" %in% trace_types(b_pts))            # webgl = TRUE
  expect_false("scattergl" %in%
                 trace_types(bld(plotly_fishmorph(proj, reference_density = FALSE,
                                                  reference_points = TRUE,
                                                  webgl = FALSE))))
  # the reference cloud gets its own legend entry, outside the species groups
  expect_equal(n_legend_traces(b_pts), 3L)

  # itv_reference finds the focal species in the reference "Species" column
  expect_s3_class(plotly_fishmorph(proj, itv_reference = TRUE), "plotly")
})

test_that("arrows are added as scaled annotations", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  expect_length(bld(plotly_fishmorph(proj, arrows = FALSE))$layout$annotations, 0)

  ann <- bld(plotly_fishmorph(proj, arrows = TRUE))$layout$annotations
  expect_length(ann, length(proj$traits))
  expect_setequal(vapply(ann, function(a) a$text, character(1)), proj$traits)
  expect_true(all(vapply(ann, function(a) isTRUE(a$showarrow), logical(1))))
  # arrows start at the ordination origin
  expect_true(all(vapply(ann, function(a) a$ax == 0 && a$ay == 0, logical(1))))

  # a smaller arrow_scale gives strictly shorter arrows
  tip <- function(sc) {
    a <- bld(plotly_fishmorph(proj, arrows = TRUE, arrow_scale = sc))$layout$annotations[[1]]
    sqrt(a$x^2 + a$y^2)
  }
  expect_lt(tip(0.4), tip(0.8))
})

test_that("axes can be changed without refitting the space", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  b <- bld(plotly_fishmorph(proj, axes = c(1, 3)))
  expect_match(axis_title(b$layout$xaxis), "^PC1")
  expect_match(axis_title(b$layout$yaxis), "^PC3")
  # the projection itself is untouched
  expect_equal(proj$axes, c(1, 2))
})

test_that("equal_aspect controls the axis anchoring", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  expect_identical(bld(plotly_fishmorph(proj))$layout$yaxis$scaleanchor, "x")
  expect_null(bld(plotly_fishmorph(proj, equal_aspect = FALSE))$layout$yaxis$scaleanchor)
})

test_that("selection filters restrict the figure only", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  expect_equal(n_legend_traces(bld(plotly_fishmorph(proj,
                                                    select_species = "Genus alpha"))), 1L)
  b <- bld(plotly_fishmorph(proj, select_specimens = paste0("spec", 1:4)))
  pts <- b$data[[length(b$data)]]
  expect_length(pts$x, 4L)
  expect_error(plotly_fishmorph(proj, select_species = "absent"),
               "match the plot selection")
})

test_that("tooltips carry the specimen identity, and the traits only on demand", {
  skip_if_not_installed("plotly")
  proj <- make_projection()

  # identity + species + coordinates, no nine-ratio line (hover_distances,
  # TRUE by default since 1.2x, adds two more lines; it is tested separately)
  b <- bld(plotly_fishmorph(proj, style = "points", hover_distances = FALSE))
  txt <- b$data[[length(b$data)]]$text
  expect_true(any(grepl("spec", txt, fixed = TRUE)))
  expect_true(all(grepl("PC1: ", txt, fixed = TRUE)))
  expect_false(any(grepl("REs = ", txt, fixed = TRUE)))
  n_lines <- function(s) vapply(strsplit(s, "<br>", fixed = TRUE), length,
                                integer(1))
  expect_equal(unique(n_lines(txt)), 4L)     # id, species, PC1, PC2

  # hover_traits = TRUE appends them, wrapped three per line: 9 ratios add
  # three lines to the four above
  b2 <- bld(plotly_fishmorph(proj, style = "points", hover_traits = TRUE,
                             hover_distances = FALSE))
  txt2 <- b2$data[[length(b2$data)]]$text
  expect_true(all(grepl("REs = ", txt2, fixed = TRUE)))
  expect_equal(unique(n_lines(txt2)), 7L)
})

test_that("the tooltip font size is set, and validated", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  expect_equal(bld(plotly_fishmorph(proj))$layout$hoverlabel$font$size, 10)
  expect_equal(bld(plotly_fishmorph(proj, hover_font_size = 8))$layout$hoverlabel$font$size, 8)
  expect_error(plotly_fishmorph(proj, hover_font_size = 0), "positive number")
})

test_that("plotly_fishmorph() validates its arguments", {
  skip_if_not_installed("plotly")
  proj <- make_projection()
  expect_error(plotly_fishmorph(list()), "fishmorph_projection")
  expect_error(plotly_fishmorph(proj, style = "nope"), "should be one of")
  expect_error(plotly_fishmorph(proj, axes = 1), "length-2")
  expect_error(plotly_fishmorph(proj, axes = c(1, 99)), "beyond the")
  expect_error(plotly_fishmorph(proj, fill_alpha = 2), "\\[0, 1\\]")
  expect_error(plotly_fishmorph(proj, arrows = TRUE, arrow_scale = 0), "\\(0, 1\\]")
})

test_that("the internal colour/colorscale helpers behave", {
  expect_equal(.rgba("#4E79A7", 0.2), "rgba(78, 121, 167, 0.2)")
  expect_equal(.rgba("white", 1), "rgba(255, 255, 255, 1)")
  cs <- .plotly_colorscale(c("#FFFFFF", "#000000"))
  expect_length(cs, 2L)
  expect_equal(cs[[1]][[1]], 0)
  expect_equal(cs[[2]][[1]], 1)
  # a single colour still spans the [0, 1] range plotly requires
  expect_equal(vapply(.plotly_colorscale("red"), function(s) s[[1]], numeric(1)),
               c(0, 1))
})
