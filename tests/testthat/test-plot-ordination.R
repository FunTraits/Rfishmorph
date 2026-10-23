# Engine helpers of the shared ordination plotter. Moved from intraitR
# (tests/testthat/test-shape_space.R) in Rfishmorph 0.17.0 with the code.

test_that(".abbreviate_species_name() abbreviates clean binomials only", {
  expect_equal(.abbreviate_species_name("Barbatula barbatula"), "B. barbatula")
  expect_equal(.abbreviate_species_name("Gobio occitaniae"), "G. occitaniae")
  # informal multi-taxon labels: only the first word is abbreviated
  expect_equal(
    .abbreviate_species_name("Phoxinus phoxinus/bigerri"),
    "P. phoxinus/bigerri"
  )
  # not a clean binomial: left unchanged rather than mangled
  expect_equal(.abbreviate_species_name("unresolved"), "unresolved")
  expect_equal(.abbreviate_species_name(""), "")
  # vectorised, preserves order and length
  expect_equal(
    .abbreviate_species_name(c("Barbatula barbatula", "Gobio occitaniae")),
    c("B. barbatula", "G. occitaniae")
  )
})

test_that(".ordination_palette() returns one distinct colour per group, any size", {
  p5 <- .ordination_palette(5)
  expect_length(p5, 5)
  expect_equal(length(unique(p5)), 5)

  p10 <- .ordination_palette(10)
  expect_length(p10, 10)
  expect_equal(length(unique(p10)), 10)

  # more groups than the curated palette has colours: falls back gracefully
  p15 <- .ordination_palette(15)
  expect_length(p15, 15)
})

test_that(".kde2d() and .density_contour() produce a sensible bivariate density estimate", {
  set.seed(11)
  n <- 200
  x <- stats::rnorm(n, mean = 3, sd = 1)
  y <- stats::rnorm(n, mean = -1, sd = 2)

  kd <- .kde2d(x, y, n = 40)
  expect_equal(length(kd$x), 40)
  expect_equal(length(kd$y), 40)
  expect_equal(dim(kd$z), c(40, 40))
  expect_true(all(kd$z >= 0))
  # the grid must comfortably contain the data (padded on both sides)
  expect_true(min(kd$x) < min(x) && max(kd$x) > max(x))
  expect_true(min(kd$y) < min(y) && max(kd$y) > max(y))

  contours <- .density_contour(x, y, level = 0.5)
  expect_true(is.list(contours))
  expect_true(length(contours) >= 1)
  expect_true(all(c("x", "y", "level") %in% names(contours[[1]])))

  # too few points: skipped gracefully rather than erroring
  expect_null(.density_contour(x[1:3], y[1:3]))
})

test_that(".covariance_ellipse() traces a level-consistent dispersion ellipse", {
  set.seed(42)
  n <- 300
  x <- stats::rnorm(n, mean = 2, sd = 3)
  y <- 0.5 * x + stats::rnorm(n, sd = 1)

  level <- 0.95
  ell <- .covariance_ellipse(x, y, level = level)
  expect_true(is.matrix(ell))
  expect_equal(ncol(ell), 2)

  # Points on the ellipse should all sit at (approximately) the same
  # Mahalanobis distance from the centroid, equal to sqrt(qchisq(level, 2)).
  S <- stats::cov(cbind(x, y))
  Sinv <- solve(S)
  centre <- c(mean(x), mean(y))
  d2 <- apply(ell, 1, function(p) {
    dv <- p - centre
    as.numeric(t(dv) %*% Sinv %*% dv)
  })
  expect_true(all(abs(d2 - stats::qchisq(level, df = 2)) < 1e-6))

  # roughly `level` proportion of the underlying data should fall inside
  d2_pts <- apply(cbind(x, y), 1, function(p) {
    dv <- p - centre
    as.numeric(t(dv) %*% Sinv %*% dv)
  })
  frac_inside <- mean(d2_pts <= stats::qchisq(level, df = 2))
  expect_true(abs(frac_inside - level) < 0.05)
})

test_that(".covariance_ellipse() returns NULL for fewer than 3 points", {
  expect_null(.covariance_ellipse(c(1, 2), c(1, 2)))
})

test_that("fm_* primitives expose the engine and share the group_colors() cache", {
  reset_group_colors()
  pal <- fm_group_palette(c("B", "A", "B"))
  expect_named(pal, c("B", "A"))
  gc <- group_colors(c("A", "B"))
  expect_identical(gc$color[gc$group == "A"], unname(pal["A"]))
  expect_identical(gc$color[gc$group == "B"], unname(pal["B"]))
  reset_group_colors()
  expect_length(ls(envir = .fm_color_cache), 0)
  expect_equal(fm_abbreviate_species("Barbatula barbatula"), "B. barbatula")

  set.seed(1)
  sc <- data.frame(PC1 = stats::rnorm(30), PC2 = stats::rnorm(30))
  gr <- factor(rep(c("A", "B", "C"), each = 10))
  tmp <- tempfile(fileext = ".pdf"); grDevices::pdf(tmp)
  on.exit({ grDevices::dev.off(); unlink(tmp) }, add = TRUE)
  expect_invisible(fm_plot_ordination(sc, gr, "PC1", "PC2", style = "hull"))
})

test_that("max_pair_distance() is the exact diameter, NA below two points", {
  expect_equal(max_pair_distance(c(0, 3, 0), c(0, 0, 4)), 5)
  set.seed(2)
  x <- stats::rnorm(200); y <- stats::rnorm(200)
  expect_equal(max_pair_distance(x, y), max(stats::dist(cbind(x, y))))
  expect_equal(max_pair_distance(cbind(x, y)), max_pair_distance(x, y))
  expect_true(is.na(max_pair_distance(1, 1)))
  expect_error(max_pair_distance(1:3, 1:2), "same length")
})
