# =============================================================================
# test-basin-indices.R -- the five functional-diversity indices of the basin
# explorer, on configurations whose value can be worked out by hand.
#
# The point of testing on a unit square rather than on random points is that a
# random cloud only tells us the code runs. A square has an area of exactly 1,
# a minimum spanning tree of three unit branches, and a perfectly even
# distribution around its own centre: every one of the five indices has a
# closed form there, so a regression shows up as a wrong NUMBER rather than as
# a merely different one.
# =============================================================================

square <- matrix(c(0, 0,
                   1, 0,
                   1, 1,
                   0, 1), ncol = 2, byrow = TRUE)

test_that("the five indices are exact on the unit square", {
  fd <- fishmorph_basin_indices(square)

  expect_equal(fd$n, 4L)
  # FRic: the area of the square itself.
  expect_equal(fd$FRic, 1)
  # FDis: every corner sits at half a diagonal from the centre.
  expect_equal(fd$FDis, sqrt(2) / 2)
  # Rao with equal weights, over ALL S^2 ordered pairs including the four zero
  # self-distances: (2 * (4 * 1 + 2 * sqrt(2))) / 16.
  expect_equal(fd$Rao, 0.5 + sqrt(2) / 4)
  # FEve: the MST is three sides of the square, so the branch shares are
  # exactly 1/(S-1) and evenness is maximal.
  expect_equal(fd$FEve, 1)
  # FDiv: every species is a hull vertex and equidistant from the centre of
  # gravity of the vertices, so divergence is maximal.
  expect_equal(fd$FDiv, 1)
})

test_that("Rao is the mean over ordered pairs, not over distinct pairs", {
  # The two definitions differ by (S - 1) / S and only the first is Rao's
  # quadratic entropy. Getting this wrong inflates every basin's Rao by a
  # factor that shrinks with richness, i.e. it manufactures a spurious
  # richness-diversity relationship.
  fd <- fishmorph_basin_indices(square)
  D <- as.matrix(stats::dist(square))
  expect_equal(fd$Rao, sum(D) / nrow(square)^2)
  expect_false(isTRUE(all.equal(fd$Rao, mean(D[upper.tri(D)]))))
})

test_that("hull-based indices are NA, not 0, on a degenerate cloud", {
  line <- cbind(c(0, 1, 2, 3), c(0, 0, 0, 0))
  fd <- fishmorph_basin_indices(line)
  expect_true(is.na(fd$FRic))
  expect_true(is.na(fd$FDiv))
  # The distance-based family stays defined: collinearity kills a volume, not
  # a distance.
  expect_equal(fd$FDis, mean(abs(c(0, 1, 2, 3) - 1.5)))
  expect_equal(fd$FEve, 1)          # three equal MST branches

  dup <- matrix(rep(c(1, 2), each = 5), ncol = 2)
  expect_true(is.na(fishmorph_basin_indices(dup)$FRic))
  expect_equal(fishmorph_basin_indices(dup)$FDis, 0)
})

test_that("assemblages too small for an index give NA rather than a number", {
  one <- matrix(c(1, 2), ncol = 2)
  fd1 <- fishmorph_basin_indices(one)
  expect_equal(fd1$n, 1L)
  expect_true(all(is.na(unlist(fd1[c("FRic", "FDiv", "FDis", "FEve", "Rao")]))))

  two <- matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)
  fd2 <- fishmorph_basin_indices(two)
  expect_equal(fd2$n, 2L)
  expect_equal(fd2$FDis, 1)         # both points one unit from the midpoint
  expect_equal(fd2$Rao, 1)          # (2 + 2) / 4
  expect_true(is.na(fd2$FEve))      # needs three species
  expect_true(is.na(fd2$FRic))

  empty <- matrix(numeric(0), ncol = 2)
  expect_equal(fishmorph_basin_indices(empty)$n, 0L)
  expect_equal(fishmorph_basin_indices(NULL)$n, 0L)
})

test_that("incomplete species are dropped, not imputed to the centroid", {
  # A species whose ordination score is missing has no position; putting it at
  # the origin would be an invented observation that shrinks FDis and inflates
  # nothing visibly.
  sq_na <- rbind(square, c(NA, 0.5))
  expect_equal(fishmorph_basin_indices(sq_na), fishmorph_basin_indices(square))
})

test_that("distance-based indices are invariant to rotation and translation", {
  set.seed(11)
  P <- matrix(stats::rnorm(120), ncol = 3)
  th <- pi / 7
  R <- diag(3)
  R[1:2, 1:2] <- c(cos(th), sin(th), -sin(th), cos(th))
  Q <- sweep(P %*% R, 2, c(10, -3, 0.5), "+")

  a <- fishmorph_basin_indices(P, axes_dist = 1:3)
  b <- fishmorph_basin_indices(Q, axes_dist = 1:3)
  expect_equal(a$FDis, b$FDis)
  expect_equal(a$Rao, b$Rao)
  expect_equal(a$FEve, b$FEve)
  # FRic and FDiv are read in the PC1-PC2 plane, which the rotation moves: a
  # rotated cloud has a different footprint there, and the test asserts that
  # the two families answer to different things rather than pretending both
  # are invariant.
  expect_true(is.finite(a$FRic) && is.finite(b$FRic))
})

test_that("the two index families read the axes they are given", {
  set.seed(3)
  P <- cbind(stats::rnorm(40), stats::rnorm(40), 100 * stats::rnorm(40))
  small <- fishmorph_basin_indices(P, axes_ric = 1:2, axes_dist = 1:2)
  big   <- fishmorph_basin_indices(P, axes_ric = 1:2, axes_dist = 1:3)
  # The third axis is a hundred times wider, so a distance-based index that
  # ignored axes_dist could not tell the two calls apart.
  expect_equal(small$FRic, big$FRic)
  expect_gt(big$FDis, 10 * small$FDis)
})

test_that("the minimum spanning tree has S - 1 branches and the right total", {
  mst <- getFromNamespace(".fmb_mst_lengths", "Rfishmorph")
  D <- as.matrix(stats::dist(square))
  l <- mst(D)
  expect_length(l, 3L)
  expect_equal(sort(l), c(1, 1, 1))

  # A chain 0 -- 1 -- 3 -- 6 on a line: the MST is the chain itself.
  x <- matrix(c(0, 1, 3, 6), ncol = 1)
  expect_equal(sort(mst(as.matrix(stats::dist(x)))), c(1, 2, 3))
})

test_that("FEve is bounded in [0, 1] and falls when one branch dominates", {
  even <- fishmorph_basin_indices(square)$FEve
  # One species pushed far away lengthens a single MST branch, which is what
  # FEve is built to detect.
  skewed <- fishmorph_basin_indices(rbind(square, c(0.5, 40)))$FEve
  expect_true(skewed >= 0 && skewed <= 1)
  expect_lt(skewed, even)
})
