# Tests for the broken body axis: which points act as hinges for Bl, and which
# must not. The rule is deliberate and was measured, not assumed, so it is
# locked here rather than left to the reading of .fm_bl_broken().

# A specimen whose axis zigzags around the 1-2 chord, so that the arc length is
# strictly greater than the straight distance and any dropped hinge shows up.
fixture <- function(hinges = c(22L, 24L, 25L), p23 = NULL) {
  P <- matrix(NA_real_, nrow = 25, ncol = 2, dimnames = list(NULL, c("X", "Y")))
  P[1, ] <- c(0, 0)
  P[2, ] <- c(100, 0)
  coords <- list("22" = c(25, 10), "24" = c(50, 0), "25" = c(75, -10))
  for (h in hinges) P[h, ] <- coords[[as.character(h)]]
  if (!is.null(p23)) P[23, ] <- p23
  fishmorph_landmarks(P, specimen = "demo")
}

bl_of <- function(x) fishmorph_segments(x, na.rm = FALSE)$Bl

test_that("the canonical frame carries 25 points", {
  x <- fishmorph_landmarks(matrix(rnorm(40), 20, 2))
  expect_identical(dim(x$coords)[1], 25L)
  # Padding must never truncate a configuration that is already wider.
  wide <- fishmorph_landmarks(matrix(rnorm(60), 30, 2))
  expect_identical(dim(wide$coords)[1], 30L)
})

test_that("with no hinge, Bl is the straight 1-2 distance", {
  expect_equal(bl_of(fixture(hinges = integer(0))), 100)
})

test_that("22, 24 and 25 all act as hinges, and are cumulative", {
  d <- function(a, b) sqrt(sum((a - b)^2))
  o <- c(0, 0); e <- c(100, 0)
  h22 <- c(25, 10); h24 <- c(50, 0); h25 <- c(75, -10)

  expect_equal(bl_of(fixture(22L)), d(o, h22) + d(h22, e))
  expect_equal(bl_of(fixture(24L)), d(o, h24) + d(h24, e))
  expect_equal(bl_of(fixture(25L)), d(o, h25) + d(h25, e))
  expect_equal(bl_of(fixture(c(22L, 24L, 25L))),
               d(o, h22) + d(h22, h24) + d(h24, h25) + d(h25, e))

  # Each hinge added can only lengthen the chain: the arc of a zigzag is longer
  # than any chord it replaces.
  expect_gt(bl_of(fixture(c(22L, 24L, 25L))), bl_of(fixture(22L)))
  expect_gt(bl_of(fixture(22L)), bl_of(fixture(integer(0))))
})

test_that("hinges are ordered along the 1-2 chord, not by their number", {
  # Same three positions, but assigned to hinge numbers in reverse order. The
  # arc length must not change: the operator's clicking order is irrelevant.
  P <- matrix(NA_real_, nrow = 25, ncol = 2, dimnames = list(NULL, c("X", "Y")))
  P[1, ] <- c(0, 0); P[2, ] <- c(100, 0)
  P[25, ] <- c(25, 10); P[24, ] <- c(50, 0); P[22, ] <- c(75, -10)
  expect_equal(bl_of(fishmorph_landmarks(P, specimen = "rev")),
               bl_of(fixture(c(22L, 24L, 25L))))
})

test_that("point 23 is NOT a hinge and cannot alter Bl", {
  # 23 lies on the line (1,9), i.e. the ventral line, not the body axis.
  # Inserting it into the chain inflated Bl by a median 8.5% on the 650 species
  # that carry 22, 23 and 24 -- hence this test, placed far off the axis so any
  # regression is unmissable.
  ref <- bl_of(fixture(c(22L, 24L, 25L)))
  expect_equal(bl_of(fixture(c(22L, 24L, 25L), p23 = c(10, -40))), ref)
  expect_equal(bl_of(fixture(integer(0), p23 = c(10, -40))), 100)
  expect_false(23L %in% Rfishmorph:::.FM_AXIS_HINGES)
})

test_that("the schema declares the frame the data actually carries", {
  sch <- fishmorph_schema()
  expect_length(sch$landmark_labels, 25L)
  expect_identical(sch$axis_hinges, c(22L, 24L, 25L))
})

test_that("an unplaced hinge is skipped, not treated as the origin", {
  # A hinge left at (0, 0) is "not placed" by convention, and must not drag the
  # chain back to the origin.
  P <- matrix(NA_real_, nrow = 25, ncol = 2, dimnames = list(NULL, c("X", "Y")))
  P[1, ] <- c(0, 0); P[2, ] <- c(100, 0); P[24, ] <- c(0, 0)
  expect_equal(bl_of(fishmorph_landmarks(P, specimen = "zero")), 100)
})
