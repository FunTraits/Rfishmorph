# Coincident points of the PROJECTION kind: point 4 declared on the mid axis
# 22 -> 24. The four historical rules copy the coordinates of one point onto
# another; this one puts a point on a LINE, so what has to be checked is not an
# equality of coordinates but a distance to that line, the abscissa that must
# NOT change, and the behaviour of everything the belly line derives from 4.

# A CURVED fish: the hinges 22 and 24 are off the chord 1-2, so the mid axis is
# a segment of its own and a projection onto it is not a projection onto 1-2.
# Without this, every test would pass on the straight axis by accident.
fm_bent_fish <- function() {
  P <- matrix(NA_real_, 25, 2, dimnames = list(NULL, c("X", "Y")))
  pts <- list(
    "1" = c(0, 0), "2" = c(1000, 0), "22" = c(300, 20), "24" = c(700, -10),
    "3" = c(470, -120), "4" = c(470, 130),
    "5" = c(100, -100), "6" = c(100, 90), "7" = c(100, 20), "8" = c(100, 100),
    "9" = c(0, 60), "10" = c(250, -30), "11" = c(250, 110), "12" = c(400, 90),
    "13" = c(100, -10), "14" = c(100, 50), "15" = c(60, 40),
    "16" = c(930, -55), "17" = c(930, 55), "18" = c(1000, -130),
    "19" = c(1000, 130))
  for (k in names(pts)) P[as.integer(k), ] <- pts[[k]]
  P
}

# similarity (rotation, possible mirror, translation). Defined here rather than
# borrowed from test-extremes.R: testthat gives each test file its own
# environment, so a helper written in one is not visible in another.
fm_bent_move <- function(P, theta = 0, mirror_x = FALSE, mirror_y = FALSE) {
  if (mirror_x) P[, 1] <- -P[, 1]
  if (mirror_y) P[, 2] <- -P[, 2]
  R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  P %*% t(R) + matrix(c(1500, 900), nrow(P), 2, byrow = TRUE)
}

test_that("the rule projects 4 onto 22-24 and keeps its abscissa", {
  P  <- fm_bent_fish()
  fr <- .fm_mid_frame(P)
  ax_before <- sum((P[4, ] - fr$o) * fr$u)
  Q <- .fm_apply_collapse(P, "Bd4")
  expect_equal(.fm_dist_mid(Q, 4L), 0, tolerance = 1e-9)
  # only the height is given up: the position along the body is the operator's
  expect_equal(sum((Q[4, ] - fr$o) * fr$u), ax_before, tolerance = 1e-9)
  # and nothing else is touched by this rule
  others <- setdiff(which(is.finite(P[, 1])), 4L)
  expect_equal(Q[others, ], P[others, ])
})

test_that("the projection is idempotent", {
  P <- .fm_apply_collapse(fm_bent_fish(), "Bd4")
  expect_equal(.fm_apply_collapse(P, "Bd4"), P)
})

test_that("the projection is invariant to the orientation of the photograph", {
  P <- fm_bent_fish()
  for (tf in list(list(theta = 0.4), list(theta = -1.1, mirror_x = TRUE),
                  list(theta = 2.3, mirror_y = TRUE))) {
    M <- do.call(fm_bent_move,c(list(P), tf))
    # projecting then moving == moving then projecting (up to float noise)
    a <- do.call(fm_bent_move,c(list(.fm_apply_collapse(P, "Bd4")), tf))
    b <- .fm_apply_collapse(M, "Bd4")
    expect_equal(a[4, ], b[4, ], tolerance = 1e-8)
  }
})

test_that("the constrained editing re-derives the belly line from the projected 4", {
  # this is what forces the rule to be applied BEFORE .fm_constrain(): 4 is the
  # master of the belly line, 11 inherits its height and 8, 9 inherit 11's.
  P  <- .fm_apply_collapse(fm_bent_fish(), "Bd4")
  Q  <- .fm_constrain(P)
  fr <- .fm_mid_frame(Q)
  expect_equal(.fm_dist_mid(Q, 4L), 0, tolerance = 1e-9)   # 4 stays on the axis
  expect_equal(.fm_dist_mid(Q, 11L), 0, tolerance = 1e-9)  # 11 follows 4
  # 8 and 9 sit on the belly line broken at 11, which now runs along the axis
  h <- function(i) sum((Q[i, ] - Q[11, ]) * c(Q[22, 2] - Q[1, 2],
                                              -(Q[22, 1] - Q[1, 1])))
  expect_equal(h(8L), 0, tolerance = 1e-6)
  expect_equal(h(9L), 0, tolerance = 1e-6)
})

test_that("a declared projection suspends only the VENTRAL extreme check", {
  P <- .fm_apply_collapse(fm_bent_fish(), "Bd4")
  P <- .fm_constrain(P)
  # 4 on the axis: 6, 10 and 14 are now below it -- the rule working, not an error
  expect_false(is.null(.fm_extreme_violations(P)))
  expect_true(all(.fm_extreme_violations(P)$point == 4L))
  expect_null(.fm_extreme_violations(P, skip = .fm_collapse_skip_extreme("Bd4")))
  # the dorsal half still holds: 5 above 3 is reported all the same
  P[5, 2] <- P[3, 2] - 70
  v <- .fm_extreme_violations(P, skip = .fm_collapse_skip_extreme("Bd4"))
  expect_equal(v$point, 3L)
  expect_equal(v$culprit, 5L)
})

test_that("the rule is read back off the geometry of a saved specimen", {
  P <- fm_bent_fish()
  expect_length(.fm_collapse_detect(P), 0L)          # a real belly is 122 px away
  expect_identical(.fm_collapse_detect(.fm_apply_collapse(P, "Bd4")), "Bd4")
  # an incomplete configuration is not "detected": no axis, no statement
  Q <- .fm_apply_collapse(P, "Bd4"); Q[4, ] <- NA_real_
  expect_length(.fm_collapse_detect(Q), 0L)
})

test_that("only the COPIED points are taken over by their rule", {
  # a projected point keeps the abscissa that was clicked, so its override must
  # survive; a copied point owes its partner everything
  expect_identical(.fm_collapse_points("Bd4"), 4L)
  expect_length(.fm_collapse_points("Bd4", kinds = "move"), 0L)
  expect_identical(.fm_collapse_points("PFi", kinds = "move"), 10L)
  expect_length(.fm_collapse_points("PFi", kinds = "project"), 0L)
})
