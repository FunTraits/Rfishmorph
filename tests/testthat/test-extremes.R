# The extreme-point convention: 3 = the most DORSAL point, 4 = the most VENTRAL.
# The functions under test are internal to the digitizing application
# (R/digitizer-app.R) and work in image pixels, Y downwards.

# synthetic fish: 25 rows, back towards the negative Y, head on the left.
fm_test_fish <- function() {
  P <- matrix(NA_real_, 25, 2, dimnames = list(NULL, c("X", "Y")))
  pts <- list(
    "1" = c(0, 0), "2" = c(1000, 0), "3" = c(470, -120), "4" = c(470, 130),
    # the eye vertical, in anatomical order from the back downwards:
    # 5 (-100) > 13 (-10) > 7 (20) > 14 (50) > 6 (90) > 8 (100)
    "5" = c(100, -100), "6" = c(100, 90), "7" = c(100, 20), "8" = c(100, 100),
    "9" = c(0, 60), "10" = c(250, -30), "11" = c(250, 110), "12" = c(400, 90),
    "13" = c(100, -10), "14" = c(100, 50), "15" = c(60, 40),
    "16" = c(930, -55), "17" = c(930, 55), "18" = c(1000, -130),
    "19" = c(1000, 130), "22" = c(500, 0), "23" = c(300, 40))
  for (k in names(pts)) P[as.integer(k), ] <- pts[[k]]
  P
}

# similarity (rotation + possible mirror + translation): the conventions must
# be invariant to the orientation of the photograph.
fm_test_move <- function(P, theta = 0, mirror_x = FALSE, mirror_y = FALSE) {
  if (mirror_x) P[, 1] <- -P[, 1]
  if (mirror_y) P[, 2] <- -P[, 2]
  R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
  P %*% t(R) + matrix(c(1500, 900), nrow(P), 2, byrow = TRUE)
}

test_that("a compliant specimen raises no violation", {
  expect_null(.fm_extreme_violations(fm_test_fish()))
})

test_that("a point overshooting 3 or 4 is detected, with the right culprit", {
  P <- fm_test_fish(); P[5, 2] <- -190          # top of the head above the back
  v <- .fm_extreme_violations(P)
  expect_equal(nrow(v), 1L)
  expect_equal(v$point, 3L)
  expect_equal(v$culprit, 5L)
  expect_equal(v$delta, 70, tolerance = 1e-6)

  Q <- fm_test_fish(); Q[6, 2] <- 200           # bottom of the head below the belly
  w <- .fm_extreme_violations(Q)
  expect_equal(w$point, 4L)
  expect_equal(w$culprit, 6L)
  expect_equal(w$delta, 70, tolerance = 1e-6)
})

test_that("caudal (16-19) and appendage tips (12, 15) are exempt", {
  P <- fm_test_fish()
  P[16, 2] <- -400; P[18, 2] <- -600            # caudal fin very high
  P[19, 2] <- 600;  P[17, 2] <- 400             # ... and very low
  P[12, 2] <- 400                               # pectoral fin below the belly
  P[15, 2] <- 300                               # jaw below the belly
  expect_null(.fm_extreme_violations(P))
})

# The ventral points 8, 9 and 11 are DERIVED from 4 (the belly line): comparing
# them with 4 would amount to testing 4 against itself. Over the 1,036 T-26
# specimens, including them flags 20.6 % of the batch (belly-line noise, median
# overshoot 0.5 % of Bl) against 1.5 % once they are excluded.
test_that("derived ventral points (8, 9, 11) are exempt", {
  P <- fm_test_fish()
  P[8, 2] <- 300; P[9, 2] <- 350; P[11, 2] <- 400
  expect_null(.fm_extreme_violations(P))
})

# --- the eye vertical, in order ----------------------------------------------
# 5, 13, 7, 14, 6, 8 lie on one vertical, and anatomy fixes their order along it.
# An inversion leaves every pair internally consistent -- Ed (13-14) keeps its
# length -- while Hd or Eh silently refers to the wrong point, which is why no
# other check catches it.

test_that("a compliant eye vertical raises no violation", {
  expect_null(.fm_eye_order_violations(fm_test_fish()))
})

test_that("point 5 must top the eye group", {
  P <- fm_test_fish(); P[5, 2] <- 0             # top of the head below 13 and 7
  v <- .fm_eye_order_violations(P)
  expect_false(is.null(v))
  expect_true(all(v$kind == "order"))
  # the group rule names the point that overshoots 5 the most (13, by 10 px)
  expect_true(any(v$point == 5L & v$culprit == 13L))
})

test_that("a swapped pair is detected where it happens", {
  # the eye clicked bottom-first: 13 and 14 exchanged
  P <- fm_test_fish(); P[13, ] <- c(100, 50); P[14, ] <- c(100, -10)
  v <- .fm_eye_order_violations(P)
  expect_true(any(v$point == 13L & v$culprit == 7L))   # 7 above 13
  expect_true(any(v$point == 7L & v$culprit == 14L))   # 14 above 7
  expect_true(all(v$delta > 0))

  # the eye centre outside its own pair
  Q <- fm_test_fish(); Q[7, 2] <- -60
  w <- .fm_eye_order_violations(Q)
  expect_true(any(w$point == 13L & w$culprit == 7L))
})

test_that("a missing point is stepped over, not treated as an inversion", {
  P <- fm_test_fish(); P[13, ] <- NA_real_
  expect_null(.fm_eye_order_violations(P))
  P[14, ] <- NA_real_; P[7, ] <- NA_real_
  expect_null(.fm_eye_order_violations(P))
})

test_that("the eye order uses the same tolerance as the extremes", {
  P <- fm_test_fish()
  P[13, 2] <- P[5, 2] - 2                       # 2 px above 5: click noise
  expect_null(.fm_eye_order_violations(P))
  P[13, 2] <- P[5, 2] - 20
  expect_false(is.null(.fm_eye_order_violations(P)))
})

test_that("the eye order is invariant to photograph orientation", {
  P <- fm_test_fish(); P[13, ] <- c(100, 50); P[14, ] <- c(100, -10)
  ref <- .fm_eye_order_violations(P)
  for (tf in list(list(theta = 0.35), list(mirror_x = TRUE),
                  list(theta = 0.6, mirror_x = TRUE, mirror_y = TRUE))) {
    v <- .fm_eye_order_violations(do.call(fm_test_move, c(list(P), tf)))
    expect_equal(v$point, ref$point)
    expect_equal(v$culprit, ref$culprit)
    expect_equal(v$delta, ref$delta, tolerance = 1e-6)
  }
})

test_that("both families of violation come back in one table", {
  P <- fm_test_fish()
  P[5, 2] <- -190                               # 5 above the back -> extreme
  P[13, ] <- c(100, 50); P[14, ] <- c(100, -10) # eye swapped     -> order
  v <- .fm_convention_violations(P)
  expect_setequal(unique(v$kind), c("extreme", "order"))
  expect_equal(v$kind[1], "extreme")            # correctable ones first
  # an order violation is NOT repaired by the automatic correction
  fixed <- .fm_fix_extremes(P, v[v$kind == "extreme", , drop = FALSE])
  expect_null(.fm_extreme_violations(fixed))
  expect_false(is.null(.fm_eye_order_violations(fixed)))
})

test_that("detection is invariant to photograph orientation", {
  P <- fm_test_fish(); P[5, 2] <- -190
  ref <- .fm_extreme_violations(P)
  for (tf in list(list(theta = 0.35), list(theta = -0.8),
                  list(mirror_x = TRUE), list(mirror_y = TRUE),
                  list(theta = 0.6, mirror_x = TRUE, mirror_y = TRUE))) {
    v <- .fm_extreme_violations(do.call(fm_test_move, c(list(P), tf)))
    expect_equal(v$point, ref$point)
    expect_equal(v$culprit, ref$culprit)
    expect_equal(v$delta, ref$delta, tolerance = 1e-6)
  }
})

test_that("small deviations stay under the tolerance", {
  P <- fm_test_fish()
  P[5, 2] <- P[3, 2] - 2                        # 2 px for a fish of 1,000 px
  expect_null(.fm_extreme_violations(P))
  P[5, 2] <- P[3, 2] - 20
  expect_false(is.null(.fm_extreme_violations(P)))
})

# The absolute floor protects SMALL images, where the relative tolerance would
# fall below click noise. On the T-26 data compliant specimens top out at
# -0.4 px of overshoot and the smallest real discrepancy is 11.8 px: the band
# 1-8 px is empty, so the floor costs no detection there.
test_that("the absolute floor protects small photographs", {
  P <- fm_test_fish()
  P[, ] <- P[, ] / 4                            # poisson de 250 px : 0.003*L < 1
  P[5, 2] <- P[3, 2] - 3                        # 3 px de depassement = bruit
  expect_null(.fm_extreme_violations(P))
  P[5, 2] <- P[3, 2] - 40                       # ... 40 px = an outright error
  expect_false(is.null(.fm_extreme_violations(P)))
})

test_that("the fix restores the convention, increases Bd and keeps 3/4 on the axis", {
  P <- fm_test_fish(); P[5, 2] <- -190; P[6, 2] <- 200
  g0 <- .fm_body_frame(P)
  v  <- .fm_extreme_violations(P)
  expect_equal(sort(v$point), c(3L, 4L))
  Q  <- .fm_fix_extremes(P, v)
  expect_null(.fm_extreme_violations(Q))        # convention retablie
  g1 <- .fm_body_frame(Q)
  # Bd augmente ...
  expect_gt(abs(g1$no[3] - g1$no[4]), abs(g0$no[3] - g0$no[4]))
  # ... and 3/4 keep their abscissa along the axis (perpendicularity intact)
  expect_equal(g1$ax[3:4], g0$ax[3:4], tolerance = 1e-8)
  # points 3 and 4 are exactly at the height of the offending points
  expect_equal(g1$no[3], g0$no[5], tolerance = 1e-8)
  expect_equal(g1$no[4], g0$no[6], tolerance = 1e-8)
  # no other point has moved
  others <- setdiff(1:25, c(3L, 4L))
  expect_equal(Q[others, ], P[others, ], tolerance = 1e-9)
})

test_that("the fix is invariant to orientation", {
  P <- fm_test_fish(); P[5, 2] <- -190
  M <- fm_test_move(P, theta = 0.4, mirror_y = TRUE)
  Q <- .fm_fix_extremes(P, .fm_extreme_violations(P))
  R <- .fm_fix_extremes(M, .fm_extreme_violations(M))
  expect_equal(R[3, ], fm_test_move(Q, theta = 0.4, mirror_y = TRUE)[3, ],
               tolerance = 1e-8)
})

test_that("a degenerate configuration is refused rather than guessed", {
  P <- fm_test_fish(); P[2, ] <- P[1, ]              # axe de longueur nulle
  expect_null(.fm_body_frame(P))
  expect_null(.fm_extreme_violations(P))
  P <- fm_test_fish(); P[3, ] <- NA_real_            # dos non mesurable
  expect_null(.fm_extreme_violations(P))
  P <- fm_test_fish(); P[4, 2] <- P[3, 2]            # 3 et 4 confondus en hauteur
  expect_null(.fm_extreme_violations(P))
})

test_that("the journal accepts the 'adjusted' status", {
  expect_true("adjusted" %in% .FM_JOURNAL_STATUS)
  jd <- file.path(tempdir(), paste0("fmj_", as.integer(Sys.time())))
  dir.create(jd, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(jd, recursive = TRUE), add = TRUE)
  jr <- fm_journal_open(jd, operator = "test")
  P  <- fm_test_fish()
  fm_journal_append(jr, row_key = "sp1", coords = P, points = c(3L, 4L, 5L),
                    status = c("3" = "adjusted", "4" = "adjusted", "5" = "placed"),
                    species = "Testus testus")
  d <- fm_journal_read(jd)
  expect_equal(sort(d$status[d$landmark %in% c("3", "4")]),
               c("adjusted", "adjusted"))
  expect_equal(d$status[d$landmark == "5"], "placed")
})
