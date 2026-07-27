# Convention des extremes : 3 = point le plus DORSAL, 4 = le plus VENTRAL.
# Les fonctions testees sont internes a l'application de digitalisation
# (R/digitizer-app.R) et travaillent en pixels image, Y vers le bas.

# poisson synthetique : 25 lignes, dos vers les Y negatifs, tete a gauche.
fm_test_fish <- function() {
  P <- matrix(NA_real_, 25, 2, dimnames = list(NULL, c("X", "Y")))
  pts <- list(
    "1" = c(0, 0), "2" = c(1000, 0), "3" = c(470, -120), "4" = c(470, 130),
    "5" = c(100, -100), "6" = c(100, 90), "7" = c(100, 20), "8" = c(100, 80),
    "9" = c(0, 60), "10" = c(250, -30), "11" = c(250, 110), "12" = c(400, 90),
    "13" = c(100, -10), "14" = c(100, 50), "15" = c(60, 40),
    "16" = c(930, -55), "17" = c(930, 55), "18" = c(1000, -130),
    "19" = c(1000, 130), "22" = c(500, 0), "23" = c(300, 40))
  for (k in names(pts)) P[as.integer(k), ] <- pts[[k]]
  P
}

# similitude (rotation + miroir eventuel + translation) : les conventions
# doivent etre invariantes a l'orientation de la photo.
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
  P <- fm_test_fish(); P[5, 2] <- -190          # haut de tete au-dessus du dos
  v <- .fm_extreme_violations(P)
  expect_equal(nrow(v), 1L)
  expect_equal(v$point, 3L)
  expect_equal(v$culprit, 5L)
  expect_equal(v$delta, 70, tolerance = 1e-6)

  Q <- fm_test_fish(); Q[11, 2] <- 200          # ventre pectoral sous le ventre
  w <- .fm_extreme_violations(Q)
  expect_equal(w$point, 4L)
  expect_equal(w$culprit, 11L)
  expect_equal(w$delta, 70, tolerance = 1e-6)
})

test_that("caudal (16-19) and appendage tips (12, 15) are exempt", {
  P <- fm_test_fish()
  P[16, 2] <- -400; P[18, 2] <- -600            # caudale tres haute
  P[19, 2] <- 600;  P[17, 2] <- 400             # ... et tres basse
  P[12, 2] <- 400                               # pectorale sous le ventre
  P[15, 2] <- 300                               # machoire sous le ventre
  expect_null(.fm_extreme_violations(P))
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
  P[5, 2] <- P[3, 2] - 2                        # 2 px pour un poisson de 1000 px
  expect_null(.fm_extreme_violations(P))
  P[5, 2] <- P[3, 2] - 20
  expect_false(is.null(.fm_extreme_violations(P)))
})

test_that("the fix restores the convention, increases Bd and keeps 3/4 on the axis", {
  P <- fm_test_fish(); P[5, 2] <- -190; P[11, 2] <- 200
  g0 <- .fm_body_frame(P)
  v  <- .fm_extreme_violations(P)
  expect_equal(sort(v$point), c(3L, 4L))
  Q  <- .fm_fix_extremes(P, v)
  expect_null(.fm_extreme_violations(Q))        # convention retablie
  g1 <- .fm_body_frame(Q)
  # Bd augmente ...
  expect_gt(abs(g1$no[3] - g1$no[4]), abs(g0$no[3] - g0$no[4]))
  # ... et 3/4 gardent leur abscisse le long de l'axe (perpendicularite intacte)
  expect_equal(g1$ax[3:4], g0$ax[3:4], tolerance = 1e-8)
  # les points 3 et 4 sont exactement a la hauteur des points fautifs
  expect_equal(g1$no[3], g0$no[5],  tolerance = 1e-8)
  expect_equal(g1$no[4], g0$no[11], tolerance = 1e-8)
  # aucun autre point n'a bouge
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
