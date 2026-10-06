test_that("schema is internally consistent", {
  sc <- fishmorph_schema()
  expect_length(sc$segments, 11)
  expect_length(sc$ratios, 9)
  # every ratio references defined segments
  segs <- sc$segments
  for (r in sc$ratios) expect_true(all(sc$ratio_def[[r]] %in% segs))
})

test_that("reconstruction round-trip recovers the input segments", {
  seg <- list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
              PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0)
  rt <- check_reconstruction_roundtrip(seg)
  expect_lt(max(rt$abs_error), 1e-6)
})

test_that("round-trip is invariant to free parameters", {
  seg <- list(Bl = 11, Bd = 4, Hd = 2.9, Eh = 1.0, Mo = 1.5, PFi = 2.2,
              PFl = 2.5, Ed = 0.5, Jl = 1.2, CPd = 1.3, CFd = 3.5)
  p <- list(f_Bd = 0.6, o_Bd = 0.3, ang_PFl = -60, ang_Jl = 10)
  rt <- check_reconstruction_roundtrip(seg, params = p)
  expect_lt(max(rt$abs_error), 1e-6)
})

test_that("ratios match their definition", {
  seg <- data.frame(specimen = "a", Bl = 10, Bd = 4, Hd = 2, Eh = 1, Mo = 1.5,
                    PFi = 2, PFl = 2.5, Ed = 0.5, Jl = 1.2, CPd = 1, CFd = 3)
  r <- fishmorph_ratios(seg)
  expect_equal(r$BEl, seg$Bl / seg$Bd)
  expect_equal(r$REs, seg$Ed / seg$Hd)
  expect_equal(r$CPt, seg$CFd / seg$CPd)
})

test_that("ratios are scale-invariant", {
  seg <- list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
              PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0)
  lm1 <- reconstruct_fishmorph_landmarks(seg, px_per_cm = 50)
  lm2 <- reconstruct_fishmorph_landmarks(seg, px_per_cm = 137)
  r1 <- fishmorph_ratios(lm1); r2 <- fishmorph_ratios(lm2)
  expect_equal(as.numeric(r1[1, -1]), as.numeric(r2[1, -1]), tolerance = 1e-8)
})

test_that("landmark I/O round-trips through CSV", {
  f <- system.file("extdata", "example_landmarks.csv", package = "Rfishmorph")
  skip_if(!nzchar(f))
  lm <- read_landmarks_csv(f)
  expect_true(is_fishmorph_landmarks(lm))
  seg <- fishmorph_segments(lm, scale_cm = 1)
  expect_true(all(fishmorph_segment_names() %in% names(seg)))
  expect_true(all(seg$Bl > 0))
})

# ---------------------------------------------------------------------------
# Non-regression: a hinge (22/24/25) projecting OUTSIDE the snout -> caudal-base
# segment made the vector of abscissas non-monotonic, and findInterval() failed
# with "'vec' must be sorted non-decreasingly".
# ---------------------------------------------------------------------------
.mk_fish <- function(hinges = list()) {
  P <- matrix(NA_real_, 25, 2, dimnames = list(NULL, c("X", "Y")))
  P[1, ] <- c(0, 0); P[2, ] <- c(500, 0)          # snout -> caudal axis
  P[3, ] <- c(250, -60); P[4, ] <- c(250, 60)     # body depth
  P[5, ] <- c(80, -40);  P[6, ] <- c(80, 40)      # head
  for (nm in names(hinges)) P[as.integer(nm), ] <- hinges[[nm]]
  fishmorph_landmarks(P, specimen = "sp", pad_to = 25L)
}

test_that("correct_geometry_conventions survives an out-of-range hinge", {
  # hinge UPSTREAM of the snout (negative abscissa)
  upstream <- .mk_fish(list("22" = c(-30, -10)))
  expect_warning(r1 <- correct_geometry_conventions(upstream), "hinge")
  expect_s3_class(r1, "fishmorph_landmarks")
  expect_true(all(is.finite(r1$coords[1:6, , 1])))

  # hinge BEYOND the caudal base (abscissa > Lc)
  beyond <- .mk_fish(list("22" = c(250, -40), "24" = c(560, -20)))
  expect_warning(r2 <- correct_geometry_conventions(beyond), "hinge")
  expect_s3_class(r2, "fishmorph_landmarks")

  # both: no usable hinge at all, the specimen still goes through
  expect_warning(
    r3 <- correct_geometry_conventions(
      .mk_fish(list("22" = c(-30, -10), "24" = c(560, -20)))), "hinge")
  expect_s3_class(r3, "fishmorph_landmarks")
})

test_that("valid hinges are still used, and none triggers a warning", {
  droit  <- .mk_fish(list("22" = c(250, 0)))
  courbe <- .mk_fish(list("22" = c(250, -40), "24" = c(400, -15)))
  expect_warning(a <- correct_geometry_conventions(droit),  regexp = NA)
  expect_warning(b <- correct_geometry_conventions(courbe), regexp = NA)

  # a fish already straight is not moved: LM1 stays at the origin
  expect_equal(unname(a$coords[1, , 1]), c(0, 0), tolerance = 1e-8)

  # Straightening ALIGNS the chain 1 -> 22 -> 24 -> 2 on the direction of the
  # FIRST segment (1 -> 22); the resulting axis is therefore not horizontal, and
  # testing b$coords[2, 2, 1] == 0 would be wrong. The property to check is
  # collinearity: the cross product of two consecutive segments vanishes.
  cross <- function(M, i, j, k)
    (M[j, 1] - M[i, 1]) * (M[k, 2] - M[i, 2]) -
    (M[j, 2] - M[i, 2]) * (M[k, 1] - M[i, 1])
  M <- b$coords[, , 1]
  scale_px <- sqrt(sum((M[2, ] - M[1, ])^2))
  expect_lt(abs(cross(M, 1L, 22L, 24L)) / scale_px^2, 1e-8)
  expect_lt(abs(cross(M, 1L, 24L,  2L)) / scale_px^2, 1e-8)

  # straightening preserves the ARC LENGTH: the straightened chord 1-2 equals
  # the sum of the three segments of the original broken axis.
  O <- courbe$coords[, , 1]
  arc <- sqrt(sum((O[22, ] - O[1, ])^2)) + sqrt(sum((O[24, ] - O[22, ])^2)) +
         sqrt(sum((O[2, ] - O[24, ])^2))
  expect_equal(scale_px, arc, tolerance = 1e-6)
})

test_that("a fish without any hinge is returned unchanged by unbending", {
  plain <- .mk_fish()
  expect_silent(r <- correct_geometry_conventions(plain))
  expect_equal(r$coords[1, , 1], plain$coords[1, , 1], tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# Axes phylogenetiques PRECALCULES (pcoaPhylogenyFish.rds)
# ---------------------------------------------------------------------------
test_that("load_fishmorph_phylo_axes reads the bundled table", {
  ax <- load_fishmorph_phylo_axes()
  expect_s3_class(ax, "data.frame")
  expect_identical(names(ax)[1], "species")
  expect_equal(ncol(ax) - 1L, 10L)                 # 10 axes disponibles
  expect_gt(nrow(ax), 8000L)
  expect_false(anyDuplicated(ax$species) > 0L)
  expect_true(all(vapply(ax[-1], is.numeric, logical(1))))
  # names are canonicalised: Genus_species, never a space nor a dot
  expect_false(any(grepl("[ .]", ax$species)))
  # axes ordered by decreasing eigenvalue -> decreasing variance
  sds <- vapply(ax[-1], stats::sd, numeric(1))
  expect_true(all(diff(sds) < 1e-8))
})

test_that("k truncates, and the session cache returns an identical table", {
  expect_equal(ncol(load_fishmorph_phylo_axes(k = 3)), 4L)
  expect_equal(ncol(load_fishmorph_phylo_axes(k = 99)), 11L)   # borne a 10
  expect_identical(load_fishmorph_phylo_axes(),
                   load_fishmorph_phylo_axes(refresh = TRUE))
})

test_that("phylo axes are broadcast to a groups vector without recomputing", {
  sp  <- c("Coilia.nasus", "Aaptosyax grypus", "Coilia_nasus", NA,
           "Espece.inexistante")
  pax <- Rfishmorph:::.phylo_axes_for_species(sp, k_phylo = 4)
  expect_equal(pax$source, "precomputed axis table")
  expect_equal(pax$k_used, 4L)
  expect_equal(nrow(pax$axes), length(sp))         # one row per element
  # dot/space/underscore are equivalent: same axes for the same species
  expect_equal(pax$axes[1, ], pax$axes[3, ], ignore_attr = TRUE)
  # species absent -> NA, and not an error
  expect_true(all(is.na(pax$axes[5, ])))
  expect_equal(pax$n_matched, 2L)                  # nasus + grypus
})

test_that("an explicit axis table takes precedence, and a bad one is reported", {
  fake <- data.frame(species = c("Coilia_nasus", "Aaptosyax_grypus"),
                     a = c(1, 2), b = c(3, 4))
  pax <- Rfishmorph:::.phylo_axes_for_species(c("Coilia.nasus", "Aaptosyax.grypus"),
                                             k_phylo = 10, axes = fake)
  expect_equal(pax$source, "supplied axis table")
  expect_equal(pax$k_used, 2L)                     # bounded by the number of columns
  expect_equal(unname(pax$axes[[1]]), c(1, 2))

  bad <- Rfishmorph:::.phylo_axes_for_species("Coilia.nasus",
                                             axes = data.frame(x = 1))
  expect_null(bad$axes)
  expect_match(bad$reason, "species")
})

test_that("missforest_phylo works WITHOUT groups (species come from the data)", {
  skip_if_not_installed("missForest")
  set.seed(1)
  ref <- load_fishmorph_reference("sample")
  ref$REs[1:5] <- NA; ref$BEl[6:8] <- NA
  # no `groups`: the species is detected on its own and serves only as a key
  # to cbind the phylogenetic axes. That is all missforest_phylo asks for --
  # there is no grouping factor to supply.
  expect_warning(
    imp <- impute_traits(ref, cols = fishmorph_ratio_names(),
                         method = "missforest_phylo"),
    regexp = NA)
  expect_false(anyNA(imp[, fishmorph_ratio_names()]))
  expect_equal(attr(imp, "n_imputed"), 8L)
})

test_that("species and groups are independent arguments", {
  ref <- load_fishmorph_reference("sample")
  # explicit `species`, with no `groups` at all
  pax <- Rfishmorph:::.phylo_axes_for_species(ref$Species, k_phylo = 5)
  expect_equal(pax$k_used, 5L)
  expect_equal(pax$n_matched, length(unique(ref$Species)))
  # and with no species at all, we say so plainly
  none <- Rfishmorph:::.phylo_axes_for_species(NULL)
  expect_null(none$axes)
  expect_match(none$reason, "species")
})

test_that("a groups factor with too many levels is dropped, not fatal", {
  skip_if_not_installed("missForest")
  set.seed(2)
  ref <- load_fishmorph_reference("sample")
  ref$REs[1:3] <- NA
  # 400 levels: randomForest refuses more than 53 -> a warning, not an error
  expect_warning(
    imp <- impute_traits(ref, cols = fishmorph_ratio_names(),
                         method = "missforest", groups = ref$Species),
    "53 categories")
  expect_false(anyNA(imp[, fishmorph_ratio_names()]))
})
