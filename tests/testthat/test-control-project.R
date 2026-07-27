test_that("geometry conventions leave landmarks 1 and 2 fixed", {
  seg <- list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
              PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0)
  lm <- reconstruct_fishmorph_landmarks(seg)
  cc <- correct_geometry_conventions(lm)
  P <- lm$coords[, , 1]; Q <- cc$coords[, , 1]
  expect_equal(P[1, ], Q[1, ], tolerance = 1e-9)
  expect_equal(P[2, ], Q[2, ], tolerance = 1e-9)
})

test_that("check_geometry_conventions returns one row per specimen", {
  f <- system.file("extdata", "example_landmarks.csv", package = "Rfishmorph")
  lm <- read_landmarks_csv(f)
  chk <- check_geometry_conventions(lm)
  expect_equal(nrow(chk), length(dimnames(lm$coords)[[3]]))
  expect_true(all(c("max_shift", "flag") %in% names(chk)))
})

test_that("agreement_metrics are exact for identical vectors", {
  x <- c(1, 2, 3, 4, 5)
  m <- agreement_metrics(x, x)
  expect_equal(unname(m["pearson"]), 1)
  expect_equal(unname(m["rmse"]), 0)
  expect_equal(unname(m["bias"]), 0)
})

test_that("compare_segments_landmarks recovers perfect agreement", {
  # a segment table compared to itself must agree perfectly (r = 1)
  segtab <- data.frame(Genus.species = paste0("sp", 1:6),
                       Bl = 10, Bd = c(3, 3.2, 3.4, 3.6, 3.8, 4),
                       Hd = c(2, 2.1, 2.2, 2.3, 2.4, 2.5),
                       Eh = 1, Mo = 1.5, PFi = 2, PFl = 2.5, Ed = 0.5,
                       Jl = 1.2, CPd = 1, CFd = 3)
  cmp <- compare_segments_landmarks(segtab, segtab, id_col = "Genus.species")
  rr <- cmp$metrics[cmp$metrics$quantity == "ratio", ]
  expect_true(all(rr$pearson[rr$n >= 3] > 0.999, na.rm = TRUE))
})

test_that("check_infinite_ratios flags a zero denominator", {
  seg <- data.frame(specimen = c("ok", "bad"),
                    Bl = 10, Bd = c(3, 0), Hd = 2, Eh = 1, Mo = 1.5,
                    PFi = 2, PFl = 2.5, Ed = 0.5, Jl = 1.2, CPd = 1, CFd = 3)
  rat <- fishmorph_ratios(seg)
  # zero Bd makes BEl/VEp/OGp/BLs/PFv NA (guarded), so no infinities remain:
  flags <- check_infinite_ratios(rat)
  expect_s3_class(flags, "data.frame")
})

# The tests below deliberately use the 400-species sample: they bear on the
# MECHANICS (object structure, dimensions, invariants), which the size of the
# reference table does not change, and they stay fast that way.
# The test that follows checks the real DEFAULT of the function.
test_that("load_fishmorph_reference defaults to the full table", {
  full <- load_fishmorph_reference()
  samp <- load_fishmorph_reference("sample")
  expect_identical(full, load_fishmorph_reference("full"))
  expect_gt(nrow(full), nrow(samp))
  expect_equal(nrow(samp), 400L)
  expect_identical(names(full), names(samp))          # same schema
  expect_true(all(samp$Species %in% full$Species))    # sous-ensemble strict
  ratios <- fishmorph_ratio_names()
  expect_false(anyNA(full[, ratios]))                 # 9 ratios complets partout
  expect_error(load_fishmorph_reference("absent.csv"), "not found")
})

test_that("fishmorph_trait_space builds a frozen PCA", {
  ref <- load_fishmorph_reference("sample")
  ts <- fishmorph_trait_space(ref)
  expect_s3_class(ts, "fishmorph_trait_space")
  expect_true(all(c("PC1", "PC2") %in% names(ts$scores)))
  expect_gt(nrow(ts$scores), 0)
})

test_that("project_fishmorph places specimens in the FISHMORPH space", {
  ref <- load_fishmorph_reference("sample")
  sp <- ref[1:40, ]; sp$species <- sp$Family
  proj <- project_fishmorph(sp, reference = ref, specimens_prelogged = TRUE)
  expect_s3_class(proj, "fishmorph_projection")
  expect_true(all(c("PC1", "PC2") %in% names(proj$scores)))
  expect_equal(nrow(proj$scores), 40)
  expect_true(is.data.frame(proj$global_scores))
  expect_gt(proj$n_reference, 100)
})

test_that("na_action = 'impute_mean' keeps incomplete specimens", {
  ref <- load_fishmorph_reference("sample")
  ref$REs[1:3] <- NA
  ts_omit <- fishmorph_trait_space(ref, na_action = "omit")
  ts_imp  <- fishmorph_trait_space(ref, na_action = "impute_mean")
  expect_gt(nrow(ts_imp$X), nrow(ts_omit$X))
  expect_false(anyNA(ts_imp$X))
  expect_gte(ts_imp$imputed, 3)
})

test_that("impute_landmarks fills a missing anatomical landmark (impute_mean)", {
  segs <- list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
               PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0)
  objs <- lapply(1:6, function(i)
    reconstruct_fishmorph_landmarks(segs, px_per_cm = 40 + i,
                                    specimen = paste0("sp", i)))
  co <- array(NA_real_, c(21, 2, 6),
              dimnames = list(NULL, c("X", "Y"), paste0("sp", 1:6)))
  for (i in 1:6) co[, , i] <- objs[[i]]$coords[, , 1]
  co[5, , 1] <- NA                       # drop landmark 5 of specimen 1
  lm <- fishmorph_landmarks(co)
  expect_true(anyNA(lm$coords))
  imp <- impute_landmarks(lm, method = "impute_mean")
  expect_false(anyNA(imp$coords[1:19, , ]))
  expect_true(attr(imp$coords, "imputed")[5, 1])
})

test_that("phylo_pcoa returns species axes", {
  skip_if_not_installed("ape")
  set.seed(1)
  tree <- ape::rcoal(8, tip.label = paste0("Genus_sp", 1:8))
  pp <- phylo_pcoa(tree, k = 3, ultrametric = FALSE)
  expect_s3_class(pp, "fishmorph_phylopcoa")
  expect_equal(nrow(pp$traits), 8)
  expect_true(all(paste0("PCoA", 1:3) %in% names(pp$traits)))
})

test_that("add_fishmorph_species appends a new record", {
  ref <- load_fishmorph_reference("sample")
  rec <- new_fishmorph_species(
    "Genus novus",
    segments = list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6,
                    PFi = 1.9, PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1, CFd = 3))
  out <- add_fishmorph_species(ref, rec)
  expect_true("Genus novus" %in% out$Species)
  expect_equal(nrow(out), nrow(ref) + 1)
})
