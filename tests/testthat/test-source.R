# Tests for the segment / landmark campaign switch.
#
# The landmark table is a snapshot of an ongoing re-measurement and may simply
# not be installed, so every test that needs it is skipped rather than failed
# when it is absent. What is tested unconditionally is the *resolution logic*:
# who wins between the argument, the option and the default.

test_that("source resolution: argument beats option beats default", {
  old <- getOption("fishmorph.source")
  on.exit(options(fishmorph.source = old), add = TRUE)

  options(fishmorph.source = NULL)
  expect_identical(get_fishmorph_source(), "segment")

  set_fishmorph_source("landmark")
  expect_identical(get_fishmorph_source(), "landmark")

  # The explicit argument must win over the session option, otherwise a script
  # that pins one call would still depend on the state of the session.
  expect_identical(Rfishmorph:::.fm_resolve_source("segment"), "segment")

  expect_error(Rfishmorph:::.fm_resolve_source("landmarks"), "arg")
})

test_that("set_fishmorph_source() returns the previous value", {
  old <- getOption("fishmorph.source")
  on.exit(options(fishmorph.source = old), add = TRUE)

  set_fishmorph_source("segment")
  prev <- set_fishmorph_source("landmark")
  expect_identical(prev, "segment")
  expect_identical(getOption("fishmorph.source"), "landmark")
})

test_that("each source maps to its own bundled file name", {
  expect_identical(Rfishmorph:::.fm_source_file("segment"),
                   "fishmorph_data.csv")
  expect_identical(Rfishmorph:::.fm_source_file("landmark"),
                   "fishmorph_data_landmarks.csv")
})

test_that("the segment table loads and is tagged with its source", {
  ref <- load_fishmorph_reference(source = "segment", quiet = TRUE)
  expect_s3_class(ref, "data.frame")
  expect_true(all(fishmorph_ratio_names() %in% names(ref)))
  expect_identical(attr(ref, "fishmorph_source"), "segment")
})

test_that("loading announces the campaign and the species count", {
  # The message is not decoration: the two tables share their columns but not
  # their species pool, so a silent load is how one ends up comparing spaces
  # built on different pools.
  expect_message(load_fishmorph_reference(source = "segment"),
                 "segment.*[0-9]+ species")
  expect_silent(load_fishmorph_reference(source = "segment", quiet = TRUE))
})

test_that("an absent landmark table fails with an actionable message", {
  skip_if(nzchar(fishmorph_space_data("landmark")),
          "the landmark table is installed")
  expect_error(load_fishmorph_reference(source = "landmark"),
               "build_fishmorph_landmark_table")
})

test_that("the landmark table, when present, matches the segment layout", {
  p <- fishmorph_space_data("landmark")
  skip_if(!nzchar(p), "landmark table not built yet")

  lmk <- load_fishmorph_reference(source = "landmark", quiet = TRUE)
  seg <- load_fishmorph_reference(source = "segment",  quiet = TRUE)

  # Same trait columns and same order: the two must be drop-in substitutes.
  expect_true(all(fishmorph_ratio_names() %in% names(lmk)))
  expect_identical(names(seg)[1:4], names(lmk)[1:4])

  # Same scale, tested on BEl alone. A blanket cap on the nine ratios does not
  # discriminate: BEl = Bl/Bd legitimately reaches 1.62 on the published table
  # (raw 40, an eel), so any threshold low enough to catch a raw table also
  # flags the correct one. BEl is the trait whose two regimes are an order of
  # magnitude apart -- under 2 when logged, above 30 when raw.
  X <- data.matrix(lmk[fishmorph_ratio_names()])
  expect_true(all(X >= 0, na.rm = TRUE))
  expect_lt(max(lmk$BEl, na.rm = TRUE), 3)

  # It is a subset of the pool, by construction, and it must say so.
  expect_lte(nrow(lmk), nrow(seg))
  expect_true("n_imputed" %in% names(lmk))
})

test_that("fishmorph_trait_space() refits on the source it is given", {
  skip_on_cran()
  seg <- load_fishmorph_reference(source = "segment", quiet = TRUE)
  ts <- fishmorph_trait_space(seg[seq_len(200), ])
  expect_s3_class(ts, "fishmorph_trait_space")
  expect_identical(ts$traits, fishmorph_ratio_names())
})

test_that("build_fishmorph_landmark_table() refuses to build from nothing", {
  expect_error(build_fishmorph_landmark_table(),
               "at least one landmark store")
})
