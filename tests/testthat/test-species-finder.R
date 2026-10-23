# Tests for find_fishmorph_species() -- the engine of the "Species finder" tab
# of launch_fishmorph_space().

ref <- data.frame(
  Species = c("Salmo trutta", "Salmo salar", "Gobio gobio", "Gobio occitaniae",
              "Barbus barbus"),
  Family  = c("Salmonidae", "Salmonidae", "Gobionidae", "Gobionidae",
              "Cyprinidae"),
  Order   = c("Salmoniformes", "Salmoniformes", "Cypriniformes",
              "Cypriniformes", "Cypriniformes"),
  IUCN    = c("LC", "LC", "LC", NA, "LC"),
  BEl = 1, VEp = 1, REs = 1, OGp = 1, RMl = 1, BLs = 1, PFv = 1, PFs = 1,
  CPt = c(1, 1, 1, NA, 1),
  stringsAsFactors = FALSE)

test_that("names are cleaned to binomials", {
  cl <- Rfishmorph:::.fm_clean_binomial(
    c("salmo_trutta", "  Salmo   TRUTTA ", "Salmo trutta Linnaeus, 1758",
      "Salmo trutta fario", "Gobio cf. gobio", "Barbus sp.", "Barbus spp",
      "", NA, "1758"))
  expect_identical(cl, c("Salmo trutta", "Salmo trutta", "Salmo trutta",
                         "Salmo trutta", "Gobio gobio", "Barbus", "Barbus",
                         "", "", ""))
})

test_that("the status ladder is exact > close > genus_only > absent", {
  r <- find_fishmorph_species(
    c("salmo_trutta", "Salmo truta", "Salmo marmoratus", "Esox lucius", ""),
    reference = ref)
  expect_identical(r$status,
                   c("exact", "close", "genus_only", "absent", "invalid"))
  expect_identical(r$match[1:2], c("Salmo trutta", "Salmo trutta"))
  expect_identical(r$distance[1:2], c(0L, 1L))
  expect_identical(r$Family[3], "Salmonidae")
  expect_identical(r$n_genus, c(2L, 2L, 2L, 0L, 0L))
})

test_that("max_distance = 0 disables approximate matching", {
  r <- find_fishmorph_species("Salmo truta", reference = ref, max_distance = 0)
  expect_identical(r$status, "genus_only")
  expect_true(is.na(r$match))
  # the near name is still offered as a suggestion, flagged by its distance
  expect_match(r$suggestions, "Salmo trutta [1]", fixed = TRUE)
})

test_that("genus queries list the species of the genus", {
  r <- find_fishmorph_species(c("Gobio sp.", "Esox"), reference = ref)
  expect_identical(r$status, c("genus_only", "absent"))
  expect_identical(r$suggestions[1], "Gobio gobio; Gobio occitaniae")
})

test_that("complete_traits flags species outside the morphological space", {
  r <- find_fishmorph_species(c("Gobio gobio", "Gobio occitaniae"),
                              reference = ref)
  expect_identical(r$complete_traits, c(TRUE, FALSE))
})

test_that("bad arguments fail loudly", {
  expect_error(find_fishmorph_species("x", reference = data.frame(a = 1)),
               "Species")
  expect_error(find_fishmorph_species("x", reference = ref, max_distance = -1),
               "max_distance")
})

test_that("the bundled table answers a known species", {
  p <- fishmorph_space_data("segment")
  skip_if(!nzchar(p), "bundled table not installed")
  r <- find_fishmorph_species("Aaptosyax grypus", source = "segment")
  expect_identical(r$status, "exact")
})
