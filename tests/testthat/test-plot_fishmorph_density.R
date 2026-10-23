# Moved from intraitR (tests/testthat/test-plot_fishmorph_density.R) in Rfishmorph 0.17.0, with the code it tests.

traits9 <- c("REs", "VEp", "RMl", "OGp", "BEl", "BLs", "PFv", "PFs", "CPt")

make_reference <- function(n = 80, seed = 3) {
  set.seed(seed)
  m <- matrix(runif(n * length(traits9), 0.05, 0.9), nrow = n)
  colnames(m) <- traits9
  df <- as.data.frame(m)
  rownames(df) <- paste0("ref", seq_len(n))
  df
}

make_projection <- function() {
  ref <- make_reference()
  sp <- ref[1:16, traits9]
  sp$species <- rep(c("Genus alpha", "Genus beta"), each = 8)
  project_fishmorph(sp, reference = ref,
                    reference_prelogged = TRUE, specimens_prelogged = TRUE)
}

test_that("project_fishmorph() stores the reference trait matrix", {
  proj <- make_projection()
  expect_true(!is.null(proj$reference_traits))
  expect_equal(ncol(proj$reference_traits), length(traits9))
  expect_setequal(colnames(proj$reference_traits), traits9)
  expect_equal(nrow(proj$reference_traits), proj$n_reference)
})

test_that("reconstruction fallback matches the stored reference traits", {
  proj <- make_projection()
  M <- proj$global_scores_all %*% t(proj$loadings)
  if (is.numeric(proj$pca$scale))  M <- sweep(M, 2, proj$pca$scale, "*")
  if (is.numeric(proj$pca$center)) M <- sweep(M, 2, proj$pca$center, "+")
  colnames(M) <- rownames(proj$loadings)
  expect_equal(unname(M[, traits9]), unname(proj$reference_traits[, traits9]),
               tolerance = 1e-8)
})

test_that("plot_fishmorph_density() runs for axes, ratios and both", {
  proj <- make_projection()
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  out <- plot_fishmorph_density(proj)                       # both (default)
  expect_type(out, "list")
  # one element per axis (2) + one per ratio (9)
  expect_length(out, length(proj$axes) + length(traits9))
  expect_true(all(c("reference") %in% names(out[[1]])))

  expect_invisible(plot_fishmorph_density(proj, what = "axes"))
  expect_invisible(plot_fishmorph_density(proj, what = "ratios"))
  expect_invisible(plot_fishmorph_density(proj, what = "ratios",
                                          traits = c("BEl", "REs")))
  expect_invisible(plot_fishmorph_density(proj, what = "axes", axes = 1:3))
  expect_invisible(plot_fishmorph_density(proj, select_species = "Genus alpha",
                                          legend = FALSE))
  # percent-of-max axis + translucent species fills toggles
  expect_invisible(plot_fishmorph_density(proj, species_fill = FALSE))
  expect_invisible(plot_fishmorph_density(proj, what = "axes",
                                          species_fill_alpha = 0.4,
                                          reference_fill = FALSE))
})

test_that("plot_fishmorph_density() validates its arguments", {
  proj <- make_projection()
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(plot_fishmorph_density(proj, what = "ratios", traits = "NOPE"),
               "not in this space")
  expect_error(plot_fishmorph_density(proj, what = "axes", axes = 999),
               "components")
  expect_error(plot_fishmorph_density(proj, select_species = "absent"),
               "match the selection")
  expect_error(plot_fishmorph_density(list()),
               "fishmorph_projection")
})

test_that("plot_fishmorph_density() works on a projection lacking reference_traits", {
  proj <- make_projection()
  proj$reference_traits <- NULL   # emulate a projection built before this field
  grDevices::pdf(file = tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot_fishmorph_density(proj, what = "ratios"))
})

test_that("project_fishmorph() carries the fields the ITV analysis needs", {
  proj <- make_projection()
  expect_s3_class(proj, "fishmorph_projection")
  expect_equal(proj$volume_dims, 2L)
  expect_equal(ncol(proj$scores_all), length(traits9))
  expect_equal(nrow(proj$specimen_traits), nrow(proj$scores))
  expect_setequal(proj$trait_ranges_reference$trait, traits9)
  expect_equal(proj$trait_ranges_reference$range,
               proj$trait_ranges_reference$max - proj$trait_ranges_reference$min)
  ref <- make_reference()
  sp <- ref[1:6, traits9]; sp$species <- "A"
  expect_error(project_fishmorph(sp, reference = ref, volume_dims = 99,
                                 specimens_prelogged = TRUE), "volume_dims")
})
