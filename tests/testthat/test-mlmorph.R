# ml-morph bridge of the digitizer: resource discovery and the worker's command
# line. The prediction itself needs Python + dlib and is not run here.

fake_mlmorph <- function(runs = c("campaign", "app"), with_dataset = TRUE) {
  d <- tempfile("mlmorph_"); dir.create(d)
  for (r in runs) {
    dir.create(file.path(d, paste0("mlmorph_run_", r)))
    writeLines("x", file.path(d, paste0("mlmorph_run_", r), "predictor.dat"))
    if (with_dataset) dir.create(file.path(d, paste0("mlmorph_dataset_", r)))
  }
  d
}

test_that("an empty folder yields no model, without error", {
  res <- fm_mlmorph_resources(mlmorph_dir = tempdir())
  expect_s3_class(res$models, "data.frame")
  expect_equal(nrow(res$models), 0L)
})

test_that("models are found campaign first, and only with their training set", {
  d <- fake_mlmorph(c("app", "campaign"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  res <- fm_mlmorph_resources(mlmorph_dir = d)
  expect_equal(res$models$name, c("campaign", "app"))
  expect_true(all(dir.exists(res$models$dataset)))
  expect_match(basename(res$models$dataset[1]), "^mlmorph_dataset_campaign$")

  d2 <- fake_mlmorph("campaign", with_dataset = FALSE)
  on.exit(unlink(d2, recursive = TRUE), add = TRUE)
  expect_equal(nrow(fm_mlmorph_resources(mlmorph_dir = d2)$models), 0L)
})

test_that("any other mlmorph_run_<name> is listed after the known ones", {
  d <- fake_mlmorph(c("fishmorph", "campaign"))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  expect_equal(fm_mlmorph_resources(mlmorph_dir = d)$models$name,
               c("campaign", "fishmorph"))
})

test_that("a supplied predictor is listed first and not duplicated", {
  d <- fake_mlmorph("campaign")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  p <- file.path(d, "mlmorph_run_campaign", "predictor.dat")
  res <- fm_mlmorph_resources(mlmorph_dir = d, predictor = p)
  expect_equal(res$models$name, "supplied")
})

test_that("the worker command line carries the axis, the dorsal hint, no pin", {
  P <- matrix(NA_real_, 25, 2)
  P[1, ] <- c(100, 200); P[2, ] <- c(900, 210); P[3, ] <- c(500, 120)
  a <- Rfishmorph:::.fm_mlmorph_args("w.py", "img.jpg", P,
                                                        "pred.dat", "ds", "out.csv")
  expect_true(all(c("--snout", "--caudal", "--dorsal", "--no-pin-clicks") %in% a))
  expect_equal(a[which(a == "--snout") + 1L], "100.000,200.000")
  P[3, ] <- NA
  a2 <- Rfishmorph:::.fm_mlmorph_args("w.py", "img.jpg", P, "pred.dat", "ds", "out.csv")
  expect_false("--dorsal" %in% a2)
  P[1, ] <- NA
  expect_error(Rfishmorph:::.fm_mlmorph_args("w.py", "img.jpg", P, "p", "d", "o"),
               "LM1 and LM2")
})

test_that("the worker script is shipped with the package", {
  expect_true(nzchar(system.file("mlmorph", "predict_new_image.py",
                                 package = "Rfishmorph")))
})

test_that("'predicted' is part of the journal vocabulary", {
  expect_true("predicted" %in% Rfishmorph:::.FM_JOURNAL_STATUS)
})

test_that("the interpreter path is kept as given (a venv symlink is not resolved)", {
  py3 <- unname(Sys.which("python3")); skip_if(!nzchar(py3))
  d <- tempfile("venv_"); dir.create(file.path(d, "bin"), recursive = TRUE)
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  link <- file.path(d, "bin", "python"); file.symlink(py3, link)
  expect_identical(Rfishmorph:::.fm_pick_python(link, check = FALSE), link)
})

test_that("an interpreter that cannot import the dependencies is skipped", {
  skip_on_os("windows")
  bad <- tempfile("badpy_"); writeLines(c("#!/bin/sh", "exit 1"), bad); Sys.chmod(bad, "755")
  good <- tempfile("goodpy_"); writeLines(c("#!/bin/sh", "exit 0"), good); Sys.chmod(good, "755")
  on.exit(unlink(c(bad, good)), add = TRUE)
  expect_identical(Rfishmorph:::.fm_pick_python(c(bad, good)), good)
  # none passes -> the first existing one, so the worker's error names the gap
  expect_identical(Rfishmorph:::.fm_pick_python(c(bad, tempfile())), bad)
  expect_true(is.na(Rfishmorph:::.fm_pick_python(c(tempfile(), ""))))
})

test_that("a neural-network folder (model.onnx) is found like a dlib one", {
  d <- fake_mlmorph("combined")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "mlmorph_run_cnn")); dir.create(file.path(d, "mlmorph_dataset_cnn"))
  writeLines("x", file.path(d, "mlmorph_run_cnn", "model.onnx"))
  res <- fm_mlmorph_resources(mlmorph_dir = d, check_python = FALSE)
  expect_true("cnn" %in% res$models$name)
  expect_match(res$models$predictor[res$models$name == "cnn"], "model\\.onnx$")
})

test_that("auto mode: no click required, the global network is passed, clicks kept", {
  P <- matrix(NA_real_, 19, 2)
  a <- Rfishmorph:::.fm_mlmorph_args("w.py", "img.jpg", P, "m.onnx", "ds", "o.csv",
                                     global = "g/model.onnx")
  expect_true("--auto" %in% a)
  expect_true(any(grepl("g/model.onnx", a, fixed = TRUE)))
  expect_false("--snout" %in% a)
  P[1, ] <- c(10, 20)
  a2 <- Rfishmorph:::.fm_mlmorph_args("w.py", "img.jpg", P, "m.onnx", "ds", "o.csv",
                                      global = "g/model.onnx")
  expect_true("--snout" %in% a2); expect_false("--caudal" %in% a2)
})

test_that("the global network is found but never listed as a landmark model", {
  d <- fake_mlmorph("combined")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  dir.create(file.path(d, "mlmorph_run_global")); dir.create(file.path(d, "mlmorph_dataset_global"))
  writeLines("x", file.path(d, "mlmorph_run_global", "model.onnx"))
  res <- fm_mlmorph_resources(mlmorph_dir = d, check_python = FALSE)
  expect_false("global" %in% res$models$name)
  expect_match(res$global, "mlmorph_run_global/model\\.onnx$")
  res0 <- fm_mlmorph_resources(mlmorph_dir = fake_mlmorph("combined"), check_python = FALSE)
  expect_true(is.na(res0$global))
})
