# =============================================================================
# mlmorph.R
#
# Predictor-assisted digitizing: the bridge between launch_fishmorph_digitizer()
# and an ml-morph shape predictor (Porto & Voje 2020; dlib ensemble of
# regression trees), run by a small Python worker shipped in inst/mlmorph/.
#
# The worker and the model contract are SHARED with intraitR::digitize_landmarks()
# -- same predict_new_image.py, same ml_morph/ folder of trained models -- so a
# model trained once serves both applications:
#   <mlmorph_dir>/mlmorph_run_<name>/predictor.dat      the model (19 landmarks),
#       or model.onnx + cnn_params.json for a neural network (train_cnn.py)
#   <mlmorph_dir>/mlmorph_dataset_<name>/               its training set: the
#       {train,val,test}.tps + split_assignments.csv the worker rebuilds the
#       fixed dlib box from, and canon_params.txt (canonical frame: body length,
#       crop, `center=midpoint` / `refine=0`).
#
# The worker is handed the snout (LM1) and caudal base (LM2) clicks, plus a
# dorsal point (LM3, its seed is enough) that orients the fish back up. It
# rebuilds the canonical frame the model was trained in -- axis 1->2
# horizontal, head left, back up, standard length fixed -- predicts the 19
# anatomical landmarks there and maps them back to the pixels of the image.
# Hinges (22, 24, 25) and the scale bar (20, 21) are not predicted.
# =============================================================================

# Model folders listed first in an ml-morph directory: the neural network
# (CNN, heatmaps; the default, best on both sources), the COMBINED dlib model
# (INTRAIT campaign + FISHMORPH species photographs), the campaign-only model,
# then the two earlier T-26 models. Any other mlmorph_run_<name> holding
# a predictor.dat and its mlmorph_dataset_<name> is listed after them.
.FM_MLMORPH_RUNS <- c(cnn      = "mlmorph_run_cnn",
                      combined = "mlmorph_run_combined",
                      campaign = "mlmorph_run_campaign",
                      app      = "mlmorph_run_app",
                      aligned  = "mlmorph_run_aligned")

# First non-empty, existing path of a candidate vector (NA if none).
.fm_first_existing <- function(x, dir = FALSE) {
  x <- x[!is.na(x) & nzchar(x)]
  ok <- if (dir) dir.exists(x) else file.exists(x)
  if (any(ok)) normalizePath(x[ok][1], mustWork = FALSE) else NA_character_
}

#' Locate the ml-morph resources used by the digitizer's "Predict" button
#'
#' Resolves the three things a prediction needs: the folder of trained models,
#' the Python interpreter (with `numpy`, `opencv-python` and `dlib`), and the
#' worker script. Each argument wins when given; otherwise the environment
#' variables, then the conventional locations are tried. Nothing here is
#' required: when no model is found the digitizer opens as usual and the
#' prediction button is simply disabled.
#'
#' @param mlmorph_dir Folder holding `mlmorph_run_*/predictor.dat` (dlib) or
#'   `mlmorph_run_*/model.onnx` (neural network, needs `onnxruntime`) and the
#'   matching `mlmorph_dataset_*/` (`cnn`, `combined`, `campaign`, `app`, `aligned` listed first,
#'   then any other `mlmorph_run_<name>`). `NULL` tries `RFISHMORPH_MLMORPH_DIR`, then
#'   `INTRAITR_MLMORPH_DIR` (the folder is shared with intraitR).
#' @param predictor Optional path of one specific `predictor.dat`, listed first.
#'   Its training set is looked for beside it (`mlmorph_run_X` ->
#'   `mlmorph_dataset_X`).
#' @param python Python interpreter. `NULL` tries `RFISHMORPH_MLMORPH_PY`,
#'   `INTRAITR_MLMORPH_PY`, `~/.venv_mlmorph/bin/python`,
#'   `<mlmorph_dir>/.venv_mlmorph/bin/python`, then `python3`.
#' @param check_python When `TRUE` (default) each candidate interpreter is
#'   asked to import `numpy`, `cv2` and `dlib`, and the first that can is
#'   kept -- a `python3` without them would otherwise be picked silently and
#'   fail at the first prediction.
#' @return A list: `dir`, `python`, `worker`, `models`, a data frame with
#'   one row per usable model (`name`, `predictor`, `dataset`), and `global`,
#'   the path of the GLOBAL network (`mlmorph_run_global/model.onnx`,
#'   `train_global.py`) used by the "Auto" button to find the snout, the caudal
#'   base and the dorsal side without any click (`NA` when absent). That folder
#'   is not a landmark model and is never listed in `models`.
#' @seealso [launch_fishmorph_digitizer()]
#' @examples
#' res <- fm_mlmorph_resources(mlmorph_dir = tempdir())
#' nrow(res$models)   # 0: no model in an empty folder
#' @export
fm_mlmorph_resources <- function(mlmorph_dir = NULL, predictor = NULL,
                                 python = NULL, check_python = TRUE) {
  dir <- .fm_first_existing(c(mlmorph_dir %||% NA_character_,
                              Sys.getenv("RFISHMORPH_MLMORPH_DIR", ""),
                              Sys.getenv("INTRAITR_MLMORPH_DIR", "")), dir = TRUE)
  py <- .fm_pick_python(c(python %||% NA_character_,
                          Sys.getenv("RFISHMORPH_MLMORPH_PY", ""),
                          Sys.getenv("INTRAITR_MLMORPH_PY", ""),
                          "~/.venv_mlmorph/bin/python",
                          if (!is.na(dir)) file.path(dir, ".venv_mlmorph", "bin", "python"),
                          unname(Sys.which("python3"))),
                        check = check_python)
  worker <- system.file("mlmorph", "predict_new_image.py", package = "Rfishmorph")
  if (!nzchar(worker) && !is.na(dir) &&
      file.exists(file.path(dir, "predict_new_image.py")))
    worker <- normalizePath(file.path(dir, "predict_new_image.py"))
  if (!nzchar(worker)) worker <- NA_character_

  # a model is usable only with its training set beside it: the worker rebuilds
  # the fixed box -- and reads the canonical frame -- from that folder
  ds_of <- function(pred) {
    run <- basename(dirname(pred))
    file.path(dirname(dirname(pred)), sub("^mlmorph_run", "mlmorph_dataset", run))
  }
  cand <- character(0)
  if (!is.null(predictor)) cand <- c(supplied = predictor)
  if (!is.na(dir)) {
    # the known models first, in that order, then any other mlmorph_run_<name>
    # (a model trained later -- on FISHMORPH itself, say) under its own name
    runs <- basename(list.dirs(dir, full.names = TRUE, recursive = FALSE))
    runs <- runs[grepl("^mlmorph_run_", runs)]
    # mlmorph_run_global is the whole-photograph locator of the "Auto" mode,
    # not a model of the canonical frame: never offered as a predictor
    other <- setdiff(runs, c(.FM_MLMORPH_RUNS, "mlmorph_run_global"))
    nm <- c(names(.FM_MLMORPH_RUNS), sub("^mlmorph_run_", "", other))
    # a model folder holds either a dlib predictor (predictor.dat) or a neural
    # network exported by train_cnn.py (model.onnx + cnn_params.json)
    run_dirs <- file.path(dir, c(.FM_MLMORPH_RUNS, other))
    files <- ifelse(file.exists(file.path(run_dirs, "model.onnx")),
                    file.path(run_dirs, "model.onnx"), file.path(run_dirs, "predictor.dat"))
    cand <- c(cand, stats::setNames(files, nm))
  }
  keep <- file.exists(cand) & dir.exists(ds_of(cand))
  cand <- cand[keep]
  models <- data.frame(name = names(cand),
                       predictor = unname(normalizePath(cand, mustWork = FALSE)),
                       dataset = unname(normalizePath(ds_of(cand), mustWork = FALSE)),
                       stringsAsFactors = FALSE)
  models <- models[!duplicated(models$predictor), , drop = FALSE]
  glob <- if (!is.na(dir)) file.path(dir, "mlmorph_run_global", "model.onnx") else NA_character_
  glob <- if (!is.na(glob) && file.exists(glob)) normalizePath(glob) else NA_character_
  list(dir = dir, python = py, worker = worker, models = models, global = glob)
}

# Python interpreter: the first candidate that exists and, when `check`, can
# import the worker's dependencies. The path is NEVER normalised: in a virtual
# environment bin/python is a symlink to the base interpreter, and resolving it
# would launch that base interpreter, outside the venv and without its packages
# (numpy, cv2, dlib). If no candidate passes the check, the first existing one
# is returned, so the error the worker raises names what is missing.
.fm_pick_python <- function(cand, check = TRUE) {
  cand <- path.expand(cand[!is.na(cand) & nzchar(cand)])
  cand <- unique(cand[file.exists(cand)])
  if (!length(cand)) return(NA_character_)
  if (!check) return(cand[1])
  for (py in cand) {
    ok <- tryCatch(suppressWarnings(system2(py, c("-c", shQuote("import numpy, cv2, dlib")),
                                            stdout = FALSE, stderr = FALSE)),
                   error = function(e) 1L)
    if (identical(as.integer(ok), 0L)) return(py)
  }
  cand[1]
}

# Command-line arguments of the worker. `P` is the current configuration
# (rows = landmark numbers, image pixels); only LM1, LM2 and, when present,
# LM3 (orientation) are handed over. LM1 and LM2 are NOT pinned: the caller
# keeps the operator's clicks for them and only reads 3..19 back.
# `global`: path of the global network -> fully automatic mode (--auto): LM1,
# LM2 and the dorsal side are found on the whole photograph; a point already
# placed (1, 2, 3) is still passed and takes precedence over the detection.
.fm_mlmorph_args <- function(worker, image, P, predictor, dataset, out, global = NA) {
  fin <- function(i) !is.null(P) && i <= nrow(P) && all(is.finite(P[i, ]))
  auto <- !is.na(global)
  if (!auto && (!fin(1L) || !fin(2L))) stop("LM1 and LM2 must be placed first.", call. = FALSE)
  xy <- function(i) sprintf("%.3f,%.3f", P[i, 1], P[i, 2])
  c(shQuote(worker), "--image", shQuote(image),
    if (auto) c("--auto", "--global-model", shQuote(global)),
    if (fin(1L)) c("--snout", xy(1L)), if (fin(2L)) c("--caudal", xy(2L)),
    if (fin(3L)) c("--dorsal", xy(3L)),
    "--no-pin-clicks",
    "--dataset-dir", shQuote(dataset), "--predictor", shQuote(predictor),
    "--out", shQuote(out))
}

# Runs the worker. Returns a 19 x 2 matrix (rows "1".."19", image pixels), or
# a character string carrying the error when the prediction failed -- the app
# reports it instead of stopping.
.fm_mlmorph_predict <- function(res, model, image, P, auto = FALSE) {
  if (is.na(res$python) || is.na(res$worker))
    return("No Python interpreter or worker script found (see ?fm_mlmorph_resources).")
  m <- res$models[res$models$name == model | res$models$predictor == model, , drop = FALSE]
  if (!nrow(m)) return(paste0("Unknown model: ", model))
  if (auto && (is.null(res$global) || is.na(res$global)))
    return("Auto mode unavailable: no global network (mlmorph_run_global/model.onnx).")
  out <- tempfile(fileext = ".csv"); on.exit(unlink(out), add = TRUE)
  args <- tryCatch(.fm_mlmorph_args(res$worker, image, P, m$predictor[1],
                                    m$dataset[1], out,
                                    global = if (auto) res$global else NA),
                   error = function(e) conditionMessage(e))
  if (length(args) == 1L) return(args)
  log <- tryCatch(suppressWarnings(system2(res$python, args, stdout = TRUE, stderr = TRUE)),
                  error = function(e) conditionMessage(e))
  if (!file.exists(out)) {
    msg <- paste("Prediction failed:", paste(utils::tail(log, 3), collapse = " | "))
    if (any(grepl("onnxruntime", log)))
      msg <- paste0(msg, " -- the neural-network model needs onnxruntime: ",
                    "~/.venv_mlmorph/bin/pip install onnxruntime")
    else if (any(grepl("ModuleNotFoundError|No module named", log)))
      msg <- paste0(msg, " -- the interpreter ", res$python, " lacks numpy/opencv/dlib: ",
                    "pass `python = \"~/.venv_mlmorph/bin/python\"` to ",
                    "launch_fishmorph_digitizer().")
    return(msg)
  }
  d <- utils::read.csv(out)
  # hinges 22/24 of the broken axis, when the model places them
  # (train_cnn.py --hinges): returned as the attribute "hinges"
  h <- d[d$landmark %in% c(22L, 24L) & is.finite(d$X) & is.finite(d$Y), , drop = FALSE]
  d <- d[d$landmark >= 1 & d$landmark <= 19, , drop = FALSE]
  M <- matrix(NA_real_, 19L, 2L, dimnames = list(as.character(1:19), c("X", "Y")))
  M[as.character(d$landmark), ] <- as.matrix(d[, c("X", "Y")])
  if (nrow(h)) attr(M, "hinges") <- matrix(as.matrix(h[, c("X", "Y")]), ncol = 2L,
                                           dimnames = list(as.character(h$landmark), c("X", "Y")))
  M
}
