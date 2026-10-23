# Locate the ml-morph resources used by the digitizer's "Predict" button

Resolves the three things a prediction needs: the folder of trained
models, the Python interpreter (with `numpy`, `opencv-python` and
`dlib`), and the worker script. Each argument wins when given; otherwise
the environment variables, then the conventional locations are tried.
Nothing here is required: when no model is found the digitizer opens as
usual and the prediction button is simply disabled.

## Usage

``` r
fm_mlmorph_resources(
  mlmorph_dir = NULL,
  predictor = NULL,
  python = NULL,
  check_python = TRUE
)
```

## Arguments

- mlmorph_dir:

  Folder holding `mlmorph_run_*/predictor.dat` (dlib) or
  `mlmorph_run_*/model.onnx` (neural network, needs `onnxruntime`) and
  the matching `mlmorph_dataset_*/` (`cnn`, `combined`, `campaign`,
  `app`, `aligned` listed first, then any other `mlmorph_run_<name>`).
  `NULL` tries `RFISHMORPH_MLMORPH_DIR`, then `INTRAITR_MLMORPH_DIR`
  (the folder is shared with intraitR).

- predictor:

  Optional path of one specific `predictor.dat`, listed first. Its
  training set is looked for beside it (`mlmorph_run_X` -\>
  `mlmorph_dataset_X`).

- python:

  Python interpreter. `NULL` tries `RFISHMORPH_MLMORPH_PY`,
  `INTRAITR_MLMORPH_PY`, `~/.venv_mlmorph/bin/python`,
  `<mlmorph_dir>/.venv_mlmorph/bin/python`, then `python3`.

- check_python:

  When `TRUE` (default) each candidate interpreter is asked to import
  `numpy`, `cv2` and `dlib`, and the first that can is kept – a
  `python3` without them would otherwise be picked silently and fail at
  the first prediction.

## Value

A list: `dir`, `python`, `worker`, `models`, a data frame with one row
per usable model (`name`, `predictor`, `dataset`), and `global`, the
path of the GLOBAL network (`mlmorph_run_global/model.onnx`,
`train_global.py`) used by the "Auto" button to find the snout, the
caudal base and the dorsal side without any click (`NA` when absent).
That folder is not a landmark model and is never listed in `models`.

## See also

[`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)

## Examples

``` r
res <- fm_mlmorph_resources(mlmorph_dir = tempdir())
nrow(res$models)   # 0: no model in an empty folder
#> [1] 0
```
