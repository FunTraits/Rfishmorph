# =============================================================================
# defunct.R -- what Rfishmorph no longer does, and where it went
#
# Rfishmorph 0.18.0 is the package of the FISHMORPH REFERENCE and of the
# VISUALISATION of its functional space. Measuring -- the digitizer, its
# append-only journal, the DuckDB database built from it, the ml-morph
# prediction -- moved to intraitR, which merged the two digitizers into one
# application with two targets (the field campaign and the FISHMORPH reference
# workbook). These names are kept for one version so that a script calling them
# stops with the name of the replacement rather than "could not find function".
# =============================================================================

.fm_defunct <- function(old, new, note = NULL) {
  .Defunct(new = new, package = "Rfishmorph", msg = paste0(
    "'", old, "()' was removed from Rfishmorph 0.18.0: measuring moved to ",
    "intraitR. Use ", new, "() instead",
    if (!is.null(note)) paste0(" (", note, ")") else "", "."))
}

#' Functions moved to intraitR (defunct)
#'
#' Rfishmorph no longer measures: the FISHMORPH digitizer, its journal, the
#' DuckDB database and the ml-morph prediction are in intraitR since
#' Rfishmorph 0.18.0 / intraitR 1.44.0. Each of these stops with the name of
#' its replacement:
#'
#' | Rfishmorph (removed)              | intraitR                               |
#' |-----------------------------------|----------------------------------------|
#' | `launch_fishmorph_digitizer()`    | `digitize_fishmorph_reference()`       |
#' | `launch_fishmorph_reconstructor()`| `digitize_fishmorph_reference()`       |
#' | `fm_journal_open()`, `fm_journal_append()` | `landmark_journal_open()`, `landmark_journal_append()` |
#' | `fm_journal_read()`               | `landmark_journal_read()` (reads the old journals too) |
#' | `fm_journal_status()`             | `landmark_journal_status()`            |
#' | `fm_journal_history()`            | `landmark_journal_history()`           |
#' | `fishmorph_consolidate()`         | `consolidate_landmarks()`              |
#' | `fishmorph_journal_qc()`          | `landmark_journal_qc(expect = c(1:19, 22, 23))` |
#' | `fm_save_workbook_atomic()`       | `write_workbook_atomic()`              |
#' | `fishmorph_build_db()`, `fishmorph_db_connect()`, `fishmorph_validate()` | same names in intraitR |
#' | `fm_mlmorph_resources()`          | the `mlmorph_dir`/`predictor`/`python` arguments of `digitize_fishmorph_reference()` |
#'
#' @param ... Ignored.
#' @return Nothing: each stops with an error.
#' @name Rfishmorph-defunct
#' @keywords internal
NULL

#' @rdname Rfishmorph-defunct
#' @export
launch_fishmorph_digitizer <- function(...)
  .fm_defunct("launch_fishmorph_digitizer", "intraitR::digitize_fishmorph_reference",
              "same arguments; new_sheet now defaults to \"New_specimen\"")
#' @rdname Rfishmorph-defunct
#' @export
launch_fishmorph_reconstructor <- function(...)
  .fm_defunct("launch_fishmorph_reconstructor", "intraitR::digitize_fishmorph_reference")
#' @rdname Rfishmorph-defunct
#' @export
fm_journal_open <- function(...)
  .fm_defunct("fm_journal_open", "intraitR::landmark_journal_open")
#' @rdname Rfishmorph-defunct
#' @export
fm_journal_append <- function(...)
  .fm_defunct("fm_journal_append", "intraitR::landmark_journal_append")
#' @rdname Rfishmorph-defunct
#' @export
fm_journal_read <- function(...)
  .fm_defunct("fm_journal_read", "intraitR::landmark_journal_read",
              "it reads the journals of this package too")
#' @rdname Rfishmorph-defunct
#' @export
fm_journal_status <- function(...)
  .fm_defunct("fm_journal_status", "intraitR::landmark_journal_status")
#' @rdname Rfishmorph-defunct
#' @export
fm_journal_history <- function(...)
  .fm_defunct("fm_journal_history", "intraitR::landmark_journal_history")
#' @rdname Rfishmorph-defunct
#' @export
fishmorph_consolidate <- function(...)
  .fm_defunct("fishmorph_consolidate", "intraitR::consolidate_landmarks",
              "long = TRUE, drop_na_points = and status_columns = are there")
#' @rdname Rfishmorph-defunct
#' @export
fishmorph_journal_qc <- function(...)
  .fm_defunct("fishmorph_journal_qc", "intraitR::landmark_journal_qc",
              "pass expect = c(1:19, 22, 23)")
#' @rdname Rfishmorph-defunct
#' @export
fm_save_workbook_atomic <- function(...)
  .fm_defunct("fm_save_workbook_atomic", "intraitR::write_workbook_atomic")
#' @rdname Rfishmorph-defunct
#' @export
fishmorph_build_db <- function(...)
  .fm_defunct("fishmorph_build_db", "intraitR::fishmorph_build_db")
#' @rdname Rfishmorph-defunct
#' @export
fishmorph_db_connect <- function(...)
  .fm_defunct("fishmorph_db_connect", "intraitR::fishmorph_db_connect")
#' @rdname Rfishmorph-defunct
#' @export
fishmorph_validate <- function(...)
  .fm_defunct("fishmorph_validate", "intraitR::fishmorph_validate")
#' @rdname Rfishmorph-defunct
#' @export
fm_mlmorph_resources <- function(...)
  .fm_defunct("fm_mlmorph_resources", "intraitR::digitize_fishmorph_reference",
              "pass mlmorph_dir, predictor and python there")
