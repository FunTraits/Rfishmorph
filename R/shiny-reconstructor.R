# =============================================================================
# shiny-reconstructor.R -- backward compatibility.
#
# The digitizing tool now lives in R/digitizer-app.R under the name
# launch_fishmorph_digitizer(). The former prototype (a single photograph loaded
# by upload, parameters on sliders, CSV export) has been removed: maintaining
# two implementations of the SAME geometry guaranteed that they would eventually
# drift apart, and that is exactly the kind of silent divergence which produces
# two incomparable sets of landmarks inside one database.
#
# This file keeps nothing but a deprecated alias.
# =============================================================================

#' Launch the reconstruction tool (deprecated)
#'
#' Since version 0.2.0 the digitizing tool is [launch_fishmorph_digitizer()],
#' which works directly on the FISHMORPH workbook, handles three working queues
#' (reconstruct, correct, new photographs) and secures every record with an
#' append-only journal.
#'
#' The former prototype took one photograph at a time and exported an isolated
#' CSV: there is no exact correspondence between its arguments and those of the
#' new tool, which requires a workbook. This function warns, then redirects.
#'
#' @param segments_csv Ignored. Kept so that existing calls do not break.
#' @param ... Passed to [launch_fishmorph_digitizer()].
#' @return Invisibly `NULL`.
#' @seealso [launch_fishmorph_digitizer()]
#' @export
launch_fishmorph_reconstructor <- function(segments_csv = NULL, ...) {
  if (!is.null(segments_csv))
    warning("`segments_csv` is no longer used: the new tool reads the segments ",
            "straight from the FISHMORPH workbook.", call. = FALSE)
  .Deprecated(
    new = "launch_fishmorph_digitizer",
    package = "Rfishmorph",
    msg = paste0(
      "launch_fishmorph_reconstructor() is deprecated since Rfishmorph 0.2.0.\n",
      "  Use launch_fishmorph_digitizer(xlsx_path =, photo_dir =, mode =):\n",
      "  the tool works on the FISHMORPH workbook and journals every ",
      "record."))
  launch_fishmorph_digitizer(...)
}
