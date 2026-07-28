# =============================================================================
# space-app.R -- launcher for the morphological space explorer.
#
# The application itself (formerly the standalone "FishMorphSpace" project)
# lives in inst/shiny/fishmorph_space/app.R. It is NOT rewritten here: this file
# only checks its dependencies, resolves the data set, and launches it.
#
# The default data set is bundled in inst/extdata/fishmorph_data.csv (9,556
# species). CAUTION: its trait columns are ALREADY log10(x + 1) -- do not
# transform them again before projecting new individuals.
# =============================================================================

# Dependencies of the visualisation app. They are in Suggests: anyone who only
# wants to compute ratios has no reason to install plotly or DT.
.FMS_DEPS <- c("shiny", "bslib", "ggplot2", "dplyr", "tidyr", "DT",
               "RColorBrewer", "scales", "ggrepel", "plotly", "MASS", "magrittr")

# Check a set of dependencies and produce an ACTIONABLE installation message
# (the install.packages() line, ready to copy) rather than a failure on the
# first missing library() inside the app.
.fm_require <- function(pkgs, what) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(miss)) return(invisible(TRUE))
  stop(what, " requires ", length(miss), " package(s) that are not installed: ",
       paste(miss, collapse = ", "), "\n  install.packages(c(",
       paste0('"', miss, '"', collapse = ", "), "))", call. = FALSE)
}

#' Morphological space explorer for freshwater fishes
#'
#' Launches the 'shiny' application that explores the global morphological space
#' of freshwater fishes from the FISHMORPH database (Brosse et al. 2021):
#' principal component analysis of the nine dimensionless traits, density of the
#' functional space, colouring by order or by IUCN status, and projection of a
#' user-supplied set of species.
#'
#' @param data Path to a trait CSV (semicolon-separated) replacing the bundled
#'   data set. Useful to work on a more recent version, or on the result of
#'   [fishmorph_build_db()] exported to CSV. `NULL` uses the data set bundled
#'   with the package.
#' @param source Which measurement campaign to explore: `"segment"` or
#'   `"landmark"`. `NULL` (default) follows
#'   `getOption("fishmorph.source", "segment")`. Ignored when `data` is given.
#' @param launch.browser Open in the default browser.
#' @param ... Passed to [shiny::runApp()] (for example `port`).
#' @return Invisibly `NULL`; called for its side effect.
#' @seealso [launch_fishmorph_digitizer()] to produce the landmarks,
#'   [set_fishmorph_source()],
#'   [project_fishmorph()] to project specimens by computation rather than
#'   interactively.
#' @examples
#' \dontrun{
#' launch_fishmorph_space()
#' launch_fishmorph_space(source = "landmark")
#' launch_fishmorph_space(data = "my_traits.csv")
#' }
#' @export
launch_fishmorph_space <- function(data = NULL, source = NULL,
                                   launch.browser = TRUE, ...) {
  .fm_require(.FMS_DEPS, "The morphological space explorer")

  appdir <- system.file("shiny", "fishmorph_space", package = "Rfishmorph")
  if (!nzchar(appdir) || !file.exists(file.path(appdir, "app.R")))
    stop("Application not found in the package. Reinstall 'Rfishmorph'.",
         call. = FALSE)

  if (is.null(data)) {
    src <- .fm_resolve_source(source)
    data <- fishmorph_space_data(src)
    if (!nzchar(data))
      stop("The \"", src, "\" table is not bundled with this installation.",
           if (identical(src, "landmark"))
             "\n  Build it first with build_fishmorph_landmark_table()." else "",
           call. = FALSE)
  }
  if (!is.null(data)) {
    if (!file.exists(data))
      stop("Data set not found: ", data, call. = FALSE)
    old <- options(Rfishmorph.space_data = normalizePath(data))
    on.exit(options(old), add = TRUE)
  }
  shiny::runApp(appdir, launch.browser = launch.browser, ...)
  invisible(NULL)
}

#' Path of the bundled FISHMORPH trait table
#'
#' Returns the path of the CSV used by default by [launch_fishmorph_space()],
#' to inspect it or read it directly.
#'
#' A reminder: its trait columns are ALREADY log10(x + 1). Reading them back to
#' project new individuals therefore needs no further transformation, and
#' applying one would distort the projection.
#'
#' @param source Which measurement campaign to point at: `"segment"` (the
#'   published table) or `"landmark"` (traits recomputed from the landmark
#'   re-digitization). `NULL` (default) follows
#'   `getOption("fishmorph.source", "segment")`, see [set_fishmorph_source()].
#' @return A file path (character string), or `""` when the requested table is
#'   not bundled -- which is the normal state of the landmark table until
#'   [build_fishmorph_landmark_table()] has been run.
#' @seealso [set_fishmorph_source()], [load_fishmorph_reference()]
#' @examples
#' p <- fishmorph_space_data()
#' if (nzchar(p)) utils::head(utils::read.csv(p, sep = ";"), 3)
#' @export
fishmorph_space_data <- function(source = NULL)
  system.file("extdata", .fm_source_file(.fm_resolve_source(source)),
              package = "Rfishmorph")
