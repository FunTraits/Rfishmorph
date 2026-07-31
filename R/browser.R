# =============================================================================
# browser.R -- opening an application where it can actually be used.
#
# THE PROBLEM
#   `shiny::runApp(launch.browser = TRUE)` ends in `utils::browseURL()`, which
#   goes through `options("browser")`. Inside RStudio that option is REPLACED by
#   a handler that keeps a localhost URL inside the IDE, so the application
#   lands in the Viewer pane -- a few hundred pixels wide, with no address bar,
#   no second tab and a JavaScript engine that is not the one the interface was
#   built against. Asking for a browser and getting a pane is not a preference
#   issue: a leaflet map or a plotly figure in that pane is unusable.
#
# THE FIX, AND WHY IT IS LOOKED UP RATHER THAN ASSUMED
#   RStudio exposes its own "external window" handler. It is fetched BY NAME
#   from `tools:rstudio`, and every failure -- not in RStudio, name gone in a
#   future version, environment not attached -- falls back to `browseURL()`.
#   A launcher must never abort because an internal name of another program
#   moved.
#
# The same three functions exist, identically, in the other two packages.
# Three packages, one behaviour, and no cross-dependency for six lines.
# =============================================================================

# RStudio's external-window handler, or NULL.
.fm_rstudio_external <- function() {
  if (!nzchar(Sys.getenv("RSTUDIO"))) return(NULL)
  e <- tryCatch(as.environment("tools:rstudio"), error = function(...) NULL)
  if (is.null(e)) return(NULL)
  nm <- ".rs.invokeShinyWindowExternal"
  if (!exists(nm, envir = e, inherits = FALSE)) return(NULL)
  f <- tryCatch(get(nm, envir = e), error = function(...) NULL)
  if (is.function(f)) f else NULL
}

# Resolve what the caller asked for into what runApp() needs.
#
#   TRUE / "browser"  the system browser, forced past the IDE (the default)
#   FALSE             open nothing; the URL is printed and can be pasted
#   "viewer"          RStudio's pane, for whoever wants it back
#   a function        used as given
.fm_browser <- function(launch.browser = TRUE) {
  if (is.function(launch.browser)) return(launch.browser)
  if (isFALSE(launch.browser)) return(FALSE)
  if (is.character(launch.browser) && length(launch.browser) == 1L) {
    if (identical(launch.browser, "viewer"))
      return(getOption("shiny.launch.browser", TRUE))
    if (!identical(launch.browser, "browser"))
      stop("`launch.browser` must be TRUE, FALSE, \"browser\", \"viewer\", ",
           "or a function.", call. = FALSE)
  }
  ext <- .fm_rstudio_external()
  if (!is.null(ext)) return(ext)
  # Outside RStudio browseURL() already opens the system browser; inside it,
  # this line is only reached when the handler could not be found, and it is
  # still better than refusing to launch.
  function(url) utils::browseURL(url)
}
