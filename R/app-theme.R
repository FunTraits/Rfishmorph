#' Common look of the Shiny applications of the FISHMORPH family
#'
#' One `bslib` theme for the applications of Rfishmorph, intraitR and
#' FishInTrait, defined once here -- the package every other one depends on --
#' so that a change of look is made in one place.
#'
#' The look is taken from the subject: the colours of a river (deep running
#' water for the accent, washed shallows for the page, the riverbed for the
#' text) and of sunlit gravel for the rare second accent (keyboard focus, the
#' active tab, and the dotted rule under the header -- a row of landmarks along
#' a profile). The type is IBM Plex Sans, drawn for technical interfaces, with
#' tabular figures in tables of coordinates; its condensed width sets the
#' titles and card headers, compact in toolbars, like the labels of an
#' instrument. Species names keep a true italic.
#'
#' The fonts ship WITH the package (`inst/fonts`, SIL Open Font License) and
#' are embedded in the compiled stylesheet: an application has to open in the
#' field, offline or behind a proxy, where [bslib::font_google()] cannot
#' download anything.
#'
#' The palette is also exposed as CSS variables, for the inline styles of the
#' applications: `--fm-ink`, `--fm-muted`, `--fm-line`, `--fm-wash`,
#' `--fm-gravel` and `--fm-river`.
#'
#' @param primary Accent colour (buttons, links). The default is the river
#'   green-blue of the data-management and exploration applications; the
#'   digitizer passes its own blue, so that its two action buttons stand out.
#' @param ... Further Bootstrap variables, passed to [bslib::bs_theme()].
#' @return A `bslib` theme object.
#' @examples
#' if (requireNamespace("bslib", quietly = TRUE)) {
#'   th <- app_theme()
#'   th_digitizer <- app_theme(primary = "#2c5bb8")
#' }
#' @export
app_theme <- function(primary = FM_PALETTE[["river"]], ...) {
  if (!requireNamespace("bslib", quietly = TRUE))
    stop("Package \"bslib\" is required by app_theme().", call. = FALSE)
  pal <- FM_PALETTE
  sans <- c("'IBM Plex Sans'", "system-ui", "-apple-system", "'Segoe UI'",
            "'Helvetica Neue'", "Arial", "sans-serif")
  cond <- c("'IBM Plex Sans Condensed'", sans)
  th <- bslib::bs_theme(
    version = 5,
    bg = pal[["wash"]], fg = pal[["ink"]],
    primary = primary, secondary = pal[["muted"]],
    success = "#2f7a4f", info = pal[["river"]], warning = pal[["gravel"]],
    danger = "#a83a2c",
    base_font = paste(sans, collapse = ", "),
    heading_font = paste(cond, collapse = ", "),
    "font-size-base" = "0.9375rem",
    "line-height-base" = 1.5,
    "headings-font-weight" = 600,
    "border-radius" = "0.375rem",
    "border-radius-sm" = "0.25rem",
    "border-radius-lg" = "0.5rem",
    "border-color" = pal[["line"]],
    "body-secondary-color" = pal[["muted"]],
    "card-bg" = "#ffffff",
    "card-border-color" = pal[["line"]],
    "card-cap-bg" = "#ffffff",
    "card-box-shadow" = "none",
    "input-bg" = "#ffffff",
    "input-border-color" = "#b9c8c5",
    "input-focus-border-color" = primary,
    "input-btn-focus-width" = "0.2rem",
    "input-btn-focus-color" = "rgba(168, 116, 18, 0.35)",
    "link-decoration" = "none",
    "link-hover-decoration" = "underline",
    "table-border-color" = pal[["line"]],
    "nav-tabs-border-color" = pal[["line"]],
    ...)
  th <- bslib::bs_add_rules(th, .fm_font_faces())
  bslib::bs_add_rules(th, .fm_app_rules(pal))
}

# The palette, named after what it is taken from.
FM_PALETTE <- c(
  river  = "#1d5e6b",   # deep running water: the accent
  ink    = "#1a2a2f",   # the riverbed: text
  muted  = "#566a70",   # wet stone: secondary text
  line   = "#cfdad8",   # hairlines between things
  wash   = "#eef3f2",   # the shallows: the page behind the cards
  gravel = "#c8901a",   # sunlit gravel: the rare second accent
  focus  = "#a87412"    # the same, dark enough for a visible focus ring (3:1)
)

# @font-face rules with the files embedded as data URIs (cached by bslib once
# compiled). Latin + latin-ext: accented French, Portuguese and Estonian names.
.fm_font_faces <- function() {
  dir <- system.file("fonts", package = "Rfishmorph")
  if (!nzchar(dir)) return("")
  if (!requireNamespace("base64enc", quietly = TRUE)) return("")
  ranges <- c(
    latin = paste0("U+0000-00FF,U+0131,U+0152-0153,U+02BB-02BC,U+02C6,U+02DA,",
                   "U+02DC,U+0304,U+0308,U+0329,U+2000-206F,U+20AC,U+2122,",
                   "U+2191,U+2193,U+2212,U+2215,U+FEFF,U+FFFD"),
    `latin-ext` = paste0("U+0100-02BA,U+02BD-02C5,U+02C7-02CC,U+02CE-02D7,",
                         "U+02DD-02FF,U+0304,U+0308,U+0329,U+1D00-1DBF,",
                         "U+1E00-1E9F,U+1EF2-1EFF,U+2020,U+20A0-20AB,",
                         "U+20AD-20C0,U+2113,U+2C60-2C7F,U+A720-A7FF"))
  faces <- rbind(
    data.frame(family = "IBM Plex Sans", stem = "ibm-plex-sans",
               weight = c(400, 500, 600, 400),
               style = c("normal", "normal", "normal", "italic")),
    data.frame(family = "IBM Plex Sans Condensed",
               stem = "ibm-plex-sans-condensed", weight = 600, style = "normal"))
  out <- character(0)
  for (i in seq_len(nrow(faces))) for (sub in names(ranges)) {
    f <- file.path(dir, sprintf("%s-%s-%d-%s.woff2", faces$stem[i], sub,
                                faces$weight[i], faces$style[i]))
    if (!file.exists(f)) next
    b64 <- base64enc::base64encode(f)
    out <- c(out, sprintf(paste0(
      "@font-face{font-family:'%s';font-style:%s;font-weight:%d;",
      "font-display:swap;src:url(data:font/woff2;base64,%s) format('woff2');",
      "unicode-range:%s;}"),
      faces$family[i], faces$style[i], faces$weight[i], b64, ranges[[sub]]))
  }
  paste(out, collapse = "\n")
}

# What the Bootstrap variables cannot say.
.fm_app_rules <- function(pal) {
  sprintf("
:root {
  --fm-river: %s; --fm-ink: %s; --fm-muted: %s; --fm-line: %s;
  --fm-wash: %s; --fm-gravel: %s; --fm-focus: %s;
}
body { font-feature-settings: 'kern' 1; -webkit-font-smoothing: antialiased; }
h1, h2, h3, h4, h5, h6, .card-header, .navbar-brand, .modal-title,
.sidebar-title, .accordion-button {
  font-family: 'IBM Plex Sans Condensed', 'IBM Plex Sans', system-ui, sans-serif;
  font-weight: 600; letter-spacing: 0.005em;
}
em, i, .species { font-style: italic; }

/* The one memorable thing: a row of landmarks along the edge of the header. */
.navbar, .bslib-page-title, .app-title-bar {
  position: relative;
}
.navbar::after, .bslib-page-title::after, .app-title-bar::after {
  content: ''; position: absolute; left: 0; right: 0; bottom: -4px; height: 4px;
  background-image: radial-gradient(circle, var(--fm-gravel) 1.5px, transparent 1.9px);
  background-size: 18px 4px; background-repeat: repeat-x; background-position: 0 50%%;
  pointer-events: none; z-index: 2;
}
.navbar, .navbar.navbar-default {
  --bslib-navbar-default-bg: #ffffff; --bslib-navbar-light-bg: #ffffff;
  background-color: #ffffff !important; border-bottom: 1px solid var(--fm-line);
}
.navbar .navbar-brand { color: var(--fm-ink); font-size: 1.15rem; }
.navbar .nav-link { color: var(--fm-muted); font-weight: 500; }
.navbar .nav-link:hover { color: var(--fm-ink); }
.navbar .nav-link.active {
  color: var(--fm-ink);
  box-shadow: inset 0 -2px 0 var(--fm-gravel);
}

/* Cards: hairline, no shadow; the header names the card, it does not shout. */
.card { box-shadow: none; border-color: var(--fm-line); }
.card-header {
  background: #ffffff; border-bottom: 1px solid var(--fm-line);
  font-size: 0.95rem; color: var(--fm-ink); padding: 0.55rem 1rem;
}
.bslib-sidebar-layout > .sidebar { background: #ffffff; border-color: var(--fm-line); }

/* Buttons: quiet by default, the primary one carries the action. */
.btn { font-weight: 500; }
.btn-link { color: var(--bs-primary); border-color: transparent; background: transparent; }
/* Shiny gives every actionButton btn-default, the coloured ones included:
   only the plain ones are re-dressed. */
.btn-default:not(.btn-primary):not(.btn-success):not(.btn-info):not(.btn-warning):not(.btn-danger):not(.btn-link):not([class*=btn-outline-]),
.btn-secondary, .btn-outline-secondary {
  background-color: #ffffff; border-color: #b9c8c5; color: var(--fm-ink);
}
.btn-default:not(.btn-primary):not(.btn-success):not(.btn-info):not(.btn-warning):not(.btn-danger):not(.btn-link):not([class*=btn-outline-]):hover,
.btn-outline-secondary:hover {
  background-color: var(--fm-wash); border-color: var(--fm-muted); color: var(--fm-ink);
}

/* Keyboard focus, visible on every control. */
:focus-visible, .btn:focus-visible, .form-control:focus, .form-select:focus,
.form-check-input:focus, .nav-link:focus-visible {
  outline: 2px solid var(--fm-focus) !important; outline-offset: 2px;
  box-shadow: none !important;
}

/* A DT table in a card is a fill item and gets a FIXED height (400 px): with
   more rows than that, it spilled over whatever came after it -- a plot, a
   button. These apps are documents that scroll: a table takes its own height. */
.html-widget.datatables { height: auto !important; min-height: 0; }
.html-fill-container > .html-widget.datatables.html-fill-item { flex: 0 0 auto !important; }

/* Tables of coordinates and counts: figures in columns. */
table, .dataTable, .shiny-table, pre, .shiny-text-output { font-variant-numeric: tabular-nums; }
table.dataTable thead th, .table thead th {
  font-family: 'IBM Plex Sans Condensed', 'IBM Plex Sans', sans-serif;
  font-weight: 600; color: var(--fm-ink); border-bottom-color: var(--fm-line) !important;
}
pre { background: #ffffff; border: 1px solid var(--fm-line); color: var(--fm-ink);
  border-radius: 0.375rem; }
/* A page without a navbar (digitizer, photo editor) gets the same title bar. */
.app-title-bar { background: #ffffff; border-bottom: 1px solid var(--fm-line); }
/* Table pagination: light, the current page in the accent. */
.page-link, .dataTables_wrapper .pagination .page-link {
  color: var(--fm-ink); background-color: #ffffff; border-color: var(--fm-line);
}
.page-link:hover { background-color: var(--fm-wash); color: var(--fm-ink); }
.page-item.active .page-link {
  background-color: var(--bs-primary); border-color: var(--bs-primary); color: #ffffff;
}
.page-item.disabled .page-link { background-color: #ffffff; color: #9aaaa8; }
.dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select {
  border: 1px solid #b9c8c5; border-radius: 0.25rem; padding: 2px 6px; background: #ffffff;
}
.shiny-notification { border-radius: 0.375rem; border: 1px solid var(--fm-line);
  box-shadow: 0 6px 18px rgba(26, 42, 47, 0.12); font-size: 0.9rem; }
.shiny-notification-message { border-left: 4px solid var(--fm-river); }
.shiny-notification-warning { border-left: 4px solid var(--fm-gravel); }
.shiny-notification-error { border-left: 4px solid #a83a2c; }
::selection { background: rgba(200, 144, 26, 0.28); }
@media (prefers-reduced-motion: reduce) { * { transition: none !important; } }
", pal[["river"]], pal[["ink"]], pal[["muted"]], pal[["line"]], pal[["wash"]],
     pal[["gravel"]], pal[["focus"]])
}
