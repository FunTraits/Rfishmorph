# Common look of the Shiny applications of the FISHMORPH family

One `bslib` theme for the applications of Rfishmorph, intraitR and
FishInTrait, defined once here – the package every other one depends on
– so that a change of look is made in one place.

## Usage

``` r
app_theme(primary = FM_PALETTE[["river"]], ...)
```

## Arguments

- primary:

  Accent colour (buttons, links). The default is the river green-blue of
  the data-management and exploration applications; the digitizer passes
  its own blue, so that its two action buttons stand out.

- ...:

  Further Bootstrap variables, passed to
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html).

## Value

A `bslib` theme object.

## Details

The look is taken from the subject: the colours of a river (deep running
water for the accent, washed shallows for the page, the riverbed for the
text) and of sunlit gravel for the rare second accent (keyboard focus,
the active tab, and the dotted rule under the header – a row of
landmarks along a profile). The type is IBM Plex Sans, drawn for
technical interfaces, with tabular figures in tables of coordinates; its
condensed width sets the titles and card headers, compact in toolbars,
like the labels of an instrument. Species names keep a true italic.

The fonts ship WITH the package (`inst/fonts`, SIL Open Font License)
and are embedded in the compiled stylesheet: an application has to open
in the field, offline or behind a proxy, where
[`bslib::font_google()`](https://rstudio.github.io/sass/reference/font_face.html)
cannot download anything.

The palette is also exposed as CSS variables, for the inline styles of
the applications: `--fm-ink`, `--fm-muted`, `--fm-line`, `--fm-wash`,
`--fm-gravel` and `--fm-river`.

## Examples

``` r
if (requireNamespace("bslib", quietly = TRUE)) {
  th <- app_theme()
  th_digitizer <- app_theme(primary = "#2c5bb8")
}
```
