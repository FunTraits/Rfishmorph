# Global drainage-basin explorer

Launches the 'shiny' application that reads the world's drainage basins
through the FISHMORPH morphological space: a choropleth world map of the
basins with their species composition and their functional richness, the
global functional space reacting to the basin clicked on that map, and a
sortable table of every basin with its country, its biogeographic realm,
its species count and its functional-diversity indices.

## Usage

``` r
launch_fishmorph_basins(
  cache = NULL,
  photos = NULL,
  launch.browser = TRUE,
  ...
)
```

## Arguments

- cache:

  Path to the `.rds` written by
  [`prepare_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/prepare_fishmorph_basins.md).
  `NULL` follows `getOption("Rfishmorph.basin_cache")`, then looks for
  `fishmorph_basins.rds` in the working directory.

- photos:

  Optional folder of species photographs, the same one
  [`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)
  reads (`Genus_species.jpg`, matched on a normalised name). The
  photographs stay LOCAL and are never copied into the cache: 7,588 of
  them are about a gigabyte, and the panel that displays them only ever
  needs one at a time. Without this argument the specimen panel still
  draws the landmarks and the segments, on a blank background.

- launch.browser:

  Where the application opens. `TRUE` (default) or `"browser"` forces
  the system browser, past the RStudio Viewer pane; `"viewer"` restores
  the pane; `FALSE` opens nothing and prints the URL; a function is used
  as given.

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) (for
  example `port`).

## Value

Invisibly `NULL`; called for its side effect.

## Details

The application computes no morphology and no index of its own: it reads
the cache written by
[`prepare_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/prepare_fishmorph_basins.md),
which is where the shapefile, the occurrence tables and the ordination
were resolved once. What it adds is the reading of them – by basin, by
species, by country, by realm.

Selection works on two levels, deliberately kept apart. The FILTERS in
the sidebar – a species, a country, a realm, a list of basins – light up
every basin that satisfies them, and the functional space then shows
those basins pooled. Clicking one polygon FOCUSES it, and the space, the
composition table and the summary then describe that basin alone.
Selecting a species additionally marks it in the space, so that "where
does this fish live, and where does it sit morphologically" is one
question rather than two.

## See also

[`prepare_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/prepare_fishmorph_basins.md)
to build the cache,
[`fishmorph_basin_indices()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_basin_indices.md)
for the indices it reports,
[`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md)
for the species-level morphological space.

## Examples

``` r
if (FALSE) { # \dontrun{
prepare_fishmorph_basins("Bassin/Basin_202412_3364.shp",
                         occurrences = "Occurrence_Table.csv",
                         cas = "Bassin/cas_freshwater_202412.xlsx",
                         out = "Bassin/fishmorph_basins.rds")
launch_fishmorph_basins("Bassin/fishmorph_basins.rds")
} # }
```
