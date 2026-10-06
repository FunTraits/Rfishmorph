# Build the cache read by the basin explorer

Reads the four sources of the global drainage-basin view – the basin
polygons, one or two species-by-basin occurrence tables, and the
FISHMORPH trait table – ordinates the trait table once, computes the
functional-diversity indices of every basin under every source, and
writes the result as a single `.rds` file that
[`launch_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_basins.md)
reads in under a second.

## Usage

``` r
prepare_fishmorph_basins(
  shapefile,
  occurrences = NULL,
  cas = NULL,
  landmarks = NULL,
  landmarks_db = NULL,
  rivers = NULL,
  rivers_max_order = 5L,
  rivers_tolerance = 0.005,
  traits = NULL,
  traits_landmark = NULL,
  size_traits = c("MBl", "MBw"),
  out = "fishmorph_basins.rds",
  simplify_keep = 0.03,
  axes_ric = 1:2,
  axes_dist = 1:4,
  n_axes = 4L,
  scale = TRUE,
  quiet = FALSE
)

# S3 method for class 'fishmorph_basin_cache'
print(x, ...)
```

## Arguments

- shapefile:

  Path to the basin polygons (`.shp`, with its sidecar
  `.dbf`/`.shx`/`.prj`, or any other format
  [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
  accepts). The attribute table is expected to carry a basin name, and
  optionally a basin identifier, a country, a biogeographic realm and a
  centroid; the column names are matched loosely, and a missing centroid
  is computed.

- occurrences:

  Path to a Tedesco-style occurrence CSV with columns `Basin`, `Species`
  and, if available, `Status` (`native` / `exotic`). `NULL` to skip that
  source.

- cas:

  Path to the Catalogue of Fishes freshwater workbook, whose `basin`
  column holds semicolon-separated basin names. `NULL` to skip.

- landmarks:

  Optional path to the FISHMORPH publication workbook (sheets
  `Global_Landmark` and `Global_segments`, keyed on `Genus.species`).
  Adding it lets the application draw each species the way
  [`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)
  does – landmarks, body segments and reference lines – read-only. Only
  the COORDINATES are stored: the photographs stay on disk and are found
  at launch through
  [`launch_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_basins.md)'s
  `photos` argument.

- landmarks_db:

  Optional path to the digitizer's DuckDB store
  ([`fishmorph_build_db()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_build_db.md)).
  Its coordinates OVERRIDE the workbook's, species by species – the same
  precedence
  [`build_fishmorph_landmark_table()`](https://funtraits.github.io/Rfishmorph/reference/build_fishmorph_landmark_table.md)
  applies, and for the same reason: the workbook is rewritten every
  `xlsx_flush_every` records while the journal is written at every one,
  so a workbook lagging its journal is the normal state of an active
  campaign. Without this argument the specimen panel and the comparison
  can show measurements that the ordination has already left behind.

- rivers:

  Optional path to a HydroRIVERS layer (Lehner and Grill 2013), global
  `HydroRIVERS_v10.gdb` / `.shp` or a regional tile, downloaded from
  <https://www.hydrosheds.org>. `NULL` skips it and the application
  falls back on a raster tile overlay. Adding it makes the river network
  a piece of DATA carried by the cache rather than a third-party service
  that can be retired.

- rivers_max_order:

  Highest `ORD_FLOW` class kept, `1` to `10`. The class is a LOGARITHMIC
  DISCHARGE class running the counter-intuitive way: `1` is a reach
  carrying at least 100,000 cubic metres per second, `4` at least 100,
  `5` at least 10, `6` at least 1, `10` less than 0.001. Keeping
  `ORD_FLOW <= n` keeps the BIG rivers, and the cost climbs by roughly
  an order of magnitude per class: about 40,000 reaches at `4`, 200,000
  at `5`, 1.5 million at `6`. Defaults to `5`, which draws a network
  dense enough to read a mid-sized basin without making the cache
  unusable. The number of reaches actually kept is reported, so a first
  run tells you whether to go up or down.

- rivers_tolerance:

  Simplification tolerance of the reaches, in degrees. Defaults to
  `0.005`, roughly 550 m at the equator – the resolution of the 15
  arc-second grid HydroRIVERS was extracted from, so the thinning
  removes vertices the source never resolved rather than geometry it
  did. `0` disables it.

- traits:

  Path to the trait table of the SEGMENT campaign. `NULL` uses
  [`fishmorph_space_data()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_space_data.md)`("segment")`,
  whose trait columns are ALREADY log10(x + 1) – they are not
  transformed again here.

- traits_landmark:

  Path to the trait table of the LANDMARK campaign, the one
  [`build_fishmorph_landmark_table()`](https://funtraits.github.io/Rfishmorph/reference/build_fishmorph_landmark_table.md)
  produces. `NULL` uses
  [`fishmorph_space_data()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_space_data.md)`("landmark")`
  when it is bundled. Every campaign found gets its OWN ordination and
  its own set of basin indices, and the application switches between
  them.

  The two spaces are not comparable point for point: independent
  principal component analyses on different species pools give different
  axes, so a species does not keep its coordinates across campaigns and
  neither do the hull-based indices. What they can be compared on is the
  RANKING of basins and the sign of a contrast, not the value of `FRic`.

- size_traits:

  Which SIZE variables join the nine dimensionless ratios in the
  ordination: any of `"MBl"` (maximum body length) and `"MBw"` (maximum
  body weight), or `character(0)` for none. Both by default.

  This is a choice about what the functional space MEANS, not a
  technical setting. The nine ratios are quotients of two lengths
  measured on the same fish; `MBl` and `MBw` are species attributes from
  FishBase, stored as log10(x + 1) of centimetres and grams. Including
  them makes the first axis largely a size axis – on the published table
  their loadings on PC1 are -0.51 and -0.51, ahead of every shape ratio
  – and the resulting `FRic` is then not comparable with the FISHMORPH
  space of Brosse et al. (2021), which is built on the ratios alone. The
  two are also nearly collinear (Pearson r = 0.925), so asking for both
  lets size enter twice.

  Whatever is asked for also enters the missing-data accounting: a
  species without a body length then leaves the space entirely rather
  than sitting in it at an invented size.

- out:

  Path of the `.rds` cache to write.

- simplify_keep:

  Proportion of vertices kept when simplifying the polygons, in
  `(0, 1]`. `1` keeps the geometry untouched, which makes a very large
  cache and a sluggish map. Defaults to `0.03`.

- axes_ric, axes_dist:

  Axes used by the two families of index, passed to
  [`fishmorph_basin_indices()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_basin_indices.md).

- n_axes:

  Number of ordination axes stored in the cache. Defaults to `4`.

- scale:

  Passed to [`stats::prcomp()`](https://rdrr.io/r/stats/prcomp.html):
  standardise the nine ratios before the ordination. `TRUE` by default,
  and it should stay `TRUE` – the ratios are dimensionless but not
  commensurate, and an unstandardised PCA lets the most variable ratio
  write the first axis on its own.

- quiet:

  Suppress the progress messages.

- x:

  A `"fishmorph_basin_cache"`.

- ...:

  Unused.

## Value

Invisibly, the cache (a list), which is also written to `out`.

[`print()`](https://rdrr.io/r/base/print.html) invisibly returns `x`.

## Details

Everything expensive happens here, on purpose. Nothing the application
computes depends on what the user clicks, so nothing the application
shows needs to be computed while they wait.

## See also

[`launch_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_basins.md),
[`fishmorph_basin_indices()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_basin_indices.md)

## Examples

``` r
if (FALSE) { # \dontrun{
prepare_fishmorph_basins(
  shapefile   = "Bassin/Basin_202412_3364.shp",
  occurrences = "Occurrence_Table.csv",
  cas         = "Bassin/cas_freshwater_202412.xlsx",
  out         = "Bassin/fishmorph_basins.rds")
} # }
```
