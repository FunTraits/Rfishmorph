# Build the landmark-derived FISHMORPH trait table

Recomputes the nine FISHMORPH ratios from landmark configurations
instead of reading them from the published segment measurements, and
assembles a table with the same layout as `fishmorph_data.csv` so that
the two can be swapped with
[`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)'s
`source` argument.

## Usage

``` r
build_fishmorph_landmark_table(
  xlsx = NULL,
  db = NULL,
  sheet = "Global_Landmark",
  metadata = NULL,
  na_action = c("missforest_phylo", "missforest", "impute_mean", "impute_group_mean",
    "omit", "keep"),
  log = TRUE,
  file = NULL,
  missforest_ntree = 100,
  missforest_maxiter = 10,
  tree = NULL,
  missforest_phylo_k = 10,
  phylo_axes = NULL,
  verbose = TRUE
)
```

## Arguments

- xlsx:

  Path to the FISHMORPH publication workbook holding the landmark sheet
  (`FISHMORPH_PUBLI_9556sp_reconstructed.xlsx`). `NULL` skips it.

- db:

  Path to the digitizer DuckDB store (`fishmorph.duckdb`), read through
  its `v_landmarks_wide` view. `NULL` skips it. When a species is
  present in both stores the DuckDB record wins, being the more recent
  digitization.

- sheet:

  Landmark sheet name in `xlsx` (default `"Global_Landmark"`).

- metadata:

  Table supplying the **non-morphometric** columns only – `Species`,
  `Family`, `Order`, `Genus`, `MBl`, `MBw`, `IUCN` – joined by species
  name. `NULL` (default) reads them from the segment table, which is
  simply where they are stored. The join is restricted to that
  whitelist, so no ratio can cross over even if the supplied table
  carries some. This argument is deliberately *not* called `reference`:
  it does not define the trait space and plays no part in the segments,
  ratios or imputation.

- na_action:

  How to fill missing ratios: `"missforest_phylo"` (default),
  `"missforest"`, `"impute_mean"`, `"impute_group_mean"`, `"omit"` or
  `"keep"` (leave `NA`).

- log:

  Return the ratios on the `log10(x + 1)` scale (default `TRUE`),
  matching `fishmorph_data.csv`. Imputation always runs on the raw
  scale.

- file:

  Optional path to write the table as a `;`-separated CSV.

- missforest_ntree, missforest_maxiter, tree, missforest_phylo_k,
  phylo_axes:

  Passed through to the imputation, see
  [`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md).

- verbose:

  Report coverage and imputation counts (default `TRUE`).

## Value

A data frame with `Species, Family, Order, Genus`, the nine ratios,
`MBl`, `MBw`, `IUCN`, plus the provenance columns `store` (`"xlsx"` or
`"duckdb"`) and `n_imputed`.

## Independence from the segment table

The two campaigns are separate measurements and are kept separate here.
Segments, ratios, imputation and, downstream, the ordination are derived
from the landmark geometry alone: at no point is a segment measurement
read, copied or used as a prior. The segment table is touched once, at
the very end, and only to attach columns that no landmark configuration
could produce – taxonomy, IUCN status, and the FishBase maximum body
length and width. Those are species attributes rather than measurements
on the plate, so they are the same whichever campaign is read; the
segment CSV is only the file that stores them. Supply `metadata` to take
them from anywhere else.

## Why a second table

Segment measurements fix the eleven segment *lengths* but say nothing
about where the depth segments sit along the body axis, nor how a depth
is split between the dorsal and ventral sides. Landmark digitizing
supplies exactly that missing information, so the position ratios
(`OGp`, `VEp`, `PFv`) are the ones that genuinely change; the size
ratios are expected to stay close to their segment values. Do not read a
high segment/landmark correlation as validation: for several traits it
is a property of the construction.

## Coverage

The table contains **only the species that have actually been
digitized**, so it is shorter than `fishmorph_data.csv` (8,970 species)
and grows with the re-measurement campaign. Every function that builds a
trait space on it therefore describes a smaller species pool;
[`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)
reports the row count so this can never happen silently.

## Missing ratios

A ratio is `NA` when one of its two segments could not be measured,
whether because the structure is absent (no pectoral fin, no caudal fin,
terminal mouth) or because the specimen is only partly digitized.
`na_action` decides what happens then; the default `"missforest_phylo"`
imputes on the **raw** scale using the precomputed phylogenetic PCoA
axes
([`load_fishmorph_phylo_axes()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylo_axes.md)),
i.e. the same coordinate system as every other imputation in the
package. The per-species `n_imputed` column records how many of the nine
ratios were filled in, so imputed species can always be excluded
downstream.

## See also

[`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)
and its `source` argument,
[`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md),
[`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md)

## Examples

``` r
if (FALSE) { # \dontrun{
tab <- build_fishmorph_landmark_table(
  xlsx = "FishMORPH/FISHMORPH_PUBLI_9556sp_reconstructed.xlsx",
  db   = "FishMORPH/fishmorph.duckdb",
  file = "inst/extdata/fishmorph_data_landmarks.csv")
} # }
```
