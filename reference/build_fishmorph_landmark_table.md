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
  new_sheet = c("New_specimen", "new_specimens"),
  metadata = NULL,
  fishbase_size = FALSE,
  impute_size = TRUE,
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

  Landmark sheet(s) in `xlsx` holding the PUBLISHED specimens (default
  `"Global_Landmark"`).

- new_sheet:

  Sheet(s) of `xlsx` holding the specimens digitized SINCE the
  publication – species absent from FISHMORPH, entered either through
  the digitizer's "new" queue or through the "Absent from FISHMORPH"
  panel of FishInTrait. Both spellings in use are looked for by default;
  those the workbook does not carry are simply skipped. `NULL` ignores
  them, which is the behaviour of the versions before this argument
  existed. A species present in both `sheet` and `new_sheet` keeps its
  `sheet` record: the new sheets are a STAGING AREA, promoted to
  `Global_Landmark` once validated, so a duplicate means the promotion
  has already happened and the staged row is the stale one. The `store`
  column of the returned table names the origin (`"xlsx"`, `"xlsx_new"`
  or `"duckdb"`), so a table built on unvalidated specimens can always
  be traced back. Mind that one row of a staging sheet is one PHOTOGRAPH
  and not one species – several specimens of the same species are the
  point of the plate mode – whereas this table is one row per species:
  the first row met is kept and the others are dropped, without
  averaging. Pooling repeated specimens is a decision about the
  campaign, not a detail of the reading.

- metadata:

  Table supplying the **non-morphometric** columns only – `Species`,
  `Family`, `Order`, `Genus`, `MBl`, `MBw`, `IUCN` – joined by species
  name. `NULL` (default) reads them from the segment table, which is
  simply where they are stored. The join is restricted to that
  whitelist, so no ratio can cross over even if the supplied table
  carries some. This argument is deliberately *not* called `reference`:
  it does not define the trait space and plays no part in the segments,
  ratios or imputation.

- fishbase_size:

  Fill the `MBl` / `MBw` the metadata table cannot supply from FishBase,
  through
  [`fishmorph_fishbase_size()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_fishbase_size.md)
  (default `FALSE`: it needs the network and the `rfishbase` package). A
  species digitized since the publication is by construction absent from
  the segment table, where those two columns live, so it comes out sized
  `NA` – and
  [`prepare_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/prepare_fishmorph_basins.md)
  runs [`complete.cases()`](https://rdrr.io/r/stats/complete.cases.html)
  over the ratios AND `size_traits`, which drops it from the functional
  space altogether. The maximum STANDARD length and the maximum weight
  are converted to `log10(x + 1)`, the scale of the rest of the table.
  An existing value is never overwritten, and the added `size_source`
  column (`"fishmorph_publi"` / `"fishbase"`) keeps the two apart –
  without it a derived size becomes indistinguishable from a published
  one at the next read, and the origin of a point in the ordination is
  lost.

- impute_size:

  Impute the `MBl` / `MBw` still missing after the metadata join and, if
  asked for, after FishBase (default `TRUE`). A SECOND pass, run with
  the same `na_action`, in which the nine ratios – complete by then –
  and the phylogenetic axes predict the size, and nothing predicts the
  ratios back: the values already in the table are left bit for bit as
  they were. `"keep"` and `"omit"` are not honoured here, the first
  because it means leaving the gaps and the second because dropping a
  species for want of a size is a decision for the analysis, not for the
  reading. Imputed sizes are marked `size_source = "imputed"`, and they
  should be: an invented body length weighs on the first axis exactly
  like a measured one, and no other column tells them apart. One column
  cannot describe two: `size_source` records the last operation that
  touched EITHER of `MBl` and `MBw`. In practice the two are missing
  together – both come from the same join, and the current table has
  exactly 466 of each – so the ambiguity is theoretical; split it into
  two columns if that ever stops holding.

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
`MBl`, `MBw`, `IUCN`, plus the provenance columns `store` (`"xlsx"`,
`"xlsx_new"` or `"duckdb"`), `size_source` (`"fishmorph_publi"`,
`"fishbase"` or `"imputed"`) and `n_imputed`. `n_imputed` counts RATIOS
only: a size that was imputed is reported by `size_source`, not by it.

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
