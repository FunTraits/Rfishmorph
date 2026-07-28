# Load the FISHMORPH reference database

Reads the published FISHMORPH trait table (9 ratios + maximum body
length and width + taxonomy + IUCN status). Since Rfishmorph 0.2.0 the
default is the **full table** (`fishmorph_data.csv`, 8,970 species with
all 9 ratios complete), which is also what
[`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md)
explores. The 400-species sample is still bundled and reachable with
`file = "sample"` for fast examples and tests.

## Usage

``` r
load_fishmorph_reference(
  file = NULL,
  sheet = NULL,
  source = NULL,
  quiet = FALSE
)
```

## Arguments

- file:

  Path to a FISHMORPH CSV (`;`-separated) or XLSX file. `NULL` (default)
  loads the full bundled table for the active `source`; `"sample"` loads
  the 400-species segment sample; `"full"` is an explicit synonym of
  `NULL`.

- sheet:

  Sheet name when `file` is an XLSX (default `"Global_ratios"` then the
  first sheet).

- source:

  Which measurement campaign to read: `"segment"` (the published table)
  or `"landmark"` (the re-digitized one). Defaults to
  `getOption("fishmorph.source", "segment")`. Ignored when `file` is an
  explicit path.

- quiet:

  Suppress the one-line message reporting which table was loaded and how
  many species it holds (default `FALSE`). That message exists so an
  analysis can never silently run on the partial landmark pool.

## Value

A data frame with columns `Species, Family, Order, Genus`, the 9 ratio
columns, `MBl`, `MBw` and `IUCN` when available.

## Trait scale

The bundled tables are **already** `log10(x + 1)` transformed, both the
full one and the sample. Do not transform them again: calling
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
with `log = TRUE` on this output would take the logarithm twice and
silently distort the ordination. Ratios recomputed from landmarks with
[`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md)
are on the raw scale and *do* need it.

## Segment- vs landmark-derived traits

Two measurement campaigns describe the same species pool.
`source = "segment"` reads the published table, whose ratios come from
the eleven segments measured on the plates (Brosse et al. 2021).
`source = "landmark"` reads `fishmorph_data_landmarks.csv`, whose ratios
are recomputed from the landmark re-digitization through
[`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md),
and which therefore covers **only the species already digitized** –
fewer rows, reported on load. The two tables share their column names,
separator and `log10(x + 1)` scale, so they are interchangeable wherever
a reference is expected. The default is read from
`getOption("fishmorph.source")` and can be set once per session with
[`set_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md).
See
[`build_fishmorph_landmark_table()`](https://funtraits.github.io/Rfishmorph/reference/build_fishmorph_landmark_table.md)
for how the landmark table is produced.

## See also

[`fishmorph_space_data()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_space_data.md)
for the path of the bundled full table,
[`set_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md),
[`build_fishmorph_landmark_table()`](https://funtraits.github.io/Rfishmorph/reference/build_fishmorph_landmark_table.md),
[`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md)
to explore it interactively.

## Examples

``` r
ref <- load_fishmorph_reference()          # 8,970 species (segments)
#> FISHMORPH reference: segment (published) -- 8970 species.
nrow(ref)
#> [1] 8970
small <- load_fishmorph_reference("sample")  # 400 species
#> FISHMORPH reference: segment (published) -- 400 species.
if (FALSE) { # \dontrun{
lmk <- load_fishmorph_reference(source = "landmark")  # re-digitized subset
} # }
```
