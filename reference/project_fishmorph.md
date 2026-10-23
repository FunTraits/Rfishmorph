# Project specimens into the global FISHMORPH functional space

Builds a fixed functional trait space by PCA of a reference database of
FISHMORPH ratios (typically the full FISHMORPH database, ~9,000
species), then places new specimens into that same, frozen space with
[`stats::predict()`](https://rdrr.io/r/stats/predict.html), without
re-estimating the ordination. `intraitR::project_fishmorph()` is a thin
layer over this function that adds the intraspecific-variability
analysis (`itv_proportion()`).

## Usage

``` r
project_fishmorph(
  specimens,
  reference = NULL,
  source = NULL,
  traits = fishmorph_ratio_names(),
  groups = NULL,
  select_species = NULL,
  select_specimens = NULL,
  reference_prelogged = TRUE,
  specimens_prelogged = FALSE,
  log_transform = TRUE,
  scale = TRUE,
  axes = c(1, 2),
  volume_dims = 2L,
  na_action = c("omit", "fail")
)
```

## Arguments

- specimens:

  A data frame (e.g. from
  [`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md))
  with one row per specimen and at least the `traits` columns, plus a
  `species` column or a `groups` argument for colouring.

- reference:

  The reference trait table: a data frame/matrix with the `traits`
  columns, or a path to a delimited file (`;`-separated read with
  [`utils::read.csv2()`](https://rdrr.io/r/utils/read.table.html),
  `,`-separated with
  [`utils::read.csv()`](https://rdrr.io/r/utils/read.table.html)). If
  `NULL` (default), the bundled reference
  ([`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md))
  is used, for the campaign named by `source`.

- source:

  Which bundled reference to use when `reference` is `NULL`: `"segment"`
  or `"landmark"`. `NULL` (default) follows
  `getOption("fishmorph.source", "segment")`, see
  [`set_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md).
  The ordination is always fitted on whichever table is supplied, so
  switching `source` refits the space rather than reprojecting into the
  other one – scores from the two campaigns are not comparable term by
  term.

- traits:

  Character vector of trait columns present on both sides. Defaults to
  the nine FISHMORPH ratios.

- groups:

  Optional grouping vector (one per specimen); defaults to a `species`
  column of `specimens`.

- select_species, select_specimens:

  Optional filters on the projected specimens.

- reference_prelogged, specimens_prelogged:

  Whether each input is already `log10(x + 1)`-transformed. The
  FISHMORPH database is distributed pre-logged
  (`reference_prelogged = TRUE`);
  [`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md)
  returns raw ratios (`specimens_prelogged = FALSE`).

- log_transform:

  Apply `log10(x + 1)` to bring raw inputs onto the reference scale
  (default `TRUE`).

- scale:

  Standardise traits to unit variance in the PCA (default `TRUE`).

- axes:

  Length-2 integer vector of ordination axes (default `c(1, 2)`).

- volume_dims:

  Number of leading principal components on which functional volumes are
  later computed (default `2L`); stored in the result and checked
  against the number of components available.

- na_action:

  `"omit"` (default) drops specimens with missing traits; `"fail"`
  errors.

## Value

An object of class `"fishmorph_projection"` with `scores` (plotted
axes), `scores_all` (all components), `global_scores`,
`global_scores_all`, `global_species`, `groups`, `var_explained`,
`loadings`, `axes`, `volume_dims`, `traits`, `trait_ranges_reference`
(per-trait min/max/range of the reference on the analysis scale),
`reference_traits`, `specimen_traits` (analysis scale), `n_reference`
and `pca`. Has [`print()`](https://rdrr.io/r/base/print.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`plotly_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/plotly_fishmorph.md)
and
[`plot_fishmorph_density()`](https://funtraits.github.io/Rfishmorph/reference/plot_fishmorph_density.md)
methods/companions.

## References

Brosse et al. (2021) *Global Ecology and Biogeography* 30:2330-2336.

## See also

[`plot.fishmorph_projection()`](https://funtraits.github.io/Rfishmorph/reference/plot.fishmorph_projection.md),
[`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md),
[`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)

## Examples

``` r
# \donttest{
ref <- load_fishmorph_reference()
#> FISHMORPH reference: segment (published) -- 8970 species.
# a handful of focal specimens (here, reuse some reference rows as demo)
sp <- ref[1:40, ]; sp$species <- sp$Family
proj <- project_fishmorph(sp, reference = ref, specimens_prelogged = TRUE)
plot(proj, style = "hull")

# }
```
