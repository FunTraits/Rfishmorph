# Impute missing (NA) landmark coordinates

Faithful port of `intraitR::impute_landmarks()`. Estimates missing 2D
coordinates directly in the landmark array. `"tps"`/`"regression"` use
[`geomorph::estimate.missing()`](https://rdrr.io/pkg/geomorph/man/estimate.missing.html)
(thin-plate spline / multivariate regression on the geometric
covariation among landmarks); `"impute_mean"`, `"impute_group_mean"`,
`"missforest"` and `"missforest_phylo"` treat each coordinate as a
numeric variable and impute it statistically, mirroring
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)'s
`na_action`. `"missforest_phylo"` augments the random-forest predictors
with phylogenetic PCoA axes (see
[`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md)).

## Usage

``` r
impute_landmarks(
  landmarks,
  method = c("tps", "regression", "impute_mean", "impute_group_mean", "missforest",
    "missforest_phylo"),
  groups = NULL,
  species = NULL,
  missforest_ntree = 100,
  missforest_maxiter = 10,
  tree = NULL,
  missforest_phylo_k = 10,
  phylo_axes = NULL
)
```

## Arguments

- landmarks:

  A `fishmorph_landmarks` (or `intrait_landmarks`) object, or a raw p x
  k x n array, with at least one `NA` coordinate.

- method:

  One of `"tps"` (default), `"regression"`, `"impute_mean"`,
  `"impute_group_mean"`, `"missforest"`, `"missforest_phylo"`.

- groups:

  Optional factor/character, one value per specimen (species labels).
  Auto-detected from `metadata$species` when available. Required by
  `"impute_group_mean"`, and by `"missforest_phylo"` for phylogenetic
  matching.

- species:

  Species identifier for **each row**, used only to look up the
  phylogenetic axes of `"missforest_phylo"`. `NULL` (default)
  auto-detects a `Genus.species` / `Species` / `species` column. This is
  deliberately separate from `groups`: the phylogeny needs to know which
  species a row belongs to, not a categorical predictor for the forest.

- missforest_ntree, missforest_maxiter:

  Passed to
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html)
  for the missForest methods.

- tree:

  Used by `"missforest_phylo"`: a `"phylo"` object, or `NULL` (default)
  to use
  [`load_fishmorph_phylogeny()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylogeny.md).

- missforest_phylo_k:

  Max number of phylogenetic PCoA axes to add (default 10, the number
  available in the precomputed table).

- phylo_axes:

  Used by `"missforest_phylo"`. `NULL` (default) uses the
  **precomputed** axes of
  [`load_fishmorph_phylo_axes()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylo_axes.md),
  so that every call shares one and the same phylogenetic coordinate
  system. Supply a data frame (a `species` column plus one column per
  axis) to use your own.

## Value

An object of the same class as `landmarks`, with landmarks 1-19
completed.

## Details

Only anatomical landmarks 1-19 are imputed; the scale bar (20-21) and
the optional curvature point (22) are left untouched. The returned
`coords` carry an `"imputed"` attribute (a p x n logical matrix) marking
estimated points.

## References

Stekhoven & Buhlmann (2012) *Bioinformatics* 28:112-118.

## See also

[`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md),
[`load_fishmorph_phylogeny()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylogeny.md),
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
