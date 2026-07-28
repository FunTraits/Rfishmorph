# Impute missing values in a segment / ratio table

Fills `NA`s in the chosen numeric columns (by default the 11 FISHMORPH
segments) using the same imputation methods as intraitR, so results stay
concordant: column mean, within-group mean, random forest
(`missForest`), or random forest augmented with phylogenetic PCoA axes
(`"missforest_phylo"`, see
[`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md)).
Use it instead of dropping incomplete rows (e.g. your
[`complete.cases()`](https://rdrr.io/r/stats/complete.cases.html)
filter).

## Usage

``` r
impute_traits(
  data,
  cols = fishmorph_segment_names(),
  method = c("missforest_phylo", "missforest", "impute_group_mean", "impute_mean"),
  groups = NULL,
  species = NULL,
  tree = NULL,
  missforest_phylo_k = 10,
  phylo_axes = NULL,
  missforest_ntree = 100,
  missforest_maxiter = 10
)
```

## Arguments

- data:

  A data frame (e.g. the `Global_segments` sheet) with the columns named
  in `cols`.

- cols:

  Columns to impute. Default the 11 segments
  ([`fishmorph_segment_names()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segment_names.md));
  pass
  [`fishmorph_ratio_names()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratio_names.md)
  to impute the 9 ratios instead.

- method:

  `"missforest_phylo"` (default), `"missforest"`, `"impute_group_mean"`
  or `"impute_mean"`.

- groups:

  Optional grouping vector (one per row), used by `"impute_group_mean"`
  (required) and `"missforest"`/`"missforest_phylo"` (as an auxiliary
  predictor; species labels for the phylogeny). If `NULL`, a
  `Genus.species`/`species`/`Species` column of `data` is used when
  present.

- species:

  Species identifier for **each row**, used only to look up the
  phylogenetic axes of `"missforest_phylo"`. `NULL` (default)
  auto-detects a `Genus.species` / `Species` / `species` column. This is
  deliberately separate from `groups`: the phylogeny needs to know which
  species a row belongs to, not a categorical predictor for the forest.

- tree, missforest_phylo_k, missforest_ntree, missforest_maxiter:

  Passed to the missForest methods (see
  [`impute_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/impute_landmarks.md)).

- phylo_axes:

  Used by `"missforest_phylo"`. `NULL` (default) uses the
  **precomputed** axes of
  [`load_fishmorph_phylo_axes()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylo_axes.md),
  so that every call shares one and the same phylogenetic coordinate
  system. Supply a data frame (a `species` column plus one column per
  axis) to use your own.

## Value

`data` with the `cols` imputed. `attr(, "n_imputed")` gives the number
of filled cells.

## See also

[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md),
[`impute_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/impute_landmarks.md)

## Examples

``` r
# \donttest{
# impute the 11 segments of a Global_segments table, by species + phylogeny
seg_imp <- impute_traits(pub_seg, groups = pub_seg$Genus.species)
#> Error: object 'pub_seg' not found
# }
```
