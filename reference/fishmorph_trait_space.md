# Build the FISHMORPH functional trait space

Fits a principal component analysis on a set of (optionally
log-transformed and scaled) morphological traits. This is the ordination
used to describe the FISHMORPH functional space; once fitted it is
frozen and reused to project new specimens with
[`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md),
so every projection lives in the same coordinate system.

## Usage

``` r
fishmorph_trait_space(
  data = NULL,
  source = NULL,
  traits = fishmorph_ratio_names(),
  groups = NULL,
  log = FALSE,
  scale = TRUE,
  na_action = c("omit", "fail", "impute_mean", "impute_group_mean", "missforest",
    "missforest_phylo"),
  missforest_ntree = 100,
  missforest_maxiter = 10,
  tree = NULL,
  missforest_phylo_k = 10,
  phylo_axes = NULL,
  species = NULL
)
```

## Arguments

- data:

  A data frame of traits, or a `fishmorph_landmarks` object (its ratios
  are computed first). `NULL` (default) fits the space on the bundled
  reference of the campaign named by `source`.

- source:

  Which bundled table to fit when `data` is `NULL`: `"segment"` or
  `"landmark"`. `NULL` (default) follows
  `getOption("fishmorph.source", "segment")`, see
  [`set_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md).
  The PCA is refitted on the chosen table, so the two campaigns define
  two distinct coordinate systems: axis order and sign may differ, and
  scores are not comparable across campaigns without a Procrustes
  alignment.

- traits:

  Character vector of trait columns. Defaults to the 9 FISHMORPH ratios.

- groups:

  Optional grouping vector (e.g. species) used by the plot method.

- log:

  Apply a `log10(x + 1)` transform to traits before ordination (default
  `FALSE`), matching
  [`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md)
  and the FISHMORPH scale. Several FISHMORPH ratios are position ratios
  that legitimately reach 0 (e.g. `OGp`, `VEp`, `PFv`); log-transforming
  them would set those specimens to `NA`. Enable it only for size-like,
  strictly positive trait sets.

- scale:

  Scale traits to unit variance (default `TRUE`).

- na_action:

  How to handle specimens with missing traits. Same options as
  `intraitR::trait_space()`, so the two packages stay concordant:
  `"omit"` (default, drop incomplete rows), `"fail"`, `"impute_mean"`,
  `"impute_group_mean"` (needs `groups`), `"missforest"` (random-forest
  imputation via missForest), or `"missforest_phylo"` (missForest
  augmented with phylogenetic PCoA axes; see
  [`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md)).
  Imputation is performed on the raw trait scale (before any log
  transform).

- missforest_ntree, missforest_maxiter:

  Passed to
  [`missForest::missForest()`](https://rdrr.io/pkg/missForest/man/missForest.html)
  for the missForest `na_action`s.

- tree:

  Used by `na_action = "missforest_phylo"`: a `"phylo"` object, or
  `NULL` (default) to use
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

- species:

  Species identifier for **each row**, used only to look up the
  phylogenetic axes of `"missforest_phylo"`. `NULL` (default)
  auto-detects a `Genus.species` / `Species` / `species` column. This is
  deliberately separate from `groups`: the phylogeny needs to know which
  species a row belongs to, not a categorical predictor for the forest.

## Value

An object of class `fishmorph_trait_space` with elements `pca` (the
`prcomp` object), `scores`, `X` (the trait matrix used), `traits`,
`groups`, `log`, `scale`, `ids`, `na_action` and `imputed` (number of
filled cells).

## Examples

``` r
ref <- load_fishmorph_reference()
#> FISHMORPH reference: segment (published) -- 8970 species.
ts <- fishmorph_trait_space(ref, groups = ref$Family)
ts
#> <fishmorph_trait_space>
#>   specimens : 8970
#>   traits    : BEl, VEp, REs, OGp, RMl, BLs, PFv, PFs, CPt
#>   transform : no log10, scaled
#>   variance  : PC1 26.4%, PC2 20.8% (cum 47.2%)
#>   na_action : omit
# \donttest{
# keep incomplete specimens by imputing with phylogenetically-informed
# random forests (as in intraitR), using species as groups:
ts2 <- fishmorph_trait_space(ref, groups = ref$Species,
                             na_action = "missforest_phylo")
# }
```
