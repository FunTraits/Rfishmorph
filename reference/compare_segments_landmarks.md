# Compare landmark-derived and published FISHMORPH measurements

Verifies the internal consistency of a FISHMORPH workbook by comparing,
for each specimen that carries both, the morphology measured from
digitized landmarks with the published segment measurements. Two
scale-invariant quantities are compared: the 9 ratios (each a quotient
of segments, so the pixel-to-cm factor cancels) and the segments
expressed as a fraction of body length. A large disagreement flags a
digitizing or transcription error.

## Usage

``` r
compare_segments_landmarks(
  landmarks,
  published,
  id_col = NULL,
  variant = c("standard", "calibrated"),
  space = FALSE,
  group_col = NULL,
  log = FALSE,
  na_action = c("omit", "fail", "impute_mean", "impute_group_mean", "missforest",
    "missforest_phylo"),
  missforest_ntree = 100,
  missforest_maxiter = 10,
  tree = NULL,
  missforest_phylo_k = 10,
  phylo_axes = NULL
)
```

## Arguments

- landmarks:

  A `fishmorph_landmarks` object, OR a data frame of landmark-derived
  segments already carrying an id column.

- published:

  A data frame of published segments (or ratios) with an id column
  matching the landmarks.

- id_col:

  Name of the identifier column in `published` (default tries
  `specimen`, `Genus.species`, `species`, `id`).

- variant:

  For the published side, use the `"standard"` segments or the
  calibrated vertical-position variants (`"calibrated"`:
  `Bd2, Eh2, Mo2, PFi2` when present).

- space:

  If `TRUE`, also build a shared FISHMORPH functional space: a frozen
  PCA fitted on the segment- (published) ratios of the matched species,
  onto which BOTH the segment- and the landmark-derived ratios are
  projected. The two clouds then live in one coordinate system and are
  directly comparable (as in `compare_functional_spaces_fishmorph.R`).
  Default `FALSE`.

- group_col:

  Optional name of a column in `published` (e.g. `"Order"` or
  `"Family"`) used to colour the species in the functional-space plot.

- log:

  Passed to
  [`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
  when `space = TRUE` (default `FALSE`).

- na_action:

  For `space = TRUE`: how to handle species with missing ratios when
  building/projecting the shared space. `"omit"` (default) drops them;
  `"impute_mean"`, `"impute_group_mean"`, `"missforest"` or
  `"missforest_phylo"` impute both the segment- and landmark-ratio
  matrices so no species is lost (same options as
  [`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)).

- missforest_ntree, missforest_maxiter, tree, missforest_phylo_k,
  phylo_axes:

  Passed to the missForest imputation when `na_action` is a missForest
  method. Note that `"missforest_phylo"` takes its species key from
  `id_col`, not from `group_col`: the former identifies the species of
  each row, the latter is a coarser grouping (`"Order"`, `"Family"`)
  used as an auxiliary predictor and for colouring.

## Value

An object of class `fishmorph_comparison`: a list with `metrics` (one
row per quantity x trait), `ratios` and `segments` (merged wide tables
used for plotting), `id_col`, and – when `space = TRUE` – `space` (the
frozen
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)),
`space_scores` (projected PC scores with a `source` column, `"segment"`
vs `"landmark"`), and `space_shift`: a data frame, one row per species,
sorted by **decreasing** displacement between the two methods, with
`distance` (Euclidean shift over all shared PCs), `dist_2d` (shift in
the two plotted axes), `dPC1`/`dPC2` and `group`. The top rows are the
species whose morphospace position changes most between the segment- and
landmark-based measurements. When vegan is available, `procrustes` holds
a Procrustes test
([`vegan::protest()`](https://vegandevs.github.io/vegan/reference/procrustes.html))
of the concordance between the two configurations (`correlation`,
`significance`, `ss`, `n`).

## See also

[`plot_segment_landmark_agreement()`](https://funtraits.github.io/Rfishmorph/reference/plot_segment_landmark_agreement.md),
[`summary.fishmorph_comparison()`](https://funtraits.github.io/Rfishmorph/reference/summary.fishmorph_comparison.md)
