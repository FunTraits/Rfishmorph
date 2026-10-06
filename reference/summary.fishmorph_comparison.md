# Summary of a segment-versus-landmark comparison

Prints what a table of metrics does not say at a glance: the ratio
agreement (n, Pearson r, bias, RMSE) trait by trait, the WEAKEST ratio –
the one that decides whether the two measurement routes can be pooled at
all – and, when the comparison was run with `space = TRUE`, the species
whose position in the functional space moves most between the two
routes, plus the Procrustes test of the two spaces.

## Usage

``` r
# S3 method for class 'fishmorph_comparison'
summary(object, ...)
```

## Arguments

- object:

  A `fishmorph_comparison` object returned by
  [`compare_segments_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/compare_segments_landmarks.md).

- ...:

  Ignored, present for compatibility with the generic.

## Value

The metrics `data.frame` of `object`, invisibly (NULL if no specimen was
comparable).

## See also

[`compare_segments_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/compare_segments_landmarks.md),
[`plot_segment_landmark_agreement()`](https://funtraits.github.io/Rfishmorph/reference/plot_segment_landmark_agreement.md)
