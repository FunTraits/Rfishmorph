# Plot landmark-vs-published agreement

Plot landmark-vs-published agreement

## Usage

``` r
plot_segment_landmark_agreement(
  x,
  type = c("scatter", "bar", "space"),
  style = c("hull", "spider", "density", "none"),
  reference_density = TRUE,
  arrows = FALSE,
  arrow_scale = 0.8,
  arrow_col = "grey20",
  engine = c("ggplot2", "base"),
  ...
)
```

## Arguments

- x:

  A `fishmorph_comparison` object from
  [`compare_segments_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/compare_segments_landmarks.md).

- type:

  `"scatter"` (published vs landmark, one panel per ratio), `"bar"`
  (Pearson r per ratio), or `"space"` (the two FISHMORPH functional
  spaces – segment-based and landmark-based – side by side on the shared
  frozen ordination; requires
  `compare_segments_landmarks(..., space = TRUE)`).

- style:

  For `type = "space"`: per-group geometry, `"hull"` (default),
  `"spider"`, `"density"` or `"none"` (see
  [`plot.fishmorph_projection()`](https://funtraits.github.io/Rfishmorph/reference/plot.fishmorph_projection.md)).

- reference_density:

  For `type = "space"`: draw the shared-space density heatmap behind
  each panel (default `TRUE`).

- arrows:

  For `type = "space"`: overlay the trait loadings as biplot arrows on
  each panel (default `FALSE`).

- arrow_scale, arrow_col:

  Loading-arrow length fraction (in `(0, 1]`) and colour, when
  `arrows = TRUE`.

- engine:

  `"ggplot2"` (default) or `"base"`. `type = "space"` always uses base
  graphics (the intraitR ordination style).

- ...:

  Passed through (e.g. to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  `type = "space"`).

## Value

A ggplot object (invisibly for base / `"space"`).
