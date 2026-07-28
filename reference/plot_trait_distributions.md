# Distributions of the FISHMORPH traits

Faceted density/violin summaries of the 9 ratios (or any chosen traits),
useful to inspect a new species against the reference distribution.

## Usage

``` r
plot_trait_distributions(
  data,
  traits = fishmorph_ratio_names(),
  highlight = NULL,
  log = FALSE,
  engine = c("ggplot2", "base"),
  ...
)
```

## Arguments

- data:

  A data frame of traits or a `fishmorph_landmarks` object.

- traits:

  Trait columns (default the 9 ratios).

- highlight:

  Optional named numeric vector (trait = value) or one-row data frame
  drawn as a vertical rule on each panel (e.g. a new species).

- log:

  Log10 x-axis (default `FALSE`).

- engine:

  `"ggplot2"` (default) or `"base"`.

- ...:

  Ignored.

## Value

A ggplot object (invisibly for base).
