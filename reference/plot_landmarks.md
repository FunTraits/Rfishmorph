# Plot a digitized / reconstructed fish

Draws the landmarks and the 11 segments of one specimen. With ggplot2
available it returns a ggplot object; otherwise it draws with base
graphics.

## Usage

``` r
plot_landmarks(
  x,
  specimen = 1,
  label_points = TRUE,
  segments = TRUE,
  engine = c("ggplot2", "base"),
  ...
)
```

## Arguments

- x:

  A `fishmorph_landmarks` object.

- specimen:

  Which specimen (name or index) to draw. Default first.

- label_points:

  Annotate landmark numbers (default `TRUE`).

- segments:

  Draw the 11 segments (default `TRUE`).

- engine:

  `"ggplot2"` (default when available) or `"base"`.

- ...:

  Ignored.

## Value

A ggplot object (invisibly for base).
