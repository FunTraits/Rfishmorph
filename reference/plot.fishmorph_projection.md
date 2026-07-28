# Plot specimens projected into the global FISHMORPH space

Draws the reference database as a kernel-density heatmap with the
projected specimens on top, coloured by species, with per-species convex
hulls (or spider/density/points). Faithful port of
`intraitR:::plot.intrait_fishmorph_projection()`.

## Usage

``` r
# S3 method for class 'fishmorph_projection'
plot(
  x,
  style = c("hull", "spider", "density", "points"),
  reference_density = TRUE,
  reference_points = FALSE,
  itv_reference = FALSE,
  arrows = FALSE,
  arrow_scale = 0.8,
  arrow_col = "grey20",
  background = TRUE,
  background_col = "grey75",
  density_probs = c(0.25, 0.5, 0.99),
  density_palette = NULL,
  select_species = NULL,
  select_specimens = NULL,
  ellipse_level = 0.95,
  density_level = 0.95,
  legend = TRUE,
  legend_position = "outside",
  legend_title = "Species",
  legend_italic = TRUE,
  abbreviate_species = TRUE,
  ...
)
```

## Arguments

- x:

  A `fishmorph_projection` object.

- style:

  `"hull"` (default), `"spider"`, `"density"` or `"points"`.

- reference_density:

  Draw the reference density heatmap (default `TRUE`).

- reference_points:

  Draw the reference as a light point cloud (default `FALSE`).

- itv_reference:

  Mark the focal species' own reference-database points (default
  `FALSE`).

- arrows:

  Overlay trait loading arrows (biplot; default `FALSE`).

- arrow_scale, arrow_col:

  Loading-arrow length fraction and colour.

- background:

  Master switch for all reference layers (default `TRUE`).

- background_col:

  Reference point-cloud colour.

- density_probs, density_palette:

  Heatmap contour probabilities / palette.

- select_species, select_specimens:

  Restrict the plot to a subset.

- ellipse_level, density_level:

  Coverage for `"spider"`/`"density"`.

- legend, legend_position, legend_title, legend_italic,
  abbreviate_species:

  Legend controls (defaults tuned for species names).

- ...:

  Passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly `x`.
