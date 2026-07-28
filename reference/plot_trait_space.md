# Plot the FISHMORPH functional trait space (intraitR style)

Draws a `fishmorph_trait_space` with the same base-graphics engine as
`intraitR::plot.intrait_traitspace()`/[`plot.fishmorph_projection()`](https://funtraits.github.io/Rfishmorph/reference/plot.fishmorph_projection.md):
per-group geometry (convex hull, spider/ellipse, or density contour),
stable per-species colours and an outside legend. Optionally overlays
the whole-space kernel-density heatmap (the "global base" density) and
the trait loading arrows (biplot).

## Usage

``` r
plot_trait_space(
  x,
  style = c("hull", "spider", "density", "none"),
  axes = c(1, 2),
  reference_density = FALSE,
  reference_points = FALSE,
  density_probs = c(0.25, 0.5, 0.99),
  arrows = FALSE,
  arrow_scale = 0.8,
  arrow_col = "grey20",
  ellipse_level = 0.95,
  density_level = 0.95,
  legend = !is.null(x$groups),
  legend_position = "outside",
  legend_title = "Group",
  legend_italic = FALSE,
  abbreviate_species = FALSE,
  ...
)
```

## Arguments

- x:

  A `fishmorph_trait_space` object.

- style:

  `"hull"` (default), `"spider"`, `"density"` or `"none"` (points only)
  – the per-group geometry, as in intraitR.

- axes:

  Length-2 principal components to display (default `c(1, 2)`).

- reference_density:

  Draw the whole space's kernel-density heatmap (white-to-red gradient +
  HDR contours) behind the points – the density of the global base.
  Default `FALSE`.

- reference_points:

  Draw all points again as a light background cloud. Default `FALSE`.

- density_probs:

  Coverage probabilities for the heatmap contour lines.

- arrows:

  Overlay the trait loadings as biplot arrows (default `FALSE`).

- arrow_scale, arrow_col:

  Arrow length fraction and colour.

- ellipse_level, density_level:

  Coverage for `"spider"`/`"density"`.

- legend, legend_position, legend_title, legend_italic,
  abbreviate_species:

  Legend controls (single legend, outside by default).

- ...:

  Passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly `x`.

## See also

[`plot.fishmorph_projection()`](https://funtraits.github.io/Rfishmorph/reference/plot.fishmorph_projection.md)

## Examples

``` r
ref <- load_fishmorph_reference()
#> FISHMORPH reference: segment (published) -- 8970 species.
ts  <- fishmorph_trait_space(ref, groups = ref$Order)
plot(ts, style = "hull")                              # per-Order hulls

plot(ts, style = "hull", reference_density = TRUE)   # + global density heatmap

plot(ts, style = "hull", arrows = TRUE)              # + trait loading arrows
```
