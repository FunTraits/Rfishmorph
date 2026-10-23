# Plot specimens projected into the global FISHMORPH space

Draws the reference database as a background layer – by default a
kernel-density heatmap of the whole FISHMORPH morphospace – with the
projected specimens on top of it, grouped/coloured by species, in any of
the display styles shared with
[`plot_functional_space()`](https://funtraits.github.io/Rfishmorph/reference/plot_functional_space.md)
and `intraitR::plot.intrait_traitspace()`. The background can
equivalently (or additionally) be shown as the raw reference point
cloud, and the focal species' own positions *in the reference database*
can be marked, to see how a group's projected intraspecific trait
variation (ITV) sits relative to its single database point.

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

  A `"fishmorph_projection"` object from
  [`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md)
  (including intraitR's `"intrait_fishmorph_projection"` subclass).

- style:

  Character, one of `"hull"` (default; per-species convex hull = the ITV
  footprint), `"spider"` (dispersion ellipse + spokes to the centroid),
  `"density"` (per-species kernel-density contour of the projected
  specimens), or `"points"` (the projected points only, no per-group
  geometry). Note this is the *foreground* per-species geometry and is
  independent of `reference_density`, which controls the *background*
  reference heatmap.

- reference_density:

  Logical, draw the reference database's distribution as a filled
  kernel-density heatmap (a white-to-red gradient with a few nested
  highest-density-region contour lines) behind the projected specimens –
  the recommended way to show a ~9,000-species morphospace, which is
  unreadable as raw points. Defaults to `TRUE`.

- reference_points:

  Logical, draw the reference database species as a light background
  point cloud (the previous default display). Can be combined with
  `reference_density`. Defaults to `FALSE`.

- itv_reference:

  Logical, mark the focal species' *own* entries in the reference
  database – i.e. the FISHMORPH points of exactly the species for which
  projected ITV is shown – as filled circles coloured to match their
  species, so the single database morphotype can be compared with the
  spread of the projected individuals. Requires the projection to carry
  reference species labels (`x$global_species`, added by
  [`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md));
  species with no matching reference row are skipped with a warning.
  Defaults to `FALSE`.

- arrows:

  Logical, overlay the trait *loadings* (the PCA variable contributions
  stored in `x$loadings`) as a biplot: one arrow per trait, drawn from
  the origin in the direction of that trait's loading on the two plotted
  axes, with the trait name at the arrow tip. This turns the score plot
  into a standard PCA biplot, showing which morphological ratios drive
  each axis and hence how a group's position in the morphospace should
  be read ecomorphologically (e.g. an ITV footprint stretched along the
  body-elongation arrow varies mostly in body shape). The arrows are a
  purely visual overlay and do not change the ordination. Defaults to
  `FALSE`.

- arrow_scale:

  Numeric in `(0, 1]`, how far the longest loading arrow reaches, as a
  fraction of the distance from the origin to the nearest plot edge; all
  arrows are scaled by this same factor so their relative lengths and
  directions are preserved. Loadings are unit-scaled vectors with no
  natural size in score units, so this is a display choice only. Smaller
  values keep the arrows clear of the outer points; larger values make
  short loadings more legible. Only used when `arrows = TRUE`. Defaults
  to `0.8`.

- arrow_col:

  Colour of the loading arrows and their trait labels (when
  `arrows = TRUE`). Defaults to `"grey20"`.

- background:

  Logical master switch: when `FALSE`, no reference background layer of
  any kind is drawn (overrides `reference_density`, `reference_points`
  and `itv_reference`). Defaults to `TRUE`.

- background_col:

  Colour of the reference point cloud (when `reference_points = TRUE`).
  Defaults to `"grey75"`.

- density_probs:

  Numeric vector of coverage probabilities for the reference heatmap's
  highest-density-region contour lines. Defaults to
  `c(0.25, 0.5, 0.99)`.

- density_palette:

  Optional character vector of colours for the reference heatmap
  gradient (low to high density). `NULL` (default) uses a
  white-to-dark-red ColorBrewer YlOrRd ramp.

- select_species, select_specimens:

  Optional filters restricting the plot to some groups (matched against
  `x$groups`) or specimen identifiers (matched against
  `rownames(x$scores)`); combined by intersection.

- ellipse_level, density_level:

  Coverage probabilities for the `"spider"` ellipse and `"density"`
  contour respectively. Default `0.95`.

- legend:

  Logical, draw a species legend. Defaults to `TRUE`.

- legend_position, legend_title, legend_italic, abbreviate_species:

  Legend controls, as in
  [`plot_functional_space()`](https://funtraits.github.io/Rfishmorph/reference/plot_functional_space.md);
  defaults here are tuned for species names (`legend_title = "Species"`,
  italic, abbreviated binomials).

- ...:

  Further arguments passed to
  [`graphics::plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Value

Invisibly `x`.

## Details

In the [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method,
`select_species`/`select_specimens` restrict *this plot* to a subset of
the already-projected species or specimen identifiers, without
recomputing the space (the reference background is unchanged); `NULL`
(default) plots everything projected. When `itv_reference = TRUE`, the
reference points marked follow the same selection: only the focal
species still shown after `select_species`/`select_specimens` are
matched against the reference database (matching is case-insensitive and
treats spaces and underscores as equivalent, so `"Squalius cephalus"`
and `"Squalius_cephalus"` match).
