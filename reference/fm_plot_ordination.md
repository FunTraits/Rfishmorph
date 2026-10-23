# Shared ordination-plot primitives (for package developers)

The engine behind every ordination figure of Rfishmorph and intraitR
(species hulls, spiders, dispersion ellipses, density contours,
reference heatmap, legend). They are exported so that intraitR's
trait-space, shape-space and bootstrap plots draw through this one
implementation and share its session colour cache; end users should call
the high-level functions
([`plot.fishmorph_projection()`](https://funtraits.github.io/Rfishmorph/reference/plot.fishmorph_projection.md),
[`plot_functional_space()`](https://funtraits.github.io/Rfishmorph/reference/plot_functional_space.md),
[`plotly_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/plotly_fishmorph.md))
instead.

## Usage

``` r
fm_plot_ordination(...)

fm_group_palette(labels)

fm_abbreviate_species(x)
```

## Arguments

- ...:

  Arguments passed to the internal ordination plotter.

- labels:

  Character vector of group labels.

- x:

  Character vector of species names.

## Value

`fm_plot_ordination()`: invisibly `NULL` (called for its side effect).
`fm_group_palette()`: a named character vector of colours, one per
unique label. `fm_abbreviate_species()`: a character vector the length
of `x`.

## Details

- `fm_plot_ordination()` draws scores coloured by group on the current
  device; its arguments are those of the internal plotter (`scores`,
  `groups`, `xlab`, `ylab`, `style`, `ellipse_level`, `density_level`,
  `legend`, `legend_position`, `legend_title`, `legend_italic`,
  `abbreviate_species`, `space_name`, `background`, `background_col`,
  `background_density`, `background_points`, `density_probs`,
  `density_palette`, `highlight`, `highlight_col`, and graphical `...`).

- `fm_group_palette()` returns the session-stable colour of each label,
  assigning new colours to labels never seen before.

- `fm_abbreviate_species()` turns clean binomials into `"G. species"`.

## See also

[`group_colors()`](https://funtraits.github.io/Rfishmorph/reference/group_colors.md),
[`reset_group_colors()`](https://funtraits.github.io/Rfishmorph/reference/reset_group_colors.md)

## Examples

``` r
fm_group_palette(c("Salmo trutta", "Squalius cephalus"))
#>      Salmo trutta Squalius cephalus 
#>         "#4E79A7"         "#F28E2B" 
fm_abbreviate_species("Barbatula barbatula")
#> [1] "B. barbatula"
```
