# Plot species in the FISHMORPH functional space (landmark- or segment-based)

Ports the `intraitR` functional-space plot (`trait_space()` /
[`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md)
plotting) into Rfishmorph. Species are placed in the FISHMORPH
morphospace and coloured, with per-group convex hulls and trait loading
arrows. The `source` argument selects whether the morphological traits
come from digitized **landmarks** or from measured **segments**, so the
same species can be compared across measurement methods on one
ordination (as in `compare_functional_spaces_fishmorph.R`).

## Usage

``` r
plot_functional_space(
  x,
  source = c("segment", "landmark"),
  groups = NULL,
  reference = NULL,
  axes = c(1, 2),
  hull = TRUE,
  density = FALSE,
  global = FALSE,
  points = TRUE,
  loadings = TRUE,
  backbone = TRUE,
  legend = TRUE,
  scale_cm = NULL,
  engine = c("ggplot2", "base"),
  ...
)
```

## Arguments

- x:

  The data to place: a `fishmorph_landmarks` object when
  `source = "landmark"`, or a data frame of segments/ratios when
  `source = "segment"`.

- source:

  `"segment"` (default) or `"landmark"`: how the 9 ratios are obtained
  from `x`.

- groups:

  Optional grouping vector (e.g. species) used for colour and hulls.
  Defaults to specimen/`Species` identifiers found in `x`.

- reference:

  Optional frozen `fishmorph_trait_space` backbone to project onto. When
  `NULL`, the ordination is fitted on `x`.

- axes:

  Length-2 principal components to display (default `c(1, 2)`).

- hull:

  Draw convex hulls (per group, or one global hull when
  `global = TRUE`). Default `TRUE`.

- density:

  Overlay a 2D kernel density (per-group contour lines, or a filled
  global surface when `global = TRUE`). Default `FALSE`. ggplot2 only.

- global:

  Pool all specimens and ignore `groups`: a single colour, one overall
  hull/density (the whole functional space at once). Default `FALSE`.

- points:

  Draw the specimen points (default `TRUE`); set `FALSE` to show only
  hulls/density.

- loadings:

  Overlay trait loading arrows (default `TRUE`).

- backbone:

  When a `reference` is given, draw its cloud as light-grey context
  points (default `TRUE`).

- legend:

  Show the group colour legend (default `TRUE`). Set `FALSE` to drop it
  (useful when there are many groups, e.g. species).

- scale_cm:

  Optional scale for landmark-derived measurements (irrelevant to
  ratios; kept for interface parity).

- engine:

  `"ggplot2"` (default) or `"base"`.

- ...:

  Ignored.

## Value

A ggplot object (invisibly for base).

## Details

The ordination is either supplied via `reference` (a frozen
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
backbone, e.g. the full FISHMORPH database, onto which the data are
projected) or, when `reference = NULL`, fitted on the data themselves.
Projecting onto a shared frozen backbone is the correct way to compare
landmark- and segment-based positions, because both then live in one
coordinate system.

## See also

[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md),
[`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md),
[`plot_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/plot_trait_space.md)

## Examples

``` r
ref <- load_fishmorph_reference()
#> FISHMORPH reference: segment (published) -- 8970 species.
backbone <- fishmorph_trait_space(ref)
# segment-based species positions projected on the frozen backbone
plot_functional_space(ref, source = "segment", groups = ref$Order,
                      reference = backbone)

# landmark-based positions for digitized specimens
lm <- read_landmarks_csv(
  system.file("extdata", "example_landmarks.csv", package = "Rfishmorph"))
plot_functional_space(lm, source = "landmark", reference = backbone)
#> Ignoring unknown labels:
#> • fill : "group"
```
