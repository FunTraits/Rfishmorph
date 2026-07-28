# Apply the "zero-ratio" conventions to landmarks

Some FISHMORPH position ratios are set to exactly 0 by convention for
certain species – e.g. a terminal mouth has `OGp = 0`, a ventral
pectoral insertion has `PFv = 0`. A ratio `R = num / den` is 0 when its
numerator segment has zero length, i.e. when the two landmarks defining
that segment coincide. This function enforces that on the landmark
configuration: for every specimen whose reference value of a chosen
ratio is 0, it collapses the relevant landmark onto its partner so the
landmark-derived ratio also becomes exactly 0 (matching the convention
seen as the vertical stripe at `published = 0` in the
segment-vs-landmark scatter).

## Usage

``` r
correct_zero_ratio_landmarks(
  landmarks,
  reference,
  id_col = NULL,
  ratios = c("OGp", "PFv"),
  tol = 0
)
```

## Arguments

- landmarks:

  A `fishmorph_landmarks` object.

- reference:

  A data frame carrying the reference ratio values (the convention),
  with an identifier column matching the specimen names and a column for
  each ratio in `ratios` (e.g. the published FISHMORPH ratio table). If
  the ratio columns are absent but segment columns are present, the
  ratios are computed with
  [`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md).

- id_col:

  Name of the identifier column in `reference` (default tries
  `Genus.species`, `specimen`, `species`, `id`).

- ratios:

  Which zero-conventions to enforce (default `c("OGp", "PFv")`).

- tol:

  Reference values `<= tol` are treated as zero (default 0).

## Value

The corrected `fishmorph_landmarks` object. `attr(, "n_collapsed")`
reports how many specimens were adjusted per ratio.

## Details

The collapses applied (numerator segment -\> landmarks moved): `OGp`
(`Mo = |1-9|`) moves 9 onto 1; `PFv` (`PFi = |10-11|`) moves 11 onto 10;
`VEp` (`Eh = |7-8|`) moves 8 onto 7. Any ratio whose numerator is a
segment can be added via `ratios`.

## See also

[`correct_geometry_conventions()`](https://funtraits.github.io/Rfishmorph/reference/correct_geometry_conventions.md),
[`compare_segments_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/compare_segments_landmarks.md)

## Examples

``` r
ref <- load_fishmorph_reference()          # has OGp, PFv columns
#> FISHMORPH reference: segment (published) -- 8970 species.
lm  <- reconstruct_fishmorph_landmarks(
  list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
       PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0),
  specimen = ref$Species[1])
correct_zero_ratio_landmarks(lm, ref, id_col = "Species")
#> correct_zero_ratio_landmarks(): collapsed OGp in 0 specimen(s), PFv in 0 specimen(s).
#> <fishmorph_landmarks>
#>   specimens : 1
#>   landmarks : 25 (2D)
#>   names     : Aaptosyax grypus
```
