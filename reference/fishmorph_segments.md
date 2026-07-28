# Compute the 11 FISHMORPH segments from landmarks

Measures the eleven body segments
(`Bl, Bd, Hd, Eh, Mo, PFi, PFl, Ed, Jl, CPd, CFd`) from digitized
landmark configurations. Body length `Bl` follows the 1-22-2 broken line
when the optional curvature landmark 22 is present and non-zero,
otherwise the straight 1-2 distance (matching intraitR).

## Usage

``` r
fishmorph_segments(x, scale_cm = NULL, na.rm = TRUE)
```

## Arguments

- x:

  A `fishmorph_landmarks` (or `intrait_landmarks`) object.

- scale_cm:

  Real length (cm) of the scale-bar segment (landmarks 20-21). Ignored
  when landmarks 20-21 are absent.

- na.rm:

  Passed through to keep only rows with usable coordinates.

## Value

A data frame, one row per specimen, with column `specimen` and the 11
segment columns. `attr(, "unit")` records `"cm"` or `"raw"`.

## Details

Absolute lengths require a scale. The scale is taken, in order, from
`scale_cm` combined with the scale-bar landmarks 20-21, then from the
object's `scale` slot, and otherwise the segments are returned in the
input (pixel) units with a warning-free `attr(, "unit") = "raw"`.

## See also

[`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md),
[`fishmorph_traits()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_traits.md)

## Examples

``` r
lm <- reconstruct_fishmorph_landmarks(
  list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
       PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0))
fishmorph_segments(lm, scale_cm = 1)
#>        specimen Bl  Bd  Hd  Eh  Mo PFi PFl  Ed  Jl CPd CFd
#> 1 reconstructed 10 3.2 2.4 1.1 1.6 1.9 2.1 0.6 1.3   1   3
```
