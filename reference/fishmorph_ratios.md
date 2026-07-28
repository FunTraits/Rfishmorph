# Compute the 9 FISHMORPH ratios

Derives the nine dimensionless morphological ratios from a table of
segments. Because each ratio is a quotient of two segments of the same
specimen, the unknown pixel-to-cm factor cancels: ratios are
scale-invariant and can be computed from raw (pixel) segments.

## Usage

``` r
fishmorph_ratios(x, scale_cm = NULL)
```

## Arguments

- x:

  Either a `fishmorph_landmarks` object or a data frame of segments (as
  returned by
  [`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md)).

- scale_cm:

  Passed to
  [`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md)
  when `x` is a landmark object (irrelevant to the ratio values
  themselves).

## Value

A data frame with column `specimen` and the 9 ratio columns. Non-finite
or zero denominators yield `NA`.

## Examples

``` r
seg <- data.frame(specimen = "a", Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1,
                  Mo = 1.6, PFi = 1.9, PFl = 2.1, Ed = 0.6, Jl = 1.3,
                  CPd = 1.0, CFd = 3.0)
fishmorph_ratios(seg)
#>   specimen   BEl     VEp  REs OGp       RMl  BLs     PFv  PFs CPt
#> 1        a 3.125 0.34375 0.25 0.5 0.5416667 0.75 0.59375 0.21   3
```
