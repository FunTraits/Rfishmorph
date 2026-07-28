# Full FISHMORPH trait table (segments + ratios)

Convenience wrapper returning the 11 segments and the 9 ratios in one
table.

## Usage

``` r
fishmorph_traits(x, scale_cm = NULL)
```

## Arguments

- x:

  A `fishmorph_landmarks` object.

- scale_cm:

  Scale-bar length in cm (see
  [`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md)).

## Value

A data frame with `specimen`, 11 segment and 9 ratio columns.
