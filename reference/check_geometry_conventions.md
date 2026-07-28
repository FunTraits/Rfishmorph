# Check the FISHMORPH digitizing conventions

Measures, per specimen, how far a configuration is from satisfying the
five geometric conventions (see
[`correct_geometry_conventions()`](https://funtraits.github.io/Rfishmorph/reference/correct_geometry_conventions.md))
by comparing it with its corrected version. Large residuals point to a
mis-placed landmark.

## Usage

``` r
check_geometry_conventions(x, tolerance = 0)
```

## Arguments

- x:

  A `fishmorph_landmarks` object.

- tolerance:

  Residual (in the specimen's own units) above which a specimen is
  flagged. Default 0 returns the residual for every specimen.

## Value

A data frame with `specimen`, `max_shift`, `mean_shift` and, for each
convention group, the residual; plus a logical `flag`.
