# Check the reconstruction round-trip

Verifies that reconstructing landmarks from segments and re-measuring
them returns the input segments to machine precision, whatever the free
parameters. A large error signals a geometry bug, not a placement
choice.

## Usage

``` r
check_reconstruction_roundtrip(segments = NULL, tol = 1e-06, ...)
```

## Arguments

- segments:

  Named list / data frame of the 11 segments (cm). If `NULL`, a built-in
  test specimen is used.

- tol:

  Tolerance for the OK/FAIL verdict.

- ...:

  Passed to
  [`reconstruct_fishmorph_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/reconstruct_fishmorph_landmarks.md).

## Value

Invisibly, a data frame with `input`, `recovered` and `abs_error`.

## Examples

``` r
check_reconstruction_roundtrip()
#> Round-trip: max error = 2.22e-16 cm (OK)
```
