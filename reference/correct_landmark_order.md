# Correct impossible vertical landmark orderings

Enforces the dorsal-to-ventral Y ordering of the FISHMORPH landmarks so
that geometrically impossible configurations (which produce out-of-range
ratios such as `VEp > 1`) are removed. Along each ordered chain, any
landmark whose Y crosses its dorsal neighbour is snapped onto that
neighbour. The dorsal-ventral direction is detected per specimen
(landmarks 3 vs 4), so the function is correct in both image (Y-down)
and Cartesian (Y-up) coordinates.

## Usage

``` r
correct_landmark_order(landmarks, chains = .FM_ORDER_CHAINS, report = TRUE)
```

## Arguments

- landmarks:

  A `fishmorph_landmarks` object (e.g. straight after
  `fishmorph_landmarks(lm_df)`).

- chains:

  Named list of ordered landmark chains to enforce. Defaults to the
  FISHMORPH set above; override to add/remove chains.

- report:

  Print a one-line summary (default `TRUE`).

## Value

The corrected `fishmorph_landmarks` object. `attr(, "order_report")` is
a data frame (`specimen`, `n_corrected`, `flagged`).

## Details

Chains enforced (dorsal -\> ventral): the body/head/eye column
`3, 5, 13, 7, 14, 6, 8, 4` (bounding the eye between the body-depth
landmarks, so `VEp <= 1`), the pectoral `10, 11`, the caudal peduncle
`16, 17`, the caudal fin `18, 19`, and the snout/mouth `1, 9`.

## See also

[`check_landmark_order()`](https://funtraits.github.io/Rfishmorph/reference/check_landmark_order.md),
[`correct_geometry_conventions()`](https://funtraits.github.io/Rfishmorph/reference/correct_geometry_conventions.md)

## Examples

``` r
lm <- read_landmarks_csv(
  system.file("extdata", "example_landmarks.csv", package = "Rfishmorph"))
lm2 <- correct_landmark_order(lm)
#> correct_landmark_order(): adjusted 4 landmark(s) in 2 of 2 specimen(s).
```
