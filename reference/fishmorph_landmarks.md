# Create a fishmorph_landmarks object

Create a fishmorph_landmarks object

## Usage

``` r
fishmorph_landmarks(
  coords,
  metadata = NULL,
  scale = NULL,
  specimen = NULL,
  pad_to = .FM_N_POINTS
)
```

## Arguments

- coords:

  Numeric array `[n_landmarks, 2, n_specimens]` (X, Y on the 2nd
  margin), a `[n_landmarks, 2]` matrix (single specimen), or a data
  frame of landmark coordinates (see
  [`read_landmarks_csv()`](https://funtraits.github.io/Rfishmorph/reference/read_landmarks_csv.md)
  for the accepted layout).

- metadata:

  Optional data frame, one per specimen.

- scale:

  Optional numeric vector of scale factors (units per pixel).

- specimen:

  Optional specimen name(s) used when `coords` has none.

- pad_to:

  Minimum number of landmarks the object must carry. When the input has
  fewer, the missing landmarks are added as `NA` rows so the
  configuration still follows the FISHMORPH scheme and is accepted by
  downstream functions
  ([`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md),
  `plot_fishmorph_points()`, ...). Default `25L` since Rfishmorph 0.5.0:
  the canonical frame is the one the digitizer records – 19 anatomical
  landmarks, the scale bar 20-21, the curvature hinge 22, the derived
  point 23 and the extra axis hinges 24-25. Padding to 21 used to leave
  objects one shape and the stored data another, so a configuration that
  had hinges lost them on the way in. `NA` means "not placed" and every
  routine skips it, so the wider frame costs nothing. Set `NULL` to
  disable padding.

## Value

An object of class `fishmorph_landmarks`.

## Examples

``` r
P <- matrix(rnorm(44), 22, 2, dimnames = list(NULL, c("X", "Y")))
fishmorph_landmarks(P, specimen = "demo")
#> <fishmorph_landmarks>
#>   specimens : 1
#>   landmarks : 25 (2D)
#>   names     : demo
# a 20-landmark config is padded to 25 (scale bar and hinges as NA):
dim(fishmorph_landmarks(matrix(rnorm(40), 20, 2))$coords)
#> [1] 25  2  1
```
