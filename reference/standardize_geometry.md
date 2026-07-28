# Standardize landmark geometry (rescale, translate, optionally orient)

Places every specimen in a common frame: landmark 1 (snout) at the
origin and the body axis 1-2 along the positive X direction. This is a
similarity transform (rotation + translation + optional isotropic
rescale); it does not alter any segment ratio.

## Usage

``` r
standardize_geometry(x, orient = TRUE, rescale = FALSE, dorsal_up = FALSE)
```

## Arguments

- x:

  A `fishmorph_landmarks` object.

- orient:

  If `TRUE` (default), rotate so the body axis 1-2 is horizontal with
  the snout on the left.

- rescale:

  If `TRUE`, rescale each specimen so `Bl = |1-2| = 1`. Useful to
  overlay specimens of different sizes; leaves ratios unchanged.

- dorsal_up:

  If `TRUE`, canonically orient each specimen dorsal-side up by
  reflecting it across the body axis whenever the dorsal landmark (3)
  lies below the ventral landmark (4). This removes the up/down mirror
  difference between configurations digitized in image coordinates (Y
  increasing downward) and those in Cartesian coordinates (e.g.
  [`reconstruct_fishmorph_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/reconstruct_fishmorph_landmarks.md)
  output), so measured and reconstructed fish overlay in the same
  orientation. A reflection preserves all segments and ratios. Default
  `FALSE`.

## Value

A `fishmorph_landmarks` object in the standardized frame.
