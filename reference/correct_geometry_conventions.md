# Enforce the FISHMORPH digitizing conventions (optionally straightening)

First, when `straighten = TRUE` (default), each bent specimen is
"unbent" along its broken body axis: the polyline 1 -\> 22 -\> 24 -\> 2
(using whichever of the hinge landmarks 22/24/25 are present, ordered
along the 1-2 chord) is laid out as a single straight line by rotating
each body segment rigidly about its proximal hinge (arc length
preserved). Specimens without any hinge landmark – i.e. straight fish,
or data digitized without landmark 22 – are left unchanged, so the
behaviour is backward compatible.

## Usage

``` r
correct_geometry_conventions(x, tolerance_coord = 1e-06, straighten = TRUE)
```

## Arguments

- x:

  A `fishmorph_landmarks` object.

- tolerance_coord:

  Coordinate tolerance (kept for interface parity).

- straighten:

  If `TRUE` (default), unbend the broken axis 1-22-24-2 before applying
  the conventions. Set `FALSE` to keep the historical straight-axis
  behaviour (single axis 1-2, no unbending).

## Value

A corrected `fishmorph_landmarks` object.

## Details

Then the five geometric conventions that a correctly digitized FISHMORPH
configuration must satisfy are applied, in the frame of the (now
straight) body axis 1-2:

1.  landmark 9 shares the axial position of the snout (1);

2.  landmark 4 shares the axial position of 3 (`Bd` vertical);

3.  landmark 11 shares the axial position of 10 (`PFi` vertical);

4.  the eye group `{5,13,7,14,6,8}` shares one axial position (eye
    vertical);

5.  the belly group `{9,8,11,4}` shares one normal position (belly
    line).

Perpendicularity (3-4, 10-11 vertical) and the belly line are thus
enforced relative to the straightened axis. The transform snaps to group
medians and leaves landmark 1 fixed.
