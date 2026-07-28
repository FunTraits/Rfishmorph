# Structural AND morphometric control

The SQL constraints guarantee the coherence of the CONTAINER; this
function questions the plausibility of the CONTENT. A set of coordinates
can satisfy every constraint and describe an impossible fish.

## Usage

``` r
fishmorph_validate(x, expect = c(1:19, 22L, 23L), bounds = .FM_RATIO_BOUNDS)
```

## Arguments

- x:

  A journal directory, or a long data.frame (the output of
  `fishmorph_consolidate(long = TRUE)`).

- expect:

  Points expected for a complete specimen.

- bounds:

  Envelope of the ratios (default: .FM_RATIO_BOUNDS).

## Value

data.frame: specimen_id, species, photo_file, severity, problem,
landmark, detail.

## Details

Severities: "error" = certain inconsistency (point outside the image,
coincident points, degenerate axis); "warning" = to be looked at
(proportion outside the envelope of the 9,556 species); "info" =
traceability (point never checked, declared non-measurable, no scale
bar).
