# Default free parameters for landmark reconstruction

The parameters NOT identified by the segments alone: `f_*` = fraction
along the body axis (0 = snout, 1 = caudal base); `o_*` = dorsal share
of a depth (0..1); `ang_*` = angle (degrees) of a free segment relative
to the body axis.

## Usage

``` r
fishmorph_reconstruction_defaults(preset = c("template", "app"))
```

## Arguments

- preset:

  Which default set to return. `"template"` is the generic FISHMORPH
  gabarit; `"app"` is the set recalibrated on the medians of the
  already-digitized species in the FISHMORPH workbook, as used by
  [`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)
  (head and pectoral placed more realistically; `o_PF` is negative
  because the pectoral insertion sits below the body midline).

## Value

A named list of default parameter values.
