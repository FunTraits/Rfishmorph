# Reconstruct the 21 FISHMORPH landmarks from segments

Reconstruct the 21 FISHMORPH landmarks from segments

## Usage

``` r
reconstruct_fishmorph_landmarks(
  segments,
  anchor_snout = NULL,
  anchor_caudal = NULL,
  px_per_cm = 50,
  params = list(),
  scale_cm = 1,
  specimen = "reconstructed",
  preset = c("template", "app"),
  add_curvature = FALSE
)
```

## Arguments

- segments:

  A data frame (one row) or named list holding the 11 segments
  `Bl, Bd, Hd, Eh, Mo, PFi, PFl, Ed, Jl, CPd, CFd` (in cm).

- anchor_snout, anchor_caudal:

  Optional `c(x, y)` pixel positions of the snout (landmark 1) and
  caudal base (landmark 2). When both are given, the scale is deduced
  from `|2-1| / Bl`. When `NULL`, a synthetic horizontal anchor is
  generated using `px_per_cm`.

- px_per_cm:

  Scale used only for the synthetic anchor.

- params:

  Named list of free parameters (see
  [`fishmorph_reconstruction_defaults()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_reconstruction_defaults.md));
  missing entries take the value from `preset`. Explicit `params` always
  override the preset.

- scale_cm:

  Real length (cm) of the calibration segment (landmarks 20-21).

- specimen:

  Specimen name.

- preset:

  Default placement rules: `"template"` (generic gabarit) or `"app"` to
  reproduce
  [`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)
  / `fishmorph_reconstruct_app` (offsets recalibrated on the workbook
  medians). Both keep the segment lengths **locked** to the input, so
  the round-trip stays exact regardless of preset.

- add_curvature:

  Place the optional body-length curvature landmark 22 on the axis
  midpoint (as the app does). It lies on the straight 1-2 line, so `Bl`
  is unchanged and the round-trip remains exact. Adds a 22nd landmark.

## Value

A `fishmorph_landmarks` object with 21 (or 22) landmarks in the
Cartesian convention (Y up), compatible with
[`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md).

## Examples

``` r
seg <- list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
            PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0)
lm  <- reconstruct_fishmorph_landmarks(seg)                 # template
lm2 <- reconstruct_fishmorph_landmarks(seg, preset = "app") # app rules
fishmorph_segments(lm2, scale_cm = 1)[, c("Bl", "Bd", "Hd")]
#>   Bl  Bd  Hd
#> 1 10 3.2 2.4
```
