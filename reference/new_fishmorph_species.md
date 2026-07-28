# Create a FISHMORPH record for a new species

Builds a single, schema-compliant FISHMORPH row from digitized
landmarks, a set of segments, or ready-made ratios, optionally resolving
the name against FishBase and running basic quality control.

## Usage

``` r
new_fishmorph_species(
  species,
  landmarks = NULL,
  segments = NULL,
  ratios = NULL,
  scale_cm = 1,
  family = NA,
  order = NA,
  genus = NULL,
  max_body_length = NA,
  max_body_width = NA,
  iucn = NA,
  validate = FALSE
)
```

## Arguments

- species:

  Binomial "Genus species".

- landmarks:

  Optional `fishmorph_landmarks` object (segments and ratios are
  computed from it).

- segments:

  Optional named list/one-row data frame of the 11 segments.

- ratios:

  Optional named list/one-row data frame of the 9 ratios (used when
  neither landmarks nor segments are supplied).

- scale_cm:

  Scale-bar length in cm when `landmarks` are supplied.

- family, order, genus:

  Optional taxonomy; `genus` defaults to the first token of `species`.

- max_body_length, max_body_width:

  Optional `MBl`/`MBw` (cm).

- iucn:

  Optional IUCN status code.

- validate:

  Resolve `species` via
  [`validate_species_names()`](https://funtraits.github.io/Rfishmorph/reference/validate_species_names.md)
  (default `FALSE`; requires rfishbase).

## Value

A one-row data frame with taxonomy, the 9 ratios, the 11 segments (as
`seg_*` columns) and `MBl`/`MBw`/`IUCN`. `attr(, "qc")` holds the
[`check_infinite_ratios()`](https://funtraits.github.io/Rfishmorph/reference/check_infinite_ratios.md)
report.

## Examples

``` r
new_fishmorph_species(
  "Genus novus",
  segments = list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6,
                  PFi = 1.9, PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1, CFd = 3))
#>       Species Species_accepted taxonomy_status Genus Family Order   BEl     VEp
#> 1 Genus novus             <NA>            <NA> Genus     NA    NA 3.125 0.34375
#>    REs OGp       RMl  BLs     PFv  PFs CPt seg_Bl seg_Bd seg_Hd seg_Eh seg_Mo
#> 1 0.25 0.5 0.5416667 0.75 0.59375 0.21   3     10    3.2    2.4    1.1    1.6
#>   seg_PFi seg_PFl seg_Ed seg_Jl seg_CPd seg_CFd MBl MBw IUCN
#> 1     1.9     2.1    0.6    1.3       1       3  NA  NA   NA
```
