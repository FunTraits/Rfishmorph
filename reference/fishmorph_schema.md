# FISHMORPH digitizing scheme

Returns the full scheme used throughout the package: landmark labels,
the 11 segment/landmark-pair map, the 9 ratio definitions and their
ecological meaning. All numeric routines derive from these constants.

## Usage

``` r
fishmorph_schema()
```

## Value

A named list with elements `landmark_labels`, `segment_pairs`,
`segments`, `ratio_def`, `ratios`, `ratio_long`, `ratio_function`,
`derived_points` and `axis_hinges`.

## Broken body axis

Points 22, 24 and 25 are hinges, not anatomical landmarks. Body length
`Bl` is the arc length along the chain 1 -\> (hinges placed) -\> 2, the
hinges being sorted along the 1-2 chord so that the order in which they
were clicked does not matter. With no hinge placed the chain collapses
to the straight 1-2 distance, which is the historical definition. All
three are recorded like any other point; point 23 is derived and is
*not* a hinge.

## References

Brosse et al. (2021) *Global Ecology and Biogeography* 30:2330-2336.

## Examples

``` r
str(fishmorph_schema())
#> List of 9
#>  $ landmark_labels: chr [1:25] "snout_tip" "caudal_base" "body_dorsal" "body_ventral" ...
#>  $ segment_pairs  :List of 11
#>   ..$ Bl : num [1:2] 1 2
#>   ..$ Bd : num [1:2] 3 4
#>   ..$ Hd : num [1:2] 5 6
#>   ..$ Eh : num [1:2] 7 8
#>   ..$ Mo : num [1:2] 1 9
#>   ..$ PFi: num [1:2] 10 11
#>   ..$ PFl: num [1:2] 10 12
#>   ..$ Ed : num [1:2] 13 14
#>   ..$ Jl : num [1:2] 1 15
#>   ..$ CPd: num [1:2] 16 17
#>   ..$ CFd: num [1:2] 18 19
#>  $ segments       : chr [1:11] "Bl" "Bd" "Hd" "Eh" ...
#>  $ ratio_def      :List of 9
#>   ..$ BEl: chr [1:2] "Bl" "Bd"
#>   ..$ VEp: chr [1:2] "Eh" "Bd"
#>   ..$ REs: chr [1:2] "Ed" "Hd"
#>   ..$ OGp: chr [1:2] "Mo" "Bd"
#>   ..$ RMl: chr [1:2] "Jl" "Hd"
#>   ..$ BLs: chr [1:2] "Hd" "Bd"
#>   ..$ PFv: chr [1:2] "PFi" "Bd"
#>   ..$ PFs: chr [1:2] "PFl" "Bl"
#>   ..$ CPt: chr [1:2] "CFd" "CPd"
#>  $ ratios         : chr [1:9] "BEl" "VEp" "REs" "OGp" ...
#>  $ ratio_long     : Named chr [1:9] "Body elongation" "Vertical eye position" "Relative eye size" "Oral gape position" ...
#>   ..- attr(*, "names")= chr [1:9] "BEl" "VEp" "REs" "OGp" ...
#>  $ ratio_function : Named chr [1:9] "Hydrodynamism / position in the water column" "Vertical position of prey" "Visual acuity" "Feeding position in the water column" ...
#>   ..- attr(*, "names")= chr [1:9] "BEl" "VEp" "REs" "OGp" ...
#>  $ derived_points : int [1:5] 8 9 11 15 22
#>  $ axis_hinges    : int [1:3] 22 24 25
```
