# Choose the FISHMORPH measurement campaign for the session

Sets `options(fishmorph.source = )`, the default consulted by
[`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md),
[`fishmorph_space_data()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_space_data.md),
[`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md)
and the shiny explorers whenever their own `source` argument is left at
`NULL`. Every function keeps an explicit `source` argument, which always
wins: use the option to switch a whole script, the argument when a
single call must be pinned regardless of the session state.

## Usage

``` r
set_fishmorph_source(source = c("segment", "landmark"))

get_fishmorph_source()
```

## Arguments

- source:

  `"segment"` (published segment measurements, the package default) or
  `"landmark"` (traits recomputed from the landmark re-digitization).

## Value

Invisibly, the previous value.

## See also

[`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md),
`get_fishmorph_source()`,
[`build_fishmorph_landmark_table()`](https://funtraits.github.io/Rfishmorph/reference/build_fishmorph_landmark_table.md)

## Examples

``` r
old <- set_fishmorph_source("segment")
get_fishmorph_source()
#> [1] "segment"
set_fishmorph_source(old)
```
