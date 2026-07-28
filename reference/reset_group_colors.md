# Reset the session-level species colour cache

The ordination plot methods draw each species in a colour taken from a
session-persistent cache, so a species keeps the same colour across
figures. `reset_group_colors()` clears that cache. Ported from intraitR.

## Usage

``` r
reset_group_colors()
```

## Value

Invisibly `NULL`.

## See also

[`group_colors()`](https://funtraits.github.io/Rfishmorph/reference/group_colors.md)
