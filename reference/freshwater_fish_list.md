# Build a global list of freshwater fishes

Returns FishBase species flagged as occurring in fresh water. Uses the
FishBase `ecology`/`species` tables (`Fresh == -1`) via rfishbase.
Results are large; cache them for reuse.

## Usage

``` r
freshwater_fish_list(include_brackish = FALSE, fields = NULL)
```

## Arguments

- include_brackish:

  Also include brackish-water species (default `FALSE`).

- fields:

  Extra species-level fields to return (e.g. `"Family"`).

## Value

A data frame with at least `Species` and the freshwater/brackish flags,
one row per species.

## Examples

``` r
if (FALSE) { # \dontrun{
fw <- freshwater_fish_list()
nrow(fw)
} # }
```
