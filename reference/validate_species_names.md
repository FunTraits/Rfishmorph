# Validate and resolve species names against FishBase

Cleans binomials and resolves them to currently accepted FishBase names,
turning synonyms into valid names. Uses
[`rfishbase::validate_names()`](https://docs.ropensci.org/rfishbase/reference/validate_names.html)
(and `synonyms()` for status) when available.

## Usage

``` r
validate_species_names(species, verbose = TRUE)
```

## Arguments

- species:

  Character vector of "Genus species" names.

- verbose:

  Print a short summary (default `TRUE`).

## Value

A data frame with `input`, `cleaned`, `accepted` (validated FishBase
name or `NA`), `status` (`"accepted"`, `"synonym"`, `"unresolved"`) and
`changed` (logical).

## Examples

``` r
if (FALSE) { # \dontrun{
validate_species_names(c("Salmo trutta", "Barbus barbus", "Esox_lucius"))
} # }
```
