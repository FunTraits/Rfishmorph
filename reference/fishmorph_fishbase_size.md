# Maximum standard length and weight from FishBase

Returns, per species, the maximum STANDARD length in centimetres and the
maximum weight in grams – the two raw quantities behind the `MBl` and
`MBw` columns of the FISHMORPH tables, which store them as
`log10(x + 1)`.

## Usage

``` r
fishmorph_fishbase_size(
  species,
  convert_length = TRUE,
  ratio_bounds = c(0.5, 1),
  verbose = TRUE
)
```

## Arguments

- species:

  Character vector of names, in any of the `"Genus species"`,
  `"Genus.species"` or `"Genus_species"` forms.

- convert_length:

  Convert a non-standard length type to SL (default `TRUE`). `FALSE`
  restricts the result to species FishBase already reports in SL, which
  is what the published FISHMORPH column did.

- ratio_bounds:

  Bounds on the accepted SL / (measured length) ratio (default
  `c(0.5, 1)`).

- verbose:

  Report the counts at each step (default `TRUE`).

## Value

A data frame with `Species`, `MBl_cm`, `MBw_g`, `length_type` (the type
FishBase reported), `length_source` (`"SL"`, `"converted"` or `NA`),
`weight_source` (`"observed"`, `"a*L^b"` or `NA`) and `lw_type`.

## What is done, and why

The length comes from
[`rfishbase::species()`](https://docs.ropensci.org/rfishbase/reference/species.html),
in the type given by `LTypeMaxM`. A standard length is taken as it is; a
total or fork length is CONVERTED through the FishBase LENGTH-LENGTH
table, whose orientation is measured rather than assumed (see the note
on `ropensci/rfishbase#119` in the source). A conversion whose result
falls outside `ratio_bounds` – a standard length below half the total
length, or above it – is refused: the species then comes back with `NA`
rather than with a number no one can check.

The weight is the maximum published weight when FishBase has one, and
`a * L^b` otherwise, `a` and `b` being taken from the length-weight
study with the highest coefficient of determination, preferring a
regression fitted on standard length. `weight_source` records which of
the two it is: an observed maximum and a derived one are not the same
quantity, and a column that mixes them without saying so cannot be
audited.

## References

Froese, R. and D. Pauly, eds. FishBase, <https://www.fishbase.org>.

## See also

[`build_fishmorph_landmark_table()`](https://funtraits.github.io/Rfishmorph/reference/build_fishmorph_landmark_table.md),
whose `fishbase_size` argument calls this to fill the species the
segment table cannot supply.

## Examples

``` r
if (FALSE) { # \dontrun{
fishmorph_fishbase_size(c("Gobio occitaniae", "Salmo trutta"))
} # }
```
