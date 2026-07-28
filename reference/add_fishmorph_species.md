# Append new species to a FISHMORPH table

Adds one or more records (from
[`new_fishmorph_species()`](https://funtraits.github.io/Rfishmorph/reference/new_fishmorph_species.md))
to an existing FISHMORPH reference table, aligning columns, blocking
duplicate species and reporting quality-control flags.

## Usage

``` r
add_fishmorph_species(
  reference,
  new_records,
  species_col = "Species",
  overwrite = FALSE
)
```

## Arguments

- reference:

  A FISHMORPH data frame (e.g. from
  [`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)).

- new_records:

  A single record or a list/data frame of records.

- species_col:

  Species column in `reference` (default `Species`).

- overwrite:

  Replace an existing species instead of erroring (default `FALSE`).

## Value

The augmented reference table. `attr(, "added")` lists the species
added; `attr(, "qc")` collects QC flags.
