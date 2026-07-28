# Update the taxonomy of a FISHMORPH table

Adds resolved FishBase names (and, optionally, updated Family/Order) to
a FISHMORPH trait table, keeping the original names for traceability.

## Usage

``` r
update_fishmorph_taxonomy(data, species_col = NULL, add_classification = TRUE)
```

## Arguments

- data:

  A data frame with a species column.

- species_col:

  Name of the species column (default tries `Species`, `species`,
  `Genus.species`).

- add_classification:

  Also pull `Family`/`Order` from FishBase
  ([`rfishbase::load_taxa()`](https://docs.ropensci.org/rfishbase/reference/load_taxa.html)),
  default `TRUE`.

## Value

`data` with added columns `Species_accepted`, `taxonomy_status` and,
when requested, `Family_fb`, `Order_fb`.
