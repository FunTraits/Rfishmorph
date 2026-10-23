# Look up species in the FISHMORPH table

Tells, for each name supplied, whether the species is present in the
FISHMORPH trait table, and if not, whether a close spelling or its genus
is. This is the engine of the "Species finder" tab of
[`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md).

## Usage

``` r
find_fishmorph_species(
  species,
  reference = NULL,
  source = NULL,
  max_distance = 2,
  n_suggestions = 3
)
```

## Arguments

- species:

  Character vector of names to look up.

- reference:

  The FISHMORPH table: a data frame with a `Species` column (and
  optionally `Family`, `Order`, `IUCN` and the nine trait columns), or
  the path to a semicolon-separated CSV. `NULL` (default) reads the
  table bundled with the package for `source`.

- source:

  `"segment"` or `"landmark"`, used only when `reference` is `NULL`.
  `NULL` follows
  [`get_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md).

- max_distance:

  Maximum edit distance (Levenshtein) for a `"close"` match. `0`
  disables approximate matching.

- n_suggestions:

  Number of nearest names reported in `suggestions`.

## Value

A data frame with one row per query and the columns `query`,
`query_clean`, `status`, `match` (the FISHMORPH name retained),
`distance` (edit distance to it), `Family`, `Order`, `IUCN` (of the
match, or of the genus for `"genus_only"`), `complete_traits` (whether
the matched species has all nine traits, i.e. is placed in the
morphological space), `n_genus` (number of FISHMORPH species in the
queried genus) and `suggestions` (the nearest names, with their edit
distance in brackets; for a genus query, species of that genus; `NA` for
an exact match).

## Details

Names are first cleaned: underscores and repeated blanks become single
spaces, authorities and subspecies are dropped (only the first two words
are kept), the qualifiers `cf.` and `aff.` are removed, and `sp.`/`spp.`
turn the query into a genus query. Matching is case-insensitive.

The `status` column follows a ladder of certainty: `"exact"` (the
binomial is in the table); `"close"` (no exact match but a name at most
`max_distance` character edits away – typically a typo or a gender
ending, to be checked by eye, never accepted blindly); `"genus_only"`
(the species is absent but other species of its genus are present);
`"absent"`; `"invalid"` (nothing usable after cleaning). A close match
is not a synonymy check: use
[`validate_species_names()`](https://funtraits.github.io/Rfishmorph/reference/validate_species_names.md)
to resolve names against FishBase.

## See also

[`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md),
[`validate_species_names()`](https://funtraits.github.io/Rfishmorph/reference/validate_species_names.md),
[`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)

## Examples

``` r
ref <- data.frame(
  Species = c("Salmo trutta", "Salmo salar", "Gobio gobio"),
  Family  = c("Salmonidae", "Salmonidae", "Gobionidae"),
  Order   = c("Salmoniformes", "Salmoniformes", "Cypriniformes"))
find_fishmorph_species(c("salmo_trutta", "Salmo truta", "Salmo marmoratus",
                         "Gobio sp.", "Esox lucius"), reference = ref)
#>              query      query_clean     status        match distance     Family
#> 1     salmo_trutta     Salmo trutta      exact Salmo trutta        0 Salmonidae
#> 2      Salmo truta      Salmo truta      close Salmo trutta        1 Salmonidae
#> 3 Salmo marmoratus Salmo marmoratus genus_only         <NA>       NA Salmonidae
#> 4        Gobio sp.            Gobio genus_only         <NA>       NA Gobionidae
#> 5      Esox lucius      Esox lucius     absent         <NA>       NA       <NA>
#>           Order IUCN complete_traits n_genus
#> 1 Salmoniformes <NA>              NA       2
#> 2 Salmoniformes <NA>              NA       2
#> 3 Salmoniformes <NA>              NA       2
#> 4 Cypriniformes <NA>              NA       1
#> 5          <NA> <NA>              NA       0
#>                                             suggestions
#> 1                                                  <NA>
#> 2    Salmo trutta [1]; Salmo salar [5]; Gobio gobio [9]
#> 3   Salmo salar [8]; Salmo trutta [8]; Gobio gobio [13]
#> 4                                           Gobio gobio
#> 5 Gobio gobio [10]; Salmo salar [11]; Salmo trutta [11]
```
