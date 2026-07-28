# Read digitized landmarks from a CSV file

Accepts two layouts. **Wide**: one row per specimen with columns
`1_X, 1_Y, 2_X, 2_Y, ...` plus an identifier column (`specimen`,
`Genus.species`, `id` or `species`). **Long**: columns `specimen`,
`landmark`, `X`, `Y`.

## Usage

``` r
read_landmarks_csv(file, sep = ",", dec = ".", id_col = NULL)
```

## Arguments

- file:

  Path to the CSV file.

- sep:

  Field separator (default `","`; the FISHMORPH exports use `";"`).

- dec:

  Decimal mark.

- id_col:

  Optional name of the identifier column.

## Value

A `fishmorph_landmarks` object.

## See also

[`write_landmarks_csv()`](https://funtraits.github.io/Rfishmorph/reference/write_landmarks_csv.md)
