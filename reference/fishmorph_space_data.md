# Path of the bundled FISHMORPH trait table

Returns the path of the CSV used by default by
[`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md),
to inspect it or read it directly.

## Usage

``` r
fishmorph_space_data(source = NULL)
```

## Arguments

- source:

  Which measurement campaign to point at: `"segment"` (the published
  table) or `"landmark"` (traits recomputed from the landmark
  re-digitization). `NULL` (default) follows
  `getOption("fishmorph.source", "segment")`, see
  [`set_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md).

## Value

A file path (character string), or `""` when the requested table is not
bundled – which is the normal state of the landmark table until
[`build_fishmorph_landmark_table()`](https://funtraits.github.io/Rfishmorph/reference/build_fishmorph_landmark_table.md)
has been run.

## Details

A reminder: its trait columns are ALREADY log10(x + 1). Reading them
back to project new individuals therefore needs no further
transformation, and applying one would distort the projection.

## See also

[`set_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md),
[`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)

## Examples

``` r
p <- fishmorph_space_data()
if (nzchar(p)) utils::head(utils::read.csv(p, sep = ";"), 3)
#>                   Species     Family         Order         Genus       REs
#> 1        Aaptosyax grypus Cyprinidae Cypriniformes     Aaptosyax 0.1383088
#> 2  Abactochromis labrosus  Cichlidae   Perciformes Abactochromis 0.1315481
#> 3 Abbottina liaoningensis Cyprinidae Cypriniformes     Abbottina 0.1179345
#>         VEp       RMl        OGp       BEl       BLs        PFv        PFs
#> 1 0.2102226 0.2715300 0.21022258 0.7465652 0.1717209 0.10502790 0.05846702
#> 2 0.2176681 0.1495753 0.17891930 0.5793340 0.2361493 0.12390694 0.08674951
#> 3 0.1813252 0.1502099 0.08062114 0.7454128 0.2018053 0.04356546 0.07488693
#>         CPt       MBl      MBw IUCN
#> 1 0.6383885 2.1172713 4.477136   CR
#> 2 0.5472321 1.0969100 1.767808   LC
#> 3 0.4920029 0.8808136 1.472751 <NA>
```
