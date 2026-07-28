# Launch the reconstruction tool (deprecated)

Since version 0.2.0 the digitizing tool is
[`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md),
which works directly on the FISHMORPH workbook, handles three working
queues (reconstruct, correct, new photographs) and secures every record
with an append-only journal.

## Usage

``` r
launch_fishmorph_reconstructor(segments_csv = NULL, ...)
```

## Arguments

- segments_csv:

  Ignored. Kept so that existing calls do not break.

- ...:

  Passed to
  [`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md).

## Value

Invisibly `NULL`.

## Details

The former prototype took one photograph at a time and exported an
isolated CSV: there is no exact correspondence between its arguments and
those of the new tool, which requires a workbook. This function warns,
then redirects.

## See also

[`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)
