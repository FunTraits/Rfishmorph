# Rebuild the analysable table from the journals

For each key (`row_key`), only the LAST record is kept – maximum
timestamp, maximum `record_id` in case of a tie. Earlier records stay in
the journals: they are the history of corrections, readable through
[`fm_journal_history()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_history.md),
and they are never lost.

## Usage

``` r
fishmorph_consolidate(
  journal_dir,
  long = FALSE,
  drop_na_points = TRUE,
  out_csv = NULL,
  out_xlsx = NULL
)
```

## Arguments

- journal_dir:

  Journal directory, or an already-read data.frame.

- long:

  TRUE -\> return the long format kept (one row per point) instead of
  the wide table.

- drop_na_points:

  TRUE (default) -\> points marked "na" come out as NA. FALSE -\>
  whatever coordinates they carry are kept.

- out_csv, out_xlsx:

  Optional export paths (the xlsx one requires openxlsx).

## Value

A wide data.frame: one row per key, columns `<n>_X` / `<n>_Y`.
