# Functions moved to intraitR (defunct)

Rfishmorph no longer measures: the FISHMORPH digitizer, its journal, the
DuckDB database and the ml-morph prediction are in intraitR since
Rfishmorph 0.18.0 / intraitR 1.44.0. Each of these stops with the name
of its replacement:

## Usage

``` r
launch_fishmorph_digitizer(...)

launch_fishmorph_reconstructor(...)

fm_journal_open(...)

fm_journal_append(...)

fm_journal_read(...)

fm_journal_status(...)

fm_journal_history(...)

fishmorph_consolidate(...)

fishmorph_journal_qc(...)

fm_save_workbook_atomic(...)

fishmorph_build_db(...)

fishmorph_db_connect(...)

fishmorph_validate(...)

fm_mlmorph_resources(...)
```

## Arguments

- ...:

  Ignored.

## Value

Nothing: each stops with an error.

## Details

|  |  |
|----|----|
| Rfishmorph (removed) | intraitR |
| `launch_fishmorph_digitizer()` | `digitize_fishmorph_reference()` |
| `launch_fishmorph_reconstructor()` | `digitize_fishmorph_reference()` |
| `fm_journal_open()`, `fm_journal_append()` | `landmark_journal_open()`, `landmark_journal_append()` |
| `fm_journal_read()` | `landmark_journal_read()` (reads the old journals too) |
| `fm_journal_status()` | `landmark_journal_status()` |
| `fm_journal_history()` | `landmark_journal_history()` |
| `fishmorph_consolidate()` | `consolidate_landmarks()` |
| `fishmorph_journal_qc()` | `landmark_journal_qc(expect = c(1:19, 22, 23))` |
| `fm_save_workbook_atomic()` | `write_workbook_atomic()` |
| `fishmorph_build_db()`, `fishmorph_db_connect()`, `fishmorph_validate()` | same names in intraitR |
| `fm_mlmorph_resources()` | the `mlmorph_dir`/`predictor`/`python` arguments of `digitize_fishmorph_reference()` |
