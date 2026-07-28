# State of the journals in a directory

To be called FIRST whenever a consolidation returns an empty result: it
says immediately whether the directory is the right one, which files are
in it, and how many records each contains. A journal with 0 records is
the normal state of a session opened then closed without saving
anything: the app creates the file at LAUNCH, not at the first
"Enregistrer".

## Usage

``` r
fm_journal_status(journal_dir)
```

## Arguments

- journal_dir:

  Journal directory.

## Value

data.frame: file, bytes, n_lines, n_records, n_points, period.
