# Rebuild the DuckDB database from the journals

Idempotent: two successive calls give the same database. The previous
database is overwritten (it is a derived artefact), never updated in
place.

## Usage

``` r
fishmorph_build_db(
  journal_dir,
  db_path = NULL,
  export_dir = NULL,
  validate = TRUE,
  stop_on_error = FALSE
)
```

## Arguments

- journal_dir:

  Journal directory (or an already-read long data.frame).

- db_path:

  Path of the .duckdb file. NULL -\> no database on disk, everything
  happens in memory (useful to validate without writing anything).

- export_dir:

  Directory for the Parquet + CSV export. NULL -\> no export.

- validate:

  TRUE -\> run fishmorph_validate() and attach the report.

- stop_on_error:

  TRUE -\> abort if anomalies of severity "error" are detected, BEFORE
  writing anything.

## Value

An invisible list: `db_path`, `n_specimens`, `n_points`, `issues`.
