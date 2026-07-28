# Open a session journal (append-only)

Creates `journal_dir` if needed and a TSV file specific to the session.
The file is only ever APPENDED to: it is never re-read nor rewritten by
the app, and becomes immutable the moment the session ends.

## Usage

``` r
fm_journal_open(journal_dir, operator = NULL, app_version = NA_character_)
```

## Arguments

- journal_dir:

  Journal directory.

- operator:

  Operator identifier (default: the system user).

- app_version:

  Version of the digitizing tool, traced in every row.

## Value

A journal "handle" to pass to
[`fm_journal_append()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_append.md).
