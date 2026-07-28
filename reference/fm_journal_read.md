# Read and concatenate every journal in a directory

Tolerant by construction: a last line truncated by a crash is discarded
(mandatory columns missing), and a journal written by an earlier version
(fewer columns) is filled with NA.

## Usage

``` r
fm_journal_read(journal_dir)
```

## Arguments

- journal_dir:

  Journal directory (or a vector of directories).

## Value

A LONG data.frame, one row per point and per record.
