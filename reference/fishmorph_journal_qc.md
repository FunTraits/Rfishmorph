# Quick quality control of a consolidation

Reports what a table of coordinates does not show: points never checked
(still at their seed), points declared non-measurable, points snapped by
a convention (columns `n_adjusted` / `adjusted`: 3 or 4 brought back to
the maximum body depth by the application), incomplete specimens.

## Usage

``` r
fishmorph_journal_qc(journal_dir, expect = c(1:19, 22L, 23L))
```

## Arguments

- journal_dir:

  Journal directory, or an already-read data.frame.

- expect:

  Points expected for a complete specimen.
