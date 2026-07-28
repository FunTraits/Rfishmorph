# ATOMIC write of an openxlsx workbook

`saveWorkbook()` overwrites its target in place: while it is being
rewritten (seconds, for a workbook of several Mb) the file is in an
intermediate state, and an interruption destroys it. We therefore write
to a temporary file in the SAME directory – a necessary condition for
the rename to be atomic, a cross-volume rename being in fact a copy –
then switch by renaming.

## Usage

``` r
fm_save_workbook_atomic(wb, path, keep_prev = TRUE)
```

## Arguments

- wb:

  An openxlsx object.

- path:

  Target path.

- keep_prev:

  Keep the previous generation (default TRUE).

## Value

TRUE (invisibly) if the write succeeded.

## Details

The old file is not deleted but moved to ".prev.xlsx", which gives a
one-generation backup at no cost. Should the final rename fail, the old
file is restored.
