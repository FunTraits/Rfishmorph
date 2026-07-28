# Append one record (one specimen) to the journal

A "record" = one press of "Enregistrer", that is one row per landmark,
all sharing the same `record_id`. Writing is a plain
`cat(append = TRUE)` of a block of text built beforehand: the existing
file is never re-read nor rewritten, so an interruption can only
truncate the last line (which will be discarded when read).

## Usage

``` r
fm_journal_append(
  jr,
  row_key,
  coords,
  points,
  status = NULL,
  species = NA,
  photo_file = NA,
  mode = NA,
  target_sheet = NA,
  img_w = NA,
  img_h = NA,
  ruler_mm = NA,
  mm_per_px = NA
)
```

## Arguments

- jr:

  Handle returned by
  [`fm_journal_open()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_open.md).

- row_key:

  Deduplication key (species, or photo file in "new" mode).

- coords:

  Two-column matrix (X, Y) indexed by landmark number.

- points:

  Landmark numbers to record.

- status:

  Named vector (name = point number) of statuses; default "placed".

- species, photo_file, mode, target_sheet, img_w, img_h, ruler_mm,
  mm_per_px:

  Metadata.

## Value

The `record_id` written (invisibly), or NULL if there was nothing to
write.
