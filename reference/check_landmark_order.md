# Report vertical-ordering inconsistencies in landmarks (no correction)

Dry-run companion to
[`correct_landmark_order()`](https://funtraits.github.io/Rfishmorph/reference/correct_landmark_order.md):
returns which specimens have impossible dorsal-ventral landmark
orderings and how many landmarks would be moved, without altering the
data.

## Usage

``` r
check_landmark_order(landmarks, chains = .FM_ORDER_CHAINS)
```

## Arguments

- landmarks:

  A `fishmorph_landmarks` object.

- chains:

  Chains to check (see
  [`correct_landmark_order()`](https://funtraits.github.io/Rfishmorph/reference/correct_landmark_order.md)).

## Value

A data frame (`specimen`, `n_corrected`, `flagged`), the rows with
`flagged = TRUE` being the inconsistent specimens.
