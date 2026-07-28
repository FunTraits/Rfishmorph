# Write landmarks to a CSV file (long layout)

Write landmarks to a CSV file (long layout)

## Usage

``` r
write_landmarks_csv(x, file, cartesian = TRUE, image_height = NULL)
```

## Arguments

- x:

  A `fishmorph_landmarks` object.

- file:

  Output path.

- cartesian:

  If `TRUE` (default) keep the Cartesian convention (Y up). Set `FALSE`
  to flip Y for image (Y-down) exports.

- image_height:

  Image height in pixels, required when `cartesian = FALSE`.

## Value

Invisibly, the written data frame.
