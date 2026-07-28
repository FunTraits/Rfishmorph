# Agreement metrics between two paired numeric vectors

Agreement metrics between two paired numeric vectors

## Usage

``` r
agreement_metrics(x, y)
```

## Arguments

- x, y:

  Numeric vectors of equal length (`x` = landmark-derived, `y` =
  published, by convention, so `bias = mean(x - y)`).

## Value

A named numeric vector: `n`, `pearson`, `spearman`, `bias`, `rmse`,
`mae`.
