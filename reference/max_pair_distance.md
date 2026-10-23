# Diameter of a point cloud: the distance between its two most distant points

The largest Euclidean distance between any two points of a planar cloud.
Its purpose in this package is to be a **denominator**: a distance read
off an ordination – a specimen to its species centroid, a centroid to a
reference point – is in score units, which are arbitrary in magnitude
and depend on the axis pair displayed, so the number is uninterpretable
on its own. Divided by the diameter of the cloud it is read against, it
becomes a share of the total spread of that cloud: "this individual sits
7% of the FISHMORPH morphospace away from its species' mean" is a
statement a reader can weigh, and it is comparable between axis pairs
and between campaigns.

## Usage

``` r
max_pair_distance(x, y = NULL)
```

## Arguments

- x:

  Either a numeric vector of first coordinates, or a two-column matrix /
  data.frame of coordinates (in which case `y` is left `NULL`).

- y:

  Numeric vector of second coordinates, the same length as `x`, when `x`
  is a vector.

## Value

A single numeric value, the maximum distance between two points.
`NA_real_` when fewer than two points are finite, so that a caller
dividing by it produces `NA` rather than a spurious number.

## Why the convex hull

The two most distant points of a cloud are necessarily vertices of its
convex hull – a point strictly inside the hull is closer to every other
point than some vertex is – so the search can be restricted to the hull
without approximation. This matters: the FISHMORPH reference holds
~9,500 species, whose full pairwise distance matrix is ~45 million
values (several hundred megabytes), while its hull has a few dozen
vertices. The result is exact, not a sample.

## See also

[`plotly_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/plotly_fishmorph.md)
and
[`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md),
which use it to express tooltip distances as a percentage of the
reference cloud's diameter. Re-exported by intraitR.

## Examples

``` r
set.seed(1)
x <- rnorm(500); y <- rnorm(500)
max_pair_distance(x, y)
#> [1] 6.853371
max_pair_distance(cbind(x, y))
#> [1] 6.853371

# a distance expressed as a share of the cloud's total spread
100 * sqrt((x[1] - mean(x))^2 + (y[1] - mean(y))^2) / max_pair_distance(x, y)
#> [1] 9.640429
```
