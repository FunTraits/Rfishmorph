# Functional-diversity indices of one assemblage

Computes, for a single set of species positioned in a functional space,
the five indices reported by the basin explorer: functional richness
`FRic`, divergence `FDiv`, dispersion `FDis`, evenness `FEve` and Rao's
quadratic entropy `Rao`. All five are computed with EQUAL species
weights – the occurrence tables record presence, not abundance, and
weighting presences by anything would be inventing a quantity the data
do not hold.

## Usage

``` r
fishmorph_basin_indices(scores, axes_ric = 1:2, axes_dist = 1:4)
```

## Arguments

- scores:

  Numeric matrix or data frame of ordination scores, one row per
  species, columns being the axes. Rows with any missing coordinate are
  dropped before anything is computed.

- axes_ric:

  Integer vector of length 2, the columns of `scores` carrying the plane
  on which `FRic` and `FDiv` are measured. Defaults to `1:2`.

- axes_dist:

  Integer vector, the columns of `scores` used by `FDis`, `FEve` and
  `Rao`. Defaults to `1:4`, truncated to what `scores` has.

## Value

A one-row `data.frame` with columns `n` (species actually used), `FRic`,
`FDiv`, `FDis`, `FEve` and `Rao`. An index that the assemblage is too
small or too degenerate to support is `NA`, never `0`.

## Details

The two families of index are deliberately computed on DIFFERENT numbers
of axes, and the split is the honest one rather than the convenient one.
`FRic` and `FDiv` rest on a convex hull, whose volume is meaningless as
soon as the number of species approaches the number of axes and which is
undefined below it; they are computed in the PLANE that the application
draws, so that the number in the table is the area the eye sees. `FDis`,
`FEve` and `Rao` rest on distances, which are stable in higher dimension
and lose information when projected; they use as many axes as
`axes_dist` provides.

Definitions follow Villeger, Mason and Mouillot (2008) for `FRic`,
`FEve` and `FDiv`, Laliberte and Legendre (2010) for `FDis`, and
Botta-Dukat (2005) for `Rao`, all in their equal-weight form:

`FRic` is the area of the convex hull of the assemblage in the
`axes_ric` plane, and needs at least three non-collinear species.

`FDiv` compares each species' distance to the centre of gravity of the
HULL VERTICES with the mean of those distances, so that an assemblage
whose species crowd the middle of its own hull scores low and one whose
species sit near its edge scores high. It needs the same three species
as `FRic`.

`FDis` is the mean distance of the species to the centroid of the
assemblage, and needs two species.

`FEve` is the regularity of the minimum spanning tree linking the
species: the branch lengths are turned into shares, each share is capped
at `1 / (S - 1)`, and the capped sum is rescaled to `[0, 1]`. It needs
three species.

`Rao` is the mean pairwise distance in the sense of Rao's quadratic
entropy, `sum(d_ij) / S^2` with equal weights – which is the mean over
ALL ordered pairs including the S zero self-distances, not the mean over
distinct pairs. The two differ by a factor `(S - 1) / S` and only the
first is Rao's index.

## References

Botta-Dukat, Z. (2005). Rao's quadratic entropy as a measure of
functional diversity based on multiple traits. Journal of Vegetation
Science, 16, 533-540.

Laliberte, E., & Legendre, P. (2010). A distance-based framework for
measuring functional diversity from multiple traits. Ecology, 91,
299-305.

Villeger, S., Mason, N. W. H., & Mouillot, D. (2008). New
multidimensional functional diversity indices for a multifaceted
framework in functional ecology. Ecology, 89, 2290-2301.

## See also

[`prepare_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/prepare_fishmorph_basins.md),
which calls this once per basin,
[`launch_fishmorph_basins()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_basins.md).

## Examples

``` r
set.seed(1)
sc <- matrix(stats::rnorm(200), ncol = 4)
fishmorph_basin_indices(sc)
#>    n     FRic     FDiv     FDis     FEve      Rao
#> 1 50 11.73187 0.705999 1.736231 0.881956 2.429388
```
