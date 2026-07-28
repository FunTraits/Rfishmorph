# Bundled global fish phylogeny

Loads the FISHMORPH phylogenetic tree bundled with the package, for use
with
[`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md)
and the `"missforest_phylo"` imputation option of
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
and
[`impute_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/impute_landmarks.md).
This is the same `FishMORPH_Phylogeny.rds` object used by intraitR.

## Usage

``` r
load_fishmorph_phylogeny()
```

## Value

An object of class `"phylo"` (tip labels formatted `"Genus.species"`).

## See also

[`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md),
[`impute_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/impute_landmarks.md),
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
