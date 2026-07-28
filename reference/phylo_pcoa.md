# Phylogenetic Principal Coordinates Analysis

Derives quantitative phylogenetic axes from a tree by PCoA of its
patristic (cophenetic) distances among species. Faithful port of
`intraitR::phylo_pcoa()`.

## Usage

``` r
phylo_pcoa(
  tree,
  species = NULL,
  k = NULL,
  correction = c("none", "cailliez", "lingoes"),
  ultrametric = TRUE,
  ultrametric_method = c("nnls", "extend")
)
```

## Arguments

- tree:

  An object of class `"phylo"` (e.g.
  [`load_fishmorph_phylogeny()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylogeny.md)).

- species:

  Optional character vector of species to retain. Names are matched
  after collapsing spaces/underscores/dots.

- k:

  Number of phylogenetic axes to keep (default: all positive-eigenvalue
  axes).

- correction:

  `"none"` (default), `"cailliez"` or `"lingoes"` (see
  [`ape::pcoa()`](https://rdrr.io/pkg/ape/man/pcoa.html)).

- ultrametric:

  Coerce the tree to ultrametric before computing distances (default
  `TRUE`; uses phytools).

- ultrametric_method:

  `"nnls"` (default) or `"extend"`.

## Value

An object of class `"fishmorph_phylopcoa"` with `traits` (species +
`PCoA1..PCoAk`), `var_explained`, `k`, `correction`, `tree` and
`dropped_species`.

## Details

For the FISHMORPH species pool you normally want
[`load_fishmorph_phylo_axes()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylo_axes.md)
instead: it returns the same kind of axes, precomputed once over all
8,970 species, so that successive analyses share a single phylogenetic
coordinate system. Use `phylo_pcoa()` when you work with a different
tree, or want a correction (`"cailliez"`, `"lingoes"`).

## See also

[`load_fishmorph_phylogeny()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylogeny.md),
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
