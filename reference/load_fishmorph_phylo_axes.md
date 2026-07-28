# Precomputed phylogenetic PCoA axes for the FISHMORPH species pool

Loads `pcoaPhylogenyFish.rds`, the **precomputed** principal-coordinate
axes of the global fish phylogeny for the 8,970 FISHMORPH species (10
axes, `Eigen.1` to `Eigen.10`, ordered by decreasing eigenvalue).

## Usage

``` r
load_fishmorph_phylo_axes(file = NULL, k = NULL, refresh = FALSE)
```

## Arguments

- file:

  Optional path to an alternative axis table: either an `.rds` holding a
  data frame, or a whitespace-separated text file with species as row
  names and one column per axis. `NULL` uses the bundled file.

- k:

  Number of axes to return (first `k` columns). `NULL` returns all.

- refresh:

  Force a re-read instead of using the session cache.

## Value

A data frame with a `species` column (`Genus_species`) followed by the
axis columns, named `phylo_1`, `phylo_2`, ...

## Why precomputed

The alternative,
[`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md),
eigendecomposes the patristic distance matrix of the tree. That matrix
is *n by n*: for 8,970 species it is roughly 80 million entries, and the
decomposition is cubic in *n*. Recomputing it on every imputation is
both slow and, more importantly, **not reproducible across calls** – the
axes depend on which subset of species happens to be present in the data
at hand, so two analyses on different subsets end up in different
phylogenetic coordinate systems and are not comparable.

Reading a fixed table instead makes the axes a *property of the
phylogeny* rather than of the current dataset. Every imputation,
whatever the species subset, then lives in one and the same phylogenetic
space.

## File format

The bundled table is a compressed `.rds` (about 540 kB against 1.8 MB
for the original whitespace-separated text). Both formats are accepted
and dispatched on the extension, so `file` can point at either. To
regenerate the `.rds` from the text source:

    txt <- read.table("pcoaPhylogenyFish.txt", header = TRUE)
    ax  <- data.frame(species = gsub("[ ._]+", "_", rownames(txt)), txt,
                      row.names = NULL)
    names(ax)[-1] <- paste0("phylo_", seq_len(ncol(txt)))
    saveRDS(ax, "pcoaPhylogenyFish.rds", compress = "xz")

## See also

[`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md)
to recompute axes from a tree,
[`load_fishmorph_phylogeny()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylogeny.md)
for the tree itself.

## Examples

``` r
ax <- load_fishmorph_phylo_axes(k = 3)
dim(ax)
#> [1] 8970    4
head(ax, 3)
#>                   species  phylo_1    phylo_2   phylo_3
#> 1        Aaptosyax_grypus 1.163439 -0.4148371 0.3580825
#> 2  Abactochromis_labrosus 1.163439 -0.4148371 0.3580825
#> 3 Abbottina_liaoningensis 1.163944 -0.4152019 0.3587335
```
