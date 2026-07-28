# Bundled data

Rfishmorph ships three files under `inst/extdata`:

- `fishmorph_data.csv`:

  The **full** FISHMORPH trait table (`;`-separated): 8,970 species with
  all 9 ratios complete, plus `Species, Family, Order, Genus`, `MBl`,
  `MBw` and `IUCN` (8,384 assessed). This is the default of
  [`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)
  since version 0.2.0, and the table explored by
  [`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md).

- `fishmorph_reference_sample.csv`:

  A 400-species subset of the above, identical in columns and scale, for
  fast examples and tests. Load it with
  `load_fishmorph_reference("sample")`.

- `example_landmarks.csv`:

  Two reconstructed specimens in wide layout (`specimen`, `1_X`, `1_Y`,
  ...). Read with
  [`read_landmarks_csv()`](https://funtraits.github.io/Rfishmorph/reference/read_landmarks_csv.md).

## Details

Two further files live under `inst/extdata/Phylogeny`:

- `FishMORPH_Phylogeny.rds`:

  The global fish tree, class `"phylo"`. Loaded by
  [`load_fishmorph_phylogeny()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylogeny.md).

- `pcoaPhylogenyFish.rds`:

  The **precomputed** phylogenetic PCoA axes of that tree (8,970
  species, 10 axes). Loaded by
  [`load_fishmorph_phylo_axes()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylo_axes.md)
  and used by the `"missforest_phylo"` imputation methods, so that every
  analysis shares one phylogenetic coordinate system.

## Trait scale

Both trait tables are **already** `log10(x + 1)` transformed. Passing
them to
[`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
with `log = TRUE` would take the logarithm twice. Ratios recomputed from
landmarks by
[`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md)
are on the raw scale and do need the transform.

## Examples

``` r
ref <- load_fishmorph_reference()
#> FISHMORPH reference: segment (published) -- 8970 species.
nrow(ref)
#> [1] 8970
head(ref)
#>                   Species        Family         Order         Genus        REs
#> 1        Aaptosyax grypus    Cyprinidae Cypriniformes     Aaptosyax 0.13830884
#> 2  Abactochromis labrosus     Cichlidae   Perciformes Abactochromis 0.13154807
#> 3 Abbottina liaoningensis    Cyprinidae Cypriniformes     Abbottina 0.11793451
#> 4 Abbottina obtusirostris    Cyprinidae Cypriniformes     Abbottina 0.09542385
#> 5     Abbottina rivularis    Cyprinidae Cypriniformes     Abbottina 0.12673377
#> 6       Aborichthys kempi Nemacheilidae Cypriniformes   Aborichthys 0.11149201
#>         VEp       RMl        OGp       BEl       BLs        PFv        PFs
#> 1 0.2102226 0.2715300 0.21022258 0.7465652 0.1717209 0.10502790 0.05846702
#> 2 0.2176681 0.1495753 0.17891930 0.5793340 0.2361493 0.12390694 0.08674951
#> 3 0.1813252 0.1502099 0.08062114 0.7454128 0.2018053 0.04356546 0.07488693
#> 4 0.1697233 0.1261569 0.12143600 0.7581949 0.2315525 0.00000000 0.07922101
#> 5 0.1898094 0.1394888 0.13818766 0.7639892 0.2333829 0.00000000 0.06105147
#> 6 0.2308113 0.1440424 0.11839704 0.9146080 0.2073317 0.07154540 0.06332746
#>         CPt       MBl      MBw IUCN
#> 1 0.6383885 2.1172713 4.477136   CR
#> 2 0.5472321 1.0969100 1.767808   LC
#> 3 0.4920029 0.8808136 1.472751 <NA>
#> 4 0.4403627 0.8239619 1.161368 <NA>
#> 5 0.6204937 1.0610744 1.797866   LC
#> 6 0.4156333 0.9590414 1.424925   NT
lm <- read_landmarks_csv(
  system.file("extdata", "example_landmarks.csv", package = "Rfishmorph"))
fishmorph_segments(lm, scale_cm = 1)
#>          specimen Bl  Bd  Hd  Eh  Mo PFi      PFl  Ed       Jl CPd CFd
#> 1    Salmo_trutta 12 3.0 2.6 1.2 1.7 2.0 2.300000 0.7 1.499999 1.1 3.1
#> 2 Cyprinus_carpio 15 5.2 3.4 1.6 2.1 2.6 2.899999 0.8 1.400000 1.6 4.0
```
