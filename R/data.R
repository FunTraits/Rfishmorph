# =============================================================================
# data.R -- documentation for bundled example data (inst/extdata).
# =============================================================================

#' Bundled data
#'
#' Rfishmorph ships three files under `inst/extdata`:
#' \describe{
#'   \item{`fishmorph_data.csv`}{The **full** FISHMORPH trait table
#'     (`;`-separated): 8,970 species with all 9 ratios complete, plus
#'     `Species, Family, Order, Genus`, `MBl`, `MBw` and `IUCN` (8,384
#'     assessed). This is the default of [load_fishmorph_reference()] since
#'     version 0.2.0, and the table explored by [launch_fishmorph_space()].}
#'   \item{`fishmorph_reference_sample.csv`}{A 400-species subset of the above,
#'     identical in columns and scale, for fast examples and tests. Load it with
#'     `load_fishmorph_reference("sample")`.}
#'   \item{`example_landmarks.csv`}{Two reconstructed specimens in wide layout
#'     (`specimen`, `1_X`, `1_Y`, ...). Read with [read_landmarks_csv()].}
#' }
#'
#' Two further files live under `inst/extdata/Phylogeny`:
#' \describe{
#'   \item{`FishMORPH_Phylogeny.rds`}{The global fish tree, class `"phylo"`.
#'     Loaded by [load_fishmorph_phylogeny()].}
#'   \item{`pcoaPhylogenyFish.rds`}{The **precomputed** phylogenetic PCoA axes
#'     of that tree (8,970 species, 10 axes). Loaded by
#'     [load_fishmorph_phylo_axes()] and used by the `"missforest_phylo"`
#'     imputation methods, so that every analysis shares one phylogenetic
#'     coordinate system.}
#' }
#'
#' @section Trait scale:
#' Both trait tables are **already** `log10(x + 1)` transformed. Passing them to
#' [fishmorph_trait_space()] with `log = TRUE` would take the logarithm twice.
#' Ratios recomputed from landmarks by [fishmorph_ratios()] are on the raw scale
#' and do need the transform.
#'
#' @examples
#' ref <- load_fishmorph_reference()
#' nrow(ref)
#' head(ref)
#' lm <- read_landmarks_csv(
#'   system.file("extdata", "example_landmarks.csv", package = "Rfishmorph"))
#' fishmorph_segments(lm, scale_cm = 1)
#' @name Rfishmorph-data
NULL
