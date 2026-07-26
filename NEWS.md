# Rfishmorph 0.2.0

## Package renamed

* `FishMORPHR` is now **`Rfishmorph`**. Update `library()` calls accordingly.

## Digitizing application

* New `launch_fishmorph_digitizer()`: works directly on the FISHMORPH workbook
  with three switchable queues — `"reconstruct"` (species without landmarks),
  `"correct"` (already landmarked, reloaded from the workbook) and `"new"`
  (photographs absent from the workbook, appended to a `new_specimens` sheet).
  Adds a broken body axis with hinge points 22/24/25 for curved specimens, an
  optional 20/21 scale bar giving `mm_per_px`, and constrained editing that
  enforces the FISHMORPH geometric conventions live.
* In `"new"` mode there are no measured segments: points are seeded from the
  **median segment/Bl proportions of the 9556 reference species**, then corrected
  by hand. No length is locked in that mode.
* `launch_fishmorph_reconstructor()` is **deprecated** and redirects to the new
  tool. The old single-photo prototype was removed: two implementations of the
  same geometry were bound to diverge.

## Data layer: append-only journal

* New `fm_journal_open()`, `fm_journal_append()`, `fm_journal_read()`,
  `fm_journal_status()`, `fm_journal_history()`, `fishmorph_consolidate()`,
  `fishmorph_journal_qc()`.
* Every save is appended to a per-session TSV in long format (one row per point)
  before anything is written to the workbook. A crash can at worst truncate the
  last line, which is detected and discarded on read.
* Each point records a `status`: `placed`, `seeded` (never verified by the
  operator), `derived`, `na`. This distinction is invisible in a wide
  coordinate table and is what makes quality control possible.
* Workbook writes are now **atomic** (`fm_save_workbook_atomic()`: temporary file
  then rename, previous generation kept as `.prev.xlsx`) and batched via
  `xlsx_flush_every`, taking the multi-megabyte rewrite out of the input loop.

## Data layer: derived DuckDB database

* New `fishmorph_build_db()`, `fishmorph_db_connect()`, `fishmorph_validate()`.
* Three constrained tables (`record`, `specimen`, `landmark_obs`) and three
  views (`v_landmarks_wide`, `v_ratios`, `v_specimen_qc`), plus Parquet and CSV
  exports for archiving.
* `fishmorph_validate()` adds morphometric plausibility on top of SQL
  constraints, comparing each specimen to the empirical envelope (quantiles
  0.001 and 0.999) of the 9556 reference species.

## Exploration application

* New `launch_fishmorph_space()`: the former stand-alone *FishMorphSpace* app,
  now shipped in `inst/shiny/fishmorph_space/`, with the 9556-species trait table
  embedded in `inst/extdata/fishmorph_data.csv` (`fishmorph_space_data()`).
  A `data =` argument points it at a more recent table.

## Phylogenetic imputation

* New `load_fishmorph_phylo_axes()`: reads `inst/extdata/Phylogeny/
  pcoaPhylogenyFish.rds`, the **precomputed** PCoA axes of the global fish
  phylogeny (8,970 species, 10 axes), cached once per session. Shipped as a
  compressed `.rds` (540 kB instead of 1.8 MB of text); the loader still accepts
  the whitespace-separated text format, dispatching on the file extension.
* `"missforest_phylo"` now uses that table by default instead of
  eigendecomposing the patristic distance matrix on every call. Beyond the cost,
  this fixes a **comparability** problem: axes recomputed on whichever species
  happened to be present defined a different coordinate system for each
  analysis, so two imputations on two subsets did not live in the same
  phylogenetic space.
* `impute_traits()`, `impute_landmarks()` and `fishmorph_trait_space()` gain a
  `phylo_axes` argument to supply an alternative table. Passing `tree` still
  recomputes from that tree, as before; the bundled tree remains the fallback if
  the table is unreachable.
* The imputation message now names the **source** of the axes, so two runs can be
  told apart.
* **`species` is now a separate argument** from `groups` in `impute_traits()`,
  `impute_landmarks()`, `fishmorph_trait_space()` and
  `compare_segments_landmarks()`. The two were conflated, so
  `"missforest_phylo"` refused to work without a `groups` vector -- yet the
  phylogeny only needs to know which species each row belongs to, not a
  categorical predictor for the forest. `species` is auto-detected from a
  `Genus.species` / `Species` / `species` column (or, for landmarks, from the
  metadata or the specimen names); `groups` is no longer auto-filled with
  species. In `compare_segments_landmarks()` the key is taken from `id_col`,
  not from `group_col`.
* A `groups` factor with more than 53 levels is now dropped from the missForest
  predictors with a warning instead of failing: `randomForest` cannot handle more
  than 53 categories, so auto-filling `groups` with several thousand species
  names would have made the imputation error out.

## Bug fixes

* `correct_geometry_conventions()` failed with `'vec' must be sorted
  non-decreasingly and not contain NAs` when a hinge landmark (22, 24 or 25)
  projected outside the snout-to-caudal segment. Such a hinge is now discarded
  from the axis polyline, with an aggregated warning naming the specimens.

## Dependencies

* All application and database packages are in `Suggests`; each launcher checks
  its own dependencies and prints a ready-to-paste `install.packages()` call.

## Earlier in this cycle

* `project_fishmorph()` is now a faithful port of `intraitR::project_fishmorph()`
  (specimens projected into the frozen FISHMORPH space built from a reference
  database), with `print()` and `plot()` methods. `plot(proj, style = "hull")`
  reproduces the intraitR figure: reference kernel-density heatmap + per-species
  convex hulls / spider / density, species legend, optional loading arrows and
  `itv_reference` points. Added `group_colors()` / `reset_group_colors()`.
* Imputation aligned with intraitR: `impute_landmarks()`, `phylo_pcoa()`,
  `load_fishmorph_phylogeny()`, and `fishmorph_trait_space(na_action = ...)`
  including `"missforest_phylo"`.

# Rfishmorph 0.1.0

First release. FISHMORPH routines split out of `intraitR` into a stand-alone
package.

* Schema and core measurements: `fishmorph_schema()`, `fishmorph_segments()`,
  `fishmorph_ratios()`, `fishmorph_traits()`.
* Landmark container and I/O interoperable with `intrait_landmarks`:
  `fishmorph_landmarks()`, `read_landmarks_csv()`, `write_landmarks_csv()`.
* Geometry: `standardize_geometry()`, `correct_geometry_conventions()`.
* Reconstruction (inverse of the segments) with an exact round-trip guarantee:
  `reconstruct_fishmorph_landmarks()`, `check_reconstruction_roundtrip()`,
  `launch_fishmorph_reconstructor()`.
* Functional trait space (frozen PCA + projection): `fishmorph_trait_space()`,
  `project_fishmorph()`, `load_fishmorph_reference()`.
* Visualization (ggplot2 + base fallback): `plot_landmarks()`,
  `plot_trait_space()`, `plot_trait_distributions()`,
  `plot_segment_landmark_agreement()`.
* Quality control: `compare_segments_landmarks()`, `agreement_metrics()`,
  `check_infinite_ratios()`, `check_geometry_conventions()`.
* Species management via `rfishbase`: `validate_species_names()`,
  `update_fishmorph_taxonomy()`, `freshwater_fish_list()`,
  `new_fishmorph_species()`, `add_fishmorph_species()`.
