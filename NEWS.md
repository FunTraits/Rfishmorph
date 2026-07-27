# Rfishmorph 0.4.0

## The eye vertical is checked, in order

* The save-time check now also verifies the ORDER of the six points that the
  FISHMORPH conventions place on one vertical -- 5, 13, 7, 14, 6, 8, from the
  back downwards: top of the head, top of the eye, centre of the eye, bottom of
  the eye, bottom of the head, body underside. Two things are tested and they
  are not the same statement: that **5 tops the group** (the `Hd` analogue of
  the 3/4 rule for `Bd`), and that **every consecutive pair is in order**, which
  catches a local swap the first test cannot see.
* This is the failure no other check catches, because each pair stays
  internally consistent: with 13 and 14 exchanged -- the eye clicked
  bottom-first -- `Ed` (13-14) keeps its exact length while `Eh` (7-8) silently
  refers to the wrong edge of the eye. Nothing in a coordinate table shows it.
* Settled on the data, as the `Bd` rule was. Over the 4,151 species already
  digitized in the workbook, the expected order holds for **99.5 %** of them:
  8 above 6 in 22 specimens (0.53 %), 13 above 5 in 10 (0.24 %, the same ten as
  "5 does not top the group"), 6 above 14 in 2, 14 above 7 in 1. A convention
  that a hand-digitized corpus already satisfies to that degree is a convention,
  not a preference, and the residue is worth looking at one specimen at a time.
* An inversion is reported but **never corrected automatically**: moving a point
  to satisfy the order would invent a measurement rather than repair one. The
  dialog therefore offers *Measure again* (which selects and zooms on the point
  found on the wrong side, not the reference it was compared with) and *Save
  without correcting*; the *Correct automatically* button only appears when
  there is an extreme-point violation, which is the only kind that can be
  repaired by moving a landmark.
* Same tolerance as the extremes, `max(5 px, 0.003 * Bl)`, and the same
  invariance: heights are read perpendicular to the body axis and the dorsal
  side from the relative position of 3 and 4, so the test holds head left or
  right, photograph flipped, or mirrored. Missing points are stepped over rather
  than breaking the chain.
* New internals `.fm_eye_order_violations()` and `.fm_convention_violations()`;
  the violation tables gain a `kind` column (`"extreme"` / `"order"`).

## A tabbed, themed digitizer

* The side panel of `launch_fishmorph_digitizer()` is a **tabset** — `Specimen`,
  `Display`, `Checks`, `Seed` — instead of one long scroll. The controls
  fall into groups touched at different rhythms (once per specimen, once per
  photograph, once per session), and stacking them in one column put the ones
  used constantly below the ones used never.
* With `bslib` (already in `Suggests`) the page uses a Bootstrap 5 theme, cards
  and a wider sidebar; without it, the same content falls back to the standard
  Shiny layout. No feature depends on `bslib`, only the appearance does. The
  page is deliberately **not** `fillable`: it is a document that scrolls, and in
  a filling page the 620 px photograph is squeezed by the bars above and the
  panels below.
* The **queue selector** (`To reconstruct` / `Correct existing` / `New
  photographs`) moves from the action bar to the head of the side panel: it decides
  what the whole session is doing, and it sat one button away from
  "Save & next". `Mark NA` moves the other way, next to the landmark bar,
  with the zoom controls: those act on the point under the cursor.
* The **landmark bar is bare and on one line** — the buttons share the width
  rather than wrapping, so a given point keeps its place on screen whatever the
  window size. The entry order, the broken-axis conventions and the colour code
  move to a card at the foot of the page: they are read on the first specimen
  and never again, but above the photograph they cost three lines of scroll on
  each of the following thousands.
* A header strip shows what the session IS — workbook, photographs, journal,
  operator — since those are arguments of the launcher and are not editable from
  the app. The `Checks` tab shows the state of both write layers.

## Every point is written, and a lost one is now loud

* Verified end to end: `save_pts = 1..19, 22, 23, 24, 25` for the landmark sheet
  (and `+ 20, 21` for new specimens), the columns `24_X`..`25_Y` being created at
  start-up by `ensure_cols()`. Over the 201 records of the existing journals,
  every record carries its 23 points: 22 `placed` 201/201, 23 `derived` 165 (36
  `na`, the derivation needing 6 and 9), 24 `placed` 196, 25 `placed` 9 — the
  hinges being optional by design.
* The on-screen legend claimed **"24/25 are NOT recorded"**, which had
  been untrue since the hinge columns were added. An operator reading it had
  every reason not to bother placing them. Corrected, and the legend now states
  what is written and why: the hinges are not landmarks and belong in no shape
  analysis, but they define the frames each convention was applied in — without
  them a species reopened for correction comes back with a straight axis.
* Writing a point whose column is missing from the sheet used to `next` in
  silence. It now raises a persistent error naming the points, since a
  hand-edited sheet is the one case where a placed point could vanish without
  trace (the journal, as always, still has it).

# Rfishmorph 0.3.0

## Extreme-point convention checked on save

* `launch_fishmorph_digitizer()` now verifies, when "Save & next" is
  pressed, that landmark 3 is the most **dorsal** and landmark 4 the most
  **ventral** point of the body outline — the definition of `Bd` as the maximum
  body depth. A specimen whose 5 (head top) sits above 3, or whose 6 (head
  bottom) sits below 4, silently under-estimates `Bd`; this is now caught before
  anything reaches the journal or the workbook.
* On violation a dialog offers three routes: **remeasure** (the offending point
  becomes active and the view centres on it), **auto-correct** (3, resp. 4, takes
  the height of the point overshooting it while keeping its position along the
  axis, so `Bd` grows and the 3-4 perpendicularity convention is preserved), or
  **save as is**.
* Heights are measured perpendicular to the body axis 1-2, so a tilted
  photograph does not bias the test, and the dorsal side is inferred from the
  relative position of 3 and 4 — the check therefore holds whatever the
  orientation (head left or right, flipped photograph, "Flip dorsal/ventral"
  ticked). Caudal peduncle and fin (16-19) and appendage tips (12 pectoral,
  15 jaw) are excluded, as are the scale bar (20, 21), the derived point (23)
  and the hinges (24, 25). Tolerance: 0.003 of body length.
* The **derived ventral points 8, 9 and 11 are excluded too**: they are computed
  from landmark 4 (belly line), so testing whether 4 is the lowest point against
  them is circular. Settled on the data rather than by argument -- over the 1,036
  digitized T-26 specimens of the intraitR corpus, including 8/9/11 flags 20.6%
  of the batch (198 of 213 flags are those three points, median overshoot 0.5% of
  `Bl`, i.e. belly-line noise), whereas excluding them flags 1.5% at a median
  overshoot of 6.8% of `Bl`, with a flag rate flat from 0.003 to 0.02 `Bl`.
  The comparison set is therefore 1, 2, 5, 6, 7, 10, 13, 14, 22 -- the landmarks
  that are independent measurements on the body outline.
* Tolerance is `max(5 px, 0.003 * Bl)`. The 5 px absolute floor matters on small
  photographs, where the relative term falls below click noise. It is free:
  compliant T-26 specimens top out at -0.4 px of overshoot (p98) while the
  smallest real breach is 11.8 px, so any floor from 1 to 8 px flags the same
  16 specimens.
* The check is toggled by the new "Check 3/4 (extremes) on save"
  box (on by default).
* New journal status **`"adjusted"`** for points relocated by that automatic
  correction, distinct from `"placed"` (operator-pointed) and `"seeded"`
  (never verified). `fishmorph_journal_qc()` reports them, so an auto-corrected
  `Bd` remains traceable specimen by specimen.

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
