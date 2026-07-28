# Changelog

## Rfishmorph 0.5.0

### The broken body axis, stated explicitly

- [`fishmorph_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_landmarks.md)
  now pads to **25** points instead of 21 (`.FM_N_POINTS`): 19
  anatomical landmarks, the scale bar 20-21, the curvature hinge 22, the
  derived point 23 and the extra axis hinges 24-25. The digitizer has
  been recording 25 points for a while, so the object was one shape and
  the stored data another, and a configuration carrying hinges lost them
  on the way in. `NA` means “not placed” and every routine skips it, so
  the wider frame costs nothing.
- [`fishmorph_schema()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_schema.md)
  gains the labels of points 23-25 and an `axis_hinges` element. It
  described a 22-point scheme while the data carried 25, which is an odd
  thing for a single source of truth to do. `.fm_bl_broken()` now reads
  `.FM_AXIS_HINGES` rather than a hard-coded vector, so scheme and
  computation can no longer drift apart.
- Confirmed and locked by tests: `Bl` is the arc length
  `1 -> (hinges placed) -> 2`, hinges 22, 24 and 25 being treated
  identically and **sorted along the 1-2 chord**, so the order in which
  they were clicked is irrelevant. With no hinge the chain collapses to
  the straight 1-2 distance.
- **Point 23 is not a hinge and never enters the chain.** It lies on the
  line (1, 9), the ventral line running back from the snout, not on the
  body axis; its purpose is the segment 23-6, the axial
  snout-to-head-base distance. Inserting it into the chain sends the
  polyline down to the belly and back, inflating `Bl` by a median 8.5%
  and up to 27% on the 650 species that carry 22, 23 and 24 — an error
  that would propagate to `BEl` and `PFs`, and unevenly, since
  deep-bellied fishes suffer most. Measured on the current data, not
  assumed, and now covered by `test-bl-axis.R`.

### Two measurement campaigns, one API

- The FISHMORPH traits now come in two flavours, and every function that
  reads the global table lets you say which: `source = "segment"` is the
  published table (`fishmorph_data.csv`, 8,970 species, ratios from the
  eleven segments measured on the plates), `source = "landmark"` is
  `fishmorph_data_landmarks.csv`, whose ratios are recomputed from the
  landmark re-digitization through
  [`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md).
  Added to
  [`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md),
  [`fishmorph_space_data()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_space_data.md),
  [`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md),
  [`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md)
  and
  [`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md).
- [`set_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md)
  /
  [`get_fishmorph_source()`](https://funtraits.github.io/Rfishmorph/reference/set_fishmorph_source.md)
  set the session default (`options(fishmorph.source = )`) consulted
  whenever `source` is left `NULL`. The argument always wins over the
  option, so a script can pin one call without disturbing the rest, and
  a shared script never depends on the state of the session that runs
  it.
- [`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md)
  now says out loud which campaign it loaded and how many species it
  holds, and tags the result with `attr(, "fishmorph_source")`. The
  landmark table covers **only the digitized species**, so an analysis
  run on it describes a smaller pool than one run on the segment table;
  that must not be discoverable only after the fact. Pass `quiet = TRUE`
  to silence it.

### Building the landmark table

- [`build_fishmorph_landmark_table()`](https://funtraits.github.io/Rfishmorph/reference/build_fishmorph_landmark_table.md)
  assembles the landmark trait table from the publication workbook
  (`Global_Landmark` sheet) and/or the digitizer DuckDB store, following
  one homogeneous geometric path: landmarks -\>
  [`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md)
  -\>
  [`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md)
  -\> imputation -\> `log10(x + 1)`. Species held by both stores are
  taken from DuckDB, the more recent digitization. Taxonomy, `MBl`,
  `MBw` and `IUCN`, which no landmark can yield, are joined from the
  segment table.
- Ratios missing because a structure is absent or a specimen only partly
  digitized are filled with `na_action = "missforest_phylo"` by default,
  on the raw scale and on the precomputed phylogenetic PCoA axes,
  i.e. the same coordinate system as every other imputation in the
  package. The per-species `n_imputed` column keeps the count, so
  imputed species stay excludable.
- `data-raw/build_fishmorph_landmark_table.R` regenerates the shipped
  snapshot with a pinned seed and prints a segment-vs-landmark
  correlation table. Read that table with care: the reconstruction
  imposes the segment *lengths*, so a high correlation on the size
  ratios is a property of the construction and not evidence of
  agreement. The position ratios `OGp`, `VEp` and `PFv` are where the
  landmark campaign carries independent information.

### Note on comparability

- A “landmark” space is a **refitted** PCA, not a reprojection into the
  segment space. Axis order and sign may differ between campaigns, and
  scores are not comparable term by term without an explicit alignment.

## Rfishmorph 0.4.0

### Coincident points: a measurement of zero

- A bar under the photograph declares the segments that are ZERO on the
  species in view. A zero is a measurement like any other – neither a
  missing value nor a placement error – and the FISHMORPH ratios are
  defined to take it: `OGp = 0` for a mouth opening on the ventral
  profile, `PFv = 0` for a pectoral fin inserted on the belly. Four
  rules: **`Mo = 0`** (9, and 23, take the coordinates of 1),
  **`6 = 8`** (the bottom of the head is the body underside, and 23
  follows 9), **`PFi = 0`** (10 takes the coordinates of 11) and
  **`5 = 13`** (an eye reaching the top of the head).
- That 23 follows 9 under `6 = 8` is a **consequence, not an extra
  convention**: 23 is the intersection of the line (1, 9) with the line
  through 6 parallel to the head axis, and the belly line {9, 8, 11} is
  itself parallel to that axis, so once 6 sits on the belly line that
  parallel IS the belly line and the intersection is 9. Checked
  numerically, tilted photograph included: the derivation lands on 9 to
  machine precision. The rule states it explicitly so that it holds even
  when the derivation is degenerate.
- **Nothing is deleted.** Both points keep a position, both are drawn on
  the photograph and both are written to the workbook; one simply takes
  the coordinates of the other, so the segment between them measures
  zero. A coincidence is a measurement, an absence is `NA`, and the two
  must not be confused downstream.
- In `"reconstruct"` mode a zero already comes from the workbook – the
  points are laid out from the measured segments. The rules are for
  `"new"`, where the points are seeded from medians, and `"correct"`,
  where a specimen is being repaired. They are applied at the END of
  `recon()`, after `.fm_constrain()` and after point 23 is rebuilt,
  because the constrained editing re-derives the ventral points on the
  belly line at every click.
- Which point moves is a protocol decision and is not the same for every
  rule. For the mouth the fixed point is 1, the snout – and 23, built on
  the line (1, 9), is undefined once 9 sits on 1, so it follows 1 rather
  than becoming `NA`. For the two ventral rules the belly line holds: 8
  and 11 are its intersections with the eye and the pectoral verticals,
  so the head bottom and the fin insertion come onto them. For the eye
  at the top of the head, 5 comes onto 13, since moving 13 would change
  `Ed`, a measurement in its own right.
- Points moved by a rule take the `"adjusted"` status in the journal,
  whose meaning widens accordingly: placed by a rule the operator
  invoked, neither pointed at nor left at a seed. Declarations are reset
  for every species.

### The eye vertical is checked, in order

- The save-time check now also verifies the ORDER of the six points that
  the FISHMORPH conventions place on one vertical – 5, 13, 7, 14, 6, 8,
  from the back downwards: top of the head, top of the eye, centre of
  the eye, bottom of the eye, bottom of the head, body underside. Two
  things are tested and they are not the same statement: that **5 tops
  the group** (the `Hd` analogue of the 3/4 rule for `Bd`), and that
  **every consecutive pair is in order**, which catches a local swap the
  first test cannot see.
- This is the failure no other check catches, because each pair stays
  internally consistent: with 13 and 14 exchanged – the eye clicked
  bottom-first – `Ed` (13-14) keeps its exact length while `Eh` (7-8)
  silently refers to the wrong edge of the eye. Nothing in a coordinate
  table shows it.
- Settled on the data, as the `Bd` rule was. Over the 4,151 species
  already digitized in the workbook, the expected order holds for **99.5
  %** of them: 8 above 6 in 22 specimens (0.53 %), 13 above 5 in 10
  (0.24 %, the same ten as “5 does not top the group”), 6 above 14 in 2,
  14 above 7 in 1. A convention that a hand-digitized corpus already
  satisfies to that degree is a convention, not a preference, and the
  residue is worth looking at one specimen at a time.
- An inversion is reported but **never corrected automatically**: moving
  a point to satisfy the order would invent a measurement rather than
  repair one. The dialog therefore offers *Measure again* (which selects
  and zooms on the point found on the wrong side, not the reference it
  was compared with) and *Save without correcting*; the *Correct
  automatically* button only appears when there is an extreme-point
  violation, which is the only kind that can be repaired by moving a
  landmark.
- Same tolerance as the extremes, `max(5 px, 0.003 * Bl)`, and the same
  invariance: heights are read perpendicular to the body axis and the
  dorsal side from the relative position of 3 and 4, so the test holds
  head left or right, photograph flipped, or mirrored. Missing points
  are stepped over rather than breaking the chain.
- New internals `.fm_eye_order_violations()` and
  `.fm_convention_violations()`; the violation tables gain a `kind`
  column (`"extreme"` / `"order"`).

### A tabbed, themed digitizer

- The side panel of
  [`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)
  is a **tabset** — `Specimen`, `Display`, `Checks`, `Seed` — instead of
  one long scroll. The controls fall into groups touched at different
  rhythms (once per specimen, once per photograph, once per session),
  and stacking them in one column put the ones used constantly below the
  ones used never.
- With `bslib` (already in `Suggests`) the page uses a Bootstrap 5
  theme, cards and a wider sidebar; without it, the same content falls
  back to the standard Shiny layout. No feature depends on `bslib`, only
  the appearance does. The page is deliberately **not** `fillable`: it
  is a document that scrolls, and in a filling page the 620 px
  photograph is squeezed by the bars above and the panels below.
- The **queue selector** (`To reconstruct` / `Correct existing` /
  `New photographs`) moves from the action bar to the head of the side
  panel: it decides what the whole session is doing, and it sat one
  button away from “Save & next”. `Mark NA` moves the other way, next to
  the landmark bar, with the zoom controls: those act on the point under
  the cursor.
- The **landmark bar is bare and on one line** — the buttons share the
  width rather than wrapping, so a given point keeps its place on screen
  whatever the window size. The entry order, the broken-axis conventions
  and the colour code move to a card at the foot of the page: they are
  read on the first specimen and never again, but above the photograph
  they cost three lines of scroll on each of the following thousands.
- A header strip shows what the session IS — workbook, photographs,
  journal, operator — since those are arguments of the launcher and are
  not editable from the app. The `Checks` tab shows the state of both
  write layers.

### Every point is written, and a lost one is now loud

- Verified end to end: `save_pts = 1..19, 22, 23, 24, 25` for the
  landmark sheet (and `+ 20, 21` for new specimens), the columns
  `24_X`..`25_Y` being created at start-up by `ensure_cols()`. Over the
  201 records of the existing journals, every record carries its 23
  points: 22 `placed` 201/201, 23 `derived` 165 (36 `na`, the derivation
  needing 6 and 9), 24 `placed` 196, 25 `placed` 9 — the hinges being
  optional by design.
- The on-screen legend claimed **“24/25 are NOT recorded”**, which had
  been untrue since the hinge columns were added. An operator reading it
  had every reason not to bother placing them. Corrected, and the legend
  now states what is written and why: the hinges are not landmarks and
  belong in no shape analysis, but they define the frames each
  convention was applied in — without them a species reopened for
  correction comes back with a straight axis.
- Writing a point whose column is missing from the sheet used to
  [`next`](https://rdrr.io/r/base/Control.html) in silence. It now
  raises a persistent error naming the points, since a hand-edited sheet
  is the one case where a placed point could vanish without trace (the
  journal, as always, still has it).

## Rfishmorph 0.3.0

### Extreme-point convention checked on save

- [`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md)
  now verifies, when “Save & next” is pressed, that landmark 3 is the
  most **dorsal** and landmark 4 the most **ventral** point of the body
  outline — the definition of `Bd` as the maximum body depth. A specimen
  whose 5 (head top) sits above 3, or whose 6 (head bottom) sits below
  4, silently under-estimates `Bd`; this is now caught before anything
  reaches the journal or the workbook.
- On violation a dialog offers three routes: **remeasure** (the
  offending point becomes active and the view centres on it),
  **auto-correct** (3, resp. 4, takes the height of the point
  overshooting it while keeping its position along the axis, so `Bd`
  grows and the 3-4 perpendicularity convention is preserved), or **save
  as is**.
- Heights are measured perpendicular to the body axis 1-2, so a tilted
  photograph does not bias the test, and the dorsal side is inferred
  from the relative position of 3 and 4 — the check therefore holds
  whatever the orientation (head left or right, flipped photograph,
  “Flip dorsal/ventral” ticked). Caudal peduncle and fin (16-19) and
  appendage tips (12 pectoral, 15 jaw) are excluded, as are the scale
  bar (20, 21), the derived point (23) and the hinges (24, 25).
  Tolerance: 0.003 of body length.
- The **derived ventral points 8, 9 and 11 are excluded too**: they are
  computed from landmark 4 (belly line), so testing whether 4 is the
  lowest point against them is circular. Settled on the data rather than
  by argument – over the 1,036 digitized T-26 specimens of the intraitR
  corpus, including 8/9/11 flags 20.6% of the batch (198 of 213 flags
  are those three points, median overshoot 0.5% of `Bl`, i.e. belly-line
  noise), whereas excluding them flags 1.5% at a median overshoot of
  6.8% of `Bl`, with a flag rate flat from 0.003 to 0.02 `Bl`. The
  comparison set is therefore 1, 2, 5, 6, 7, 10, 13, 14, 22 – the
  landmarks that are independent measurements on the body outline.
- Tolerance is `max(5 px, 0.003 * Bl)`. The 5 px absolute floor matters
  on small photographs, where the relative term falls below click noise.
  It is free: compliant T-26 specimens top out at -0.4 px of overshoot
  (p98) while the smallest real breach is 11.8 px, so any floor from 1
  to 8 px flags the same 16 specimens.
- The check is toggled by the new “Check 3/4 (extremes) on save” box (on
  by default).
- New journal status **`"adjusted"`** for points relocated by that
  automatic correction, distinct from `"placed"` (operator-pointed) and
  `"seeded"` (never verified).
  [`fishmorph_journal_qc()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_journal_qc.md)
  reports them, so an auto-corrected `Bd` remains traceable specimen by
  specimen.

## Rfishmorph 0.2.0

### Package renamed

- `FishMORPHR` is now **`Rfishmorph`**. Update
  [`library()`](https://rdrr.io/r/base/library.html) calls accordingly.

### Digitizing application

- New
  [`launch_fishmorph_digitizer()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_digitizer.md):
  works directly on the FISHMORPH workbook with three switchable queues
  — `"reconstruct"` (species without landmarks), `"correct"` (already
  landmarked, reloaded from the workbook) and `"new"` (photographs
  absent from the workbook, appended to a `new_specimens` sheet). Adds a
  broken body axis with hinge points 22/24/25 for curved specimens, an
  optional 20/21 scale bar giving `mm_per_px`, and constrained editing
  that enforces the FISHMORPH geometric conventions live.
- In `"new"` mode there are no measured segments: points are seeded from
  the **median segment/Bl proportions of the 9556 reference species**,
  then corrected by hand. No length is locked in that mode.
- [`launch_fishmorph_reconstructor()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_reconstructor.md)
  is **deprecated** and redirects to the new tool. The old single-photo
  prototype was removed: two implementations of the same geometry were
  bound to diverge.

### Data layer: append-only journal

- New
  [`fm_journal_open()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_open.md),
  [`fm_journal_append()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_append.md),
  [`fm_journal_read()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_read.md),
  [`fm_journal_status()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_status.md),
  [`fm_journal_history()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_history.md),
  [`fishmorph_consolidate()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_consolidate.md),
  [`fishmorph_journal_qc()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_journal_qc.md).
- Every save is appended to a per-session TSV in long format (one row
  per point) before anything is written to the workbook. A crash can at
  worst truncate the last line, which is detected and discarded on read.
- Each point records a `status`: `placed`, `seeded` (never verified by
  the operator), `derived`, `na`. This distinction is invisible in a
  wide coordinate table and is what makes quality control possible.
- Workbook writes are now **atomic**
  ([`fm_save_workbook_atomic()`](https://funtraits.github.io/Rfishmorph/reference/fm_save_workbook_atomic.md):
  temporary file then rename, previous generation kept as `.prev.xlsx`)
  and batched via `xlsx_flush_every`, taking the multi-megabyte rewrite
  out of the input loop.

### Data layer: derived DuckDB database

- New
  [`fishmorph_build_db()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_build_db.md),
  [`fishmorph_db_connect()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_db_connect.md),
  [`fishmorph_validate()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_validate.md).
- Three constrained tables (`record`, `specimen`, `landmark_obs`) and
  three views (`v_landmarks_wide`, `v_ratios`, `v_specimen_qc`), plus
  Parquet and CSV exports for archiving.
- [`fishmorph_validate()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_validate.md)
  adds morphometric plausibility on top of SQL constraints, comparing
  each specimen to the empirical envelope (quantiles 0.001 and 0.999) of
  the 9556 reference species.

### Exploration application

- New
  [`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md):
  the former stand-alone *FishMorphSpace* app, now shipped in
  `inst/shiny/fishmorph_space/`, with the 9556-species trait table
  embedded in `inst/extdata/fishmorph_data.csv`
  ([`fishmorph_space_data()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_space_data.md)).
  A `data =` argument points it at a more recent table.

### Phylogenetic imputation

- New
  [`load_fishmorph_phylo_axes()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylo_axes.md):
  reads `inst/extdata/Phylogeny/ pcoaPhylogenyFish.rds`, the
  **precomputed** PCoA axes of the global fish phylogeny (8,970 species,
  10 axes), cached once per session. Shipped as a compressed `.rds` (540
  kB instead of 1.8 MB of text); the loader still accepts the
  whitespace-separated text format, dispatching on the file extension.
- `"missforest_phylo"` now uses that table by default instead of
  eigendecomposing the patristic distance matrix on every call. Beyond
  the cost, this fixes a **comparability** problem: axes recomputed on
  whichever species happened to be present defined a different
  coordinate system for each analysis, so two imputations on two subsets
  did not live in the same phylogenetic space.
- [`impute_traits()`](https://funtraits.github.io/Rfishmorph/reference/impute_traits.md),
  [`impute_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/impute_landmarks.md)
  and
  [`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
  gain a `phylo_axes` argument to supply an alternative table. Passing
  `tree` still recomputes from that tree, as before; the bundled tree
  remains the fallback if the table is unreachable.
- The imputation message now names the **source** of the axes, so two
  runs can be told apart.
- **`species` is now a separate argument** from `groups` in
  [`impute_traits()`](https://funtraits.github.io/Rfishmorph/reference/impute_traits.md),
  [`impute_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/impute_landmarks.md),
  [`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md)
  and
  [`compare_segments_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/compare_segments_landmarks.md).
  The two were conflated, so `"missforest_phylo"` refused to work
  without a `groups` vector – yet the phylogeny only needs to know which
  species each row belongs to, not a categorical predictor for the
  forest. `species` is auto-detected from a `Genus.species` / `Species`
  / `species` column (or, for landmarks, from the metadata or the
  specimen names); `groups` is no longer auto-filled with species. In
  [`compare_segments_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/compare_segments_landmarks.md)
  the key is taken from `id_col`, not from `group_col`.
- A `groups` factor with more than 53 levels is now dropped from the
  missForest predictors with a warning instead of failing:
  `randomForest` cannot handle more than 53 categories, so auto-filling
  `groups` with several thousand species names would have made the
  imputation error out.

### Bug fixes

- [`correct_geometry_conventions()`](https://funtraits.github.io/Rfishmorph/reference/correct_geometry_conventions.md)
  failed with
  `'vec' must be sorted non-decreasingly and not contain NAs` when a
  hinge landmark (22, 24 or 25) projected outside the snout-to-caudal
  segment. Such a hinge is now discarded from the axis polyline, with an
  aggregated warning naming the specimens.

### Dependencies

- All application and database packages are in `Suggests`; each launcher
  checks its own dependencies and prints a ready-to-paste
  [`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
  call.

### Earlier in this cycle

- [`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md)
  is now a faithful port of `intraitR::project_fishmorph()` (specimens
  projected into the frozen FISHMORPH space built from a reference
  database), with [`print()`](https://rdrr.io/r/base/print.html) and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) methods.
  `plot(proj, style = "hull")` reproduces the intraitR figure: reference
  kernel-density heatmap + per-species convex hulls / spider / density,
  species legend, optional loading arrows and `itv_reference` points.
  Added
  [`group_colors()`](https://funtraits.github.io/Rfishmorph/reference/group_colors.md)
  /
  [`reset_group_colors()`](https://funtraits.github.io/Rfishmorph/reference/reset_group_colors.md).
- Imputation aligned with intraitR:
  [`impute_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/impute_landmarks.md),
  [`phylo_pcoa()`](https://funtraits.github.io/Rfishmorph/reference/phylo_pcoa.md),
  [`load_fishmorph_phylogeny()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_phylogeny.md),
  and `fishmorph_trait_space(na_action = ...)` including
  `"missforest_phylo"`.

## Rfishmorph 0.1.0

First release. FISHMORPH routines split out of `intraitR` into a
stand-alone package.

- Schema and core measurements:
  [`fishmorph_schema()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_schema.md),
  [`fishmorph_segments()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_segments.md),
  [`fishmorph_ratios()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_ratios.md),
  [`fishmorph_traits()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_traits.md).
- Landmark container and I/O interoperable with `intrait_landmarks`:
  [`fishmorph_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_landmarks.md),
  [`read_landmarks_csv()`](https://funtraits.github.io/Rfishmorph/reference/read_landmarks_csv.md),
  [`write_landmarks_csv()`](https://funtraits.github.io/Rfishmorph/reference/write_landmarks_csv.md).
- Geometry:
  [`standardize_geometry()`](https://funtraits.github.io/Rfishmorph/reference/standardize_geometry.md),
  [`correct_geometry_conventions()`](https://funtraits.github.io/Rfishmorph/reference/correct_geometry_conventions.md).
- Reconstruction (inverse of the segments) with an exact round-trip
  guarantee:
  [`reconstruct_fishmorph_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/reconstruct_fishmorph_landmarks.md),
  [`check_reconstruction_roundtrip()`](https://funtraits.github.io/Rfishmorph/reference/check_reconstruction_roundtrip.md),
  [`launch_fishmorph_reconstructor()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_reconstructor.md).
- Functional trait space (frozen PCA + projection):
  [`fishmorph_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_trait_space.md),
  [`project_fishmorph()`](https://funtraits.github.io/Rfishmorph/reference/project_fishmorph.md),
  [`load_fishmorph_reference()`](https://funtraits.github.io/Rfishmorph/reference/load_fishmorph_reference.md).
- Visualization (ggplot2 + base fallback):
  [`plot_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/plot_landmarks.md),
  [`plot_trait_space()`](https://funtraits.github.io/Rfishmorph/reference/plot_trait_space.md),
  [`plot_trait_distributions()`](https://funtraits.github.io/Rfishmorph/reference/plot_trait_distributions.md),
  [`plot_segment_landmark_agreement()`](https://funtraits.github.io/Rfishmorph/reference/plot_segment_landmark_agreement.md).
- Quality control:
  [`compare_segments_landmarks()`](https://funtraits.github.io/Rfishmorph/reference/compare_segments_landmarks.md),
  [`agreement_metrics()`](https://funtraits.github.io/Rfishmorph/reference/agreement_metrics.md),
  [`check_infinite_ratios()`](https://funtraits.github.io/Rfishmorph/reference/check_infinite_ratios.md),
  [`check_geometry_conventions()`](https://funtraits.github.io/Rfishmorph/reference/check_geometry_conventions.md).
- Species management via `rfishbase`:
  [`validate_species_names()`](https://funtraits.github.io/Rfishmorph/reference/validate_species_names.md),
  [`update_fishmorph_taxonomy()`](https://funtraits.github.io/Rfishmorph/reference/update_fishmorph_taxonomy.md),
  [`freshwater_fish_list()`](https://funtraits.github.io/Rfishmorph/reference/freshwater_fish_list.md),
  [`new_fishmorph_species()`](https://funtraits.github.io/Rfishmorph/reference/new_fishmorph_species.md),
  [`add_fishmorph_species()`](https://funtraits.github.io/Rfishmorph/reference/add_fishmorph_species.md).
