# Rfishmorph

<!-- badges: start -->
[![R-CMD-check](https://github.com/FunTraits/Rfishmorph/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FunTraits/Rfishmorph/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/FunTraits/Rfishmorph/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/FunTraits/Rfishmorph/actions/workflows/test-coverage.yaml)
[![Codecov test coverage](https://codecov.io/gh/FunTraits/Rfishmorph/branch/main/graph/badge.svg)](https://codecov.io/gh/FunTraits/Rfishmorph)
[![pkgdown](https://github.com/FunTraits/Rfishmorph/actions/workflows/pkgdown.yaml/badge.svg)](https://funtraits.github.io/Rfishmorph/)
<!-- badges: end -->

**Morphological traits, landmarks and quality control for the FISHMORPH
database.**

Rfishmorph collects the FISHMORPH routines originally developed inside
`intraitR` into a stand-alone package dedicated to the FISHMORPH
morphological database of freshwater fishes (Brosse et al., 2021). It
reconstructs the 21 FISHMORPH landmarks, computes the 11 body segments
and the 9 dimensionless ratios, standardizes landmark geometry and
enforces the FISHMORPH digitizing conventions, projects specimens into
the FISHMORPH functional trait space, and provides visualization and
quality-control tools. It also offers a guided workflow to add new
species, using `rfishbase` to validate taxonomy and build a global
freshwater fish list.

## Installation

``` r
# install.packages("devtools")
remotes::install_github("FunTraits/Rfishmorph")
```

The core functions depend only on base R. Optional features use
`ggplot2` (plots), `rfishbase` (taxonomy / freshwater list),
`readxl`/`writexl` (Excel I/O) and `shiny`/`jpeg`/`png` (interactive
reconstruction). After editing the roxygen comments, regenerate the help
pages with `devtools::document()`.

## The FISHMORPH scheme

|  |  |  |
|------------------------|------------------------|------------------------|
| **21 landmarks** | anatomical points digitized on a lateral photo | `fishmorph_schema()$landmark_labels` |
| **11 segments** | `Bl, Bd, Hd, Eh, Mo, PFi, PFl, Ed, Jl, CPd, CFd` | `fishmorph_segment_names()` |
| **9 ratios** | `BEl, VEp, REs, OGp, RMl, BLs, PFv, PFs, CPt` | `fishmorph_ratio_names()` |

Ratios are dimensionless (each is a quotient of two segments of the same
specimen), so they are scale-invariant and comparable across studies.

## Typical workflows

### 1. Measure a specimen

``` r
library(Rfishmorph)

lm  <- read_landmarks_csv(
  system.file("extdata", "example_landmarks.csv", package = "Rfishmorph"))
seg <- fishmorph_segments(lm, scale_cm = 1)   # 11 segments (cm)
rat <- fishmorph_ratios(seg)                  # 9 ratios
plot_landmarks(lm, specimen = 1)              # ggplot of points + segments
```

### 2. Functional trait space

``` r
ref <- load_fishmorph_reference()             # bundled 400-species sample
ts  <- fishmorph_trait_space(ref, groups = ref$Order)   # frozen PCA
plot_trait_space(ts,style = "none",arrows = T)                          # PCA with hulls + loadings
```

Project focal specimens into the global FISHMORPH morphospace and draw
the reference density heatmap with per-species footprints (the intraitR
figure):

``` r
# `specimens` = focal individuals from fishmorph_ratios(); reference = database
proj <- project_fishmorph(specimens, reference = ref)
plot(proj, style = "hull")                    # density heatmap + species hulls
plot(proj, style = "hull", arrows = TRUE)     # add trait loading arrows
plot(proj, style = "spider", itv_reference = TRUE)
```

### 3. Quality control: segments vs landmarks

``` r
# does a workbook's published morphology match its digitized landmarks?
cmp <- compare_segments_landmarks(landmarks = lm, published = published_segments,
                                  id_col = "Genus.species")
summary(cmp)
plot_segment_landmark_agreement(cmp, type = "scatter")
plot_segment_landmark_agreement(cmp, type = "bar")

check_infinite_ratios(rat)          # zero-denominator / outlier ratios
check_geometry_conventions(lm)      # departure from the 5 digitizing rules
```

### 4. Add a new species

``` r
# resolve the name against FishBase (needs rfishbase)
validate_species_names(c("Salmo trutta", "Esox_lucius"))

# build a schema-compliant record, then append it to the reference
rec <- new_fishmorph_species(
  "Genus novus",
  segments = list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6, PFi = 1.9,
                  PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1.0, CFd = 3.0),
  family = "Cyprinidae", order = "Cypriniformes")
ref2 <- add_fishmorph_species(ref, rec)

# a global list of freshwater fishes (needs rfishbase)
fw <- freshwater_fish_list()
```

### 5. Explore the space (digitizing lives in intraitR)

Since 0.18.0 Rfishmorph only *visualises* FISHMORPH. Digitizing
photographs, the append-only landmark journal and the DuckDB database
moved to **intraitR**, where a single digitizer serves both FISHMORPH and
intraspecific campaigns. The old entry points (`launch_fishmorph_digitizer()`,
`fm_journal_*()`, `fishmorph_consolidate()`, `fishmorph_build_db()`, ...)
are defunct stubs that name their replacement (`?Rfishmorph-defunct`).

``` r
# --- digitize, consolidate, build the database: intraitR ------------------
intraitR::digitize_fishmorph_reference(
  xlsx_path = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
  operator  = "AT", mode = "new")
jdir <- "FishMORPH/landmark_journal"
intraitR::landmark_journal_status(jdir)
base <- intraitR::consolidate_landmarks(jdir)
res  <- intraitR::fishmorph_build_db(jdir, db_path = "FishMORPH/fishmorph.duckdb")

# --- explore: Rfishmorph --------------------------------------------------
launch_fishmorph_space()                      # embedded 9556-species dataset
find_fishmorph_species(c("Salmo trutta", "Gobio sp."))  # is it in FISHMORPH?
```

## Data architecture

Digitizing never overwrites what is already written:

```         
photographs --> append-only journal --> DuckDB database --> Parquet / CSV
(local only)    (source of truth,       (constraints,       (citable archive)
                 one TSV per session)    views, SQL)
```

The journal is the only durable layer. The database and the workbook are
**derived artefacts**: `intraitR::fishmorph_build_db()` rebuilds them from scratch
in seconds, which is what makes it safe to keep them in a synchronised
folder. Never write into the database by hand — the next rebuild will
erase it.

Each point carries a `status` that a wide coordinate table cannot
express: `clicked`, `seeded` (**still at its seed position,
never checked**), `derived` (computed: 8, 9, 11, 15, 23), `na` (declared
unmeasurable). In `reconstruct` mode a `seeded` point still sits exactly
where the input segments put it, so validating its ratio against the
FISHMORPH envelope is circular — filter on `status` before any
validation analysis.

## Round-trip guarantee

`reconstruct_fishmorph_landmarks()` is the inverse of
`fishmorph_segments()`: the free placement parameters change only
*where* landmarks sit, never the segment *lengths*.
`check_reconstruction_roundtrip()` verifies that
`segments -> landmarks -> segments` returns the input to machine
precision.

## Reference

Brosse, S., Charpin, N., Su, G., Toussaint, A., Herrera-R, G.A.,
Tedesco, P.A. & Villeger, S. (2021). FISHMORPH: A global database on
morphological traits of freshwater fishes. *Global Ecology and
Biogeography*, 30, 2330–2336. <https://doi.org/10.1111/geb.13395>

## Note on provenance

The FISHMORPH functions here mirror the `intraitR` implementations
(`fishmorph_segments()`, `fishmorph_ratios()`, `standardize_geometry()`,
`correct_geometry_conventions()`, `project_fishmorph()` /
`trait_space()`), so the two packages remain interoperable while
FISHMORPH support migrates out of `intraitR`. Objects of class
`intrait_landmarks` are accepted throughout. \# Rfishmorph
