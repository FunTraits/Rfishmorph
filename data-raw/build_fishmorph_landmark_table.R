# =============================================================================
# data-raw/build_fishmorph_landmark_table.R
#
# Regenerates inst/extdata/fishmorph_data_landmarks.csv from the two landmark
# stores. Re-run this whenever the re-digitization has progressed: the shipped
# CSV is a dated snapshot of an ongoing campaign, not a frozen data set.
#
#   source("data-raw/build_fishmorph_landmark_table.R")
#
# Requires: readxl, DBI, duckdb, missForest, ape (for the phylogenetic axes).
# =============================================================================

devtools::load_all(".")

## Adjust if the working copy of the database lives elsewhere.
FM_ROOT <- "~/Library/CloudStorage/OneDrive-Personnel/iCloud Drive/00_Papier_5_EnProjet/Packages/00_Scripts/FishMORPH/FishMORPH"

xlsx <- file.path(FM_ROOT, "FISHMORPH_PUBLI_9556sp_reconstructed.xlsx")
db   <- file.path(FM_ROOT, "fishmorph.duckdb")
out  <- file.path("inst", "extdata", "fishmorph_data_landmarks.csv")

stopifnot(file.exists(xlsx))
if (!file.exists(db)) {
  message("DuckDB store not found, building from the workbook only: ", db)
  db <- NULL
}

set.seed(20260728)   # missForest is stochastic; pin it so the CSV is reproducible

## `metadata` supplies taxonomy / IUCN / MBl / MBw only -- the columns no
## landmark can yield. Left NULL it reads them from the segment table, which is
## simply where they live; nothing morphometric crosses over.
tab <- build_fishmorph_landmark_table(
  xlsx        = xlsx,
  db          = db,
  metadata    = NULL,
  na_action   = "missforest_phylo",
  log         = TRUE,
  file        = out,
  verbose     = TRUE)

## ---- sanity checks, run every time ----------------------------------------
## They are cheap and they catch the two failure modes that would silently
## corrupt every downstream analysis: a scale mismatch with the segment table,
## and ratios drifting outside their admissible range.
seg <- load_fishmorph_reference(source = "segment", quiet = TRUE)
rat <- fishmorph_ratio_names()

stopifnot(all(rat %in% names(tab)))
stopifnot(identical(names(seg)[seq_len(4)], names(tab)[seq_len(4)]))

cat("\n-- Range comparison (log10(x + 1) scale) ------------------------------\n")
print(data.frame(
  trait       = rat,
  seg_min     = round(vapply(seg[rat], min, 0, na.rm = TRUE), 3),
  seg_max     = round(vapply(seg[rat], max, 0, na.rm = TRUE), 3),
  lmk_min     = round(vapply(tab[rat], min, 0, na.rm = TRUE), 3),
  lmk_max     = round(vapply(tab[rat], max, 0, na.rm = TRUE), 3),
  row.names   = NULL))

## Correlation with the segment table on the shared species. Expected to be
## high for the size ratios (they are largely inherited from the segment
## lengths through the reconstruction) and clearly lower for the position
## ratios OGp / VEp / PFv, which is where landmark digitizing adds information.
key <- function(x) gsub("[ ._]+", "_", trimws(x))
m <- match(key(tab$Species), key(seg$Species))
shared <- !is.na(m)
cat(sprintf("\n-- Segment/landmark correlation on %d shared species -----------\n",
            sum(shared)))
print(round(vapply(rat, function(r)
  stats::cor(tab[[r]][shared], seg[[r]][m[shared]],
             use = "complete.obs"), numeric(1)), 3))

cat("\nDone. Snapshot written to ", out, "\n", sep = "")
