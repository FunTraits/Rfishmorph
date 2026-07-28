# =============================================================================
# landmark-table.R -- build the landmark-derived FISHMORPH trait table.
#
# The historical FISHMORPH table (inst/extdata/fishmorph_data.csv) stores the
# nine ratios as they were computed from the ELEVEN SEGMENTS measured on the
# published plates. Since 2026 the whole database is being re-measured as
# LANDMARK configurations, so a second, geometrically homogeneous trait table
# can be derived: landmarks -> fishmorph_segments() -> fishmorph_ratios().
#
# This file builds that second table. It is deliberately a *builder*, not a
# frozen data file: the re-measurement is ongoing, so the CSV shipped under
# inst/extdata/fishmorph_data_landmarks.csv is a snapshot that is meant to be
# regenerated as digitizing progresses.
#
# Both trait tables share exactly the same column names, separator (";") and
# scale (log10(x + 1)), so they are drop-in substitutes for one another
# everywhere `load_fishmorph_reference()` is used -- see `source=` there.
# =============================================================================

#' Build the landmark-derived FISHMORPH trait table
#'
#' Recomputes the nine FISHMORPH ratios from landmark configurations instead of
#' reading them from the published segment measurements, and assembles a table
#' with the same layout as `fishmorph_data.csv` so that the two can be swapped
#' with [load_fishmorph_reference()]'s `source` argument.
#'
#' @section Independence from the segment table:
#' The two campaigns are separate measurements and are kept separate here.
#' Segments, ratios, imputation and, downstream, the ordination are derived from
#' the landmark geometry alone: at no point is a segment measurement read,
#' copied or used as a prior. The segment table is touched once, at the very
#' end, and only to attach columns that no landmark configuration could produce
#' -- taxonomy, IUCN status, and the FishBase maximum body length and width.
#' Those are species attributes rather than measurements on the plate, so they
#' are the same whichever campaign is read; the segment CSV is only the file
#' that stores them. Supply `metadata` to take them from anywhere else.
#'
#' @section Why a second table:
#' Segment measurements fix the eleven segment *lengths* but say nothing about
#' where the depth segments sit along the body axis, nor how a depth is split
#' between the dorsal and ventral sides. Landmark digitizing supplies exactly
#' that missing information, so the position ratios (`OGp`, `VEp`, `PFv`) are
#' the ones that genuinely change; the size ratios are expected to stay close to
#' their segment values. Do not read a high segment/landmark correlation as
#' validation: for several traits it is a property of the construction.
#'
#' @section Coverage:
#' The table contains **only the species that have actually been digitized**, so
#' it is shorter than `fishmorph_data.csv` (8,970 species) and grows with the
#' re-measurement campaign. Every function that builds a trait space on it
#' therefore describes a smaller species pool; `load_fishmorph_reference()`
#' reports the row count so this can never happen silently.
#'
#' @section Missing ratios:
#' A ratio is `NA` when one of its two segments could not be measured, whether
#' because the structure is absent (no pectoral fin, no caudal fin, terminal
#' mouth) or because the specimen is only partly digitized. `na_action` decides
#' what happens then; the default `"missforest_phylo"` imputes on the
#' **raw** scale using the precomputed phylogenetic PCoA axes
#' ([load_fishmorph_phylo_axes()]), i.e. the same coordinate system as every
#' other imputation in the package. The per-species `n_imputed` column records
#' how many of the nine ratios were filled in, so imputed species can always be
#' excluded downstream.
#'
#' @param xlsx Path to the FISHMORPH publication workbook holding the landmark
#'   sheet (`FISHMORPH_PUBLI_9556sp_reconstructed.xlsx`). `NULL` skips it.
#' @param db Path to the digitizer DuckDB store (`fishmorph.duckdb`), read
#'   through its `v_landmarks_wide` view. `NULL` skips it. When a species is
#'   present in both stores the DuckDB record wins, being the more recent
#'   digitization.
#' @param sheet Landmark sheet name in `xlsx` (default `"Global_Landmark"`).
#' @param metadata Table supplying the **non-morphometric** columns only --
#'   `Species`, `Family`, `Order`, `Genus`, `MBl`, `MBw`, `IUCN` -- joined by
#'   species name. `NULL` (default) reads them from the segment table, which is
#'   simply where they are stored. The join is restricted to that whitelist, so
#'   no ratio can cross over even if the supplied table carries some. This
#'   argument is deliberately *not* called `reference`: it does not define the
#'   trait space and plays no part in the segments, ratios or imputation.
#' @param na_action How to fill missing ratios: `"missforest_phylo"` (default),
#'   `"missforest"`, `"impute_mean"`, `"impute_group_mean"`, `"omit"` or
#'   `"keep"` (leave `NA`).
#' @param log Return the ratios on the `log10(x + 1)` scale (default `TRUE`),
#'   matching `fishmorph_data.csv`. Imputation always runs on the raw scale.
#' @param file Optional path to write the table as a `;`-separated CSV.
#' @param missforest_ntree,missforest_maxiter,tree,missforest_phylo_k,phylo_axes
#'   Passed through to the imputation, see [fishmorph_trait_space()].
#' @param verbose Report coverage and imputation counts (default `TRUE`).
#' @return A data frame with `Species, Family, Order, Genus`, the nine ratios,
#'   `MBl`, `MBw`, `IUCN`, plus the provenance columns `store` (`"xlsx"` or
#'   `"duckdb"`) and `n_imputed`.
#' @seealso [load_fishmorph_reference()] and its `source` argument,
#'   [fishmorph_segments()], [fishmorph_ratios()]
#' @examples
#' \dontrun{
#' tab <- build_fishmorph_landmark_table(
#'   xlsx = "FishMORPH/FISHMORPH_PUBLI_9556sp_reconstructed.xlsx",
#'   db   = "FishMORPH/fishmorph.duckdb",
#'   file = "inst/extdata/fishmorph_data_landmarks.csv")
#' }
#' @export
build_fishmorph_landmark_table <- function(xlsx = NULL, db = NULL,
                                           sheet = "Global_Landmark",
                                           metadata = NULL,
                                           na_action = c("missforest_phylo",
                                                         "missforest",
                                                         "impute_mean",
                                                         "impute_group_mean",
                                                         "omit", "keep"),
                                           log = TRUE,
                                           file = NULL,
                                           missforest_ntree = 100,
                                           missforest_maxiter = 10,
                                           tree = NULL,
                                           missforest_phylo_k = 10,
                                           phylo_axes = NULL,
                                           verbose = TRUE) {
  na_action <- match.arg(na_action)
  if (is.null(xlsx) && is.null(db))
    stop("Supply at least one landmark store: `xlsx` and/or `db`.", call. = FALSE)

  ## -- 1. read the landmark stores -----------------------------------------
  stores <- list()
  if (!is.null(xlsx)) stores$xlsx   <- .fm_lm_from_xlsx(xlsx, sheet)
  if (!is.null(db))   stores$duckdb <- .fm_lm_from_duckdb(db)

  wide <- do.call(rbind, lapply(names(stores), function(s) {
    d <- stores[[s]]
    if (is.null(d) || !nrow(d)) return(NULL)
    d$store <- s
    d
  }))
  if (is.null(wide) || !nrow(wide))
    stop("No digitized specimen found in the supplied store(s).", call. = FALSE)

  ## De-duplicate on the canonical species key. The DuckDB store is the live
  ## digitizer output, so it supersedes the workbook when both hold a species.
  wide$.key <- .canon_species_name(wide$Genus.species)
  ord <- order(match(wide$store, c("duckdb", "xlsx")))
  wide <- wide[ord, , drop = FALSE]
  dup <- duplicated(wide$.key)
  if (verbose && any(dup))
    message(sprintf("%d species present in both stores; keeping the DuckDB record.",
                    sum(dup)))
  wide <- wide[!dup, , drop = FALSE]
  wide <- wide[order(wide$.key), , drop = FALSE]

  ## -- 2. landmarks -> segments -> ratios ----------------------------------
  ## `na.rm = FALSE` is essential here: the default drops partly-digitized
  ## specimens, which would silently desynchronise the ratio rows from `wide`
  ## and quietly shrink the table. Rows are realigned by specimen instead, and
  ## species whose body length itself is unusable are dropped explicitly.
  lm  <- .fm_lm_wide_to_object(wide)
  seg <- fishmorph_segments(lm, na.rm = FALSE)
  rat <- fishmorph_ratios(seg)
  rat <- rat[match(wide$.key, rat$specimen), , drop = FALSE]

  usable <- is.finite(seg$Bl[match(wide$.key, seg$specimen)]) &
    seg$Bl[match(wide$.key, seg$specimen)] > 0
  if (verbose && any(!usable))
    message(sprintf("%d species dropped: body length (landmarks 1-2) missing or zero.",
                    sum(!usable)))
  wide <- wide[usable, , drop = FALSE]
  rat  <- rat[usable, , drop = FALSE]

  X <- data.matrix(rat[.FM_RATIOS])
  rownames(X) <- wide$.key
  n_missing_before <- rowSums(is.na(X))

  if (verbose) {
    per_trait <- colSums(is.na(X))
    message(sprintf("Ratios computed for %d species; missing cells per trait: %s",
                    nrow(X),
                    paste(sprintf("%s=%d", names(per_trait), per_trait),
                          collapse = ", ")))
  }

  ## -- 3. imputation, on the RAW scale -------------------------------------
  keep <- rep(TRUE, nrow(X))
  if (na_action != "keep" && anyNA(X)) {
    res <- .apply_na_action(X, groups = NULL, na_action = na_action,
                            missforest_ntree = missforest_ntree,
                            missforest_maxiter = missforest_maxiter,
                            context = "landmark ratios",
                            tree = tree,
                            missforest_phylo_k = missforest_phylo_k,
                            phylo_axes = phylo_axes,
                            species = wide$.key)
    X <- res$X
    keep <- res$keep
  }
  wide <- wide[keep, , drop = FALSE]
  n_imputed <- n_missing_before[keep]
  if (na_action %in% c("keep", "omit")) n_imputed[] <- 0L

  ## -- 4. the FISHMORPH scale ----------------------------------------------
  if (log) X <- log10(X + 1)

  ## -- 5. NON-MORPHOMETRIC metadata, joined by species name ----------------
  ## Everything above -- segments, ratios, imputation, scale -- is derived from
  ## the landmark geometry alone and has not seen the segment table. This step
  ## adds only the columns no landmark configuration could ever yield:
  ## taxonomy, IUCN status, and the FishBase maximum body length and width.
  ## Those are species attributes, not measurements taken on the plate: they
  ## are identical whichever campaign is read, and the segment CSV is merely
  ## the file that happens to carry them. Nothing morphometric crosses over --
  ## the join is restricted to the whitelist below, so a stray ratio column in
  ## `metadata` cannot contaminate the table.
  if (is.null(metadata)) metadata <- load_fishmorph_reference(source = "segment",
                                                              quiet = TRUE)
  metadata <- as.data.frame(metadata)
  .FM_META_COLS <- c("Species", "Family", "Order", "Genus", "MBl", "MBw", "IUCN")
  leaked <- intersect(.FM_RATIOS, names(metadata))
  metadata <- metadata[intersect(.FM_META_COLS, names(metadata))]
  if (verbose)
    message(sprintf(
      "Metadata joined by species name: %s%s.",
      paste(names(metadata), collapse = ", "),
      if (length(leaked))
        sprintf(" (%d ratio column(s) present in `metadata` were ignored)",
                length(leaked)) else ""))

  ref_key <- .canon_species_name(metadata[["Species"]])
  m <- match(wide$.key, ref_key)
  if (verbose && anyNA(m))
    message(sprintf(paste("%d digitized species absent from the metadata table:",
                          "taxonomy taken from the landmark store,",
                          "MBl/MBw/IUCN left NA."), sum(is.na(m))))

  take <- function(col, fallback) {
    v <- if (col %in% names(metadata)) metadata[[col]][m] else rep(NA, length(m))
    bad <- is.na(v)
    if (any(bad) && !is.null(fallback)) v[bad] <- fallback[bad]
    v
  }
  species_lbl <- gsub("_", " ", wide$.key)

  out <- data.frame(
    Species = ifelse(is.na(m), species_lbl, metadata[["Species"]][m]),
    Family  = take("Family", wide$Family),
    Order   = take("Order",  wide$Order),
    Genus   = take("Genus",  wide$Genus),
    stringsAsFactors = FALSE)
  for (rn in c("REs", "VEp", "RMl", "OGp", "BEl", "BLs", "PFv", "PFs", "CPt"))
    out[[rn]] <- unname(X[, rn])
  out$MBl   <- take("MBl", NULL)
  out$MBw   <- take("MBw", NULL)
  out$IUCN  <- take("IUCN", NULL)
  out$store <- wide$store
  out$n_imputed <- as.integer(n_imputed)
  rownames(out) <- NULL

  if (verbose)
    message(sprintf(
      "Landmark trait table: %d species, %d fully measured, %d with >=1 imputed ratio (scale: %s).",
      nrow(out), sum(out$n_imputed == 0L), sum(out$n_imputed > 0L),
      if (log) "log10(x + 1)" else "raw"))

  if (!is.null(file)) {
    utils::write.table(out, file, sep = ";", dec = ".", row.names = FALSE,
                       quote = FALSE, na = "", fileEncoding = "UTF-8")
    if (verbose) message("Written: ", file)
  }
  out
}

# ---- store readers ----------------------------------------------------------
# Both stores expose the same wide layout: taxonomy columns plus `<point>_X` /
# `<point>_Y`. They are normalised here to a single data frame carrying
# `Genus.species`, `Family`, `Order`, `Genus` and the coordinate columns.

.fm_lm_wide_cols <- function(nms) grep("^[0-9]+_[XY]$", nms, value = TRUE)

.fm_lm_from_xlsx <- function(path, sheet = "Global_Landmark") {
  .fm_require("readxl", "read the FISHMORPH landmark workbook")
  if (!file.exists(path))
    stop("Landmark workbook not found: ", path, call. = FALSE)
  df <- as.data.frame(readxl::read_excel(path, sheet = sheet,
                                         guess_max = 1048576))
  co <- .fm_lm_wide_cols(names(df))
  if (!length(co))
    stop("Sheet '", sheet, "' holds no `<point>_X` / `<point>_Y` column.",
         call. = FALSE)
  for (cc in co) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  ## A row counts as digitized once the snout tip is placed.
  anchor <- if ("1_X" %in% co) df[["1_X"]] else df[[co[1]]]
  df <- df[!is.na(anchor), , drop = FALSE]
  keep <- intersect(c("Genus.species", "Family", "Order", "Genus"), names(df))
  df[c(keep, co)]
}

.fm_lm_from_duckdb <- function(path, view = "v_landmarks_wide") {
  .fm_require("DBI", "read the digitizer DuckDB store")
  .fm_require("duckdb", "read the digitizer DuckDB store")
  if (!file.exists(path))
    stop("DuckDB store not found: ", path, call. = FALSE)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  df <- DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", view))
  co <- .fm_lm_wide_cols(names(df))
  if (!length(co)) return(NULL)
  ## The digitizer keys on `species`, already in Genus.species form.
  df$Genus.species <- df[["species"]]
  df$Genus <- sub("[._ ].*$", "", df$Genus.species)
  df$Family <- NA_character_
  df$Order  <- NA_character_
  anchor <- if ("1_X" %in% co) df[["1_X"]] else df[[co[1]]]
  df <- df[!is.na(anchor), , drop = FALSE]
  df[c("Genus.species", "Family", "Order", "Genus", co)]
}

# Wide taxonomy+coordinates -> `fishmorph_landmarks`. The publication workbook
# and the digitizer both store image coordinates (Y pointing DOWN); Y is negated
# so the object follows the package's Cartesian convention. Ratios are built
# from euclidean distances and are unaffected either way, but a landmark object
# that plots upside down would be a trap for every other function.
.fm_lm_wide_to_object <- function(wide) {
  co  <- .fm_lm_wide_cols(names(wide))
  pts <- sort(unique(as.integer(sub("_[XY]$", "", co))))
  npt <- max(pts)
  n   <- nrow(wide)
  arr <- array(NA_real_, dim = c(npt, 2, n),
               dimnames = list(NULL, c("X", "Y"), wide$.key))
  for (p in pts) {
    cx <- paste0(p, "_X"); cy <- paste0(p, "_Y")
    if (cx %in% co) arr[p, 1, ] <- wide[[cx]]
    if (cy %in% co) arr[p, 2, ] <- -wide[[cy]]
  }
  structure(list(coords = arr, scale = NULL,
                 metadata = data.frame(specimen = wide$.key,
                                       store = wide$store,
                                       stringsAsFactors = FALSE)),
            class = "fishmorph_landmarks")
}
