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
#' @param sheet Landmark sheet(s) in `xlsx` holding the PUBLISHED specimens
#'   (default `"Global_Landmark"`).
#' @param new_sheet Sheet(s) of `xlsx` holding the specimens digitized SINCE the
#'   publication -- species absent from FISHMORPH, entered either through the
#'   digitizer's "new" queue or through the "Absent from FISHMORPH" panel of
#'   FishInTrait. Both spellings in use are looked for by default; those the
#'   workbook does not carry are simply skipped. `NULL` ignores them, which is
#'   the behaviour of the versions before this argument existed. A species
#'   present in both `sheet` and `new_sheet` keeps its `sheet` record: the new
#'   sheets are a STAGING AREA, promoted to `Global_Landmark` once validated,
#'   so a duplicate means the promotion has already happened and the staged row
#'   is the stale one. The `store` column of the returned table names the
#'   origin (`"xlsx"`, `"xlsx_new"` or `"duckdb"`), so a table built on
#'   unvalidated specimens can always be traced back. Mind that one row of a
#'   staging sheet is one PHOTOGRAPH and not one species -- several specimens
#'   of the same species are the point of the plate mode -- whereas this table
#'   is one row per species: the first row met is kept and the others are
#'   dropped, without averaging. Pooling repeated specimens is a decision about
#'   the campaign, not a detail of the reading.
#' @param metadata Table supplying the **non-morphometric** columns only --
#'   `Species`, `Family`, `Order`, `Genus`, `MBl`, `MBw`, `IUCN` -- joined by
#'   species name. `NULL` (default) reads them from the segment table, which is
#'   simply where they are stored. The join is restricted to that whitelist, so
#'   no ratio can cross over even if the supplied table carries some. This
#'   argument is deliberately *not* called `reference`: it does not define the
#'   trait space and plays no part in the segments, ratios or imputation.
#' @param fishbase_size Fill the `MBl` / `MBw` the metadata table cannot supply
#'   from FishBase, through [fishmorph_fishbase_size()] (default `FALSE`:
#'   it needs the network and the `rfishbase` package). A species digitized
#'   since the publication is by construction absent from the segment table,
#'   where those two columns live, so it comes out sized `NA` -- and
#'   [prepare_fishmorph_basins()] runs `complete.cases()` over the ratios AND
#'   `size_traits`, which drops it from the functional space altogether. The
#'   maximum STANDARD length and the maximum weight are converted to
#'   `log10(x + 1)`, the scale of the rest of the table. An existing value is
#'   never overwritten, and the added `size_source` column
#'   (`"fishmorph_publi"` / `"fishbase"`) keeps the two apart -- without it a
#'   derived size becomes indistinguishable from a published one at the next
#'   read, and the origin of a point in the ordination is lost.
#' @param impute_size Impute the `MBl` / `MBw` still missing after the metadata
#'   join and, if asked for, after FishBase (default `TRUE`). A SECOND pass,
#'   run with the same `na_action`, in which the nine ratios -- complete by
#'   then -- and the phylogenetic axes predict the size, and nothing predicts
#'   the ratios back: the values already in the table are left bit for bit as
#'   they were. `"keep"` and `"omit"` are not honoured here, the first because
#'   it means leaving the gaps and the second because dropping a species for
#'   want of a size is a decision for the analysis, not for the reading.
#'   Imputed sizes are marked `size_source = "imputed"`, and they should be:
#'   an invented body length weighs on the first axis exactly like a measured
#'   one, and no other column tells them apart. One column cannot describe two:
#'   `size_source` records the last operation that touched EITHER of `MBl` and
#'   `MBw`. In practice the two are missing together -- both come from the same
#'   join, and the current table has exactly 466 of each -- so the ambiguity is
#'   theoretical; split it into two columns if that ever stops holding.
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
#'   `MBl`, `MBw`, `IUCN`, plus the provenance columns `store` (`"xlsx"`,
#'   `"xlsx_new"` or `"duckdb"`), `size_source` (`"fishmorph_publi"`,
#'   `"fishbase"` or `"imputed"`) and `n_imputed`. `n_imputed` counts RATIOS
#'   only: a size that was imputed is reported by `size_source`, not by it.
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
                                           new_sheet = c("New_specimen",
                                                         "new_specimens"),
                                           metadata = NULL,
                                           fishbase_size = FALSE,
                                           impute_size = TRUE,
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
  ## THREE stores, not two: the published sheet, the staging sheets of the
  ## species digitized since, and the live DuckDB. Leaving the staging sheets
  ## out was silently dropping every species added since the publication --
  ## precisely the ones a running campaign produces.
  stores <- list()
  if (!is.null(xlsx)) {
    stores$xlsx <- .fm_lm_from_xlsx(xlsx, sheet)
    if (!is.null(new_sheet) && length(new_sheet))
      stores$xlsx_new <- .fm_lm_from_xlsx(xlsx, new_sheet, optional = TRUE,
                                          verbose = verbose)
  }
  if (!is.null(db))   stores$duckdb <- .fm_lm_from_duckdb(db)
  if (verbose && !is.null(stores$xlsx_new))
    message(sprintf("%d specimen(s) read from the new-specimen sheet(s): %s.",
                    nrow(stores$xlsx_new),
                    paste(sort(unique(stores$xlsx_new$Genus.species)),
                          collapse = ", ")))

  wide <- .fm_lm_bind_stores(stores, verbose)
  if (is.null(wide) || !nrow(wide))
    stop("No digitized specimen found in the supplied store(s).", call. = FALSE)

  ## De-duplicate on the canonical species key, by DECREASING authority:
  ##   duckdb   : the live digitizer output, written at every record
  ##   xlsx     : the published sheet, validated but updated only in batches
  ##   xlsx_new : the staging sheets, not yet promoted
  ## `order()` is a stable sort, so ties inside a store keep the order the
  ## sheets were named in.
  wide$.key <- .canon_species_name(wide$Genus.species)
  ord <- order(match(wide$store, .FM_LM_STORE_RANK))
  wide <- wide[ord, , drop = FALSE]

  ## The TAXONOMY is not subject to the store priority, and must not be.
  ## The priority arbitrates a MEASUREMENT: of two configurations of the same
  ## species, the most recent wins. A family is not a measurement -- it is the
  ## same whichever store is read -- and the DuckDB, built from a journal that
  ## records a species name and nothing else, carries none at all
  ## (`.fm_lm_from_duckdb()` sets `Family` and `Order` to NA by construction).
  ## Applying the row priority to it therefore ERASED a family that the workbook
  ## held, and did so precisely for the species that had just been re-digitized:
  ## the more recent the work, the more certain the loss.
  ##
  ## So: the geometry comes from the winning row, the taxonomy from the
  ## highest-ranked store that actually HAS one. `wide` is already sorted by
  ## rank, so the first non-empty value per species is exactly that.
  .first_filled <- function(col) {
    if (!col %in% names(wide)) return(stats::setNames(character(0), character(0)))
    v <- trimws(as.character(wide[[col]]))
    v[is.na(v) | !nzchar(v) | tolower(v) %in% c("na", "unknown")] <- NA_character_
    idx <- which(!is.na(v))                     # rows carrying a value, in rank order
    idx <- idx[!duplicated(wide$.key[idx])]     # the first per species
    stats::setNames(v[idx], wide$.key[idx])
  }
  tax_fill <- list(Family = .first_filled("Family"),
                   Order  = .first_filled("Order"),
                   Genus  = .first_filled("Genus"),
                   IUCN   = .first_filled("IUCN"))

  dup <- duplicated(wide$.key)
  if (verbose && any(dup))
    message(sprintf("%d species held by more than one store; kept by priority %s.",
                    sum(dup), paste(.FM_LM_STORE_RANK, collapse = " > ")))
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

  ## The fallback is the CROSS-STORE taxonomy, not the winning row's own: see
  ## `tax_fill` above. `wide$Family` would be NA for every species the DuckDB
  ## supplies, which is every species that has just been re-digitized.
  tax_of <- function(col) unname(tax_fill[[col]][wide$.key])
  out <- data.frame(
    Species = ifelse(is.na(m), species_lbl, metadata[["Species"]][m]),
    Family  = take("Family", tax_of("Family")),
    Order   = take("Order",  tax_of("Order")),
    Genus   = take("Genus",  tax_of("Genus")),
    stringsAsFactors = FALSE)
  for (rn in c("REs", "VEp", "RMl", "OGp", "BEl", "BLs", "PFv", "PFs", "CPt"))
    out[[rn]] <- unname(X[, rn])
  out$MBl   <- take("MBl", NULL)
  out$MBw   <- take("MBw", NULL)
  out$IUCN  <- take("IUCN", tax_of("IUCN"))
  out$store <- wide$store
  out$n_imputed <- as.integer(n_imputed)
  out$size_source <- ifelse(is.na(out$MBl) & is.na(out$MBw), NA_character_,
                            "fishmorph_publi")
  rownames(out) <- NULL

  ## -- 6. the sizes the segment table cannot supply ------------------------
  ## `MBl` / `MBw` come from the metadata join, that is from the SEGMENT table.
  ## A species digitized since the publication is not in it -- that is what
  ## makes it new -- so it comes out here with no size at all. It is not a
  ## harmless gap: `prepare_fishmorph_basins()` runs `complete.cases()` over the
  ## nine ratios AND `size_traits`, so a species without a body length leaves
  ## the functional space entirely rather than sitting in it at an invented
  ## size. Measured shape, and no point on the map.
  ##
  ## FishBase fills them, on the same terms as the published column: maximum
  ## STANDARD length in cm, maximum weight in g, both taken to log10(x + 1)
  ## here since that is the scale the rest of the table is on. An existing
  ## value is never overwritten -- FishBase completes the campaign, it does not
  ## arbitrate it -- and `size_source` says where each one comes from.
  if (isTRUE(fishbase_size)) {
    need <- is.na(out$MBl) | is.na(out$MBw)
    if (!any(need)) {
      if (verbose) message("FishBase: every species already has MBl and MBw.")
    } else {
      fb <- fishmorph_fishbase_size(out$Species[need], verbose = verbose)
      k <- match(.canon_species_name(out$Species), .canon_species_name(fb$Species))
      newL <- log10(fb$MBl_cm[k] + 1)
      newW <- log10(fb$MBw_g[k] + 1)
      fillL <- is.na(out$MBl) & is.finite(newL)
      fillW <- is.na(out$MBw) & is.finite(newW)
      out$MBl[fillL] <- newL[fillL]
      out$MBw[fillW] <- newW[fillW]
      out$size_source[fillL | fillW] <- "fishbase"
      if (verbose) {
        ## The control that matters is not "the column changed" but "the added
        ## values are of the same order as the old ones": a centimetre/metre or
        ## gram/kilogram slip is invisible any other way, and would move a
        ## species by two log units in the ordination.
        old <- out$size_source %in% "fishmorph_publi" & is.finite(out$MBl)
        new <- out$size_source %in% "fishbase" & is.finite(out$MBl)
        message(sprintf("FishBase: %d MBl and %d MBw filled in; %d species still without a size.",
                        sum(fillL), sum(fillW), sum(is.na(out$MBl) | is.na(out$MBw))))
        if (any(new))
          message(sprintf(paste("  control MBl (log10 cm + 1): published median",
                                "%.2f [%.2f-%.2f], FishBase median %.2f [%.2f-%.2f]",
                                "-- an order of magnitude apart is a UNIT, not biology."),
                          stats::median(out$MBl[old]), min(out$MBl[old]), max(out$MBl[old]),
                          stats::median(out$MBl[new]), min(out$MBl[new]), max(out$MBl[new])))
      }
    }
  }

  ## -- 7. imputing the sizes FishBase could not supply either ---------------
  ## A SECOND pass, deliberately, and not the nine ratios and the two sizes in
  ## one matrix. A joint run would let `MBl` and `MBw` act as predictors of the
  ## ratios, which would change the imputed value of every partly measured
  ## species already in the table -- a silent revision of numbers that have been
  ## published. Here the ratios (complete after step 3) predict the size and
  ## nothing predicts them back: strictly additive, and the allometric
  ## information still travels in the direction that is being asked for.
  ##
  ## The predictors are taken AS THEY STAND in `X`, hence on the scale `log`
  ## chose; the sizes are on log10(x + 1) in every case, since that is the scale
  ## the column is defined on. A random forest is invariant to a monotone
  ## rescaling of its predictors, so the mixture costs nothing -- but the value
  ## written back is a log size, which is what the column expects.
  ##
  ## `"omit"` and `"keep"` are NOT honoured here. `"keep"` means leaving the
  ## gaps, and `"omit"` would drop a species for want of a size -- a different
  ## decision from dropping it for want of a ratio, and one that belongs to the
  ## analysis, not to the reading. `prepare_fishmorph_basins()` already makes it
  ## explicitly, through its `complete.cases()` on `size_traits`.
  if (isTRUE(impute_size) && !(na_action %in% c("keep", "omit")) &&
      (anyNA(out$MBl) || anyNA(out$MBw))) {
    S <- cbind(X, MBl = out$MBl, MBw = out$MBw)
    rownames(S) <- wide$.key
    dead <- colSums(!is.na(S)) < 2L        # a wholly empty column cannot be imputed
    if (any(dead[c("MBl", "MBw")])) {
      if (verbose)
        message("Size imputation skipped: ",
                paste(c("MBl", "MBw")[dead[c("MBl", "MBw")]], collapse = " and "),
                " has no observed value to learn from.")
    } else {
      res_s <- .apply_na_action(S[, !dead, drop = FALSE], groups = NULL,
                                na_action = na_action,
                                missforest_ntree = missforest_ntree,
                                missforest_maxiter = missforest_maxiter,
                                context = "maximum size",
                                tree = tree,
                                missforest_phylo_k = missforest_phylo_k,
                                phylo_axes = phylo_axes,
                                species = wide$.key)
      Si <- res_s$X
      fillL <- is.na(out$MBl) & is.finite(Si[, "MBl"])
      fillW <- is.na(out$MBw) & is.finite(Si[, "MBw"])
      out$MBl[fillL] <- Si[fillL, "MBl"]
      out$MBw[fillW] <- Si[fillW, "MBw"]
      out$size_source[fillL | fillW] <- "imputed"
      if (verbose) {
        message(sprintf(paste("Size imputation (%s): %d MBl and %d MBw filled;",
                              "%d species still without a size."),
                        na_action, sum(fillL), sum(fillW),
                        sum(is.na(out$MBl) | is.na(out$MBw))))
        ## Same control as for FishBase, and for the same reason: an imputed
        ## size that lands an order of magnitude away from the observed ones is
        ## a bug in the predictors, not a small fish.
        obs <- !(out$size_source %in% "imputed") & is.finite(out$MBl)
        imp <- out$size_source %in% "imputed" & is.finite(out$MBl)
        if (any(imp) && any(obs))
          message(sprintf(paste("  control MBl: observed median %.2f [%.2f-%.2f],",
                                "imputed median %.2f [%.2f-%.2f]."),
                          stats::median(out$MBl[obs]), min(out$MBl[obs]), max(out$MBl[obs]),
                          stats::median(out$MBl[imp]), min(out$MBl[imp]), max(out$MBl[imp])))
        message("  These species carry size_source = \"imputed\": an invented ",
                "body length weighs on PC1 like a measured one, and only that ",
                "column tells them apart.")
      }
    }
  }

  if (verbose) {
    message(sprintf(
      "Landmark trait table: %d species, %d fully measured, %d with >=1 imputed ratio (scale: %s).",
      nrow(out), sum(out$n_imputed == 0L), sum(out$n_imputed > 0L),
      if (log) "log10(x + 1)" else "raw"))
    ## The provenance of the SIZE, spelled out in the same breath as that of the
    ## ratios: the two travel together into the ordination and are weighted the
    ## same, so reporting one and not the other would misrepresent the table.
    ss <- table(factor(out$size_source,
                       levels = c("fishmorph_publi", "fishbase", "imputed")),
                useNA = "no")
    message(sprintf("  size (MBl/MBw): %d published, %d from FishBase, %d imputed, %d absent.",
                    ss[["fishmorph_publi"]], ss[["fishbase"]], ss[["imputed"]],
                    sum(is.na(out$MBl) | is.na(out$MBw))))
  }

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

# Stack the landmark stores on their UNION of point columns.
#
# The two stores do not carry the same points, and nothing says they should:
# the publication workbook holds 1-19, 22 and the derived 23, while the
# digitizer journal -- and therefore the DuckDB store built from it -- also
# holds the axis hinges 24 and 25, which did not exist when the workbook was
# written. A plain rbind() then dies on "numbers of columns of arguments do not
# match", naming neither the stores nor the points.
#
# The union is the right answer, and the intersection would be a silent
# corruption: 24 and 25 are what .fm_axis_chain() follows to measure Bl along
# the BROKEN axis, so dropping them would fall back on the straight chord and
# shorten every curved fish by a median 8.5 % (see .FM_AXIS_HINGES in schema.R)
# -- a change of results that no message would announce. A point a store does
# not carry is a point NOT PLACED there, which is exactly what NA means.
.fm_lm_bind_stores <- function(stores, verbose = TRUE) {
  parts <- stores[!vapply(stores, function(d) is.null(d) || !nrow(d), logical(1))]
  if (!length(parts)) return(NULL)

  pts <- sort(unique(unlist(lapply(parts, function(d)
    .fm_lm_wide_cols(names(d))), use.names = FALSE)))
  # Numeric order (1_X, 1_Y, 2_X, ...), not lexical, which would file 10 before 2.
  if (length(pts))
    pts <- pts[order(as.integer(sub("_[XY]$", "", pts)), sub("^[0-9]+_", "", pts))]
  meta <- c("Genus.species", "Family", "Order", "Genus", "IUCN")

  if (verbose && length(parts) > 1L) {
    per <- lapply(parts, function(d) .fm_lm_wide_cols(names(d)))
    extra <- lapply(names(per), function(s) setdiff(pts, per[[s]]))
    names(extra) <- names(per)
    for (s in names(extra)) if (length(extra[[s]]))
      message(sprintf("Store '%s' does not carry %s; filled with NA.",
                      s, paste(sort(unique(sub("_[XY]$", "", extra[[s]]))),
                               collapse = ", ")))
  }

  parts <- lapply(names(parts), function(s) {
    d <- parts[[s]]
    for (m in setdiff(meta, names(d))) d[[m]] <- NA_character_
    for (p in setdiff(pts, names(d))) d[[p]] <- NA_real_
    d$store <- s
    d[c(meta, pts, "store")]
  })
  do.call(rbind, parts)
}

# Points 20 and 21 are the two ends of the SCALE BAR, not anatomy. The
# digitizer records them on the new-specimen sheets to convert pixels into
# millimetres; reading them here would slip a ruler into the landmark array,
# where every other function assumes a point is a body part.
.FM_LM_NOT_ANATOMY <- c(20L, 21L)

# The landmark stores in DECREASING order of authority. Used to de-duplicate
# on the species key, and nowhere else.
.FM_LM_STORE_RANK <- c("duckdb", "xlsx", "xlsx_new")

# `sheet` may name SEVERAL sheets, read and stacked in the order given, which
# is also their order of authority once de-duplicated upstream. `optional`
# tolerates a sheet the workbook does not carry: the two spellings of the
# new-specimen sheet coexist in the field, and demanding both would make the
# function fail on every workbook that holds only one.
.fm_lm_from_xlsx <- function(path, sheet = "Global_Landmark",
                             optional = FALSE, verbose = TRUE) {
  .fm_require("readxl", "read the FISHMORPH landmark workbook")
  if (!file.exists(path))
    stop("Landmark workbook not found: ", path, call. = FALSE)
  have <- readxl::excel_sheets(path)
  miss <- setdiff(sheet, have)
  if (length(miss)) {
    if (!optional)
      stop("Sheet(s) not found in ", basename(path), ": ",
           paste(miss, collapse = ", "), call. = FALSE)
    if (verbose)
      message(sprintf("Sheet(s) absent from %s, skipped: %s.",
                      basename(path), paste(miss, collapse = ", ")))
  }
  sheet <- intersect(sheet, have)
  if (!length(sheet)) return(NULL)

  one <- function(sh) {
    df <- as.data.frame(readxl::read_excel(path, sheet = sh,
                                           guess_max = 1048576))
    co <- .fm_lm_wide_cols(names(df))
    co <- co[!(as.integer(sub("_[XY]$", "", co)) %in% .FM_LM_NOT_ANATOMY)]
    if (!length(co)) {
      if (optional) return(NULL)
      stop("Sheet '", sh, "' holds no `<point>_X` / `<point>_Y` column.",
           call. = FALSE)
    }
    for (cc in co) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
    ## A row counts as digitized once the snout tip is placed.
    anchor <- if ("1_X" %in% co) df[["1_X"]] else df[[co[1]]]
    df <- df[!is.na(anchor), , drop = FALSE]
    ## A staging sheet reduced to its header row is the normal state of a
    ## workbook on which nothing new has been digitized yet.
    if (!nrow(df)) return(NULL)
    ## `IUCN` is read when the sheet carries it. No landmark yields a threat
    ## status and no store computes one: it can only be TYPED, and the only
    ## place it can be typed for a species absent from the segment table is the
    ## sheet that species lives on. A column that is not there costs nothing.
    keep <- intersect(c("Genus.species", "Family", "Order", "Genus", "IUCN"),
                      names(df))
    df[c(keep, co)]
  }

  parts <- Filter(Negate(is.null), lapply(sheet, one))
  if (!length(parts)) return(NULL)
  if (length(parts) == 1L) return(parts[[1]])

  ## Sheets need not carry the same points: stack them on the UNION, the
  ## absent ones being NA. The intersection would silently amputate the
  ## published sheet of whatever a staging sheet happens not to hold.
  cols <- unique(unlist(lapply(parts, names), use.names = FALSE))
  parts <- lapply(parts, function(d) {
    for (cc in setdiff(cols, names(d)))
      d[[cc]] <- if (grepl("_[XY]$", cc)) NA_real_ else NA_character_
    d[cols]
  })
  do.call(rbind, parts)
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
  ## The journal records a species name and no rank above it, so this store has
  ## no family and no order to give. They are NOT lost: the builder resolves the
  ## taxonomy across ALL stores (`tax_fill`) instead of taking it from the row
  ## that won the geometry -- otherwise a re-digitized species would come out
  ## with an empty family precisely because its measurement is the freshest.
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
