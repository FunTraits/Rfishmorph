# =============================================================================
# add-species.R -- guided workflow to create and append new FISHMORPH species.
# =============================================================================

#' Create a FISHMORPH record for a new species
#'
#' Builds a single, schema-compliant FISHMORPH row from digitized landmarks, a
#' set of segments, or ready-made ratios, optionally resolving the name against
#' FishBase and running basic quality control.
#'
#' @param species Binomial "Genus species".
#' @param landmarks Optional `fishmorph_landmarks` object (segments and ratios
#'   are computed from it).
#' @param segments Optional named list/one-row data frame of the 11 segments.
#' @param ratios Optional named list/one-row data frame of the 9 ratios (used
#'   when neither landmarks nor segments are supplied).
#' @param scale_cm Scale-bar length in cm when `landmarks` are supplied.
#' @param family,order,genus Optional taxonomy; `genus` defaults to the first
#'   token of `species`.
#' @param max_body_length,max_body_width Optional `MBl`/`MBw` (cm).
#' @param iucn Optional IUCN status code.
#' @param validate Resolve `species` via [validate_species_names()] (default
#'   `FALSE`; requires rfishbase).
#' @return A one-row data frame with taxonomy, the 9 ratios, the 11 segments (as
#'   `seg_*` columns) and `MBl`/`MBw`/`IUCN`. `attr(, "qc")` holds the
#'   [check_infinite_ratios()] report.
#' @examples
#' new_fishmorph_species(
#'   "Genus novus",
#'   segments = list(Bl = 10, Bd = 3.2, Hd = 2.4, Eh = 1.1, Mo = 1.6,
#'                   PFi = 1.9, PFl = 2.1, Ed = 0.6, Jl = 1.3, CPd = 1, CFd = 3))
#' @export
new_fishmorph_species <- function(species, landmarks = NULL, segments = NULL,
                                  ratios = NULL, scale_cm = 1, family = NA,
                                  order = NA, genus = NULL,
                                  max_body_length = NA, max_body_width = NA,
                                  iucn = NA, validate = FALSE) {
  if (is.null(genus)) genus <- strsplit(trimws(species), "\\s+")[[1]][1]
  seg_row <- NULL; rat_row <- NULL
  if (!is.null(landmarks)) {
    tr <- fishmorph_traits(landmarks, scale_cm = scale_cm)
    seg_row <- tr[1, fishmorph_segment_names(), drop = FALSE]
    rat_row <- tr[1, fishmorph_ratio_names(), drop = FALSE]
  } else if (!is.null(segments)) {
    seg_row <- as.data.frame(as.list(segments))[1, , drop = FALSE]
    rr <- fishmorph_ratios(cbind(specimen = species, seg_row))
    rat_row <- rr[1, fishmorph_ratio_names(), drop = FALSE]
  } else if (!is.null(ratios)) {
    rat_row <- as.data.frame(as.list(ratios))[1, , drop = FALSE]
    miss <- setdiff(fishmorph_ratio_names(), names(rat_row))
    if (length(miss))
      stop("Missing ratio(s): ", paste(miss, collapse = ", "), call. = FALSE)
  } else {
    stop("Provide one of `landmarks`, `segments` or `ratios`.", call. = FALSE)
  }

  accepted <- NA_character_; status <- NA_character_
  if (validate) {
    v <- validate_species_names(species, verbose = FALSE)
    accepted <- v$accepted[1]; status <- v$status[1]
  }

  rec <- data.frame(
    Species = species,
    Species_accepted = accepted,
    taxonomy_status = status,
    Genus = genus, Family = family, Order = order,
    stringsAsFactors = FALSE)
  for (rn in fishmorph_ratio_names())
    rec[[rn]] <- if (!is.null(rat_row)) as.numeric(rat_row[[rn]]) else NA_real_
  if (!is.null(seg_row))
    for (sn in fishmorph_segment_names())
      rec[[paste0("seg_", sn)]] <- as.numeric(seg_row[[sn]])
  rec$MBl <- as.numeric(max_body_length)
  rec$MBw <- as.numeric(max_body_width)
  rec$IUCN <- iucn

  qc <- check_infinite_ratios(rec)
  if (nrow(qc))
    warning("QC flags on the new record: ",
            paste(sprintf("%s=%s(%s)", qc$trait, signif(qc$value, 3), qc$reason),
                  collapse = "; "), call. = FALSE)
  attr(rec, "qc") <- qc
  rec
}

#' Append new species to a FISHMORPH table
#'
#' Adds one or more records (from [new_fishmorph_species()]) to an existing
#' FISHMORPH reference table, aligning columns, blocking duplicate species and
#' reporting quality-control flags.
#'
#' @param reference A FISHMORPH data frame (e.g. from
#'   [load_fishmorph_reference()]).
#' @param new_records A single record or a list/data frame of records.
#' @param species_col Species column in `reference` (default `Species`).
#' @param overwrite Replace an existing species instead of erroring (default
#'   `FALSE`).
#' @return The augmented reference table. `attr(, "added")` lists the species
#'   added; `attr(, "qc")` collects QC flags.
#' @export
add_fishmorph_species <- function(reference, new_records, species_col = "Species",
                                  overwrite = FALSE) {
  if (is.data.frame(new_records)) {
    recs <- split(new_records, seq_len(nrow(new_records)))
  } else if (inherits(new_records, "data.frame")) {
    recs <- list(new_records)
  } else recs <- new_records
  recs <- lapply(recs, as.data.frame)

  added <- character(0); qc_all <- list()
  for (rec in recs) {
    sp <- rec[[species_col]] %||% rec$Species
    if (is.null(sp)) stop("Record has no species name.", call. = FALSE)
    exists_i <- which(as.character(reference[[species_col]]) == as.character(sp))
    if (length(exists_i)) {
      if (!overwrite)
        stop(sprintf("Species '%s' already present; set overwrite = TRUE.", sp),
             call. = FALSE)
      reference <- reference[-exists_i, , drop = FALSE]
    }
    # align columns (union), fill missing with NA
    miss_in_ref <- setdiff(names(rec), names(reference))
    for (c in miss_in_ref) reference[[c]] <- NA
    miss_in_rec <- setdiff(names(reference), names(rec))
    for (c in miss_in_rec) rec[[c]] <- NA
    reference <- rbind(reference, rec[names(reference)])
    added <- c(added, as.character(sp))
    q <- attr(rec, "qc"); if (!is.null(q) && nrow(q)) qc_all[[sp]] <- q
  }
  rownames(reference) <- NULL
  attr(reference, "added") <- added
  attr(reference, "qc") <- if (length(qc_all)) do.call(rbind, qc_all) else NULL
  message(sprintf("Added %d species: %s", length(added),
                  paste(added, collapse = ", ")))
  reference
}
