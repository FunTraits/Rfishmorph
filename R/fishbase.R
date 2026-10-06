# =============================================================================
# fishbase.R -- taxonomy resolution and a global freshwater fish list via
# rfishbase. All functions degrade gracefully when rfishbase is not installed or
# offline; nothing here is a hard dependency.
# =============================================================================

.need_rfishbase <- function() {
  if (!requireNamespace("rfishbase", quietly = TRUE))
    stop("Package 'rfishbase' is required for this function. ",
         "Install it with install.packages('rfishbase').", call. = FALSE)
}

# tidy "Genus species" strings: trim, collapse spaces, capitalize genus
.clean_binomial <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[_.]+", " ", x)
  x <- gsub("\\s+", " ", x)
  # capitalize first letter, lower the rest of the first token; keep epithet lower
  parts <- strsplit(x, " ")
  vapply(parts, function(p) {
    if (length(p) == 0 || all(!nzchar(p))) return(NA_character_)
    p[1] <- paste0(toupper(substring(p[1], 1, 1)),
                   tolower(substring(p[1], 2)))
    if (length(p) >= 2) p[2] <- tolower(p[2])
    paste(p[seq_len(min(length(p), 2))], collapse = " ")
  }, character(1))
}

#' Validate and resolve species names against FishBase
#'
#' Cleans binomials and resolves them to currently accepted FishBase names,
#' turning synonyms into valid names. Uses `rfishbase::validate_names()` (and
#' `synonyms()` for status) when available.
#'
#' @param species Character vector of "Genus species" names.
#' @param verbose Print a short summary (default `TRUE`).
#' @return A data frame with `input`, `cleaned`, `accepted` (validated FishBase
#'   name or `NA`), `status` (`"accepted"`, `"synonym"`, `"unresolved"`) and
#'   `changed` (logical).
#' @examples
#' \dontrun{
#' validate_species_names(c("Salmo trutta", "Barbus barbus", "Esox_lucius"))
#' }
#' @export
validate_species_names <- function(species, verbose = TRUE) {
  cleaned <- .clean_binomial(species)
  out <- data.frame(input = species, cleaned = cleaned,
                    accepted = NA_character_, status = "unresolved",
                    changed = FALSE, stringsAsFactors = FALSE)
  .need_rfishbase()
  uc <- unique(stats::na.omit(cleaned))
  val <- tryCatch(rfishbase::validate_names(uc), error = function(e) NULL)
  # validate_names returns accepted names (recycled/length-matched to input)
  map <- stats::setNames(rep(NA_character_, length(uc)), uc)
  if (!is.null(val) && length(val)) {
    v <- as.character(val)
    map[seq_len(min(length(v), length(uc)))] <-
      v[seq_len(min(length(v), length(uc)))]
  }
  acc <- map[cleaned]
  out$accepted <- unname(acc)
  out$status <- ifelse(is.na(out$accepted), "unresolved",
                       ifelse(out$accepted == out$cleaned, "accepted",
                              "synonym"))
  out$changed <- !is.na(out$accepted) & out$accepted != out$cleaned
  if (verbose)
    message(sprintf("Validated %d name(s): %d accepted, %d synonym(s) updated, %d unresolved.",
                    nrow(out), sum(out$status == "accepted"),
                    sum(out$status == "synonym"), sum(out$status == "unresolved")))
  out
}

#' Update the taxonomy of a FISHMORPH table
#'
#' Adds resolved FishBase names (and, optionally, updated Family/Order) to a
#' FISHMORPH trait table, keeping the original names for traceability.
#'
#' @param data A data frame with a species column.
#' @param species_col Name of the species column (default tries `Species`,
#'   `species`, `Genus.species`).
#' @param add_classification Also pull `Family`/`Order` from FishBase
#'   (`rfishbase::load_taxa()`), default `TRUE`.
#' @return `data` with added columns `Species_accepted`, `taxonomy_status` and,
#'   when requested, `Family_fb`, `Order_fb`.
#' @export
update_fishmorph_taxonomy <- function(data, species_col = NULL,
                                      add_classification = TRUE) {
  species_col <- species_col %||% intersect(c("Species", "species",
                                              "Genus.species"), names(data))[1]
  if (is.na(species_col))
    stop("No species column found; set `species_col`.", call. = FALSE)
  res <- validate_species_names(data[[species_col]], verbose = TRUE)
  data$Species_accepted <- res$accepted
  data$taxonomy_status <- res$status
  if (add_classification) {
    .need_rfishbase()
    taxa <- tryCatch(rfishbase::load_taxa(), error = function(e) NULL)
    if (!is.null(taxa)) {
      taxa <- as.data.frame(taxa)
      key <- if ("Species" %in% names(taxa)) "Species" else names(taxa)[1]
      m <- match(data$Species_accepted, taxa[[key]])
      if ("Family" %in% names(taxa)) data$Family_fb <- taxa$Family[m]
      if ("Order" %in% names(taxa))  data$Order_fb  <- taxa$Order[m]
    }
  }
  data
}

#' Build a global list of freshwater fishes
#'
#' Returns FishBase species flagged as occurring in fresh water. Uses the FishBase
#' `ecology`/`species` tables (`Fresh == -1`) via rfishbase. Results are large;
#' cache them for reuse.
#'
#' @param include_brackish Also include brackish-water species (default `FALSE`).
#' @param fields Extra species-level fields to return (e.g. `"Family"`).
#' @return A data frame with at least `Species` and the freshwater/brackish
#'   flags, one row per species.
#' @examples
#' \dontrun{
#' fw <- freshwater_fish_list()
#' nrow(fw)
#' }
#' @export
freshwater_fish_list <- function(include_brackish = FALSE, fields = NULL) {
  .need_rfishbase()
  sp <- tryCatch(rfishbase::species(), error = function(e) NULL)
  if (is.null(sp))
    stop("Could not retrieve the FishBase species table (offline?).",
         call. = FALSE)
  sp <- as.data.frame(sp)
  # habitat flags: Fresh / Brack / Saltwater are coded -1 (TRUE) / 0 (FALSE)
  is_fresh <- if ("Fresh" %in% names(sp)) sp$Fresh %in% c(-1, 1, TRUE) else NA
  is_brack <- if ("Brack" %in% names(sp)) sp$Brack %in% c(-1, 1, TRUE) else FALSE
  keep <- is_fresh | (include_brackish & is_brack)
  keep[is.na(keep)] <- FALSE
  cols <- unique(c("Species", "Genus", "Family", "Order", "Fresh", "Brack",
                   "Saltwater", fields))
  cols <- intersect(cols, names(sp))
  out <- sp[keep, cols, drop = FALSE]
  rownames(out) <- NULL
  message(sprintf("Freshwater%s species retrieved: %d.",
                  if (include_brackish) " (+ brackish)" else "", nrow(out)))
  out
}

# =============================================================================
# MAXIMUM SIZE FROM FISHBASE -- MBl (cm) and MBw (g)
#
# FISHMORPH stores `MBl = log10(maximum STANDARD length in cm + 1)` and
# `MBw = log10(maximum weight in g + 1)`. The conversion to logs is left to the
# caller: everything below works, and reports, on the RAW scale, where a wrong
# unit is still visible.
#
# Two traps, and neither announces itself.
#
# (1) THE LENGTH TYPE. `rfishbase::species()$Length` is the maximum published
#     length in whatever type `LTypeMaxM` says -- most often TL. FISHMORPH is
#     built on STANDARD length: the campaign that produced the published column
#     discarded every species whose `LTypeMaxM` was not "SL". Feeding a total
#     length into the same column would inflate it by ten to twenty-five per
#     cent, a bias no reading of the file could reveal. Rather than discard
#     those species, the length is CONVERTED through the FishBase LENGTH-LENGTH
#     table, and the conversion is refused when its result is not physically
#     possible (see `.fm_fb_sl_direction()`).
#
# (2) THE ORIENTATION OF THE LENGTH-LENGTH TABLE. The FishBase manual states
#     `Length2 = a + b * Length1`. rfishbase has carried an open report since
#     2017 (ropensci/rfishbase#119) that the two fields come out the other way
#     round, and fishbase.org labels the same columns "unknown" and "known"
#     rather than 1 and 2. Hard-coding either reading would be a coin toss on a
#     systematic bias, so the orientation is MEASURED on the data actually
#     downloaded: a standard length is shorter than a total length, so the
#     orientation whose slopes come out below 1 is the right one. If a future
#     rfishbase silently swaps the columns back, this recalibrates itself.
# =============================================================================

# Which orientation of the LENGTH-LENGTH table converts TOWARDS SL?
# Returns "l1_from_l2" (Length1 = a + b * Length2, the reading of issue #119)
# or "l2_from_l1" (the reading of the manual), plus the evidence for it.
.fm_fb_sl_direction <- function(ll, verbose = TRUE) {
  need <- c("Length1", "Length2", "a", "b")
  if (is.null(ll) || !nrow(ll) || !all(need %in% names(ll)))
    return(list(dir = NA_character_, n = c(0L, 0L), med = c(NA_real_, NA_real_)))
  # An NA length type must not propagate into the masks: `NA & TRUE` is NA, and
  # indexing with it would silently put NA rows into the median.
  t1 <- toupper(trimws(as.character(ll$Length1))); t1[is.na(t1)] <- ""
  t2 <- toupper(trimws(as.character(ll$Length2))); t2[is.na(t2)] <- ""
  b  <- suppressWarnings(as.numeric(ll$b))
  ok <- is.finite(b) & b > 0
  # rows pairing SL with a LONGER type; under the correct reading the slope
  # that takes the long type to SL is below 1.
  long <- c("TL", "FL")
  m_l2 <- ok & (t1 %in% long) & t2 == "SL"   # manual: SL = a + b * long
  m_l1 <- ok & (t2 %in% long) & t1 == "SL"   # issue #119: SL = a + b * long
  med <- c(l2_from_l1 = stats::median(b[m_l2], na.rm = TRUE),
           l1_from_l2 = stats::median(b[m_l1], na.rm = TRUE))
  n   <- c(l2_from_l1 = sum(m_l2), l1_from_l2 = sum(m_l1))
  # the orientation whose median slope is below 1 wins; a tie or an empty side
  # leaves NA, and the caller then converts nothing rather than guessing.
  cand <- names(med)[is.finite(med) & med < 1 & n >= 5L]
  dir <- if (length(cand) == 1L) cand else NA_character_
  if (verbose) {
    message(sprintf(paste("  LENGTH-LENGTH orientation: manual reading n=%d",
                          "median b=%s | issue-119 reading n=%d median b=%s"),
                    n[["l2_from_l1"]], format(med[["l2_from_l1"]], digits = 3),
                    n[["l1_from_l2"]], format(med[["l1_from_l2"]], digits = 3)))
    message("  -> orientation retained: ",
            if (is.na(dir)) "NONE (ambiguous): no length is converted" else dir)
  }
  list(dir = dir, n = n, med = med)
}

# Best (a, b) of the length-weight relationship W = a * L^b, one pair per
# species. Selection: the study with the highest `CoeffDetermination`, records
# without one sorting last -- the base-R equivalent of the slice_max() used to
# build the published table.
#
# `Type` (the length type the regression was fitted on) is preferred to "SL"
# when the column exists, so that the length fed into `a * L^b` is the length
# the coefficients were fitted for. The published FISHMORPH column did NOT make
# that distinction; the fallback therefore keeps the best regression whatever
# its type, and `lw_type` records which was used.
.fm_fb_length_weight_ab <- function(species) {
  sp <- .clean_binomial(species)
  uniq <- unique(sp[!is.na(sp) & nzchar(sp)])
  out <- data.frame(Species = uniq, a = NA_real_, b = NA_real_,
                    lw_type = NA_character_, stringsAsFactors = FALSE)
  if (!length(uniq)) return(out)
  lw <- tryCatch(as.data.frame(rfishbase::length_weight(uniq)),
                 error = function(e) {
                   warning("rfishbase::length_weight() failed: ",
                           conditionMessage(e), call. = FALSE); NULL })
  if (is.null(lw) || !nrow(lw) || !all(c("Species", "a", "b") %in% names(lw)))
    return(out)
  lw$a <- suppressWarnings(as.numeric(lw$a))
  lw$b <- suppressWarnings(as.numeric(lw$b))
  lw <- lw[is.finite(lw$a) & is.finite(lw$b), , drop = FALSE]
  if (!nrow(lw)) return(out)
  lw$Species <- .clean_binomial(lw$Species)
  ty <- if ("Type" %in% names(lw)) toupper(trimws(as.character(lw$Type)))
        else rep(NA_character_, nrow(lw))
  cd <- if ("CoeffDetermination" %in% names(lw))
          suppressWarnings(as.numeric(lw$CoeffDetermination))
        else rep(NA_real_, nrow(lw))
  # sort key: SL regressions first, then decreasing r2 (no r2 last)
  o <- order(lw$Species, !(ty %in% "SL"), -ifelse(is.na(cd), -Inf, cd))
  lw <- lw[o, , drop = FALSE]; ty <- ty[o]
  keep <- !duplicated(lw$Species)
  best <- lw[keep, c("Species", "a", "b"), drop = FALSE]
  best$lw_type <- ty[keep]
  i <- match(out$Species, best$Species)
  out$a <- best$a[i]; out$b <- best$b[i]; out$lw_type <- best$lw_type[i]
  out
}

#' Maximum standard length and weight from FishBase
#'
#' Returns, per species, the maximum STANDARD length in centimetres and the
#' maximum weight in grams -- the two raw quantities behind the `MBl` and `MBw`
#' columns of the FISHMORPH tables, which store them as `log10(x + 1)`.
#'
#' @section What is done, and why:
#' The length comes from `rfishbase::species()`, in the type given by
#' `LTypeMaxM`. A standard length is taken as it is; a total or fork length is
#' CONVERTED through the FishBase LENGTH-LENGTH table, whose orientation is
#' measured rather than assumed (see the note on `ropensci/rfishbase#119` in
#' the source). A conversion whose result falls outside
#' `ratio_bounds` -- a standard length below half the total length, or above
#' it -- is refused: the species then comes back with `NA` rather than with a
#' number no one can check.
#'
#' The weight is the maximum published weight when FishBase has one, and
#' `a * L^b` otherwise, `a` and `b` being taken from the length-weight study
#' with the highest coefficient of determination, preferring a regression
#' fitted on standard length. `weight_source` records which of the two it is:
#' an observed maximum and a derived one are not the same quantity, and a
#' column that mixes them without saying so cannot be audited.
#'
#' @param species Character vector of names, in any of the `"Genus species"`,
#'   `"Genus.species"` or `"Genus_species"` forms.
#' @param convert_length Convert a non-standard length type to SL (default
#'   `TRUE`). `FALSE` restricts the result to species FishBase already reports
#'   in SL, which is what the published FISHMORPH column did.
#' @param ratio_bounds Bounds on the accepted SL / (measured length) ratio
#'   (default `c(0.5, 1)`).
#' @param verbose Report the counts at each step (default `TRUE`).
#' @return A data frame with `Species`, `MBl_cm`, `MBw_g`, `length_type` (the
#'   type FishBase reported), `length_source` (`"SL"`, `"converted"` or `NA`),
#'   `weight_source` (`"observed"`, `"a*L^b"` or `NA`) and `lw_type`.
#' @references Froese, R. and D. Pauly, eds. FishBase, \url{https://www.fishbase.org}.
#' @seealso [build_fishmorph_landmark_table()], whose `fishbase_size` argument
#'   calls this to fill the species the segment table cannot supply.
#' @examples
#' \dontrun{
#' fishmorph_fishbase_size(c("Gobio occitaniae", "Salmo trutta"))
#' }
#' @export
fishmorph_fishbase_size <- function(species, convert_length = TRUE,
                                    ratio_bounds = c(0.5, 1),
                                    verbose = TRUE) {
  .need_rfishbase()
  sp <- .clean_binomial(species)
  uniq <- unique(sp[!is.na(sp) & nzchar(sp)])
  out <- data.frame(Species = uniq, MBl_cm = NA_real_, MBw_g = NA_real_,
                    length_type = NA_character_, length_source = NA_character_,
                    weight_source = NA_character_, lw_type = NA_character_,
                    stringsAsFactors = FALSE)
  if (!length(uniq)) return(out)
  if (verbose) message(sprintf("FishBase: %d species asked for.", length(uniq)))

  ## -- 1. the species table: maximum length, its type, maximum weight --------
  info <- tryCatch(as.data.frame(rfishbase::species(
    uniq, fields = c("Species", "Length", "LTypeMaxM", "Weight"))),
    error = function(e) {
      warning("rfishbase::species() failed: ", conditionMessage(e),
              call. = FALSE); NULL })
  if (is.null(info) || !nrow(info)) return(out)
  info$Species <- .clean_binomial(info$Species)
  info <- info[!duplicated(info$Species), , drop = FALSE]
  i <- match(out$Species, info$Species)
  L  <- suppressWarnings(as.numeric(info$Length[i]))
  ty <- toupper(trimws(as.character(info$LTypeMaxM[i])))
  W  <- suppressWarnings(as.numeric(info$Weight[i]))
  out$length_type <- ty
  is_sl <- !is.na(ty) & ty == "SL" & is.finite(L)
  out$MBl_cm[is_sl] <- L[is_sl]
  out$length_source[is_sl] <- "SL"
  if (verbose)
    message(sprintf("  length found for %d species, of which %d already in SL.",
                    sum(is.finite(L)), sum(is_sl)))

  ## -- 2. converting the other length types to SL ----------------------------
  todo <- is.finite(L) & !is_sl & !is.na(ty) & ty %in% c("TL", "FL")
  if (convert_length && any(todo)) {
    ll <- tryCatch(as.data.frame(rfishbase::length_length(out$Species[todo])),
                   error = function(e) {
                     warning("rfishbase::length_length() failed: ",
                             conditionMessage(e), call. = FALSE); NULL })
    cal <- .fm_fb_sl_direction(ll, verbose = verbose)
    if (!is.na(cal$dir) && !is.null(ll) && nrow(ll)) {
      ll$Species <- .clean_binomial(ll$Species)
      t1 <- toupper(trimws(as.character(ll$Length1))); t1[is.na(t1)] <- ""
      t2 <- toupper(trimws(as.character(ll$Length2))); t2[is.na(t2)] <- ""
      # `from` = the type we hold, `to` = SL, whichever column each lives in
      from <- if (identical(cal$dir, "l2_from_l1")) t1 else t2
      to   <- if (identical(cal$dir, "l2_from_l1")) t2 else t1
      # `a` is absent on the records FishBase stores as a bare ratio
      # (Length2 = b * Length1): the intercept is then zero, not missing.
      aa <- suppressWarnings(as.numeric(ll$a)); aa[is.na(aa)] <- 0
      bb <- suppressWarnings(as.numeric(ll$b))
      use <- to == "SL" & from %in% c("TL", "FL") & is.finite(bb) & bb > 0
      ll <- data.frame(Species = ll$Species[use], from = from[use],
                       a = aa[use], b = bb[use], stringsAsFactors = FALSE)
      k <- match(paste(out$Species, ty), paste(ll$Species, ll$from))
      sl <- ll$a[k] + ll$b[k] * L
      ratio <- sl / L
      ok <- todo & is.finite(sl) & is.finite(ratio) &
        ratio > ratio_bounds[1] & ratio <= ratio_bounds[2]
      out$MBl_cm[ok] <- sl[ok]
      out$length_source[ok] <- "converted"
      if (verbose) {
        refused <- sum(todo & is.finite(sl) & !ok)
        message(sprintf(paste("  %d length(s) converted to SL (median ratio",
                              "%.3f); %d refused as out of bounds; %d without",
                              "a conversion."),
                        sum(ok), stats::median(ratio[ok]), refused,
                        sum(todo & !is.finite(sl))))
      }
    }
  }

  ## -- 3. the weight ---------------------------------------------------------
  obs <- is.finite(W) & W > 0
  out$MBw_g[obs] <- W[obs]
  out$weight_source[obs] <- "observed"
  ab <- .fm_fb_length_weight_ab(out$Species)
  j <- match(out$Species, ab$Species)
  out$lw_type <- ab$lw_type[j]
  der <- ab$a[j] * out$MBl_cm^ab$b[j]
  fill <- !obs & is.finite(der) & der > 0
  out$MBw_g[fill] <- der[fill]
  out$weight_source[fill] <- "a*L^b"
  if (verbose)
    message(sprintf(paste("  weight: %d observed, %d derived from a*L^b",
                          "(of which %d on a non-SL regression); %d species",
                          "left without a size."),
                    sum(obs), sum(fill),
                    sum(fill & !(out$lw_type %in% "SL")),
                    sum(is.na(out$MBl_cm) | is.na(out$MBw_g))))
  out
}
