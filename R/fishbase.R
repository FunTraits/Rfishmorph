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
