# =============================================================================
# species-finder.R -- does a species exist in the FISHMORPH table?
#
# Pure lookup logic, kept OUT of the shiny app on purpose: the "Species finder"
# tab of launch_fishmorph_space() calls find_fishmorph_species(), so what the
# app answers is exactly what the console answers, and what the tests lock.
#
# Matching ladder, from the most to the least certain answer:
#   exact      the cleaned binomial is in the table (case-insensitive)
#   close      no exact match, but a name within `max_distance` edits
#              (Levenshtein) -- a typo, a gender ending (-us/-a), a
#              transliteration. It is a CANDIDATE, never a silent correction.
#   genus_only the species is absent but its genus is present
#   absent     neither the species nor the genus
#   invalid    nothing left after cleaning
# A close match is not a synonymy check: a valid name absent from FISHMORPH
# may sit one letter away from a different, valid species. Taxonomic
# validation is validate_species_names() (FishBase), not this function.
# =============================================================================

# Reduce free text to a "Genus epithet" binomial (or a bare genus).
# Handles underscores, repeated blanks, authorities ("Salmo trutta Linnaeus,
# 1758"), qualifiers (cf., aff.), "sp."/"spp." and case. Subspecies and
# authorities are dropped: FISHMORPH is keyed on binomials.
.fm_clean_binomial <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("[_[:space:]]+", " ", x)
  x <- gsub("[^A-Za-z .-]", "", x)
  x <- trimws(x)
  vapply(strsplit(x, " ", fixed = TRUE), function(t) {
    t <- t[nzchar(t)]
    bare <- sub("\\.+$", "", tolower(t))
    t <- t[!(bare %in% c("cf", "aff"))]
    t <- sub("\\.+$", "", t)
    t <- t[nzchar(t)]
    if (!length(t)) return("")
    g <- paste0(toupper(substr(t[1], 1, 1)), tolower(substring(t[1], 2)))
    if (length(t) < 2L || (tolower(t[2]) %in% c("sp", "spp"))) return(g)
    paste(g, tolower(t[2]))
  }, character(1), USE.NAMES = FALSE)
}

#' Look up species in the FISHMORPH table
#'
#' Tells, for each name supplied, whether the species is present in the
#' FISHMORPH trait table, and if not, whether a close spelling or its genus is.
#' This is the engine of the "Species finder" tab of [launch_fishmorph_space()].
#'
#' Names are first cleaned: underscores and repeated blanks become single
#' spaces, authorities and subspecies are dropped (only the first two words are
#' kept), the qualifiers `cf.` and `aff.` are removed, and `sp.`/`spp.` turn the
#' query into a genus query. Matching is case-insensitive.
#'
#' The `status` column follows a ladder of certainty: `"exact"` (the binomial
#' is in the table); `"close"` (no exact match but a name at most
#' `max_distance` character edits away -- typically a typo or a gender ending,
#' to be checked by eye, never accepted blindly); `"genus_only"` (the species is
#' absent but other species of its genus are present); `"absent"`; `"invalid"`
#' (nothing usable after cleaning). A close match is not a synonymy check: use
#' [validate_species_names()] to resolve names against FishBase.
#'
#' @param species Character vector of names to look up.
#' @param reference The FISHMORPH table: a data frame with a `Species` column
#'   (and optionally `Family`, `Order`, `IUCN` and the nine trait columns), or
#'   the path to a semicolon-separated CSV. `NULL` (default) reads the table
#'   bundled with the package for `source`.
#' @param source `"segment"` or `"landmark"`, used only when `reference` is
#'   `NULL`. `NULL` follows [get_fishmorph_source()].
#' @param max_distance Maximum edit distance (Levenshtein) for a `"close"`
#'   match. `0` disables approximate matching.
#' @param n_suggestions Number of nearest names reported in `suggestions`.
#' @return A data frame with one row per query and the columns `query`,
#'   `query_clean`, `status`, `match` (the FISHMORPH name retained), `distance`
#'   (edit distance to it), `Family`, `Order`, `IUCN` (of the match, or of the
#'   genus for `"genus_only"`), `complete_traits` (whether the matched species
#'   has all nine traits, i.e. is placed in the morphological space), `n_genus`
#'   (number of FISHMORPH species in the queried genus) and `suggestions` (the
#'   nearest names, with their edit distance in brackets; for a genus query,
#'   species of that genus; `NA` for an exact match).
#' @seealso [launch_fishmorph_space()], [validate_species_names()],
#'   [load_fishmorph_reference()]
#' @examples
#' ref <- data.frame(
#'   Species = c("Salmo trutta", "Salmo salar", "Gobio gobio"),
#'   Family  = c("Salmonidae", "Salmonidae", "Gobionidae"),
#'   Order   = c("Salmoniformes", "Salmoniformes", "Cypriniformes"))
#' find_fishmorph_species(c("salmo_trutta", "Salmo truta", "Salmo marmoratus",
#'                          "Gobio sp.", "Esox lucius"), reference = ref)
#' @export
find_fishmorph_species <- function(species, reference = NULL, source = NULL,
                                   max_distance = 2, n_suggestions = 3) {
  if (is.null(reference)) {
    reference <- fishmorph_space_data(source)
    if (!nzchar(reference))
      stop("The FISHMORPH table for this source is not bundled; pass ",
           "`reference` explicitly.", call. = FALSE)
  }
  if (is.character(reference) && length(reference) == 1L) {
    if (!file.exists(reference))
      stop("Reference table not found: ", reference, call. = FALSE)
    reference <- utils::read.csv(reference, sep = ";",
                                 stringsAsFactors = FALSE)
  }
  if (!is.data.frame(reference) || !("Species" %in% names(reference)))
    stop("`reference` must be a data frame with a `Species` column.",
         call. = FALSE)
  if (!nrow(reference))
    stop("`reference` has no rows.", call. = FALSE)
  if (!is.numeric(max_distance) || length(max_distance) != 1L ||
      is.na(max_distance) || max_distance < 0)
    stop("`max_distance` must be a single number >= 0.", call. = FALSE)
  n_suggestions <- max(0L, as.integer(n_suggestions)[1])

  ref_sp  <- .fm_clean_binomial(reference$Species)
  ref_lc  <- tolower(ref_sp)
  ref_gen <- sub(" .*$", "", ref_sp)
  col <- function(nm)
    if (nm %in% names(reference)) as.character(reference[[nm]]) else
      rep(NA_character_, nrow(reference))
  fam <- col("Family"); ord <- col("Order"); iucn <- col("IUCN")
  traits <- intersect(c("BEl", "VEp", "REs", "OGp", "RMl", "BLs", "PFv",
                        "PFs", "CPt"), names(reference))
  complete <- if (length(traits) == 9L)
    stats::complete.cases(reference[, traits, drop = FALSE]) else
      rep(NA, nrow(reference))

  species <- as.character(species)
  q <- .fm_clean_binomial(species)
  n <- length(q)
  out <- data.frame(
    query = species, query_clean = q, status = rep(NA_character_, n),
    match = NA_character_, distance = NA_integer_,
    Family = NA_character_, Order = NA_character_, IUCN = NA_character_,
    complete_traits = NA, n_genus = 0L, suggestions = NA_character_,
    stringsAsFactors = FALSE)

  for (i in seq_len(n)) {
    qi <- q[i]
    if (!nzchar(qi)) { out$status[i] <- "invalid"; next }
    gi <- sub(" .*$", "", qi)
    in_genus <- which(ref_gen == gi)
    out$n_genus[i] <- length(in_genus)

    # Genus-level query ("Barbus", "Barbus sp."): no species to match.
    if (!grepl(" ", qi, fixed = TRUE)) {
      if (length(in_genus)) {
        out$status[i] <- "genus_only"
        out$Family[i] <- fam[in_genus[1]]
        out$Order[i]  <- ord[in_genus[1]]
        if (n_suggestions > 0L)
          out$suggestions[i] <- paste(
            utils::head(sort(unique(ref_sp[in_genus])), n_suggestions),
            collapse = "; ")
      } else out$status[i] <- "absent"
      next
    }

    # Exact hit: no edit distance to compute (and no suggestion to make) --
    # keeps a long list of valid names fast.
    ex <- match(tolower(qi), ref_lc)
    if (!is.na(ex)) {
      d <- rep(0, length(ref_lc)); best <- ex
    } else {
      d <- as.vector(utils::adist(tolower(qi), ref_lc))
      best <- order(d, ref_sp)[1]
    }
    if (is.na(ex) && n_suggestions > 0L) {
      o <- order(d, ref_sp)
      s <- o[seq_len(min(n_suggestions, length(o)))]
      out$suggestions[i] <- paste0(ref_sp[s], " [", d[s], "]",
                                   collapse = "; ")
    }
    if (d[best] <= max_distance) {
      out$status[i]   <- if (d[best] == 0) "exact" else "close"
      out$match[i]    <- as.character(reference$Species[best])
      out$distance[i] <- as.integer(d[best])
      out$Family[i]   <- fam[best]
      out$Order[i]    <- ord[best]
      out$IUCN[i]     <- iucn[best]
      out$complete_traits[i] <- complete[best]
    } else if (length(in_genus)) {
      out$status[i] <- "genus_only"
      out$Family[i] <- fam[in_genus[1]]
      out$Order[i]  <- ord[in_genus[1]]
    } else {
      out$status[i] <- "absent"
    }
  }
  out
}
