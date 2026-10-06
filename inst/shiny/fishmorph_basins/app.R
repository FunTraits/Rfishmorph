# =============================================================================
# Rfishmorph -- global drainage-basin explorer.
#
# Three panels, one shared selection:
#   1. Carte        world choropleth of the basins; click focuses one basin.
#   2. Espace       the FISHMORPH morphological space, reacting to that focus.
#   3. Tableau      one row per basin, with its functional-diversity indices.
#
# This file is a THIN layer, on the model of inst/shiny/fishmorph_space/app.R:
# the shapefile, the occurrence tables, the ordination and the indices were all
# resolved once by Rfishmorph::prepare_fishmorph_basins() and arrive here as a
# cache. What the application adds is the reading of them.
#
# Run with Rfishmorph::launch_fishmorph_basins(), or with shiny::runApp() on
# this directory once options(Rfishmorph.basin_cache = ...) is set.
# =============================================================================

library(shiny)
library(bslib)
library(DT)
library(plotly)
library(leaflet)
library(sf)

# shiny::runApp() sources this file into an environment whose PARENT IS THE
# USER'S GLOBAL ENVIRONMENT, not the shiny namespace: anything the caller
# happens to have named `need`, `validate` or `req` in their workspace is found
# before the shiny function of that name, and every guarded reactive then dies
# at the call. Binding the three here puts them in the application's own
# environment, which comes first in the lookup chain.
need     <- shiny::need
validate <- shiny::validate
req      <- shiny::req

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

# =============================================================================
# Data
# =============================================================================

.fmb_cache_path <- function() {
  p <- getOption("Rfishmorph.basin_cache", NULL)
  if (!is.null(p) && nzchar(p) && file.exists(p)) return(p)
  if (file.exists("fishmorph_basins.rds")) return("fishmorph_basins.rds")
  stop("Basin cache not found. Build it once:\n",
       "  Rfishmorph::prepare_fishmorph_basins(\n",
       "    shapefile   = \"Basin_202412_3364.shp\",\n",
       "    occurrences = \"Occurrence_Table.csv\",\n",
       "    cas         = \"cas_freshwater_202412.xlsx\",\n",
       "    out         = \"fishmorph_basins.rds\")\n",
       "then launch_fishmorph_basins(\"fishmorph_basins.rds\").", call. = FALSE)
}

CACHE <- readRDS(.fmb_cache_path())

BAS  <- CACHE$basins                      # one row per basin
GEO  <- CACHE$geometry                    # simplified sf polygons
SPE  <- CACHE$species                     # identity register, one row per species
CTRY <- CACHE$countries                   # exploded basin-by-country
OCC  <- CACHE$occ                         # named list of occurrence tables
IDX  <- CACHE$indices                     # named list of index tables
SET  <- CACHE$meta$settings

# One trait set per MEASUREMENT CAMPAIGN, each with its own ordination, all
# aligned row for row on SPE. A cache built before this split has none, and
# must be rebuilt rather than half-read: guessing which campaign its single
# ordination came from is exactly the ambiguity this restructuring removes.
# --- version handshake, before anything is read ------------------------------
# The cache and this file are written and read by different processes,
# installed at different moments, and the assumption that they match is the one
# that is regularly false. Checked here, once, so that a mismatch is named --
# and says WHICH of the two is behind -- instead of surfacing forty reactives
# later as "undefined columns selected", which names neither.
FMB_FORMAT <- 5L
cache_format <- CACHE$meta$format %||% 1L
if (cache_format < FMB_FORMAT)
  stop("Basin cache in format ", cache_format, ", this application expects ",
       FMB_FORMAT, ".\n  The cache is behind: rebuild it with ",
       "prepare_fishmorph_basins().", call. = FALSE)
if (cache_format > FMB_FORMAT)
  stop("Basin cache in format ", cache_format, ", this application expects ",
       FMB_FORMAT, ".\n  The APPLICATION is behind: reinstall 'Rfishmorph' ",
       "(devtools::install()), or run it with devtools::load_all().",
       call. = FALSE)

need_cols <- c("species_key", "species", "order", "family", "iucn")
miss_cols <- setdiff(need_cols, names(SPE))
if (length(miss_cols))
  stop("The species register lacks: ", paste(miss_cols, collapse = ", "),
       ".\n  Rebuild the cache with prepare_fishmorph_basins().", call. = FALSE)

TRAITSETS <- CACHE$traits
if (!is.list(TRAITSETS) || !length(TRAITSETS))
  stop("This cache carries no campaign-specific ordination.\n  Rebuild it ",
       "with prepare_fishmorph_basins().", call. = FALSE)

CAMP_LAB <- c(segment = "Segments (campagne publiee)",
              landmark = "Landmarks (re-digitalisation)")
CAMPAIGNS <- stats::setNames(
  names(TRAITSETS),
  ifelse(names(TRAITSETS) %in% names(CAMP_LAB),
         CAMP_LAB[names(TRAITSETS)], names(TRAITSETS)))

BROW <- stats::setNames(seq_len(nrow(BAS)), BAS$basin_id)
SROW <- stats::setNames(seq_len(nrow(SPE)), SPE$species_key)

# Taxonomic indexes: family -> species rows, order -> species rows. Built once
# at startup for the same reason as BY_SPEC -- "every species of the
# Characidae" must be a lookup, not a scan of the register on every keystroke.
# Rows, not keys, because every panel downstream (scores, has, ordination)
# addresses species BY ROW; the keys are recovered from SPE when needed.
.tax_index <- function(x) {
  x <- as.character(x)
  ok <- !is.na(x) & nzchar(x) & !(tolower(x) %in% c("na", "unknown", "inconnu"))
  split(which(ok), x[ok])
}
FAM_ROWS <- .tax_index(SPE$family)
ORD_ROWS <- .tax_index(SPE$order)

# The GENUS. Unlike the order and the family it is not an external attribute
# that a register may or may not carry: it is the first word of the binomial,
# and a species name that did not contain it would not be a species name. The
# cache column is used when it is filled -- it comes from the trait table and
# is authoritative -- and the name itself answers whenever it is not, which is
# the case of every species added since the publication, whose metadata join
# found nothing. A genus level with holes would be worse than none: the box
# would silently drop exactly the species one opens it to look for.
SPE$genus <- local({
  g <- if ("genus" %in% names(SPE)) as.character(SPE$genus)
       else rep(NA_character_, nrow(SPE))
  from_name <- sub("[ ._].*$", "", trimws(as.character(SPE$species)))
  bad <- is.na(g) | !nzchar(g) | tolower(g) %in% c("na", "unknown", "inconnu")
  g[bad] <- from_name[bad]
  g
})
GEN_ROWS <- .tax_index(SPE$genus)

# Labels carry the size of the taxon. A family is a unit whose weight the user
# cannot guess -- 4,000 species in one, 3 in the next -- and reading "(12 esp.)"
# before clicking is what stops a selection from being a surprise.
.tax_choices <- function(idx) {
  if (!length(idx)) return(character(0))
  o <- order(names(idx))
  stats::setNames(names(idx)[o],
                  sprintf("%s (%d esp.)", names(idx)[o], lengths(idx)[o]))
}

# The parent link, family -> order, and its inverse. A family SHOULD name one
# order, but the register is a compilation and a handful of families appear
# under two: the parent kept is then the modal one, i.e. the order of most of
# the species carrying that family name, and the minority rows keep their own
# order for every other purpose. Deciding here, once, is what stops the
# hierarchy from answering differently in the sidebar and in the map.
.fam_parent <- function(fam, ord) {
  fam <- as.character(fam); ord <- as.character(ord)
  ok <- !is.na(fam) & nzchar(fam) & !is.na(ord) & nzchar(ord)
  if (!any(ok)) return(stats::setNames(character(0), character(0)))
  tb <- table(factor(fam[ok]), factor(ord[ok]))
  stats::setNames(colnames(tb)[max.col(tb, ties.method = "first")],
                  rownames(tb))
}
ORD_OF_FAM <- .fam_parent(SPE$family, SPE$order)
FAM_BY_ORD <- split(names(ORD_OF_FAM), unname(ORD_OF_FAM))
# Same link, one rank lower. `.fam_parent()` is written on (child, parent) and
# not on (family, order) in particular, so the modal-parent rule that settles a
# family straddling two orders settles a genus straddling two families in
# exactly the same way -- and settles it HERE, once, rather than differently in
# the sidebar and on the map.
FAM_OF_GEN <- .fam_parent(SPE$genus, SPE$family)
GEN_BY_FAM <- split(names(FAM_OF_GEN), unname(FAM_OF_GEN))

# Occurrence tables carry species KEYS; every panel needs species ROWS. The
# translation is done once, here, rather than in a reactive -- it is a
# match() over ~200,000 records and it never changes.
for (s in names(OCC)) OCC[[s]]$row <- unname(SROW[OCC[[s]]$species_key])

# Two indexes built once at startup: basin -> species rows, and species ->
# basins. The second is what makes "light up every basin where this fish
# lives" a lookup rather than a scan of the whole table on every keystroke.
BY_BASIN  <- lapply(OCC, function(o) split(o$row, o$basin_id))
BY_SPEC   <- lapply(OCC, function(o) split(o$basin_id, o$species_key))
HAS_STAT  <- vapply(OCC, function(o) !all(is.na(o$status)), logical(1))

SOURCE_LAB <- c(tedesco = "Tedesco et al. (natif / exotique)",
                cas = "Catalogue of Fishes (2024-12)")
SOURCES <- stats::setNames(names(OCC),
                           ifelse(names(OCC) %in% names(SOURCE_LAB),
                                  SOURCE_LAB[names(OCC)], names(OCC)))

# Choropleth variables. Species counts and FRic are strongly right-skewed --
# a linear colour scale on 3,364 basins paints the world one colour and the
# Amazon another -- so they are binned on quantiles; the indices bounded in
# [0, 1] keep a continuous scale, where a value IS comparable across basins.
MAP_VARS <- c(
  "Richesse specifique (S)"          = "n_species",
  "Especes a morphologie mesuree"    = "n_traits",
  "Couverture morphologique"         = "coverage",
  "FRic (% de l'espace mondial)"     = "FRic_pct",
  "FRic (aire du polygone convexe)"  = "FRic",
  "FDiv (divergence)"                = "FDiv",
  "FDis (dispersion)"                = "FDis",
  "FEve (regularite)"                = "FEve",
  "Rao (entropie quadratique)"       = "Rao")
BOUNDED_VARS  <- c("coverage", "FDiv", "FEve")

# Variables whose colour scale is FIXED, not fitted on what happens to be in
# view. FRic as a share of the world pool is bounded in [0, 100] because the
# hull of a subset is contained in the hull of the set -- so 0 and 100 are real
# endpoints, and a scale stretched to the observed range would redefine its own
# maximum on every filter, making two maps of the same variable incomparable.
FIXED_DOMAIN <- list(FRic_pct = c(0, 100))

# Base layers. Beyond the neutral CartoDB tiles, the list is chosen for what a
# drainage basin is read against: three of them draw the river network and the
# relief that produced it, which is the context a basin outline is meaningless
# without. "Aucun" exists for the outline mode, where a white background is
# what makes 3,364 nested boundaries legible.
TILES <- c("CartoDB.Positron", "CartoDB.DarkMatter", "Esri.NatGeoWorldMap",
           "Esri.WorldTopoMap", "Esri.WorldPhysical", "Esri.WorldShadedRelief",
           "OpenTopoMap", "OpenStreetMap", "Aucun (fond blanc)")

# Esri Hydro Reference Overlay: a transparent raster tile layer whose line width
# is proportional to discharge, compiled from HydroSHEDS (WWF), GTOPO30, SRTM,
# GLWD and GRDC -- i.e. the same hydrological lineage as most global basin
# delineations, though NOT as the Tedesco basins used here, which are an
# independent delineation. The two are therefore a visual cross-check, not a
# nested pair, and a river running just outside a basin boundary is a
# disagreement between two data sets rather than an error in either.
#
# CAUTION: Esri put this layer in mature support in June 2025 and announces its
# retirement for December 2026; its replacement is a VECTOR tile basemap, which
# leaflet cannot draw without a plugin and an API key. When the tiles stop
# coming the map will show the basins on a blank hydro layer rather than fail,
# and the durable answer is to carry HydroRIVERS in the cache as data.
HYDRO_URL <- paste0("https://tiles.arcgis.com/tiles/P3ePLMYs2RVChkJx/arcgis/",
                    "rest/services/Esri_Hydro_Reference_Overlay/MapServer/",
                    "tile/{z}/{y}/{x}")
HYDRO_ATTR <- paste("Esri Hydro Reference Overlay -- Esri, HydroSHEDS (c) WWF,",
                    "USGS, NASA")

# HydroRIVERS carried by the cache, when prepare_fishmorph_basins(rivers = ...)
# was given one. This is the durable form of the same layer: data rather than a
# third-party service, attributed reach by reach to the basin it runs in, and
# not subject to anyone's retirement schedule.
RIV <- CACHE$rivers
HAS_RIVERS <- inherits(RIV, "sf") && nrow(RIV) > 0
RIV_MAXORD <- if (HAS_RIVERS) max(RIV$ord_flow, na.rm = TRUE) else 5L
RIV_ATTR <- paste("HydroRIVERS v1.0 -- Lehner, B., Grill, G. (2013),",
                  "Hydrological Processes 27:2171-2186, www.hydrosheds.org")

# A hard ceiling on how many reaches leave for the browser. Leaflet will accept
# two hundred thousand polylines and then stop responding; refusing to send them
# and SAYING SO is better than a map that silently dies. When the ceiling binds,
# the reaches kept are the largest ones -- dropping at random would thin the
# network everywhere instead of coarsening it, which reads as missing data.
RIV_CAP <- 25000L

HYDRO_CHOICES <- c("Aucun" = "none", "Esri (tuiles raster)" = "esri")
if (HAS_RIVERS) HYDRO_CHOICES <- c(HYDRO_CHOICES,
                                   "HydroRIVERS (cache local)" = "local")

# --- specimen panel: landmarks, segments, photographs ------------------------
#
# The drawing conventions below are those of launch_fishmorph_digitizer(), and
# they are DUPLICATED here rather than imported. Same reason FishInTrait
# duplicates intraitR's cloud diameter: the digitizer's constants are internal,
# an app directory has to stay runnable with shiny::runApp() on its own, and a
# read-only viewer that reached into another app's internals would break every
# time that app was refactored. The price is that the two must be changed
# together, which is why the segment table names the digitizer explicitly.
LM <- CACHE$landmarks
HAS_LM <- is.list(LM) && length(LM$species_key) > 0
LM_ROW <- if (HAS_LM) stats::setNames(seq_along(LM$species_key), LM$species_key) else NULL
# Raw segments, one table per campaign. The segment campaign contributes the
# published Global_segments sheet; the landmark campaign contributes the same
# eleven distances RECOMPUTED from the coordinates, so that the two columns of
# the panel are two measurements of one fish rather than a table and a table.
LM_SEG <- if (HAS_LM) LM$segments else NULL
LM_SEG_ROW <- if (!is.null(LM_SEG))
  stats::setNames(seq_len(nrow(LM_SEG)), LM_SEG$species_key) else NULL
LM_SEG_LM <- if (is.list(LM)) LM$segments_lm else NULL   # matrix, rownames = key
# Per-ratio "measured on a photograph" masks, one per campaign, built at cache
# time (see prepare_fishmorph_basins). TRUE / FALSE / NA, the third state being
# species the source does not cover at all.
LM_MEAS <- list(segment = if (is.list(LM)) LM$measured else NULL,
                landmark = if (is.list(LM)) LM$measured_lm else NULL)

# Which segments each ratio is built from (Brosse et al. 2021, Table 1), so the
# panel can name the reason a ratio is flagged as estimated.
# The confrontation of the two campaigns, computed once by
# prepare_fishmorph_basins() through Rfishmorph::compare_segments_landmarks().
# It carries the ONE thing the rest of the app deliberately does not have: a
# shared ordination. Comparing two configurations means superimposing them, and
# two independent PCAs cannot be superimposed.
CMP <- CACHE$comparison
HAS_CMP <- is.list(CMP) && !is.null(CMP$ratios) && nrow(CMP$ratios) > 0
CMP_ID <- if (HAS_CMP) CMP$id_col %||% "species_key" else NULL

# Same normalisation as .fmb_key() in the package, with the dot added: the
# comparison tables are keyed on species_key, but $space_shift may carry the
# published "Genus.species" spelling, and one form must reach the register.
.fmb_norm_key <- function(x)
  tolower(gsub("[ _.]+", " ", trimws(as.character(x))))

RATIO_DEF <- list(
  BEl = c("Bl", "Bd"),  VEp = c("Eh", "Bd"),  REs = c("Ed", "Hd"),
  OGp = c("Mo", "Bd"),  RMl = c("Jl", "Hd"),  BLs = c("Hd", "Bd"),
  PFv = c("PFi", "Bd"), PFs = c("PFl", "Bl"), CPt = c("CFd", "CPd"))

# Four labels, three colours. A missing SEGMENT and an imputed RATIO are the
# same fact seen at two levels -- the segment was not measured, so the ratio
# built on it had to be estimated -- and they share the red; but calling an
# absent segment "impute" would say the segment itself was estimated, which it
# was not. The vocabulary follows what each table actually holds.
# Two readings of the same two tables, and they cannot share the cells: an
# ECART is a property of the pair, an ORIGINE a property of each side. A
# selector says which one carries the colour, so that neither is inferred from
# a hue that means something else.
#
# The origin vocabulary keeps its earlier meaning. "impute" here says: this
# campaign has no raw measurement, so the value the ORDINATION used was filled
# in -- the comparison cell itself stays a dash, since no invented number is
# printed. "FishBase" is for MBl and MBw, which are species attributes and
# never a measurement on the picture.
STATUS_LV  <- c("mesure", "impute", "non mesure", "inconnu", "FishBase")
STATUS_COL <- c("#15803d", "#b91c1c", "#b91c1c", "#6b7280", "#1d4ed8")

# Landmark pairs measuring each body segment (digitizer's .FM_PAIR_SEG).
PAIR_SEG <- list(
  Bl  = c(1, 2),  Bd  = c(3, 4),   Hd  = c(5, 6),   Eh  = c(7, 8),
  Mo  = c(1, 9),  PFi = c(10, 11), PFl = c(10, 12), Ed  = c(13, 14),
  Jl  = c(1, 15), CPd = c(16, 17), CFd = c(18, 19))
HINGES <- c(22L, 24L, 25L)

TRAIT_LAB <- c(
  BEl = "Elongation du corps (Bl/Bd)",
  VEp = "Position verticale de l'oeil (Eh/Bd)",
  REs = "Taille relative de l'oeil (Ed/Hd)",
  OGp = "Position de la bouche (Mo/Bd)",
  RMl = "Longueur relative du maxillaire (Jl/Hd)",
  BLs = "Forme laterale du corps (Hd/Bd)",
  PFv = "Position verticale de la pectorale (PFi/Bd)",
  PFs = "Taille de la pectorale (PFl/Bl)",
  CPt = "Etranglement du pedoncule caudal (CFd/CPd)",
  MBl = "Longueur maximale du corps (cm, FishBase)",
  MBw = "Masse maximale du corps (g, FishBase)")

# The variables the ordination ACTUALLY ran on, read from the cache rather than
# hard-coded: whether size joined the nine ratios is a choice made at
# preparation time, and a panel that listed a fixed set would show a column the
# space does not contain, or hide one it does.
TRAITS <- SET$vars %||% names(TRAIT_LAB)[1:9]
SIZE_TRAITS <- intersect(SET$size_traits %||% character(0), TRAITS)


# `x[["name"]]` on an atomic vector ERRORS when the name is absent, while
# `x["name"]` returns NA. Every lookup below is on a key that may legitimately
# be missing -- a species without landmarks, without a photograph, without a
# row in the register -- so they all go through this.
at <- function(x, k) {
  if (is.null(x) || is.null(k) || !length(k) || !nzchar(k)) return(NA_integer_)
  unname(x[k])
}

PHOTO_DIR <- getOption("Rfishmorph.basin_photos", NULL)
HAS_PHOTO_DIR <- !is.null(PHOTO_DIR) && nzchar(PHOTO_DIR) && dir.exists(PHOTO_DIR)
HAS_JPEG <- requireNamespace("jpeg", quietly = TRUE)
HAS_PNG  <- requireNamespace("png", quietly = TRUE)

# Normalisation of a photograph file name onto a species key. Kept identical to
# the digitizer's .fm_photo_index(): a trailing " 1", a "_profile" suffix and
# any separator are removed, because that is how the folder is actually named
# and a stricter rule would silently lose several hundred pictures.
.fmb_photo_key <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("\\s*\\d+$", "", x)
  x <- gsub("_(profile|dessin|dessous|dessus|photo)$", "", x, ignore.case = TRUE)
  x <- gsub("[^A-Za-z]+", "_", x)
  tolower(gsub("^_|_$", "", x))
}

# The extension of a photograph in this collection is not evidence of its
# format: about 7 % of the ".jpg" files are in fact PNG, GIF or BMP (the same
# observation drives read_img() in digitizer-app.R). Choosing the reader from
# the file NAME therefore fails on one file in fourteen, and fails silently --
# the panel shows landmarks on a blank background and looks like a species
# without a photograph. The format is read from the magic bytes instead.
read_photo <- function(f) {
  sig <- tryCatch(readBin(f, "raw", n = 8L), error = function(e) raw(0))
  is_jpeg <- length(sig) >= 2 && sig[1] == as.raw(0xFF) && sig[2] == as.raw(0xD8)
  is_png <- length(sig) >= 8 &&
    all(sig[1:8] == as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)))
  tryCatch({
    if (is_jpeg && HAS_JPEG) return(jpeg::readJPEG(f))
    if (is_png && HAS_PNG) return(png::readPNG(f))
    if (requireNamespace("magick", quietly = TRUE) && HAS_PNG) {
      tmp <- tempfile(fileext = ".png")
      on.exit(unlink(tmp), add = TRUE)
      magick::image_write(magick::image_read(f), tmp, format = "png")
      return(png::readPNG(tmp))
    }
    NULL
  }, error = function(e) NULL)
}

PHOTOS <- if (HAS_PHOTO_DIR) {
  f <- list.files(PHOTO_DIR, pattern = "\\.(jpg|jpeg|png)$",
                  full.names = TRUE, ignore.case = TRUE)
  k <- vapply(f, .fmb_photo_key, character(1), USE.NAMES = FALSE)
  stats::setNames(f[!duplicated(k)], k[!duplicated(k)])
} else character(0)

INDEX_COLS <- c("FRic", "FDiv", "FDis", "FEve", "Rao")

IUCN_COLS <- c(LC = "#60B347", NT = "#CCE226", VU = "#F9A825", EN = "#E65100",
               CR = "#B71C1C", EW = "#7B1FA2", EX = "#212121", DD = "#9E9E9E")

THEME <- bslib::bs_theme(version = 5, primary = "#0f766e",
                         base_font = bslib::font_google("Inter"),
                         heading_font = bslib::font_google("Inter"))

card_x <- function(title, ...) card(card_header(title), card_body(..., gap = "8px"))
hint <- function(...) tags$p(style = "color:#6b7280;font-size:12px;margin:2px 0;", ...)
# An axis label carries the variance ITS OWN campaign explains. "PC1 (34 %)" is
# not a property of the number 1: the two campaigns are ordinated separately, so
# the same axis name covers two different axes and two different shares.
ax_label <- function(a, ts) {
  i <- as.integer(sub("PC", "", a))
  if (is.na(i) || i > length(ts$var_explained)) return(a)
  sprintf("%s (%.1f %%)", a, 100 * ts$var_explained[i])
}
fmt <- function(x, d = 3) {
  if (!length(x) || all(!is.finite(x))) return("—")
  ifelse(is.finite(x), formatC(x, format = "f", digits = d), "—")
}
# NA is a legitimate value everywhere in this cache -- a basin the source does
# not cover, an index an assemblage is too small to support -- so every place
# that prints a value prints it through here rather than letting "NA" reach the
# interface, where it reads as a bug rather than as an absence.
nz <- function(x, alt = "—") {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- alt
  x
}

# Convex hull of a planar cloud, closed, for drawing. The AREA reported in the
# tables is never recomputed here: it comes from the cache, on PC1-PC2. What is
# drawn on another axis pair is the hull of that pair, and the panel says so --
# a polygon whose area is not the number in the table must not look like it is.
hull_xy <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3L) return(NULL)
  h <- tryCatch(grDevices::chull(x, y), error = function(e) NULL)
  if (is.null(h) || length(h) < 3L) return(NULL)
  data.frame(x = c(x[h], x[h[1]]), y = c(y[h], y[h[1]]))
}

# =============================================================================
# UI
# =============================================================================

ui <- page_navbar(
  title = "Rfishmorph — bassins versants",
  theme = THEME, id = "nav", fillable = FALSE,

  sidebar = sidebar(
    width = 330, open = "open",

    h6("Methode de mesure", class = "text-primary fw-bold"),
    radioButtons("campaign", NULL, choices = CAMPAIGNS,
                 selected = CAMPAIGNS[[1]]),
    hint("Chaque campagne a son ORDINATION PROPRE : les axes, les scores et ",
         "les indices changent avec elle, et un FRic de 12 en segments n'est ",
         "pas un FRic de 12 en landmarks. Ce qui se compare d'une campagne a ",
         "l'autre est le classement des bassins et le signe d'un contraste, ",
         "pas la valeur d'un indice."),

    hr(),
    h6("Source des occurrences", class = "text-primary fw-bold"),
    radioButtons("src", NULL, choices = SOURCES, selected = SOURCES[[1]]),
    conditionalPanel(
      condition = sprintf("[%s].indexOf(input.src) >= 0",
                          paste0("'", names(HAS_STAT)[HAS_STAT], "'",
                                 collapse = ",")),
      radioButtons("status", "Statut retenu",
                   c("Toutes les especes" = "all", "Natives seules" = "native",
                     "Exotiques seules" = "exotic"), selected = "all")
    ),
    hint("Les indices sont precalcules pour chaque statut : changer de filtre ",
         "ne relance aucun calcul."),

    hr(),
    h6("Selection manuelle", class = "text-primary fw-bold"),
    # The four taxonomic boxes are stacked in the order of the classification
    # and COUPLED: see the observers in the server, which restrict each level to
    # what the level above allows and fill the levels above from what is chosen
    # below. Stacked in any other order they would read as four independent
    # filters, which is exactly what they are not.
    # The GENUS is the level the register does not store as such -- it is the
    # first word of the binomial -- but it is the one a morphological question
    # is most often asked at: "does this genus occupy one region of the space or
    # scatter across it" was, until now, a family selection followed by reading
    # the names one by one in the composition table.
    selectizeInput("f_order", "1. Ordre(s)", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Ordre taxonomique…",
                                  maxOptions = 200)),
    selectizeInput("f_family", "2. Famille(s)", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Famille taxonomique…",
                                  maxOptions = 200)),
    selectizeInput("f_genus", "3. Genre(s)", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Genre…",
                                  maxOptions = 200)),
    selectizeInput("f_species", "4. Espece(s)", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Nom scientifique…",
                                  maxOptions = 200)),
    uiOutput("tax_path"),
    # `input.f_order` is NULL, not an empty array, while nothing is selected:
    # reading `.length` off it directly would throw in the browser and freeze
    # the panel in whatever state it was last drawn in.
    conditionalPanel(
      condition = paste("(input.f_order || []).length > 0 ||",
                        "(input.f_family || []).length > 0 ||",
                        "(input.f_genus || []).length > 0"),
      checkboxInput("tax_restrict",
                    "Restreindre l'assemblage au taxon", TRUE),
      hint("Cochee, la composition, l'espace fonctionnel et les indices ne ",
           "portent que sur les especes du taxon presentes dans les bassins ",
           "allumes — les indices decrivent alors CE clade, et ne sont pas ",
           "ceux du tableau, qui porte sur l'assemblage entier. Decochee, ",
           "l'assemblage complet reste affiche et le taxon y est surligne.")
    ),
    selectizeInput("f_basin", "Bassin(s)", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Nom de bassin…",
                                  maxOptions = 200)),
    selectizeInput("f_country", "Pays", choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Pays…")),
    selectizeInput("f_eco", "Ecoregion (royaume biogeographique)",
                   choices = NULL, multiple = TRUE,
                   options = list(placeholder = "Ecoregion…")),
    radioButtons("combine", "Combinaison des criteres",
                 c("Intersection (ET)" = "and", "Union (OU)" = "or"), "and",
                 inline = TRUE),
    checkboxInput("fit_sel", "Recadrer la carte sur la selection", TRUE),
    hint("Des qu'un critere est actif, la carte ne montre QUE les bassins ",
         "retenus ; l'espace fonctionnel les met en commun. Un clic sur un ",
         "polygone focalise ce bassin seul. L'echelle de couleur reste ",
         "ajustee sur tous les bassins, pour qu'un bassin garde la meme ",
         "couleur quel que soit le filtre."),
    actionButton("reset", "Reinitialiser la selection",
                 class = "btn-sm btn-outline-secondary w-100"),

    hr(),
    # In the sidebar rather than in one panel's header: it drives the specimen
    # tables of tab 2 AND the outlier table of tab 4, and a control that steers
    # a page from another page's header is a control nobody finds.
    h6("Ecart entre campagnes", class = "text-primary fw-bold"),
    sliderInput("sp_thr", "Seuil (%)", min = 2, max = 60, value = 15,
                step = 1, ticks = FALSE),
    hint("Sur les 3 326 especes mesurees deux fois, l'ecart median vaut 4,9 % ",
         "pour les rapports (q75 = 12,3 %, q90 = 26,5 %) et 2,9 % pour les ",
         "segments : 15 % signale a peu pres le quart le plus divergent."),

    hr(),
    uiOutput("sel_info")
  ),

  # --- 1. carte --------------------------------------------------------------
  nav_panel(
    "1. Carte mondiale",
    layout_columns(
      col_widths = 12,
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
              span("Bassins versants"),
              div(class = "d-flex gap-2 align-items-center flex-wrap",
                  selectInput("map_var", NULL, choices = MAP_VARS,
                              selected = "n_species", width = "250px"),
                  conditionalPanel(
                  condition = sprintf("[%s].indexOf(input.map_var) >= 0",
                                      paste0("'", names(FIXED_DOMAIN), "'",
                                             collapse = ",")),
                  selectInput("map_trans", NULL,
                              c("Rampe racine" = "sqrt",
                                "Rampe lineaire" = "linear",
                                "Rampe log" = "log"),
                              selected = "sqrt", width = "170px")),
                selectInput("map_style", NULL,
                              choices = c("Remplissage (choroplethe)" = "fill",
                                          "Contours seuls" = "outline"),
                              selected = "fill", width = "180px"),
                  selectInput("map_tiles", NULL, choices = TILES,
                              selected = "CartoDB.Positron", width = "210px"),
                  selectInput("hydro", NULL, choices = HYDRO_CHOICES,
                              selected = if (HAS_RIVERS) "local" else "none",
                              width = "200px"),
                  conditionalPanel(
                    condition = "input.hydro == 'local'",
                    sliderInput("riv_order", NULL, min = 1,
                                max = RIV_MAXORD,
                                value = min(4L, RIV_MAXORD), step = 1,
                                width = "160px", ticks = FALSE))))
        ),
        leafletOutput("map", height = "600px"),
        card_footer(uiOutput("map_note"))
      )
    ),
    layout_columns(
      col_widths = c(4, 8),
      card_x("Bassin focalise", uiOutput("focus_box")),
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Composition specifique"),
              downloadButton("dl_compo", "CSV",
                             class = "btn-sm btn-outline-primary"))),
        DTOutput("compo_tab", height = "340px")
      )
    )
  ),

  # --- 2. espace fonctionnel -------------------------------------------------
  nav_panel(
    "2. Espace fonctionnel",
    layout_columns(
      col_widths = c(9, 3),
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Espace morphologique FISHMORPH"),
              div(class = "d-flex gap-2",
                  selectInput("ax", "X", choices = NULL, width = "130px"),
                  selectInput("ay", "Y", choices = NULL, width = "130px"),
                  selectInput("colour_by", "Couleur",
                              c("Ordre" = "order", "Statut IUCN" = "iucn",
                                "Aucune" = "none"),
                              selected = "order", width = "150px")))
        ),
        plotlyOutput("space", height = "620px"),
        card_footer(uiOutput("space_note"))
      ),
      div(
        card_x("Assemblage represente", uiOutput("space_summary")),
        card_x("Options",
               checkboxInput("bg_points", "Nuage FISHMORPH complet en fond",
                             TRUE),
               checkboxInput("bg_contour", "Contour de densite du nuage global",
                             FALSE),
               checkboxInput("show_hull", "Polygone convexe de l'assemblage",
                             TRUE),
               checkboxInput("show_arrows", "Fleches des traits", FALSE),
               sliderInput("pt_size", "Taille des points", 3, 12, 6, 1))
      )
    ),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            span("Specimen — photographie et mesures (lecture seule)"),
            div(class = "d-flex gap-2 align-items-center flex-wrap",
                actionButton("sp_prev", "‹",
                             class = "btn-sm btn-outline-secondary"),
                selectizeInput("sp_focus", NULL, choices = NULL,
                               width = "300px"),
                actionButton("sp_next", "›",
                             class = "btn-sm btn-outline-secondary"),
                checkboxInput("sp_lines", "Reperes", TRUE),
                checkboxInput("sp_labels", "Numeros", FALSE),
                radioButtons("sp_colour", "Colorer par",
                             c("Ecart" = "gap", "Origine" = "origin"),
                             selected = "gap", inline = TRUE)))
      ),
      layout_columns(
        col_widths = c(7, 5),
        plotOutput("sp_photo", height = "440px"),
        # No fixed height on the DT outputs. A DTOutput given one clips its
        # container without shrinking the table, so a nine-row table inside
        # 260 px is drawn straight over whatever follows it. The scrolling is
        # delegated to DT itself (scrollY), which sizes the container to match.
        div(class = "d-flex flex-column gap-3",
            uiOutput("sp_head"),
            DTOutput("sp_ratios"),
            DTOutput("sp_segments"))
      ),
      card_footer(uiOutput("sp_note"))
    )
  ),

  # --- 3. tableau ------------------------------------------------------------
  nav_panel(
    "3. Tableau des bassins",
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span("Un bassin par ligne — cliquer une ligne la focalise"),
            div(class = "d-flex gap-3 align-items-center",
                checkboxInput("tab_restrict",
                              "Restreindre à la sélection", FALSE),
                downloadButton("dl_table", "Telecharger (CSV)",
                               class = "btn-sm btn-outline-primary")))
      ),
      DTOutput("tab", height = "700px"),
      card_footer(uiOutput("tab_note"))
    )
  ),

  # --- 4. comparaison --------------------------------------------------------
  nav_panel(
    "4. Comparaison",
    card(
      card_header("Concordance des deux campagnes"),
      uiOutput("cmp_head")
    ),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            span("Les deux nuages dans l'ordination PARTAGEE"),
            div(class = "d-flex gap-2 align-items-center",
                selectInput("cmp_colour", NULL,
                            c("Sans couleur" = "none", "Ordre" = "group"),
                            selected = "group", width = "160px"),
                checkboxInput("cmp_link",
                              "ReliER les paires par un segment", FALSE)))
      ),
      plotlyOutput("cmp_space", height = "560px"),
      card_footer(uiOutput("cmp_space_note"))
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
              span("Biplots methode contre methode"),
              radioButtons("cmp_what", NULL,
                           c("Rapports" = "ratios",
                             "Segments (/ Bl)" = "segments"),
                           selected = "ratios", inline = TRUE))
        ),
        plotOutput("cmp_biplots", height = "560px"),
        card_footer(uiOutput("cmp_biplot_note"))
      ),
      div(
        card(card_header("Accord par variable"),
             DTOutput("cmp_metrics")),
        card(card_header("Especes les plus deplacees — cliquer pour l'onglet 2"),
             DTOutput("cmp_shift"))
      )
    ),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            span("Qui fait les points aberrants — une ligne par espece ET par variable"),
            div(class = "d-flex gap-2 align-items-center",
                checkboxInput("cmp_out_only",
                              "Seulement au-dela du seuil", TRUE),
                downloadButton("dl_cmp_out", "CSV",
                               class = "btn-sm btn-outline-primary")))
      ),
      DTOutput("cmp_outliers"),
      card_footer(uiOutput("cmp_out_note"))
    )
  ),

  nav_spacer(),
  nav_item(uiOutput("cache_stamp"))
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # --- populating the heavy selectors ---------------------------------------
  # Species and basins are tens of thousands of choices: sent to the browser in
  # full they freeze the sidebar, so selectize is fed server-side and filters
  # on the server as the user types.
  # The species box is NOT filled here. It is filled by the observer on
  # spe_pool() further down, which runs once at startup with an empty taxonomic
  # selection -- i.e. with the whole register -- and again whenever the levels
  # above narrow it. Filling it here as well would send fifteen thousand
  # choices to the browser twice, and put the pool in two places at once.
  #
  # Orders and families are HUNDREDS of choices, not tens of thousands: they go
  # to the browser in full, deliberately. A server-side selectize only knows the
  # options it has been asked for, so setting a selection on one -- which the
  # upward coupling below does constantly -- would leave the box holding a value
  # it cannot display. Client-side, the option always exists.
  updateSelectizeInput(session, "f_order", choices = .tax_choices(ORD_ROWS))
  updateSelectizeInput(session, "f_family", choices = .tax_choices(FAM_ROWS))
  # The genus box goes to the browser in full too, for the reason just given:
  # the upward coupling from the species SETS a genus, and a server-side
  # selectize would then hold a value it has never been sent and cannot draw.
  # Under two thousand options, this is a few tens of kilobytes -- the species
  # register, five times larger, is the one that has to stay server-side.
  updateSelectizeInput(session, "f_genus", choices = .tax_choices(GEN_ROWS))
  updateSelectizeInput(session, "f_basin",
                       choices = stats::setNames(BAS$basin_id, BAS$basin),
                       server = TRUE)
  updateSelectizeInput(session, "f_country",
                       choices = sort(unique(CTRY$country)), server = TRUE)
  updateSelectizeInput(session, "f_eco",
                       choices = sort(unique(stats::na.omit(BAS$ecoregion))),
                       server = TRUE)
  # --- the active measurement campaign --------------------------------------
  camp <- reactive(input$campaign %||% names(TRAITSETS)[1])
  TS <- reactive(TRAITSETS[[camp()]])
  axes_of <- reactive(colnames(TS()$scores))

  # The axis selectors are rebuilt when the campaign changes, because the
  # variance shares in their labels belong to that campaign. The chosen axis
  # NUMBERS are kept -- PC1 stays PC1 -- while making clear, through the label,
  # that it is not the same PC1.
  observe({
    ax <- axes_of()
    lab <- vapply(ax, ax_label, "", ts = TS())
    cur_x <- isolate(input$ax); cur_y <- isolate(input$ay)
    updateSelectInput(session, "ax", choices = stats::setNames(ax, lab),
                      selected = if (!is.null(cur_x) && cur_x %in% ax) cur_x
                                 else ax[1])
    updateSelectInput(session, "ay", choices = stats::setNames(ax, lab),
                      selected = if (!is.null(cur_y) && cur_y %in% ax) cur_y
                                 else ax[min(2, length(ax))])
  })

  focus <- reactiveVal(NULL)       # a single basin_id, or NULL

  observeEvent(input$reset, {
    updateSelectizeInput(session, "f_species", selected = character(0))
    # Choices, not only the selection: the two coupled boxes were narrowed by
    # the selection being cleared, and a reset that left them narrowed would
    # leave the sidebar in a state no filter explains.
    updateSelectizeInput(session, "f_order", choices = .tax_choices(ORD_ROWS),
                         selected = character(0))
    updateSelectizeInput(session, "f_family", choices = .tax_choices(FAM_ROWS),
                         selected = character(0))
    updateSelectizeInput(session, "f_genus", choices = .tax_choices(GEN_ROWS),
                         selected = character(0))
    updateSelectizeInput(session, "f_basin", selected = character(0))
    updateSelectizeInput(session, "f_country", selected = character(0))
    updateSelectizeInput(session, "f_eco", selected = character(0))
    focus(NULL)
  })

  # --- the current source ----------------------------------------------------
  src <- reactive(input$src %||% names(OCC)[1])
  status <- reactive({
    s <- input$status %||% "all"
    if (isTRUE(HAS_STAT[[src()]])) s else "all"
  })

  # Index table of the current source and status, aligned on BAS: one row per
  # basin, NA where the source records nothing. Aligning here rather than
  # merging in each output is what lets the map, the table and the summary read
  # the same object and so agree by construction.
  stats_now <- reactive({
    d <- IDX[[src()]]
    d <- d[d$campaign == camp() & d$status == status(), , drop = FALSE]
    m <- match(BAS$basin_id, d$basin_id)
    out <- BAS[, c("basin_id", "basin", "country", "ecoregion", "lon", "lat")]
    for (cl in c("n_species", "n_native", "n_exotic", "n_traits", "coverage",
                 "FRic_pct", INDEX_COLS))
      out[[cl]] <- d[[cl]][m]
    out$n_species[is.na(out$n_species)] <- 0L
    out
  })

  occ_now <- reactive({
    o <- OCC[[src()]]
    if (identical(status(), "all")) return(o)
    o[o$status %in% status(), , drop = FALSE]
  })

  by_basin_now <- reactive({
    if (identical(status(), "all")) return(BY_BASIN[[src()]])
    split(occ_now()$row, occ_now()$basin_id)
  })

  # --- the taxonomic hierarchy: ordre > famille > espece ----------------------
  #
  # DOWNWARD, each level offers only what the level above it allows. UPWARD,
  # choosing a family or a species selects the taxa it belongs to, so that the
  # three boxes always read as one path through the classification instead of
  # three filters that happen to be stacked.
  #
  # The two directions form a cycle, and the only thing that keeps it from
  # oscillating is that every observer below compares its target state to the
  # current one and updates ONLY on a difference. Nothing here may be rewritten
  # as an unconditional update.

  # Families compatible with the selected orders -- all of them when no order is
  # selected, since an empty level is the absence of a constraint, not an empty
  # set. Families whose parent order is unknown are kept only in that case:
  # under an order filter they cannot be shown as belonging to it.
  fam_pool <- reactive({
    o <- input$f_order
    if (!length(o)) names(FAM_ROWS)
    else intersect(names(FAM_ROWS), unlist(FAM_BY_ORD[o], use.names = FALSE))
  })

  # Genera compatible with the levels above. The family is read first when it
  # is set, and the order only otherwise -- the same "most precise wins" rule as
  # everywhere below. Going through the species rows for the order rather than
  # through GEN_BY_FAM keeps a genus whose family is unknown visible under its
  # order, which the family index cannot express.
  gen_pool <- reactive({
    if (length(input$f_family))
      intersect(names(GEN_ROWS),
                unlist(GEN_BY_FAM[input$f_family], use.names = FALSE))
    else if (length(input$f_order))
      intersect(names(GEN_ROWS),
                unique(SPE$genus[unlist(ORD_ROWS[input$f_order],
                                        use.names = FALSE)]))
    else names(GEN_ROWS)
  })

  # Species rows compatible with the levels above, the MOST PRECISE one winning:
  # a genus selection already lies inside the selected families, which already
  # lie inside the selected orders, so reading the upper levels too would only
  # widen what the lower one just narrowed.
  spe_pool <- reactive({
    if (length(input$f_genus))
      sort(unique(unlist(GEN_ROWS[input$f_genus], use.names = FALSE)))
    else if (length(input$f_family))
      sort(unique(unlist(FAM_ROWS[input$f_family], use.names = FALSE)))
    else if (length(input$f_order))
      sort(unique(unlist(ORD_ROWS[input$f_order], use.names = FALSE)))
    else seq_len(nrow(SPE))
  })

  # Downward, order -> family.
  observeEvent(input$f_order, ignoreNULL = FALSE, ignoreInit = TRUE, {
    fp <- fam_pool()
    keep <- intersect(isolate(input$f_family), fp)
    updateSelectizeInput(session, "f_family", choices = .tax_choices(FAM_ROWS[fp]),
                         selected = keep)
  })

  # Downward, (order, family) -> genus. On the pool and not on the inputs, for
  # the same reason as the species observer below.
  observe({
    gp <- gen_pool()
    keep <- intersect(isolate(input$f_genus), gp)
    updateSelectizeInput(session, "f_genus", choices = .tax_choices(GEN_ROWS[gp]),
                         selected = keep)
  })

  # Downward, (order, family, genus) -> species. Written as one observe on the
  # pool rather than as observers on the inputs: emptying the genus box changes
  # the species pool without changing the family box, and an observer watching
  # the inputs would miss exactly that case.
  observe({
    sp <- spe_pool()
    keep <- intersect(isolate(input$f_species), SPE$species_key[sp])
    updateSelectizeInput(
      session, "f_species",
      choices = stats::setNames(SPE$species_key[sp], SPE$species[sp]),
      selected = keep, server = TRUE)
  })

  # Upward, family -> order. The parent is ADDED, never substituted: a selection
  # the user made at another level is theirs, not the app's, to remove.
  observeEvent(input$f_family, ignoreNULL = FALSE, ignoreInit = TRUE, {
    fam <- input$f_family
    if (!length(fam)) return()
    par <- unique(unname(ORD_OF_FAM[fam]))
    par <- par[!is.na(par)]
    cur <- isolate(input$f_order)
    if (length(setdiff(par, cur)))
      updateSelectizeInput(session, "f_order", choices = .tax_choices(ORD_ROWS),
                           selected = union(cur, par))
  })

  # Upward, genus -> family, which then carries on to the order through the
  # observer above.
  observeEvent(input$f_genus, ignoreNULL = FALSE, ignoreInit = TRUE, {
    gen <- input$f_genus
    if (!length(gen)) return()
    par <- unique(unname(FAM_OF_GEN[gen]))
    par <- par[!is.na(par)]
    cur <- isolate(input$f_family)
    if (length(setdiff(par, cur))) {
      fp <- union(isolate(fam_pool()), par)
      updateSelectizeInput(session, "f_family",
                           choices = .tax_choices(FAM_ROWS[fp]),
                           selected = union(cur, par))
    }
  })

  # Upward, species -> genus, which then carries on to the family and to the
  # order through the observers above. One hop at a time, so that each rule --
  # "a genus implies its family", "a family implies its order" -- is written
  # once and only once.
  observeEvent(input$f_species, ignoreNULL = FALSE, ignoreInit = TRUE, {
    k <- input$f_species
    if (!length(k)) return()
    r <- unname(SROW[k]); r <- r[!is.na(r)]
    gen <- intersect(unique(as.character(SPE$genus[r])), names(GEN_ROWS))
    cur <- isolate(input$f_genus)
    if (length(setdiff(gen, cur))) {
      gp <- union(isolate(gen_pool()), gen)
      updateSelectizeInput(session, "f_genus",
                           choices = .tax_choices(GEN_ROWS[gp]),
                           selected = union(cur, gen))
    }
  })

  # --- the lit set -----------------------------------------------------------
  # Each criterion produces a set of basins; `combine` says whether they are
  # intersected or united. An empty criterion contributes nothing either way --
  # it is not a set of zero basins, it is the absence of a constraint.
  #
  # The taxonomic criterion is the MOST PRECISE level selected, not the union of
  # the levels: the coupling above guarantees the family lies inside the order,
  # so uniting them would undo the narrowing the user just asked for -- picking
  # the Loricariidae would light every Siluriformes basin. Species keep their
  # own criterion, and their own highlight in the ordination.
  taxon_rows <- reactive({
    r <- if (length(input$f_genus))
      unlist(GEN_ROWS[input$f_genus], use.names = FALSE)
    else if (length(input$f_family))
      unlist(FAM_ROWS[input$f_family], use.names = FALSE)
    else if (length(input$f_order))
      unlist(ORD_ROWS[input$f_order], use.names = FALSE)
    else integer(0)
    r <- r[!is.na(r)]
    if (!length(r)) integer(0) else sort(unique(r))
  })

  # What the operative level is CALLED, for the panels that name it.
  taxon_label <- reactive({
    if (length(input$f_genus)) paste(input$f_genus, collapse = ", ")
    else if (length(input$f_family)) paste(input$f_family, collapse = ", ")
    else if (length(input$f_order)) paste(input$f_order, collapse = ", ")
    else ""
  })

  # The path itself, written under the three boxes. Coupled selectors hide what
  # they have done -- a family list silently shortened from 500 entries to 12
  # looks like a list, not like a consequence -- and this line is where the
  # narrowing is said out loud.
  output$tax_path <- renderUI({
    o <- input$f_order; f <- input$f_family
    g <- input$f_genus;  s <- input$f_species
    if (!length(o) && !length(f) && !length(g) && !length(s))
      return(hint("Les quatre niveaux sont emboites : un ordre choisi ne laisse ",
                  "que ses familles, une famille que ses genres, un genre que ",
                  "ses especes. Le niveau le plus precis commande la carte."))
    step <- function(lab, v, n)
      sprintf("%s : %s", lab,
              if (length(v)) paste(v, collapse = ", ") else sprintf("%d au choix", n))
    tagList(
      hint(paste(c(step("Ordre", o, length(ORD_ROWS)),
                   step("Famille", f, length(fam_pool())),
                   step("Genre", g, length(gen_pool())),
                   step("Espece",
                        if (length(s)) SPE$species[match(s, SPE$species_key)]
                        else NULL,
                        length(spe_pool()))),
                 collapse = " › ")),
      # The cascade has a cost: a family chosen shuts the species box on that
      # family, and reaching another clade means widening first. This link is
      # the widening, and it exists so that the cost is one click rather than
      # three deletions -- or a full reset that would also drop the basins,
      # countries and ecoregions the user is still working with.
      actionLink("tax_clear", "vider les quatre niveaux",
                 style = "font-size:12px;"))
  })

  observeEvent(input$tax_clear, {
    updateSelectizeInput(session, "f_order", choices = .tax_choices(ORD_ROWS),
                         selected = character(0))
    updateSelectizeInput(session, "f_family", choices = .tax_choices(FAM_ROWS),
                         selected = character(0))
    updateSelectizeInput(session, "f_genus", choices = .tax_choices(GEN_ROWS),
                         selected = character(0))
    updateSelectizeInput(session, "f_species", selected = character(0))
  })

  # Whether the taxon narrows the assemblage or merely marks it. Kept as a
  # reactive because four outputs ask the question and the answer must be the
  # same in all of them: an unselected taxon never restricts anything.
  tax_restrict <- reactive(length(taxon_rows()) > 0 &&
                             isTRUE(input$tax_restrict))

  lit <- reactive({
    sets <- list()
    if (length(input$f_species)) {
      bs <- BY_SPEC[[src()]][input$f_species]
      v <- unique(unlist(bs, use.names = FALSE))
      if (!identical(status(), "all")) {
        o <- occ_now()
        v <- intersect(v, unique(o$basin_id[o$species_key %in% input$f_species]))
      }
      sets$species <- v %||% character(0)
    }
    # The taxon lights every basin holding at least one of its species, under
    # the CURRENT status filter: a family that is only exotic where it occurs
    # must light nothing when "natives seules" is asked for, and reading the
    # occurrence table already filtered is what guarantees it.
    tr <- taxon_rows()
    if (length(tr)) {
      o <- occ_now()
      sets$taxon <- unique(o$basin_id[o$row %in% tr])
    }
    if (length(input$f_basin)) sets$basin <- input$f_basin
    if (length(input$f_country))
      sets$country <- unique(CTRY$basin_id[CTRY$country %in% input$f_country])
    if (length(input$f_eco))
      sets$eco <- BAS$basin_id[BAS$ecoregion %in% input$f_eco]
    if (!length(sets)) return(character(0))
    if (identical(input$combine, "or"))
      unique(unlist(sets, use.names = FALSE))
    else Reduce(intersect, sets)
  })

  active <- reactive({
    f <- focus()
    if (!is.null(f)) f else lit()
  })

  output$sel_info <- renderUI({
    l <- lit(); f <- focus()
    tr <- taxon_rows()
    tagList(
      h6("Selection", class = "text-primary fw-bold"),
      tags$p(style = "font-size:13px;margin:2px 0;",
             if (length(l)) sprintf("%d bassin(s) allumé(s)", length(l))
             else "Aucun filtre : tous les bassins."),
      if (length(tr)) tags$p(
        style = "font-size:13px;margin:2px 0;",
        sprintf("Taxon : %s — %d espèce(s), %d mesurée(s) ; assemblage %s.",
                taxon_label(), length(tr), sum(TS()$has[tr]),
                if (tax_restrict()) "restreint" else "surligné")),
      if (!is.null(f)) tags$p(
        style = "font-size:13px;margin:2px 0;font-weight:600;",
        paste("Focus :", BAS$basin[BROW[[f]]])),
      if (!is.null(f)) actionLink("clear_focus", "retirer le focus")
    )
  })
  observeEvent(input$clear_focus, focus(NULL))

  # --- 1. map ----------------------------------------------------------------
  # The widget is created once, empty, and every later change goes through a
  # proxy. Rebuilding it on each choropleth change would work and would also
  # throw the user back to the world view every time they change variable.
  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(worldCopyJump = TRUE,
                                          preferCanvas = TRUE))
    # Tiles always land in leaflet's tilePane, BELOW the polygons: a river
    # network drawn there would sit under an 85 %-opaque choropleth and be
    # invisible exactly when it is asked for. Its own pane, above the shapes,
    # is the only way to overlay it without making the polygons translucent.
    m <- addMapPane(m, "hydro", zIndex = 450)
    m <- addProviderTiles(m, "CartoDB.Positron", group = "base")
    m <- setView(m, lng = 10, lat = 20, zoom = 2)
    addScaleBar(m, position = "bottomleft")
  })

  add_hydro_tiles <- function(p)
    addTiles(p, urlTemplate = HYDRO_URL, attribution = HYDRO_ATTR,
             group = "hydro",
             options = tileOptions(pane = "hydro", opacity = 0.9,
                                   maxNativeZoom = 15))

  # Which reaches are sent to the browser. The rule follows the selection: with
  # a filter active only the reaches of the displayed basins go out, which is
  # both what the user is looking at and what keeps the count sane -- the
  # network of one basin is a few hundred lines, the network of the world is
  # hundreds of thousands.
  riv_now <- reactive({
    if (!HAS_RIVERS) return(NULL)
    ord <- input$riv_order %||% min(4L, RIV_MAXORD)
    keep <- !is.na(RIV$ord_flow) & RIV$ord_flow <= ord
    l <- lit()
    if (length(l)) keep <- keep & !is.na(RIV$basin_id) & RIV$basin_id %in% l
    d <- RIV[keep, , drop = FALSE]
    truncated <- FALSE
    if (nrow(d) > RIV_CAP) {
      # Keep the largest rivers, deterministically: order by discharge class
      # then by discharge itself. Random thinning would remove reaches
      # everywhere and make a complete network look like a patchy one.
      o <- order(d$ord_flow, -replace(d$discharge, is.na(d$discharge), -Inf))
      d <- d[o[seq_len(RIV_CAP)], , drop = FALSE]
      truncated <- TRUE
    }
    attr(d, "truncated") <- truncated
    d
  })

  add_hydro_local <- function(p) {
    d <- riv_now()
    if (is.null(d) || !nrow(d)) return(invisible(p))
    # Line width carries the discharge class, which is the only way a network
    # drawn at world scale says anything: without it the Amazon and a coastal
    # creek are the same blue thread.
    w <- pmax(0.4, 2.6 - 0.45 * (d$ord_flow - 1))
    # No hover label, and `interactive = FALSE`, deliberately. The reaches sit
    # in a pane ABOVE the polygons, so an interactive river would swallow the
    # click meant for the basin under it -- and selecting a basin is the whole
    # interaction of this map. A tooltip on a river is not worth losing that.
    addPolylines(
      p, data = d, group = "hydro", color = "#1d4ed8", weight = w,
      opacity = 0.75, smoothFactor = 1.5,
      options = pathOptions(pane = "hydro", interactive = FALSE,
                            clickable = FALSE))
  }

  draw_hydro <- function(p) {
    h <- input$hydro %||% "none"
    if (identical(h, "esri")) add_hydro_tiles(p)
    else if (identical(h, "local")) add_hydro_local(p)
    invisible(p)
  }

  observeEvent(input$map_tiles, {
    p <- leafletProxy("map")
    clearGroup(p, "base")
    clearTiles(p)
    if (!identical(input$map_tiles, "Aucun (fond blanc)"))
      addProviderTiles(p, input$map_tiles, group = "base")
    # clearTiles() takes the raster hydro layer down with the basemap, so it is
    # put back here rather than left to a separate observer that would not know
    # the basemap had just been wiped.
    draw_hydro(p)
  }, ignoreInit = TRUE)

  observe({
    p <- leafletProxy("map")
    clearGroup(p, "hydro")
    draw_hydro(p)
  })

  map_pal <- reactive({
    var <- input$map_var %||% "n_species"
    v <- stats_now()[[var]]
    fin <- v[is.finite(v)]
    if (!length(fin)) return(NULL)
    if (!is.null(FIXED_DOMAIN[[var]])) {
      # Continuous, over the whole theoretical range, whatever the data shows.
      # The transform bends the RAMP, never the numbers: the legend still runs
      # 0 to 100 % and a basin keeps its value. Hull shares are strongly
      # right-skewed -- most basins occupy a few per cent of the world's
      # morphospace and a handful occupy tens -- so a linear ramp paints almost
      # every basin the same colour, which is a property of the distribution
      # and not of the map.
      tr <- input$map_trans %||% "sqrt"
      f <- switch(tr,
                  sqrt = function(x) sqrt(pmax(x, 0) / 100),
                  log  = function(x) log1p(pmax(x, 0)) / log1p(100),
                  function(x) pmax(x, 0) / 100)
      cols <- grDevices::colorRamp(
        grDevices::hcl.colors(9, "Viridis"))(f(seq(0, 100, length.out = 256)))
      ramp <- grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], maxColorValue = 255)
      return(colorNumeric(ramp, domain = FIXED_DOMAIN[[var]],
                          na.color = "#e5e7eb"))
    }
    if (var %in% BOUNDED_VARS) {
      colorNumeric("viridis", domain = range(fin), na.color = "#e5e7eb")
    } else {
      br <- unique(stats::quantile(fin, probs = seq(0, 1, length.out = 8),
                                   na.rm = TRUE))
      if (length(br) < 3) return(colorNumeric("YlOrRd", domain = range(fin),
                                              na.color = "#e5e7eb"))
      colorBin("YlOrRd", domain = range(fin), bins = br, na.color = "#e5e7eb")
    }
  })

  # Which basins the map draws. A filter SUBSETS the layer rather than
  # highlighting it on top of the others: keeping 3,364 polygons underneath a
  # selection of twelve makes the twelve harder to see, not easier, and it is
  # the cost of every redraw for no information.
  # The colour scale is NOT rebuilt on the subset. It is fitted once on all the
  # basins the source covers, so that a basin keeps the same colour whatever
  # is selected around it -- a scale that rescales with the selection turns
  # every filter into a different, and silently incomparable, map.
  map_idx <- reactive({
    l <- lit()
    if (!length(l)) return(seq_len(nrow(BAS)))
    i <- match(l, BAS$basin_id)
    i[!is.na(i)]
  })

  observe({
    st <- stats_now()
    var <- input$map_var %||% "n_species"
    pal <- map_pal()
    keep <- map_idx()

    p0 <- leafletProxy("map")
    clearGroup(p0, "basins")
    removeControl(p0, "legend")
    if (!length(keep)) return()

    st <- st[keep, , drop = FALSE]
    v <- st[[var]]
    g <- GEO[keep, , drop = FALSE]
    g$value <- v
    g$lab <- sprintf(
      "<b>%s</b><br/>%s%s<br/>S = %s, mesurées = %s (%s)<br/>%s = %s",
      st$basin, ifelse(is.na(st$country), "", st$country),
      ifelse(is.na(st$ecoregion), "", paste0(" — ", st$ecoregion)),
      ifelse(is.na(st$n_species), "—", st$n_species),
      ifelse(is.na(st$n_traits), "—", st$n_traits),
      ifelse(is.na(st$coverage), "—",
             paste0(round(100 * st$coverage), " %")),
      names(MAP_VARS)[match(var, MAP_VARS)],
      ifelse(is.na(v), "—",
             formatC(v, format = "g", digits = 4)))

    # A filtered layer is drawn with a heavier outline: with the neighbours
    # gone there is no longer a continuous mosaic to read the shapes against,
    # and a small basin alone on an ocean tile would otherwise disappear.
    filtered <- length(lit()) > 0
    outline <- identical(input$map_style %||% "fill", "outline")

    # Outline mode keeps `fill = TRUE` with `fillOpacity = 0` rather than
    # `fill = FALSE`. The two look identical and behave differently: an unfilled
    # path is clickable on its stroke ALONE, which turns selecting a basin into
    # aiming at a one-pixel line. A transparent fill stays a hit target.
    # The variable is carried by the STROKE colour in this mode, so the map
    # still says something -- an outline map that dropped the choropleth would
    # be a different map, not a lighter one.
    p <- leafletProxy("map", data = g)
    addPolygons(
      p, layerId = ~basin_id, group = "basins",
      stroke = TRUE,
      color = if (outline) {
        if (is.null(pal)) "#334155" else ~pal(value)
      } else if (filtered) "#0f766e" else "#94a3b8",
      weight = if (outline) {
        if (filtered) 2 else 0.8
      } else if (filtered) 1.4 else 0.3,
      opacity = if (outline) 0.95 else if (filtered) 1 else 0.7,
      fill = TRUE,
      fillColor = if (is.null(pal)) "#e5e7eb" else ~pal(value),
      fillOpacity = if (outline) 0 else 0.85, smoothFactor = 1.5,
      label = lapply(g$lab, HTML),
      highlightOptions = highlightOptions(
        weight = 3, color = "#0f766e", bringToFront = TRUE,
        fillOpacity = if (outline) 0.25 else 0.85))
    # The legend is fitted on ALL the basins of the source, not on the subset:
    # see map_idx(). Its breaks therefore do not move when the filter does.
    vall <- if (!is.null(FIXED_DOMAIN[[var]]))
      seq(FIXED_DOMAIN[[var]][1], FIXED_DOMAIN[[var]][2], length.out = 101)
      else stats_now()[[var]]
    if (!is.null(pal))
      addLegend(p, "bottomright", pal = pal, values = vall[is.finite(vall)],
                title = names(MAP_VARS)[match(var, MAP_VARS)],
                opacity = 0.9, layerId = "legend", na.label = "sans donnee")
  })

  # Recentring is a separate observer, and optional: a species present in five
  # hundred basins on four continents has a bounding box that is the world, and
  # zooming to it every time a filter changes would be motion without
  # information.
  observeEvent(list(lit(), input$fit_sel), {
    if (!isTRUE(input$fit_sel)) return()
    p <- leafletProxy("map")
    keep <- map_idx()
    if (!length(lit())) {
      setView(p, lng = 10, lat = 20, zoom = 2)
      return()
    }
    if (!length(keep)) return()
    bb <- tryCatch(suppressWarnings(sf::st_bbox(GEO[keep, , drop = FALSE])),
                   error = function(e) NULL)
    if (is.null(bb) || any(!is.finite(as.numeric(bb)))) return()
    if (identical(as.numeric(bb[["xmin"]]), as.numeric(bb[["xmax"]])))
      setView(p, lng = bb[["xmin"]], lat = bb[["ymin"]], zoom = 7)
    else
      fitBounds(p, bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })

  observe({
    f <- focus()
    p <- leafletProxy("map")
    clearGroup(p, "focus")
    if (is.null(f)) return()
    g <- GEO[GEO$basin_id %in% f, , drop = FALSE]
    if (!nrow(g)) return()
    addPolygons(p, data = g, group = "focus",
                layerId = paste0("focus_", g$basin_id),
                stroke = TRUE, color = "#b91c1c", weight = 3, opacity = 1,
                fill = FALSE)
  })

  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    if (is.null(id)) return()
    id <- sub("^focus_", "", id)
    focus(if (identical(focus(), id)) NULL else id)
  })

  output$map_note <- renderUI({
    st <- stats_now()
    var <- input$map_var %||% "n_species"
    keep <- map_idx()
    filtered <- length(lit()) > 0
    v <- st[[var]][keep]
    fin <- v[is.finite(v)]
    if (filtered && !length(keep))
      return(hint("Aucun bassin ne satisfait tous les criteres. ",
                  "Essayez la combinaison par union (OU)."))
    out <- hint(sprintf(
      paste0("%s. Variable : %s, renseignée pour %d d'entre eux ",
             "(médiane %s, étendue %s – %s). %s"),
      if (filtered)
        sprintf("%d bassin(s) affiché(s) sur %d — sélection active",
                length(keep), nrow(st))
      else sprintf("%d bassins cartographiés", nrow(st)),
      names(MAP_VARS)[match(var, MAP_VARS)],
      length(fin),
      fmt(stats::median(fin)), fmt(suppressWarnings(min(fin))),
      fmt(suppressWarnings(max(fin))),
      if (!is.null(FIXED_DOMAIN[[var]]))
        sprintf(paste0("Échelle CONTINUE et FIXE de 0 à 100 %%, jamais ajustée ",
                       "sur ce qui est affiché : deux cartes de cette variable ",
                       "restent donc comparables. La part est bornée par ",
                       "construction — l'enveloppe d'un sous-ensemble est ",
                       "contenue dans celle du pool. Distribution observée : ",
                       "médiane %s %%, q90 %s %%, maximum %s %%. Elle est très ",
                       "dissymétrique, d'où la rampe racine par défaut, qui ",
                       "courbe les COULEURS sans toucher aux valeurs ni aux ",
                       "graduations. "),
                fmt(stats::median(fin), 1),
                fmt(suppressWarnings(stats::quantile(fin, 0.9, names = FALSE)), 1),
                fmt(suppressWarnings(max(fin)), 1))
      else if (identical(input$map_style %||% "fill", "outline"))
        "En mode contours, la variable est portée par la couleur du trait. "
      else
        sprintf(paste0("Les comptes et FRic sont discrétisés en classes de ",
                       "quantiles, les indices bornés dans [0, 1] gardent une ",
                       "échelle continue ; dans les deux cas l'échelle est ",
                       "ajustée sur les %d bassins de la source, pas sur la ",
                       "sélection, pour rester comparable d'un filtre à ",
                       "l'autre."), nrow(st))))
    h <- input$hydro %||% "none"
    if (identical(h, "none")) return(out)
    if (identical(h, "esri"))
      return(tagList(out, hint(
        "Réseau : Esri Hydro Reference Overlay (HydroSHEDS/WWF, GTOPO30, ",
        "SRTM, GRDC), largeur de trait proportionnelle au débit. Couche en ",
        "support de maturité chez Esri, retrait annoncé pour décembre 2026 ; ",
        "l'option HydroRIVERS ci-contre porte la même information comme ",
        "donnée locale.")))
    d <- riv_now()
    n <- if (is.null(d)) 0L else nrow(d)
    tagList(out, hint(sprintf(
      paste0("Réseau : HydroRIVERS v1.0 (Lehner & Grill 2013), %d tronçon(s) ",
             "affiché(s) jusqu'à la classe de débit ORD_FLOW %d, soit ",
             "≥ %s m3/s ; épaisseur du trait proportionnelle à la classe.%s ",
             "Délimitation INDÉPENDANTE de celle des bassins (Tedesco) : un ",
             "cours d'eau longeant une limite signale un désaccord entre deux ",
             "jeux, pas une erreur."),
      n, input$riv_order %||% 4L,
      # ORD_FLOW n <=> discharge >= 10^(6 - n) m3/s (Lehner & Grill 2013).
      format(10^(6 - (input$riv_order %||% 4L)), scientific = FALSE),
      if (isTRUE(attr(d, "truncated")))
        sprintf(" Plafond de %d atteint : seuls les plus gros cours d'eau sont envoyés au navigateur — filtrez ou baissez la classe.",
                RIV_CAP)
      else "")))
  })

  # --- composition of the focused basin --------------------------------------
  compo <- reactive({
    a <- active()
    if (!length(a)) return(NULL)
    bb <- by_basin_now()
    rows <- unique(unlist(bb[a], use.names = FALSE))
    rows <- rows[!is.na(rows)]
    # The taxonomic restriction is applied HERE, on the composition, and
    # nowhere else: the species table, the ordination, the convex hull and the
    # recomputed indices all read compo(), so narrowing it once narrows them
    # together instead of leaving a hull drawn on one set and a FRic on another.
    if (tax_restrict()) rows <- intersect(rows, taxon_rows())
    if (!length(rows)) return(NULL)
    o <- occ_now()
    o <- o[o$basin_id %in% a, , drop = FALSE]
    st <- tapply(o$status, o$row, function(z)
      paste(sort(unique(stats::na.omit(z))), collapse = "/"))
    d <- SPE[rows, c("species", "order", "family", "iucn"), drop = FALSE]
    # "Has a morphology" is now a property of the ACTIVE campaign, not of the
    # species: the same fish is measured in one campaign and not in the other,
    # and that difference is the whole point of the switch.
    d$has_traits <- TS()$has[rows]
    d$status <- unname(st[as.character(rows)])
    d$row <- rows
    d
  })

  output$focus_box <- renderUI({
    a <- active()
    st <- stats_now()
    if (!length(a))
      return(hint("Cliquez un bassin sur la carte, ou utilisez la ",
                  "sélection manuelle."))
    if (length(a) > 1) {
      sub <- st[st$basin_id %in% a, , drop = FALSE]
      return(tagList(
        tags$p(sprintf("%d bassins sélectionnés", nrow(sub))),
        tags$table(
          class = "table table-sm",
          tags$tbody(
            tags$tr(tags$td("Especes (union)"),
                    tags$td(length(unique(unlist(by_basin_now()[a]))))),
            tags$tr(tags$td("S mediane par bassin"),
                    tags$td(fmt(stats::median(sub$n_species, na.rm = TRUE), 0))),
            tags$tr(tags$td("FRic mediane"),
                    tags$td(fmt(stats::median(sub$FRic, na.rm = TRUE)))),
            tags$tr(tags$td("Couverture mediane"),
                    tags$td(fmt(stats::median(sub$coverage, na.rm = TRUE), 2)))))
      ))
    }
    r <- st[st$basin_id == a[1], , drop = FALSE]
    if (!nrow(r)) return(hint("Bassin inconnu de la source courante."))
    row3 <- function(k, v) tags$tr(tags$td(k), tags$td(v))
    tagList(
      tags$h5(r$basin),
      tags$table(class = "table table-sm", tags$tbody(
        row3("Pays", nz(r$country)),
        row3("Ecoregion", nz(r$ecoregion)),
        row3("Richesse specifique", r$n_species),
        row3("dont natives / exotiques",
             sprintf("%s / %s",
                     ifelse(is.na(r$n_native), "—", r$n_native),
                     ifelse(is.na(r$n_exotic), "—", r$n_exotic))),
        row3("Especes a morphologie", sprintf("%s (%s)",
             ifelse(is.na(r$n_traits), "—", r$n_traits),
             ifelse(is.na(r$coverage), "—",
                    paste0(round(100 * r$coverage), " %")))),
        row3("FRic (% du monde)",
             if (is.na(r$FRic_pct)) "\u2014" else
               sprintf("%s %%", fmt(r$FRic_pct, 2))),
        row3("FRic (aire)", fmt(r$FRic)), row3("FDiv", fmt(r$FDiv)),
        row3("FDis", fmt(r$FDis)), row3("FEve", fmt(r$FEve)),
        row3("Rao",  fmt(r$Rao)))),
      hint("FRic et FDiv sont mesurés dans le plan PC1-PC2 ; FDis, FEve ",
           "et Rao sur ", length(SET$axes_dist), " axes.")
    )
  })

  output$compo_tab <- renderDT({
    d <- compo()
    validate(need(!is.null(d), "Aucune espece pour cette selection."))
    d <- d[order(!d$has_traits, d$order, d$species), , drop = FALSE]
    out <- data.frame(
      Espece = d$species, Ordre = d$order, Famille = d$family,
      IUCN = d$iucn, Statut = d$status,
      Morphologie = ifelse(d$has_traits, "oui", "non"),
      stringsAsFactors = FALSE)
    datatable(out, rownames = FALSE, selection = "none",
              options = list(pageLength = 10, scrollX = TRUE, dom = "ftip"))
  })

  output$dl_compo <- downloadHandler(
    filename = function() sprintf("composition_%s.csv",
                                  format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      d <- compo()
      if (is.null(d)) d <- data.frame()
      utils::write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8")
    })

  # --- 2. functional space ---------------------------------------------------
  sel_rows <- reactive({
    d <- compo()
    if (is.null(d)) return(integer(0))
    r <- d$row[d$has_traits]
    r[!is.na(r)]
  })

  sel_species_rows <- reactive({
    if (!length(input$f_species)) return(integer(0))
    r <- unname(SROW[input$f_species])
    r <- r[!is.na(r)]
    r[TS()$has[r]]
  })

  # Species of the selected taxon, drawn as their own layer when the assemblage
  # is NOT restricted -- restricted, they are already the whole cloud and a
  # second identical layer would only double the points. When no basin filter
  # narrows the scene, the layer is the taxon over the entire FISHMORPH pool,
  # which is the "show me this family in the space" reading of the selector.
  taxon_space_rows <- reactive({
    tr <- taxon_rows()
    if (!length(tr) || tax_restrict()) return(integer(0))
    rs <- sel_rows()
    if (length(rs)) tr <- intersect(tr, rs)
    tr[TS()$has[tr]]
  })

  output$space <- renderPlotly({
    ts <- TS()
    AX <- axes_of()
    ax <- input$ax %||% AX[1]
    ay <- input$ay %||% AX[min(2, length(AX))]
    validate(need(ax %in% AX && ay %in% AX, "Axes indisponibles."))
    pool <- which(ts$has)
    X <- ts$scores[, ax]; Y <- ts$scores[, ay]

    # Every point carries its species KEY in `customdata`. A click then comes
    # back naming the fish, instead of a (curveNumber, pointNumber) pair that
    # would have to be decoded against a trace layout which changes with the
    # colouring, the filters and the hull -- and would silently point at the
    # wrong species the first time a trace was added.
    p <- plot_ly(source = "fmspace")
    if (isTRUE(input$bg_points))
      p <- add_trace(
        p, type = "scattergl", mode = "markers",
        x = X[pool], y = Y[pool], customdata = SPE$species_key[pool],
        marker = list(size = 3, color = "rgba(148,163,184,0.35)"),
        hoverinfo = "text", text = SPE$species[pool],
        name = sprintf("FISHMORPH (%d esp.)", length(pool)))

    if (isTRUE(input$bg_contour) &&
        requireNamespace("MASS", quietly = TRUE)) {
      k <- MASS::kde2d(X[pool], Y[pool], n = 120)
      p <- add_trace(p, type = "contour", x = k$x, y = k$y, z = t(k$z),
                     contours = list(coloring = "lines", showlabels = FALSE),
                     line = list(width = 1), showscale = FALSE,
                     colorscale = "Greys", hoverinfo = "skip",
                     name = "densite globale")
    }

    rs <- sel_rows()
    if (length(rs)) {
      d <- SPE[rs, , drop = FALSE]
      # Coordinates come from the ACTIVE campaign's score matrix, never from
      # the register: SPE holds identity, not position, precisely so that a
      # species cannot carry the coordinates of a campaign it was not measured
      # in.
      dx <- X[rs]; dy <- Y[rs]
      cb <- input$colour_by %||% "order"
      txt <- sprintf("%s<br>%s | %s<br>IUCN %s", d$species,
                     nz(d$order, "ordre inconnu"), nz(d$family, ""),
                     nz(d$iucn, "NE"))
      if (identical(cb, "none")) {
        p <- add_trace(p, type = "scattergl", mode = "markers",
                       x = dx, y = dy, customdata = d$species_key,
                       marker = list(size = input$pt_size %||% 6,
                                     color = "#0f766e",
                                     line = list(width = 0.5, color = "white")),
                       text = txt, hoverinfo = "text", name = "assemblage")
      } else {
        key <- if (identical(cb, "iucn")) d$iucn else d$order
        key[is.na(key) | !nzchar(key)] <- "inconnu"
        lv <- names(sort(table(key), decreasing = TRUE))
        cols <- if (identical(cb, "iucn"))
          ifelse(lv %in% names(IUCN_COLS), IUCN_COLS[lv], "#9E9E9E")
        else grDevices::hcl.colors(length(lv), "Dark 3")
        for (i in seq_along(lv)) {
          j <- key == lv[i]
          p <- add_trace(p, type = "scattergl", mode = "markers",
                         x = dx[j], y = dy[j],
                         customdata = d$species_key[j],
                         marker = list(size = input$pt_size %||% 6,
                                       color = cols[i],
                                       line = list(width = 0.5, color = "white")),
                         text = txt[j], hoverinfo = "text",
                         name = sprintf("%s (%d)", lv[i], sum(j)),
                         legendgroup = lv[i])
        }
      }
      if (isTRUE(input$show_hull)) {
        h <- hull_xy(dx, dy)
        if (!is.null(h))
          p <- add_trace(p, type = "scatter", mode = "lines",
                         x = h$x, y = h$y,
                         line = list(color = "#0f766e", width = 2, dash = "dot"),
                         hoverinfo = "skip", showlegend = TRUE,
                         name = "polygone convexe")
      }
    }

    tsr <- taxon_space_rows()
    if (length(tsr)) {
      dt <- SPE[tsr, , drop = FALSE]
      p <- add_trace(
        p, type = "scattergl", mode = "markers",
        x = X[tsr], y = Y[tsr], customdata = dt$species_key,
        marker = list(size = (input$pt_size %||% 6) + 2, color = "#d97706",
                      line = list(width = 0.8, color = "#7c2d12")),
        text = sprintf("%s<br>%s | %s", dt$species, nz(dt$order, ""),
                       nz(dt$family, "")),
        hoverinfo = "text",
        name = sprintf("%s (%d esp.)", taxon_label(), length(tsr)))
      if (isTRUE(input$show_hull)) {
        ht <- hull_xy(X[tsr], Y[tsr])
        if (!is.null(ht))
          p <- add_trace(p, type = "scatter", mode = "lines",
                         x = ht$x, y = ht$y,
                         line = list(color = "#d97706", width = 1.5,
                                     dash = "dash"),
                         hoverinfo = "skip", showlegend = TRUE,
                         name = "polygone convexe du taxon")
      }
    }

    ss <- sel_species_rows()
    if (length(ss))
      p <- add_trace(p, type = "scatter", mode = "markers",
                     x = X[ss], y = Y[ss],
                     customdata = SPE$species_key[ss],
                     marker = list(size = (input$pt_size %||% 6) + 6,
                                   symbol = "diamond", color = "#b91c1c",
                                   line = list(width = 1.2, color = "black")),
                     text = SPE$species[ss], hoverinfo = "text",
                     name = "espece(s) selectionnee(s)")

    LOAD <- ts$loadings
    if (isTRUE(input$show_arrows) && !is.null(LOAD)) {
      s <- 0.9 * max(abs(c(X[pool], Y[pool])), na.rm = TRUE)
      lx <- LOAD[, ax] * s; ly <- LOAD[, ay] * s
      for (i in seq_along(lx))
        p <- add_trace(p, type = "scatter", mode = "lines+text",
                       x = c(0, lx[i]), y = c(0, ly[i]),
                       text = c("", rownames(LOAD)[i]),
                       textposition = "top center",
                       line = list(color = "#1f2937", width = 1),
                       hoverinfo = "skip", showlegend = FALSE)
    }

    p <- layout(p,
           xaxis = list(title = ax_label(ax, ts), zeroline = TRUE,
                        zerolinecolor = "#e5e7eb"),
           yaxis = list(title = ax_label(ay, ts), zeroline = TRUE,
                        zerolinecolor = "#e5e7eb",
                        scaleanchor = "x", scaleratio = 1),
           legend = list(orientation = "v", x = 1.02, y = 1),
           hovermode = "closest",
           margin = list(l = 60, r = 20, t = 20, b = 50))
    event_register(p, "plotly_click")
  })

  # Indices of the assemblage CURRENTLY drawn. When the focus is one basin they
  # reproduce the cache exactly; when several basins are pooled they describe
  # the pool, which is a different assemblage and no row of the table.
  space_fd <- reactive({
    rs <- sel_rows()
    if (!length(rs)) return(NULL)
    Rfishmorph::fishmorph_basin_indices(
      TS()$scores[rs, , drop = FALSE],
      axes_ric = SET$axes_ric %||% 1:2,
      axes_dist = SET$axes_dist %||% seq_along(axes_of()))
  })

  output$space_summary <- renderUI({
    a <- active()
    fd <- space_fd()
    if (is.null(fd)) return(hint("Aucune espece mesuree dans cette selection."))
    d <- compo()
    tagList(
      tags$p(style = "font-size:13px;",
             if (length(a) == 1) BAS$basin[BROW[[a]]]
             else sprintf("%d bassins mis en commun", length(a))),
      tags$table(class = "table table-sm", tags$tbody(
        tags$tr(tags$td("Especes"), tags$td(nrow(d))),
        tags$tr(tags$td("dont mesurees"), tags$td(fd$n)),
        tags$tr(tags$td("FRic"), tags$td(fmt(fd$FRic))),
        tags$tr(tags$td("FDiv"), tags$td(fmt(fd$FDiv))),
        tags$tr(tags$td("FDis"), tags$td(fmt(fd$FDis))),
        tags$tr(tags$td("FEve"), tags$td(fmt(fd$FEve))),
        tags$tr(tags$td("Rao"), tags$td(fmt(fd$Rao))))),
      if (tax_restrict()) hint(
        "Assemblage RESTREINT à ", taxon_label(),
        " : ces indices décrivent ce clade dans les bassins allumés, et ne ",
        "sont comparables ni au tableau des bassins ni à une autre sélection ",
        "taxonomique — un FRic calculé sur un sous-ensemble d'espèces est ",
        "borné par celui de l'assemblage entier."),
      hint("Recalculés sur l'assemblage affiché."))
  })

  output$space_note <- renderUI({
    ts <- TS(); AX <- axes_of()
    ax <- input$ax %||% AX[1]; ay <- input$ay %||% AX[min(2, length(AX))]
    same <- identical(sort(c(ax, ay)), sort(paste0("PC", SET$axes_ric)))
    hint(sprintf(
      paste0("Campagne %s : ACP propre sur les %d rapports FISHMORPH, %s, ",
             "%d espèces mesurées (PC1-PC2 = %.1f %% de variance). Les axes ",
             "d'une campagne ne sont pas ceux de l'autre — un point ne garde ",
             "pas ses coordonnées quand on bascule. %s"),
      names(CAMPAIGNS)[match(camp(), CAMPAIGNS)],
      length(SET$traits),
      if (isTRUE(SET$scale)) "variables centrees-reduites" else "variables centrees",
      ts$n_species,
      100 * sum(ts$var_explained[seq_len(min(2, ts$n_axes))]),
      if (same)
        "Le polygone dessiné est celui dont l'aire est reportée comme FRic."
      else
        paste0("Attention : FRic des tableaux est mesuré dans le plan PC",
               SET$axes_ric[1], "-PC", SET$axes_ric[2],
               " ; le polygone dessiné ici est celui du plan affiché ",
               "et son aire n'est pas ce nombre.")))
  })

  # --- 2b. specimen panel ----------------------------------------------------
  # Read-only by construction: nothing here writes, and the drawing carries no
  # click handler. It is the digitizer's picture without the digitizer's verbs.

  # The species offered are those of the assemblage currently drawn, restricted
  # to the ones the cache can actually show something for. Offering a species
  # with neither landmarks nor photograph would put an empty frame behind a
  # name and make an absence of data look like a failure of the application.
  # ONE piece of state for "which fish is on screen", written by three
  # different gestures -- a click in the ordination, a species picked in the
  # sidebar, the panel's own selector -- and read by everything below. Letting
  # the dropdown be the state instead would make the plot click a special case
  # that has to reach into a widget, and the two would drift apart.
  sp_sel <- reactiveVal(NULL)

  # A species is worth opening when the panel can show ANYTHING about it: a
  # digitized shape, a photograph, or -- failing both -- the nine published
  # ratios. Gating on the picture alone would hide the measurements of the
  # 6,230 species of the published table that were never re-digitized, which
  # are exactly the ones a reader is most likely to want to check.
  showable <- function(k) {
    if (!length(k)) return(logical(0))
    i <- match(k, SPE$species_key)
    hs <- TS()$has
    has_ratio <- !is.na(i) & !is.na(hs[i]) & hs[i]
    (HAS_LM & k %in% names(LM_ROW)) |
      (gsub("[^a-z]+", "_", k) %in% names(PHOTOS)) | has_ratio
  }

  # Selecting a species the cache cannot illustrate must SAY so. Silently
  # falling back on another fish would answer a click about one species with a
  # picture of a different one, which is worse than showing nothing.
  show_species <- function(k) {
    if (any(showable(k))) return(sp_sel(k))
    i <- at(SROW, k)
    showNotification(
      sprintf("%s : ni landmarks ni photographie dans cette installation.",
              if (is.na(i)) k else SPE$species[i]),
      type = "warning", duration = 4)
    invisible(NULL)
  }

  sp_choices <- reactive({
    d <- compo()
    k <- if (is.null(d) || !nrow(d)) character(0) else SPE$species_key[d$row]
    k <- k[showable(k)]
    # A fish clicked outside the current assemblage is added to the list rather
    # than refused. Clicking a point and being shown nothing would be the app
    # contradicting its own drawing.
    cur <- sp_sel()
    if (!is.null(cur) && nzchar(cur) && !cur %in% k && any(showable(cur)))
      k <- c(k, cur)
    k <- unique(k)
    if (!length(k)) return(character(0))
    nm <- SPE$species[match(k, SPE$species_key)]
    o <- order(nm)
    stats::setNames(k[o], nm[o])
  })

  # Keep the dropdown showing what sp_sel() holds, and fall back to the first
  # showable species when the assemblage changes under it.
  observe({
    ch <- sp_choices()
    cur <- sp_sel()
    if (is.null(cur) || !cur %in% ch) cur <- if (length(ch)) ch[[1]] else NULL
    if (!identical(cur, isolate(sp_sel()))) sp_sel(cur)
    if (!identical(cur, isolate(input$sp_focus)))
      updateSelectizeInput(session, "sp_focus", choices = ch, selected = cur,
                           server = TRUE)
  })

  observeEvent(input$sp_focus, {
    k <- input$sp_focus
    if (!is.null(k) && nzchar(k) && !identical(k, sp_sel())) sp_sel(k)
  })

  # A click anywhere in the ordination -- background cloud included -- names a
  # species through customdata.
  observeEvent(event_data("plotly_click", source = "fmspace"), {
    e <- event_data("plotly_click", source = "fmspace")
    k <- e$customdata
    if (is.null(k) || !length(k)) return()
    k <- as.character(k)[1]
    if (is.na(k) || !nzchar(k)) return()
    show_species(k)
  })

  # Picking a species in the sidebar is the same gesture by another route: it
  # already lights up its basins and marks it in the ordination, so it should
  # show its picture too.
  observeEvent(input$f_species, {
    k <- input$f_species
    if (length(k)) show_species(k[[length(k)]])
  }, ignoreNULL = FALSE)

  step_sp <- function(by) {
    ch <- sp_choices()
    if (!length(ch)) return()
    i <- match(sp_sel() %||% ch[[1]], ch)
    if (is.na(i)) i <- 1L
    sp_sel(ch[[((i - 1L + by) %% length(ch)) + 1L]])
  }
  observeEvent(input$sp_prev, step_sp(-1L))
  observeEvent(input$sp_next, step_sp(+1L))

  # Landmarks of the selected species, in the pixel frame of ITS photograph:
  # x to the right, y DOWNWARD, exactly as digitized. Anything that redressed
  # the y axis here would put the points on the mirror image of the fish.
  sp_P <- reactive({
    k <- sp_sel()
    if (!HAS_LM || is.null(k) || !nzchar(k)) return(NULL)
    i <- at(LM_ROW, k)
    if (is.na(i)) return(NULL)
    cbind(LM$X[i, ], LM$Y[i, ])
  })

  # The photograph is read once per species and kept, so that ticking a
  # checkbox redraws from memory instead of re-decoding a 300 kB JPEG.
  sp_img <- reactiveVal(NULL)
  observeEvent(sp_sel(), {
    sp_img(NULL)
    k <- sp_sel()
    if (is.null(k) || !nzchar(k) || !length(PHOTOS)) return()
    f <- unname(PHOTOS[gsub("[^a-z]+", "_", k)])
    if (is.na(f) || !file.exists(f)) return()
    sp_img(read_photo(f))
  }, ignoreNULL = FALSE)

  output$sp_photo <- renderPlot({
    P <- sp_P()
    img <- sp_img()
    if (is.null(P) && is.null(img)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, if (!HAS_LM && !length(PHOTOS))
        paste0("Ni landmarks ni photographies dans cette installation.\n",
               "prepare_fishmorph_basins(landmarks = ...) et\n",
               "launch_fishmorph_basins(photos = ...)")
        else paste0(
          "Espece non redigitalisee et sans photographie.\n\n",
          "Ses rapports publies restent affiches a droite : la mesure\n",
          "existe, c'est le cliche et les landmarks qui manquent.\n",
          "6 230 des 9 557 lignes de la feuille Global_Landmark\n",
          "sont dans ce cas."), cex = 0.9)
      return()
    }
    # The frame is the image when there is one, and the bounding box of the
    # points padded by a tenth when there is not: a shape drawn edge to edge
    # reads as a fish that fills its photograph, which it never does.
    if (!is.null(img)) {
      w <- ncol(img); h <- nrow(img)
      xl <- c(0, w); yl <- c(h, 0)
    } else {
      rx <- range(P[, 1], na.rm = TRUE); ry <- range(P[, 2], na.rm = TRUE)
      px <- 0.1 * diff(rx); py <- 0.1 * diff(ry)
      xl <- rx + c(-px, px); yl <- rev(ry + c(-py, py))
    }
    op <- graphics::par(mar = c(0, 0, 0, 0)); on.exit(graphics::par(op))
    graphics::plot(NA, xlim = xl, ylim = yl, asp = 1, xaxs = "i", yaxs = "i",
                   axes = FALSE, xlab = "", ylab = "")
    if (!is.null(img))
      graphics::rasterImage(img, 0, nrow(img), ncol(img), 0)
    if (is.null(P)) return()

    fin <- function(i) i <= nrow(P) && all(is.finite(P[i, ]))
    path <- function(pts, ...) {
      pts <- pts[vapply(pts, fin, logical(1))]
      if (length(pts) > 1) graphics::lines(P[pts, 1], P[pts, 2], ...)
    }
    if (isTRUE(input$sp_lines)) {
      path(c(1, 5, 3, 16, 18, 19, 17, 4, 6, 1), col = "grey30", lwd = 1)
      path(c(9, 8, 11, 4), col = "grey85", lty = 3, lwd = 1)
      path(c(1, 9), col = "grey60", lwd = 1)
      if (fin(6) && fin(23))
        graphics::segments(P[23, 1], P[23, 2], P[6, 1], P[6, 2],
                           col = "magenta", lwd = 1.5)
      path(c(5, 13, 7, 14, 6, 8), col = "grey85", lty = 3, lwd = 1)
      if (fin(7) && fin(13) && fin(14)) {
        er <- sqrt(sum((P[13, ] - P[14, ])^2)) / 2
        th <- seq(0, 2 * pi, length.out = 60)
        graphics::lines(P[7, 1] + er * cos(th), P[7, 2] + er * sin(th),
                        col = "grey85", lty = 3, lwd = 1)
      }
    }
    cols <- grDevices::hcl.colors(length(PAIR_SEG), "Dark3")
    for (k in seq_along(PAIR_SEG)) {
      ab <- PAIR_SEG[[k]]
      if (names(PAIR_SEG)[k] == "Bl") {
        # Bl runs along the BROKEN axis 1 -> hinges -> 2, not along the chord:
        # on a curved fish the two differ, and it is the broken axis that was
        # measured.
        hs <- HINGES[vapply(HINGES, fin, logical(1))]
        if (length(hs) > 1 && fin(1) && fin(2)) {
          uc <- P[2, ] - P[1, ]
          hs <- hs[order(vapply(hs, function(i) sum((P[i, ] - P[1, ]) * uc),
                                numeric(1)))]
        }
        ch <- c(1L, hs, 2L)
        ch <- ch[vapply(ch, fin, logical(1))]
        if (length(ch) > 1) graphics::lines(P[ch, 1], P[ch, 2],
                                            col = cols[k], lwd = 2)
      } else if (fin(ab[1]) && fin(ab[2]))
        graphics::segments(P[ab[1], 1], P[ab[1], 2], P[ab[2], 1], P[ab[2], 2],
                           col = cols[k], lwd = 2)
    }
    lp <- which(vapply(seq_len(nrow(P)), fin, logical(1)))
    lp <- setdiff(lp, HINGES)
    if (length(lp))
      graphics::points(P[lp, 1], P[lp, 2], pch = 21, bg = "white", cex = 1.1)
    xh <- intersect(HINGES, which(vapply(seq_len(nrow(P)), fin, logical(1))))
    if (length(xh))
      graphics::points(P[xh, 1], P[xh, 2], pch = 21, bg = "gold", cex = 1.2)
    if (isTRUE(input$sp_labels) && length(lp)) {
      # Two points at the same pixel stack one label over the other, so a
      # legitimate zero segment looks like a lost point. One label per position,
      # carrying every number it holds.
      grp <- split(lp, paste(round(P[lp, 1], 1), round(P[lp, 2], 1)))
      for (g in grp)
        graphics::text(P[g[1], 1], P[g[1], 2], paste(sort(g), collapse = "+"),
                       pos = 3, cex = 0.7, font = if (length(g) > 1) 2 else 1,
                       col = if (length(g) > 1) "orange" else "yellow")
    }
    graphics::legend("topright", names(PAIR_SEG), col = cols, lwd = 2,
                     bg = "white", cex = 0.75, ncol = 2)
  })

  output$sp_head <- renderUI({
    k <- sp_sel()
    if (is.null(k) || !nzchar(k)) return(hint("Aucune espece selectionnee."))
    i <- at(SROW, k)
    if (is.na(i)) return(NULL)
    tagList(
      tags$h5(tags$em(SPE$species[i])),
      hint(sprintf("%s · %s · %s · IUCN %s", nz(SPE$order[i]),
                   nz(SPE$family[i]), nz(SPE$genus[i]),
                   nz(SPE$iucn[i], "NE"))))
  })

  # --- comparison of the two campaigns, on ONE specimen ----------------------
  # These two tables deliberately IGNORE the global campaign selector. Their
  # purpose is to put the two measurements of the same fish side by side, and a
  # comparison that showed only the active one would not be a comparison.
  #
  # The values are recomputed from the RAW measurements of each campaign, never
  # taken from the published trait tables: those have been through imputation,
  # so comparing them would compare two imputations rather than two methods.
  # Where a campaign cannot produce a quantity, the cell is a dash -- an
  # imputed number in its place would silently close a gap that is the result.

  # Symmetric relative difference, in per cent. Chosen over |a - b| / b because
  # a FISHMORPH segment is legitimately ZERO -- Mo = 0 is a terminal mouth,
  # PFi = 0 a pectoral fin inserted on the midline -- and the asymmetric form is
  # undefined there, precisely on the 954 and 737 species where the two methods
  # disagree most. Bounded at 200 %, which is what "one method says zero, the
  # other does not" reads as.
  dsym <- function(a, b) {
    if (!is.finite(a) || !is.finite(b)) return(NA_real_)
    if (a == 0 && b == 0) return(0)
    200 * abs(a - b) / (abs(a) + abs(b))
  }

  ratios_from_seg <- function(sv) {
    vapply(names(RATIO_DEF), function(r) {
      ab <- RATIO_DEF[[r]]
      a <- unname(sv[ab[1]]); b <- unname(sv[ab[2]])
      if (length(a) != 1 || length(b) != 1 || !is.finite(a) || !is.finite(b) ||
          b == 0) NA_real_ else a / b
    }, numeric(1))
  }

  # The raw segments of both campaigns for the selected specimen.
  sp_pair <- reactive({
    k <- sp_sel()
    if (is.null(k) || !nzchar(k)) return(NULL)
    i <- at(LM_SEG_ROW, k)
    sseg <- if (is.na(i)) NULL else {
      r <- LM_SEG[i, setdiff(names(LM_SEG), "species_key"), drop = FALSE]
      # `as.numeric()` on a data.frame row fails ("cannot coerce type 'list'"):
      # a one-row selection of several columns is still a list of columns.
      stats::setNames(as.numeric(unlist(r[1, ], use.names = FALSE)), names(r))
    }
    j <- if (is.matrix(LM_SEG_LM))
      at(stats::setNames(seq_len(nrow(LM_SEG_LM)), rownames(LM_SEG_LM)), k)
      else NA_integer_
    slm <- if (is.na(j)) NULL else
      stats::setNames(as.numeric(LM_SEG_LM[j, ]), colnames(LM_SEG_LM))
    if (is.null(sseg) && is.null(slm)) return(NULL)
    list(seg = sseg, lm = slm)
  })

  thr <- reactive(input$sp_thr %||% 15)

  # Colouring: the SPREAD, not the value. Two tiers, and the slider sets the
  # first. The default of 15 % sits between the third quartile and the ninth
  # decile of the observed spread on the 3,326 species measured twice (ratios:
  # median 4.9 %, q75 12.3 %, q90 26.5 %), so it flags roughly the worst
  # quarter rather than an arbitrary fraction.
  # Origin of one campaign's ratios, in the vocabulary of the earlier panel.
  # `nm` names the row set, so the size rows can be answered without a mask.
  orig_of <- function(camp_nm, k, rows) {
    MM <- LM_MEAS[[camp_nm]]
    out <- rep("inconnu", length(rows))
    if (is.matrix(MM)) {
      m <- MM[match(k, rownames(MM)), ]
      if (!all(is.na(m))) {
        j <- match(rows, colnames(MM))
        ok <- !is.na(j)
        out[ok] <- ifelse(is.na(m[j[ok]]), "inconnu",
                          ifelse(m[j[ok]], "mesure", "impute"))
      }
    }
    out[rows %in% SIZE_TRAITS] <- "FishBase"
    out
  }

  # ONE styler for both tables and both readings. Under "gap" the whole row
  # follows the spread; under "origin" each campaign's column follows its OWN
  # origin -- a single origin colour across both would claim that what is true
  # of one measurement is true of the other.
  style_cells <- function(dt, label_col) {
    if (identical(input$sp_colour %||% "gap", "origin")) {
      dt <- formatStyle(dt, columns = c(label_col, "Segments"),
                        valueColumns = "O.seg", fontWeight = "bold",
                        color = styleEqual(STATUS_LV, STATUS_COL))
      return(formatStyle(dt, columns = "Landmarks", valueColumns = "O.lm",
                         fontWeight = "bold",
                         color = styleEqual(STATUS_LV, STATUS_COL)))
    }
    t1 <- thr(); t2 <- 2 * t1
    formatStyle(dt, columns = c(label_col, "Segments", "Landmarks", "Ecart"),
                valueColumns = "Ecart",
                color = styleInterval(c(t1, t2),
                                      c("#111827", "#c2410c", "#b91c1c")),
                fontWeight = styleInterval(t1, c("normal", "bold")))
  }

  output$sp_ratios <- renderDT({
    P <- sp_pair()
    validate(need(!is.null(P), "Aucune mesure brute pour cette espece."))
    rs <- if (is.null(P$seg)) stats::setNames(rep(NA_real_, length(RATIO_DEF)),
                                              names(RATIO_DEF))
          else ratios_from_seg(P$seg)
    rl <- if (is.null(P$lm)) stats::setNames(rep(NA_real_, length(RATIO_DEF)),
                                             names(RATIO_DEF))
          else ratios_from_seg(P$lm)
    ec <- vapply(names(RATIO_DEF), function(r) dsym(rs[r], rl[r]), numeric(1))
    k <- sp_sel(); rn <- names(RATIO_DEF)
    out <- data.frame(
      Rapport = rn,
      Segments = round(unname(rs), 4),
      Landmarks = round(unname(rl), 4),
      Ecart = round(unname(ec), 1),
      O.seg = orig_of("segment", k, rn),
      O.lm  = orig_of("landmark", k, rn),
      stringsAsFactors = FALSE)
    dt <- datatable(out, rownames = FALSE, selection = "none",
                    class = "compact stripe",
                    colnames = c("Rapport", "Segments", "Landmarks", "Ecart %",
                                 "Orig. seg", "Orig. lm"),
                    options = list(dom = "t", paging = FALSE, ordering = FALSE,
                                   scrollX = TRUE, scrollY = "230px",
                                   scrollCollapse = TRUE))
    style_cells(dt, "Rapport")
  })

  output$sp_segments <- renderDT({
    P <- sp_pair()
    validate(need(!is.null(P), "Aucune mesure brute pour cette espece."))
    nms <- union(names(P$seg) %||% character(0),
                 names(P$lm) %||% character(0))
    nms <- setdiff(nms, "MaxLength")          # remonte plus bas avec MBl/MBw
    gv <- function(v, n) if (is.null(v) || !n %in% names(v)) NA_real_ else unname(v[n])
    a <- vapply(nms, function(n) gv(P$seg, n), numeric(1))
    b <- vapply(nms, function(n) gv(P$lm, n), numeric(1))
    ec <- vapply(seq_along(nms), function(i) dsym(a[i], b[i]), numeric(1))
    out <- data.frame(Mesure = nms, Unite = "px",
                      Segments = round(unname(a), 1),
                      Landmarks = round(unname(b), 1),
                      Ecart = round(ec, 1), stringsAsFactors = FALSE)

    # MBl and MBw close the table rather than the ratio one: they are raw
    # quantities in centimetres and grams, they do not depend on how the fish
    # was digitized, and putting them in a comparison of two methods would add
    # two rows whose spread is zero by construction.
    k <- sp_sel(); j <- at(SROW, k)
    if (length(SIZE_TRAITS) && !is.na(j)) {
      sv <- 10^TS()$ratios[j, SIZE_TRAITS] - 1
      out <- rbind(out, data.frame(
        Mesure = SIZE_TRAITS,
        Unite = ifelse(SIZE_TRAITS == "MBw", "g", "cm"),
        Segments = round(as.numeric(sv), 1),
        Landmarks = round(as.numeric(sv), 1),
        Ecart = NA_real_, stringsAsFactors = FALSE))
    }
    # Here the origin is read straight off the raw value: a segment present in
    # the source was measured, a segment absent was not. A ZERO is measured --
    # Mo = 0 is a terminal mouth, Bbl = 0 a fish without barbels, both
    # observations -- so the test is on NA and not on truth.
    out$O.seg <- ifelse(out$Mesure %in% SIZE_TRAITS, "FishBase",
                        ifelse(is.na(out$Segments), "non mesure", "mesure"))
    out$O.lm <- ifelse(out$Mesure %in% SIZE_TRAITS, "FishBase",
                       ifelse(is.na(out$Landmarks), "non mesure", "mesure"))
    dt <- datatable(out, rownames = FALSE, selection = "none",
                    class = "compact stripe",
                    colnames = c("Mesure", "Unite", "Segments", "Landmarks",
                                 "Ecart %", "Orig. seg", "Orig. lm"),
                    options = list(dom = "t", paging = FALSE, ordering = FALSE,
                                   scrollX = TRUE, scrollY = "230px",
                                   scrollCollapse = TRUE))
    style_cells(dt, "Mesure")
  })

  output$sp_note <- renderUI({
    n <- length(sp_choices())
    swatch <- function(col, lab) tags$span(
      style = sprintf("color:%s;font-weight:600;margin-right:10px;", col), lab)
    tagList(
      tags$p(style = "font-size:12px;margin:2px 0;",
             if (identical(input$sp_colour %||% "gap", "origin"))
               tagList(swatch("#15803d", "vert = mesure sur la photographie"),
                       swatch("#b91c1c", "rouge = imputee / non mesuree"),
                       swatch("#1d4ed8", "bleu = attribut d'espece (FishBase)"),
                       swatch("#6b7280", "gris = statut inconnu"))
             else
               tagList(swatch("#111827", sprintf("ecart < %d %%", thr())),
                       swatch("#c2410c", sprintf("%d - %d %%", thr(), 2 * thr())),
                       swatch("#b91c1c", sprintf("> %d %%", 2 * thr())))),
      hint("Les colonnes Orig. seg et Orig. lm gardent le vocabulaire ",
           "precedent, mais par campagne : « impute » y signifie que CETTE ",
           "campagne n'a pas la mesure brute, donc que la valeur utilisee par ",
           "l'ordination a ete completee — la cellule de comparaison, elle, ",
           "reste un tiret, aucun nombre invente n'etant imprime. Le selecteur ",
           "« Colorer par » choisit laquelle des deux lectures porte la ",
           "couleur : un ecart est une propriete de la PAIRE, une origine une ",
           "propriete de CHAQUE cote, et elles ne peuvent pas teindre les ",
           "memes cellules sans se faire passer l'une pour l'autre."),
      hint("Les deux colonnes comparent les DEUX campagnes sur ce specimen, ",
           "quel que soit le choix general de l'application : c'est leur seul ",
           "objet. Les valeurs sont recalculees depuis les mesures BRUTES de ",
           "chaque campagne, jamais lues dans les tables de traits publiees, ",
           "qui ont subi l'imputation — comparer celles-ci reviendrait a ",
           "comparer deux imputations plutot que deux methodes. Un tiret ",
           "signale qu'une campagne ne produit pas la quantite ; y mettre une ",
           "valeur imputee fermerait discretement l'ecart que l'on cherche."),
      hint("L'ecart est symetrique, 200|a-b|/(|a|+|b|), parce qu'un segment ",
           "FISHMORPH vaut legitimement ZERO — Mo = 0 est une bouche ",
           "terminale, PFi = 0 une pectorale inseree sur l'axe median — et la ",
           "forme |a-b|/b n'y est pas definie, precisement sur les 954 et 737 ",
           "especes ou les deux methodes divergent le plus. Sur les 3 326 ",
           "especes mesurees deux fois, l'ecart median vaut 4,9 % pour les ",
           "rapports (q75 = 12,3 %, q90 = 26,5 %) et 2,9 % pour les segments : ",
           "le seuil de 15 % par defaut signale environ le quart le plus ",
           "divergent. Les pixels des deux campagnes SONT comparables — Bl ne ",
           "diverge que de 1,2 % en median, les photographies sont les memes."),
      if (length(SIZE_TRAITS)) hint(sprintf(
        paste0("L'ordination inclut %s en plus des neuf rapports. Ce sont des ",
               "attributs d'espece, pas des mesures sur ce cliche, et ils ne ",
               "sont pas sans dimension : ils portent l'essentiel de PC1, qui ",
               "devient donc largement un axe de TAILLE. MBl et MBw sont ",
               "correles a r = 0,925, donc la taille y entre deux fois. Les ",
               "indices ne sont plus comparables a l'espace FISHMORPH publie, ",
               "bati sur les rapports seuls."),
        paste(SIZE_TRAITS, collapse = " et "))),
      if (is.null(LM_SEG) || !is.matrix(LM_SEG_LM)) tags$p(
        style = "font-size:12px;margin:2px 0;color:#a16207;font-weight:600;",
        "Ce cache ne porte pas les mesures brutes des deux campagnes : la ",
        "comparaison est incomplete. Reconstruisez-le avec ",
        "prepare_fishmorph_basins(landmarks = ...)."),
      hint(sprintf(paste0(
        "Campagne active : %s. Les rapports et l'ordination viennent de sa ",
        "propre table (%s) ; les segments bruts, de la feuille publiee pour ",
        "la campagne SEGMENTS et des distances RECALCULEES entre landmarks ",
        "pour la campagne LANDMARKS ; les photographies, du dossier passe a ",
        "launch_fishmorph_basins(photos = ). Ces sources ne couvrent PAS les ",
        "memes especes : 6 230 des 9 557 lignes de Global_Landmark n'ont ",
        "aucune coordonnee, et le dossier de cliches est plus petit encore. ",
        "Une espece peut donc avoir des rapports publies sans photographie ni ",
        "landmark — c'est le cas d'Acipenser sturio."),
        names(CAMPAIGNS)[match(camp(), CAMPAIGNS)],
        basename(TS()$source_file %||% "?"))),
      hint("Ce statut est DEDUIT, non lu : la table de traits publiee ne porte ",
           "aucun drapeau par rapport (le n_imputed de la table landmark est un ",
           "compte par espece, qui ne dit pas LEQUEL a ete estime). Un rapport ",
           "est dit mesure quand les deux segments dont il est fait sont ",
           "presents dans la feuille Global_segments, et impute sinon. Les 811 ",
           "especes de la table publiee absentes de cette feuille restent en ",
           "gris : rien dans la donnee ne permet de trancher, et les peindre en ",
           "vert affirmerait une mesure que la source n'a jamais faite. Un ",
           "segment NUL est mesure, pas manquant — Mo = 0 est une bouche ",
           "terminale, Bbl = 0 un poisson sans barbillons."),
      hint(sprintf(
        paste0("%d espece(s) affichable(s) dans l'assemblage courant. Les ",
               "segments sont en PIXELS de la photographie d'origine, tels que ",
               "digitalises : ils ne sont pas convertis en millimetres, faute ",
               "d'une echelle propre au cliche — MaxLength est une longueur ",
               "maximale d'espece (FishBase), pas la taille du poisson ",
               "photographie, et l'utiliser comme etalon fabriquerait une ",
               "mesure que la donnee ne porte pas. Les rapports, eux, sont ",
               "sans dimension et donc comparables. %sPanneau en lecture ",
               "seule : aucune modification n'est possible ici, utilisez ",
               "launch_fishmorph_digitizer() pour corriger un point."),
        n,
        if (!length(PHOTOS))
          paste0("Photographies non fournies ",
                 "(launch_fishmorph_basins(photos = ...)) : seuls les ",
                 "landmarks sont dessines. ")
        else ""))
    )
  })

  # --- 3. table --------------------------------------------------------------
  table_data <- reactive({
    d <- stats_now()
    if (isTRUE(input$tab_restrict) && length(lit()))
      d <- d[d$basin_id %in% lit(), , drop = FALSE]
    out <- data.frame(
      Bassin = d$basin, Pays = d$country, Ecoregion = d$ecoregion,
      S = d$n_species, Natives = d$n_native, Exotiques = d$n_exotic,
      Mesurees = d$n_traits, Couverture = round(d$coverage, 3),
      `FRic %` = round(d$FRic_pct, 2), FRic = round(d$FRic, 4),
      FDiv = round(d$FDiv, 4),
      FDis = round(d$FDis, 4), FEve = round(d$FEve, 4), Rao = round(d$Rao, 4),
      # Without this, data.frame() mangles "FRic %" into "FRic.." and the
      # column the user sorts on is not the one they read.
      check.names = FALSE, stringsAsFactors = FALSE)
    attr(out, "ids") <- d$basin_id
    out
  })

  output$tab <- renderDT({
    out <- table_data()
    datatable(out, rownames = FALSE, selection = "single", filter = "top",
              options = list(pageLength = 25, scrollX = TRUE, scrollY = "520px",
                             dom = "ftip", order = list(list(3, "desc"))))
  })

  observeEvent(input$tab_rows_selected, {
    i <- input$tab_rows_selected
    ids <- attr(table_data(), "ids")
    if (length(i) && i <= length(ids)) focus(ids[i])
  })

  output$tab_note <- renderUI({
    d <- table_data()
    hint(sprintf(
      paste0("%d bassins. Source : %s ; statut : %s. FRic et FDiv dans le plan ",
             "PC%d-PC%d, FDis / FEve / Rao sur %d axes, poids spécifiques ",
             "égaux (les tables d'occurrence enregistrent une présence, ",
             "pas une abondance). Un indice vaut NA quand l'assemblage est trop ",
             "petit ou dégénéré, jamais 0."),
      nrow(d), names(SOURCES)[match(src(), SOURCES)], status(),
      SET$axes_ric[1], SET$axes_ric[2], length(SET$axes_dist)))
  })

  output$dl_table <- downloadHandler(
    filename = function() sprintf("bassins_%s_%s_%s.csv", src(), status(),
                                  format(Sys.Date(), "%Y%m%d")),
    content = function(file)
      utils::write.csv(table_data(), file, row.names = FALSE,
                       fileEncoding = "UTF-8"))

  # --- 4. comparison ---------------------------------------------------------
  # Everything here reads CMP, computed once at cache time. The panel draws; it
  # does not fit, rotate or test anything.

  output$cmp_head <- renderUI({
    if (!HAS_CMP)
      return(hint("Ce cache ne porte pas la comparaison : il faut les mesures ",
                  "brutes des DEUX campagnes. Reconstruisez-le avec ",
                  "prepare_fishmorph_basins(landmarks = ...)."))
    p <- CMP$procrustes
    n <- nrow(CMP$ratios)
    box <- function(lab, val, sub = NULL) tags$div(
      class = "border rounded p-2 me-3", style = "min-width:190px;",
      tags$div(style = "font-size:12px;color:#6b7280;", lab),
      tags$div(style = "font-size:22px;font-weight:600;", val),
      if (!is.null(sub)) tags$div(style = "font-size:11px;color:#6b7280;", sub))
    tagList(
      div(class = "d-flex flex-wrap",
          box("Especes mesurees deux fois", n),
          if (is.null(p)) box("Procrustes", "—", "package 'vegan' absent")
          else box("Correlation de Procrustes", fmt(p$correlation, 3),
                   sprintf("p = %.3f, %s permutations, n = %d",
                           p$significance,
                           # vegan has returned `permutations` both as a count
                           # and as a control object over its versions; %d on
                           # the latter kills the panel for a caption.
                           tryCatch(format(as.integer(p$permutations)),
                                    error = function(e) "?",
                                    warning = function(w) "?"),
                           p$n)),
          if (!is.null(p)) box("Somme des carres residuels", fmt(p$ss, 4),
                               "0 = superposition parfaite")),
      hint("Le test de Procrustes superpose les deux configurations en ",
           "autorisant translation, rotation et mise a l'echelle, puis ",
           "compare le residu a celui de configurations permutees. Une ",
           "correlation elevee dit que les deux campagnes rangent les especes ",
           "de la MEME facon les unes par rapport aux autres ; elle ne dit pas ",
           "qu'un poisson est a la meme place, ni que les indices d'un bassin ",
           "sont les memes. Une p-value ne mesure ici que l'improbabilite d'un ",
           "accord nul, ce qui n'est pas l'hypothese interessante quand on ",
           "compare deux mesures du meme objet."))
  })

  cmp_scores <- reactive({
    validate(need(HAS_CMP && !is.null(CMP$space_scores) && !is.null(CMP$space),
                  paste0("Ce cache ne porte pas l'ordination partagee. ",
                         "Reconstruisez-le : la comparaison a besoin de ",
                         "compare_segments_landmarks(space = TRUE).")))
    CMP$space_scores
  })

  output$cmp_space <- renderPlotly({
    ss <- cmp_scores()
    ve <- 100 * (CMP$space$pca$sdev^2 / sum(CMP$space$pca$sdev^2))
    lab <- function(i) sprintf("PC%d (%.1f %%)", i, ve[i])
    idc <- intersect(c("species", CMP_ID, "id"), names(ss))[1]
    has_grp <- "group" %in% names(ss) && !all(is.na(ss$group))
    by_grp <- has_grp && identical(input$cmp_colour %||% "group", "group")

    p <- plot_ly(source = "fmcmp")
    # One panel per campaign, on the SAME axes and the same ranges: two clouds
    # drawn on independently scaled panels would look alike whatever they are.
    for (k in seq_along(c("segment", "landmark"))) {
      src <- c("segment", "landmark")[k]
      d <- ss[ss$source == src, , drop = FALSE]
      if (!nrow(d)) next
      if (by_grp) {
        g <- d$group; g[is.na(g) | !nzchar(g)] <- "inconnu"
        lv <- names(sort(table(g), decreasing = TRUE))
        cols <- grDevices::hcl.colors(length(lv), "Dark 3")
        for (i in seq_along(lv)) {
          j <- g == lv[i]
          p <- add_trace(p, type = "scattergl", mode = "markers",
                         x = d$PC1[j], y = d$PC2[j], xaxis = if (k == 1L) "x" else "x2",
                         yaxis = "y", customdata = d[[idc]][j],
                         marker = list(size = 4, color = cols[i]),
                         text = d[[idc]][j], hoverinfo = "text",
                         name = lv[i], legendgroup = lv[i],
                         showlegend = (k == 1))
        }
      } else {
        p <- add_trace(p, type = "scattergl", mode = "markers",
                       x = d$PC1, y = d$PC2, xaxis = if (k == 1L) "x" else "x2",
                       yaxis = "y", customdata = d[[idc]],
                       marker = list(size = 4, color = "#0f766e"),
                       text = d[[idc]], hoverinfo = "text", name = src,
                       showlegend = FALSE)
      }
    }
    # The pairing segments are the only thing that shows the DISPLACEMENT rather
    # than the two shapes; they are off by default because three thousand of
    # them hide the clouds they are drawn on.
    if (isTRUE(input$cmp_link)) {
      a <- ss[ss$source == "segment", , drop = FALSE]
      b <- ss[ss$source == "landmark", , drop = FALSE]
      m <- match(a[[idc]], b[[idc]])
      ok <- !is.na(m)
      n <- sum(ok)
      if (n) {
        xs <- as.vector(rbind(a$PC1[ok], b$PC1[m[ok]], NA))
        ys <- as.vector(rbind(a$PC2[ok], b$PC2[m[ok]], NA))
        p <- add_trace(p, type = "scattergl", mode = "lines",
                       x = xs, y = ys, xaxis = "x", yaxis = "y",
                       line = list(color = "rgba(15,23,42,0.25)", width = 0.5),
                       hoverinfo = "skip", showlegend = FALSE)
      }
    }
    rx <- range(ss$PC1, na.rm = TRUE); ry <- range(ss$PC2, na.rm = TRUE)
    p <- layout(
      p,
      xaxis  = list(domain = c(0, 0.48), title = paste("Segments —", lab(1)),
                    range = rx, zeroline = TRUE, zerolinecolor = "#e5e7eb"),
      xaxis2 = list(domain = c(0.52, 1), title = paste("Landmarks —", lab(1)),
                    range = rx, zeroline = TRUE, zerolinecolor = "#e5e7eb"),
      yaxis  = list(title = lab(2), range = ry, zeroline = TRUE,
                    zerolinecolor = "#e5e7eb"),
      legend = list(orientation = "h", y = -0.18),
      hovermode = "closest", margin = list(l = 60, r = 20, t = 20, b = 90))
    event_register(p, "plotly_click")
  })

  output$cmp_space_note <- renderUI({
    if (!HAS_CMP) return(NULL)
    hint("UNE ordination, figee sur les rapports de la campagne SEGMENTS, dans ",
         "laquelle les deux jeux de mesures sont projetes. C'est le seul ",
         "endroit de l'application ou les deux campagnes partagent un repere, ",
         "et il le faut : un deplacement n'est lisible que si les axes ne ",
         "bougent pas. Ailleurs, chaque campagne garde son ordination propre, ",
         "pour la raison inverse — elle doit pouvoir se lire seule. Les deux ",
         "panneaux partagent les memes echelles ; sur des echelles ajustees ",
         "separement, deux nuages quelconques se ressemblent.")
  })

  output$cmp_biplots <- renderPlot({
    validate(need(HAS_CMP, "Comparaison absente du cache."))
    what <- input$cmp_what %||% "ratios"
    m <- if (identical(what, "segments")) CMP$segments else CMP$ratios
    validate(need(!is.null(m) && nrow(m), "Table indisponible."))
    vars <- unique(sub("\\.(pub|lm)$", "",
                       grep("\\.(pub|lm)$", names(m), value = TRUE)))
    vars <- vars[vapply(vars, function(v)
      all(paste0(v, c(".pub", ".lm")) %in% names(m)), logical(1))]
    validate(need(length(vars), "Aucune variable appariee."))
    long <- do.call(rbind, lapply(vars, function(v) data.frame(
      trait = v, published = m[[paste0(v, ".pub")]],
      landmark = m[[paste0(v, ".lm")]], stringsAsFactors = FALSE)))
    long <- long[is.finite(long$published) & is.finite(long$landmark), ]
    # r is printed on each panel: a cloud along the 1:1 line and a cloud along
    # a line of slope 3 look equally "correlated" to the eye, and only one of
    # them means the two methods agree.
    labs <- vapply(vars, function(v) {
      d <- long[long$trait == v, ]
      sprintf("%s  (r = %.2f, n = %d)", v,
              suppressWarnings(stats::cor(d$published, d$landmark)), nrow(d))
    }, character(1))
    long$panel <- factor(labs[match(long$trait, vars)], levels = labs)
    ggplot2::ggplot(long, ggplot2::aes(x = published, y = landmark)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#b91c1c",
                           linetype = 2) +
      ggplot2::geom_point(colour = "#0f766e", size = 0.6, alpha = 0.45) +
      ggplot2::facet_wrap(~panel, scales = "free") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::labs(x = "campagne segments", y = "campagne landmarks")
  })

  output$cmp_biplot_note <- renderUI({
    if (!HAS_CMP) return(NULL)
    hint("La droite rouge est la premiere bissectrice, pas une regression : ",
         "la question est l'ACCORD, pas la correlation. Un nuage bien aligne ",
         "mais decale de la bissectrice signale un biais systematique entre ",
         "les deux methodes, qu'un r eleve ne revelerait pas. Les segments ",
         "sont rapportes a Bl, ce qui elimine l'echelle pixel de chaque cliche ",
         "et rend les deux campagnes comparables sans etalonnage.")
  })

  output$cmp_metrics <- renderDT({
    validate(need(HAS_CMP && !is.null(CMP$metrics), "Comparaison absente."))
    what <- input$cmp_what %||% "ratios"
    q <- if (identical(what, "segments")) "segment" else "ratio"
    m <- CMP$metrics[CMP$metrics$quantity == q, , drop = FALSE]
    validate(need(nrow(m), "Aucune metrique pour ce jeu."))
    out <- data.frame(
      Variable = m$trait, n = m$n,
      r = round(m$pearson, 3), rho = round(m$spearman, 3),
      Biais = round(m$bias, 4), RMSE = round(m$rmse, 4),
      stringsAsFactors = FALSE)
    out <- out[order(out$r), , drop = FALSE]
    datatable(out, rownames = FALSE, selection = "none",
              class = "compact stripe",
              options = list(dom = "t", paging = FALSE, ordering = TRUE,
                             scrollX = TRUE, scrollY = "240px",
                             scrollCollapse = TRUE)) |>
      formatStyle("r", color = styleInterval(c(0.8, 0.95),
                                             c("#b91c1c", "#c2410c", "#15803d")),
                  fontWeight = "bold")
  })

  # One row per species AND per variable: the biplots show that REs disagrees,
  # this says WHICH fish makes the point at 21 and what each method read on it.
  # A panel that shows a disagreement without naming its cause sends the reader
  # back to the workbook by hand.
  cmp_long <- reactive({
    validate(need(HAS_CMP, "Comparaison absente du cache."))
    what <- input$cmp_what %||% "ratios"
    m <- if (identical(what, "segments")) CMP$segments else CMP$ratios
    validate(need(!is.null(m) && nrow(m), "Table indisponible."))
    vars <- unique(sub("\\.(pub|lm)$", "",
                       grep("\\.(pub|lm)$", names(m), value = TRUE)))
    vars <- vars[vapply(vars, function(v)
      all(paste0(v, c(".pub", ".lm")) %in% names(m)), logical(1))]
    validate(need(length(vars), "Aucune variable appariee."))
    key <- as.character(m[[CMP_ID]])
    nm <- SPE$species[match(.fmb_norm_key(key), SPE$species_key)]
    nm[is.na(nm)] <- key[is.na(nm)]
    # Vectorised form of dsym(): same symmetric definition, applied column-wise
    # rather than one pair at a time -- thirty thousand scalar calls would make
    # a table that redraws on every keystroke of the filter.
    dsv <- function(a, b) {
      out <- 200 * abs(a - b) / (abs(a) + abs(b))
      out[is.finite(a) & is.finite(b) & a == 0 & b == 0] <- 0
      out[!is.finite(a) | !is.finite(b)] <- NA_real_
      out
    }
    d <- do.call(rbind, lapply(vars, function(v) {
      a <- m[[paste0(v, ".pub")]]; b <- m[[paste0(v, ".lm")]]
      data.frame(Variable = v, Espece = nm, cle = key,
                 Segments = a, Landmarks = b, Ecart = dsv(a, b),
                 stringsAsFactors = FALSE)
    }))
    d <- d[!is.na(d$Ecart), , drop = FALSE]
    d[order(-d$Ecart), , drop = FALSE]
  })

  output$cmp_outliers <- renderDT({
    d <- cmp_long()
    if (isTRUE(input$cmp_out_only %||% TRUE)) d <- d[d$Ecart >= thr(), , drop = FALSE]
    validate(need(nrow(d), sprintf(
      "Aucun ecart au-dela de %d %%. Baissez le seuil ou decochez le filtre.",
      thr())))
    out <- data.frame(
      Variable = d$Variable, Espece = d$Espece,
      Segments = signif(d$Segments, 4), Landmarks = signif(d$Landmarks, 4),
      Ecart = round(d$Ecart, 1), stringsAsFactors = FALSE)
    datatable(out, rownames = FALSE, selection = "single", filter = "top",
              class = "compact stripe",
              colnames = c("Variable", "Espece", "Segments", "Landmarks",
                           "Ecart %"),
              options = list(pageLength = 12, scrollX = TRUE, dom = "ftip",
                             order = list(list(4, "desc")))) |>
      formatStyle("Ecart", fontWeight = "bold",
                  color = styleInterval(c(thr(), 2 * thr()),
                                        c("#111827", "#c2410c", "#b91c1c")))
  })

  observeEvent(input$cmp_outliers_rows_selected, {
    i <- input$cmp_outliers_rows_selected
    d <- cmp_long()
    if (isTRUE(input$cmp_out_only %||% TRUE)) d <- d[d$Ecart >= thr(), , drop = FALSE]
    if (!length(i) || i > nrow(d)) return()
    show_species(.fmb_norm_key(d$cle[i]))
    nav_select("nav", "2. Espace fonctionnel")
  })

  output$dl_cmp_out <- downloadHandler(
    filename = function() sprintf("ecarts_%s_%s.csv",
                                  input$cmp_what %||% "ratios",
                                  format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      d <- cmp_long()
      if (isTRUE(input$cmp_out_only %||% TRUE)) d <- d[d$Ecart >= thr(), , drop = FALSE]
      utils::write.csv(d, file, row.names = FALSE, fileEncoding = "UTF-8")
    })

  output$cmp_out_note <- renderUI({
    if (!HAS_CMP) return(NULL)
    d <- cmp_long()
    n_tot <- nrow(d); n_out <- sum(d$Ecart >= thr(), na.rm = TRUE)
    hint(sprintf(
      paste0("%d couples espece x variable, dont %d (%.1f %%) au-dela du seuil ",
             "de %d %% regle dans la barre laterale. Filtrez la colonne Variable pour ",
             "isoler un trait, puis triez Segments ou Landmarks pour trouver ",
             "une valeur extreme plutot qu'un simple desaccord — les deux ",
             "questions sont distinctes : un REs de 21 est impossible (le ",
             "diametre de l'oeil ne fait pas vingt fois la hauteur de la tete) ",
             "et signale une erreur de digitalisation, tandis qu'un desaccord ",
             "de 30 %% sur deux valeurs plausibles est une divergence de ",
             "methode. Un clic sur une ligne ouvre l'espece dans le panneau ",
             "specimen."),
      n_tot, n_out, 100 * n_out / max(n_tot, 1), thr()))
  })

  output$cmp_shift <- renderDT({
    validate(need(HAS_CMP && !is.null(CMP$space_shift),
                  "Deplacements absents du cache."))
    ss <- CMP$space_shift
    keep <- intersect(c("species", CMP_ID, "group", "distance", "dist_2d"),
                      names(ss))
    out <- utils::head(ss[, keep, drop = FALSE], 200)
    num <- vapply(out, is.numeric, logical(1))
    out[num] <- lapply(out[num], round, 3)
    datatable(out, rownames = FALSE, selection = "single",
              class = "compact stripe",
              options = list(pageLength = 10, scrollX = TRUE, dom = "ftip"))
  })

  # A species clicked in either the shared space or the displacement table goes
  # to the specimen panel, where the disagreement can be looked at on the fish.
  observeEvent(event_data("plotly_click", source = "fmcmp"), {
    e <- event_data("plotly_click", source = "fmcmp")
    k <- e$customdata
    if (is.null(k) || !length(k)) return()
    k <- as.character(k)[1]
    if (!is.na(k) && nzchar(k)) {
      show_species(.fmb_norm_key(k))
      nav_select("nav", "2. Espace fonctionnel")
    }
  })

  observeEvent(input$cmp_shift_rows_selected, {
    i <- input$cmp_shift_rows_selected
    ss <- CMP$space_shift
    if (!length(i) || is.null(ss) || i > nrow(ss)) return()
    idc <- intersect(c("species", CMP_ID), names(ss))[1]
    show_species(.fmb_norm_key(as.character(ss[[idc]][i])))
    nav_select("nav", "2. Espace fonctionnel")
  })

  output$cache_stamp <- renderUI(
    hint(sprintf("cache du %s",
                 format(CACHE$meta$created, "%Y-%m-%d %H:%M"))))
}

shinyApp(ui, server)
