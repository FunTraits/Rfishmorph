# =============================================================================
# basin-app.R -- global drainage-basin explorer.
#
# Three objects, in the order they are used:
#
#   prepare_fishmorph_basins()  reads the heavy sources ONCE (a 3,364-polygon
#                               shapefile, two occurrence tables, the FISHMORPH
#                               trait table), builds the ordination, computes
#                               the functional-diversity indices of every basin
#                               and writes a single cache file.
#   fishmorph_basin_indices()   the index engine, exported on its own so that
#                               the numbers in the application can be
#                               reproduced -- and contradicted -- from the
#                               console.
#   launch_fishmorph_basins()   checks dependencies, resolves the cache, runs
#                               inst/shiny/fishmorph_basins/app.R.
#
# The separation is the one already used by launch_fishmorph_space(): the app
# directory stays runnable with shiny::runApp() alone, which is what makes it
# debuggable, and the launcher communicates with it by OPTION rather than by
# global variable, so two caches can be opened one after the other in the same
# session without leaking state.
#
# WHY A CACHE, and not a read-at-startup app: the shapefile is ~50 MB, the two
# occurrence tables hold ~210,000 species-by-basin records, and the indices of
# 3,364 basins are a few minutes of convex hulls and distance matrices. None of
# that depends on anything the user does in the interface, so none of it belongs
# in a reactive. It is computed once, dated, and reread in under a second.
# =============================================================================

# Dependencies of the basin explorer. sf reads and simplifies the polygons,
# leaflet draws them; the rest is shared with the morphological-space app.
.FMB_APP_DEPS  <- c("shiny", "bslib", "DT", "plotly", "leaflet", "sf", "ggplot2")

# Dependencies of the PREPARATION step, which is where the shapefile and the
# workbooks are actually read. Kept apart from the app's: someone who received
# a cache built by a colleague needs none of them.
.FMB_PREP_DEPS <- c("sf")

# The nine dimensionless FISHMORPH ratios, in the order of Brosse et al. (2021).
# MBl and MBw are deliberately excluded: they are body SIZE, already log10 in
# the bundled table, and including them makes the first axis a size axis.
.FMB_TRAITS <- c("BEl", "VEp", "REs", "OGp", "RMl", "BLs", "PFv", "PFs", "CPt")

# The two SIZE variables, kept in their own vector rather than merged into the
# nine above, because they are a different kind of quantity and the difference
# has consequences the interface must be able to state:
#
#   * They are not dimensionless. The nine ratios are quotients of two lengths
#     measured on the same fish; MBl is a maximum body length in centimetres and
#     MBw a maximum body weight in grams, both stored as log10(x + 1).
#   * They are not measured on the photograph. They are species-level attributes
#     from FishBase, so they can never be reported as "measured on the picture"
#     alongside a segment that was.
#   * They are nearly collinear (r = 0.925 on the 8,970 species of the published
#     table: log-weight is essentially log a + b log-length). In a standardised
#     PCA the pair therefore enters size TWICE and carries about 1.0 of PC1 on
#     its own -- loadings -0.508 and -0.511, ahead of every shape ratio. That is
#     a legitimate choice of space, not a mistake, but it makes PC1 a size axis
#     and the resulting FRic no longer comparable with the FISHMORPH space of
#     Brosse et al. (2021), which is built on the nine ratios alone.
#
# Which of them enters the ordination is therefore an ARGUMENT of
# prepare_fishmorph_basins(), recorded in the cache, and displayed by the app.
.FMB_SIZE <- c("MBl", "MBw")

# Layout version of the cache. Bumped whenever prepare_fishmorph_basins()
# changes WHERE something lives, and checked by the application at startup.
#
# Without it, a cache and an application that disagree fail deep inside a
# reactive, on the first column that moved -- "undefined columns selected",
# naming neither the cache nor the version. The two objects are written and
# read by different processes, installed at different moments; assuming they
# match is the one assumption that is regularly false.
#   1  first layout: one ordination, has_traits and the ratios in $species
#   2  one trait set per measurement campaign in $traits; $species is identity
#   3  size traits (MBl, MBw) optionally in the ordination; $traits[[i]]$vars
#   4  $comparison: shared ordination + Procrustes between the two campaigns
#   5  FRic_pct in $indices, $traits[[i]]$fric_world as its denominator
.FMB_CACHE_FORMAT <- 5L

# Species names have to survive the round trip between three sources that
# spell them three ways -- "Squalius cephalus", "Squalius_cephalus",
# "SQUALIUS CEPHALUS". The rule is intraitR's own (`itv_reference`): case
# ignored, spaces and underscores equivalent. Defined ONCE, so that an
# occurrence table and a trait table can never match a species differently.
.fmb_key <- function(x)
  tolower(gsub("[ _]+", " ", trimws(as.character(x))))

`%|N|%` <- function(x, y) if (is.null(x) || !length(x) || all(is.na(x))) y else x


# =============================================================================
# 1. The index engine
# =============================================================================

# Minimum spanning tree by Prim's algorithm, returning its S - 1 branch
# LENGTHS. Written out rather than taken from vegan::spantree or ape::mst
# because FEve is the only thing the package needs an MST for, and neither
# package is worth a dependency for fifteen lines. O(S^2), which is the right
# complexity here: the distance matrix is dense and already built.
.fmb_mst_lengths <- function(D) {
  n <- nrow(D)
  if (n < 2L) return(numeric(0))
  inside <- logical(n)
  inside[1L] <- TRUE
  best <- D[1L, ]
  best[1L] <- Inf
  out <- numeric(n - 1L)
  for (k in seq_len(n - 1L)) {
    cand <- ifelse(inside, Inf, best)
    j <- which.min(cand)
    out[k] <- cand[j]
    inside[j] <- TRUE
    upd <- !inside & D[j, ] < best
    best[upd] <- D[j, upd]
  }
  out[is.finite(out)]
}

# Area of the convex hull of a planar point cloud (shoelace formula on
# grDevices::chull). Exact, dependency-free, and NA -- not zero -- for a cloud
# of fewer than three points or a degenerate one: an unmeasurable area is not a
# null one, and reporting 0 would put a basin whose species happen to be
# collinear at the bottom of a ranking rather than out of it.
.fmb_hull_area <- function(xy) {
  xy <- xy[stats::complete.cases(xy), , drop = FALSE]
  if (nrow(xy) < 3L) return(NA_real_)
  h <- tryCatch(grDevices::chull(xy[, 1], xy[, 2]), error = function(e) NULL)
  if (is.null(h) || length(h) < 3L) return(NA_real_)
  x <- xy[h, 1]; y <- xy[h, 2]
  j <- c(seq_along(x)[-1L], 1L)
  a <- abs(sum(x * y[j] - x[j] * y)) / 2
  if (is.finite(a) && a > 0) a else NA_real_
}

#' Functional-diversity indices of one assemblage
#'
#' Computes, for a single set of species positioned in a functional space, the
#' five indices reported by the basin explorer: functional richness `FRic`,
#' divergence `FDiv`, dispersion `FDis`, evenness `FEve` and Rao's quadratic
#' entropy `Rao`. All five are computed with EQUAL species weights -- the
#' occurrence tables record presence, not abundance, and weighting presences by
#' anything would be inventing a quantity the data do not hold.
#'
#' The two families of index are deliberately computed on DIFFERENT numbers of
#' axes, and the split is the honest one rather than the convenient one.
#' `FRic` and `FDiv` rest on a convex hull, whose volume is meaningless as soon
#' as the number of species approaches the number of axes and which is
#' undefined below it; they are computed in the PLANE that the application
#' draws, so that the number in the table is the area the eye sees. `FDis`,
#' `FEve` and `Rao` rest on distances, which are stable in higher dimension and
#' lose information when projected; they use as many axes as `axes_dist`
#' provides.
#'
#' @param scores Numeric matrix or data frame of ordination scores, one row per
#'   species, columns being the axes. Rows with any missing coordinate are
#'   dropped before anything is computed.
#' @param axes_ric Integer vector of length 2, the columns of `scores` carrying
#'   the plane on which `FRic` and `FDiv` are measured. Defaults to `1:2`.
#' @param axes_dist Integer vector, the columns of `scores` used by `FDis`,
#'   `FEve` and `Rao`. Defaults to `1:4`, truncated to what `scores` has.
#' @return A one-row `data.frame` with columns `n` (species actually used),
#'   `FRic`, `FDiv`, `FDis`, `FEve` and `Rao`. An index that the assemblage is
#'   too small or too degenerate to support is `NA`, never `0`.
#'
#' @details
#' Definitions follow Villeger, Mason and Mouillot (2008) for `FRic`, `FEve`
#' and `FDiv`, Laliberte and Legendre (2010) for `FDis`, and Botta-Dukat (2005)
#' for `Rao`, all in their equal-weight form:
#'
#' `FRic` is the area of the convex hull of the assemblage in the `axes_ric`
#' plane, and needs at least three non-collinear species.
#'
#' `FDiv` compares each species' distance to the centre of gravity of the HULL
#' VERTICES with the mean of those distances, so that an assemblage whose
#' species crowd the middle of its own hull scores low and one whose species sit
#' near its edge scores high. It needs the same three species as `FRic`.
#'
#' `FDis` is the mean distance of the species to the centroid of the
#' assemblage, and needs two species.
#'
#' `FEve` is the regularity of the minimum spanning tree linking the species:
#' the branch lengths are turned into shares, each share is capped at
#' `1 / (S - 1)`, and the capped sum is rescaled to `[0, 1]`. It needs three
#' species.
#'
#' `Rao` is the mean pairwise distance in the sense of Rao's quadratic entropy,
#' `sum(d_ij) / S^2` with equal weights -- which is the mean over ALL ordered
#' pairs including the S zero self-distances, not the mean over distinct pairs.
#' The two differ by a factor `(S - 1) / S` and only the first is Rao's index.
#'
#' @references
#' Botta-Dukat, Z. (2005). Rao's quadratic entropy as a measure of functional
#' diversity based on multiple traits. Journal of Vegetation Science, 16,
#' 533-540.
#'
#' Laliberte, E., & Legendre, P. (2010). A distance-based framework for
#' measuring functional diversity from multiple traits. Ecology, 91, 299-305.
#'
#' Villeger, S., Mason, N. W. H., & Mouillot, D. (2008). New multidimensional
#' functional diversity indices for a multifaceted framework in functional
#' ecology. Ecology, 89, 2290-2301.
#'
#' @seealso [prepare_fishmorph_basins()], which calls this once per basin,
#'   [launch_fishmorph_basins()].
#' @examples
#' set.seed(1)
#' sc <- matrix(stats::rnorm(200), ncol = 4)
#' fishmorph_basin_indices(sc)
#' @export
fishmorph_basin_indices <- function(scores, axes_ric = 1:2, axes_dist = 1:4) {
  empty <- data.frame(n = 0L, FRic = NA_real_, FDiv = NA_real_,
                      FDis = NA_real_, FEve = NA_real_, Rao = NA_real_)
  if (is.null(scores)) return(empty)
  P <- as.matrix(scores)
  if (!nrow(P) || !ncol(P)) return(empty)
  storage.mode(P) <- "double"
  P <- P[stats::complete.cases(P), , drop = FALSE]
  S <- nrow(P)
  if (!S) return(empty)

  ar <- axes_ric[axes_ric <= ncol(P)]
  ad <- axes_dist[axes_dist <= ncol(P)]
  out <- empty
  out$n <- S
  if (S < 2L) return(out)

  # -- distance-based family, on axes_dist -----------------------------------
  if (length(ad) >= 1L) {
    Pd <- P[, ad, drop = FALSE]
    cen <- colMeans(Pd)
    out$FDis <- mean(sqrt(rowSums(sweep(Pd, 2L, cen, "-")^2)))
    D <- as.matrix(stats::dist(Pd))
    out$Rao <- sum(D) / (S * S)
    if (S >= 3L) {
      l <- .fmb_mst_lengths(D)
      tot <- sum(l)
      if (length(l) == S - 1L && is.finite(tot) && tot > 0) {
        pew <- l / tot
        thr <- 1 / (S - 1)
        out$FEve <- (sum(pmin(pew, thr)) - thr) / (1 - thr)
      }
    }
  }

  # -- hull-based family, on the drawn plane ---------------------------------
  if (length(ar) == 2L && S >= 3L) {
    Pr <- P[, ar, drop = FALSE]
    out$FRic <- .fmb_hull_area(Pr)
    if (!is.na(out$FRic)) {
      h <- grDevices::chull(Pr[, 1], Pr[, 2])
      G <- colMeans(Pr[h, , drop = FALSE])
      dG <- sqrt(rowSums(sweep(Pr, 2L, G, "-")^2))
      dbar <- mean(dG)
      dd <- mean(dG - dbar)
      dabs <- mean(abs(dG - dbar))
      den <- dabs + dbar
      if (is.finite(den) && den > 0) out$FDiv <- (dd + dbar) / den
    }
  }
  out
}


# =============================================================================
# 2. Building the cache
# =============================================================================

# Simplify the basin polygons for the web. 3,364 untouched drainage basins are
# ~50 MB of coordinates: leaflet will draw them, slowly, and then redraw them
# on every choropleth change. rmapshaper::ms_simplify() is preferred because it
# is topology-preserving -- neighbouring basins keep their shared boundary
# rather than drifting apart and leaving slivers; sf::st_simplify() is the
# fallback and is honest about being a per-feature approximation.
.fmb_simplify <- function(g, keep) {
  if (keep >= 1) return(g)
  if (requireNamespace("rmapshaper", quietly = TRUE)) {
    out <- tryCatch(rmapshaper::ms_simplify(g, keep = keep, keep_shapes = TRUE,
                                            explode = FALSE),
                    error = function(e) NULL)
    if (!is.null(out)) return(out)
    message("  rmapshaper failed, falling back on sf::st_simplify().")
  } else {
    message("  'rmapshaper' is not installed: falling back on ",
            "sf::st_simplify() (per-feature, may open slivers between basins).",
            "\n  install.packages(\"rmapshaper\") for a topology-preserving ",
            "simplification.")
  }
  # sf::st_simplify() on geographic coordinates goes through s2, which VALIDATES
  # every ring and refuses the whole layer on the first self-intersecting one:
  # "Loop 5 is not valid: Edge 12 crosses edge 14". Published hydrological
  # layers routinely carry a handful of such rings -- a bowtie left by a
  # digitizing step, a duplicated vertex on a coastline -- and one of them
  # would abort a cache built from the other 3,363 basins. s2 is therefore
  # switched off for the simplification, which then runs in the plane through
  # GEOS, and the result is repaired before it is handed back. Planar
  # simplification of degrees is an approximation, but it is an approximation
  # of a DISPLAY geometry: nothing downstream measures an area on these
  # polygons, the indices are computed in the trait space.
  tol <- max(0.005, 0.25 * (1 - keep))
  old <- suppressMessages(sf::sf_use_s2())
  on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))

  out <- tryCatch(
    suppressWarnings(sf::st_simplify(g, dTolerance = tol,
                                     preserveTopology = TRUE)),
    error = function(e) {
      message("  st_simplify() failed (", conditionMessage(e),
              "); repairing the geometry first ...")
      NULL
    })
  if (is.null(out)) {
    g2 <- suppressWarnings(sf::st_make_valid(g))
    out <- tryCatch(
      suppressWarnings(sf::st_simplify(g2, dTolerance = tol,
                                       preserveTopology = TRUE)),
      error = function(e) {
        message("  simplification impossible (", conditionMessage(e),
                "); the full geometry is kept -- the map will be slow.\n",
                "  install.packages(\"rmapshaper\") is the way out.")
        g
      })
  }
  # An empty geometry means a basin lost by the simplification: it is put back
  # at full resolution rather than dropped, because a hole in the map is a
  # missing basin and reads as an absence of data.
  bad <- suppressWarnings(sf::st_is_empty(out))
  if (any(bad)) {
    message("  ", sum(bad), " basin(s) collapsed by the simplification: ",
            "restored at full resolution.")
    out[bad] <- g[bad]
  }
  suppressWarnings(sf::st_make_valid(out))
}

# Read HydroRIVERS (Lehner & Grill 2013), keep the reaches above a discharge
# class, thin their geometry and attach each one to the basin it runs in.
#
# The ORDER OF OPERATIONS is the whole point. HydroRIVERS holds 8,477,883
# reaches; reading them all and filtering afterwards needs several gigabytes for
# a layer of which 98 % is then thrown away. The filter is therefore pushed down
# into GDAL as an OGR SQL query, so the reaches below the threshold are never
# materialised in R at all.
#
# ORD_FLOW is a LOGARITHMIC DISCHARGE class and it runs the counter-intuitive
# way: 1 is a river carrying at least 100,000 m3/s, 10 a trickle under
# 0.001 m3/s. Keeping "ORD_FLOW <= n" therefore keeps the BIG rivers. Getting
# the inequality backwards would silently keep eight million headwater streams,
# which is why the threshold is translated into a discharge in the progress
# message rather than left as a bare number.
.fmb_read_rivers <- function(path, max_order, tol, geom_ref, basin_ids, say) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  max_order <- as.integer(max_order)
  # ORD_FLOW n <=> discharge >= 10^(6 - n) m3/s: n = 1 is 100,000, n = 6 is 1,
  # n = 9 is 0.001. Written as a formula once, and printed, so that the
  # threshold the user asked for is stated in the unit they think in.
  say("Reading HydroRIVERS (ORD_FLOW <= ", max_order, ", i.e. a long-term ",
      "average discharge of at least ",
      format(10^(6 - max_order), scientific = FALSE), " m3/s) ...")

  lyr <- tryCatch(sf::st_layers(path)$name[1], error = function(e) NA_character_)
  riv <- NULL
  if (!is.na(lyr)) {
    q <- sprintf("SELECT ORD_FLOW, DIS_AV_CMS FROM \"%s\" WHERE ORD_FLOW <= %d",
                 lyr, max_order)
    riv <- tryCatch(sf::st_read(path, query = q, quiet = TRUE),
                    error = function(e) NULL)
    # A driver can answer the query and hand back a plain table without the
    # geometry column. That is not a river network, and treating it as one
    # would fail three steps later with an unrelated message.
    if (!inherits(riv, "sf") || !nrow(riv)) riv <- NULL
  }
  if (is.null(riv)) {
    say("  the pushed-down SQL filter was refused by the driver; reading the ",
        "whole layer instead -- this needs several GB of memory.")
    riv <- sf::st_read(path, quiet = TRUE)
    nm <- names(riv)
    oc <- nm[toupper(nm) == "ORD_FLOW"][1]
    if (is.na(oc))
      stop("No ORD_FLOW column in ", path, ": is this really HydroRIVERS?",
           call. = FALSE)
    riv <- riv[!is.na(riv[[oc]]) & riv[[oc]] <= max_order, , drop = FALSE]
  }
  names(riv)[toupper(names(riv)) == "ORD_FLOW"] <- "ord_flow"
  names(riv)[toupper(names(riv)) == "DIS_AV_CMS"] <- "discharge"
  if (!"discharge" %in% names(riv)) riv$discharge <- NA_real_
  say("  ", nrow(riv), " reaches kept.")
  if (!nrow(riv)) return(NULL)

  if (is.na(sf::st_crs(riv))) sf::st_crs(riv) <- 4326
  riv <- sf::st_transform(riv, 4326)

  old_s2 <- suppressMessages(sf::sf_use_s2())
  on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  suppressMessages(sf::sf_use_s2(FALSE))

  if (is.finite(tol) && tol > 0) {
    say("  thinning the geometry (dTolerance = ", tol, " degrees) ...")
    gs <- tryCatch(suppressWarnings(
      sf::st_simplify(sf::st_geometry(riv), dTolerance = tol,
                      preserveTopology = FALSE)),
      error = function(e) NULL)
    if (!is.null(gs)) sf::st_geometry(riv) <- gs
  }

  # Each reach is attributed to ONE basin, via a point on the reach rather than
  # by intersecting the lines with the polygons. Intersecting would cut every
  # reach that crosses a watershed divide into two records and multiply the
  # layer; a representative point gives the one-to-one relation HydroRIVERS
  # itself describes between a reach and the sub-basin it resides in.
  # A reach falling in no basin keeps basin_id = NA and is KEPT: the Tedesco
  # delineation does not cover the whole land surface, and a river that runs
  # outside it is information about the delineation, not noise.
  say("  attributing the reaches to their basin ...")
  mid <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(riv)))
  hit <- suppressMessages(sf::st_intersects(mid, geom_ref))
  idx <- vapply(hit, function(z) if (length(z)) z[1] else NA_integer_, integer(1))
  out <- sf::st_sf(basin_id = basin_ids[idx],
                   ord_flow = as.integer(riv$ord_flow),
                   discharge = as.numeric(riv$discharge),
                   geometry = sf::st_geometry(riv))
  say("  ", sum(!is.na(out$basin_id)), " reach(es) inside a mapped basin, ",
      sum(is.na(out$basin_id)), " outside.")
  out
}

# Read the FISHMORPH publication workbook: the digitized landmarks and the raw
# body segments, keyed on "Genus.species".
#
# What is stored is the COORDINATES, not the photographs. 7,588 images are
# ~1 GB and belong on disk; twenty-three points per species are 3 MB and belong
# in the cache. The application therefore always knows the shape of a fish and
# only sometimes has its picture, which is the right way round: the drawing
# degrades to an outline rather than to nothing.
.fmb_read_landmarks <- function(path, say, db = NULL) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  is_xl <- ext %in% c("xlsx", "xlsm", "xls")
  if (is_xl && !requireNamespace("readxl", quietly = TRUE))
    stop("Reading the FISHMORPH workbook needs 'readxl'.\n",
         "  install.packages(\"readxl\")", call. = FALSE)
  sheets <- if (is_xl) readxl::excel_sheets(path) else NA_character_
  rd <- function(want) {
    if (!is_xl) return(utils::read.csv(path, sep = ";", check.names = FALSE,
                                       stringsAsFactors = FALSE))
    s <- sheets[tolower(sheets) == tolower(want)]
    if (!length(s)) return(NULL)
    as.data.frame(readxl::read_excel(path, sheet = s[1]), stringsAsFactors = FALSE)
  }

  lm <- rd("Global_Landmark")
  if (is.null(lm)) {
    say("  no 'Global_Landmark' sheet in ", basename(path), "; skipped.")
    return(NULL)
  }
  nm <- names(lm)
  kc <- nm[nm %in% c("Genus.species", "Genus_species", "Species", "species_key")][1]
  if (is.na(kc)) {
    say("  no species column in 'Global_Landmark'; skipped.")
    return(NULL)
  }
  key <- .fmb_key(gsub("[._]+", " ", as.character(lm[[kc]])))

  # Columns are "<n>_X" / "<n>_Y" with n the landmark number, and the numbers
  # are NOT contiguous: the published table holds 1-19, 22 and the derived 23.
  # The matrices are therefore indexed BY LANDMARK NUMBER, with holes, so that
  # P[6, ] is landmark 6 in the application exactly as it is in the digitizer.
  # Re-packing them densely would silently renumber the anatomy.
  gx <- grep("^[0-9]+_X$", nm, value = TRUE)
  gy <- grep("^[0-9]+_Y$", nm, value = TRUE)
  ix <- as.integer(sub("_X$", "", gx))
  iy <- as.integer(sub("_Y$", "", gy))
  if (!length(ix)) {
    say("  no '<n>_X' landmark columns found; skipped.")
    return(NULL)
  }
  np <- max(c(ix, iy))
  X <- matrix(NA_real_, nrow(lm), np)
  Y <- matrix(NA_real_, nrow(lm), np)
  for (k in seq_along(gx)) X[, ix[k]] <- suppressWarnings(as.numeric(lm[[gx[k]]]))
  for (k in seq_along(gy)) Y[, iy[k]] <- suppressWarnings(as.numeric(lm[[gy[k]]]))

  # --- the live store wins over the workbook ---------------------------------
  # The workbook is rewritten by the digitizer only every `xlsx_flush_every`
  # records, or on an explicit click, or on a clean session end; the journal --
  # and the DuckDB store built from it -- is written at every record. The two
  # therefore drift, and a workbook a week behind its journal is the normal
  # state of an active campaign, not an accident.
  #
  # That drift used to split the application in two: the ordination came from
  # the trait table (journal -> DuckDB -> CSV, hence current) while the specimen
  # panel and the comparison read the coordinates of the workbook (hence stale).
  # One correction, two answers on one screen. The DuckDB store now overrides
  # the workbook species by species -- the same precedence
  # build_fishmorph_landmark_table() already applies -- so everything the
  # application shows comes from one state of the measurements.
  if (!is.null(db) && nzchar(db) && file.exists(db)) {
    say("  merging the live DuckDB store over the workbook ...")
    dd <- tryCatch(.fm_lm_from_duckdb(db), error = function(e) {
      say("    could not read the store (", conditionMessage(e),
          "); the workbook is used alone.")
      NULL
    })
    if (!is.null(dd) && nrow(dd)) {
      dk <- .fmb_key(gsub("[._]+", " ", as.character(dd$Genus.species)))
      dgx <- grep("^[0-9]+_X$", names(dd), value = TRUE)
      dgy <- grep("^[0-9]+_Y$", names(dd), value = TRUE)
      dix <- as.integer(sub("_X$", "", dgx)); diy <- as.integer(sub("_Y$", "", dgy))
      np2 <- max(c(np, dix, diy))
      if (np2 > np) {                       # the store carries points 24-25
        X <- cbind(X, matrix(NA_real_, nrow(X), np2 - np))
        Y <- cbind(Y, matrix(NA_real_, nrow(Y), np2 - np))
        np <- np2
      }
      DX <- matrix(NA_real_, nrow(dd), np); DY <- matrix(NA_real_, nrow(dd), np)
      for (k in seq_along(dgx)) DX[, dix[k]] <- suppressWarnings(as.numeric(dd[[dgx[k]]]))
      for (k in seq_along(dgy)) DY[, diy[k]] <- suppressWarnings(as.numeric(dd[[dgy[k]]]))
      keepd <- !duplicated(dk) & nzchar(dk)
      dk <- dk[keepd]; DX <- DX[keepd, , drop = FALSE]; DY <- DY[keepd, , drop = FALSE]
      m <- match(dk, key)
      hit <- !is.na(m)
      if (any(hit)) { X[m[hit], ] <- DX[hit, ]; Y[m[hit], ] <- DY[hit, ] }
      if (any(!hit)) {
        key <- c(key, dk[!hit])
        X <- rbind(X, DX[!hit, , drop = FALSE])
        Y <- rbind(Y, DY[!hit, , drop = FALSE])
      }
      say("    ", sum(hit), " species refreshed from the store, ",
          sum(!hit), " added.")
    }
  }
  # A row of the landmark sheet is not the same thing as a digitized species.
  # 6,230 of the 9,557 rows carry NO coordinate at all -- they exist because the
  # sheet lists every species of the published table, digitized or not. Keeping
  # them would make the viewer believe it can draw those species, and it would
  # then show an empty frame under a name, which reads as a broken application
  # rather than as an undigitized fish.
  keep <- !duplicated(key) & nzchar(key)
  has_xy <- rowSums(!is.na(X)) > 0 | rowSums(!is.na(Y)) > 0
  n_empty <- sum(keep & !has_xy)
  keep <- keep & has_xy
  say("  ", sum(keep), " species actually digitized (", np, " point slots); ",
      n_empty, " row(s) of the sheet carry no coordinate and are dropped.")

  seg <- rd("Global_segments")
  segdf <- NULL
  if (!is.null(seg)) {
    sn <- names(seg)
    sk <- sn[sn %in% c("Genus.species", "Genus_species", "Species")][1]
    cand <- c("Bl", "Bd", "Hd", "CPd", "CFd", "Ed", "Jl", "Bbl", "PFl", "PFd",
              "Pp", "PFi", "Eh", "Mo", "CFs", "PFs", "MaxLength")
    have <- intersect(cand, sn)
    if (!is.na(sk) && length(have)) {
      segdf <- data.frame(species_key = .fmb_key(gsub("[._]+", " ", as.character(seg[[sk]]))),
                          stringsAsFactors = FALSE)
      for (h in have) segdf[[h]] <- suppressWarnings(as.numeric(seg[[h]]))
      segdf <- segdf[!duplicated(segdf$species_key) & nzchar(segdf$species_key), ,
                     drop = FALSE]
      say("  ", nrow(segdf), " species with raw segments (", length(have),
          " columns).")
    }
  }
  # Which of the nine ratios were MEASURED on a photograph, and which were
  # filled in. The published trait table carries no per-trait flag -- the
  # landmark table's `n_imputed` is a per-species COUNT, which cannot say WHICH
  # ratio was estimated -- so the status is derived from the raw segments, where
  # it is unambiguous: a ratio is measured exactly when both of the segments it
  # is built from were measured.
  #
  # Three states, not two. A species ABSENT from the segment sheet (811 of the
  # 8,970 in the published trait table) is `NA` here, not FALSE: nothing in the
  # data says whether its ratios were measured or estimated, and colouring it
  # either way would be an assertion the source does not support.
  #
  # A segment of ZERO is measured, not missing: `Mo = 0` is a terminal mouth and
  # `Bbl = 0` a fish without barbels, both observations. Only NA is an absence,
  # which is why the test is is.na() and not a truth test.
  meas <- NULL
  if (!is.null(segdf)) {
    meas <- matrix(NA, nrow(segdf), length(.FM_RATIOS),
                   dimnames = list(segdf$species_key, .FM_RATIOS))
    for (r in .FM_RATIOS) {
      ab <- .FM_RATIO_DEF[[r]]
      if (all(ab %in% names(segdf)))
        meas[, r] <- !is.na(segdf[[ab[1]]]) & !is.na(segdf[[ab[2]]])
    }
    say("  ratios mesures (campagne SEGMENTS) : ", sum(meas, na.rm = TRUE),
        " / ", sum(!is.na(meas)), ".")
  }

  # --- the same two quantities, recomputed from the LANDMARKS -----------------
  # The segment campaign and the landmark campaign are two measurements of the
  # same fish, and the only way to compare them is to derive them the same way:
  # a segment is the distance between its two points, a ratio is the quotient of
  # two segments, a ratio is measured when both its points pairs exist. Reading
  # the landmark side from the published landmark trait table instead would
  # compare a table to a table, not a method to a method -- and that table has
  # already been through imputation, so the per-ratio status would be lost.
  KX <- X[keep, , drop = FALSE]; KY <- Y[keep, , drop = FALSE]
  kk <- key[keep]
  dseg <- matrix(NA_real_, length(kk), length(.FM_SEGMENTS),
                 dimnames = list(kk, .FM_SEGMENTS))
  d2 <- function(i, j) sqrt((KX[, i] - KX[, j])^2 + (KY[, i] - KY[, j])^2)
  np_ok <- function(i) i <= ncol(KX)
  for (s in .FM_SEGMENTS) {
    ab <- .FM_SEGMENT_PAIRS[[s]]
    if (all(vapply(ab, np_ok, logical(1)))) dseg[, s] <- d2(ab[1], ab[2])
  }
  # Bl follows the BROKEN axis 1 -> 22 -> 2 when the curvature hinge is there.
  # The straight chord underestimates the body length of a bent fish -- by a
  # median 8.5 % on the species that carry point 22, per .FM_AXIS_HINGES in
  # schema.R -- and using it here would make the landmark method look
  # systematically "shorter" than the segment one for a reason that is an
  # artefact of the formula, not a difference between the campaigns.
  if (np_ok(22L)) {
    bent <- is.finite(KX[, 22]) & is.finite(KY[, 22]) &
      !(KX[, 22] == 0 & KY[, 22] == 0)
    if (any(bent)) dseg[bent, "Bl"] <- (d2(1L, 22L) + d2(22L, 2L))[bent]
  }

  meas_lm <- matrix(NA, length(kk), length(.FM_RATIOS),
                    dimnames = list(kk, .FM_RATIOS))
  seg_ok <- !is.na(dseg)
  for (r in .FM_RATIOS) {
    ab <- .FM_RATIO_DEF[[r]]
    if (all(ab %in% colnames(seg_ok))) meas_lm[, r] <- seg_ok[, ab[1]] & seg_ok[, ab[2]]
  }
  say("  ratios mesurables (campagne LANDMARKS) : ", sum(meas_lm, na.rm = TRUE),
      " / ", length(meas_lm), " sur ", length(kk), " especes digitalisees.")

  list(species_key = kk, X = KX, Y = KY,
       segments = segdf, measured = meas,
       segments_lm = dseg, measured_lm = meas_lm)
}

# Read the Tedesco-style occurrence table: one row per basin-by-species record,
# with a native/exotic status. Returns basin key, species key, status.
.fmb_read_tedesco <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  # These tables circulate as CSV exports of unknown provenance and the
  # published ones are not UTF-8 (a non-breaking space in a species author
  # field is enough). Reading them as UTF-8 fails on the byte rather than on
  # the row, so the fallback is explicit rather than left to the locale.
  d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE,
                                fileEncoding = "UTF-8"),
                error = function(e) NULL,
                warning = function(w) NULL)
  if (is.null(d) || !nrow(d))
    d <- utils::read.csv(path, stringsAsFactors = FALSE,
                         fileEncoding = "latin1")
  nm <- names(d)
  cb <- nm[tolower(nm) %in% c("basin", "drainage", "basin_name")][1]
  cs <- nm[tolower(nm) %in% c("species", "sp", "taxon")][1]
  ct <- nm[tolower(nm) %in% c("status", "occurrence_status")][1]
  if (is.na(cb) || is.na(cs))
    stop("The occurrence table needs a 'Basin' and a 'Species' column; found: ",
         paste(nm, collapse = ", "), call. = FALSE)
  out <- data.frame(basin = trimws(as.character(d[[cb]])),
                    species_key = .fmb_key(d[[cs]]),
                    status = if (is.na(ct)) NA_character_ else
                      tolower(trimws(as.character(d[[ct]]))),
                    stringsAsFactors = FALSE)
  out <- out[nzchar(out$basin) & nzchar(out$species_key), , drop = FALSE]
  unique(out)
}

# Read the Catalogue of Fishes freshwater export: one row per species, with the
# basins it occupies collapsed into a single semicolon-separated cell. The cell
# is exploded here rather than in the app, because a string split repeated in a
# reactive is a string split repeated for nothing.
.fmb_read_cas <- function(path, sheet = "CAS") {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Reading the Catalogue of Fishes workbook needs 'readxl'.\n",
         "  install.packages(\"readxl\")", call. = FALSE)
  d <- as.data.frame(readxl::read_excel(path, sheet = sheet),
                     stringsAsFactors = FALSE)
  nm <- names(d)
  pick <- function(...) {
    cand <- c(...)
    hit <- nm[tolower(nm) %in% cand]
    if (length(hit)) hit[1] else NA_character_
  }
  cs <- pick("valid_name", "species", "scientific_name")
  cb <- pick("basin", "basins", "drainage")
  if (is.na(cs) || is.na(cb))
    stop("The Catalogue of Fishes sheet needs a 'valid_name' and a 'basin' ",
         "column; found: ", paste(nm, collapse = ", "), call. = FALSE)
  sp <- .fmb_key(d[[cs]])
  bs <- strsplit(ifelse(is.na(d[[cb]]), "", as.character(d[[cb]])), ";", fixed = TRUE)
  n <- lengths(bs)
  out <- data.frame(basin = trimws(unlist(bs, use.names = FALSE)),
                    species_key = rep(sp, n),
                    status = NA_character_,
                    stringsAsFactors = FALSE)
  out <- out[nzchar(out$basin) & nzchar(out$species_key), , drop = FALSE]
  # Taxonomy carried by this source, kept aside: it fills the order and family
  # of species that FISHMORPH does not measure, which is what lets the
  # composition table stay informative where the functional space is empty.
  tax <- data.frame(
    species_key = sp,
    species = trimws(as.character(d[[cs]])),
    order  = if (!is.na(pick("order")))  as.character(d[[pick("order")]])  else NA_character_,
    family = if (!is.na(pick("family"))) as.character(d[[pick("family")]]) else NA_character_,
    stringsAsFactors = FALSE)
  tax <- tax[!duplicated(tax$species_key), , drop = FALSE]
  list(occ = unique(out), taxonomy = tax)
}

#' Build the cache read by the basin explorer
#'
#' Reads the four sources of the global drainage-basin view -- the basin
#' polygons, one or two species-by-basin occurrence tables, and the FISHMORPH
#' trait table -- ordinates the trait table once, computes the
#' functional-diversity indices of every basin under every source, and writes
#' the result as a single `.rds` file that [launch_fishmorph_basins()] reads in
#' under a second.
#'
#' Everything expensive happens here, on purpose. Nothing the application
#' computes depends on what the user clicks, so nothing the application shows
#' needs to be computed while they wait.
#'
#' @param shapefile Path to the basin polygons (`.shp`, with its sidecar
#'   `.dbf`/`.shx`/`.prj`, or any other format `sf::st_read()` accepts). The
#'   attribute table is expected to carry a basin name, and optionally a basin
#'   identifier, a country, a biogeographic realm and a centroid; the column
#'   names are matched loosely, and a missing centroid is computed.
#' @param occurrences Path to a Tedesco-style occurrence CSV with columns
#'   `Basin`, `Species` and, if available, `Status` (`native` / `exotic`).
#'   `NULL` to skip that source.
#' @param cas Path to the Catalogue of Fishes freshwater workbook, whose
#'   `basin` column holds semicolon-separated basin names. `NULL` to skip.
#' @param landmarks Optional path to the FISHMORPH publication workbook (sheets
#'   `Global_Landmark` and `Global_segments`, keyed on `Genus.species`). Adding
#'   it lets the application draw each species the way
#'   [launch_fishmorph_digitizer()] does -- landmarks, body segments and
#'   reference lines -- read-only. Only the COORDINATES are stored: the
#'   photographs stay on disk and are found at launch through
#'   [launch_fishmorph_basins()]'s `photos` argument.
#' @param size_traits Which SIZE variables join the nine dimensionless ratios in
#'   the ordination: any of `"MBl"` (maximum body length) and `"MBw"` (maximum
#'   body weight), or `character(0)` for none. Both by default.
#'
#'   This is a choice about what the functional space MEANS, not a technical
#'   setting. The nine ratios are quotients of two lengths measured on the same
#'   fish; `MBl` and `MBw` are species attributes from FishBase, stored as
#'   log10(x + 1) of centimetres and grams. Including them makes the first axis
#'   largely a size axis -- on the published table their loadings on PC1 are
#'   -0.51 and -0.51, ahead of every shape ratio -- and the resulting `FRic` is
#'   then not comparable with the FISHMORPH space of Brosse et al. (2021), which
#'   is built on the ratios alone. The two are also nearly collinear
#'   (Pearson r = 0.925), so asking for both lets size enter twice.
#'
#'   Whatever is asked for also enters the missing-data accounting: a species
#'   without a body length then leaves the space entirely rather than sitting
#'   in it at an invented size.
#' @param landmarks_db Optional path to the digitizer's DuckDB store
#'   ([fishmorph_build_db()]). Its coordinates OVERRIDE the workbook's, species
#'   by species -- the same precedence [build_fishmorph_landmark_table()]
#'   applies, and for the same reason: the workbook is rewritten every
#'   `xlsx_flush_every` records while the journal is written at every one, so a
#'   workbook lagging its journal is the normal state of an active campaign.
#'   Without this argument the specimen panel and the comparison can show
#'   measurements that the ordination has already left behind.
#' @param rivers Optional path to a HydroRIVERS layer (Lehner and Grill 2013),
#'   global `HydroRIVERS_v10.gdb` / `.shp` or a regional tile, downloaded from
#'   <https://www.hydrosheds.org>. `NULL` skips it and the application falls
#'   back on a raster tile overlay. Adding it makes the river network a piece of
#'   DATA carried by the cache rather than a third-party service that can be
#'   retired.
#' @param rivers_max_order Highest `ORD_FLOW` class kept, `1` to `10`. The
#'   class is a LOGARITHMIC DISCHARGE class running the counter-intuitive way:
#'   `1` is a reach carrying at least 100,000 cubic metres per second, `4` at
#'   least 100, `5` at least 10, `6` at least 1, `10` less than 0.001. Keeping
#'   `ORD_FLOW <= n` keeps the BIG rivers, and the cost climbs by roughly an
#'   order of magnitude per class: about 40,000 reaches at `4`, 200,000 at `5`,
#'   1.5 million at `6`. Defaults to `5`, which draws a network dense enough to
#'   read a mid-sized basin without making the cache unusable. The number of
#'   reaches actually kept is reported, so a first run tells you whether to go
#'   up or down.
#' @param rivers_tolerance Simplification tolerance of the reaches, in degrees.
#'   Defaults to `0.005`, roughly 550 m at the equator -- the resolution of the
#'   15 arc-second grid HydroRIVERS was extracted from, so the thinning removes
#'   vertices the source never resolved rather than geometry it did. `0`
#'   disables it.
#' @param traits Path to the trait table of the SEGMENT campaign. `NULL` uses
#'   [fishmorph_space_data()]`("segment")`, whose trait columns are ALREADY
#'   log10(x + 1) -- they are not transformed again here.
#' @param traits_landmark Path to the trait table of the LANDMARK campaign,
#'   the one [build_fishmorph_landmark_table()] produces. `NULL` uses
#'   [fishmorph_space_data()]`("landmark")` when it is bundled. Every campaign
#'   found gets its OWN ordination and its own set of basin indices, and the
#'   application switches between them.
#'
#'   The two spaces are not comparable point for point: independent principal
#'   component analyses on different species pools give different axes, so a
#'   species does not keep its coordinates across campaigns and neither do the
#'   hull-based indices. What they can be compared on is the RANKING of basins
#'   and the sign of a contrast, not the value of `FRic`.
#' @param out Path of the `.rds` cache to write.
#' @param simplify_keep Proportion of vertices kept when simplifying the
#'   polygons, in `(0, 1]`. `1` keeps the geometry untouched, which makes a very
#'   large cache and a sluggish map. Defaults to `0.03`.
#' @param axes_ric,axes_dist Axes used by the two families of index, passed to
#'   [fishmorph_basin_indices()].
#' @param n_axes Number of ordination axes stored in the cache. Defaults to `4`.
#' @param scale Passed to [stats::prcomp()]: standardise the nine ratios before
#'   the ordination. `TRUE` by default, and it should stay `TRUE` -- the ratios
#'   are dimensionless but not commensurate, and an unstandardised PCA lets the
#'   most variable ratio write the first axis on its own.
#' @param quiet Suppress the progress messages.
#' @return Invisibly, the cache (a list), which is also written to `out`.
#' @seealso [launch_fishmorph_basins()], [fishmorph_basin_indices()]
#' @examples
#' \dontrun{
#' prepare_fishmorph_basins(
#'   shapefile   = "Bassin/Basin_202412_3364.shp",
#'   occurrences = "Occurrence_Table.csv",
#'   cas         = "Bassin/cas_freshwater_202412.xlsx",
#'   out         = "Bassin/fishmorph_basins.rds")
#' }
#' @export
prepare_fishmorph_basins <- function(shapefile,
                                     occurrences = NULL,
                                     cas = NULL,
                                     landmarks = NULL,
                                     landmarks_db = NULL,
                                     rivers = NULL,
                                     rivers_max_order = 5L,
                                     rivers_tolerance = 0.005,
                                     traits = NULL,
                                     traits_landmark = NULL,
                                     size_traits = c("MBl", "MBw"),
                                     out = "fishmorph_basins.rds",
                                     simplify_keep = 0.03,
                                     axes_ric = 1:2,
                                     axes_dist = 1:4,
                                     n_axes = 4L,
                                     scale = TRUE,
                                     quiet = FALSE) {
  .fm_require(.FMB_PREP_DEPS, "Preparing the basin cache")
  if (is.null(occurrences) && is.null(cas))
    stop("At least one occurrence source is needed: `occurrences` (Tedesco) ",
         "or `cas` (Catalogue of Fishes).", call. = FALSE)
  if (!is.numeric(simplify_keep) || length(simplify_keep) != 1 ||
      simplify_keep <= 0 || simplify_keep > 1)
    stop("`simplify_keep` must be a single number in (0, 1].", call. = FALSE)
  say <- function(...) if (!quiet) message(...)

  # -- 1. polygons -----------------------------------------------------------
  say("Reading the basin polygons ...")
  g <- sf::st_read(shapefile, quiet = TRUE)
  g <- sf::st_zm(g, drop = TRUE, what = "ZM")
  if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 4326)

  nm <- setdiff(names(g), attr(g, "sf_column"))
  low <- tolower(nm)
  pick <- function(...) {
    cand <- c(...)
    hit <- nm[low %in% cand]
    if (length(hit)) hit[1] else NA_character_
  }
  c_name <- pick("basin", "basin_name", "bas_name", "name")
  c_id   <- pick("basin_d", "basin_id", "bas_id", "id")
  c_ctry <- pick("country", "countries", "cntry")
  c_eco  <- pick("bggrph_", "biogeographic_realm", "realm", "ecoregion",
                 "ecoreg", "biogeo")
  c_lon  <- pick("cntr_ln", "centroid_lon", "lon", "longitude", "x")
  c_lat  <- pick("cntr_lt", "centroid_lat", "lat", "latitude", "y")
  c_nsp  <- pick("n_specs", "n_species", "nspec", "richness")
  if (is.na(c_name))
    stop("No basin-name column found in the shapefile; columns are: ",
         paste(nm, collapse = ", "), call. = FALSE)

  basins <- data.frame(
    basin    = trimws(as.character(g[[c_name]])),
    basin_id = if (is.na(c_id)) NA_character_ else trimws(as.character(g[[c_id]])),
    country  = if (is.na(c_ctry)) NA_character_ else trimws(as.character(g[[c_ctry]])),
    ecoregion = if (is.na(c_eco)) NA_character_ else trimws(as.character(g[[c_eco]])),
    n_specs_source = if (is.na(c_nsp)) NA_integer_ else
      suppressWarnings(as.integer(g[[c_nsp]])),
    stringsAsFactors = FALSE)
  if (anyNA(basins$basin_id) || !all(nzchar(basins$basin_id)))
    basins$basin_id <- sprintf("B%05d", seq_len(nrow(basins)))

  if (!is.na(c_lon) && !is.na(c_lat)) {
    basins$lon <- suppressWarnings(as.numeric(g[[c_lon]]))
    basins$lat <- suppressWarnings(as.numeric(g[[c_lat]]))
  } else {
    say("  no centroid columns: computing them from the geometry ...")
    # Same reason as in .fmb_simplify(): s2 refuses a layer holding one
    # self-intersecting ring, and a representative point is a display quantity.
    old_s2 <- suppressMessages(sf::sf_use_s2())
    suppressMessages(sf::sf_use_s2(FALSE))
    ct <- suppressWarnings(sf::st_coordinates(sf::st_point_on_surface(
      sf::st_make_valid(g))))
    suppressMessages(sf::sf_use_s2(old_s2))
    basins$lon <- ct[, 1]
    basins$lat <- ct[, 2]
  }

  say("Simplifying the polygons (keep = ", simplify_keep, ") ...")
  gs <- .fmb_simplify(sf::st_geometry(g), simplify_keep)
  geom <- sf::st_sf(basin_id = basins$basin_id, basin = basins$basin,
                    geometry = gs)

  # Countries arrive as a single cell listing several of them for a
  # transboundary basin. The exploded long form is what the country selector
  # needs; the original string is kept for display, because "Brazil;Peru" is
  # the truth about the Amazon and a single country would not be.
  ctry_long <- local({
    parts <- strsplit(ifelse(is.na(basins$country), "", basins$country), ";",
                      fixed = TRUE)
    data.frame(basin_id = rep(basins$basin_id, lengths(parts)),
               country = trimws(unlist(parts, use.names = FALSE)),
               stringsAsFactors = FALSE)
  })
  ctry_long <- unique(ctry_long[nzchar(ctry_long$country), , drop = FALSE])

  key2id <- stats::setNames(basins$basin_id, .fmb_key(basins$basin))

  # The rivers are attributed against the FULL-resolution basin geometry, not
  # the simplified one: a reach a kilometre from a divide would be assigned to
  # the wrong basin by a boundary that has been moved for the sake of the
  # browser. Simplification is a display decision and must not reach the data.
  RIV <- .fmb_read_rivers(rivers, rivers_max_order, rivers_tolerance,
                          sf::st_geometry(g), basins$basin_id, say)

  LM <- NULL
  if (!is.null(landmarks)) {
    say("Reading the FISHMORPH landmark workbook ...")
    # A workbook older than the journal it is supposed to mirror is the single
    # most common cause of "I corrected it and the app still shows the old
    # value". It is stated, with both dates, rather than left to be discovered.
    if (!is.null(landmarks_db) && nzchar(landmarks_db) &&
        file.exists(landmarks_db) && file.exists(landmarks) &&
        file.mtime(landmarks_db) > file.mtime(landmarks))
      say("  NOTE: the workbook (", format(file.mtime(landmarks), "%Y-%m-%d %H:%M"),
          ") is OLDER than the store (",
          format(file.mtime(landmarks_db), "%Y-%m-%d %H:%M"),
          "); the store takes precedence, as it should.")
    LM <- .fmb_read_landmarks(landmarks, say, db = landmarks_db)
  }

  # -- 2. traits: one table per MEASUREMENT CAMPAIGN --------------------------
  # The segment campaign and the landmark re-digitization are two measurements
  # of the same fauna, and they are kept apart all the way down: their own trait
  # table, their own ordination, their own diversity indices. Pooling them would
  # mean an ordination whose axes are partly a method effect, and a basin whose
  # richness changed because its species were measured twice rather than because
  # anything ecological differs.
  #
  # The two are NOT comparable point for point: independent PCAs on different
  # species pools give different axes, so a fish does not keep its coordinates
  # when the method is switched. The application says so where it matters.
  size_traits <- intersect(as.character(size_traits), .FMB_SIZE)
  read_traits <- function(path, label) {
    if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
    d <- utils::read.csv(path, sep = ";", stringsAsFactors = FALSE)
    miss <- setdiff(.FMB_TRAITS, names(d))
    if (length(miss)) {
      say("  the \"", label, "\" table lacks ", paste(miss, collapse = ", "),
          "; skipped.")
      return(NULL)
    }
    # A size column that was ASKED FOR and is absent is not silently dropped:
    # the campaign would then be ordinated on a different set of variables from
    # its neighbour, and nothing on screen would say so.
    ms <- setdiff(size_traits, names(d))
    if (length(ms))
      stop("The \"", label, "\" trait table has no ", paste(ms, collapse = ", "),
           " column, but size_traits asks for it.\n  Either rebuild the table ",
           "with build_fishmorph_landmark_table(fishbase_size = TRUE), ",
           "or drop it from `size_traits`.", call. = FALSE)
    d$species_key <- .fmb_key(d$Species)
    d[!duplicated(d$species_key), , drop = FALSE]
  }

  say("Reading the FISHMORPH trait tables ...")
  tp <- traits %|N|% fishmorph_space_data("segment")
  tl <- traits_landmark %|N|% fishmorph_space_data("landmark")
  FMS <- list(segment = read_traits(tp, "segment"),
              landmark = read_traits(tl, "landmark"))
  FMS <- FMS[!vapply(FMS, is.null, logical(1))]
  if (!length(FMS))
    stop("No usable FISHMORPH trait table.\n  segment: ", tp,
         "\n  landmark: ", tl, call. = FALSE)
  say("  campaigns available: ", paste(names(FMS), collapse = ", "), ".")

  # ONE identity register for every species any source mentions, so that a name,
  # an order and an IUCN status are stored once. What varies by campaign -- what
  # was measured, and where it lands in that campaign's space -- lives in
  # matrices ALIGNED on this register, never in extra columns of it.
  ident <- do.call(rbind, lapply(FMS, function(d) data.frame(
    species_key = d$species_key, species = d$Species,
    order = d$Order %|N|% NA_character_, family = d$Family %|N|% NA_character_,
    genus = d$Genus %|N|% NA_character_, iucn = d$IUCN %|N|% NA_character_,
    stringsAsFactors = FALSE)))
  species <- ident[!duplicated(ident$species_key), , drop = FALSE]
  rownames(species) <- NULL

  # -- 3. occurrences --------------------------------------------------------
  sources <- list()
  if (!is.null(occurrences)) {
    say("Reading the occurrence table (Tedesco) ...")
    o <- .fmb_read_tedesco(occurrences)
    o$basin_id <- unname(key2id[.fmb_key(o$basin)])
    lost <- sum(is.na(o$basin_id))
    if (lost)
      say("  ", lost, " record(s) name a basin absent from the shapefile ",
          "(", length(unique(o$basin[is.na(o$basin_id)])), " basin name(s)); ",
          "they are dropped.")
    sources$tedesco <- o[!is.na(o$basin_id), c("basin_id", "species_key", "status")]
  }
  extra_tax <- NULL
  if (!is.null(cas)) {
    say("Reading the Catalogue of Fishes workbook ...")
    cc <- .fmb_read_cas(cas)
    o <- cc$occ
    o$basin_id <- unname(key2id[.fmb_key(o$basin)])
    lost <- sum(is.na(o$basin_id))
    if (lost)
      say("  ", lost, " record(s) name a basin absent from the shapefile; ",
          "they are dropped.")
    sources$cas <- o[!is.na(o$basin_id), c("basin_id", "species_key", "status")]
    extra_tax <- cc$taxonomy
  }

  # Species present in an occurrence table but absent from FISHMORPH are kept
  # in the species register, flagged has_traits = FALSE. They must appear in
  # the composition table -- a basin whose 80 species include 20 measured ones
  # is a different object from a basin of 20 species, and an interface that
  # showed only the measured ones would hide exactly the bias the coverage
  # column exists to expose.
  all_keys <- unique(unlist(lapply(sources, function(z) z$species_key),
                            use.names = FALSE))
  new_keys <- setdiff(all_keys, species$species_key)
  if (length(new_keys)) {
    add <- data.frame(species_key = new_keys,
                      species = new_keys, order = NA_character_,
                      family = NA_character_, genus = NA_character_,
                      iucn = NA_character_,
                      stringsAsFactors = FALSE)
    if (!is.null(extra_tax)) {
      m <- match(add$species_key, extra_tax$species_key)
      hit <- !is.na(m)
      add$species[hit] <- extra_tax$species[m[hit]]
      add$order[hit]   <- extra_tax$order[m[hit]]
      add$family[hit]  <- extra_tax$family[m[hit]]
    }
    # A key is lower case by construction. Where no source gives the published
    # spelling back, the initial is restored rather than showing a binomial in
    # lower case in a species selector next to properly capitalised ones.
    raw <- add$species == add$species_key
    add$species[raw] <- sub("^(.)", "\\U\\1", add$species[raw], perl = TRUE)
    add$genus <- sub(" .*$", "", add$species)
    species <- rbind(species, add[, names(species), drop = FALSE])
    say("  ", length(new_keys), " occurring species have no FISHMORPH ",
        "morphology; kept in the register, excluded from the indices.")
  }
  rownames(species) <- NULL
  sp_row <- stats::setNames(seq_len(nrow(species)), species$species_key)
  n_sp <- nrow(species)

  # -- 3b. one ordination per campaign, aligned on the register ---------------
  # Each campaign is standardised and ordinated on ITS OWN pool of complete
  # species. Fitting one PCA and projecting the other into it would make the two
  # comparable, which is a different question from the one asked here and a
  # different guarantee: the choice made is two independent spaces, so a
  # coordinate means nothing across campaigns and the interface must never
  # invite that comparison.
  # The variables the ordination runs on: the nine shape ratios, plus whatever
  # `size_traits` asks for. `has` is complete.cases over ALL of them, so asking
  # for size makes a species without a body length drop out of the space
  # entirely rather than sit in it at an invented size -- which is the point of
  # counting size in the NA accounting.
  VARS <- c(.FMB_TRAITS, size_traits)
  traitsets <- list()
  for (nm in names(FMS)) {
    d <- FMS[[nm]]
    i <- match(d$species_key, species$species_key)
    ratios <- matrix(NA_real_, n_sp, length(VARS), dimnames = list(NULL, VARS))
    ratios[i, ] <- as.matrix(d[, VARS, drop = FALSE])
    has <- stats::complete.cases(ratios)
    if (sum(has) < 3L) {
      say("  campaign \"", nm, "\": fewer than three complete species; skipped.")
      next
    }
    pca <- stats::prcomp(ratios[has, , drop = FALSE], center = TRUE,
                         scale. = scale)
    k <- min(as.integer(n_axes), ncol(pca$x))
    sc <- matrix(NA_real_, n_sp, k,
                 dimnames = list(NULL, paste0("PC", seq_len(k))))
    sc[has, ] <- pca$x[, seq_len(k), drop = FALSE]
    ve <- (pca$sdev^2 / sum(pca$sdev^2))[seq_len(k)]
    say("  campaign \"", nm, "\": ", sum(has), " species complete on ",
        length(VARS), " variable(s), PC1-PC2 = ",
        round(100 * sum(ve[1:min(2, k)]), 1), " % of variance.")
    if (length(size_traits)) {
      l1 <- abs(pca$rotation[size_traits, 1])
      say("    size loading on PC1: ",
          paste(sprintf("%s = %.2f", size_traits, l1), collapse = ", "),
          " -- PC1 is a size axis to that extent.")
    }
    # The functional richness of the WHOLE pool, on the plane FRic is measured
    # in. It is the denominator that turns a hull area -- a number in squared
    # score units, meaningless on its own and incomparable between campaigns --
    # into a share of the morphospace the world's freshwater fishes occupy.
    #
    # The ratio is bounded in [0, 100] by construction and not by convention:
    # the hull of a subset is contained in the hull of the set, so no basin can
    # exceed the pool it is drawn from. That is what makes a fixed 0-100 scale
    # honest, where a scale fitted on the observed range would silently redefine
    # its own maximum every time the data changed.
    fric_world <- .fmb_hull_area(sc[has, axes_ric, drop = FALSE])
    say("    FRic of the whole pool (denominator of the % scale): ",
        format(fric_world, digits = 5))

    traitsets[[nm]] <- list(
      vars = VARS, size_traits = size_traits, fric_world = fric_world,
      # The ratios are stored AS READ, i.e. still log10(x + 1). The
      # back-transformation belongs where the number is displayed, so that the
      # cache never holds two versions of the same quantity.
      ratios = ratios, has = has, scores = sc, n_axes = k,
      var_explained = ve,
      loadings = pca$rotation[, seq_len(k), drop = FALSE],
      n_species = sum(has), source_file = if (identical(nm, "segment")) tp else tl)
  }
  if (!length(traitsets))
    stop("No campaign has enough complete species to be ordinated.",
         call. = FALSE)

  # -- 4. indices ------------------------------------------------------------
  # One table per source AND per status subset. The alternative -- one table
  # for the pooled assemblage, and a recomputation when the user asks for
  # natives only -- would put three thousand convex hulls behind a radio
  # button. The subsets are cheap because the exotic one is small, and
  # precomputing them is what keeps every number in the interface a number
  # that was computed the same way.
  # -- 3c. the two campaigns, confronted --------------------------------------
  # This is the one place where the two campaigns share a coordinate system, and
  # it has to be: comparing two configurations means superimposing them, and two
  # independent ordinations cannot be superimposed. compare_segments_landmarks()
  # freezes ONE ordination on the segment ratios and projects both sets of
  # measurements into it, so a displacement between the two clouds is a
  # displacement of the fish and not a change of basis. The rest of the
  # application keeps its two separate spaces, for the opposite reason: each
  # campaign must be readable on its own terms.
  #
  # Computed here, once, like everything else -- a Procrustes test with 999
  # permutations on three thousand species does not belong behind a tab click.
  comparison <- NULL
  if (!is.null(LM) && !is.null(LM$segments) && is.matrix(LM$segments_lm)) {
    say("Confronting the two campaigns (shared ordination + Procrustes) ...")
    lmseg <- as.data.frame(LM$segments_lm, stringsAsFactors = FALSE)
    lmseg$species_key <- rownames(LM$segments_lm)
    pub <- LM$segments
    # The grouping is only used to colour the two clouds; taking it from the
    # register rather than from the sheet keeps one spelling of an order name.
    pub$Order <- species$order[match(pub$species_key, species$species_key)]
    if (!requireNamespace("vegan", quietly = TRUE))
      say("  'vegan' is not installed: the Procrustes test will be missing. ",
          "install.packages(\"vegan\")")
    comparison <- tryCatch(
      compare_segments_landmarks(landmarks = lmseg, published = pub,
                                 id_col = "species_key", space = TRUE,
                                 group_col = "Order"),
      error = function(e) {
        say("  comparison failed (", conditionMessage(e), "); skipped.")
        NULL
      })
    if (!is.null(comparison)) {
      p <- comparison$procrustes
      say("  ", nrow(comparison$ratios), " species measured in both campaigns",
          if (is.null(p)) "." else
            sprintf("; Procrustes correlation = %.3f, p = %.3f.",
                    p$correlation, p$significance))
    }
  }

  # The table gained a `campaign` column rather than a third level of nesting.
  # A basin's richness does not depend on how its fishes were measured, but its
  # coverage and its five indices do, so the row is duplicated per campaign and
  # every number in it was computed under the campaign named beside it.
  idx <- list()
  for (src in names(sources)) {
    o <- sources[[src]]
    o$row <- unname(sp_row[o$species_key])
    subsets <- list(all = rep(TRUE, nrow(o)))
    if (!all(is.na(o$status))) {
      subsets$native <- o$status %in% "native"
      subsets$exotic <- o$status %in% "exotic"
    }
    parts <- list()
    for (camp in names(traitsets)) {
      TS <- traitsets[[camp]]
      for (sub in names(subsets)) {
        oo <- o[subsets[[sub]], , drop = FALSE]
        if (!nrow(oo)) next
        say("Computing the indices of ", length(unique(oo$basin_id)),
            " basins (occurrences: ", src, ", campaign: ", camp,
            ", status: ", sub, ") ...")
        spl <- split(oo$row, oo$basin_id)
        stat <- split(oo$status, oo$basin_id)
        res <- vector("list", length(spl))
        for (i in seq_along(spl)) {
          rows <- unique(spl[[i]])
          rows <- rows[!is.na(rows)]
          keep <- rows[TS$has[rows]]
          fd <- fishmorph_basin_indices(TS$scores[keep, , drop = FALSE],
                                        axes_ric = axes_ric,
                                        axes_dist = axes_dist)
          st <- stat[[i]]
          res[[i]] <- data.frame(
            campaign = camp,
            status = sub,
            basin_id = names(spl)[i],
            n_species = length(rows),
            n_native = if (all(is.na(st))) NA_integer_ else sum(st %in% "native"),
            n_exotic = if (all(is.na(st))) NA_integer_ else sum(st %in% "exotic"),
            n_traits = length(keep),
            coverage = if (length(rows)) length(keep) / length(rows) else NA_real_,
            fd[, c("FRic", "FDiv", "FDis", "FEve", "Rao")],
            # Share of the world's functional space. pmin() guards the last
            # decimal only: a subset hull cannot exceed its pool, but the
            # shoelace formula run twice on different vertex sets can land on
            # 100.0000001, and a legend capped at 100 would drop that basin.
            FRic_pct = pmin(100, 100 * fd$FRic / TS$fric_world),
            stringsAsFactors = FALSE)
          if (!quiet && i %% 500 == 0) message("      ", i, " / ", length(spl))
        }
        parts[[paste(camp, sub)]] <- do.call(rbind, res)
      }
    }
    idx[[src]] <- do.call(rbind, parts)
    rownames(idx[[src]]) <- NULL
  }

  cache <- list(
    meta = list(
      created = Sys.time(),
      format = .FMB_CACHE_FORMAT,
      package_version = as.character(utils::packageVersion("Rfishmorph")),
      sources = list(shapefile = shapefile, occurrences = occurrences,
                     cas = cas, traits = tp, traits_landmark = tl,
                     rivers = rivers, landmarks = landmarks,
                     landmarks_db = landmarks_db),
      settings = list(traits = .FMB_TRAITS, size_traits = size_traits,
                      vars = VARS, scale = scale, n_axes = n_axes,
                      axes_ric = axes_ric, axes_dist = axes_dist,
                      simplify_keep = simplify_keep,
                      campaigns = names(traitsets),
                      rivers_max_order = if (is.null(RIV)) NULL else
                        as.integer(rivers_max_order))),
    basins = basins,
    geometry = geom,
    rivers = RIV,
    landmarks = LM,
    comparison = comparison,
    countries = ctry_long,
    species = species,
    traits = traitsets,
    occ = sources,
    indices = idx)
  class(cache) <- "fishmorph_basin_cache"

  say("Writing ", out, " ...")
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  saveRDS(cache, out, compress = "xz")
  say("Done: ", nrow(basins), " basins, ", nrow(species), " species, ",
      length(sources), " occurrence source(s).")
  invisible(cache)
}

#' @param x A `"fishmorph_basin_cache"`.
#' @param ... Unused.
#' @return `print()` invisibly returns `x`.
#' @export
#' @rdname prepare_fishmorph_basins
print.fishmorph_basin_cache <- function(x, ...) {
  cat("<fishmorph_basin_cache>\n")
  cat(sprintf("  built %s with Rfishmorph %s\n",
              format(x$meta$created, "%Y-%m-%d %H:%M"),
              x$meta$package_version))
  cat(sprintf("  %d basins, %d species in the register\n",
              nrow(x$basins), nrow(x$species)))
  for (nm in names(x$traits)) {
    ts <- x$traits[[nm]]
    cat(sprintf("  campaign %-9s %5d species ordinated, %d axes, PC1-PC2 = %.1f%%\n",
                nm, ts$n_species, ts$n_axes,
                100 * sum(ts$var_explained[seq_len(min(2, ts$n_axes))])))
  }
  for (s in names(x$occ))
    cat(sprintf("  source %-8s %7d records, %5d basins, %5d species\n",
                s, nrow(x$occ[[s]]), length(unique(x$occ[[s]]$basin_id)),
                length(unique(x$occ[[s]]$species_key))))
  if (!is.null(x$rivers))
    cat(sprintf("  HydroRIVERS: %d reaches (ORD_FLOW <= %s), %d inside a basin\n",
                nrow(x$rivers), x$meta$settings$rivers_max_order,
                sum(!is.na(x$rivers$basin_id))))
  if (!is.null(x$comparison)) {
    p <- x$comparison$procrustes
    cat(sprintf("  comparison: %d species measured twice%s\n",
                nrow(x$comparison$ratios),
                if (is.null(p)) " (no Procrustes: 'vegan' absent)" else
                  sprintf(", Procrustes r = %.3f (p = %.3f)",
                          p$correlation, p$significance)))
  }
  if (!is.null(x$landmarks))
    cat(sprintf("  landmarks: %d species, %d point slots%s\n",
                length(x$landmarks$species_key), ncol(x$landmarks$X),
                if (is.null(x$landmarks$segments)) "" else
                  sprintf(", %d with raw segments",
                          nrow(x$landmarks$segments))))
  invisible(x)
}


# =============================================================================
# 3. The launcher
# =============================================================================

#' Global drainage-basin explorer
#'
#' Launches the 'shiny' application that reads the world's drainage basins
#' through the FISHMORPH morphological space: a choropleth world map of the
#' basins with their species composition and their functional richness, the
#' global functional space reacting to the basin clicked on that map, and a
#' sortable table of every basin with its country, its biogeographic realm, its
#' species count and its functional-diversity indices.
#'
#' The application computes no morphology and no index of its own: it reads the
#' cache written by [prepare_fishmorph_basins()], which is where the shapefile,
#' the occurrence tables and the ordination were resolved once. What it adds is
#' the reading of them -- by basin, by species, by country, by realm.
#'
#' Selection works on two levels, deliberately kept apart. The FILTERS in the
#' sidebar -- a species, a country, a realm, a list of basins -- light up every
#' basin that satisfies them, and the functional space then shows those basins
#' pooled. Clicking one polygon FOCUSES it, and the space, the composition
#' table and the summary then describe that basin alone. Selecting a species
#' additionally marks it in the space, so that "where does this fish live, and
#' where does it sit morphologically" is one question rather than two.
#'
#' @param cache Path to the `.rds` written by [prepare_fishmorph_basins()].
#'   `NULL` follows `getOption("Rfishmorph.basin_cache")`, then looks for
#'   `fishmorph_basins.rds` in the working directory.
#' @param photos Optional folder of species photographs, the same one
#'   [launch_fishmorph_digitizer()] reads (`Genus_species.jpg`, matched on a
#'   normalised name). The photographs stay LOCAL and are never copied into the
#'   cache: 7,588 of them are about a gigabyte, and the panel that displays them
#'   only ever needs one at a time. Without this argument the specimen panel
#'   still draws the landmarks and the segments, on a blank background.
#' @param launch.browser Where the application opens. `TRUE` (default) or
#'   `"browser"` forces the system browser, past the RStudio Viewer pane;
#'   `"viewer"` restores the pane; `FALSE` opens nothing and prints the URL; a
#'   function is used as given.
#' @param ... Passed to [shiny::runApp()] (for example `port`).
#' @return Invisibly `NULL`; called for its side effect.
#' @seealso [prepare_fishmorph_basins()] to build the cache,
#'   [fishmorph_basin_indices()] for the indices it reports,
#'   [launch_fishmorph_space()] for the species-level morphological space.
#' @examples
#' \dontrun{
#' prepare_fishmorph_basins("Bassin/Basin_202412_3364.shp",
#'                          occurrences = "Occurrence_Table.csv",
#'                          cas = "Bassin/cas_freshwater_202412.xlsx",
#'                          out = "Bassin/fishmorph_basins.rds")
#' launch_fishmorph_basins("Bassin/fishmorph_basins.rds")
#' }
#' @export
launch_fishmorph_basins <- function(cache = NULL, photos = NULL,
                                    launch.browser = TRUE, ...) {
  .fm_require(.FMB_APP_DEPS, "The drainage-basin explorer")

  appdir <- system.file("shiny", "fishmorph_basins", package = "Rfishmorph")
  if (!nzchar(appdir) || !file.exists(file.path(appdir, "app.R")))
    stop("Application not found in the package. Reinstall 'Rfishmorph'.",
         call. = FALSE)

  if (is.null(cache)) cache <- getOption("Rfishmorph.basin_cache", NULL)
  if (is.null(cache) && file.exists("fishmorph_basins.rds"))
    cache <- "fishmorph_basins.rds"
  if (is.null(cache) || !nzchar(cache))
    stop("No basin cache given. Build one first:\n",
         "  prepare_fishmorph_basins(shapefile = \"Basin_202412_3364.shp\",\n",
         "                           occurrences = \"Occurrence_Table.csv\",\n",
         "                           out = \"fishmorph_basins.rds\")",
         call. = FALSE)
  if (!file.exists(cache))
    stop("Basin cache not found: ", cache, call. = FALSE)

  if (!is.null(photos)) {
    if (!dir.exists(photos))
      stop("Photograph folder not found: ", photos, call. = FALSE)
    photos <- normalizePath(photos, winslash = "/")
  }
  old <- options(Rfishmorph.basin_cache = normalizePath(cache, winslash = "/"),
                 Rfishmorph.basin_photos = photos)
  on.exit(options(old), add = TRUE)
  shiny::runApp(appdir, launch.browser = .fm_browser(launch.browser), ...)
  invisible(NULL)
}
