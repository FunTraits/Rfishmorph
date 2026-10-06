# =============================================================================
# database.R -- derived DuckDB database + validation
#
# DERIVED database (DuckDB) of the FISHMORPH landmarks.
#
# POSITION IN THE CHAIN
#
#   TSV journals  -->  DuckDB database  -->  Parquet / CSV
#   (append-only,      (constraints,          (archival artefacts, citable,
#    SOURCE OF          types, views,          readable without any specific
#    TRUTH)             SQL queries)           software)
#
# The database is DERIVED and DISPOSABLE: fishmorph_build_db() rebuilds it
# entirely from the journals in a few seconds. That is what makes it acceptable
# to put an embedded engine inside a synchronised folder -- a database corrupted
# by OneDrive is no longer a data incident, merely a rebuild. The journals
# themselves are never rewritten.
#
# COROLLARY: NEVER write directly into the database. Every entry goes through
# the app (hence through the journal), failing which the next rebuild will erase
# it.
#
# WHAT THE DATABASE BRINGS THAT A TSV CANNOT
#   * types (a coordinate is a DOUBLE, not the string "500,5");
#   * constraints: landmark within 1..25, a single point per (specimen,
#     landmark), status within a closed vocabulary, strictly positive scale;
#   * a relational model: record / specimen / observation;
#   * ad hoc queries in SQL or through dbplyr, without loading everything into
#     memory.
#
# WHAT IT DOES NOT BRING, AND WHAT HAS TO BE CODED: MORPHOMETRIC plausibility. A
# set of coordinates can satisfy every SQL constraint and describe an impossible
# fish. fishmorph_validate() therefore confronts each specimen with the
# empirical envelope of the 9,556 reference species (see .FM_RATIO_BOUNDS).
#
# ARCHIVING: a .duckdb file is not a repository format. DuckDB's on-disk format
# is only guaranteed backward compatible from version 1.0 onwards, and a
# repository such as Zenodo expects plain text or Parquet. We therefore export
# systematically, and it is those exports that are citable.
#
# DEPENDENCIES: duckdb, DBI. fishmorph_landmark_store.R must be loaded.
# =============================================================================


# --- empirical envelope of the FISHMORPH proportions -------------------------
# 0.1 % and 99.9 % quantiles of the segment/Bl ratio, computed on
# FishMORPH_seg.csv (n = 6,492 to 7,706 species depending on the segment,
# strictly positive values). These are bounds of PLAUSIBILITY, not of validity:
# a ratio outside the envelope flags a specimen to LOOK AT, not a specimen to
# reject -- a genuinely atypical species (eel-like, sunfish) may legitimately
# fall outside it. Hence the "warning" severity rather than "error".
.FM_RATIO_BOUNDS <- data.frame(
  segment = c("Bd", "Hd", "Eh2", "Mo2", "PFi2", "PFl", "Ed", "Jl", "CPd", "CFd"),
  a       = c( 3L,   5L,   7L,    1L,    10L,    10L,   13L,  1L,   16L,   18L),
  b       = c( 4L,   6L,   8L,    9L,    11L,    12L,   14L,  15L,  17L,   19L),
  lo      = c(0.0387, 0.0210, 0.0208, 0.0108, 0.0121, 0.0239, 0.0043, 0.0072,
              0.0062, 0.0199),
  med     = c(0.2480, 0.1382, 0.1372, 0.1152, 0.0745, 0.1829, 0.0589, 0.0559,
              0.1055, 0.2593),
  hi      = c(0.7487, 0.4197, 0.4296, 0.4238, 0.3498, 0.4168, 0.1386, 0.2263,
              0.2162, 0.5342),
  stringsAsFactors = FALSE
)

# A FUNCTION and not a constant, for one reason: the status vocabulary belongs
# to the journal (`.FM_JOURNAL_STATUS`, R/journal.R) and must be read from
# there, but R sources the package files in alphabetical order and database.R
# comes before journal.R -- a constant would be evaluated while the vocabulary
# does not yet exist. Building the DDL at call time also makes the drift that
# this file used to carry impossible: the CHECK listed four statuses while the
# journal wrote five, so every specimen carrying an "adjusted" point -- i.e.
# every one where a FISHMORPH convention snapped 3 or 4, or projected 4 onto
# the mid axis since 0.6.0 -- broke the whole insertion batch.
.fm_ddl <- function() c(
# `mode` and `ts` are NULLABLE on purpose: a journal written by an earlier
# version may not carry them. Better to record "unknown provenance" than to
# refuse the data or, worse, invent a plausible value for it. The CHECK stays in
# place to forbid any value OUTSIDE the vocabulary.
"CREATE TABLE record (
   record_id    VARCHAR PRIMARY KEY,
   ts           TIMESTAMP,
   operator     VARCHAR NOT NULL,
   app_version  VARCHAR,
   mode         VARCHAR CHECK (mode IS NULL OR
                               mode IN ('reconstruct','correct','new')),
   target_sheet VARCHAR
 )",
"CREATE TABLE specimen (
   specimen_id  VARCHAR PRIMARY KEY,
   species      VARCHAR NOT NULL,
   photo_file   VARCHAR,
   img_w        INTEGER CHECK (img_w  IS NULL OR img_w  > 0),
   img_h        INTEGER CHECK (img_h  IS NULL OR img_h  > 0),
   ruler_mm     DOUBLE  CHECK (ruler_mm  IS NULL OR ruler_mm  > 0),
   mm_per_px    DOUBLE  CHECK (mm_per_px IS NULL OR mm_per_px > 0),
   record_id    VARCHAR NOT NULL
 )",
# The composite PRIMARY KEY is the constraint that matters: it makes it
# structurally impossible to have the same point twice for one specimen, which
# no wide tabular format can guarantee.
sprintf(
"CREATE TABLE landmark_obs (
   specimen_id  VARCHAR  NOT NULL,
   landmark     SMALLINT NOT NULL CHECK (landmark BETWEEN 1 AND 25),
   x            DOUBLE,
   y            DOUBLE,
   status       VARCHAR  NOT NULL
                CHECK (status IN (%s)),
   PRIMARY KEY (specimen_id, landmark)
 )", paste(.fm_sql_str(.FM_JOURNAL_STATUS), collapse = ","))
)

.fm_sql_str <- function(x) paste0("'", gsub("'", "''", x), "'")

# --- building ----------------------------------------------------------------

#' Rebuild the DuckDB database from the journals
#'
#' Idempotent: two successive calls give the same database. The previous
#' database is overwritten (it is a derived artefact), never updated in place.
#'
#' @param journal_dir Journal directory (or an already-read long data.frame).
#' @param db_path Path of the .duckdb file. NULL -> no database on disk,
#'   everything happens in memory (useful to validate without writing anything).
#' @param export_dir Directory for the Parquet + CSV export. NULL -> no export.
#' @param validate TRUE -> run fishmorph_validate() and attach the report.
#' @param stop_on_error TRUE -> abort if anomalies of severity "error" are
#'   detected, BEFORE writing anything.
#' @return An invisible list: `db_path`, `n_specimens`, `n_points`, `issues`.
#' @export
fishmorph_build_db <- function(journal_dir,
                               db_path    = NULL,
                               export_dir = NULL,
                               validate   = TRUE,
                               stop_on_error = FALSE) {
  for (p in c("DBI", "duckdb")) if (!requireNamespace(p, quietly = TRUE))
    stop("Package '", p, "' is required (install.packages(\"", p, "\")).",
         call. = FALSE)
  if (!exists("fishmorph_consolidate", mode = "function"))
    stop("fishmorph_landmark_store.R is not loaded.", call. = FALSE)

  K <- suppressWarnings(fishmorph_consolidate(journal_dir, long = TRUE,
                                              drop_na_points = FALSE))
  if (!nrow(K)) {
    if (!is.data.frame(journal_dir)) fm_journal_status(journal_dir)
    stop("No usable record: nothing to put into the database.\n",
         "  The journal is created when the app is LAUNCHED, but only fills up ",
         "at the first 'Enregistrer & suivant'.\n",
         "  Digitize at least one specimen, then run fishmorph_build_db() again.",
         call. = FALSE)
  }

  issues <- if (isTRUE(validate)) fishmorph_validate(K) else NULL
  n_err <- if (is.null(issues)) 0L else sum(issues$severity == "error")
  if (n_err > 0L) {
    msg <- sprintf("%d anomaly(ies) of severity 'error' detected.", n_err)
    if (isTRUE(stop_on_error))
      stop(msg, " Database not built. Inspect the report: ",
           "fishmorph_validate(journal_dir).", call. = FALSE)
    warning(msg, " The database is built all the same; see $issues.",
            call. = FALSE)
  }

  num <- function(v) suppressWarnings(as.numeric(v))
  int <- function(v) suppressWarnings(as.integer(num(v)))
  # Timestamp: the journal writes ISO 8601 UTC with a 'Z' suffix, which strptime
  # cannot read as such -> the Z is removed and the time zone forced.
  ts  <- as.POSIXct(strptime(sub("Z$", "", K$timestamp), "%Y-%m-%dT%H:%M:%OS",
                             tz = "UTC"), tz = "UTC")

  first_by <- function(df, key) df[!duplicated(df[[key]]), , drop = FALSE]
  rec <- first_by(data.frame(
    record_id = K$record_id, ts = ts, operator = K$operator,
    app_version = K$app_version, mode = K$mode, target_sheet = K$target_sheet,
    stringsAsFactors = FALSE), "record_id")
  spe <- first_by(data.frame(
    specimen_id = K$row_key, species = K$species, photo_file = K$photo_file,
    img_w = int(K$img_w), img_h = int(K$img_h),
    ruler_mm = num(K$ruler_mm), mm_per_px = num(K$mm_per_px),
    record_id = K$record_id, stringsAsFactors = FALSE), "specimen_id")
  spe$species[is.na(spe$species) | !nzchar(spe$species)] <- "(unknown)"
  obs <- data.frame(
    specimen_id = K$row_key, landmark = int(K$landmark),
    x = num(K$x), y = num(K$y), status = K$status, stringsAsFactors = FALSE)
  obs <- obs[!is.na(obs$landmark), , drop = FALSE]
  obs <- obs[!duplicated(obs[, c("specimen_id", "landmark")]), , drop = FALSE]

  # Brought into compliance BEFORE insertion: one violated constraint would make
  # the whole batch fail with an unhelpful message. We therefore correct
  # explicitly, and say so -- without ever inventing a value: what is unknown
  # becomes NULL, what is outside the vocabulary falls back to the only
  # defensible status.
  bad_mode <- !is.na(rec$mode) & !rec$mode %in% c("reconstruct", "correct", "new")
  if (any(bad_mode)) {
    warning(sum(bad_mode), " record(s) with an unknown mode -> NULL.", call. = FALSE)
    rec$mode[bad_mode] <- NA_character_
  }
  rec$mode[!nzchar(rec$mode %||% "") & !is.na(rec$mode)] <- NA_character_
  if (any(is.na(rec$ts)))
    warning(sum(is.na(rec$ts)), " record(s) without a readable timestamp.",
            call. = FALSE)
  no_op <- is.na(rec$operator) | !nzchar(rec$operator)
  if (any(no_op)) rec$operator[no_op] <- "(unknown)"

  bad_st <- !obs$status %in% .FM_JOURNAL_STATUS
  if (any(bad_st)) {
    warning(sum(bad_st), " observation(s) with an unknown status -> 'na'.", call. = FALSE)
    obs$status[bad_st] <- "na"
  }

  # The database is written to a TEMPORARY file then switched over: should the
  # build fail half way, the previous database stays intact.
  final <- db_path
  if (!is.null(db_path)) {
    dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)
    db_path <- file.path(dirname(db_path),
                         sprintf(".%s.building%d", basename(db_path), Sys.getpid()))
    if (file.exists(db_path)) unlink(db_path)
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path %||% ":memory:",
                        read_only = FALSE)
  ok <- FALSE
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    if (!ok && !is.null(db_path) && file.exists(db_path)) unlink(db_path)
  }, add = TRUE)

  for (ddl in .fm_ddl()) DBI::dbExecute(con, ddl)
  DBI::dbAppendTable(con, "record", rec)
  DBI::dbAppendTable(con, "specimen", spe)
  DBI::dbAppendTable(con, "landmark_obs", obs)
  if (!is.null(issues) && nrow(issues)) DBI::dbWriteTable(con, "qc_issue", issues)

  # DuckDB does not enforce foreign keys with PostgreSQL's rigour: referential
  # integrity is therefore checked EXPLICITLY, rather than assuming that a
  # declared constraint is enough.
  orph <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM landmark_obs o
      WHERE NOT EXISTS (SELECT 1 FROM specimen s WHERE s.specimen_id = o.specimen_id)")$n
  if (orph > 0) warning(orph, " observation(s) with no matching specimen.",
                        call. = FALSE)

  .fm_create_views(con, sort(unique(obs$landmark)))
  if (!is.null(export_dir)) .fm_export(con, export_dir)

  n_sp <- nrow(spe); n_pt <- nrow(obs)
  ok <- TRUE
  DBI::dbDisconnect(con, shutdown = TRUE)
  on.exit(NULL)

  if (!is.null(final)) {
    prev <- paste0(final, ".prev")
    if (file.exists(final)) { if (file.exists(prev)) unlink(prev)
                              file.rename(final, prev) }
    if (!file.rename(db_path, final)) {
      if (file.exists(prev)) file.rename(prev, final)
      stop("Switching the database over failed: ", final, call. = FALSE)
    }
    message("Database built: ", final, " (", n_sp, " specimens, ", n_pt, " points)")
  }
  invisible(list(db_path = final, n_specimens = n_sp, n_points = n_pt,
                 issues = issues))
}

# Views: wide table + morphometric ratios. They are RECOMPUTED at every query,
# hence never out of step with the observations -- unlike a "Bd" column frozen
# in a workbook.
.fm_create_views <- function(con, pts) {
  sel <- paste(vapply(pts, function(p) sprintf(
    '  MAX(CASE WHEN o.landmark = %d THEN o.x END) AS "%d_X",
  MAX(CASE WHEN o.landmark = %d THEN o.y END) AS "%d_Y"', p, p, p, p),
    character(1)), collapse = ",\n")
  DBI::dbExecute(con, sprintf(
"CREATE OR REPLACE VIEW v_landmarks_wide AS
 SELECT s.specimen_id, s.species, s.photo_file, s.mm_per_px, s.record_id,
%s
 FROM specimen s JOIN landmark_obs o USING (specimen_id)
 GROUP BY s.specimen_id, s.species, s.photo_file, s.mm_per_px, s.record_id", sel))

  d <- function(a, b) sprintf('sqrt(pow("%d_X"-"%d_X",2)+pow("%d_Y"-"%d_Y",2))',
                              b, a, b, a)
  B <- .FM_RATIO_BOUNDS
  rat <- paste(sprintf("  %s / Bl_px AS %s", d(B$a, B$b), B$segment), collapse = ",\n")
  DBI::dbExecute(con, sprintf(
"CREATE OR REPLACE VIEW v_ratios AS
 SELECT specimen_id, species, photo_file, mm_per_px, Bl_px,
        Bl_px * mm_per_px AS Bl_mm,
%s
 FROM (SELECT *, %s AS Bl_px FROM v_landmarks_wide)
 WHERE Bl_px > 0", rat, d(1L, 2L)))

  # State of the entry: how many points are still at their seed position, hence
  # never checked by eye. Invisible in a table of coordinates.
  DBI::dbExecute(con,
"CREATE OR REPLACE VIEW v_specimen_qc AS
 SELECT s.specimen_id, s.species, s.photo_file,
        r.ts, r.operator, r.mode,
        COUNT(*) FILTER (WHERE o.status = 'placed')   AS n_placed,
        COUNT(*) FILTER (WHERE o.status = 'seeded')   AS n_seeded,
        COUNT(*) FILTER (WHERE o.status = 'adjusted') AS n_adjusted,
        COUNT(*) FILTER (WHERE o.status = 'derived')  AS n_derived,
        COUNT(*) FILTER (WHERE o.status = 'na')       AS n_na,
        s.mm_per_px IS NOT NULL AS has_scale
 FROM specimen s
 JOIN record r USING (record_id)
 JOIN landmark_obs o USING (specimen_id)
 GROUP BY s.specimen_id, s.species, s.photo_file, r.ts, r.operator, r.mode,
          s.mm_per_px")
  invisible(TRUE)
}

.fm_export <- function(con, export_dir) {
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  cp <- function(what, file, fmt)
    DBI::dbExecute(con, sprintf("COPY (SELECT * FROM %s) TO %s (FORMAT %s)",
                                what, .fm_sql_str(file.path(export_dir, file)), fmt))
  # Parquet: typed, compressed, columnar -> the repository artefact.
  cp("landmark_obs",     "landmark_obs.parquet",  "parquet")
  cp("specimen",         "specimen.parquet",      "parquet")
  cp("v_landmarks_wide", "landmarks_wide.parquet", "parquet")
  cp("v_ratios",         "ratios.parquet",        "parquet")
  # CSV: redundant with the Parquet, but readable in thirty years with no
  # software at all.
  cp("v_landmarks_wide", "landmarks_wide.csv", "csv, HEADER")
  cp("v_ratios",         "ratios.csv",         "csv, HEADER")
  message("Exports written to: ", export_dir)
  invisible(TRUE)
}

#' Open the database (read-only by default)
#'
#' Read-only is the normal mode: the database is derived, and must never be
#' written to by hand. It also lets several R processes open the same file
#' simultaneously.
#' @export
fishmorph_db_connect <- function(db_path, read_only = TRUE) {
  for (p in c("DBI", "duckdb")) if (!requireNamespace(p, quietly = TRUE))
    stop("Package '", p, "' is required.", call. = FALSE)
  if (!file.exists(db_path)) stop("Database not found: ", db_path, call. = FALSE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = read_only)
}

# --- validation --------------------------------------------------------------

#' Structural AND morphometric control
#'
#' The SQL constraints guarantee the coherence of the CONTAINER; this function
#' questions the plausibility of the CONTENT. A set of coordinates can satisfy
#' every constraint and describe an impossible fish.
#'
#' Severities: "error" = certain inconsistency (point outside the image,
#' coincident points, degenerate axis); "warning" = to be looked at (proportion
#' outside the envelope of the 9,556 species); "info" = traceability (point
#' never checked, declared non-measurable, no scale bar).
#'
#' @param x A journal directory, or a long data.frame (the output of
#'   `fishmorph_consolidate(long = TRUE)`).
#' @param expect Points expected for a complete specimen.
#' @param bounds Envelope of the ratios. Defaults to the internal table
#'   `.FM_RATIO_BOUNDS` (not exported, hence not linkable).
#' @return data.frame: specimen_id, species, photo_file, severity, problem,
#'   landmark, detail.
#' @export
fishmorph_validate <- function(x, expect = c(1:19, 22L, 23L),
                               bounds = .FM_RATIO_BOUNDS) {
  K <- if (is.data.frame(x)) x else
    fishmorph_consolidate(x, long = TRUE, drop_na_points = FALSE)
  out <- list()
  add <- function(sp, sev, pb, lm = NA_integer_, detail = "") {
    if (!length(sp) || !nrow(sp)) return(invisible())
    out[[length(out) + 1L]] <<- data.frame(
      specimen_id = sp$row_key, species = sp$species, photo_file = sp$photo_file,
      severity = sev, problem = pb, landmark = lm, detail = detail,
      stringsAsFactors = FALSE)
  }
  if (!nrow(K)) return(do.call(rbind, out) %||% .fm_issue_empty())

  K$landmark <- suppressWarnings(as.integer(K$landmark))
  K$x <- suppressWarnings(as.numeric(K$x))
  K$y <- suppressWarnings(as.numeric(K$y))
  K$img_w <- suppressWarnings(as.numeric(K$img_w))
  K$img_h <- suppressWarnings(as.numeric(K$img_h))

  # 1. coordinate outside the bounds of the image -> stray click or replaced
  #    photograph. Tolerance of 1 %: a point may legitimately graze the edge.
  tol <- 0.01
  outside <- K[is.finite(K$x) & is.finite(K$y) & is.finite(K$img_w) & is.finite(K$img_h) &
            (K$x < -tol * K$img_w | K$x > (1 + tol) * K$img_w |
             K$y < -tol * K$img_h | K$y > (1 + tol) * K$img_h), , drop = FALSE]
  if (nrow(outside)) add(outside, "error", "point outside the image", outside$landmark,
                      sprintf("(%.0f, %.0f) for an image of %.0fx%.0f",
                              outside$x, outside$y, outside$img_w, outside$img_h))

  by_sp <- split(K, K$row_key)
  for (g in by_sp) {
    m <- g[1, , drop = FALSE]
    fin <- g[is.finite(g$x) & is.finite(g$y) & !(g$status %in% "na"), , drop = FALSE]

    # 2. two distinct landmarks at exactly the same pixel = a missed click
    if (nrow(fin) > 1) {
      k <- paste(round(fin$x, 1), round(fin$y, 1))
      dup <- unique(k[duplicated(k)])
      for (kk in dup) {
        lm <- sort(fin$landmark[k == kk])
        add(m, "error", "coincident points", lm[1],
            paste("points", paste(lm, collapse = "+"), "at the same pixel"))
      }
    }
    # 3. expected points absent
    miss <- setdiff(expect, g$landmark)
    if (length(miss)) add(m, "warning", "point absent", miss[1],
                          paste("missing:", paste(miss, collapse = ",")))
    # 4. traceability of the entry
    sd_ <- g$landmark[g$status %in% "seeded" & g$landmark %in% expect]
    if (length(sd_)) add(m, "info", "point never checked", sd_[1],
                         paste("still at the seed:", paste(sort(sd_), collapse = ",")))
    aj_ <- g$landmark[g$status %in% "adjusted" & g$landmark %in% expect]
    if (length(aj_)) add(m, "info", "point snapped by a convention", aj_[1],
                         paste("extremes 3/4 corrected:", paste(sort(aj_), collapse = ",")))
    na_ <- g$landmark[g$status %in% "na" & g$landmark %in% expect]
    if (length(na_)) add(m, "info", "point not measurable", na_[1],
                         paste("declared NA:", paste(sort(na_), collapse = ",")))
    if (!any(is.finite(suppressWarnings(as.numeric(m$mm_per_px)))))
      add(m, "info", "no scale bar", NA_integer_,
          "coordinates in pixels only")

    # 5. morphometric plausibility, referred to the FISHMORPH envelope
    P <- matrix(NA_real_, 25, 2)
    ok <- g$landmark >= 1 & g$landmark <= 25 & !is.na(g$landmark)
    P[g$landmark[ok], 1] <- g$x[ok]; P[g$landmark[ok], 2] <- g$y[ok]
    dd <- function(a, b) if (all(is.finite(P[c(a, b), ])))
      sqrt(sum((P[b, ] - P[a, ])^2)) else NA_real_
    Bl <- dd(1L, 2L)
    if (!is.finite(Bl) || Bl <= 0) {
      add(m, "error", "degenerate body axis", NA_integer_,
          "LM1 and LM2 coincident or absent: no relative scale is possible")
      next
    }
    for (i in seq_len(nrow(bounds))) {
      r <- dd(bounds$a[i], bounds$b[i]) / Bl
      if (!is.finite(r)) next
      if (r < bounds$lo[i] || r > bounds$hi[i])
        add(m, "warning", "proportion outside the envelope", bounds$a[i],
            sprintf("%s/Bl = %.3f outside [%.3f ; %.3f] (median %.3f)",
                    bounds$segment[i], r, bounds$lo[i], bounds$hi[i], bounds$med[i]))
    }
  }
  res <- if (length(out)) do.call(rbind, out) else .fm_issue_empty()
  sev <- factor(res$severity, levels = c("error", "warning", "info"))
  res <- res[order(sev, res$specimen_id), , drop = FALSE]
  rownames(res) <- NULL
  res
}

.fm_issue_empty <- function() data.frame(
  specimen_id = character(0), species = character(0), photo_file = character(0),
  severity = character(0), problem = character(0), landmark = integer(0),
  detail = character(0), stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# Use
# -----------------------------------------------------------------------------
# library(Rfishmorph)
#
# jdir <- "FishMORPH/landmark_journal"
#
# # (1) Rebuild the database + the archival exports. To be run as often as you
# #     like: it is a derived artefact, never an in-place update.
# res <- fishmorph_build_db(jdir,
#          db_path    = "FishMORPH/fishmorph.duckdb",
#          export_dir = "FishMORPH/exports")
# subset(res$issues, severity == "error")
#
# # (2) Validate WITHOUT writing anything (before deciding):
# iss <- fishmorph_validate(jdir)
# table(iss$severity, iss$problem)
#
# # (3) Query. For instance: specimens whose eye falls outside the envelope, or
# #     more than 3 of whose points have never been checked.
# con <- fishmorph_db_connect("FishMORPH/fishmorph.duckdb")
# DBI::dbGetQuery(con, "
#   SELECT r.specimen_id, r.species, r.Ed, q.n_seeded
#   FROM v_ratios r JOIN v_specimen_qc q USING (specimen_id)
#   WHERE r.Ed > 0.1386 OR q.n_seeded > 3
#   ORDER BY q.n_seeded DESC")
#
# # ... or in dplyr, without SQL:
# # dplyr::tbl(con, "v_ratios") |> dplyr::filter(Bd > 0.5) |> dplyr::collect()
# DBI::dbDisconnect(con, shutdown = TRUE)
#
# # (4) Start again from the exports with no engine at all (Parquet or CSV):
# # arrow::read_parquet("FishMORPH/exports/landmarks_wide.parquet")
# # read.csv("FishMORPH/exports/landmarks_wide.csv")
# -----------------------------------------------------------------------------
