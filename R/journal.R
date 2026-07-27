# =============================================================================
# journal.R -- the capture layer (append-only journal)
#
# STORAGE layer for FISHMORPH landmarks: append-only journal + consolidation.
#
# THE PROBLEM SOLVED
#   The digitizing app used to write every specimen by REWRITING IN FULL an
#   .xlsx workbook of several Mb, inside a synchronised folder (OneDrive). A
#   crash of R, a power cut or a synchronisation lock during that rewrite can
#   therefore destroy THE WHOLE file -- not merely the last specimen. The volume
#   is not the issue (a few thousand specimens x ~25 points = a few Mb): it is
#   the WRITE PATTERN that is fragile.
#
# THE PRINCIPLE
#   Never rewrite what is already written. Capture becomes a stream of immutable
#   files; the analysable table is REBUILT on demand.
#
#     photographs --[app]--> append-only journal --[consolidation]--> csv / xlsx
#                            (never modified)                          (export)
#
#   * One journal per SESSION: "landmarks_<operator>_<timestamp>.tsv". A
#     finished session is a frozen file -> OneDrive can no longer produce a
#     conflicted copy of it, and two workstations produce two files that merge
#     by plain concatenation.
#   * LONG format (one row = ONE point): adding a landmark tomorrow is no longer
#     a schema migration, only extra rows.
#   * A crash damages at worst the last line of the current journal, which is
#     detected and dropped when read.
#   * Deduplication keeps the last record per key, which yields the HISTORY of
#     corrections for free (fm_journal_history()).
#
# DEPENDENCIES: none for the journal itself (base R). openxlsx only for the
#   optional .xlsx exports.
#
# See R/digitizer-app.R (launch_fishmorph_digitizer) for the writing side;
# examples of reading back and consolidating at the end of this file.
#
# THE LAYER ABOVE: R/database.R builds, FROM these journals, a derived DuckDB
# database (types, constraints, views, SQL) and the Parquet / CSV archival
# exports. That database is disposable and rebuildable; the journals remain the
# only source of truth.
# =============================================================================

# Journal columns, in order. Any column added later MUST be appended at the END
# of this list: fm_journal_read() tolerates journals of different widths (older
# files) by filling the absent columns with NA.
.FM_JOURNAL_COLS <- c(
  "record_id",     # identifier of the RECORD (one press of "Enregistrer")
  "timestamp",     # ISO 8601 UTC, lexicographic order = chronological order
  "operator",      # who digitized
  "app_version",   # version of the digitizing tool
  "mode",          # reconstruct | correct | new
  "target_sheet",  # workbook sheet aimed at (traceability)
  "row_key",       # deduplication KEY (species, or photo file in "new" mode)
  "species",       # Genus species
  "photo_file",    # photograph file name (basename)
  "img_w", "img_h",# image size in pixels: the X/Y are in IMAGE pixels
  "ruler_mm",      # real length of the scale bar 20-21 (mm), or NA
  "mm_per_px",     # resulting scale, or NA
  "landmark",      # point number
  "x", "y",        # coordinates in image pixels (Y downwards)
  "status"         # placed | seeded | adjusted | derived | na  (see below)
)

# Meaning of `status` -- this is the information the wide workbook layout cannot
# carry, and it is precious in quality control:
#   placed  : point placed or moved by hand (or reloaded from an earlier entry)
#   seeded  : point still at its SEED position, never checked by the operator
#   adjusted: point snapped automatically by a FISHMORPH convention at the
#             operator's request (3/4 brought back to the maximum body depth):
#             neither pointed at by hand, nor a plain seed
#   derived : point computed automatically (8, 9, 11, 15, 23)
#   na      : point explicitly marked NON-MEASURABLE
.FM_JOURNAL_STATUS <- c("placed", "seeded", "adjusted", "derived", "na")


# --- utilities ---------------------------------------------------------------

# ISO 8601 timestamp in UTC, to the millisecond. In UTC and in this format the
# lexicographic order of the strings IS the chronological order: deduplication
# can therefore sort without ever re-parsing a date (and without depending on
# the workstation's time zone).
.fm_iso_now <- function() format(as.POSIXct(Sys.time(), tz = "UTC"),
                                 "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

# Neutralises whatever would break a TSV (tab, newline, quote). ALWAYS returns
# at least one element: an absent piece of metadata (NULL, or a Shiny input not
# yet initialised) would otherwise give a zero-length vector, and data.frame()
# would fail on incompatible lengths -- which would LOSE the record instead of
# degrading it.
.fm_tsv_safe <- function(x) {
  x <- as.character(x)
  if (!length(x)) return("")
  x[is.na(x)] <- ""
  gsub("[\t\r\n\"]+", " ", x)
}

#' ATOMIC write of an openxlsx workbook
#'
#' `saveWorkbook()` overwrites its target in place: while it is being rewritten
#' (seconds, for a workbook of several Mb) the file is in an intermediate state,
#' and an interruption destroys it. We therefore write to a temporary file in
#' the SAME directory -- a necessary condition for the rename to be atomic, a
#' cross-volume rename being in fact a copy -- then switch by renaming.
#'
#' The old file is not deleted but moved to "<name>.prev.xlsx", which gives a
#' one-generation backup at no cost. Should the final rename fail, the old file
#' is restored.
#'
#' @param wb An openxlsx object.
#' @param path Target path.
#' @param keep_prev Keep the previous generation (default TRUE).
#' @return TRUE (invisibly) if the write succeeded.
#' @export
fm_save_workbook_atomic <- function(wb, path, keep_prev = TRUE) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Package 'openxlsx' is required.", call. = FALSE)
  tmp <- file.path(dirname(path),
                   sprintf(".%s.tmp%d", basename(path), Sys.getpid()))
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  openxlsx::saveWorkbook(wb, tmp, overwrite = TRUE)
  if (!file.exists(tmp)) stop("Temporary write failed: ", tmp, call. = FALSE)

  prev <- sub("(\\.xlsx)?$", ".prev.xlsx", path)
  had  <- file.exists(path)
  # The old file is moved aside BEFORE renaming: on Windows, file.rename() fails
  # when the destination already exists.
  if (had) {
    if (file.exists(prev)) unlink(prev)
    if (!file.rename(path, prev))
      stop("Could not move the old file aside (locked by Excel?): ",
           path, call. = FALSE)
  }
  if (!file.rename(tmp, path)) {
    if (had) file.rename(prev, path)              # restore
    stop("Final rename failed: ", path, call. = FALSE)
  }
  if (had && !keep_prev) unlink(prev)
  invisible(TRUE)
}

# --- journal: writing --------------------------------------------------------

#' Open a session journal (append-only)
#'
#' Creates `journal_dir` if needed and a TSV file specific to the session. The
#' file is only ever APPENDED to: it is never re-read nor rewritten by the app,
#' and becomes immutable the moment the session ends.
#'
#' @param journal_dir Journal directory.
#' @param operator Operator identifier (default: the system user).
#' @param app_version Version of the digitizing tool, traced in every row.
#' @return A journal "handle" to pass to [fm_journal_append()].
#' @export
fm_journal_open <- function(journal_dir, operator = NULL, app_version = NA_character_) {
  if (is.null(operator) || !nzchar(operator))
    operator <- tryCatch(unname(Sys.info()[["user"]]), error = function(e) "unknown")
  operator <- gsub("[^A-Za-z0-9._-]+", "_", operator)
  if (!dir.exists(journal_dir))
    dir.create(journal_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(journal_dir))
    stop("Could not create the journal directory: ", journal_dir, call. = FALSE)

  stamp <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y%m%dT%H%M%SZ", tz = "UTC")
  sid   <- paste0(operator, "_", stamp)
  path  <- file.path(journal_dir, paste0("landmarks_", sid, ".tsv"))
  # unlikely collision (two launches within the same second) -> suffix
  k <- 1L
  while (file.exists(path)) {
    k <- k + 1L
    path <- file.path(journal_dir, sprintf("landmarks_%s-%d.tsv", sid, k))
  }
  cat(paste(.FM_JOURNAL_COLS, collapse = "\t"), "\n", sep = "", file = path)
  message("Session journal: ", path)
  structure(list(path = path, dir = journal_dir, operator = operator,
                 session_id = sid, app_version = as.character(app_version),
                 n = local({ e <- new.env(parent = emptyenv()); e$i <- 0L; e })),
            class = "fm_journal")
}

#' Append one record (one specimen) to the journal
#'
#' A "record" = one press of "Enregistrer", that is one row per landmark, all
#' sharing the same `record_id`. Writing is a plain `cat(append = TRUE)` of a
#' block of text built beforehand: the existing file is never re-read nor
#' rewritten, so an interruption can only truncate the last line (which will be
#' discarded when read).
#'
#' @param jr Handle returned by [fm_journal_open()].
#' @param row_key Deduplication key (species, or photo file in "new" mode).
#' @param coords Two-column matrix (X, Y) indexed by landmark number.
#' @param points Landmark numbers to record.
#' @param status Named vector (name = point number) of statuses; default "placed".
#' @param species,photo_file,mode,target_sheet,img_w,img_h,ruler_mm,mm_per_px Metadata.
#' @return The `record_id` written (invisibly), or NULL if there was nothing to write.
#' @export
fm_journal_append <- function(jr, row_key, coords, points,
                              status = NULL, species = NA, photo_file = NA,
                              mode = NA, target_sheet = NA,
                              img_w = NA, img_h = NA,
                              ruler_mm = NA, mm_per_px = NA) {
  if (!inherits(jr, "fm_journal")) stop("`jr` is not a journal.", call. = FALSE)
  points <- points[points >= 1 & points <= nrow(coords)]
  if (!length(points)) return(invisible(NULL))

  jr$n$i <- jr$n$i + 1L
  rid <- sprintf("%s-%05d", jr$session_id, jr$n$i)
  ts  <- .fm_iso_now()

  st <- rep("placed", length(points))
  if (!is.null(status)) {
    hit <- match(as.character(points), names(status))
    st[!is.na(hit)] <- as.character(status)[hit[!is.na(hit)]]
  }
  # Without a usable coordinate no other status means anything: record "na"
  # rather than let a reader believe the point was placed or computed.
  fin <- is.finite(coords[points, 1]) & is.finite(coords[points, 2])
  st[!fin] <- "na"

  # formatC(format = "f") and NOT format(): the latter applies getOption("digits")
  # (7 significant digits by default) and would round a five-digit abscissa on a
  # large photograph (12345.678 -> "12345.68"). Here the precision is fixed in
  # number of DECIMALS, never in significant digits.
  num <- function(v) {
    v <- suppressWarnings(as.numeric(v))
    if (!length(v)) return("")          # same guard as .fm_tsv_safe()
    out <- rep("", length(v))
    ok <- is.finite(v)
    if (any(ok))
      out[ok] <- formatC(v[ok], format = "f", digits = 6, drop0trailing = TRUE)
    out
  }
  rows <- data.frame(
    record_id = rid, timestamp = ts, operator = jr$operator,
    app_version = jr$app_version %||% "", mode = .fm_tsv_safe(mode),
    target_sheet = .fm_tsv_safe(target_sheet), row_key = .fm_tsv_safe(row_key),
    species = .fm_tsv_safe(species), photo_file = .fm_tsv_safe(photo_file),
    img_w = num(img_w), img_h = num(img_h),
    ruler_mm = num(ruler_mm), mm_per_px = num(mm_per_px),
    landmark = as.character(points),
    x = num(round(coords[points, 1], 3)), y = num(round(coords[points, 2], 3)),
    status = st, stringsAsFactors = FALSE)
  rows <- rows[, .FM_JOURNAL_COLS, drop = FALSE]

  txt <- paste(do.call(paste, c(unname(as.list(rows)), sep = "\t")), collapse = "\n")
  cat(txt, "\n", sep = "", file = jr$path, append = TRUE)
  invisible(rid)
}

# --- journal: reading --------------------------------------------------------

.fm_journal_empty <- function() {
  d <- as.data.frame(matrix(character(0), nrow = 0, ncol = length(.FM_JOURNAL_COLS)),
                     stringsAsFactors = FALSE)
  names(d) <- .FM_JOURNAL_COLS
  d
}

#' Read and concatenate every journal in a directory
#'
#' Tolerant by construction: a last line truncated by a crash is discarded
#' (mandatory columns missing), and a journal written by an earlier version
#' (fewer columns) is filled with NA.
#'
#' @param journal_dir Journal directory (or a vector of directories).
#' @return A LONG data.frame, one row per point and per record.
#' @export
fm_journal_read <- function(journal_dir) {
  fs <- unlist(lapply(journal_dir, function(d)
    list.files(d, pattern = "^landmarks_.*\\.tsv$", full.names = TRUE)), use.names = FALSE)
  if (!length(fs)) return(.fm_journal_empty())
  parts <- lapply(fs, function(f) {
    d <- try(utils::read.delim(f, sep = "\t", header = TRUE, quote = "",
                               comment.char = "", colClasses = "character",
                               fill = TRUE, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(d, "try-error") || is.null(d) || !nrow(d)) return(NULL)
    for (cc in setdiff(.FM_JOURNAL_COLS, names(d))) d[[cc]] <- rep(NA_character_, nrow(d))
    d <- d[, .FM_JOURNAL_COLS, drop = FALSE]
    # A line truncated mid-write (a crash) is unusable without
    # record_id/landmark/row_key -> it is dropped silently.
    ok <- !is.na(d$record_id) & nzchar(d$record_id) &
          !is.na(d$landmark)  & nzchar(d$landmark) &
          !is.na(d$row_key)
    d[ok, , drop = FALSE]
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (!length(parts)) return(.fm_journal_empty())
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' State of the journals in a directory
#'
#' To be called FIRST whenever a consolidation returns an empty result: it says
#' immediately whether the directory is the right one, which files are in it,
#' and how many records each contains. A journal with 0 records is the normal
#' state of a session opened then closed without saving anything: the app
#' creates the file at LAUNCH, not at the first "Enregistrer".
#'
#' @param journal_dir Journal directory.
#' @return data.frame: file, bytes, n_lines, n_records, n_points, period.
#' @export
fm_journal_status <- function(journal_dir) {
  ex <- dir.exists(journal_dir)
  message("Directory: ", normalizePath(journal_dir, mustWork = FALSE),
          if (ex) "" else "   [NOT FOUND]")
  if (!ex) return(invisible(NULL))
  fs <- list.files(journal_dir, pattern = "^landmarks_.*\\.tsv$", full.names = TRUE)
  other <- setdiff(list.files(journal_dir), basename(fs))
  if (!length(fs)) {
    message("No 'landmarks_*.tsv' file.",
            if (length(other))
              paste0(" The directory does contain: ",
                     paste(utils::head(other, 5), collapse = ", "),
                     " -- wrong directory, or renamed files?") else
              " Empty directory: the app has never been launched with this journal_dir.")
    return(invisible(NULL))
  }
  J <- fm_journal_read(journal_dir)
  out <- do.call(rbind, lapply(fs, function(f) {
    n_l <- length(readLines(f, warn = FALSE))
    data.frame(file = basename(f), bytes = file.size(f),
               n_lines = max(0L, n_l - 1L), stringsAsFactors = FALSE)
  }))
  out$n_records <- NA_integer_; out$n_points <- NA_integer_
  out$start <- NA_character_;   out$end <- NA_character_
  if (nrow(J)) {
    # attach each record to its session through the prefix of the record_id
    sess <- sub("-\\d+$", "", J$record_id)
    for (i in seq_len(nrow(out))) {
      sid <- sub("^landmarks_(.*)\\.tsv$", "\\1", out$file[i])
      k <- sess == sid
      out$n_records[i] <- length(unique(J$record_id[k]))
      out$n_points[i]  <- sum(k)
      if (any(k)) { out$start[i] <- min(J$timestamp[k]); out$end[i] <- max(J$timestamp[k]) }
    }
  }
  out$n_records[is.na(out$n_records)] <- 0L
  out$n_points[is.na(out$n_points)]   <- 0L
  tot <- sum(out$n_records)
  message(sprintf("%d file(s), %d record(s), %d point(s) in total.",
                  nrow(out), tot, sum(out$n_points)))
  if (tot == 0L)
    message("-> No 'Enregistrer & suivant' has been pressed yet in a session ",
            "using this journal. The file is created when the app is LAUNCHED; ",
            "it only fills up at the first save.")
  out
}

#' History of the records of one key
#'
#' Useful to check that a correction was taken into account, or to compare two
#' passes over the same specimen.
#'
#' @param journal_dir Journal directory, or an already-read data.frame.
#' @param row_key Key to inspect. NULL -> summary of every key.
#' @export
fm_journal_history <- function(journal_dir, row_key = NULL) {
  J <- if (is.data.frame(journal_dir)) journal_dir else fm_journal_read(journal_dir)
  if (!nrow(J)) return(J)
  if (!is.null(row_key)) J <- J[J$row_key %in% row_key, , drop = FALSE]
  if (!nrow(J)) return(J)
  agg <- do.call(rbind, lapply(split(J, J$record_id), function(g) data.frame(
    record_id = g$record_id[1], timestamp = g$timestamp[1], operator = g$operator[1],
    mode = g$mode[1], row_key = g$row_key[1], species = g$species[1],
    photo_file = g$photo_file[1], n_points = nrow(g),
    n_placed = sum(g$status == "placed"), n_seeded = sum(g$status == "seeded"),
    n_adjusted = sum(g$status == "adjusted"),
    n_na = sum(g$status == "na"), stringsAsFactors = FALSE)))
  agg <- agg[order(agg$row_key, agg$timestamp, agg$record_id), ]
  rownames(agg) <- NULL
  agg
}

# --- consolidation -----------------------------------------------------------

#' Rebuild the analysable table from the journals
#'
#' For each key (`row_key`), only the LAST record is kept -- maximum timestamp,
#' maximum `record_id` in case of a tie. Earlier records stay in the journals:
#' they are the history of corrections, readable through [fm_journal_history()],
#' and they are never lost.
#'
#' @param journal_dir Journal directory, or an already-read data.frame.
#' @param long TRUE -> return the long format kept (one row per point) instead
#'   of the wide table.
#' @param drop_na_points TRUE (default) -> points marked "na" come out as NA.
#'   FALSE -> whatever coordinates they carry are kept.
#' @param out_csv,out_xlsx Optional export paths (the xlsx one requires openxlsx).
#' @return A wide data.frame: one row per key, columns `<n>_X` / `<n>_Y`.
#' @export
fishmorph_consolidate <- function(journal_dir, long = FALSE, drop_na_points = TRUE,
                                  out_csv = NULL, out_xlsx = NULL) {
  J <- if (is.data.frame(journal_dir)) journal_dir else fm_journal_read(journal_dir)
  if (!nrow(J)) {
    if (!is.data.frame(journal_dir)) fm_journal_status(journal_dir)
    warning("No record in the journal (see the diagnosis above).",
            call. = FALSE)
    return(.fm_journal_empty())
  }
  # Last record per key. We work on the table of RECORDS (and not of points) so
  # as never to mix two passes over one specimen: a whole record_id is kept,
  # hence a coherent set of points.
  R <- unique(J[, c("row_key", "record_id", "timestamp")])
  R <- R[order(R$row_key, R$timestamp, R$record_id), , drop = FALSE]
  keep <- R$record_id[!duplicated(R$row_key, fromLast = TRUE)]
  K <- J[J$record_id %in% keep, , drop = FALSE]

  K$x <- suppressWarnings(as.numeric(K$x))
  K$y <- suppressWarnings(as.numeric(K$y))
  if (isTRUE(drop_na_points)) {
    bad <- K$status %in% "na"
    K$x[bad] <- NA_real_; K$y[bad] <- NA_real_
  }
  K$landmark <- suppressWarnings(as.integer(K$landmark))
  K <- K[!is.na(K$landmark), , drop = FALSE]
  if (isTRUE(long)) { rownames(K) <- NULL; return(K) }

  meta_cols <- c("row_key", "species", "photo_file", "operator", "timestamp",
                 "record_id", "mode", "target_sheet", "img_w", "img_h",
                 "ruler_mm", "mm_per_px", "app_version")
  wide <- K[!duplicated(K$record_id), meta_cols, drop = FALSE]
  wide <- wide[order(wide$row_key), , drop = FALSE]
  rownames(wide) <- NULL

  pts <- sort(unique(K$landmark))
  for (p in pts) {
    s <- K[K$landmark == p, , drop = FALSE]
    i <- match(wide$record_id, s$record_id)
    wide[[paste0(p, "_X")]] <- s$x[i]
    wide[[paste0(p, "_Y")]] <- s$y[i]
    wide[[paste0(p, "_status")]] <- s$status[i]
  }
  # Status columns grouped at the end of the table (they get in the way of
  # reading the coordinates, but they are not thrown away: they are the quality
  # control information).
  st <- grep("_status$", names(wide), value = TRUE)
  wide <- wide[, c(setdiff(names(wide), st), st), drop = FALSE]

  if (!is.null(out_csv)) {
    utils::write.csv(wide, out_csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    message("CSV export: ", out_csv, " (", nrow(wide), " rows)")
  }
  if (!is.null(out_xlsx)) {
    if (!requireNamespace("openxlsx", quietly = TRUE))
      warning("openxlsx not installed: .xlsx export skipped.", call. = FALSE)
    else {
      openxlsx::write.xlsx(wide, out_xlsx, overwrite = TRUE)
      message("XLSX export: ", out_xlsx, " (", nrow(wide), " rows)")
    }
  }
  wide
}

#' Quick quality control of a consolidation
#'
#' Reports what a table of coordinates does not show: points never checked
#' (still at their seed), points declared non-measurable, points snapped by a
#' convention (columns `n_adjusted` / `adjusted`: 3 or 4 brought back to the
#' maximum body depth by the application), incomplete specimens.
#'
#' @param journal_dir Journal directory, or an already-read data.frame.
#' @param expect Points expected for a complete specimen.
#' @export
fishmorph_journal_qc <- function(journal_dir, expect = c(1:19, 22L, 23L)) {
  K <- fishmorph_consolidate(journal_dir, long = TRUE, drop_na_points = FALSE)
  if (!nrow(K)) return(K)
  qc <- do.call(rbind, lapply(split(K, K$row_key), function(g) data.frame(
    row_key = g$row_key[1], species = g$species[1], photo_file = g$photo_file[1],
    timestamp = g$timestamp[1],
    n_missing = sum(!expect %in% g$landmark),
    missing = paste(setdiff(expect, g$landmark), collapse = ","),
    n_seeded = sum(g$status == "seeded" & g$landmark %in% expect),
    seeded = paste(g$landmark[g$status == "seeded" & g$landmark %in% expect],
                   collapse = ","),
    n_adjusted = sum(g$status == "adjusted" & g$landmark %in% expect),
    adjusted = paste(g$landmark[g$status == "adjusted" & g$landmark %in% expect],
                     collapse = ","),
    n_na = sum(g$status == "na" & g$landmark %in% expect),
    has_scale = isTRUE(is.finite(suppressWarnings(as.numeric(g$mm_per_px[1])))),
    stringsAsFactors = FALSE)))
  qc <- qc[order(-qc$n_missing, -qc$n_seeded, qc$row_key), , drop = FALSE]
  rownames(qc) <- NULL
  qc
}

# -----------------------------------------------------------------------------
# Typical use
# -----------------------------------------------------------------------------
# library(Rfishmorph)
# jdir <- "FishMORPH/landmark_journal"
#
# # (1) The analysable table (one row per key):
# base <- fishmorph_consolidate(jdir, out_csv = "consolidated_landmarks.csv")
#
# # (2) Quality control: points never checked, missing ones, missing scale:
# subset(fishmorph_journal_qc(jdir), n_seeded > 0 | n_missing > 0)
#
# # (3) History of the passes over one specimen:
# fm_journal_history(jdir, "Coilia.nasus")
#
# # (4) Long format, for geomorph / intraitR:
# lg <- fishmorph_consolidate(jdir, long = TRUE)
#
# # (5) MERGING TWO WORKSTATIONS: copy the .tsv files of both directories into
# #     one. File names include the operator and the timestamp, so no collision
# #     is possible; there is nothing to do beyond the copy.
# -----------------------------------------------------------------------------
