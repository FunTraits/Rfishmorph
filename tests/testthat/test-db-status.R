# The DuckDB schema and the journal must share ONE status vocabulary.
#
# They drifted apart once: the CHECK on landmark_obs listed four statuses while
# the journal had been writing five since "adjusted" was introduced. The
# database refused the batch as a whole, so a single point snapped by a
# FISHMORPH convention -- in a journal of several hundred specimens -- aborted
# the entire build with a message naming a SQL constraint rather than the
# specimen. The vocabulary is now read from `.FM_JOURNAL_STATUS` at call time;
# these tests are what keeps it that way.

# A minimal but COMPLETE long journal: one specimen, the 25 points of the
# canonical frame, and the statuses cycled over them so that every status the
# journal can write is present. The 25 points are not decoration: the views
# built by fishmorph_build_db() name the landmarks of the ratio pairs, so a
# stub carrying only a handful would fail on a missing COLUMN and hide whatever
# the test was meant to catch.
fm_journal_stub <- function(statuses = .FM_JOURNAL_STATUS, n = 25L) {
  statuses <- rep_len(statuses, n)
  data.frame(
    record_id    = "rec1",
    timestamp    = "2026-07-31T10:00:00.000Z",
    operator     = "AT",
    app_version  = "test",
    mode         = "reconstruct",
    target_sheet = "Global_Landmark",
    row_key      = "Genus.species",
    species      = "Genus species",
    photo_file   = "Genus_species.jpg",
    img_w        = 1000,
    img_h        = 500,
    ruler_mm     = 10,
    mm_per_px    = 0.1,
    landmark     = seq_len(n),
    x            = seq(100, by = 10, length.out = n),
    y            = seq(200, by = 10, length.out = n),
    status       = statuses,
    stringsAsFactors = FALSE)
}

test_that("the schema accepts every status the journal can write", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  res <- fishmorph_build_db(fm_journal_stub(), db_path = NULL,
                            export_dir = NULL, validate = FALSE)
  expect_equal(res$n_points, 25L)
})

test_that("'adjusted' survives the round trip through the database", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db), add = TRUE)
  fishmorph_build_db(fm_journal_stub(), db_path = db, export_dir = NULL,
                     validate = FALSE)

  con <- fishmorph_db_connect(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  st <- DBI::dbGetQuery(con, "SELECT status FROM landmark_obs")$status
  expect_setequal(st, .FM_JOURNAL_STATUS)

  # The QC view must count them too: a status the schema stores but the view
  # ignores makes the per-specimen counts fail to add up to the point total,
  # which is the same drift one level higher.
  qc <- DBI::dbGetQuery(con, "SELECT * FROM v_specimen_qc")
  expect_true("n_adjusted" %in% names(qc))
  expect_equal(qc$n_placed + qc$n_seeded + qc$n_adjusted + qc$n_derived +
                 qc$n_na, 25)
})

test_that("a status outside the vocabulary is degraded, not fatal", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("duckdb")

  J <- fm_journal_stub(c("placed", "not_a_status"))
  expect_warning(res <- fishmorph_build_db(J, db_path = NULL, export_dir = NULL,
                                           validate = FALSE),
                 "unknown status")
  expect_equal(res$n_points, 25L)
})
