# The quality score and the "checked" tick are RECORD-level fields written into
# a journal that has one row per LANDMARK: they are repeated on every line of a
# record, and what matters is that they survive the round trip -- writing,
# reading, and the consolidation into the wide table -- without being confused
# with an absence.

# a minimal set of coordinates: the journal only needs finite X/Y to write a
# point, the geometry is tested elsewhere.
jr_test_coords <- function() {
  P <- matrix(NA_real_, 25, 2)
  P[c(1L, 2L, 3L), ] <- c(0, 1000, 470, 0, 0, -120)
  P
}

# one journal directory per test: fm_journal_read() concatenates EVERY journal
# of a directory, so two tests sharing one would read each other's records.
jr_test_dir <- function() {
  d <- file.path(tempdir(), paste0("fmjrev_", as.integer(Sys.time()), "_",
                                   sample.int(1e6, 1)))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

test_that("the journal carries the score and the review of a record", {
  jd <- jr_test_dir(); on.exit(unlink(jd, recursive = TRUE), add = TRUE)
  jr <- fm_journal_open(jd, operator = "test")
  fm_journal_append(jr, row_key = "sp1", coords = jr_test_coords(),
                    points = c(1L, 2L, 3L), species = "Testus testus",
                    quality_score = 4, reviewed = TRUE)
  d <- fm_journal_read(jd)
  expect_true(all(c("quality_score", "reviewed") %in% names(d)))
  # repeated on EVERY line of the record: there is no per-record header in a
  # long journal, so a record-level field has nowhere else to live.
  expect_equal(nrow(d), 3L)
  expect_equal(unique(d$quality_score), "4")
  expect_equal(unique(d$reviewed), "TRUE")
})

test_that("an unrated record leaves the two fields empty, and 'reviewed' stays FALSE", {
  jd <- jr_test_dir(); on.exit(unlink(jd, recursive = TRUE), add = TRUE)
  jr <- fm_journal_open(jd, operator = "test")
  # not rated, but explicitly declared not checked
  fm_journal_append(jr, row_key = "sp1", coords = jr_test_coords(),
                    points = 1L, species = "A a", reviewed = FALSE)
  # nothing said at all (an older caller, or a version with no such field)
  fm_journal_append(jr, row_key = "sp2", coords = jr_test_coords(),
                    points = 1L, species = "B b")
  d <- fm_journal_read(jd)
  d1 <- d[d$row_key == "sp1", ]; d2 <- d[d$row_key == "sp2", ]
  # an absent score is empty and NOT a zero: zero would be a sixth grade
  expect_equal(d1$quality_score, "")
  # "not checked" and "nothing was said" must stay distinguishable
  expect_equal(d1$reviewed, "FALSE")
  expect_equal(d2$reviewed, "")
})

test_that("consolidation brings the score and the review into the wide table", {
  jd <- jr_test_dir(); on.exit(unlink(jd, recursive = TRUE), add = TRUE)
  jr <- fm_journal_open(jd, operator = "test")
  fm_journal_append(jr, row_key = "sp1", coords = jr_test_coords(),
                    points = c(1L, 2L), species = "A a",
                    quality_score = 2, reviewed = FALSE)
  fm_journal_append(jr, row_key = "sp2", coords = jr_test_coords(),
                    points = c(1L, 2L), species = "B b",
                    quality_score = 5, reviewed = TRUE)
  w <- fishmorph_consolidate(jd)
  expect_true(all(c("quality_score", "reviewed") %in% names(w)))
  expect_equal(w$quality_score[w$row_key == "sp1"], "2")
  expect_equal(w$reviewed[w$row_key == "sp2"], "TRUE")
})

test_that("a re-entry replaces the review of the previous pass", {
  jd <- jr_test_dir(); on.exit(unlink(jd, recursive = TRUE), add = TRUE)
  jr <- fm_journal_open(jd, operator = "test")
  fm_journal_append(jr, row_key = "sp1", coords = jr_test_coords(),
                    points = 1L, species = "A a",
                    quality_score = 1, reviewed = FALSE)
  Sys.sleep(0.01)                       # the timestamp orders the records
  fm_journal_append(jr, row_key = "sp1", coords = jr_test_coords(),
                    points = 1L, species = "A a",
                    quality_score = 5, reviewed = TRUE)
  w <- fishmorph_consolidate(jd)
  # deduplication keeps the LAST record of a key, whole: the review that comes
  # out is the one of the pass whose coordinates come out with it.
  expect_equal(nrow(w), 1L)
  expect_equal(w$quality_score, "5")
  expect_equal(w$reviewed, "TRUE")
})

test_that("a journal written without the review columns is still readable", {
  jd <- jr_test_dir(); on.exit(unlink(jd, recursive = TRUE), add = TRUE)
  # a journal of an earlier version: the two columns simply do not exist
  old <- c("record_id", "timestamp", "operator", "app_version", "mode",
           "target_sheet", "row_key", "species", "photo_file", "img_w", "img_h",
           "ruler_mm", "mm_per_px", "landmark", "x", "y", "status")
  f <- file.path(jd, "landmarks_old_20250101T000000Z.tsv")
  writeLines(c(paste(old, collapse = "\t"),
               paste(c("r1", "2025-01-01T00:00:00.000Z", "op", "0.1",
                       "correct", "Global_Landmark", "sp1", "A a", "a.jpg",
                       "100", "50", "", "", "1", "0", "0", "placed"),
                     collapse = "\t")),
             f)
  d <- fm_journal_read(jd)
  expect_equal(nrow(d), 1L)
  # filled with NA, and NOT with FALSE: the file does not say the specimen was
  # left unchecked, it says nothing at all about it.
  expect_true(is.na(d$reviewed))
  expect_true(is.na(d$quality_score))
})
