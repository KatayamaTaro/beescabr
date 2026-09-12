library(testthat)

# The Holway checklist version is written into config.R in two places -- the source
# CSV and the generated reference table -- so dropping a v4 file into
# data/reference/source/ does nothing at all: the pipeline keeps reading v3 and says
# not one word about it. Whoever inherits this project would have no way to know.
#
# And when the version DOES change, the saved answers are filed under each bee's
# checklist spelling, so a respelled bee looks like a brand-new name: you get asked
# again, and the old answer is orphaned. None of that is announced either -- a routine
# run just turns into forty questions.
src("reference/holway_version.R")

test_that("the version is read off the filename", {
  expect_equal(holway_version_of("data/reference/source/holway_2026/holway_v3_combined.csv"), 3L)
  expect_equal(holway_version_of("holway_v10_combined.csv"), 10L)
  expect_true(is.na(holway_version_of("San Diego County Bee Species Checklist, v3.xlsx")))
  expect_true(is.na(holway_version_of("notes.txt")))
})

test_that("the newest source file wins", {
  files <- c("a/holway_v3_combined.csv", "b/holway_v4_combined.csv", "c/holway_v2_combined.csv")
  n <- holway_newest_source(files)
  expect_equal(n$version, 4L)
  expect_equal(n$path, "b/holway_v4_combined.csv")
})

test_that("files that are not a combined checklist are ignored", {
  files <- c("a/holway_v3_combined.csv", "a/README.docx", "a/Checklist, v9.xlsx")
  expect_equal(holway_newest_source(files)$version, 3L)
})

test_that("no checklist files at all is not an error", {
  expect_null(holway_newest_source(character(0)))
  expect_null(holway_newest_source(c("README.docx")))
})

test_that("a newer version than the one configured is reported", {
  files <- c("a/holway_v3_combined.csv", "b/holway_v4_combined.csv")
  expect_equal(holway_newer_than(files, "a/holway_v3_combined.csv")$version, 4L)
})

test_that("nothing newer means nothing to report", {
  files <- c("a/holway_v3_combined.csv")
  expect_null(holway_newer_than(files, "a/holway_v3_combined.csv"))
})

test_that("a configured file with no version in its name still compares", {
  files <- c("b/holway_v4_combined.csv")
  expect_equal(holway_newer_than(files, "some/other.csv")$version, 4L)
})

# The announcement. Its job is to stop a routine run turning into forty unexplained
# questions -- and to report the number that describes the OPERATOR'S work, not the
# number that describes how much Holway changed. Those are very different: most
# respellings resolve against iNaturalist with no question at all.
test_that("a newer file on disk is announced, with what to do about it", {
  txt <- paste(holway_version_notice(
    newer = list(path = "data/reference/source/holway_2027/holway_v4_combined.csv", version = 4L),
    configured = "data/reference/source/holway_2026/holway_v3_combined.csv"), collapse = " ")
  expect_match(txt, "v4", fixed = TRUE)
  expect_match(txt, "v3", fixed = TRUE)
  expect_match(txt, "holway_v4_combined.csv", fixed = TRUE)
  expect_match(txt, "scripts/config.R", fixed = TRUE)      # where to point it
  expect_match(txt, "holway_combined", fixed = TRUE)       # the exact key to edit
  expect_match(txt, "holway_reference", fixed = TRUE)      # and the other one
})

test_that("nothing newer, nothing said", {
  expect_length(holway_version_notice(newer = NULL, configured = "x"), 0L)
})

# The version-bump announcement. It is the loudest thing this pipeline ever prints,
# and it was missing the one fact that decides whether the person at the keyboard
# should be running it at all: the questions that follow are TAXONOMIC judgements,
# not clerical ones. The rebuild menu already says "this needs BEE EXPERTISE, not
# just patience" -- the same warning belongs here, because this reaches the same
# place without anyone choosing it from a menu.
test_that("it says plainly that this needs someone who knows bees", {
  txt <- paste(holway_version_bump_notice(3L, 4L, 723L, "some/backup.csv"), collapse = " ")
  expect_match(txt, "EXPERTISE", fixed = TRUE)
  expect_match(txt, "not just patience", fixed = TRUE)
})

test_that("it names the two sites the answers come from", {
  txt <- paste(holway_version_bump_notice(3L, 4L, 723L, "some/backup.csv"), collapse = " ")
  expect_match(txt, "iNaturalist", fixed = TRUE)
  expect_match(txt, "ITIS", fixed = TRUE)
})

test_that("it says what the wrong answer costs", {
  txt <- paste(holway_version_bump_notice(3L, 4L, 723L, "b.csv"), collapse = " ")
  expect_match(txt, "wrong", ignore.case = TRUE)
})

test_that("it carries the numbers and the backup path", {
  txt <- paste(holway_version_bump_notice(3L, 4L, 723L, "data/x/holway_answers_v3.csv"),
               collapse = " ")
  expect_match(txt, "v3", fixed = TRUE)
  expect_match(txt, "v4", fixed = TRUE)
  expect_match(txt, "723", fixed = TRUE)
  expect_match(txt, "data/x/holway_answers_v3.csv", fixed = TRUE)
})

test_that("a failed backup is not reported as a successful one", {
  txt <- paste(holway_version_bump_notice(3L, 4L, 723L, backup = NA_character_),
               collapse = " ")
  expect_false(grepl("nothing is lost", txt, fixed = TRUE))
  expect_match(txt, "could not", ignore.case = TRUE)
})
