library(testthat)

# Stage 3 is the only thing in this project that makes anything public, and its words
# get that backwards: "Publishing into docs/" only copies files to a local folder,
# while "Deploying" -- the step that actually puts the park's site on the internet --
# says nothing about being public and never shows the URL.
#
# Short messages. This runs for several minutes and nobody reads a wall of text in
# the middle of it.
src("website/publish_messages.R")

test_that("the local copy is not called publishing", {
  expect_false(grepl("publish", .pub_copying(), ignore.case = TRUE))
  expect_match(.pub_copying(), "docs/", fixed = TRUE)
})

test_that("going live says it is going live, and where", {
  txt <- paste(.pub_deploying("https://brandi.github.io/beescabr/"), collapse = " ")
  expect_match(txt, "public", ignore.case = TRUE)
  expect_match(txt, "https://brandi.github.io/beescabr/", fixed = TRUE)
})

test_that("a missing page names what was looked for and what it costs", {
  txt <- paste(.pub_page_missing("nps_summary_tables.R", found = 0L), collapse = " ")
  expect_match(txt, "nps_summary_tables.R", fixed = TRUE)
  expect_false(grepl("NOT FOUND", txt, fixed = TRUE))
})

test_that("two files of the same name is a different problem, said differently", {
  txt <- paste(.pub_page_missing("x.R", found = 2L), collapse = " ")
  expect_match(txt, "two", ignore.case = TRUE)
  expect_false(grepl("AMBIGUOUS", txt, fixed = TRUE))
})

test_that("a failed page names the script, not just the R error", {
  txt <- paste(.pub_page_failed("least_sampled_bees.R", "object 'bee_counts' not found"),
               collapse = " ")
  expect_match(txt, "least_sampled_bees.R", fixed = TRUE)
  expect_match(txt, "object 'bee_counts' not found", fixed = TRUE)
})

test_that("the hard stop says the live site is untouched", {
  txt <- paste(.pub_stop(c("a.R", "b.R")), collapse = " ")
  expect_match(txt, "live site", ignore.case = TRUE)
  expect_match(txt, "unchanged|untouched", ignore.case = TRUE)
  expect_match(txt, "a.R", fixed = TRUE)
})

test_that("the next step is something you can type where you are", {
  txt <- paste(.pub_next_steps(), collapse = " ")
  expect_match(txt, "BEESCABR_DEPLOY", fixed = TRUE)
  expect_false(grepl("git add docs/ &&", txt, fixed = TRUE))   # shell, in an R console
})

test_that("nothing here runs past a handful of lines", {
  for (x in list(.pub_copying(), .pub_deploying("u"), .pub_page_missing("x.R", 0L),
                 .pub_page_failed("x.R", "e"), .pub_stop("a.R"), .pub_next_steps()))
    expect_lte(length(x), 5L)
})
