library(testthat)

# The help says "press l first" before ruling. It is a dead end: obs_urls is written
# ONLY by resolve_beeple_transects_per_survey.R, into the TRANSECT-TIE file. The
# windows file's real columns are year, first_name, inat_username, window_start,
# window_end, transect, review_reason, suggestion, n_obs_in_window, decision,
# decision_note -- no obs_urls, so `l` printed "(no observations near this window)"
# on every one of the nine questions and gave the operator nowhere to go.
#
# The row does carry who the surveyor is and which dates are in question, so the
# blank case can send them somewhere real instead of nowhere.
src("project_info/qc_review_survey_windows.R")

row <- function(user = "bonnienickel", n = 0L)
  data.frame(first_name = "Bonnie", inat_username = user,
             window_start = "2025-10-18", window_end = "2025-10-21",
             n_obs_in_window = n, stringsAsFactors = FALSE)

said <- function(expr) paste(capture.output(expr), collapse = "\n")

test_that("with no stored URLs it names the surveyor's page and the dates", {
  txt <- said(.rw_list_urls(row(), 1L))
  expect_match(txt, "https://www.inaturalist.org/people/bonnienickel", fixed = TRUE)
  expect_match(txt, "2025-10-18", fixed = TRUE)
  expect_match(txt, "2025-10-21", fixed = TRUE)
})

test_that("it no longer dead-ends on '(no observations near this window)'", {
  txt <- said(.rw_list_urls(row(), 1L))
  expect_false(grepl("(no observations near this window)", txt, fixed = TRUE))
})

test_that("it says WHY there is nothing to show, so the blank reads as the answer", {
  txt <- said(.rw_list_urls(row(), 1L))
  expect_match(txt, "no tagged survey", ignore.case = TRUE)
})

test_that("a surveyor with no iNaturalist name says where to add one", {
  txt <- said(.rw_list_urls(row(user = NA_character_), 1L))
  expect_match(txt, "people_manual.csv", fixed = TRUE)
  expect_false(grepl("people/NA", txt, fixed = TRUE))
})

test_that("stored URLs are still listed when there are any", {
  rv <- data.frame(obs_urls = "https://www.inaturalist.org/observations/1; https://www.inaturalist.org/observations/2",
                   first_name = "Bonnie", inat_username = "b",
                   window_start = "2025-10-18", window_end = "2025-10-21",
                   n_obs_in_window = 2L, stringsAsFactors = FALSE)
  txt <- said(.rw_list_urls(rv, 1L))
  expect_match(txt, "observations/1", fixed = TRUE)
  expect_match(txt, "observations/2", fixed = TRUE)
})

test_that("'in-CABR' is not used as an unexplained word", {
  rv <- data.frame(obs_urls = "https://www.inaturalist.org/observations/1",
                   first_name = "B", inat_username = "b", window_start = "a",
                   window_end = "b", n_obs_in_window = 1L, stringsAsFactors = FALSE)
  expect_false(grepl("in-CABR", said(.rw_list_urls(rv, 1L)), fixed = TRUE))
})

# The rest of the file, audited. Taro has never seen this pipeline: "the brain",
# "window", "ruling", "auto-confirm", "no tag = not a survey day", "equal split",
# "obs", "in-CABR" and "beeple" are all internal words, and three messages name a
# file with no folder in front of it so it cannot be opened.
said <- function(expr) gsub("[[:space:]]+", " ", paste(capture.output(expr), collapse = " "))
saidm <- function(expr) {
  out <- character(0)
  withCallingHandlers(expr, message = function(m) {
    out <<- c(out, conditionMessage(m)); invokeRestart("muffleMessage") })
  gsub("[[:space:]]+", " ", paste(out, collapse = " "))
}

test_that("the windows help says what a window IS before asking about one", {
  txt <- said(.rw_help())
  expect_match(txt, "survey day", ignore.case = TRUE)
  expect_false(grepl("the brain", txt, fixed = TRUE))
  expect_false(grepl("auto-confirm", txt, fixed = TRUE))
})

test_that("the windows help names a file you can open, not a stem", {
  txt <- said(.rw_help())
  expect_false(grepl("master_per_survey_info ", txt, fixed = TRUE))
  if (grepl("master_per_survey", txt, fixed = TRUE))
    expect_match(txt, "data/project_info/surveys/master_per_survey_info_generated.csv", fixed = TRUE)
})

test_that("the ties help names the mistagged file with its real folder", {
  txt <- said(.rtt_help())
  expect_match(txt, "data/inat_observations/review/qc_review_inat_mistagged_transects_generated.csv",
               fixed = TRUE)
})

test_that("the ties help explains a transect code is what you type", {
  txt <- said(.rtt_help())
  expect_match(txt, "transect", ignore.case = TRUE)
  expect_false(grepl("beeple", txt, ignore.case = TRUE))    # internal word for a surveyor
})

test_that("a missing review file says how to make one, runnably", {
  txt <- saidm(review_windows(path = tempfile(fileext = ".csv"), write = FALSE))
  expect_match(txt, 'source("scripts/run_data_cleaning_pipeline.R")', fixed = TRUE)
  expect_false(grepl("run finding_project_info() first", txt, fixed = TRUE))
})

test_that("a missing tie file reads as good news, not an error", {
  txt <- saidm(review_transect_ties(path = tempfile(fileext = ".csv"), write = FALSE))
  expect_match(txt, "nothing", ignore.case = TRUE)
})

test_that("nothing left to review reads as finished, not as a failure", {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(year = integer(0), first_name = character(0),
                       inat_username = character(0), window_start = character(0),
                       window_end = character(0), transect = character(0),
                       review_reason = character(0), suggestion = character(0),
                       n_obs_in_window = integer(0), decision = character(0),
                       decision_note = character(0)), f, row.names = FALSE)
  txt <- saidm(review_windows(path = f, write = FALSE))
  expect_match(txt, "nothing|all done|none left", ignore.case = TRUE)
})

test_that("the sourced-by-hand hint says what each command answers", {
  txt <- paste(.rw_sourced_hint(), collapse = " ")
  expect_match(txt, "review_windows()", fixed = TRUE)
  expect_match(txt, "review_transect_ties()", fixed = TRUE)
  expect_false(grepl("equal-split days)", txt, fixed = TRUE))   # jargon with no gloss
  expect_match(txt, "two transects", ignore.case = TRUE)
})

# The card is what the operator actually answers from, and it was the least readable
# thing in the file: two bare dates joined by an arrow with no word for what they are,
# an unexplained ">>", "y=survey" reading as a noun rather than an answer, and
# "l=list obs URLs" promising URLs this file has never had.
card <- function(sugg = "SUGGEST NO -- no tagged survey by anyone within 10 days of this planned window") {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(year = 2025L, first_name = "Bonnie", inat_username = "bonnienickel",
                       window_start = "2025-10-18", window_end = "2025-10-21",
                       transect = "TP", review_reason = "no-survey-near", suggestion = sugg,
                       n_obs_in_window = 0L, decision = NA_character_,
                       decision_note = NA_character_, stringsAsFactors = FALSE),
            f, row.names = FALSE)
  said(review_windows(path = f, write = FALSE, prompt_fn = function(p) "q"))
}

test_that("the card says what the two dates are", {
  expect_match(card(), "scheduled", ignore.case = TRUE)
})

test_that("the card asks a question rather than labelling a category", {
  txt <- card()
  expect_match(txt, "did this survey happen", ignore.case = TRUE)
  expect_false(grepl("y=survey", txt, fixed = TRUE))
})

test_that("the card does not promise URLs this file has never had", {
  expect_false(grepl("list obs URLs", card(), fixed = TRUE))
})

test_that("the unexplained >> marker is gone", {
  expect_false(grepl(">>", card(), fixed = TRUE))
})

test_that("a row with no suggestion does not print a raw internal code", {
  txt <- card(sugg = NA_character_)
  expect_false(grepl("no-survey-near", txt, fixed = TRUE))
})
