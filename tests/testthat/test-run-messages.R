# From the first-time-operator audit. Every one of these is a message the run ENDS
# on, or hands work back with -- the moment someone looks for where to go next.
.root <- .beescabr_root()
.txt <- function(f) paste(readLines(file.path(.root, f), warn = FALSE), collapse = "\n")

# The analysis run closed with "Figures + tables are in data/analysis/". That folder
# holds three entries and no figures: outputs land in the SEASON folder beneath it.
# Taro opens what he is told to open and finds nothing.
test_that("the analysis run does not send you to the wrong folder", {
  s <- .txt("scripts/run_all_analysis_pipeline.R")
  expect_false(grepl('"Figures \\+ tables are in data/analysis/"', s, perl = TRUE))
})

test_that("the analysis run names the season folder and the index", {
  s <- .txt("scripts/run_all_analysis_pipeline.R")
  expect_match(s, "DIR_REPORT", fixed = TRUE)          # the real season folder
  expect_match(s, "findings_index.csv", fixed = TRUE)  # the map to everything
})

# basename() prints a filename with no directory, so the reader is handed a name
# and left to find it. These are the two review files the cleaning run produces.
test_that("the clean step prints review paths, not bare filenames", {
  s <- .txt("scripts/inat_observations/clean/inat_bee_clean.R")
  expect_false(grepl("bx_out(basename(IBC_FIX_SURVEY))", s, fixed = TRUE))
})

# The first version of this test only checked the old string was gone, and passed
# while the replacement named two constants that do not exist. A message naming a
# path that does not resolve is worse than a vague one, so check the values.
test_that("the brain step prints paths that actually resolve", {
  e <- new.env()
  old <- setwd(.root); on.exit(setwd(old), add = TRUE)
  suppressMessages(sys.source("scripts/project_info/finding_project_info.R", e))
  for (v in c("FPI_SURVEY_DATES", "FPI_MEMBERSHIP")) {
    expect_true(exists(v, envir = e), info = v)
    expect_true(nzchar(get(v, envir = e)), info = v)
  }
  s <- .txt("scripts/project_info/finding_project_info.R")
  expect_false(grepl('bx_out("master_per_survey_info_generated.csv', s, fixed = TRUE))
})
