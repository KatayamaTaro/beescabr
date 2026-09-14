library(testthat)

# Phase 3 of the cleaning pipeline scrolls fourteen progress lines past the operator,
# every one written in this project's internal shorthand: "phantoms", "ancestor rows",
# "id-bearing taxa", "Holway base", "obs missing coords -- excluded", "SD County
# subset". Nothing is broken -- the step works -- but a supervisor who has never seen
# the project cannot tell a normal run from a bad one, which is the only thing these
# lines exist to convey.
src("reference/taxonomy_lookup_build.R")

test_that("dropped records say what they were dropped FROM, and that it is not permanent", {
  txt <- .tlb_no_coords(84L)
  expect_false(grepl("obs ", txt, fixed = TRUE))         # the abbreviation
  expect_match(txt, "no location", ignore.case = TRUE)
  expect_match(txt, "map", ignore.case = TRUE)           # excluded from the MAPPING, not deleted
})

test_that("a complex is explained the first time it is mentioned", {
  txt <- .tlb_complexes(23L)
  expect_match(txt, "look-alike", ignore.case = TRUE)
  expect_false(grepl("complex species\n", txt, fixed = TRUE))
})

test_that("the checklist base says what it is counting", {
  txt <- .tlb_reference_base(717L, 318L)
  expect_match(txt, "717", fixed = TRUE)
  expect_match(txt, "318", fixed = TRUE)
  expect_false(grepl("ancestor rows", txt, fixed = TRUE))
  expect_false(grepl("id-bearing", txt, fixed = TRUE))
  expect_match(txt, "genus|family", ignore.case = TRUE)  # says what the extra rows ARE
})

test_that("a skipped specimen addition explains itself without coining a word", {
  txt <- .tlb_phantoms(2L)
  expect_false(grepl("phantom", txt, ignore.case = TRUE))
  expect_match(txt, "no specimen and no photo", fixed = TRUE)   # says WHY it was left out
})

test_that("added specimen bees name the file they came from", {
  txt <- .tlb_additions(5L)
  expect_match(txt, "data/reference/hand_curated/specimen_additions.csv", fixed = TRUE)
})

test_that("the orphan warning says which file to add the genus to", {
  txt <- paste(.tlb_orphan_genus(c("Perdita zebrata")), collapse = " ")
  expect_match(txt, "data/reference/hand_curated/specimen_additions.csv", fixed = TRUE)
  expect_false(grepl("orphaned", txt, fixed = TRUE))
  expect_match(txt, "Perdita zebrata", fixed = TRUE)
})

# "unverified" reads as "unchecked, possibly wrong". It means only that nobody had to
# confirm the bee, which is true of every bee already on the county checklist.
test_that("the headline count does not call ordinary bees unverified", {
  txt <- .tlb_lookup_done(1115L, 0L)
  expect_false(grepl("unverified", txt, fixed = TRUE))
  expect_match(txt, "1,115", fixed = TRUE)
})

test_that("bees still awaiting a person are described as that, not as bad data", {
  txt <- .tlb_lookup_done(1115L, 12L)
  expect_match(txt, "12", fixed = TRUE)
  expect_match(txt, "waiting|confirm", ignore.case = TRUE)
})

test_that("the offline note leads with what it means, not the variable name", {
  txt <- .tlb_offline(21674L)
  expect_false(grepl("^BEESCABR_SKIP_INGEST", txt))
  expect_match(txt, "nothing new", ignore.case = TRUE)
  expect_match(txt, "21,674", fixed = TRUE)
})

test_that("the missing-checklist stop points at something that exists", {
  txt <- .tlb_no_reference("holway_sd_bee_reference_table_v3_generated.csv")
  expect_false(grepl("step 1b", txt, fixed = TRUE))      # there is no step 1b
  expect_match(txt, 'source("scripts/run_data_cleaning_pipeline.R")', fixed = TRUE)
})

test_that("one orphan reads as one, not as 'these 1'", {
  txt <- paste(.tlb_orphan_genus("Perdita zebrata"), collapse = " ")
  expect_match(txt, "One hand-added bee", fixed = TRUE)
  expect_false(grepl("These 1", txt, fixed = TRUE))
})

test_that("subspecies are described by what happened, not by where they came from", {
  expect_false(grepl("cache", .tlb_subspecies(7L), ignore.case = TRUE))
  expect_match(.tlb_subspecies(7L), "subspecies", fixed = TRUE)
  expect_match(.tlb_subspecies(0L), "none", ignore.case = TRUE)
})
