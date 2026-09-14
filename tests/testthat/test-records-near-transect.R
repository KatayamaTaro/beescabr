library(testthat)

# A SEPARATE figure from survey effort, answering a different question: where in the
# park do bee records come from, regardless of who made them. The effort figure counts
# survey walks; this one counts records near a route. They are not comparable, and the
# labels have to keep them apart -- a bar called "TP" holding visitor photographs would
# read as TP survey data, which is the confusion this whole thread started with.
#
# Pure: it takes a distance matrix, so the rule is testable without any spatial data.
src_helpers("analysis/coverage/records_near_transect.R", "RNT_SOURCED_FOR_HELPERS")

d <- function(...) matrix(c(...), nrow = length(c(...)) / 3, byrow = TRUE,
                          dimnames = list(NULL, c("BST", "TP", "OT")))

test_that("a record inside the buffer takes the nearest route", {
  r <- nearest_within(d(4, 30, 50), buffer_m = 10)
  expect_equal(r$route, "BST")
})

test_that("a record outside every buffer is left unassigned", {
  r <- nearest_within(d(40, 30, 50), buffer_m = 10)
  expect_true(is.na(r$route))
})

test_that("a record in two buffers takes the nearer one, and is counted as close", {
  r <- nearest_within(d(9, 3, 50), buffer_m = 10)
  expect_equal(r$route, "TP")
  expect_true(r$ambiguous)
})

test_that("a record in one buffer only is not flagged", {
  expect_false(nearest_within(d(3, 30, 50), buffer_m = 10)$ambiguous)
})

# Ties happen at the OT/TP junction, where the two routes run 1.1 m apart. TP is the
# route actually walked there -- it is surveyed as two transects and carries about
# double the effort of any other -- so it takes the tie. Named explicitly, because a
# silent tie-break is the kind of decision nobody finds for years.
test_that("an exact tie goes to TP", {
  r <- nearest_within(d(5, 5, 50), buffer_m = 10)   # BST and TP both at 5 m
  expect_equal(r$route, "TP")
  expect_true(r$ambiguous)
})

test_that("a tie with TP not involved is left unassigned rather than guessed", {
  m <- matrix(c(5, 50, 5), nrow = 1, dimnames = list(NULL, c("BST", "TP", "OT")))
  r <- nearest_within(m, buffer_m = 10)             # BST and OT tied, TP far away
  expect_true(is.na(r$route))
  expect_true(r$ambiguous)
})

test_that("the tie-breaker can be changed, so it is never a hidden default", {
  r <- nearest_within(d(5, 5, 50), buffer_m = 10, tie_breaker = "BST")
  expect_equal(r$route, "BST")
})

test_that("it handles many records at once", {
  m <- d(4, 30, 50,
         40, 30, 50,
         9,  3, 50)
  r <- nearest_within(m, buffer_m = 10)
  expect_equal(r$route, c("BST", NA, "TP"))
  expect_equal(r$ambiguous, c(FALSE, FALSE, TRUE))
})

test_that("the buffer width is reported, since the whole figure depends on it", {
  txt <- near_transect_caption(buffer_m = 10, assigned = 1908L, ambiguous = 45L,
                               unassigned = 2038L, tagged = 7500L)
  expect_match(txt, "10 m", fixed = TRUE)
  expect_match(txt, "1,908", fixed = TRUE)
  expect_match(txt, "2,038", fixed = TRUE)
})

test_that("the caption distinguishes tagged surveys from records placed by distance", {
  txt <- near_transect_caption(10, 1908L, 45L, 2038L, tagged = 7500L)
  expect_match(txt, "rather than surveys", fixed = TRUE)
  expect_match(txt, "two kinds of evidence", fixed = TRUE)
})

# Every record is in the figure. The ones that carry a transect keep the one the
# surveyor recorded -- geometry never overrules the person who walked it -- and only
# the ones with none are placed by distance. The two are kept visibly apart, because
# a visitor's photograph beside a route is not the same evidence as a tagged survey.
test_that("a record that already has a transect keeps it, untouched by geometry", {
  r <- combine_transect_source(c("TP", "", NA), c("BST", "UPMON", "OT"))
  expect_equal(r$transect, c("TP", "UPMON", "OT"))
  expect_equal(r$source, c("tagged", "nearby", "nearby"))
})

test_that("an untagged record with nothing within the buffer stays out", {
  r <- combine_transect_source("", NA_character_)
  expect_true(is.na(r$transect))
  expect_true(is.na(r$source))
})

test_that("the caption accounts for both kinds of evidence", {
  txt <- near_transect_caption(10, 1908L, 45L, 2038L, tagged = 7500L)
  expect_match(txt, "7,500", fixed = TRUE)
  expect_match(txt, "1,908", fixed = TRUE)
  expect_match(txt, "keep it", fixed = TRUE)
})

# The caption's numbers have to add up to the total, or it is the same defect it was
# written to fix: 8,365 tagged + 1,953 placed + 2,038 not shown = 12,356, not 12,471.
# The 115 specimens that carry no transect are not shown either, and were missing from
# the count.
test_that("shown plus not-shown equals the total", {
  tagged <- 8365L; placed <- 1953L; not_shown <- 2153L
  expect_equal(tagged + placed + not_shown, 12471L)
  txt <- near_transect_caption(10, placed, 45L, not_shown, tagged)
  expect_match(txt, "2,153", fixed = TRUE)
})
