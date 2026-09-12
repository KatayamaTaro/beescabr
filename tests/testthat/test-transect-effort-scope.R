library(testthat)

# A co-author read the figure's 8,250 against Table 2's 12,288 and asked why they did
# not match. They do not match because the figure drops every record with no transect
# on it -- 4,106 of them, almost all public observations rather than project surveys --
# while its own caption said "Scope: all records".
#
# The figure is right to drop them: a bee photographed by a visitor is not sampling
# effort, however close to a transect they were standing. What was wrong is a caption
# claiming a scope the figure does not have, so the two numbers could only be
# reconciled by email.
src("analysis/transect_effort.R")

test_that("the scope says the figure only holds records with a transect", {
  txt <- .te_scope(kept = 8365L, total = 12471L)
  expect_false(grepl("all records", txt, fixed = TRUE))
  expect_match(txt, "transect", fixed = TRUE)
})

test_that("it names the records it left out, so the two numbers reconcile on the page", {
  txt <- .te_scope(kept = 8365L, total = 12471L)
  expect_match(txt, "4,106", fixed = TRUE)
  expect_match(txt, "not surveys|public", ignore.case = TRUE)
})

test_that("it still explains OT", {
  expect_match(.te_scope(8365L, 12471L), "off-transect", fixed = TRUE)
})

test_that("nothing left out means nothing to explain", {
  txt <- .te_scope(kept = 12471L, total = 12471L)
  expect_false(grepl("0", txt, fixed = TRUE))
  expect_match(txt, "off-transect", fixed = TRUE)
})
