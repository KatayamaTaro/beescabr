library(testthat)

# authorize_inat.R is the FIRST thing a new operator runs, and it was written as a
# note to a colleague: "the two taxa you mentioned", "paste this back". It also had
# two defects that only show on a real account.
src_helpers("inat_observations/authorize_inat.R", "AUTHORIZE_INAT_SOURCED_FOR_HELPERS")

# `me$results[[1]]$login %||% "(unknown)"` -- [[1]] on an empty list throws
# "subscript out of bounds" BEFORE %||% can supply the fallback, so an empty
# users/me response ended the run in a raw R error with no instruction. The
# sibling inat_auth_login() has always guarded this exact case.
test_that("an empty users/me response does not crash the run", {
  expect_identical(.auth_login_of(list(results = list())), "")
  expect_identical(.auth_login_of(list()), "")
  expect_identical(.auth_login_of(NULL), "")
})

test_that("a login is read when there is one", {
  expect_identical(.auth_login_of(list(results = list(list(login = "brandi")))), "brandi")
})

test_that("a result with no login field is empty, not the string NULL", {
  expect_identical(.auth_login_of(list(results = list(list(id = 7)))), "")
})
