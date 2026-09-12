# A failed IUCN lookup used to return code "NE" -- the SAME value the API returns for a
# species it has genuinely not assessed. So an expired key, an outage, or a typo'd token
# looked exactly like a scientific finding, and would have relabelled Bombus crotchii
# (Endangered) as "Not Evaluated" on the public site. A failure must be distinguishable.

src("reference/enrich_lookups.R")

test_that("a real API answer of NE is reported as NE", {
  r <- .iucn_fetch_one("Apis mellifera", key = "k",
                       fetch_fn = function(...) list(red_list_category = list(code = "NE")))
  expect_equal(r$code, "NE")
  expect_true(r$ok)
})

test_that("a genuine assessment comes through", {
  r <- .iucn_fetch_one("Bombus crotchii", key = "k",
                       fetch_fn = function(...) list(red_list_category = list(code = "EN"),
                                                     year_published = "2024"))
  expect_equal(r$code, "EN")
  expect_true(r$ok)
})

test_that("a FAILED call is not reported as NE", {
  r <- .iucn_fetch_one("Bombus crotchii", key = "bad",
                       fetch_fn = function(...) stop("Token not valid! (HTTP 401)"))
  expect_false(r$ok)
  expect_true(is.na(r$code))          # NOT "NE"
})

test_that("the failure carries the reason, so a bad key can be named", {
  r <- .iucn_fetch_one("Bombus crotchii", key = "bad",
                       fetch_fn = function(...) stop("Token not valid! (HTTP 401)"))
  expect_match(r$error, "401")
})

test_that("an authentication failure is recognisable as one", {
  expect_true(.iucn_is_auth_error("Token not valid! (HTTP 401)"))
  expect_true(.iucn_is_auth_error("HTTP 403 Forbidden"))
  expect_false(.iucn_is_auth_error("Timeout was reached"))
  expect_false(.iucn_is_auth_error(NA_character_))
})

# The "we could not refresh IUCN" block lost a pair of braces: `if (!nzchar(key))`
# guarded only the first of its two message() calls, so the second half of the
# sentence -- "pipeline will ask for it, or put it in data/secrets/..." -- printed
# even when a token was already set. An operator whose only problem was a missing
# R package was told to go find an API token they already had.
test_that("a missing package is not reported as a missing token", {
  note <- .iucn_skip_note(need_n = 3L, has_pkg = FALSE, has_key = TRUE)
  txt <- paste(note, collapse = " ")
  expect_match(txt, "rredlist", fixed = TRUE)
  expect_false(grepl("token", txt, ignore.case = TRUE))
  expect_false(grepl("data/secrets", txt, fixed = TRUE))
})

test_that("a missing token keeps both halves of its sentence", {
  note <- .iucn_skip_note(need_n = 3L, has_pkg = TRUE, has_key = FALSE)
  txt <- paste(note, collapse = " ")
  expect_match(txt, "api.iucnredlist.org", fixed = TRUE)
  expect_match(txt, "data/secrets/iucn_api.env", fixed = TRUE)
  expect_false(grepl("rredlist is not installed", txt, fixed = TRUE))
})

test_that("both missing reports both, and the count leads", {
  txt <- paste(.iucn_skip_note(need_n = 3L, has_pkg = FALSE, has_key = FALSE), collapse = " ")
  expect_match(txt, "3 species", fixed = TRUE)
  expect_match(txt, "rredlist", fixed = TRUE)
  expect_match(txt, "api.iucnredlist.org", fixed = TRUE)
})
