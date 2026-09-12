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
  expect_match(txt, "3 bees", fixed = TRUE)      # "species" is the code's word, not a person's
  expect_match(txt, "rredlist", fixed = TRUE)
  expect_match(txt, "api.iucnredlist.org", fixed = TRUE)
})

# The most consequential messages in the audit. When the Red List cannot be reached,
# enrich_iucn_columns() writes "NE" / "Not Evaluated" onto EVERY bee (lines 237-238),
# so Bombus crotchii -- genuinely Endangered -- goes onto the public field guide
# reading "Not Evaluated". All the operator saw was "!! IUCN enrichment skipped:".
#
# And one message was simply false: with no token it said "using the cached values",
# but `need` is exactly the species with NO cached value. Wrong in the reassuring
# direction is the worst kind.
src("reference/enrich_lookups.R")

test_that("a skipped status pass says what it did to the table", {
  txt <- paste(.iucn_skipped_note("no internet"), collapse = " ")
  expect_match(txt, "Not Evaluated", fixed = TRUE)
  expect_match(txt, "every bee", ignore.case = TRUE)
  expect_match(txt, "do not publish|not publish", ignore.case = TRUE)
})

test_that("the no-token note does not claim cached values it does not have", {
  txt <- paste(.iucn_skip_note(3L, has_pkg = TRUE, has_key = FALSE), collapse = " ")
  expect_false(grepl("using the cached values", txt, fixed = TRUE))
  expect_match(txt, "never been looked up|no status", ignore.case = TRUE)
})

test_that("species that DO have a cached value are described as that", {
  txt <- paste(.iucn_skip_note(3L, has_pkg = FALSE, has_key = TRUE), collapse = " ")
  expect_match(txt, "rredlist", fixed = TRUE)
})

test_that("the no-token note does not promise a prompt that already happened", {
  txt <- paste(.iucn_skip_note(3L, has_pkg = TRUE, has_key = FALSE), collapse = " ")
  expect_false(grepl("pipeline will ask for it", txt, fixed = TRUE))
})

test_that("a partial failure says whether the result is usable", {
  txt <- paste(.iucn_partial_note(8L, 40L, auth = FALSE), collapse = " ")
  expect_match(txt, "32", fixed = TRUE)          # the ones that worked
  expect_match(txt, "unchanged", ignore.case = TRUE)
})

test_that("a rejected key is told apart from an outage, with a next step each", {
  expect_match(paste(.iucn_partial_note(40L, 40L, auth = TRUE), collapse = " "),
               "token", ignore.case = TRUE)
  expect_match(paste(.iucn_partial_note(40L, 40L, auth = FALSE), collapse = " "),
               "try again", ignore.case = TRUE)
})

test_that("the cache line names a file you can open", {
  expect_match(.iucn_cache_note(412L, "data/checklists/iucn/iucn_status_generated.csv"),
               "data/checklists/iucn/iucn_status_generated.csv", fixed = TRUE)
})

test_that("plant common names say what is missing and what it costs", {
  txt <- paste(.pgc_unresolved_note(12L), collapse = " ")
  expect_false(grepl("unresolved", txt, fixed = TRUE))
  expect_false(grepl("local seed", txt, fixed = TRUE))
  expect_match(txt, "Latin", ignore.case = TRUE)     # what the reader will see instead
})
