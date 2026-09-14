library(testthat)
library(dplyr)

# pick_taxon_id_by_rank(): from iNat search candidates, pick the id for a taxon at a given rank
#   ONLY on an unambiguous match -- right rank, matching (diacritic-insensitive) name, and the known
#   parent id present in the candidate's ancestry. Never guesses; returns NA when unsure.

test_that("pick_taxon_id_by_rank matches rank + diacritic-insensitive name + parent", {
  src("reference/resolve_missing_ids.R")
  cands <- list(
    list(id = 51222L, rank = "subgenus", name = "Schonnherria", ancestor_ids = c(47201L, 51111L)),
    list(id = 99999L, rank = "species",  name = "Xylocopa splendidula", ancestor_ids = c(51111L)))
  # search "Schönnherria" (with umlaut) at subgenus, parent genus Xylocopa = 51111 -> matches 51222
  expect_equal(pick_taxon_id_by_rank(cands, "subgenus", "Schönnherria", parent_id = 51111L), 51222L)
})

test_that("pick_taxon_id_by_rank returns NA when the rank doesn't match", {
  src("reference/resolve_missing_ids.R")
  cands <- list(list(id = 1L, rank = "species", name = "Schonnherria", ancestor_ids = c(51111L)))
  expect_true(is.na(pick_taxon_id_by_rank(cands, "subgenus", "Schonnherria", parent_id = 51111L)))
})

test_that("pick_taxon_id_by_rank returns NA when the parent doesn't match (wrong genus)", {
  src("reference/resolve_missing_ids.R")
  cands <- list(list(id = 1L, rank = "subgenus", name = "Schonnherria", ancestor_ids = c(48111L)))
  expect_true(is.na(pick_taxon_id_by_rank(cands, "subgenus", "Schonnherria", parent_id = 51111L)))
})

test_that("pick_taxon_id_by_rank returns NA when >1 candidate matches (ambiguous -> never guess)", {
  src("reference/resolve_missing_ids.R")
  cands <- list(
    list(id = 1L, rank = "genus", name = "Megandrena", ancestor_ids = c(47222L)),
    list(id = 2L, rank = "genus", name = "Megandrena", ancestor_ids = c(47222L)))
  expect_true(is.na(pick_taxon_id_by_rank(cands, "genus", "Megandrena", parent_id = 47222L)))
})

test_that("pick_taxon_id_by_rank matches a genus with no parent id required", {
  src("reference/resolve_missing_ids.R")
  cands <- list(list(id = 55L, rank = "genus", name = "Megandrena", ancestor_ids = c(47222L)))
  expect_equal(pick_taxon_id_by_rank(cands, "genus", "Megandrena", parent_id = NA), 55L)
})

# The verdict cache was permanent. A name searched once and not found was written to
# resolved_missing_ids.csv as "not_found_or_ambiguous" and never searched again -- so
# a bee iNaturalist published later could not be picked up automatically, and the
# operator's "17 bees still have no id" list would sit at 17 forever. A not-found
# answer is only true as of the moment it was asked, and re-asking costs a handful of
# API calls, so it is asked EVERY run. The cache keeps the answer as a record of what
# was last seen; it no longer decides whether to look.

test_that("a found id is never searched again", {
  src("reference/resolve_missing_ids.R")
  expect_false(.rmi_stale("filled", season = 2026L))
})

test_that("a not-found verdict is always searched again, however recent", {
  src("reference/resolve_missing_ids.R")
  expect_true(.rmi_stale("not_found_or_ambiguous", season = 2026L))
  expect_true(.rmi_stale(NA_character_, season = 2026L))
  expect_true(.rmi_stale("", season = 2026L))
})

test_that("it is vectorized over a whole cache column", {
  src("reference/resolve_missing_ids.R")
  expect_equal(.rmi_stale(c("filled", "not_found_or_ambiguous", "filled"), season = 2026L),
               c(FALSE, TRUE, FALSE))
})

test_that("a stale name is searched again and its verdict replaced, not duplicated", {
  src("reference/resolve_missing_ids.R")
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(key = "species|hesperapis ilicifoliae|NA",
                                  taxon_id = NA_integer_,
                                  status = "not_found_or_ambiguous",
                                  checked_season = 2025L), cache)
  df <- tibble::tibble(rank = "species", genus = "Hesperapis", species = "ilicifoliae",
                       family = NA_character_, taxon_id = NA_integer_)

  searched <- character(0)
  out <- suppressMessages(resolve_missing_taxon_ids(
    df, cache_path = cache, season = 2026L,
    fetch_fn = function(nm) { searched <<- c(searched, nm); list() }))

  expect_length(searched, 1L)                       # it looked again
  after <- readr::read_csv(cache, show_col_types = FALSE)
  expect_equal(nrow(after), 1L)                     # replaced, not appended
  expect_equal(after$checked_season[1], 2026L)      # stamped with this season
})

test_that("a verdict from THIS season is still searched again", {
  src("reference/resolve_missing_ids.R")
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(key = "species|hesperapis ilicifoliae|NA",
                                  taxon_id = NA_integer_,
                                  status = "not_found_or_ambiguous",
                                  checked_season = 2026L), cache)
  df <- tibble::tibble(rank = "species", genus = "Hesperapis", species = "ilicifoliae",
                       family = NA_character_, taxon_id = NA_integer_)
  searched <- character(0)
  suppressMessages(resolve_missing_taxon_ids(
    df, cache_path = cache, season = 2026L,
    fetch_fn = function(nm) { searched <<- c(searched, nm); list() }))
  expect_length(searched, 1L)
})

test_that("an id already found costs no API call", {
  src("reference/resolve_missing_ids.R")
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble::tibble(key = "species|hesperapis ilicifoliae|NA",
                                  taxon_id = 51111L, status = "filled",
                                  checked_season = 2025L), cache)
  df <- tibble::tibble(rank = "species", genus = "Hesperapis", species = "ilicifoliae",
                       family = NA_character_, taxon_id = NA_integer_)
  searched <- character(0)
  out <- suppressMessages(resolve_missing_taxon_ids(
    df, cache_path = cache, season = 2026L,
    fetch_fn = function(nm) { searched <<- c(searched, nm); list() }))
  expect_length(searched, 0L)
  expect_equal(out$taxon_id[1], 51111L)
})
