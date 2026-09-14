library(testthat)
library(dplyr)

# manual_overrides.R: user-curated answers for name-changed / synonym taxa the automated iNat
# search can't bridge. apply_manual_overrides fills the id (and corrects the name) at BOTH the
# Holway reference and taxonomy lookup levels; write_review_worklist emits the "look these up" prompt.

test_that("apply_manual_overrides fills id and corrects the name for a renamed species", {
  src("reference/manual_overrides.R")
  df <- tibble(
    rank = c("species", "species"),
    genus = c("Holcopasites", "Andrena"),
    species = c("minima", "baeriae"),
    subspecies = NA_character_,
    scientific_name = c("Holcopasites minima", "Andrena baeriae"),
    taxon_id = c(NA_integer_, 100L))
  ov <- tibble(rank = "species", name = "Holcopasites minima", taxon_id = 999L,
               correct_name = "Holcopasites minimus", note = "minima->minimus")
  out <- apply_manual_overrides(df, ov)
  expect_equal(out$taxon_id[1], 999L)
  expect_equal(out$scientific_name[1], "Holcopasites minimus")
  expect_equal(out$species[1], "minimus")            # epithet corrected to iNat's name
  expect_equal(out$genus[1], "Holcopasites")
  expect_equal(out$taxon_id[2], 100L)                # non-matching row untouched
  expect_equal(attr(out, "n_applied"), 1L)
})

test_that("apply_manual_overrides marks resolved and matches diacritics/case (id only)", {
  src("reference/manual_overrides.R")
  df <- tibble(rank = "subgenus", genus = "Xylocopa", subgenus = "Schönnherria",
               scientific_name = NA_character_, taxon_id = NA_integer_, resolved = FALSE)
  ov <- tibble(rank = "subgenus", name = "schonnherria", taxon_id = 571255L,
               correct_name = NA_character_, note = NA_character_)
  out <- apply_manual_overrides(df, ov)
  expect_equal(out$taxon_id[1], 571255L)
  expect_true(out$resolved[1])                       # override resolves the Holway row
})

test_that("apply_manual_overrides only matches the exact rank + name", {
  src("reference/manual_overrides.R")
  df <- tibble(rank = c("species", "genus"), genus = c("Holcopasites", "Holcopasites"),
               species = c("minima", NA_character_), taxon_id = NA_integer_,
               scientific_name = NA_character_)
  ov <- tibble(rank = "genus", name = "Holcopasites", taxon_id = 252959L,
               correct_name = NA_character_, note = NA_character_)
  out <- apply_manual_overrides(df, ov)
  expect_true(is.na(out$taxon_id[1]))    # species row NOT matched by a genus-rank override
  expect_equal(out$taxon_id[2], 252959L) # genus row matched
})

test_that("load_manual_overrides drops rows with a blank taxon_id", {
  src("reference/manual_overrides.R")
  p <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(
    rank = c("species", "species"), name = c("Holcopasites minima", "Foo bar"),
    taxon_id = c(NA, 5L), correct_name = c("Holcopasites minimus", NA), note = NA_character_), p)
  ov <- load_manual_overrides(p)
  expect_equal(nrow(ov), 1L)
  expect_equal(ov$taxon_id, 5L)
})

test_that("write_review_worklist surfaces the resolver's not_found taxa, dropping answered ones", {
  src("reference/manual_overrides.R")
  # a resolver cache like resolve_missing_ids.R writes: key = "rank|name|parent"
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(
    key = c("species|holcopasites minima|252959",
            "genus|megandrena|47222",
            "species|melissodes lupinus|52781"),
    taxon_id = c(NA, NA, 709268L),
    status = c("not_found_or_ambiguous", "not_found_or_ambiguous", "filled")), cache)
  out <- tempfile(fileext = ".csv")
  # Megandrena already answered in the overrides -> must NOT appear
  ov <- tibble(rank = "genus", name = "Megandrena", taxon_id = 55L,
               correct_name = NA_character_, note = NA_character_)
  wl <- write_review_worklist(cache_path = cache, overrides = ov, path = out)
  expect_equal(wl$name, "Holcopasites minima")            # only the open not_found, title-cased
  expect_false("Melissodes lupinus" %in% wl$name)         # 'filled' status excluded
  expect_false("Megandrena" %in% wl$name)                 # already answered -> dropped
  expect_true(all(grepl("inaturalist.org/taxa/search", wl$inat_search_url)))
  expect_true(all(is.na(wl$taxon_id)))                    # blank column to fill in
})

test_that("write_review_worklist keeps each name aligned with its own iNat link (dropped middle row)", {
  src("reference/manual_overrides.R")
  # four open not_found taxa; answering a MIDDLE one makes `keep` drop a non-terminal
  # row -- which is exactly when an index shift between the name and the URL surfaces.
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(
    key = c("species|alpha ex|100", "species|bravo ex|100",
            "species|charlie ex|100", "species|delta ex|100"),
    taxon_id = NA_integer_,
    status = "not_found_or_ambiguous"), cache)
  out <- tempfile(fileext = ".csv")
  ov <- tibble(rank = "species", name = "Bravo ex", taxon_id = 55L,
               correct_name = NA_character_, note = NA_character_)
  wl <- write_review_worklist(cache_path = cache, overrides = ov, path = out)
  expect_equal(nrow(wl), 3L)
  expect_false(any(grepl("q=NA", wl$inat_search_url)))     # no shifted-off NA link
  # each row's link must search for THAT row's own name (case-insensitive: name is
  # title-cased, the URL encodes the raw lowercase name)
  q <- vapply(sub("^.*q=", "", wl$inat_search_url), utils::URLdecode, character(1), USE.NAMES = FALSE)
  expect_equal(tolower(q), tolower(trimws(wl$name)))
})

test_that("prompt_missing_taxon_ids records an entered id (+ name) to the overrides file", {
  src("reference/manual_overrides.R")
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(key = "species|holcopasites minima|252959",
                          taxon_id = NA_integer_, status = "not_found_or_ambiguous"), cache)
  ovp <- tempfile(fileext = ".csv")
  pf <- function(prompt) "3480489 Holcopasites minimus"   # id + corrected name
  n <- prompt_missing_taxon_ids(cache_path = cache, overrides_path = ovp,
                                interactive_ok = TRUE, prompt_fn = pf)
  expect_equal(n, 1L)
  ov <- readr::read_csv(ovp, show_col_types = FALSE)
  expect_equal(ov$taxon_id[ov$name == "Holcopasites minima"], 3480489L)
  expect_equal(ov$correct_name[ov$name == "Holcopasites minima"], "Holcopasites minimus")
})

test_that("prompt_missing_taxon_ids is a no-op when non-interactive", {
  src("reference/manual_overrides.R")
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(key = "species|holcopasites minima|1",
                          taxon_id = NA_integer_, status = "not_found_or_ambiguous"), cache)
  ovp <- tempfile(fileext = ".csv")
  n <- prompt_missing_taxon_ids(cache_path = cache, overrides_path = ovp,
                                interactive_ok = FALSE, prompt_fn = function(p) "999")
  expect_equal(n, 0L)
  expect_false(file.exists(ovp))            # nothing written when non-interactive
})

test_that("prompt_missing_taxon_ids skips blank / 'n' answers", {
  src("reference/manual_overrides.R")
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(key = "species|holcopasites minima|1",
                          taxon_id = NA_integer_, status = "not_found_or_ambiguous"), cache)
  ovp <- tempfile(fileext = ".csv")
  n <- prompt_missing_taxon_ids(cache_path = cache, overrides_path = ovp,
                                interactive_ok = TRUE, prompt_fn = function(p) "n")
  expect_equal(n, 0L)                       # 'n' = no id yet -> nothing recorded
})

# The banner said `n` meant "no iNaturalist page; stop asking about it", but the
# parser treated `n` exactly like a blank line -- nothing recorded, asked again next
# run. The promise was never implemented. Brandi's call is to ask every run anyway
# (iNaturalist does add bees, so a permanent "never again" is the wrong default), so
# the fix is to make the banner tell the truth, and to accept every spelling of the
# answer rather than the three the parser happened to list.
test_that("every spelling of 'no page' and 'skip' is accepted", {
  src("reference/manual_overrides.R")
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(key = "species|holcopasites minima|1",
                          taxon_id = NA_integer_, status = "not_found_or_ambiguous"), cache)
  for (word in c("n", "N", "no", "No", "none", "NONE", "noid", "skip", "Skip", "s", "")) {
    ovp <- tempfile(fileext = ".csv")
    n <- prompt_missing_taxon_ids(cache_path = cache, overrides_path = ovp,
                                  interactive_ok = TRUE, prompt_fn = function(p) word)
    expect_equal(n, 0L, info = word)
    expect_false(file.exists(ovp), info = word)
  }
})

test_that("every spelling of quit stops the pass", {
  src("reference/manual_overrides.R")
  cache <- tempfile(fileext = ".csv")
  readr::write_csv(tibble(key = c("species|holcopasites minima|1", "species|stelis anthocopae|2"),
                          taxon_id = NA_integer_, status = "not_found_or_ambiguous"), cache)
  for (word in c("q", "Q", "quit", "Quit", "exit")) {
    asked <- 0L
    ovp <- tempfile(fileext = ".csv")
    prompt_missing_taxon_ids(cache_path = cache, overrides_path = ovp,
                             interactive_ok = TRUE,
                             prompt_fn = function(p) { asked <<- asked + 1L; word })
    expect_equal(asked, 1L, info = word)     # stopped after the first name, not both
  }
})

test_that("the banner no longer promises to stop asking", {
  src("reference/manual_overrides.R")
  said <- character(0)
  withCallingHandlers(.mo_banner(17L), message = function(m) {
    said <<- c(said, conditionMessage(m)); invokeRestart("muffleMessage") })
  txt <- gsub("[[:space:]]+", " ", paste(said, collapse = " "))   # wrapping is not content
  expect_false(grepl("stop asking", txt, ignore.case = TRUE))
  expect_match(txt, "asked again", fixed = TRUE)
})

# The merge that decides which answer wins was buried inside prompt_missing_taxon_ids,
# so the yearly taxon sweep could not record a correction without copying it. Pulled
# out so there is one rule for "a newer answer replaces an older one for the same bee".
test_that("a new answer replaces an older one for the same bee", {
  src("reference/manual_overrides.R")
  old <- tibble(rank = "species", name = "Andrena quercina", taxon_id = 62881L,
                correct_name = NA_character_, note = "old")
  new <- tibble(rank = "species", name = "Andrena quercina", taxon_id = 901455L,
                correct_name = "Andrena quercinella", note = "taxon sweep")
  out <- merge_manual_overrides(new, old)
  expect_equal(nrow(out), 1L)
  expect_equal(out$taxon_id, 901455L)
  expect_equal(out$note, "taxon sweep")
})

test_that("a different bee is kept alongside, not replaced", {
  src("reference/manual_overrides.R")
  old <- tibble(rank = "species", name = "Stelis anthocopae", taxon_id = 1L,
                correct_name = NA_character_, note = "old")
  new <- tibble(rank = "species", name = "Andrena quercina", taxon_id = 2L,
                correct_name = NA_character_, note = "new")
  expect_equal(nrow(merge_manual_overrides(new, old)), 2L)
})

test_that("the same name at a different rank is a different bee", {
  src("reference/manual_overrides.R")
  old <- tibble(rank = "genus", name = "Stelis", taxon_id = 1L,
                correct_name = NA_character_, note = "old")
  new <- tibble(rank = "subgenus", name = "Stelis", taxon_id = 2L,
                correct_name = NA_character_, note = "new")
  expect_equal(nrow(merge_manual_overrides(new, old)), 2L)
})

test_that("merging into nothing works", {
  src("reference/manual_overrides.R")
  new <- tibble(rank = "species", name = "A b", taxon_id = 1L,
                correct_name = NA_character_, note = "n")
  expect_equal(nrow(merge_manual_overrides(new, NULL)), 1L)
})

# "none" and "skip" do the same thing here -- line 259 sends both to `next`, so
# nothing is recorded either way. The banner said so, but the sentence was indented
# under `skip`, which reads as though it applied only to `skip` and left "none" looking
# permanent. Brandi read it that way mid-run and asked. Put the shared consequence
# where it cannot attach to one option.
test_that("none and skip are shown as having the same consequence", {
  src("reference/manual_overrides.R")
  said <- character(0)
  withCallingHandlers(.mo_banner(17L), message = function(m) {
    said <<- c(said, conditionMessage(m)); invokeRestart("muffleMessage") })
  lines <- unlist(strsplit(paste(said, collapse = ""), "\n", fixed = TRUE))

  i_none <- grep("^    none ", lines)
  i_skip <- grep("^    skip ", lines)
  expect_length(i_none, 1L); expect_length(i_skip, 1L)

  # the "asked again" sentence must not sit indented under skip alone
  i_again <- grep("asked again next run", lines)
  expect_true(length(i_again) == 1L)
  expect_false(grepl("^ {10,}", lines[i_again]),
               info = "the shared consequence is indented under one option")
  expect_match(lines[i_again], "[Nn]either|[Bb]oth|Either", info = "says it covers both")
})
