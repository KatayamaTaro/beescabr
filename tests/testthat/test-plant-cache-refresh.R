library(testthat)

# plant_name_cache is the one reference cache with no way to refresh it at all: it is
# not in REFRESH_CACHES, so nothing ages it and nothing prompts, and plt_resolve_names()
# takes the cached row whenever the input name is present. A plant name resolved once in
# 2024 keeps its 2024 taxon_id forever.
#
# That matters more for plants than for bees, because plants are re-resolved BY NAME.
# When iNaturalist retires a taxon the same name resolves to a DIFFERENT id -- so the
# interesting change here is not "renamed", it is "this name now points at another
# taxon", which silently re-points every bee-plant join that uses it.
src("reference/plant_taxonomy_lookup_build.R")

row <- function(input_name, taxon_id, sci, rank = "species", resolved = TRUE)
  data.frame(input_name = input_name, taxon_id = as.character(taxon_id),
             scientific_name = sci, rank = rank, resolved = as.character(resolved),
             stringsAsFactors = FALSE)

test_that("force re-resolves a name already in the cache", {
  cache <- row("Isocoma menziesii", 1L, "Isocoma menziesii")
  asked <- character(0)
  out <- plt_resolve_names("Isocoma menziesii", cache = cache, force = TRUE,
                           resolve_fn = function(nm) {
                             asked <<- c(asked, nm)
                             row(nm, 2L, "Isocoma menziesii")[, -1, drop = FALSE]
                           })
  expect_equal(asked, "Isocoma menziesii")
})

test_that("without force the cached row is still used", {
  cache <- row("Isocoma menziesii", 1L, "Isocoma menziesii")
  asked <- character(0)
  plt_resolve_names("Isocoma menziesii", cache = cache,
                    resolve_fn = function(nm) { asked <<- c(asked, nm); row(nm, 2L, "x")[, -1, drop = FALSE] })
  expect_length(asked, 0L)
})

test_that("nothing changed, nothing reported", {
  a <- row("Isocoma menziesii", 1L, "Isocoma menziesii")
  expect_equal(nrow(plant_cache_changes(a, a)), 0L)
})

test_that("a name that now points at a different taxon is the serious finding", {
  ch <- plant_cache_changes(row("Isocoma menziesii", 1L, "Isocoma menziesii"),
                            row("Isocoma menziesii", 9L, "Isocoma menziesii"))
  expect_equal(nrow(ch), 1L)
  expect_equal(ch$change, "different taxon")
  expect_equal(ch$id_was, "1")
  expect_equal(ch$id_now, "9")
})

test_that("a name that stopped resolving is reported", {
  ch <- plant_cache_changes(row("Madia sp.", 1L, "Madia"),
                            row("Madia sp.", NA, NA, resolved = FALSE))
  expect_equal(ch$change, "no longer resolves")
})

test_that("a same-id rename is reported as a rename", {
  ch <- plant_cache_changes(row("Isocoma menziesii", 1L, "Isocoma menziesii"),
                            row("Isocoma menziesii", 1L, "Isocoma menziesii var. vernonioides"))
  expect_equal(ch$change, "renamed")
})

test_that("a different taxon outranks a rename when both happened", {
  ch <- plant_cache_changes(row("Isocoma menziesii", 1L, "Isocoma menziesii"),
                            row("Isocoma menziesii", 9L, "Isocoma vernonioides"))
  expect_equal(ch$change, "different taxon")
})

test_that("only the changed rows come back", {
  before <- rbind(row("A b", 1L, "A b"), row("C d", 2L, "C d"), row("E f", 3L, "E f"))
  after  <- rbind(row("A b", 1L, "A b"), row("C d", 9L, "C d"), row("E f", 3L, "E ff"))
  ch <- plant_cache_changes(before, after)
  expect_equal(sort(ch$input_name), c("C d", "E f"))
})

test_that("a name absent from the new cache is not mistaken for a change", {
  ch <- plant_cache_changes(row("A b", 1L, "A b"), row("C d", 2L, "C d"))
  expect_equal(nrow(ch), 0L)     # not re-resolved this run, so nothing is known
})

# The tool wrote the new cache and THEN printed what moved -- so "different taxon",
# the one change that re-points every bee-plant join, was applied before anyone saw
# it. A rename is cosmetic (same id, new spelling) and can be taken silently. An id
# change is a judgement: only a person can say whether iNaturalist's new match is the
# same plant. Ask about those, take the rest.
src("utils/answers.R")

test_that("only the changes that move an id need a person", {
  ch <- data.frame(change = c("renamed", "different taxon", "no longer resolves"),
                   stringsAsFactors = FALSE)
  expect_equal(plant_changes_to_ask(ch)$change, c("different taxon", "no longer resolves"))
})

test_that("nothing to ask about when only names were tidied", {
  expect_equal(nrow(plant_changes_to_ask(data.frame(change = "renamed",
                                                    stringsAsFactors = FALSE))), 0L)
})

test_that("take means use the new number", {
  for (w in c("take", "Take", "TAKE", "t", "yes", "y"))
    expect_equal(plant_change_choice(w), "take", info = w)
})

test_that("keep means leave it alone, and Enter does the same", {
  for (w in c("keep", "Keep", "k", "no", "n", ""))
    expect_equal(plant_change_choice(w), "keep", info = w)
})

test_that("quit stops the review", {
  for (w in c("q", "quit", "Quit", "exit"))
    expect_equal(plant_change_choice(w), "quit", info = w)
})

test_that("anything else is not understood, so nothing is assumed", {
  expect_equal(plant_change_choice("banana"), "unclear")
  expect_equal(plant_change_choice("57018"), "unclear")
})

test_that("keeping restores the row we had, taking leaves the new one", {
  before <- data.frame(input_name = "Isocoma menziesii", taxon_id = "57018",
                       scientific_name = "Isocoma menziesii", rank = "species",
                       resolved = "TRUE", stringsAsFactors = FALSE)
  after  <- data.frame(input_name = "Isocoma menziesii", taxon_id = "901122",
                       scientific_name = "Isocoma menziesii", rank = "species",
                       resolved = "TRUE", stringsAsFactors = FALSE)
  kept <- plant_apply_keep(after, before, "Isocoma menziesii")
  expect_equal(kept$taxon_id, "57018")
  expect_equal(nrow(kept), 1L)                    # replaced, not appended
  expect_equal(plant_apply_keep(after, before, character(0))$taxon_id, "901122")
})
