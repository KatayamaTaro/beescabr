library(testthat)

# Every cached taxon is write-once/read-forever: prefetch_taxa() fetches only ids it
# does NOT already have, get_taxon_by_id() returns a cached blob with no age check,
# and nothing in the repo ever clears or re-fetches one. So a bee keeps whatever name,
# genus and family iNaturalist reported the first time that id was ever looked up.
#
# iNaturalist does revise taxa: a taxon that is split, lumped or swapped is retired
# (is_active = FALSE) and its records move to a NEW id. Our observations follow --
# they are re-pulled every run -- but the reference tables do not, so the checklist
# row and the records drift apart, and the bee re-appears in the county-additions
# list as if it were a discovery.
#
# Brandi's call: rebuild these caches once a year. This is the machinery for that --
# re-fetch what we already hold, and report what iNaturalist now says differently.

have_duckdb <- function() requireNamespace("duckdb", quietly = TRUE)
src("config.R")
src("inat_observations/engine/db/store_conn.R")
src("inat_observations/engine/db/taxon_store.R")
src("inat_observations/engine/api/inat_cache.R")

tx <- function(id, name, rank = "species", active = TRUE)
  list(id = id, name = name, rank = rank, is_active = active)

test_that("a cached taxon is normally left alone", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  taxon_cache_put(con, taxon_cache_key_id(1L), 1L, tx(1L, "Bombus crotchii"))

  asked <- integer(0)
  n <- prefetch_taxa(con, c(1L), verbose = FALSE, sleep_fn = function(...) NULL,
                     request_fn = function(...) { asked <<- c(asked, 1L); list() })
  expect_equal(n, 0L)
  expect_length(asked, 0L)
})

test_that("force re-fetches a taxon we already hold", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  taxon_cache_put(con, taxon_cache_key_id(1L), 1L, tx(1L, "Bombus crotchii"))

  n <- prefetch_taxa(con, c(1L), force = TRUE, verbose = FALSE, sleep_fn = function(...) NULL,
                     request_fn = function(...) list(results = list(tx(1L, "Bombus crotchii"))))
  expect_equal(n, 1L)
})

# The point of the sweep: say what iNaturalist now reports differently. Pure, so the
# comparison is testable without a network or a database.
test_that("an unchanged taxon is not reported", {
  expect_equal(nrow(taxon_changes(list(tx(1L, "Bombus crotchii")),
                                  list(tx(1L, "Bombus crotchii")))), 0L)
})

test_that("a renamed taxon is reported with both names", {
  ch <- taxon_changes(list(tx(1L, "Andrena quercina")), list(tx(1L, "Andrena quercinella")))
  expect_equal(nrow(ch), 1L)
  expect_equal(ch$taxon_id, 1L)
  expect_equal(ch$was, "Andrena quercina")
  expect_equal(ch$now, "Andrena quercinella")
  expect_equal(ch$change, "renamed")
})

test_that("a retired taxon is reported as retired, which is the serious one", {
  ch <- taxon_changes(list(tx(1L, "Stelis anthocopae")),
                      list(tx(1L, "Stelis anthocopae", active = FALSE)))
  expect_equal(ch$change, "retired")
})

test_that("an id iNaturalist no longer returns at all is reported as gone", {
  ch <- taxon_changes(list(tx(1L, "Stelis anthocopae")), list())
  expect_equal(ch$change, "gone")
  expect_true(is.na(ch$now))
})

test_that("a rank change counts as a change", {
  ch <- taxon_changes(list(tx(1L, "Lasioglossum", rank = "genus")),
                      list(tx(1L, "Lasioglossum", rank = "subgenus")))
  expect_equal(ch$change, "rank changed")
})

test_that("retired beats renamed when both happened", {
  ch <- taxon_changes(list(tx(1L, "A b")), list(tx(1L, "A c", active = FALSE)))
  expect_equal(ch$change, "retired")
})

test_that("many taxa at once, only the changed ones come back", {
  before <- list(tx(1L, "A b"), tx(2L, "C d"), tx(3L, "E f"))
  after  <- list(tx(1L, "A b"), tx(2L, "C dd"), tx(3L, "E f", active = FALSE))
  ch <- taxon_changes(before, after)
  expect_equal(sort(ch$taxon_id), c(2L, 3L))
})

# The sweep: re-ask iNaturalist about every id our reference tables hold, and report
# what moved. The trap is "gone" -- prefetch_taxa() only WRITES what comes back, so a
# retired id silently keeps its stale cache row, and reading the cache afterwards
# would show no change at all. The sweep must compare against what the API actually
# returned, not against the cache it just wrote.

test_that("nothing moved, nothing reported", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  taxon_cache_put(con, taxon_cache_key_id(1L), 1L, tx(1L, "Bombus crotchii"))
  ch <- sweep_taxon_changes(con, 1L, verbose = FALSE, sleep_fn = function(...) NULL,
                            request_fn = function(...) list(results = list(tx(1L, "Bombus crotchii"))))
  expect_equal(nrow(ch), 0L)
})

test_that("an id iNaturalist no longer returns is reported as gone", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  taxon_cache_put(con, taxon_cache_key_id(1L), 1L, tx(1L, "Stelis anthocopae"))
  ch <- sweep_taxon_changes(con, 1L, verbose = FALSE, sleep_fn = function(...) NULL,
                            request_fn = function(...) list(results = list()))
  expect_equal(ch$change, "gone")
  expect_equal(ch$was, "Stelis anthocopae")
})

test_that("a retired taxon is reported, and the old name survives to name it", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  taxon_cache_put(con, taxon_cache_key_id(7L), 7L, tx(7L, "Andrena quercina"))
  ch <- sweep_taxon_changes(con, 7L, verbose = FALSE, sleep_fn = function(...) NULL,
                            request_fn = function(...) list(results = list(tx(7L, "Andrena quercina", active = FALSE))))
  expect_equal(ch$change, "retired")
  expect_equal(ch$was, "Andrena quercina")
})

test_that("an id we have never cached cannot be compared, so it is not reported", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  ch <- sweep_taxon_changes(con, 99L, verbose = FALSE, sleep_fn = function(...) NULL,
                            request_fn = function(...) list(results = list(tx(99L, "New bee"))))
  expect_equal(nrow(ch), 0L)
})

test_that("the fresh answer replaces the cached one for taxa that still exist", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  taxon_cache_put(con, taxon_cache_key_id(1L), 1L, tx(1L, "Andrena quercina"))
  sweep_taxon_changes(con, 1L, verbose = FALSE, sleep_fn = function(...) NULL,
                      request_fn = function(...) list(results = list(tx(1L, "Andrena quercinella"))))
  expect_equal(taxon_cache_get(con, taxon_cache_key_id(1L))$name, "Andrena quercinella")
})

test_that("a retired id keeps its cache row rather than being wiped", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  taxon_cache_put(con, taxon_cache_key_id(1L), 1L, tx(1L, "Stelis anthocopae"))
  sweep_taxon_changes(con, 1L, verbose = FALSE, sleep_fn = function(...) NULL,
                      request_fn = function(...) list(results = list()))
  # the bee is still on our checklist; losing its name would make the review
  # file unreadable and break every downstream label
  expect_equal(taxon_cache_get(con, taxon_cache_key_id(1L))$name, "Stelis anthocopae")
})

test_that("ids are asked in batches, not one request each", {
  if (!have_duckdb()) skip("duckdb not installed")
  con <- store_connect(tempfile(fileext = ".duckdb")); on.exit(store_disconnect(con), add = TRUE)
  ids <- 1:70
  for (i in ids) taxon_cache_put(con, taxon_cache_key_id(i), i, tx(i, paste("Bee", i)))
  calls <- 0L
  sweep_taxon_changes(con, ids, verbose = FALSE, sleep_fn = function(...) NULL,
                      request_fn = function(path, ...) {
                        calls <<- calls + 1L
                        hit <- as.integer(strsplit(sub("^taxa/", "", path), ",")[[1]])
                        list(results = lapply(hit, function(i) tx(i, paste("Bee", i))))
                      })
  expect_lt(calls, 10L)        # 70 ids, 30 per batch -> 3
})

# iNaturalist DOES say what a retired taxon became: every taxon object carries
# current_synonymous_taxon_ids (confirmed against the real cached taxa in this repo).
# So the common revision -- a swap or a lump, one replacement -- can be a question with
# both bees on screen, instead of a review file and a manual CSV edit.
#
# A SPLIT cannot. Several replacements means the bee became two, and which of our
# records belong to which is a per-record judgement no tool can make. Those stay manual,
# and saying so is part of the job.

test_that("a retired taxon carries what replaced it", {
  ch <- taxon_changes(list(tx(1L, "Andrena quercina")),
                      list(c(tx(1L, "Andrena quercina", active = FALSE),
                             list(current_synonymous_taxon_ids = list(901455L)))))
  expect_equal(ch$replaced_by, "901455")
})

test_that("a split lists every replacement", {
  ch <- taxon_changes(list(tx(1L, "A b")),
                      list(c(tx(1L, "A b", active = FALSE),
                             list(current_synonymous_taxon_ids = list(11L, 22L)))))
  expect_equal(ch$replaced_by, "11,22")
})

test_that("no replacement listed is blank, not the word NULL", {
  ch <- taxon_changes(list(tx(1L, "A b")), list(tx(1L, "A b", active = FALSE)))
  expect_true(is.na(ch$replaced_by))
})

test_that("an unchanged or renamed taxon has nothing to be replaced by", {
  ch <- taxon_changes(list(tx(1L, "A b")), list(tx(1L, "A c")))
  expect_equal(ch$change, "renamed")
  expect_true(is.na(ch$replaced_by))
})

test_that("only a retirement with exactly ONE replacement can be offered as a choice", {
  ch <- data.frame(change      = c("retired", "retired", "retired", "gone", "renamed"),
                   replaced_by = c("901455", "11,22",    NA,        NA,     NA),
                   stringsAsFactors = FALSE)
  ask <- taxon_changes_askable(ch)
  expect_equal(nrow(ask), 1L)
  expect_equal(ask$replaced_by, "901455")
})

test_that("a split is reported as needing a person, not quietly dropped", {
  ch <- data.frame(change      = c("retired", "retired"),
                   replaced_by = c("901455", "11,22"), stringsAsFactors = FALSE)
  man <- taxon_changes_manual(ch)
  expect_equal(nrow(man), 1L)
  expect_equal(man$replaced_by, "11,22")
})

# A retirement with NO replacement named fell through both lists: not askable (no
# second number to offer) and not in the manual list either (that only looked for
# a comma). It printed in the RETIRED block with no guidance at all, which is the
# worst case to leave unexplained -- the records have moved and nothing says where.
test_that("a retirement naming no replacement still needs a person", {
  ch <- data.frame(change      = c("retired", "retired", "retired"),
                   replaced_by = c("901455", "11,22", NA), stringsAsFactors = FALSE)
  man <- taxon_changes_manual(ch)
  expect_equal(nrow(man), 2L)                       # the split AND the unnamed one
  expect_true(any(is.na(man$replaced_by)))
})

test_that("every retirement lands in exactly one of the two lists", {
  ch <- data.frame(change      = c("retired", "retired", "retired", "gone", "renamed"),
                   replaced_by = c("901455", "11,22", NA, NA, NA), stringsAsFactors = FALSE)
  n_ret <- sum(ch$change == "retired")
  expect_equal(nrow(taxon_changes_askable(ch)) + nrow(taxon_changes_manual(ch)), n_ret)
})

# "Manual list" meant: here are some bees, now go open a spreadsheet. Every other
# taxon prompt in this pipeline lets you type the number straight in, and these are
# the cases where typing matters MOST -- a split, or a retirement iNaturalist gave no
# replacement for, are exactly the ones where the tool has no candidate to offer but a
# person reading the page does. So all three flavors share one prompt, and what varies
# is only whether "take" is on the menu.
src("utils/answers.R")

test_that("a typed number is read as the new taxon_id", {
  expect_equal(taxon_change_choice("901455")$action, "id")
  expect_equal(taxon_change_choice("901455")$id, 901455L)
  expect_equal(taxon_change_choice("  901455 ")$id, 901455L)
})

test_that("keep is the answer that changes nothing, and Enter means keep", {
  for (w in c("", "keep", "Keep", "k", "no", "n", "skip"))
    expect_equal(taxon_change_choice(w)$action, "keep", info = w)
})

test_that("take is offered only when there is something to take", {
  expect_equal(taxon_change_choice("take", has_offer = TRUE)$action, "take")
  expect_equal(taxon_change_choice("take", has_offer = FALSE)$action, "unclear")
})

test_that("quit stops the review", {
  for (w in c("q", "quit", "exit")) expect_equal(taxon_change_choice(w)$action, "quit", info = w)
})

test_that("nonsense is not guessed at", {
  expect_equal(taxon_change_choice("banana")$action, "unclear")
  expect_equal(taxon_change_choice("0")$action, "unclear")
  expect_equal(taxon_change_choice("-5")$action, "unclear")
  expect_equal(taxon_change_choice("12.5")$action, "unclear")
})

test_that("a typed number wins over take even when both are available", {
  r <- taxon_change_choice("901501", has_offer = TRUE)
  expect_equal(r$action, "id")
  expect_equal(r$id, 901501L)
})
