library(testthat)

# DB-backed tests. Run only where the duckdb R package is installed (e.g. the
# user's Mac). They also need the spatial extension for the geometry column;
# if it can't load (offline first run), the test skips rather than fails.

skip_if_no_store <- function() {
  if (!have_duckdb()) skip("duckdb R package not installed")
}

open_temp_store <- function() {
  src("config.R"); src("inat_observations/engine/db/store_conn.R"); src("inat_observations/engine/db/observations_store.R")
  src("inat_observations/engine/db/taxon_store.R"); src("inat_observations/engine/db/decision_store.R")
  path <- tempfile(fileext = ".duckdb")
  con <- tryCatch(store_connect(path), error = function(e) skip(conditionMessage(e)))
  con
}

test_that("observation upsert is idempotent and tracks the max id cursor", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)

  obs <- list(list(
    id = 12345, taxon = list(id = 632955), observed_on = "2021-04-12",
    geojson = list(type = "Point", coordinates = list(-117.24, 32.67))
  ))
  expect_equal(write_observations(con, obs), 1L)
  write_observations(con, obs)  # re-upsert same id
  expect_equal(count_observations(con), 1L)
  expect_equal(max_observation_id(con), 12345L)

  raw <- read_observations_raw(con)
  expect_equal(nrow(raw), 1L)
})

test_that("taxon cache round-trips by id and by name key", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)

  expect_null(taxon_cache_get(con, taxon_cache_key_id(1)))
  taxon <- list(id = 632955, name = "Melissodes robustior", rank = "species")
  taxon_cache_put(con, taxon_cache_key_id(632955), 632955, taxon)

  hit <- taxon_cache_get(con, taxon_cache_key_id(632955))
  expect_equal(hit$name, "Melissodes robustior")
  expect_equal(taxon_cache_count(con), 1)
})

test_that("decision store records pick and skip", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)

  expect_null(decision_get(con, "Andrena quercina"))
  decision_put(con, "Andrena quercina", "pick", 361408)
  d <- decision_get(con, "Andrena quercina")
  expect_equal(d$action, "pick")
  expect_equal(d$chosen_taxon_id, 361408L)

  decision_put(con, "Foo bar", "skip")
  expect_equal(decision_get(con, "Foo bar")$action, "skip")

  # "no iNat page for this bee" is a real, permanent answer -- holway_reference_build.R
  # writes it, reads it back to stop re-asking, and switches on it for itis_valid. The
  # whitelist here never learned the word, so the write threw, the caller's tryCatch
  # swallowed the error, and the operator's answer was silently discarded: the prompt
  # promised "you will not be asked again" and then asked again every run.
  decision_put(con, "Hesperapis ilicifoliae", "no_inat_id")
  expect_equal(decision_get(con, "Hesperapis ilicifoliae")$action, "no_inat_id")

  # The prompt now re-asks about these every run, so it has to be able to say WHEN
  # you last answered -- "you said no page, back in March" is what tells you whether
  # it is worth looking again.
  expect_match(as.character(decision_get(con, "Hesperapis ilicifoliae")$decided_at),
               "^[0-9]{4}-[0-9]{2}-[0-9]{2}")
})

test_that("ingest_observations pages via raw text and DuckDB-side parsing", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  src("inat_observations/engine/api/inat_http.R"); src("inat_observations/engine/api/inat_flatten.R"); src("inat_observations/engine/api/inat_cache.R")
  src("inat_observations/engine/pipelines/ingest_inat.R")

  # fake API returns RAW response strings (what inat_request_text yields)
  recs <- lapply(1:5, function(i) list(
    id = i, taxon = list(id = i * 10), observed_on = "2021-04-12",
    geojson = list(type = "Point", coordinates = list(-117.24 + i * 0.001, 32.67))
  ))
  fake_text <- function(path, query = list(), ...) {
    above <- as.numeric(query$id_above %||% 0)
    remaining <- Filter(function(r) r$id > above, recs)
    page <- head(remaining, query$per_page)
    as.character(jsonlite::toJSON(list(total_results = length(remaining), results = page),
                                  auto_unbox = TRUE, null = "null"))
  }

  # per_page = 2 forces 3 pages; commit_every = 1 commits each page
  # state_path MUST be injected: its default is repo-root-relative, so a test run
  # would write a real last_ingest.txt under tests/testthat/ and dirty the repo.
  n <- ingest_observations(con, place_id = 1, taxon_id = 1, without_taxon_id = 1,
                           incremental = FALSE, per_page = 2L, commit_every = 1L, throttle = 0,
                           state_path = file.path(tempdir(), "last_ingest_test.txt"),
                           request_text_fn = fake_text, sleep_fn = function(...) NULL, verbose = FALSE)
  expect_equal(n, 5L)
  expect_equal(count_observations(con), 5L)
  expect_equal(max_observation_id(con), 5L)
})

test_that("read_observations_export caches the flatten and invalidates on change", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  src("inat_observations/engine/api/inat_http.R"); src("inat_observations/engine/api/inat_flatten.R"); src("inat_observations/engine/api/inat_cache.R")
  src("inat_observations/engine/db/observations_store.R"); src("inat_observations/engine/pipelines/read_inat.R")

  page <- function(recs) as.character(jsonlite::toJSON(list(results = recs),
                                                       auto_unbox = TRUE, null = "null"))
  mk <- function(i) list(id = i, taxon = list(id = 100), observed_on = "2021-04-12",
                         geojson = list(type = "Point", coordinates = list(-117.2, 32.6)))
  write_observations_page(con, page(lapply(1:3, mk)))

  taxon <- jsonlite::fromJSON(fx("taxon_sample.json"), simplifyVector = FALSE)
  fake_req <- function(path, query = list(), ...) {
    ids <- as.integer(strsplit(sub("^taxa/", "", path), ",")[[1]])
    list(results = lapply(ids, function(id) { t <- taxon; t$id <- id; t }))
  }
  cache <- tempfile(fileext = ".rds")

  d1 <- read_observations_export(con, request_fn = fake_req, verbose = FALSE, cache_path = cache)
  expect_equal(nrow(d1), 3L)
  expect_true(file.exists(cache))

  # unchanged inputs -> disk cache reused, identical result
  d2 <- read_observations_export(con, request_fn = fake_req, verbose = FALSE, cache_path = cache)
  expect_equal(nrow(d2), 3L)

  # adding an observation changes the signature -> rebuild picks it up
  write_observations_page(con, page(list(mk(4))))
  d3 <- read_observations_export(con, request_fn = fake_req, verbose = FALSE, cache_path = cache)
  expect_equal(nrow(d3), 4L)
})

test_that("resolve_taxonomy caches: second call makes no API request", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  src("inat_observations/engine/api/inat_http.R"); src("inat_observations/engine/api/inat_flatten.R"); src("inat_observations/engine/api/inat_cache.R")

  taxon <- jsonlite::fromJSON(fx("taxon_sample.json"), simplifyVector = FALSE)
  calls <- 0
  fake_request <- function(path, query = list(), user_agent = NULL) {
    calls <<- calls + 1
    list(results = list(taxon))
  }

  m1 <- resolve_taxonomy(con, c(632955), request_fn = fake_request, throttle = 0, verbose = FALSE)
  expect_equal(m1$taxon_genus_name[1], "Melissodes")
  expect_equal(calls, 1)

  # second resolve hits the cache -> no new request
  m2 <- resolve_taxonomy(con, c(632955), request_fn = fake_request, throttle = 0, verbose = FALSE)
  expect_equal(calls, 1)
  expect_equal(m2$taxon_family_name[1], "Apidae")
})

test_that("resolve_taxonomy batches many ids into one request (rate-limit fix)", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  src("inat_observations/engine/api/inat_http.R"); src("inat_observations/engine/api/inat_flatten.R"); src("inat_observations/engine/api/inat_cache.R")

  base <- jsonlite::fromJSON(fx("taxon_sample.json"), simplifyVector = FALSE)
  calls <- 0
  fake_request <- function(path, query = list(), user_agent = NULL) {
    calls <<- calls + 1
    ids <- as.integer(strsplit(sub("^taxa/", "", path), ",")[[1]])
    list(results = lapply(ids, function(id) { t <- base; t$id <- id; t }))
  }

  m <- resolve_taxonomy(con, c(632955, 111, 222), request_fn = fake_request,
                        throttle = 0, verbose = FALSE)
  expect_equal(nrow(m), 3)
  expect_equal(calls, 1)                 # 3 ids resolved in ONE batched request
  expect_true(all(m$taxon_family_name == "Apidae"))
})

# A human's pick is replayed forever: resolve_holway_row() returns the stored
# chosen_taxon_id before any API call, and nothing ever re-validates it. So when
# iNaturalist retires that taxon, the decision keeps handing back a dead number and
# the yearly sweep cannot undo it -- the pick outranks everything downstream.
#
# Wiping every decision would mean re-answering hundreds of questions, nearly all of
# which are still right. Only the ones pointing at a taxon that actually MOVED need
# asking again, so the store has to be able to say which those are, and to forget
# just those.
test_that("the store can say which decisions point at a given taxon", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  decision_put(con, "Andrena quercina", "pick", 62881L)
  decision_put(con, "Stelis anthocopae", "pick", 199123L)
  decision_put(con, "Bombus crotchii", "pick", 118970L)

  expect_setequal(decisions_for_taxa(con, c(62881L, 199123L)),
                  c("Andrena quercina", "Stelis anthocopae"))
  expect_length(decisions_for_taxa(con, 999999L), 0L)
  expect_length(decisions_for_taxa(con, integer(0)), 0L)
})

test_that("a decision with no chosen taxon is never matched", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  decision_put(con, "Hesperapis ilicifoliae", "no_inat_id")
  expect_length(decisions_for_taxa(con, 62881L), 0L)
})

test_that("forgetting one decision leaves the rest alone", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  decision_put(con, "Andrena quercina", "pick", 62881L)
  decision_put(con, "Bombus crotchii", "pick", 118970L)

  expect_equal(decision_forget(con, "Andrena quercina"), 1L)
  expect_null(decision_get(con, "Andrena quercina"))       # asked again next build
  expect_equal(decision_get(con, "Bombus crotchii")$chosen_taxon_id, 118970L)
})

test_that("forgetting several at once works, and forgetting nothing is safe", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  decision_put(con, "A b", "pick", 1L); decision_put(con, "C d", "pick", 2L)
  expect_equal(decision_forget(con, c("A b", "C d")), 2L)
  expect_equal(decision_count(con), 0L)
  expect_equal(decision_forget(con, character(0)), 0L)
})

test_that("forgetting a decision that was never made is not an error", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  expect_equal(decision_forget(con, "never asked"), 0L)
})

# The version notice needs to know which dropped checklist names had an answer on
# file, so it can say those answers are now orphaned instead of leaving them to be
# discovered years later.
test_that("the store can list every search term it holds an answer for", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  expect_length(decisions_all_terms(con), 0L)
  decision_put(con, "Andrena quercina", "pick", 62881L)
  decision_put(con, "Hesperapis ilicifoliae", "no_inat_id")
  expect_setequal(decisions_all_terms(con), c("Andrena quercina", "Hesperapis ilicifoliae"))
})

# A saved answer for a bee that has left the checklist is never read again: the Holway
# build loops over the names in the CURRENT sheet, so a dropped name is never looked
# up. They are harmless, but they accumulate across checklist versions, and someone
# reading that table in five years finds answers for bees that left two versions ago.
#
# The guard matters more than the cleanup. This is a DELETE driven by a file read: if
# the checklist fails to load, or loads empty, an unguarded sweep would wipe every
# answer in the store on the strength of a bad read.
test_that("answers for names no longer on the checklist are dropped", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  decision_put(con, "Andrena quercina", "pick", 1L)
  decision_put(con, "Holcopasites minima", "pick", 2L)      # respelled away in v4
  current <- c("Andrena quercina", "Holcopasites minimus", paste("Filler bee", 1:200))

  expect_equal(forget_orphan_decisions(con, current), 1L)
  expect_null(decision_get(con, "Holcopasites minima"))
  expect_equal(decision_get(con, "Andrena quercina")$chosen_taxon_id, 1L)
})

test_that("an empty checklist deletes nothing at all", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  decision_put(con, "Andrena quercina", "pick", 1L)
  expect_equal(forget_orphan_decisions(con, character(0)), 0L)
  expect_equal(decision_count(con), 1L)
})

test_that("a suspiciously short checklist deletes nothing -- a bad read must not wipe the store", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  for (i in 1:5) decision_put(con, paste("Bee", i), "pick", i)
  expect_equal(forget_orphan_decisions(con, c("Bee 1", "Bee 2")), 0L)   # only 2 names
  expect_equal(decision_count(con), 5L)
})

test_that("stray whitespace does not make a name look dropped", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  decision_put(con, "Andrena quercina", "pick", 1L)
  expect_equal(forget_orphan_decisions(con, c("  Andrena quercina  ", paste("Filler bee", 1:200))), 0L)
  expect_equal(decision_count(con), 1L)
})

# A checklist version bump wipes every saved answer. Not for tidiness -- because a
# surviving NAME is not evidence the bee survived unchanged. Holway can keep a
# spelling and change what it means (a split where the old name is kept for one of
# the pieces), and nothing in the string reveals that. Reusing the answer assumes it
# did not happen; wiping costs 40 minutes of machine time and ~22 real questions,
# and re-asks exactly the ambiguous bees where a concept shift is most likely.
#
# Cheap, loud, recoverable -- and "recoverable" has to be literal, not theoretical,
# so the answers are written out before they are dropped.
test_that("every answer can be written out before being dropped", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  decision_put(con, "Andrena quercina", "pick", 62881L)
  decision_put(con, "Hesperapis ilicifoliae", "no_inat_id")

  f <- tempfile(fileext = ".csv")
  expect_equal(decision_export(con, f), 2L)
  d <- read.csv(f, stringsAsFactors = FALSE)
  expect_setequal(d$search_term, c("Andrena quercina", "Hesperapis ilicifoliae"))
  expect_equal(d$chosen_taxon_id[d$search_term == "Andrena quercina"], 62881L)
  expect_true("action" %in% names(d) && "decided_at" %in% names(d))
})

test_that("exporting an empty store writes a file with no rows, not nothing", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  f <- tempfile(fileext = ".csv")
  expect_equal(decision_export(con, f), 0L)
  expect_true(file.exists(f))
})

test_that("wiping drops everything and reports how many", {
  skip_if_no_store()
  con <- open_temp_store(); on.exit(store_disconnect(con), add = TRUE)
  for (i in 1:5) decision_put(con, paste("Bee", i), "pick", i)
  expect_equal(decision_forget_all(con), 5L)
  expect_equal(decision_count(con), 0L)
  expect_equal(decision_forget_all(con), 0L)          # wiping twice is safe
})

# The trigger. An absent marker means "we have never recorded which version these
# answers came from" -- which must NOT wipe: the first run after this feature ships
# would destroy every answer on the strength of a file that never existed.
test_that("a version change is only a change when both versions are known", {
  expect_true(holway_version_bumped(now = 4L, was = 3L))
  expect_false(holway_version_bumped(now = 3L, was = 3L))
  expect_false(holway_version_bumped(now = 4L, was = NA_integer_))   # never recorded
  expect_false(holway_version_bumped(now = NA_integer_, was = 3L))   # unreadable now
  expect_false(holway_version_bumped(now = 3L, was = 4L))            # rebuilding an older one
})
