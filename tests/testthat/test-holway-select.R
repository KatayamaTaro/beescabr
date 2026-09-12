library(testthat)

sp <- function(rank) list(id = 1, name = "x", rank = rank)
mk <- function(id, name, rank) list(id = id, name = name, rank = rank)

test_that("exact name match auto-picks (subspecies, with ssp. normalization)", {
  src("reference/holway_reference_build.R")
  results <- list(mk(1, "Ashmeadiella cactorum", "species"),
                  mk(313836, "Ashmeadiella cactorum basalis", "subspecies"))
  res <- select_taxon_candidate(results, described = TRUE,
                                search_term = "Ashmeadiella cactorum ssp. basalis",
                                is_subspecies = TRUE)
  expect_equal(res$action, "pick")
  expect_equal(res$index, 2L)                       # the subspecies, not the parent
})

test_that("subspecies with no exact match does NOT grab the parent species", {
  src("reference/holway_reference_build.R")
  results <- list(mk(1, "Atoposmia copelandica", "species"))   # only the species exists
  res <- select_taxon_candidate(results, described = TRUE,
                                search_term = "Atoposmia copelandica ssp. albomarginata",
                                is_subspecies = TRUE)
  expect_equal(res$action, "skip")                  # -> routes to ITIS keep/skip
})

test_that("exact match disambiguates near-spellings (Andrena nigra vs nigrae)", {
  src("reference/holway_reference_build.R")
  results <- list(mk(1, "Andrena nigrae", "species"), mk(2, "Andrena nigra", "species"))
  res <- select_taxon_candidate(results, described = TRUE, search_term = "Andrena nigra")
  expect_equal(res$action, "pick")
  expect_equal(res$index, 2L)
})

test_that("an aff. row never inherits its Described sibling's cached taxon_id", {
  src("reference/holway.R"); src("reference/holway_reference_build.R")
  # the Described sibling ("miserabilis") is already cached under the SHARED key
  # "Habropoda miserabilis"; the aff. row must NOT read it.
  assign("decision_get", function(con, key)
    if (identical(key, "Habropoda miserabilis")) list(chosen_taxon_id = 307633L, action = "pick") else NULL,
    envir = globalenv())
  assign("decision_put", function(con, key, action, id = NA) invisible(NULL), envir = globalenv())
  on.exit(rm(list = c("decision_get", "decision_put"), envir = globalenv()), add = TRUE)

  aff <- resolve_holway_row(NULL, "Unpublished", "Habropoda", "aff. miserabilis sp. nov.", interactive_ok = FALSE)
  expect_true(is.na(aff$taxon_id))       # NOT 307633 -- aff. is a different species
  expect_equal(aff$action, "tentative")
  sib <- resolve_holway_row(NULL, "Described", "Habropoda", "miserabilis", interactive_ok = FALSE)
  expect_equal(sib$taxon_id, 307633L)    # the real species still resolves normally
})

test_that("a same-named complex never wins over the species (Andrena osmioides)", {
  src("reference/holway_reference_build.R")
  # iNat lists the COMPLEX first, then the species -- both named "Andrena osmioides"
  results <- list(mk(1258784, "Andrena osmioides", "complex"),
                  mk(573001,  "Andrena osmioides", "species"))
  res <- select_taxon_candidate(results, described = TRUE, search_term = "Andrena osmioides")
  expect_equal(res$action, "pick")
  expect_equal(res$index, 2L)                       # the SPECIES, not the complex

  # order-independent: species first, complex second -> still the species
  res_b <- select_taxon_candidate(list(mk(573001, "Andrena osmioides", "species"),
                                       mk(1258784, "Andrena osmioides", "complex")),
                                  described = TRUE, search_term = "Andrena osmioides")
  expect_equal(res_b$index, 1L)

  # NEVER a complex solo: if the only match is a complex, take nothing
  res_c <- select_taxon_candidate(list(mk(1258784, "Andrena osmioides", "complex")),
                                  described = TRUE, search_term = "Andrena osmioides")
  expect_equal(res_c$action, "skip")

  # a multi-result search where the only species is buried under a complex still
  # picks the species (no exact-name shortcut here)
  res_d <- select_taxon_candidate(list(mk(1, "Andrena osmioides", "complex"),
                                       mk(2, "Andrena foo", "species")),
                                  described = TRUE)
  expect_equal(res_d$action, "pick"); expect_equal(res_d$index, 2L)
})

test_that("resolved subspecies scientific_name gets the ssp. display form", {
  src("reference/holway_reference_build.R")
  ranks <- tibble::tibble(
    taxon_id = 313836L,
    taxon_kingdom_name = "Animalia", taxon_phylum_name = "Arthropoda",
    taxon_class_name = "Insecta", taxon_order_name = "Hymenoptera",
    taxon_superfamily_name = "Apoidea", taxon_family_name = "Megachilidae",
    taxon_subfamily_name = "Megachilinae", taxon_tribe_name = "Osmiini",
    taxon_subtribe_name = NA_character_, taxon_genus_name = "Ashmeadiella",
    taxon_species_name = "Ashmeadiella cactorum",
    taxon_subspecies_name = "Ashmeadiella cactorum basalis",
    subgenus = "Ashmeadiella", complex = NA_character_, complex_taxon_id = NA_integer_,
    rank = "subspecies")
  row <- tidy_holway_ref_row(ranks, scientific_name = "Ashmeadiella cactorum basalis",
                             common_name = NA_character_, source_sheet = "Described")
  expect_equal(row$scientific_name, "Ashmeadiella cactorum ssp. basalis")
  expect_equal(row$species, "cactorum"); expect_equal(row$subspecies, "basalis")
  expect_equal(row$taxon_id, 313836L)
})

test_that("holway_resolution_plan builds the right search per row type", {
  src("reference/holway.R"); src("reference/holway_reference_build.R")
  p_ss <- holway_resolution_plan("Described", "Ashmeadiella", split_holway_species("cactorum basalis"))
  expect_true(p_ss$is_subspecies)
  expect_equal(p_ss$term, "Ashmeadiella cactorum basalis")   # plain trinomial, no ssp.
  p_sp <- holway_resolution_plan("Described", "Stelis", split_holway_species("anthocopae"))
  expect_false(p_sp$is_subspecies)
  expect_equal(p_sp$term, "Stelis anthocopae")
  # non-Described WITH a cleaned epithet now resolves "Genus epithet" (so CF/MSN
  # names reach their real iNat taxon), not just the bare genus.
  p_cf <- holway_resolution_plan("Tentative", "Andrena", split_holway_species("CF annectens"))
  expect_equal(p_cf$term, "Andrena annectens")
  # a bare "sp. nov." leaves no epithet -> falls back to the genus.
  p_g <- holway_resolution_plan("Unpublished", "Ashmeadiella", split_holway_species("sp. nov."))
  expect_equal(p_g$term, "Ashmeadiella")
})

test_that("parse_slash_options splits a name pair into full-name candidates", {
  src("reference/holway.R"); src("reference/holway_reference_build.R")
  expect_equal(parse_slash_options("Bombus", "californicus / fervidus"),
               c("Bombus californicus", "Bombus fervidus"))
  expect_equal(parse_slash_options("Bombus", "pensylvanicus / sonorus"),
               c("Bombus pensylvanicus", "Bombus sonorus"))
})

test_that("resolve_slash_answer maps number / typed name / none", {
  src("reference/holway_reference_build.R")
  opts <- c("Bombus californicus", "Bombus fervidus")
  expect_equal(resolve_slash_answer("1", "Bombus", opts), "Bombus californicus")
  expect_equal(resolve_slash_answer("2", "Bombus", opts), "Bombus fervidus")
  expect_equal(resolve_slash_answer("sonorus", "Bombus", opts), "Bombus sonorus")  # custom
  expect_true(is.na(resolve_slash_answer("none", "Bombus", opts)))
  expect_true(is.na(resolve_slash_answer("", "Bombus", opts)))
})

test_that("unresolved_holway_ref_row builds subspecies and species keep-rows", {
  src("reference/holway.R"); src("reference/holway_reference_build.R")
  ss <- tibble::tibble(source_sheet = "Described", family = "Megachilidae",
                       subfamily = "Megachilinae", tribe = "Osmiini",
                       genus = "Atoposmia", subgenus = "(Hexosmia)",
                       species_raw = "copelandica albomarginata")
  row <- unresolved_holway_ref_row(ss, itis_valid = TRUE, is_subspecies = TRUE)
  expect_equal(row$rank, "subspecies")
  expect_equal(row$species, "copelandica"); expect_equal(row$subspecies, "albomarginata")
  expect_equal(row$scientific_name, "Atoposmia copelandica ssp. albomarginata")
  expect_equal(row$subgenus, "Hexosmia")            # parens stripped
  expect_true(row$itis_valid); expect_true(is.na(row$taxon_id))

  spp <- tibble::tibble(source_sheet = "Described", family = "Megachilidae",
                        subfamily = NA_character_, tribe = NA_character_,
                        genus = "Stelis", subgenus = "", species_raw = "anthocopae")
  row2 <- unresolved_holway_ref_row(spp, itis_valid = TRUE, is_subspecies = FALSE)
  expect_equal(row2$rank, "species"); expect_equal(row2$species, "anthocopae")
  expect_true(is.na(row2$subspecies))
  expect_equal(row2$scientific_name, "Stelis anthocopae")
})

test_that("select_taxon_candidate picks the single result", {
  src("reference/holway_reference_build.R")
  expect_equal(select_taxon_candidate(list(sp("species")), described = TRUE)$action, "pick")
})

test_that("select_taxon_candidate skips empty results", {
  src("reference/holway_reference_build.R")
  expect_equal(select_taxon_candidate(list(), described = TRUE)$action, "skip")
})

test_that("non-Described multi-result auto-skips (genus-level search)", {
  src("reference/holway_reference_build.R")
  res <- select_taxon_candidate(list(sp("species"), sp("genus")), described = FALSE)
  expect_equal(res$action, "skip")
})

test_that("Described with one species hit auto-picks it", {
  src("reference/holway_reference_build.R")
  results <- list(sp("genus"), sp("species"), sp("subgenus"))
  res <- select_taxon_candidate(results, described = TRUE)
  expect_equal(res$action, "pick")
  expect_equal(res$index, 2L)
})

test_that("Described with multiple species hits needs a prompt", {
  src("reference/holway_reference_build.R")
  res <- select_taxon_candidate(list(sp("species"), sp("species")), described = TRUE)
  expect_equal(res$action, "prompt")
})

test_that("holway_search_term: Described unchanged; non-Described resolves the cleaned name", {
  src("reference/holway.R"); src("reference/holway_reference_build.R")
  expect_equal(holway_search_term("Described", "Andrena", "quercina"), "Andrena quercina")
  # non-Described now keys on the CLEANED epithet (was the bare genus before), so
  # already-made Described picks still hit the cache but CF/MSN rows re-resolve.
  expect_equal(holway_search_term("Tentative", "Andrena", "CF annectens"), "Andrena annectens")
  expect_equal(holway_search_term("Unpublished", "Lasioglossum", "MSN pilosifrons"), "Lasioglossum pilosifrons")
  # bare "sp. nov." (nothing left after stripping) -> genus only
  expect_equal(holway_search_term("Unpublished", "Ashmeadiella", "sp. nov."), "Ashmeadiella")
})

test_that("reference rows carry the CF/MSN/aff. qualifier and keep subgenus", {
  src("reference/holway.R"); src("reference/holway_reference_build.R")
  r <- tibble::tibble(source_sheet="Tentative", family="Andrenidae", subfamily="Andreninae",
                      tribe="Andrenini", genus="Andrena", subgenus="(Micandrena)",
                      species_raw="CF annectens")
  row <- unresolved_holway_ref_row(r, itis_valid=NA, is_subspecies=FALSE,
                                   qualifier=holway_qualifier("CF annectens"))
  expect_equal(row$species,   "annectens")   # CLEAN epithet, not "CF annectens"
  expect_equal(row$qualifier, "CF")
  expect_equal(row$subgenus,  "Micandrena")  # parens stripped, kept
  # a RESOLVED tentative row: qualifier passed through; Holway subgenus fills in
  # when iNat's ancestry omits it.
  ranks <- tibble::tibble(taxon_id=573509L,
    taxon_kingdom_name="Animalia", taxon_phylum_name="Arthropoda", taxon_subphylum_name="Hexapoda",
    taxon_class_name="Insecta", taxon_subclass_name="Pterygota",
    taxon_order_name="Hymenoptera", taxon_suborder_name="Apocrita", taxon_infraorder_name="Aculeata",
    taxon_superfamily_name="Apoidea", taxon_family_name="Andrenidae", taxon_epifamily_name="Anthophila",
    taxon_subfamily_name="Andreninae", taxon_tribe_name="Andrenini", taxon_subtribe_name=NA_character_,
    taxon_genus_name="Andrena", taxon_species_name="Andrena annectens", taxon_subspecies_name=NA_character_,
    subgenus=NA_character_, complex=NA_character_, complex_taxon_id=NA_integer_, rank="species")
  row2 <- tidy_holway_ref_row(ranks, scientific_name="Andrena annectens", common_name=NA_character_,
                              source_sheet="Tentative", qualifier="CF", holway_subgenus="(Micandrena)")
  expect_equal(row2$qualifier, "CF")
  expect_equal(row2$subgenus,  "Micandrena")
  expect_equal(row2$taxon_id,  573509L)
})

test_that("retry_empty_search returns initial results untouched when non-empty", {
  src("reference/holway_reference_build.R")
  hit <- list(sp("species"))
  # fetch_fn must never be called if we already have results
  out <- retry_empty_search(hit, "Andrena x",
                            fetch_fn = function(t) stop("should not fetch"),
                            prompt_fn = function(...) stop("should not prompt"))
  expect_identical(out, hit)
})

test_that("retry_empty_search retries with the typed term until a hit", {
  src("reference/holway_reference_build.R")
  # first alternate term still empty, second finds it
  fetch <- function(t) if (identical(t, "Andrena good")) list(sp("species")) else list()
  answers <- c("Andrena bad", "Andrena good")
  i <- 0
  prompt <- function(...) { i <<- i + 1; answers[i] }
  out <- retry_empty_search(list(), "Andrena typo", fetch_fn = fetch, prompt_fn = prompt)
  expect_length(out, 1)
})

test_that("tidy_holway_ref_row reshapes to the clean lookup layout", {
  src("reference/holway_reference_build.R")
  ranks <- tibble::tibble(
    taxon_id = 42L,
    taxon_kingdom_name = "Animalia", taxon_phylum_name = "Arthropoda",
    taxon_subphylum_name = "Hexapoda",
    taxon_class_name = "Insecta", taxon_subclass_name = "Pterygota",
    taxon_order_name = "Hymenoptera", taxon_suborder_name = "Apocrita",
    taxon_infraorder_name = "Aculeata",
    taxon_superfamily_name = "Apoidea", taxon_family_name = "Andrenidae",
    taxon_epifamily_name = "Anthophila",
    taxon_subfamily_name = "Andreninae", taxon_tribe_name = "Andrenini",
    taxon_subtribe_name = NA_character_, taxon_genus_name = "Andrena",
    taxon_species_name = "Andrena annectens", taxon_subspecies_name = NA_character_,
    subgenus = NA_character_, complex = NA_character_, complex_taxon_id = NA_integer_,
    rank = "species")
  row <- tidy_holway_ref_row(ranks, scientific_name = "Andrena annectens",
                             common_name = "A mining bee", source_sheet = "Tentative")
  # metadata columns first, in the lookup's order
  expect_equal(names(row)[1:4], c("taxon_id", "scientific_name", "common_name", "rank"))
  expect_equal(row$scientific_name, "Andrena annectens")  # authoritative iNat name
  expect_equal(row$species, "annectens")                  # bare epithet, not the binomial
  expect_equal(row$genus, "Andrena")
  expect_equal(row$family, "Andrenidae")
  # the 5 sub-ranks flow straight through from the iNat ancestry
  expect_equal(row$suborder, "Apocrita")
  expect_equal(row$infraorder, "Aculeata")
  expect_equal(row$epifamily, "Anthophila")
  expect_true(row$resolved)
})

test_that("retry fires when the only hit is a complex, but NOT for a genus-only result", {
  src("reference/holway_reference_build.R")
  # only a complex came back -> ask for an alternate name; the typed name finds the species
  fetch <- function(t) if (identical(t, "Andrena realname")) list(mk(2, "Andrena realname", "species"))
                       else list(mk(1, "Andrena osmioides", "complex"))
  got <- retry_empty_search(list(mk(1, "Andrena osmioides", "complex")), "Andrena osmioides",
                            fetch_fn = fetch, prompt_fn = function(...) "Andrena realname")
  expect_true(any(vapply(got, function(t) identical(t$rank, "species"), logical(1))))

  # a genus-only result is the expected bare "sp. nov." fallback -> no retry
  hit_g <- list(mk(3, "Andrena", "genus"))
  got2 <- retry_empty_search(hit_g, "Andrena",
                             fetch_fn = function(t) stop("should not fetch"),
                             prompt_fn = function(...) stop("should not prompt"))
  expect_identical(got2, hit_g)

  # a species result short-circuits (no retry) -- unchanged
  expect_false(.needs_alt_search(list(mk(1, "Andrena x", "species"))))
  expect_true(.needs_alt_search(list()))
  expect_true(.needs_alt_search(list(mk(1, "Andrena x", "complex"))))
})

test_that("retry_empty_search stops on 'skip' and when non-interactive", {
  src("reference/holway_reference_build.R")
  # user skips -> stays empty
  out1 <- retry_empty_search(list(), "Andrena typo",
                             fetch_fn = function(t) list(sp("species")),
                             prompt_fn = function(...) "skip")
  expect_length(out1, 0)
  # non-interactive -> never prompts, stays empty
  out2 <- retry_empty_search(list(), "Andrena typo",
                             fetch_fn = function(t) stop("should not fetch"),
                             prompt_fn = function(...) stop("should not prompt"),
                             interactive_ok = FALSE)
  expect_length(out2, 0)
})

# The banner's option table now offers the WORDS "none" and "skip" rather than the
# keystrokes "n" and <Enter>. The resolver has always accepted both words, but
# nothing held it to that -- so a later tightening of this parser could silently
# break a promise the printed table makes. This is that lock.
test_that("'none' and 'skip' are accepted, not just 'n' and Enter", {
  src("reference/holway_reference_build.R")
  r <- data.frame(source_sheet = "Described", genus = "Stelis",
                  species_raw = "anthocopae", subgenus = NA_character_,
                  stringsAsFactors = FALSE)

  # skip returns before the decision cache is touched, so no store is needed
  for (word in c("skip", "s", ""))
    expect_null(suppressMessages(
      .second_pass_resolve(NULL, r, prompt_fn = function(p) word)), info = word)

  if (!requireNamespace("duckdb", quietly = TRUE)) skip("duckdb not installed")
  src("config.R"); src("inat_observations/engine/db/store_conn.R")
  src("inat_observations/engine/db/decision_store.R")
  con <- store_connect(tempfile(fileext = ".duckdb"))
  on.exit(store_disconnect(con), add = TRUE)
  for (word in c("none", "n"))
    expect_identical(suppressMessages(
      .second_pass_resolve(con, r, prompt_fn = function(p) word)), "no_inat_id",
      info = word)
})

# Both give-up parsers compared the raw string against lowercase words, while every
# other answer parser in the file lowercases first (.yn(), .second_pass_resolve()).
# So "None" or "Skip" was not an exit: in retry_empty_search it became a name to
# search -- a live API call, then the identical question again, forever -- and in
# resolve_slash_answer it fell through to paste(genus, raw) and went looking for
# "Bombus None". Now that the printed option tables offer whole words, this gets
# MORE likely, not less.
test_that("give-up words are recognized whatever the capitalization", {
  src("reference/holway_reference_build.R")
  for (word in c("none", "None", "NONE", "skip", "Skip", "SKIP", "")) {
    calls <- 0L
    suppressMessages(retry_empty_search(
      list(), "Stelis anthocopae",
      fetch_fn = function(x) {          # cap the loop so a regression fails, not hangs
        calls <<- calls + 1L
        if (calls > 2L) list(list(id = 1, name = "x", rank = "species")) else list()
      },
      prompt_fn = function(p) word))
    expect_equal(calls, 0L, info = word)      # a give-up costs no API call
  }
  for (word in c("none", "None", "NONE", "skip", "Skip", ""))
    expect_true(is.na(resolve_slash_answer(word, "Bombus", list("a", "b"))), info = word)
})
