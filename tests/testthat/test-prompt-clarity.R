# Taro's feedback on the cleaning prompts, verbatim: "what is this asking me to
# do? look it up in iNat? how do I know what the number is? what do you mean
# validate it in ITIS if it has a taxa name change."
#
# Every one of those is answerable, and the answers were in dev-docs. But the
# person hitting these is at a terminal at 9pm, not reading dev-docs. A prompt
# that needs a second document open is a broken prompt.
#
# So each one must carry, in the terminal, at the moment it asks:
#   * a link to click,
#   * what the answer keys mean,
#   * what the question is actually deciding.
src("reference/taxonomy/holway_reference_build.R")
src("reference/prompts/specimen_id_prompt.R")
src("reference/prompts/verify_prompt.R")

.said <- function(expr) {
  out <- character(0)
  withCallingHandlers(expr, message = function(m) {
    out <<- c(out, conditionMessage(m)); invokeRestart("muffleMessage")
  })
  paste(out, collapse = "")
}

test_that("the ITIS question gives a link and says what y and n do", {
  txt <- .said(itis_disposition("Lasioglossum turgiventre",
                                prompt_fn = function(p) { .p <<- p; "n" }))
  expect_match(txt, "itis.gov", fixed = TRUE)
  expect_match(txt, "Lasioglossum+turgiventre", fixed = TRUE)  # the search is pre-filled
  expect_match(txt, "valid", ignore.case = TRUE)               # what to look for
  expect_match(paste(txt, .p), "y", fixed = TRUE)
  expect_match(paste(txt, .p), "n", fixed = TRUE)
})

test_that("the ITIS question explains what ITIS is", {
  txt <- .said(itis_disposition("Andrena chalybaea", prompt_fn = function(p) "n"))
  expect_match(txt, "taxonom", ignore.case = TRUE)   # names it as a taxonomy database
})

test_that("the taxon_id question shows where to find the number", {
  add <- data.frame(scientific_name = "Andrena chalybaea", rank = "species",
                    genus = "Andrena", species = "chalybaea",
                    taxon_id = NA_integer_, stringsAsFactors = FALSE)
  txt <- .said(resolve_specimen_additions_interactive(
    add, fetch_fn = function(nm) list(), prompt_fn = function(p) { .q <<- p; "s" }))
  both <- paste(txt, .q)
  expect_match(both, "inaturalist.org", fixed = TRUE)
  expect_match(both, "Andrena+chalybaea", fixed = TRUE)   # the search is pre-filled
  expect_match(both, "taxon_id", fixed = TRUE)
})

test_that("the verify pass opens by saying what it is asking", {
  needs <- data.frame(taxon_id = 345235L, scientific_name = "Colletes hyalinus",
                      rank = "subspecies", verified = FALSE, stringsAsFactors = FALSE)
  txt <- .said(resolve_verification_interactive(needs, prompt_fn = function(p) ""))
  expect_match(txt, "San Diego", fixed = TRUE)
  # No pointer to a document. VERIFICATION.md was deleted because backing out of a
  # prompt to read a file, then coming back, is how people start guessing instead.
  # Everything needed to answer is printed here.
  expect_false(grepl("VERIFICATION.md", txt, fixed = TRUE))
})

# There are two prompt passes and they ask different questions. Told apart, they
# are obvious; run back to back with no labels, they look like the same question
# asked twice, and the reader cannot tell why they are being asked again.
#   pass 1  which iNaturalist taxon IS this bee?      -> gets a taxon_id
#   pass 2  is that taxon really found in San Diego?  -> accepts or rejects it
test_that("pass 1 says it is pass 1, and what pass 2 will do", {
  add <- data.frame(scientific_name = "Andrena chalybaea", rank = "species",
                    genus = "Andrena", species = "chalybaea",
                    taxon_id = NA_integer_, stringsAsFactors = FALSE)
  txt <- .said(resolve_specimen_additions_interactive(
    add, fetch_fn = function(nm) list(), prompt_fn = function(p) "s"))
  expect_match(txt, "PASS 1", fixed = TRUE)
  expect_match(txt, "PASS 2", fixed = TRUE)   # says what comes next
})

test_that("pass 2 says it is pass 2, and what pass 1 already did", {
  needs <- data.frame(taxon_id = 345235L, scientific_name = "Colletes hyalinus",
                      rank = "subspecies", verified = FALSE, stringsAsFactors = FALSE)
  txt <- .said(resolve_verification_interactive(needs, prompt_fn = function(p) ""))
  expect_match(txt, "PASS 2", fixed = TRUE)
  expect_match(txt, "PASS 1", fixed = TRUE)
})

test_that("the ITIS question is not mislabelled as one of the two passes", {
  txt <- .said(itis_disposition("Andrena chalybaea", prompt_fn = function(p) "n"))
  expect_false(grepl("PASS 1|PASS 2", txt))
})

# resolve_missing_ids.R already searched iNaturalist for every checklist bee it
# could not resolve and cached the verdict in resolved_missing_ids.csv
# ("not_found_or_ambiguous"). PASS 1 did not read that file, so it asked about
# those bees on every run -- Lasioglossum turgiventre among them -- and the only
# correct answer was always `s`. Asking a question you have already answered is
# how a person learns to stop reading the prompts.
test_that("PASS 1 does not ask about taxa the resolver already ruled out", {
  cache <- tempfile(fileext = ".csv")
  write.csv(data.frame(key = "species|lasioglossum turgiventre|57678",
                       taxon_id = NA, status = "not_found_or_ambiguous"),
            cache, row.names = FALSE)
  on.exit(unlink(cache), add = TRUE)

  add <- data.frame(scientific_name = "Lasioglossum turgiventre", rank = "species",
                    genus = "Lasioglossum", species = "turgiventre",
                    taxon_id = NA_integer_, stringsAsFactors = FALSE)
  asked <- 0L
  txt <- .said(resolve_specimen_additions_interactive(
    add, fetch_fn = function(nm) list(), known_missing_path = cache,
    prompt_fn = function(p) { asked <<- asked + 1L; "s" }))
  expect_equal(asked, 0L)
  expect_match(txt, "already", ignore.case = TRUE)   # says why it was skipped
})

test_that("PASS 1 still asks about a taxon the resolver has not ruled out", {
  cache <- tempfile(fileext = ".csv")
  write.csv(data.frame(key = "species|something else|1", taxon_id = NA,
                       status = "not_found_or_ambiguous"), cache, row.names = FALSE)
  on.exit(unlink(cache), add = TRUE)
  add <- data.frame(scientific_name = "Andrena chalybaea", rank = "species",
                    genus = "Andrena", species = "chalybaea",
                    taxon_id = NA_integer_, stringsAsFactors = FALSE)
  asked <- 0L
  resolve_specimen_additions_interactive(add, fetch_fn = function(nm) list(),
    known_missing_path = cache, prompt_fn = function(p) { asked <<- asked + 1L; "s" })
  expect_equal(asked, 1L)
})

# The prompt hands the reviewer a pre-filtered iNaturalist link so they do not have
# to build the query. It was built with `verifiable=any`, which INCLUDES Casual --
# drawer photos, no date, captive animals. VERIFICATION.md warned against exactly
# that filter while the prompt handed it over. The link is the thing people click,
# so the link has to be right.
test_that("the San Diego link excludes Casual records", {
  needs <- data.frame(taxon_id = 345235L, scientific_name = "Colletes hyalinus",
                      rank = "subspecies", verified = FALSE, stringsAsFactors = FALSE)
  txt <- .said(resolve_verification_interactive(needs, prompt_fn = function(p) ""))
  expect_match(txt, "verifiable=true", fixed = TRUE)
  expect_false(grepl("verifiable=any", txt, fixed = TRUE))
})

# Everything needed to answer has to be ON the prompt. Backing out to a document,
# finding the right section and coming back is how people start guessing instead.
test_that("the prompt states the rule for deciding, not just the keys", {
  needs <- data.frame(taxon_id = 345235L, scientific_name = "Colletes hyalinus",
                      rank = "subspecies", verified = FALSE, stringsAsFactors = FALSE)
  txt <- .said(resolve_verification_interactive(needs, prompt_fn = function(p) ""))
  expect_match(txt, "1 or more", fixed = TRUE)      # what counts as yes
  expect_match(txt, "none", fixed = TRUE)           # what counts as no
  expect_match(txt, "Casual", fixed = TRUE)         # and why the link is filtered
})

# VERIFICATION.md is being deleted: everything in it belongs at the prompt, where the
# person is. These two facts were only in the doc -- that an answer is remembered, and
# where it is written. Both change how someone answers: not knowing an answer sticks
# makes people hesitate over a decision they can revisit.
test_that("PASS 2 says answers are saved and not asked again", {
  needs <- data.frame(taxon_id = 345235L, scientific_name = "Colletes hyalinus",
                      rank = "subspecies", verified = FALSE, stringsAsFactors = FALSE)
  txt <- .said(resolve_verification_interactive(needs, prompt_fn = function(p) ""))
  expect_match(txt, "verified_taxa.csv", fixed = TRUE)
  expect_match(txt, "not asked again", ignore.case = TRUE)
})

# The crosswalk reviewer lists its keys but never says what a tag or a field IS,
# what happens to the observation either way, or where to look an unknown one up.
# PIPELINE_GUIDE tells you to "look the field up on iNat" and the prompt hands you
# no link. Same standard as the taxon prompts: say what is happening, what the
# answer decides, and give the address.
src("project_info/review/qc_review_mastercrosswalk.R")

# this reviewer writes with cat(), so capture stdout rather than messages
.printed <- function(expr) paste(capture.output(expr), collapse = "\n")

test_that("the crosswalk banner explains what is being decided", {
  txt <- .printed(.rk_help("fields"))
  expect_match(txt, "inaturalist.org", fixed = TRUE)     # where to look one up
  expect_match(txt, "kept", ignore.case = TRUE)          # what happens to the observation
  expect_match(txt, "dropped|drops|removed", perl = TRUE)
})

test_that("it distinguishes ignoring a field from excluding an observation", {
  txt <- .printed(.rk_help("fields"))
  expect_match(txt, "the observation is kept", ignore.case = TRUE)
})

# Taro's second sticking point, verbatim: "this doesn't tell me what I'm reviewing,
# why, what to look up" -- and iNaturalist and ITIS are both used heavily here with
# neither explained. The prompt said only:
#
#   [2nd pass] Unresolved: 'Stelis anthocopae'  subgenus (Stelis)
#     taxon_id, a name to search, 'n' = no iNat id yet, or blank to skip:
#
# Nothing about what the step is doing, why this name failed, which two sites
# answer it, or what a taxon_id looks like when you find one.
test_that("the Holway second pass says what it is doing and where to look", {
  r <- list(source_sheet = "Described", genus = "Stelis", species_raw = "anthocopae",
            subgenus = "(Stelis)")
  txt <- paste(.said(.second_pass_banner(1L)), .said(.second_pass_item(r, 1L, 1L)))
  expect_match(txt, "checklist", ignore.case = TRUE)          # what the step is
  expect_match(txt, "inaturalist.org/search", fixed = TRUE)   # where to look, 1
  expect_match(txt, "itis.gov", fixed = TRUE)                 # where to look, 2
  expect_match(txt, "Stelis+anthocopae", fixed = TRUE)        # both pre-filled
  expect_match(txt, "renam", ignore.case = TRUE)              # why it can fail
})

test_that("it explains what a taxon_id looks like", {
  r <- list(source_sheet = "Described", genus = "Stelis", species_raw = "anthocopae",
            subgenus = NA_character_)
  txt <- .said(.second_pass_banner(1L))
  expect_match(txt, "taxa/", fixed = TRUE)   # the number in an iNaturalist URL
})

# "Fill missing taxon_ids: 17 bee names need an iNat taxon_id" -- the same defect as
# PASS 1 had. Those 17 are the names resolve_missing_ids.R ALREADY searched for and
# could not resolve: CLAUDE.md documents them as bees iNaturalist has never published.
# The banner presented them as work, when for most the correct answer is 'n' and the
# only real question is whether the NAME is valid, which is an ITIS question.
src("reference/prompts/manual_overrides.R")

test_that("the fill-missing banner says these were already searched for", {
  txt <- .said(.mo_banner(17))
  expect_match(txt, "already", ignore.case = TRUE)
  expect_match(txt, "itis.gov", fixed = TRUE)      # where to check the name is real
  expect_match(txt, "n", fixed = TRUE)             # the expected answer
})

# The review gate is shared by the specimen and iNaturalist checkpoints, and both
# read badly for the same reasons: it printed a folder rather than a path you can
# open, said "the raw .xlsx" when there are nineteen of them, and labelled two rows
# "bee behavior to fix" and "bee flowers to add" when both mean one thing -- the
# flower was never recorded on the observation.
src("specimens/specimen_clean_helpers.R")

test_that("the gate prints a path you can open, not just a folder", {
  it <- data.frame(label = "duplicate IDs", count = 2L,
                   file = "qc_review_specimen_duplicates_generated.csv",
                   stringsAsFactors = FALSE)
  txt <- .said(resolve_review_gate(it, "data/specimens/specimens_clean/review",
                                   interactive_ok = FALSE))
  expect_match(txt, "data/specimens/specimens_clean/review/qc_review_specimen_duplicates_generated.csv",
               fixed = TRUE)
})

test_that("each row can carry a plain-language explanation", {
  it <- data.frame(label = "duplicate IDs", count = 2L, file = "d.csv",
                   what = "two specimens share one museum number", stringsAsFactors = FALSE)
  txt <- .said(resolve_review_gate(it, "review", interactive_ok = FALSE))
  expect_match(txt, "two specimens share one museum number", fixed = TRUE)
})

test_that("the iNat flower rows say what is actually missing", {
  labs <- .review_labels_inat()
  expect_false(any(grepl("behavior", labs, ignore.case = TRUE)))
  expect_true(all(grepl("flower", labs, ignore.case = TRUE)))
})

# "Review queue  0 unknown tags · 0 fields · 8 windows" -- a count with no path and
# no word about what a "window" is or what ruling on one means. Taro: "this is vague
# and doesn't show pathway to go look for review files."
src("project_info/finding_project_info.R")

test_that("the review-queue summary names the file for anything outstanding", {
  txt <- .said(.fpi_review_summary(tags = 0L, fields = 0L, windows = 8L))
  expect_match(txt, "qc_review_survey_beeple_date_windows_generated.csv", fixed = TRUE)
  expect_match(txt, "data/project_info/surveys/review", fixed = TRUE)
  expect_match(txt, "survey day", ignore.case = TRUE)   # what a window IS
})

test_that("nothing outstanding prints no paths to chase", {
  txt <- .said(.fpi_review_summary(tags = 0L, fields = 0L, windows = 0L))
  expect_false(grepl("qc_review", txt, fixed = TRUE))
  expect_match(txt, "nothing", ignore.case = TRUE)
})

# Naming "an observation field" is not actionable -- iNaturalist has thousands.
# The crosswalk knows exactly which two this project reads, so the message names
# them, spelled as they appear on the site.
test_that("the flower explanation names the actual iNaturalist fields to add", {
  w <- .review_what_inat()
  expect_match(w[1], "Insect on flower", fixed = TRUE)
  expect_match(w[1], "Interaction", fixed = TRUE)
  expect_match(w[1], "Visited flower of", fixed = TRUE)
})

test_that("it says what to do when the bee was not on a flower", {
  w <- .review_what_inat()
  expect_match(w[1], "No flower is a real answer", fixed = TRUE)
})

# The correction example read as a real name -- "Stelis foo" looks like something
# you could type -- rather than as a slot to fill. Then "Genus <corrected>" had the
# opposite problem: it read as "type a genus", when the answer is usually a species
# or a subspecies. Name the ranks that are allowed instead of showing a template.
test_that("the name option is not a fill-in-the-blank template", {
  txt <- .said(.second_pass_banner(1L))
  expect_false(grepl("Stelis foo", txt, fixed = TRUE))
  expect_false(grepl("<corrected", txt, fixed = TRUE))
})

test_that("the name option says which ranks you can type", {
  txt <- .said(.second_pass_banner(1L))
  for (rank in c("cientific", "genus", "subgenus", "complex", "species", "subspecies"))
    expect_match(txt, rank, fixed = TRUE, info = rank)
})

# "the single letter n" and "<Enter>" are both harder to type with confidence than
# the word for what they do -- and the resolver has always accepted both words.
# The table simply never said so.
# Answering "no iNaturalist page" used to be permanent: the row was cached and never
# asked about again. But iNaturalist DOES add bees -- that is the whole reason to look
# again -- and the automatic search caches its own not-found verdict too, so nothing in
# the pipeline would ever notice. Brandi's call: ask every run. The answer is still
# recorded (it marks the name as a real bee), it just stops suppressing the question.
test_that("a bee answered 'no iNaturalist page' is asked about again", {
  r <- list(source_sheet = "Described", species_raw = "anthocopae")
  row <- list(taxon_id = NA_integer_)
  expect_true(.second_pass_asks(r$source_sheet, r$species_raw, row))
})

test_that("a bee that HAS an id is not asked about", {
  expect_false(.second_pass_asks("Described", "anthocopae", list(taxon_id = 12345L)))
  expect_false(.second_pass_asks("Described", "anthocopae", NULL))
})

test_that("slash rows and other sheets are still left to the first pass", {
  row <- list(taxon_id = NA_integer_)
  expect_false(.second_pass_asks("Described", "anthocopae / copelandica", row))
  expect_false(.second_pass_asks("Undescribed", "sp. nov. 1", row))
})

test_that("a repeat question says what you answered last time, and when", {
  r <- list(genus = "Hesperapis", species_raw = "ilicifoliae", subgenus = NA_character_)
  txt <- .said(.second_pass_item(r, 1L, 1L,
                                 prior = list(action = "no_inat_id",
                                              decided_at = "2026-03-14 09:12:00")))
  expect_match(txt, "2026-03-14", fixed = TRUE)
  expect_match(txt, "no iNaturalist page", ignore.case = TRUE)
})

test_that("a first-time question says nothing about a previous answer", {
  r <- list(genus = "Hesperapis", species_raw = "ilicifoliae", subgenus = NA_character_)
  txt <- .said(.second_pass_item(r, 1L, 1L))
  expect_false(grepl("last", txt, ignore.case = TRUE))
})

test_that("the none option no longer promises it will stop asking", {
  txt <- gsub("[[:space:]]+", " ", .said(.second_pass_banner(1L)))
  expect_false(grepl("not be asked again", txt, fixed = TRUE))
  expect_match(txt, "both come back next run", fixed = TRUE)   # it says the opposite
})

test_that("the table offers the words, not the keystrokes", {
  lines <- strsplit(.said(.second_pass_banner(1L)), "\n", fixed = TRUE)[[1]]
  types <- trimws(substr(lines, 1L, 22L))    # the TYPE column, whatever fills it
  expect_true("none" %in% types)
  expect_true("skip" %in% types)
  expect_false(grepl("the single letter n", paste(lines, collapse = " "), fixed = TRUE))
})

# CLAUDE.md's rule for exactly this decision: "a wrong id is worse than none".
# Someone who does not know the taxon should be told that skipping is the SAFE
# answer, not left to guess because guessing feels more helpful than pressing Enter.
test_that("skipping is recommended when unsure, and says why", {
  txt <- .said(.second_pass_banner(1L))
  expect_match(txt, "not sure", ignore.case = TRUE)
  expect_match(txt, "worse than none|wrong id", perl = TRUE)
})

# The banner repeated in full for every unresolved name -- six names, six copies of
# the same twenty lines -- while the other taxon prompt prints it once and then a
# compact "[1/17] name" card. One long explanation, then short cards, is right.
test_that("the second-pass banner is separate from the per-name card", {
  b <- .said(.second_pass_banner(6L))
  expect_match(b, "WHAT THIS STEP IS DOING", fixed = TRUE)
  expect_match(b, "6", fixed = TRUE)                 # how many are coming

  r <- list(genus = "Atoposmia", species_raw = "copelandica arefacta",
            subgenus = "(Hexosmia)")
  card <- .said(.second_pass_item(r, 2L, 6L))
  expect_match(card, "[2/6]", fixed = TRUE)          # position in the queue
  expect_match(card, "Atoposmia+copelandica+arefacta", fixed = TRUE)   # both links
  expect_match(card, "itis.gov", fixed = TRUE)
  expect_false(grepl("WHAT THIS STEP IS DOING", card, fixed = TRUE))   # not repeated
})

# "the county checklist" is not a document anyone can go and open. It is Holway's
# San Diego County bee checklist, v3, published by UC San Diego -- so the banner
# names it and gives the link, the same way it links iNaturalist and ITIS.
test_that("the second pass names the checklist and links it", {
  txt <- .said(.second_pass_banner(6L))
  expect_match(txt, "Holway", fixed = TRUE)
  expect_match(txt, "v3", ignore.case = TRUE)
  expect_match(txt, "https://library.ucsd.edu/dc/collection/bb7305352v", fixed = TRUE)
})

# The step description said "iNaturalist's number for it" and the rest of the
# prompt says "taxon_id" -- two names for one thing, and nothing tying them
# together. Say the word where the thing is first introduced.
# Two listed reasons, but the common third one was missing: the taxonomy itself
# changed. A bee split into two, lumped into another, or moved to a different
# genus is not a respelling and not an absent page -- it is a real revision, and
# it is the case where the ITIS link actually earns its place.
test_that("a taxonomy change is listed as a reason a name fails", {
  txt <- .said(.second_pass_banner(1L))
  expect_match(txt, "taxonomy changed", ignore.case = TRUE)
  expect_match(txt, "split|moved", perl = TRUE)
  expect_false(grepl("one of two things", txt, fixed = TRUE))   # there are three
})

test_that("the step description names the number as the taxon_id", {
  lines <- strsplit(.said(.second_pass_banner(1L)), "\n", fixed = TRUE)[[1]]
  i <- grep("WHAT THIS STEP IS DOING", lines, fixed = TRUE)
  expect_length(i, 1L)
  para <- lines[i:(i + 4L)]                        # the heading and its sentence
  expect_match(paste(para, collapse = " "), "taxon_id", fixed = TRUE)
})

# The specimen gate names a path but not what any row MEANS or how to fix it, and
# "the raw .xlsx" is one of nineteen versioned workbooks. Taro: "this needs more
# explanation on how to fix it."
test_that("each specimen review row explains itself", {
  w <- .review_what_specimens()
  expect_equal(length(w), 4L)
  expect_match(w[2], "same", ignore.case = TRUE)      # duplicate IDs: two rows share a number
  expect_true(all(nchar(w) > 60))                     # each says something
})

test_that("the specimen fix hint names the newest workbook, not 'the raw .xlsx'", {
  h <- .specimen_fix_hint(c("cabr_bee_specimens_record_V18_2026_08_18.xlsx",
                            "cabr_bee_specimens_record_V19_2026_09_02.xlsx"))
  expect_match(h, "V19", fixed = TRUE)
  expect_false(grepl("V18", h, fixed = TRUE))
  expect_match(h, "new version", ignore.case = TRUE)  # never edit an old one
})

# Two rows sharing one fix printed the whole fix twice -- a wall of text where one
# short block would do. Identical explanations collapse to one.
test_that("a shared explanation prints once, not per row", {
  it <- data.frame(label = c("a", "b"), count = c(4L, 20L), file = c("a.csv", "b.csv"),
                   what = c("SAME EXPLANATION", "SAME EXPLANATION"), stringsAsFactors = FALSE)
  txt <- .said(resolve_review_gate(it, "review", interactive_ok = FALSE))
  expect_equal(lengths(regmatches(txt, gregexpr("SAME EXPLANATION", txt)))[[1]], 1L)
  expect_match(txt, "review/a.csv", fixed = TRUE)   # both paths still shown
  expect_match(txt, "review/b.csv", fixed = TRUE)
})

test_that("different explanations still each appear", {
  it <- data.frame(label = c("a", "b"), count = c(1L, 1L), file = c("a.csv", "b.csv"),
                   what = c("FIRST THING", "SECOND THING"), stringsAsFactors = FALSE)
  txt <- .said(resolve_review_gate(it, "review", interactive_ok = FALSE))
  expect_match(txt, "FIRST THING", fixed = TRUE)
  expect_match(txt, "SECOND THING", fixed = TRUE)
})

# The rest of holway_reference_build.R. The thread through every one of these is the
# same: the file asks the operator to IDENTIFY a bee and repeatedly gives them nothing
# to identify it with -- bare iNaturalist numbers with no link, two names with no
# indication where either came from, and "skipped." with no word on whether the bee
# comes back.

test_that("the multi-match list gives a link for every candidate", {
  txt <- paste(.hrb_candidates(list(
    list(id = 62881, name = "Andrena quercina", rank = "species"),
    list(id = 199123, name = "Andrena quercinella", rank = "subspecies"))), collapse = "\n")
  expect_match(txt, "https://www.inaturalist.org/taxa/62881", fixed = TRUE)
  expect_match(txt, "https://www.inaturalist.org/taxa/199123", fixed = TRUE)
  expect_false(grepl("id=", txt, fixed = TRUE))      # never named, never explained
})

test_that("the multi-match prompt says why there is more than one", {
  txt <- paste(.hrb_multi_lead("Andrena quercina", 2L), collapse = " ")
  expect_match(txt, "more than one", ignore.case = TRUE)
  expect_match(txt, "same name", ignore.case = TRUE)
})

test_that("the slash prompt says where the two names came from", {
  txt <- paste(.hrb_slash_lead("Andrena", c("Andrena a", "Andrena b")), collapse = " ")
  expect_match(txt, "checklist", ignore.case = TRUE)
  expect_match(txt, "two names", ignore.case = TRUE)
  expect_match(txt, "which one", ignore.case = TRUE)
})

test_that("the slash options carry a search link each", {
  txt <- paste(.hrb_slash_lead("Andrena", c("Andrena a", "Andrena b")), collapse = "\n")
  expect_match(txt, "inaturalist.org/search?q=Andrena+a", fixed = TRUE)
})

test_that("the subspecies question says what each answer does", {
  txt <- paste(.hrb_subspecies_lead("Atoposmia", "copelandica", "arefacta"), collapse = " ")
  expect_match(txt, "three words", ignore.case = TRUE)
  expect_match(txt, "inaturalist.org/search", fixed = TRUE)
  expect_match(txt, "no id", ignore.case = TRUE)      # what answering no costs
})

test_that("'only a complex' is explained where it is used", {
  txt <- .hrb_alt_lead(0L, "Stelis anthocopae")
  expect_match(txt, "nothing", ignore.case = TRUE)
  txt2 <- .hrb_alt_lead(2L, "Stelis anthocopae")
  expect_match(txt2, "look-alike", ignore.case = TRUE)
  expect_false(grepl("complex (no species/subspecies)", txt2, fixed = TRUE))
})

test_that("the ITIS opener does not name two passes nobody has heard of", {
  txt <- paste(.hrb_itis_lead("Lasioglossum turgiventre"), collapse = " ")
  expect_false(grepl("taxon_id passes", txt, fixed = TRUE))
  expect_match(txt, "checklist", ignore.case = TRUE)
})

test_that("the ITIS question offers whole words, which .yn has always accepted", {
  expect_match(.hrb_itis_ask(), "yes", fixed = TRUE)
  expect_match(.hrb_itis_ask(), "no", fixed = TRUE)
})

test_that("a skip says whether the bee comes back", {
  expect_match(.hrb_skipped("no matches"), "asked again", fixed = TRUE)
  expect_match(.hrb_skipped("no matches"), "no matches", fixed = TRUE)
})

test_that("the finished-table line says where the file is", {
  expect_match(.hrb_wrote(1035L, "data/reference/holway_sd_bee_reference_table_v3_generated.csv"),
               "data/reference/holway_sd_bee_reference_table_v3_generated.csv", fixed = TRUE)
})

# The banner promises "asked again next run, in case one gets added", and then typing
# `none` printed "won't prompt again until it's cleared." Flat contradiction, in the
# same screen. The banner is right: .second_pass_asks() never consults the decision,
# so the question comes back regardless. The confirmation line was left over from when
# a `none` answer did suppress it, and "cleared" was never defined anywhere.
test_that("confirming 'none' does not contradict the banner", {
  txt <- .hrb_no_page_note()
  expect_false(grepl("won't prompt again", txt, fixed = TRUE))
  expect_false(grepl("cleared", txt, fixed = TRUE))
  expect_match(txt, "asked again", fixed = TRUE)
})

test_that("it says what recording the answer is actually for", {
  txt <- .hrb_no_page_note()
  expect_match(txt, "real bee", ignore.case = TRUE)
})

test_that("the banner says plainly that neither none nor skip stops the question", {
  txt <- gsub("[[:space:]]+", " ", .said(.second_pass_banner(6L)))
  expect_match(txt, "Neither none nor skip stops the question", fixed = TRUE)
})

# The review gate printed its explanation and its fix instruction as single
# unwrapped lines, so both ran off the right edge of the terminal and the operator
# could not read the end of either. It also ended "never edit an old one.." -- the
# hint already ends in a full stop and the caller added another.
test_that("nothing the review gate prints runs off the screen", {
  it <- data.frame(label = "duplicate IDs", count = 2L,
                   file = "qc_review_specimen_duplicates_generated.csv",
                   what = paste("Two rows carry the same museum number, so one specimen's records",
                                "would be credited to the other. Give one of them its own number,",
                                "or delete the row if it is a duplicate entry rather than a",
                                "duplicate specimen."),
                   stringsAsFactors = FALSE)
  said <- character(0)
  withCallingHandlers(
    resolve_review_gate(it, "data/specimens/specimens_clean/review", interactive_ok = FALSE,
                        fix_hint = .specimen_fix_hint("cabr_bee_specimens_record_V19_2026_09_02.xlsx")),
    message = function(m) { said <<- c(said, conditionMessage(m)); invokeRestart("muffleMessage") })
  lines <- unlist(strsplit(paste(said, collapse = ""), "\n", fixed = TRUE))
  # a path or url is one unbroken token: wrapping it would break copy-paste, so it is
  # allowed to run long. Everything with a space in it has to fit.
  wrappable <- lines[grepl(" ", trimws(lines))]
  expect_equal(wrappable[nchar(wrappable) > 88], character(0))
})

test_that("the fix instruction wraps too, and does not double its full stop", {
  lines <- .review_fix_lines(.specimen_fix_hint("cabr_bee_specimens_record_V19_2026_09_02.xlsx"))
  expect_equal(lines[nchar(lines) > 88], character(0))
  expect_false(any(grepl("..", lines, fixed = TRUE)))
  expect_match(paste(lines, collapse = " "), "V19_2026_09_02.xlsx", fixed = TRUE)
})
