# =============================================================
# reference/taxonomy/holway_reference_build.R
# beescabr pipeline -- interactive Holway -> iNat taxon resolver
# Created: 2026-07-13 (ported from the Python process_holway_data)
#
# Resolves each row of Holway's v3 combined sheet to an iNat taxon and walks
# its ancestry to kingdom, producing a taxonomy checklist. This is the ONE
# place manual intervention is expected: when an iNat name search is
# ambiguous, a human picks the right taxon (or skips). Every decision is
# persisted in the DuckDB holway_decisions table (db/decision_store.R) so
# reruns are reproducible and never re-prompt an already-decided term.
#
# The candidate-selection RULE is a pure function (select_taxon_candidate)
# so the auto-pick / auto-skip / needs-prompt branching is unit-tested;
# the readline() prompting is the only impure part.
#
# Run: source("scripts/reference/taxonomy/holway_reference_build.R")
#   Set BEESCABR_NONINTERACTIVE=1 to auto-skip anything that would prompt
#   (for unattended reruns -- ambiguous terms are left for a human pass).
#
# Depends on: config.R, db/*, api/*.
# =============================================================

library(dplyr)
library(jsonlite)
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

if (sys.nframe() == 0L || !exists("select_taxon_candidate")) {
  # allow sourcing just the helpers for tests without pulling the world
}

# ------------------------------------------------------------
# select_taxon_candidate(): PURE decision rule for a name-search result set.
# Returns list(action, index):
#   action "pick"   -> use results[[index]]
#   action "skip"   -> no usable taxon (auto)
#   action "prompt" -> ambiguous, a human must choose
# Mirrors the Python auto-pick logic: single result -> pick; multiple ->
# for non-Described rows (genus-level searches) auto-skip; for Described
# rows pick the sole species-rank hit, or prompt if several species tie.
# ------------------------------------------------------------
# .norm_name(): normalize a taxon name for exact matching -- lowercased, with
# the subspecies markers ("ssp.", "subsp.", "var.", "f.") removed. iNat's API
# name field is the bare trinomial ("Ashmeadiella cactorum basalis") while we
# search WITH "ssp." ("Ashmeadiella cactorum ssp. basalis"); normalizing both
# lets them match. PURE.
.norm_name <- function(x) {
  x <- tolower(x %||% "")
  x <- gsub("\\b(ssp|subsp|var|f)\\.?(\\s|$)", " ", x)
  trimws(gsub("\\s+", " ", x))
}

select_taxon_candidate <- function(results, described, search_term = NULL,
                                   is_subspecies = FALSE) {
  n <- length(results)
  if (n == 0) return(list(action = "skip", index = NA_integer_))

  rnk <- vapply(results, function(t) t$rank %||% "", character(1))
  # NEVER resolve to a complex. iNat files a species and its same-named complex
  # side by side -- "Andrena osmioides" the SPECIES, and "Osmioides Species Group",
  # a COMPLEX whose scientific name is also "Andrena osmioides". The species is the
  # target (closest true rank); its complex is reached later by roll-up (the species
  # row carries the parent complex via its ancestry). So only the non-complex
  # candidates are eligible; if every result is a complex, take nothing.
  ok <- which(rnk != "complex")
  if (length(ok) == 0) return(list(action = "skip", index = NA_integer_))

  # Exact name match (ignoring ssp./subsp./var.) auto-picks -- but ONLY at the
  # ranks Holway actually uses: species or subspecies. Matching on name alone was
  # the bug: a same-named aggregate (the "Andrena osmioides" COMPLEX, or a section)
  # equalled the search string and got picked. Restricting to species/subspecies
  # (preferring species) makes the aggregate ineligible. This still grabs the
  # correct subspecies (313836) and disambiguates near-spellings (nigra vs nigrae).
  if (!is.null(search_term)) {
    want <- .norm_name(search_term)
    got  <- vapply(results, function(t) .norm_name(t$name), character(1))
    exact <- which(got == want & rnk %in% c("species", "subspecies"))
    if (length(exact) >= 1) {
      sp <- exact[rnk[exact] == "species"]
      return(list(action = "pick", index = (if (length(sp)) sp else exact)[[1]]))
    }
  }

  # A subspecies with no exact match: do NOT fall back to the parent species --
  # route it to the ITIS keep/skip question instead.
  if (isTRUE(is_subspecies)) return(list(action = "skip", index = NA_integer_))

  if (length(ok) == 1) return(list(action = "pick", index = ok[[1]]))
  species_idx <- ok[rnk[ok] == "species"]
  if (!described) return(list(action = "skip", index = NA_integer_))
  if (length(species_idx) == 1) return(list(action = "pick", index = species_idx[[1]]))
  list(action = "prompt", index = NA_integer_)   # 0 or >1 species -> ask
}

# .yn(): true when the prompt answer is yes. PURE-ish (prompt_fn injected).
.yn <- function(prompt_fn, msg) tolower(trimws(prompt_fn(msg))) %in% c("y", "yes")

# itis_disposition(): after iNat yields no usable match, ask whether the name is
# valid per ITIS. "keep" (valid, retain w/o an iNat id) or "skip" (unpublished/
# invalid). Non-interactive defaults to "skip". Impure (prompts).
itis_disposition <- function(term, prompt_fn = readline, interactive_ok = TRUE) {
  if (!isTRUE(interactive_ok)) return("skip")
  url <- paste0("https://www.itis.gov/servlet/SingleRpt/SingleRpt",
                "?search_topic=Scientific_Name&search_value=",
                gsub(" ", "+", trimws(term)))
  message("")
  message("  Building the San Diego reference checklist (this is not one of the two")
  message("  taxon_id passes -- no bee of yours is being judged here).")
  message("")
  message("  '", term, "' is on the checklist but has no iNaturalist page.")
  message("")
  message("  That happens for two different reasons, and they need opposite answers:")
  message("    - it is a real bee iNaturalist has simply never published, or")
  message("    - the name is an old one that was renamed or withdrawn.")
  message("")
  message("  ITIS is a second taxonomy database, run by the US government. It says")
  message("  which of the two this is. Open the search -- it is already filled in:")
  message("")
  message("    ", url)
  message("")
  message("  Look at the line that says TAXONOMIC STATUS:")
  message("    'valid' or 'accepted'  ->  y   real bee; keep it, with no iNaturalist id")
  message("    'invalid', 'not accepted', or nothing found  ->  n   leave it off")
  message("")
  if (.yn(prompt_fn, "  Valid in ITIS?  [y / n]: ")) "keep" else "skip"
}

# ------------------------------------------------------------
# holway_search_term(): the string to search iNat for a Holway row -- ALSO the
# stable decision-cache key.
#   Described    -> "Genus species_raw" (UNCHANGED, so already-made picks still hit
#                   the cache and are never re-prompted).
#   non-Described (Tentative/Unpublished) -> "Genus <cleaned epithet>". These carry
#                   real names once the CF/MSN/aff./sp. nov. marker is stripped
#                   (e.g. "Andrena annectens" -> 573509), so we resolve them like a
#                   species instead of collapsing every one to a bare genus row.
#                   Falls back to the genus alone when nothing is left after
#                   stripping (a bare "sp. nov.").
# ------------------------------------------------------------
holway_search_term <- function(source_sheet, genus, species_raw) {
  if (identical(source_sheet, "Described")) return(trimws(paste(genus, species_raw)))
  ep <- clean_holway_species(if (is.na(species_raw)) "" else species_raw)
  if (!is.na(ep) && ep != "") trimws(paste(genus, ep)) else trimws(genus)
}

# .needs_alt_search(): PURE. Should we ask the human for an alternate name? YES when
# the search found NOTHING, or when the only hits are a same-named COMPLEX with no
# species/subspecies (iNat elevated the name and renamed the species). A genus-only
# result (the bare "sp. nov." fallback) does NOT trigger a retry -- that genus hit
# is expected, not a miss.
.needs_alt_search <- function(results) {
  if (length(results) == 0) return(TRUE)
  rnk <- vapply(results, function(t) t$rank %||% "", character(1))
  any(rnk == "complex") && !any(rnk %in% c("species", "subspecies"))
}

# ------------------------------------------------------------
# retry_empty_search(): PURE. When the initial iNat name search returns no usable
# species/subspecies -- either NOTHING, or only a same-named complex -- and the run
# is interactive, let a human supply alternate search terms (rescues misspelled or
# since-renamed Holway names, and the "iNat only has the complex" case). Returns the
# first usable result set, or the last results if the user skips / non-interactive.
# fetch_fn(term) does the lookup; prompt_fn() reads a line.
# ------------------------------------------------------------
retry_empty_search <- function(results, term, fetch_fn,
                               prompt_fn = readline, interactive_ok = TRUE) {
  cur <- term
  while (.needs_alt_search(results) && isTRUE(interactive_ok)) {
    lead <- if (length(results) == 0) paste0("No iNat match for '", cur, "'.")
            else paste0("iNat has only a complex (no species/subspecies) for '", cur, "'.")
    message(lead, " Enter an alternate name to search, or 'none' if not found.")
    raw <- trimws(prompt_fn("Alternate search (or 'none'): "))
    if (raw == "" || tolower(raw) %in% c("none", "skip")) break
    cur <- raw
    results <- fetch_fn(cur)
  }
  results
}

# holway_resolution_plan(): PURE. Decide the iNat search string and whether the
# row is a subspecies, given the source sheet, genus, and the split species_raw
# plus the user's answer to "is this a subspecies?" (needed only for two-word
# Described entries). Returns list(term, is_subspecies, ask_subspecies).
#   - non-Described        -> search the genus alone (provisional)
#   - one-word Described    -> search "genus species"
#   - two-word Described    -> ask_subspecies = TRUE; when confirmed, search
#                             "genus species ssp. subspecies"
holway_resolution_plan <- function(source_sheet, genus, sp) {
  if (!identical(source_sheet, "Described")) {
    # Tentative/Unpublished: resolve the CLEANED "Genus epithet" (sp$species is
    # already stripped of CF/MSN/aff./sp. nov.) so it can match its real iNat
    # taxon; fall back to the genus alone when there's no epithet left.
    if (!is.na(sp$species) && sp$species != "")
      return(list(term = trimws(paste(genus, sp$species)),
                  is_subspecies = FALSE, ask_subspecies = FALSE))
    return(list(term = trimws(genus), is_subspecies = FALSE, ask_subspecies = FALSE))
  }
  if (!is.na(sp$subspecies))
    # iNat's name search wants the PLAIN trinomial -- inserting "ssp." makes it
    # miss valid subspecies (e.g. Anthophora bomboides stanfordiana). The exact-
    # match normalizer still strips ssp./subsp. so matching is unaffected.
    return(list(term = paste(genus, sp$species, sp$subspecies),
                is_subspecies = TRUE, ask_subspecies = TRUE))
  list(term = paste(genus, sp$species), is_subspecies = FALSE, ask_subspecies = FALSE)
}

# ------------------------------------------------------------
# resolve_holway_row(): resolve one row to a taxon id, consulting/recording the
# decision cache and (only when needed and allowed) prompting. The decision is
# keyed on the stable "genus species_raw" identity (unchanged from before), so
# prior picks are reused. Returns list(taxon_id, action, term). Impure.
# ------------------------------------------------------------
# parse_slash_options(): PURE. Holway writes "epithet A / epithet B" for a
# synonym or uncertain-identity pair. Split on "/" and build the candidate full
# names ("Bombus californicus", "Bombus fervidus").
parse_slash_options <- function(genus, species_raw) {
  parts <- trimws(strsplit(species_raw, "/", fixed = TRUE)[[1]])
  parts <- parts[parts != ""]
  vapply(parts, function(p) trimws(paste(genus, clean_holway_species(p))),
         character(1), USE.NAMES = FALSE)
}

# resolve_slash_answer(): PURE. Map the user's answer to a search term:
# a number -> that option; 'none' / '' -> NA (give up); anything else ->
# "genus <typed epithet>" so they can supply a current name not in the pair.
resolve_slash_answer <- function(raw, genus, opts) {
  raw <- trimws(raw)
  if (raw == "" || tolower(raw) %in% c("none", "skip")) return(NA_character_)
  idx <- suppressWarnings(as.integer(raw))
  if (!is.na(idx) && idx >= 1 && idx <= length(opts)) opts[[idx]] else paste(genus, raw)
}

# .resolve_term(): shared search -> exact-match select -> keep/skip for a single
# search term. Impure. Returns list(taxon_id, action, term).
.resolve_term <- function(con, key, term, is_subspecies, described,
                          request_fn, interactive_ok, prompt_fn) {
  results <- get_taxa_by_name(con, term, request_fn = request_fn)
  if (.needs_alt_search(results) && interactive_ok) {
    results <- retry_empty_search(
      results, term,
      fetch_fn = function(t) get_taxa_by_name(con, t, request_fn = request_fn),
      prompt_fn = prompt_fn, interactive_ok = interactive_ok)
  }
  choice <- select_taxon_candidate(results, described = described,
                                   search_term = term, is_subspecies = is_subspecies)
  if (choice$action == "pick") {
    chosen <- results[[choice$index]]
    decision_put(con, key, "pick", chosen$id)
    return(list(taxon_id = as.integer(chosen$id), action = "pick", term = key))
  }
  if (choice$action == "prompt" && interactive_ok) {
    message("\nMore than one match for '", term, "' -- choose one:")
    for (i in seq_along(results)) {
      t <- results[[i]]
      message(sprintf("  [%d] id=%s %s (%s)", i, t$id, t$name, t$rank %||% "?"))
    }
    raw <- trimws(prompt_fn("Pick a number, or 'none' if none fit: "))
    idx <- suppressWarnings(as.integer(raw))
    if (!(raw %in% c("none", "skip")) && !is.na(idx) && idx >= 1 && idx <= length(results)) {
      chosen <- results[[idx]]
      decision_put(con, key, "pick", chosen$id)
      return(list(taxon_id = as.integer(chosen$id), action = "pick", term = key))
    }
  }
  # Nothing usable on iNat -- ask the ITIS keep/skip question.
  disp <- if (interactive_ok) itis_disposition(term, prompt_fn, interactive_ok) else "skip"
  decision_put(con, key, disp)
  list(taxon_id = NA_integer_, action = disp, term = key)
}

resolve_holway_row <- function(con, source_sheet, genus, species_raw,
                               request_fn = inat_request,
                               interactive_ok = TRUE,
                               prompt_fn = readline,
                               force = FALSE) {
  key <- holway_search_term(source_sheet, genus, species_raw)  # stable cache key

  # 'aff.' ("affinis") flags a species NEAR the named one, NOT that species, so it
  # must never resolve to that taxon. Check this BEFORE the decision cache: an aff.
  # name strips to the SAME search key as its Described sibling ("aff. miserabilis"
  # -> "Habropoda miserabilis"), so reading the cache here would inherit the
  # sibling's taxon_id. Short-circuit to unresolved and DON'T write the cache, so
  # the shared key stays owned by the real species.
  if (!identical(source_sheet, "Described")) {
    qual <- holway_qualifier(species_raw)
    if (!is.na(qual) && grepl("aff", qual, ignore.case = TRUE))
      return(list(taxon_id = NA_integer_, action = "tentative", term = key))
  }

  # force = TRUE bypasses the decision cache and re-resolves from scratch. Used to
  # auto-heal an old complex mis-pick (the cached decision would otherwise stick).
  decided <- if (force) NULL else decision_get(con, key)
  if (!is.null(decided)) {
    return(list(taxon_id = decided$chosen_taxon_id, action = decided$action, term = key))
  }

  # SLASH: "epithet A / epithet B" -- a synonym/uncertainty pair. Only the user
  # knows which name is current, so they pick which one to resolve.
  if (grepl("/", species_raw, fixed = TRUE)) {
    opts <- parse_slash_options(genus, species_raw)
    term <- NA_character_
    if (interactive_ok && length(opts) > 0) {
      message("'", trimws(paste(genus, species_raw)), "' lists ", length(opts), " names:")
      for (i in seq_along(opts)) message(sprintf("  [%d] %s", i, opts[[i]]))
      term <- resolve_slash_answer(prompt_fn("Which to use? (number, a name, or 'none'): "),
                                   genus, opts)
    }
    if (is.na(term)) {
      decision_put(con, key, "skip")
      return(list(taxon_id = NA_integer_, action = "skip", term = key))
    }
    return(.resolve_term(con, key, term, is_subspecies = FALSE, described = TRUE,
                         request_fn, interactive_ok, prompt_fn))
  }

  sp   <- split_holway_species(species_raw)
  plan <- holway_resolution_plan(source_sheet, genus, sp)

  # Two-word Described entry: confirm it's really a subspecies.
  if (plan$ask_subspecies && interactive_ok) {
    if (!.yn(prompt_fn, sprintf("Is '%s %s %s' a subspecies? (y/n): ",
                                genus, sp$species, sp$subspecies))) {
      decision_put(con, key, "tentative")   # not a subspecies -> provisional, blank
      return(list(taxon_id = NA_integer_, action = "tentative", term = key))
    }
  }

  .resolve_term(con, key, plan$term, plan$is_subspecies,
                identical(source_sheet, "Described"),
                request_fn, interactive_ok, prompt_fn)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ------------------------------------------------------------
# Clean reference-table layout -- mirrors sd_bee_taxonomy_lookup_generated.csv (minus the
# lookup-only computed columns verified/holway_status/in_*): taxon_id,
# scientific_name, common_name, rank, then the 19 taxonomic levels in order,
# complex_taxon_id, source_sheet, resolved. (2026-07: added subphylum, subclass,
# suborder, infraorder, epifamily -- sourced straight from the iNat ancestry.)
# ------------------------------------------------------------
HOLWAY_REF_LEVELS <- c("kingdom", "phylum", "subphylum", "class", "subclass",
                       "order", "suborder", "infraorder", "superfamily",
                       "family", "epifamily", "subfamily", "tribe", "subtribe",
                       "genus", "subgenus", "complex", "species", "subspecies")
# complex_taxon_id is intentionally omitted from the reference-table output
# (human-facing); the value still exists internally for the checklists.
HOLWAY_REF_COLUMNS <- c("taxon_id", "scientific_name", "common_name", "rank",
                        "source_sheet", "qualifier", "resolved", "itis_valid",
                        HOLWAY_REF_LEVELS)

# .strip_parens(): "(Hexosmia)" -> "Hexosmia"; NA/"" -> NA.
.strip_parens <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "") return(NA_character_)
  out <- trimws(gsub("[()]", "", x))
  if (out == "") NA_character_ else out
}

# .bare_epithet(): last token of a binomial/trinomial ("Andrena annectens" ->
# "annectens"), matching the lookup's bare-epithet species/subspecies columns.
.bare_epithet <- function(x) {
  if (length(x) == 0 || is.na(x) || x == "") return(NA_character_)
  stringr::word(x, -1)
}

# .ssp_scientific_name(): format a subspecies name with the "ssp." marker for
# display -- "Ashmeadiella cactorum ssp. basalis". Falls back to the binomial
# when there's no subspecies epithet.
.ssp_scientific_name <- function(genus, species, subspecies) {
  parts <- c(genus, species)
  parts <- parts[!is.na(parts) & parts != ""]
  base  <- if (length(parts) > 0) paste(parts, collapse = " ") else NA_character_
  if (!is.na(subspecies) && subspecies != "" && !is.na(base))
    paste0(base, " ssp. ", subspecies) else base
}

# tidy_holway_ref_row(): PURE. Reshape one parse_taxon_ranks() result plus the
# taxon's API name/common name into the clean reference-table layout. species/
# subspecies are reduced to bare epithets; scientific_name is the AUTHORITATIVE
# iNat taxon name (taxon$name), not a derived string.
tidy_holway_ref_row <- function(ranks, scientific_name, common_name, source_sheet,
                                itis_valid = NA, qualifier = NA_character_,
                                holway_subgenus = NA_character_) {
  sp_ep <- .bare_epithet(ranks$taxon_species_name)
  ss_ep <- .bare_epithet(ranks$taxon_subspecies_name)
  # subspecies get the "ssp." display form; everything else keeps the API name
  sci <- if (identical(ranks$rank, "subspecies") && !is.na(ss_ep))
    .ssp_scientific_name(ranks$taxon_genus_name, sp_ep, ss_ep)
  else scientific_name %||% NA_character_
  # prefer iNat's subgenus; fall back to Holway's own (e.g. a resolved tentative
  # species whose iNat ancestry omits the subgenus still keeps "(Micandrena)").
  subg <- if (is.na(ranks$subgenus)) .strip_parens(holway_subgenus) else ranks$subgenus
  tibble::tibble(
    taxon_id        = as.integer(ranks$taxon_id),
    scientific_name = sci,
    common_name     = common_name %||% NA_character_,
    rank            = ranks$rank %||% NA_character_,
    kingdom     = ranks$taxon_kingdom_name,     phylum      = ranks$taxon_phylum_name,
    subphylum   = ranks$taxon_subphylum_name,   class       = ranks$taxon_class_name,
    subclass    = ranks$taxon_subclass_name,    order       = ranks$taxon_order_name,
    suborder    = ranks$taxon_suborder_name,    infraorder  = ranks$taxon_infraorder_name,
    superfamily = ranks$taxon_superfamily_name, family      = ranks$taxon_family_name,
    epifamily   = ranks$taxon_epifamily_name,   subfamily   = ranks$taxon_subfamily_name,
    tribe       = ranks$taxon_tribe_name,       subtribe    = ranks$taxon_subtribe_name,
    genus       = ranks$taxon_genus_name,       subgenus    = subg,
    complex     = ranks$complex,
    species     = sp_ep,
    subspecies  = ss_ep,
    complex_taxon_id = ranks$complex_taxon_id,
    source_sheet = source_sheet, qualifier = qualifier,
    resolved = TRUE, itis_valid = as.logical(itis_valid)
  )
}

# unresolved_holway_ref_row(): PURE. Clean-layout row for a Holway entry with no
# iNat taxon. species_raw is split into species + subspecies; when is_subspecies
# is TRUE the row is rank="subspecies" and scientific_name is the trinomial,
# otherwise rank="species" (species = the cleaned epithet, which may be two
# provisional words) and scientific_name the binomial. scientific_name is built
# from Holway's OWN names here -- the only place we construct it, because no iNat
# taxon exists to source it from. itis_valid: TRUE (valid, not on iNat),
# FALSE (checked ITIS, not valid), or NA (provisional / not asked).
unresolved_holway_ref_row <- function(r, itis_valid = NA, is_subspecies = FALSE,
                                      qualifier = NA_character_) {
  sp      <- split_holway_species(r$species_raw %||% "")
  genus   <- r$genus %||% NA_character_
  if (isTRUE(is_subspecies)) {
    species_col    <- sp$species
    subspecies_col <- sp$subspecies
    rank           <- "subspecies"
    sci            <- .ssp_scientific_name(genus, species_col, subspecies_col)
  } else {
    species_col    <- clean_holway_species(r$species_raw %||% "")
    species_col    <- ifelse(is.na(species_col) | species_col == "", NA_character_, species_col)
    subspecies_col <- NA_character_
    rank           <- if (!is.na(species_col)) "species" else NA_character_
    parts <- c(genus, species_col)
    parts <- parts[!is.na(parts) & parts != ""]
    sci   <- if (length(parts) > 0) paste(parts, collapse = " ") else NA_character_
  }

  tibble::tibble(
    taxon_id = NA_integer_, scientific_name = sci,
    common_name = NA_character_, rank = rank,
    kingdom = NA_character_, phylum = NA_character_, subphylum = NA_character_,
    class = NA_character_, subclass = NA_character_,
    order = NA_character_, suborder = NA_character_, infraorder = NA_character_,
    superfamily = NA_character_,
    family = r$family %||% NA_character_, epifamily = NA_character_,
    subfamily = r$subfamily %||% NA_character_,
    tribe = r$tribe %||% NA_character_, subtribe = NA_character_,
    genus = genus, subgenus = .strip_parens(r$subgenus %||% NA_character_),
    complex = NA_character_, species = species_col, subspecies = subspecies_col,
    complex_taxon_id = NA_integer_,
    source_sheet = r$source_sheet %||% NA_character_, qualifier = qualifier,
    resolved = FALSE, itis_valid = as.logical(itis_valid)
  )
}

# ------------------------------------------------------------
# ANCESTRY CAPTURE (replaces the rejected "species inherits parent id" roll-up).
# An unresolved species keeps taxon_id = NA. Instead of stamping a parent's id onto
# the species row, we harvest each RESOLVED taxon's ancestors and turn every one
# into its OWN full row -- appended to the SAME reference table (no side-file) -- so
# every parent taxon from kingdom down to subgenus appears as its own id-bearing
# row, even when that parent was never observed in SD County.
#
# ancestor_reference_rows(): PURE. One reference-layout row per ANCESTOR of `taxon`
# (not the taxon itself -- that IS the Holway row). Each ancestor is rebuilt as a
# synthetic taxon carrying the ancestors ABOVE it, so parse_taxon_ranks fills its
# levels down to its own rank. Tagged source_sheet = "iNat ancestry" so the lookup
# can tell an ancestor row from a Holway entry.
# ------------------------------------------------------------
ancestor_reference_rows <- function(taxon) {
  ancs <- taxon$ancestors %||% list()
  if (length(ancs) == 0) return(NULL)
  rows <- vector("list", length(ancs))
  for (k in seq_along(ancs)) {
    a <- ancs[[k]]
    if (!is.list(a)) next
    synth <- list(id = a$id, rank = a$rank, name = a$name,
                  ancestors = if (k > 1) ancs[seq_len(k - 1)] else list())
    ranks <- tryCatch(parse_taxon_ranks(synth), error = function(e) NULL)
    if (is.null(ranks) || is.na(ranks$taxon_id)) next
    rows[[k]] <- tidy_holway_ref_row(
      ranks, scientific_name = .scalar(a$name, NA_character_),
      common_name = NA_character_, source_sheet = "iNat ancestry",
      itis_valid = NA, qualifier = NA_character_, holway_subgenus = NA_character_)
  }
  dplyr::bind_rows(rows)
}

# accumulate_ancestor_rows(): grow a taxon_id-distinct table of ancestor rows.
accumulate_ancestor_rows <- function(acc, taxon) {
  if (is.null(taxon)) return(acc)
  rows <- tryCatch(ancestor_reference_rows(taxon), error = function(e) NULL)
  if (is.null(rows) || nrow(rows) == 0) return(acc)
  dplyr::distinct(dplyr::bind_rows(acc, rows), taxon_id, .keep_all = TRUE)
}

# ------------------------------------------------------------
# WALK-BACK: an unresolved (unpublished) species keeps taxon_id = NA, but its empty
# higher-taxonomy NAME columns are filled from its parent's ancestry, so the row is
# fully placed in the tree by name. The lineage above genus is identical for every
# species in a genus, so we key on genus.
# ------------------------------------------------------------
# The above-genus lineage + genus itself -- copied onto an unresolved row (its own
# subgenus/complex/species/subspecies columns are kept as Holway has them).
ANCESTRY_LINEAGE_COLS <- c("kingdom", "phylum", "subphylum", "class", "subclass",
                           "order", "suborder", "infraorder", "superfamily",
                           "family", "epifamily", "subfamily", "tribe", "subtribe",
                           "genus")

# genus_lineage_map(): PURE. One lineage per genus from id-bearing rows (resolved
# Holway + ancestor rows), preferring the clean genus-rank row. Keyed on .gkey
# (lowercased genus).
genus_lineage_map <- function(id_rows) {
  if (is.null(id_rows) || nrow(id_rows) == 0)
    return(tibble::tibble(.gkey = character(0)))
  id_rows |>
    dplyr::filter(!is.na(genus), genus != "") |>
    dplyr::mutate(.gkey = tolower(trimws(genus)),
                  .isgenus = !is.na(rank) & rank == "genus") |>
    dplyr::arrange(.gkey, dplyr::desc(.isgenus)) |>
    dplyr::group_by(.gkey) |> dplyr::slice(1) |> dplyr::ungroup() |>
    dplyr::select(.gkey, dplyr::any_of(ANCESTRY_LINEAGE_COLS))
}

# fill_unresolved_lineage(): PURE. Fill each unresolved row's (taxon_id NA) blank
# lineage columns from its genus's lineage -- coalesce, so Holway's own family/
# subfamily/tribe are kept and only the gaps (kingdom..superfamily, etc.) fill in.
# taxon_id STAYS NA -- no id is borrowed, only names.
fill_unresolved_lineage <- function(ref, gmap) {
  if (is.null(gmap) || nrow(gmap) == 0) return(ref)
  lut <- gmap
  names(lut)[names(lut) != ".gkey"] <- paste0(".p_", names(lut)[names(lut) != ".gkey"])
  ref <- ref |>
    dplyr::mutate(.gkey = tolower(trimws(genus))) |>
    dplyr::left_join(lut, by = ".gkey")
  for (col in ANCESTRY_LINEAGE_COLS) {
    pc <- paste0(".p_", col)
    if (!pc %in% names(ref)) next
    need_fill <- is.na(ref$taxon_id) & (is.na(ref[[col]]) | ref[[col]] == "")
    ref[[col]][need_fill] <- ref[[pc]][need_fill]
  }
  ref |> dplyr::select(-dplyr::starts_with(".p_"), -.gkey)
}

# resolve_missing_genera(): IMPURE (fetches). For unresolved-row genera not already
# covered by a resolved sibling, look the genus up on iNat (exact genus-rank match),
# pull its FULL taxon (with ancestors), and return the genus row + its ancestor rows
# to append. Best-effort: a genus that doesn't resolve is left blank.
resolve_missing_genera <- function(unresolved_genera, known_gkeys, con,
                                   request_fn = inat_request) {
  need <- setdiff(unique(tolower(trimws(unresolved_genera))), known_gkeys)
  need <- need[!is.na(need) & need != ""]
  if (length(need) == 0) return(NULL)
  out <- list()
  for (g in need) {
    res <- tryCatch(get_taxa_by_name(con, g, request_fn = request_fn),
                    error = function(e) list())
    hit <- NULL
    for (t in res)
      if (identical(t$rank %||% "", "genus") &&
          identical(.norm_name(t$name %||% ""), .norm_name(g))) { hit <- t; break }
    if (is.null(hit)) next
    full <- tryCatch(get_taxon_by_id(con, hit$id, request_fn = request_fn),
                     error = function(e) NULL)
    if (is.null(full)) next
    genus_row <- tryCatch(tidy_holway_ref_row(
      parse_taxon_ranks(full), scientific_name = .scalar(full$name, NA_character_),
      common_name = NA_character_, source_sheet = "iNat ancestry",
      itis_valid = NA, qualifier = NA_character_, holway_subgenus = NA_character_),
      error = function(e) NULL)
    out[[length(out) + 1]] <- dplyr::bind_rows(genus_row, ancestor_reference_rows(full))
  }
  if (length(out)) dplyr::bind_rows(out) else NULL
}

# ------------------------------------------------------------
# SECOND PASS -- after the whole sheet is resolved, revisit the rows that SHOULD be
# on iNat but came back blank: the "Described" species + subspecies. The human either
# types the iNat taxon_id directly, or types a corrected/renamed name to search
# (e.g. Calliopsis anthidius -> "Calliopsis anthida"). Slash "A / B" rows are handled
# in the first pass and are NOT revisited; aff./sp.nov. rows are left alone (nothing
# to resolve them to). The pick is cached so a later run reuses it with no prompt.
# ------------------------------------------------------------
#' The one-time explanation for the second pass
#'
#' Printed once before the first name, not per name. It used to repeat in full for
#' every unresolved bee -- six names meant six copies of the same twenty lines --
#' while the sibling prompt in manual_overrides.R printed its banner once and then
#' a compact card each time. That shape is the right one.
#'
#' @param n How many names are about to be asked about.
#' @return Invisibly NULL; prints.
.second_pass_banner <- function(n) {
  message("")
  message("  ", n, " name", if (n == 1) "" else "s",
          " on the Holway San Diego County Bee Checklist v3 did not")
  message("  match anything on iNaturalist.")
  message("     the checklist   https://library.ucsd.edu/dc/collection/bb7305352v")
  message("")
  message("  WHAT THIS STEP IS DOING")
  message("    Matching every name on that checklist to iNaturalist's number for the")
  message("    same bee -- its taxon_id -- so the pipeline can join on the taxon_id")
  message("    instead of the name. Names get respelled; taxon_ids do not.")
  message("")
  message("  WHY A NAME DOES NOT MATCH -- usually one of three things:")
  message("    * iNaturalist lists it under a different name (renamed, or a synonym)")
  message("    * the taxonomy changed -- the bee was split, lumped into another, or")
  message("      moved to a different genus, so it is filed somewhere else now")
  message("    * iNaturalist has no page for it at all, which is normal for some bees")
  message("")
  message("  Each name below comes with two links, already filled in: iNaturalist,")
  message("  and ITIS (the US government taxonomy database), which says whether a")
  message("  name is still accepted and what it was renamed to.")
  message("")
  .row <- function(k, v) message("    ", formatC(k, width = -18), "  ", v)
  message("  FOUR THINGS YOU CAN TYPE -- pick one:")
  .row("TYPE", "WHAT HAPPENS")
  .row(strrep("-", 18), strrep("-", 46))
  .row("345235", "a number -> becomes this bee's taxon_id. It is the")
  .row("", "  number in inaturalist.org/taxa/345235-Colletes-hyalinus")
  .row("a name", "scientific name, genus, subgenus, complex, species")
  .row("", "  or subspecies -> searches iNaturalist for that instead")
  .row("none", "iNaturalist has no page for this bee -> recorded. You")
  .row("", "  are asked again next run, in case one gets added.")
  .row("skip", "not sure -> nothing recorded, asked again next run.")
  .row("", "  Pressing Enter does the same.")
  message("")
  message("  NOT SURE? PRESS ENTER. A wrong taxon_id is worse than none -- it silently")
  message("  attaches this bee's records to a different bee. Skipping costs nothing.")
  invisible(NULL)
}

#' Is this checklist row one the second pass asks about?
#'
#' PURE. Described rows with no iNaturalist id, excluding the slash "A / B" rows the
#' first pass handles. A previous "no iNaturalist page" answer used to make this FALSE
#' forever -- but iNaturalist does add bees, and the automatic resolver caches its own
#' not-found verdict too, so nothing in the pipeline would ever have noticed. Ask every
#' run; the card says what was answered last time so it does not read as amnesia.
#'
#' @param source_sheet The Holway sheet the row came from.
#' @param species_raw The raw species cell.
#' @param row The built reference row, or NULL if it could not be built.
#' @return TRUE if the operator should be asked about this row.
.second_pass_asks <- function(source_sheet, species_raw, row) {
  if (!identical(source_sheet, "Described")) return(FALSE)
  sr <- species_raw; if (is.na(sr)) sr <- ""
  if (grepl("/", sr, fixed = TRUE)) return(FALSE)
  !(is.null(row) || !is.na(row$taxon_id))
}

#' One unresolved name, as a compact card
#'
#' @param r The checklist row (`genus`, `species_raw`, `subgenus`).
#' @param k Position in the queue.
#' @param n How many there are in total.
#' @param prior What was decided last run, from `decision_get()`, or NULL the first
#'   time. A "no iNaturalist page" answer no longer silences the question, so the
#'   card has to say the question is a repeat and how long ago it was answered --
#'   otherwise it reads as the pipeline having forgotten.
#' @return Invisibly NULL; prints.
.second_pass_item <- function(r, k, n, prior = NULL) {
  label <- trimws(paste(r$genus %||% "", r$species_raw %||% ""))
  subg  <- .strip_parens(r$subgenus %||% NA_character_)
  q     <- gsub(" ", "+", trimws(label))
  message("")
  message(sprintf("  [%d/%d] '%s'%s", k, n, label,
                  if (!is.na(subg)) paste0("  subgenus (", subg, ")") else ""))
  if (!is.null(prior) && identical(prior$action %||% "", "no_inat_id"))
    message("     last time you said: no iNaturalist page  (",
            substr(as.character(prior$decided_at %||% ""), 1, 10), ")")
  message("     iNaturalist  https://www.inaturalist.org/search?q=", q)
  message("     ITIS         https://www.itis.gov/servlet/SingleRpt/SingleRpt",
          "?search_topic=Scientific_Name&search_value=", q)
  invisible(NULL)
}

# .second_pass_resolve(): IMPURE. Resolve ONE unresolved Described row interactively.
# Returns the chosen FULL taxon (with ancestors) or NULL if skipped / not found.
.second_pass_resolve <- function(con, r, request_fn = inat_request, prompt_fn = readline,
                                 .k = 1L, .n = 1L) {
  key   <- holway_search_term(r$source_sheet, r$genus, r$species_raw)
  label <- trimws(paste(r$genus %||% "", r$species_raw %||% ""))
  subg  <- .strip_parens(r$subgenus %||% NA_character_)
  # a "no iNaturalist page" answer no longer silences this question, so show what
  # was answered last time -- otherwise a repeat reads as the pipeline forgetting.
  prior <- tryCatch(decision_get(con, key), error = function(e) NULL)
  .second_pass_item(r, .k, .n, prior = prior)
  raw <- trimws(prompt_fn("   > "))
  if (raw == "" || tolower(raw) %in% c("skip", "s")) return(NULL)
  if (tolower(raw) %in% c("n", "no", "noid", "none")) {
    decision_put(con, key, "no_inat_id")
    message("  marked 'no iNat id yet' -- won't prompt again until it's cleared.")
    return("no_inat_id")
  }

  # a bare number -> a taxon_id the human looked up on iNaturalist.org
  if (grepl("^[0-9]+$", raw)) {
    id    <- as.integer(raw)
    taxon <- tryCatch(get_taxon_by_id(con, id, request_fn = request_fn), error = function(e) NULL)
    if (is.null(taxon)) { message("  no iNat taxon for id ", id, " -- skipped."); return(NULL) }
    message(sprintf("  -> id %s = %s (%s)", id, .scalar(taxon$name, "?"),
                    parse_taxon_ranks(taxon)$rank %||% "?"))
    decision_put(con, key, "pick", id)
    return(taxon)
  }

  # otherwise a (possibly renamed) name -> search, list candidates, pick one
  results <- tryCatch(get_taxa_by_name(con, raw, request_fn = request_fn), error = function(e) list())
  if (length(results) == 0) { message("  no matches for '", raw, "' -- skipped."); return(NULL) }
  for (j in seq_along(results)) {
    t <- results[[j]]
    message(sprintf("   [%d] id=%s  %s  (%s)", j, t$id %||% "?", t$name %||% "?", t$rank %||% "?"))
  }
  pj <- suppressWarnings(as.integer(trimws(prompt_fn("  pick a number (or blank to skip): "))))
  if (is.na(pj) || pj < 1 || pj > length(results)) { message("  skipped."); return(NULL) }
  chosen <- results[[pj]]
  full   <- tryCatch(get_taxon_by_id(con, as.integer(chosen$id), request_fn = request_fn),
                     error = function(e) NULL)
  decision_put(con, key, "pick", as.integer(chosen$id))
  full %||% chosen
}

# run_described_second_pass(): loop the unresolved Described rows through
# .second_pass_resolve(), rebuilding each row and capturing its ancestry on success.
# Returns the updated list(rows, ancestry). No-op when non-interactive or none remain.
run_described_second_pass <- function(con, holway_df, rows, ancestry,
                                      request_fn = inat_request, prompt_fn = readline,
                                      interactive_ok = TRUE) {
  if (!isTRUE(interactive_ok)) return(list(rows = rows, ancestry = ancestry))
  todo <- which(vapply(seq_len(nrow(holway_df)),
                       function(i) .second_pass_asks(holway_df$source_sheet[i],
                                                     holway_df$species_raw[i], rows[[i]]),
                       logical(1)))
  if (length(todo) == 0) return(list(rows = rows, ancestry = ancestry))

  .second_pass_banner(length(todo))   # once, not per name
  .k <- 0L
  for (i in todo) {
    r <- holway_df[i, ]
    .k <- .k + 1L
    is_ss <- !grepl("/", r$species_raw %||% "", fixed = TRUE) &&
             !is.na(split_holway_species(r$species_raw %||% "")$subspecies)
    qual  <- holway_qualifier(r$species_raw %||% "")
    out <- tryCatch(.second_pass_resolve(con, r, request_fn = request_fn, prompt_fn = prompt_fn,
                                        .k = .k, .n = length(todo)),
                    error = function(e) NULL)
    if (is.character(out) && length(out) == 1L && out == "no_inat_id") {
      # human confirmed there's no iNat taxon yet -> keep it blank but mark it valid
      # (itis_valid = TRUE) so future runs stop prompting (cache carries the decision).
      rows[[i]] <- unresolved_holway_ref_row(r, itis_valid = TRUE,
                                             is_subspecies = is_ss, qualifier = qual)
      next
    }
    if (is.null(out)) next
    rows[[i]] <- tidy_holway_ref_row(
      parse_taxon_ranks(out),
      scientific_name = .scalar(out$name, NA_character_),
      common_name     = .scalar(out$preferred_common_name, NA_character_),
      source_sheet    = r$source_sheet, itis_valid = NA,
      qualifier = qual, holway_subgenus = r$subgenus %||% NA_character_)
    ancestry <- accumulate_ancestor_rows(ancestry, out)
  }
  list(rows = rows, ancestry = ancestry)
}

# ------------------------------------------------------------
# build_holway_reference(): full interactive run over the Holway sheet.
# Produces a taxonomy tibble in the clean reference-table layout (one row per
# Holway entry, full ancestry) and returns it; the runner writes it to disk.
# ------------------------------------------------------------
#' Resolve every Holway checklist name to an iNaturalist taxon
#'
#' Already-decided taxa are batch-prefetched with their ancestors first, so the
#' per-row lookups below are cache hits rather than hundreds of throttled
#' requests. Names that cannot be resolved unambiguously are left without an id
#' on purpose -- a wrong id is worse than none (see LIMITATIONS.md).
#'
#' @param con An open DuckDB connection to the cache.
#' @param holway_df The Holway checklist, as loaded by `load_holway()`.
#' @param request_fn Injection point for the API call; tests pass a fake so no
#'   test ever touches the real API.
#' @param interactive_ok Allow prompting a human for an ambiguous name. Set
#'   `FALSE` for an unattended run.
#' @return The resolved reference table.
build_holway_reference <- function(con, holway_df,
                                   request_fn = inat_request,
                                   interactive_ok = TRUE) {
  # Batch-prefetch every already-decided taxon in full (with ancestors) so the
  # per-row get_taxon_by_id() calls below are cache hits -- turns hundreds of
  # single requests into a few throttled batch requests.
  if (exists("prefetch_taxa")) {
    decided_ids <- integer(0)
    for (i in seq_len(nrow(holway_df))) {
      d <- decision_get(con, holway_search_term(holway_df$source_sheet[i],
                                                holway_df$genus[i], holway_df$species_raw[i]))
      if (!is.null(d) && !is.na(d$chosen_taxon_id)) decided_ids <- c(decided_ids, d$chosen_taxon_id)
    }
    if (length(decided_ids) > 0)
      prefetch_taxa(con, unique(decided_ids), request_fn = request_fn)
  }

  rows <- vector("list", nrow(holway_df))
  ancestry <- NULL   # grows to distinct (taxon_id, rank, name) over resolved taxa
  for (i in seq_len(nrow(holway_df))) {
    r <- holway_df[i, ]
    res <- resolve_holway_row(con, r$source_sheet, r$genus, r$species_raw,
                              request_fn = request_fn, interactive_ok = interactive_ok)
    # Fetch the resolved taxon once. AUTO-HEAL an old complex mis-pick: if the
    # cached decision landed on a complex-rank taxon, re-resolve with the fixed
    # rule (which prefers the same-named species and never takes a complex solo).
    taxon <- if (!is.na(res$taxon_id))
      get_taxon_by_id(con, res$taxon_id, request_fn = request_fn) else NULL
    if (!is.null(taxon) && identical(parse_taxon_ranks(taxon)$rank, "complex")) {
      res   <- resolve_holway_row(con, r$source_sheet, r$genus, r$species_raw,
                                  request_fn = request_fn, interactive_ok = interactive_ok,
                                  force = TRUE)
      taxon <- if (!is.na(res$taxon_id))
        get_taxon_by_id(con, res$taxon_id, request_fn = request_fn) else NULL
    }
    # Harvest THIS resolved taxon's ancestors as full id-bearing rows. A resolved
    # sibling is what lets an unobserved parent genus/family appear as its own row
    # AND lets an unresolved sibling's lineage names be walked back below.
    ancestry <- accumulate_ancestor_rows(ancestry, taxon)
    # a two-word entry that reached keep/skip was a confirmed subspecies (but a
    # slash "A / B" pair is never a subspecies)
    is_ss <- !grepl("/", r$species_raw %||% "", fixed = TRUE) &&
             !is.na(split_holway_species(r$species_raw %||% "")$subspecies)
    qual  <- holway_qualifier(r$species_raw %||% "")   # "CF"/"MSN"/"aff."/"sp. nov." or NA
    rows[[i]] <- if (!is.null(taxon)) {
      ranks <- parse_taxon_ranks(taxon)
      tidy_holway_ref_row(
        ranks,
        scientific_name = .scalar(taxon$name, NA_character_),
        common_name     = .scalar(taxon$preferred_common_name, NA_character_),
        source_sheet    = r$source_sheet, itis_valid = NA,
        qualifier = qual, holway_subgenus = r$subgenus %||% NA_character_)
    } else {
      # No direct iNat species/subspecies match. This row keeps taxon_id = NA --
      # a species/subspecies NEVER inherits a parent's id. Its parent taxa each get
      # their OWN id-bearing row in the taxonomy lookup (filled from the ancestry
      # side-table below), so the bee is still placed in the tree without the
      # species row borrowing an id it doesn't own.
      itis <- switch(res$action %||% "skip", keep = TRUE, no_inat_id = TRUE, skip = FALSE, NA)
      unresolved_holway_ref_row(r, itis_valid = itis, is_subspecies = is_ss, qualifier = qual)
    }
    if (i %% 150 == 0 || i == nrow(holway_df)) bx_cont("resolving ", i, " / ", nrow(holway_df), " …")
  }

  # SECOND PASS: revisit unresolved Described rows (subspecies + renamed species like
  # Calliopsis anthidius -> anthida). Runs before the walk-back/append so a newly
  # resolved row contributes its own ancestry too.
  sp2 <- run_described_second_pass(con, holway_df, rows, ancestry,
                                   request_fn = request_fn, interactive_ok = interactive_ok)
  rows <- sp2$rows; ancestry <- sp2$ancestry

  ref <- dplyr::bind_rows(rows) |> dplyr::select(dplyr::any_of(HOLWAY_REF_COLUMNS))
  anc_rows <- ancestry

  # WALK-BACK (Goal 2): fetch ancestry for any unresolved-row genus not already
  # covered by a resolved sibling, so every unpublished bee's lineage can be filled.
  unresolved_genera <- ref$genus[is.na(ref$taxon_id) & !is.na(ref$genus)]
  known_gkeys <- unique(tolower(trimws(c(
    ref$genus[!is.na(ref$taxon_id)],
    if (!is.null(anc_rows)) anc_rows$genus else character(0)))))
  known_gkeys <- known_gkeys[!is.na(known_gkeys) & known_gkeys != ""]
  fetched <- tryCatch(resolve_missing_genera(unresolved_genera, known_gkeys, con,
                                             request_fn = request_fn),
                      error = function(e) NULL)
  if (!is.null(fetched))
    anc_rows <- dplyr::distinct(dplyr::bind_rows(anc_rows, fetched), taxon_id, .keep_all = TRUE)

  # Fill each unresolved (unpublished) row's blank lineage names from its genus's
  # ancestry. taxon_id stays NA -- names only, never an id.
  id_rows <- dplyr::bind_rows(dplyr::filter(ref, !is.na(taxon_id)), anc_rows)
  ref <- fill_unresolved_lineage(ref, genus_lineage_map(id_rows))

  # APPEND ancestor taxa (Goal 1): each parent gets its OWN row with its OWN iNat id,
  # in the SAME table. Drop any ancestor whose taxon_id already belongs to a Holway
  # row (the Holway entry wins, keeping its source_sheet/qualifier).
  if (!is.null(anc_rows) && nrow(anc_rows) > 0) {
    have_ids <- ref$taxon_id[!is.na(ref$taxon_id)]
    add <- anc_rows |>
      dplyr::filter(!is.na(taxon_id), !(taxon_id %in% have_ids)) |>
      dplyr::select(dplyr::any_of(HOLWAY_REF_COLUMNS))
    ref <- dplyr::bind_rows(ref, add)
  }
  # user-curated manual overrides (name-changed / synonym taxa, e.g. Holway 'Holcopasites minima'
  # == iNat 'minimus') win over the automated resolution, so a hand-recorded id + corrected name
  # land in the Holway reference table too -- not just the lookup. Sourced by the pipeline /
  # standalone runner; guarded so isolated unit tests that don't source it still run.
  if (!exists("apply_manual_overrides")) source(file.path("scripts", "reference/prompts/manual_overrides.R"))
  ref <- apply_manual_overrides(ref)
  ref
}

# ------------------------------------------------------------
# Runner (only when executed directly, not when sourced for tests OR by the pipeline).
# ------------------------------------------------------------
# .holway_autorun_ok(): should the bottom-of-file runner fire? TRUE only when this file is
# run/sourced at top level (env is globalenv) with BEESCABR_RUN_HOLWAY set AND the pipeline
# runner is NOT the one sourcing it. run_data_cleaning_pipeline.R sets BEESCABR_SOURCED_BY_RUNNER (and calls
# build_holway_reference itself at stage 4); without this sentinel guard -- if BEESCABR_RUN_HOLWAY
# lingers in the session env -- the table built TWICE (once at source-time, once at stage 4).
# Pure so the branching is unit-tested; pass environment() from the call site (top level).
.holway_autorun_ok <- function(env,
                               run_flag = Sys.getenv("BEESCABR_RUN_HOLWAY", unset = NA),
                               sourced_by_runner = exists("BEESCABR_SOURCED_BY_RUNNER")) {
  identical(env, globalenv()) && !is.na(run_flag) && !isTRUE(sourced_by_runner)
}

if (.holway_autorun_ok(environment())) {
  source("scripts/config.R")
  source("scripts/inat_observations/engine/db/store_conn.R"); source("scripts/inat_observations/engine/db/taxon_store.R")
  source("scripts/inat_observations/engine/db/decision_store.R"); source("scripts/inat_observations/engine/api/inat_http.R")
  source("scripts/inat_observations/engine/api/inat_flatten.R"); source("scripts/inat_observations/engine/api/inat_cache.R")
  source("scripts/reference/taxonomy/holway.R")
  source("scripts/reference/prompts/manual_overrides.R")

  con <- store_connect()
  on.exit(store_disconnect(con), add = TRUE)
  holway_df <- load_holway(PATHS$holway_combined)
  interactive_ok <- Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"
  ref <- build_holway_reference(con, holway_df, interactive_ok = interactive_ok)
  write.csv(ref, PATHS$holway_reference, row.names = FALSE, na = "")
  is_anc <- !is.na(ref$source_sheet) & ref$source_sheet == "iNat ancestry"
  message("Wrote ", sum(ref$resolved[!is_anc], na.rm = TRUE), " resolved of ",
          sum(!is_anc), " Holway rows + ", sum(is_anc), " ancestor rows = ",
          nrow(ref), " total -> ", basename(PATHS$holway_reference))
}
