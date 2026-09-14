# =============================================================
# config.R
# beescabr pipeline -- central configuration
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# Single source of truth for the constants that used to be scattered
# across native_bee_checklist.R, inat_bee_clean.R, spatial_utils.R and
# the old Python pipeline (place_id, taxon ids, CRS, the iNat user-agent,
# the DuckDB cache path, and the observation-field id map).
#
# Source this near the top of any script in the pipeline:
#   source("scripts/config.R")
# =============================================================

# ---- iNaturalist API ---------------------------------------------------------
# ---- EXTERNAL API VERSIONS (single source of truth; quote these in any write-up) ----
# iNaturalist: REST API v1. Everything the pipeline pulls -- observations, taxa,
#   observation fields -- goes through https://api.inaturalist.org/v1/. iNat also has a
#   newer v2, which we do NOT use; a move to v2 would change response shapes and must be
#   a deliberate migration (see dev-docs/TODO.md).
# IUCN Red List: API v4, reached through the rredlist R package rather than raw HTTP.
#   Requires a free token (data/secrets/iucn_api.env or IUCN_REDLIST_KEY). Recorded in
#   the source column of data/checklists/iucn/iucn_status_generated.csv on every fetch, so each
#   cached status carries the API version it came from.
# ---- Packages the pipeline needs ---------------------------------------------
# THE LIST lives here because config.R is the constants file; INSTALLING lives in
# scripts/utils/install_requirements.R because config.R is sourced by every script and
# must not touch the network at load. Run the installer once on a new machine:
#     source("scripts/utils/install_requirements.R")
# The per-script auto-install blocks stay as a safety net; they should rarely fire.
# The oldest R this codebase can even be PARSED by. 34 scripts use the native pipe
# `|>` and 16 use the `\(x)` lambda; both arrived in R 4.1.0, and on anything older
# they are syntax errors -- so the failure is "unexpected '>'" inside a file the
# reader had no reason to open. Checked here because config.R is sourced by every
# script, so the message lands before anything else can go wrong.
BEESCABR_MIN_R <- "4.1.0"

#' Stop unless R is new enough to parse this codebase
#'
#' @param current The running R version; injectable so the check is testable.
#' @param min Minimum supported version.
#' @param stop_fn Injectable, so a test can capture the message instead of halting.
#' @return Invisibly TRUE when the version is fine.
beescabr_check_r_version <- function(current = getRversion(), min = BEESCABR_MIN_R,
                                     stop_fn = stop) {
  if (package_version(current) < package_version(min))
    stop_fn("beescabr needs R ", min, " or newer -- this is R ", current, ".\n",
            "  34 scripts use the |> pipe and 16 use \\(x), which R ", current,
            " cannot parse.\n",
            "  Update R at https://cloud.r-project.org, then reopen beescabr.Rproj.",
            call. = FALSE)
  invisible(TRUE)
}
beescabr_check_r_version()

BEESCABR_PACKAGES <- c(
  # data wrangling + IO
  "dplyr", "tidyr", "purrr", "stringr", "stringi", "tibble", "readr", "readxl", "lubridate",
  # storage + web
  "duckdb", "DBI", "httr2", "jsonlite",
  # spatial + maps
  "sf", "leaflet", "ggspatial", "prettymapr", "htmlwidgets", "htmltools",
  # figures
  "ggplot2", "ggrepel", "ggridges", "ggtext", "ggpattern", "cowplot", "gridExtra", "scales",
  # ecology + stats
  "vegan", "iNEXT", "bipartite", "igraph",
  # reference data + docs
  "rredlist", "pdftools", "rmarkdown",
  # magick: shrinks the team photos before they are embedded in the acknowledgements
  # page. macOS has `sips` built in; magick is what covers Windows and Linux, where the
  # page was otherwise built with full-size headshots in it (13 MB against 2.2 MB).
  "magick",
  # tests
  "testthat", "withr",
  # quality of life
  # askpass/getPass hide an API key while it is typed, ragg renders figure text
  # better. These were once a separate "optional" list that
  # install_requirements.R named and then walked past, so a fresh machine finished
  # setup still missing them. One list, everything installed.
  "askpass", "getPass", "ragg")

# beescabr_require(): the per-script dependency guard. CHECKS, never installs.
#
# Every script used to carry its own for(pkg in ...) install.packages() block. That
# duplicated the list (those blocks covered 14 packages while the real set is 36, so they
# had already drifted), and it meant running one script could silently install software.
# Now a script calls beescabr_require() and, if anything is missing, stops with the single
# command that fixes it. Installing is install_requirements.R's job alone.
#
# Defaults to the WHOLE list rather than a per-script subset: a subset is another list to
# keep in sync, and that is exactly the drift this replaced.
# have_fn / stop_fn are injectable so the failure path is testable.
beescabr_require <- function(pkgs = BEESCABR_PACKAGES,
                             have_fn = function(p) requireNamespace(p, quietly = TRUE),
                             stop_fn = stop) {
  miss <- pkgs[!vapply(pkgs, have_fn, logical(1))]
  if (length(miss))
    stop_fn("beescabr: ", length(miss), " required package(s) missing: ",
            paste(miss, collapse = ", "), "\n",
            "  Run this once, then try again:\n",
            "    source('scripts/utils/install_requirements.R')",
            call. = FALSE)
  invisible(TRUE)
}

INAT_API_VERSION <- "v1"
IUCN_API_VERSION <- "v4"
IUCN_API_CLIENT  <- "rredlist"

INAT_BASE_URL   <- "https://api.inaturalist.org/v1/"
INAT_USER_AGENT <- "beescabr pipeline (brandirenesanchez16@gmail.com)"

# Pause between API calls (seconds). iNat's hard limit is ~100 requests/min
# and they ask you to keep well under it; with ~0.3-0.5s of network time per
# request, a 0.5s pause lands around ~70-80 req/min -- fast but courteous.
# iNaturalist's published limits (v2 API docs): 100 requests/minute is the hard cap, they
# ASK for 60/minute or lower, and under 10,000/day, warning that they "may institute blocks
# without notification". 1 second between calls puts us at 60/minute, the rate they ask for.
# It was 0.5 (120/minute), over even the hard cap. Do not lower it: a block would cost the
# whole season's ingest, and the pull is not time-critical.
# The transport still backs off exponentially on 429/5xx.
INAT_THROTTLE_SEC <- 1.0

# ---- Taxa / place ------------------------------------------------------------
# Anthophila (all bees). Root taxon for every observation query.
TAXON_ANTHOPHILA <- 630955L
# Apis mellifera (Western honey bee) -- excluded via without_taxon_id so the
# checklist stays native-only. (This is the 47219 seen in the Python query.)
TAXON_APIS_MELLIFERA <- 47219L
# iNat place_id for the "San Diego County 25 Mile Buffer" custom place that
# the retired CSV export was scoped to. Same id used in the Python pipeline.
PLACE_SD_COUNTY_BUFFER <- 118491L

# ---- PLANT ingest: taxon / place ---------------------------------------------
# Vascular plants (Tracheophyta) -- the ROOT taxon for the plant pull. Chosen
# over Angiospermae because gymnosperms AND angiosperms both bear pollen (the bee
# forage we care about); the few ferns/lycophytes that ride along are harmless and
# get dropped by the flowering/pollen filter downstream. Same philosophy as the
# bee ingest: pull broad at the root, narrow in the clean/analysis layer.
# Sanity-check the id on the first run (the ingest prints it): api taxa/211194.
TAXON_TRACHEOPHYTA <- 211194L
# iNat place for the plant pull. Point Loma Peninsula (132551) FULLY CONTAINS the
# cabr_survey_box (which is hand-drawn to extend past the NPS monument outline),
# so it won't clip edge survey spots; the brain re-filters to the exact box by
# point-in-polygon, discarding the rest. Cabrillo National Monument alone
# (place 4715) is tighter/smaller but risks clipping the box -- kept for reference.
PLACE_POINT_LOMA    <- 132551L
PLACE_CABR_MONUMENT <- 4715L

# ---- Analysis scope: the FAIR WINDOW (journal method-comparison) --------------
# The pipeline feeds TWO papers with different scopes:
#   * JOURNAL paper (lethal vs non-lethal method comparison) -- must compare the two
#     methods on equal footing, so every method-comparison figure restricts to the
#     FAIR WINDOW: survey records only (is_survey), FAIR_MONTHS, FAIR_YEARS, and
#     attributed only (blank/casual surveyor_type dropped). This keeps "non-lethal"
#     to beeple survey photos -- no casual public, no interns' 2024 iNaturalist photos.
#   * NPS CABRILLO REPORT (what bees the park holds) -- wants ALL records, casual
#     public included, so report figures do NOT apply this window.
# Defined ONCE here so the window can't drift across scripts (it did before this).
FAIR_MONTHS <- 3:10          # Mar-Oct: the standardized survey protocol season (interns netted Mar through Oct)
FAIR_YEARS  <- 2021:2023     # the only years the lethal-netting surveys ran (so non-lethal gets no extra years)

# ---- Paper output roots ------------------------------------------------------
# Each analysis script writes its figures + tables STRAIGHT into the paper folder it
# belongs to (no neutral theme folder, no separate collector step). A concept sub-path
# is kept under each root (e.g. .../richness/rarefaction/by_transect) so files stay
# organised and never collide.
#   * REPORT  (all records)  -- the NPS Cabrillo park report.
#   * JOURNAL (fair window)  -- the lethal-vs-non-lethal method paper.
# SPLIT scripts (same concept, two datasets) write their all-records version under
# DIR_REPORT and their fair-window version under DIR_JOURNAL.
# SEASON YEAR. Each season's deliverables live in their own folder so last year's
# outputs stay frozen as the record of what was actually reported. This follows the
# calendar year automatically -- a new season needs NO code edit. To rebuild an older
# season (or to keep working in 2026 after the new year rolls over), set
# BEESCABR_SEASON_YEAR=2026 before running.
beescabr_season_year <- function(override = Sys.getenv("BEESCABR_SEASON_YEAR", ""),
                                 today = Sys.Date()) {
  y <- suppressWarnings(as.integer(override))
  if (!is.na(y) && y > 1900L && y < 2200L) return(y)
  as.integer(format(today, "%Y"))
}
# ONE folder per season. The report and the paper are two framings of the same
# year's data, not two analyses -- 11 scripts write to both -- so they share a tree.
# Journal figures are told apart by "_journal" in the filename, never by folder.
beescabr_analysis_dir <- function(year = beescabr_season_year()) sprintf("data/analysis/%d_generated", year)
beescabr_report_dir  <- beescabr_analysis_dir
beescabr_journal_dir <- beescabr_analysis_dir

BEESCABR_SEASON <- beescabr_season_year()
DIR_REPORT  <- beescabr_report_dir(BEESCABR_SEASON)
DIR_JOURNAL <- beescabr_journal_dir(BEESCABR_SEASON)

# ---- DuckDB cache ------------------------------------------------------------
# One on-disk DuckDB file acts as the cache for BOTH observation objects
# (with a spatial geometry column) and taxon request objects. This replaces
# the retired CSV export entirely and the Python taxon_cache.json.
DB_CACHE_PATH <- "data/inat_observations/cache/inat_cache.duckdb"

# Cached flattened export frame (RDS). Flattening ~77k observations from JSON
# and resolving their taxonomy is slow, so read_observations_export() memoizes
# the result here and only rebuilds when the observation/taxon cache content
# changes (a content fingerprint decides). Delete this file or set
# BEESCABR_REFRESH_FLAT=1 to force a rebuild.
EXPORT_FLAT_CACHE <- "data/inat_observations/cache/export_flat.rds"

# Records when observations were last ingested, so a refresh can also re-pull
# observations that were re-identified/edited on iNaturalist since then
# (updated_since), not just brand-new ones.
INGEST_STATE_PATH <- "data/inat_observations/cache/last_ingest.txt"

# ---- PLANT cache (kept SEPARATE from the bee cache) ---------------------------
# The plant ingest uses its OWN DuckDB file, export RDS, and updated_since state,
# so its incremental id-cursor never collides with the bee cache and a plant pull
# never invalidates the (expensive) 77k-bee export fingerprint. Identical schema
# and code path -- just a different connection + paths (see ingest_plants.R).
DB_CACHE_PATH_PLANT     <- "data/inat_observations/cache/inat_cache_plant.duckdb"
EXPORT_FLAT_PLANT_CACHE <- "data/inat_observations/cache/export_flat_plant.rds"
INGEST_STATE_PATH_PLANT <- "data/inat_observations/cache/last_ingest_plant.txt"

# ---- Observation-field id map ------------------------------------------------
# The flatten step builds `field:<lower(name)>` columns generically from the
# API `ofvs` array (guaranteeing name-parity with the old CSV export). This
# map is the STABLE-ID reference for the handful of fields the pipeline treats
# specially -- names can be edited on iNat, ids cannot. Use these when logic
# must not break on a display-name change.
KNOWN_FIELD_IDS <- list(
  flower_visited      = 3126L,   # datatype "taxon" -> read ofv$taxon$name, NOT ofv$value
  transect_name       = 19447L,  # datatype "text"  -> structured transect (e.g. "TP1")
  tags_override       = 20521L,  # manual survey-hashtag correction
  nest                = 4837L,
  nesting             = 988L
)

# Export column names (post "field:" prefix, lowercased) that get coalesced
# into single tidy columns downstream. Kept here so the mapping lives in one
# place instead of being buried in a mutate() in inat_bee_clean.R.
FLOWER_VISITED_SOURCES <- c(
  "field:interaction->visited flower of",  # authoritative
  "field:name of associated plant",
  "field:nectar / pollen delivering plant",
  "field:nectar plant"
)
NESTING_SOURCES <- c("field:nest", "field:nesting")

# ---- Output paths ------------------------------------------------------------
# Centralized so renames happen in one spot (the old scripts hardcoded these
# inline in many places).
PATHS <- list(
  taxonomy_lookup = "data/reference/sd_bee_taxonomy_lookup_generated.csv",
  # Internal-only: species -> species-complex taxon_id map. complex_taxon_id is
  # an iNat implementation detail (it looks identical to a species scientific
  # name and confuses non-scientists), so it's stripped from the public
  # checklists and parked here for the specimen complex-match step to read.
  complex_map = "data/inat_observations/cache/complex_taxon_id_map.csv",
  # Built ONCE from Holway's v3 checklist (resolving names -> iNat taxon_ids is
  # slow + interactive), then reused every run. Bump the version suffix only when
  # Holway ships a new checklist and you rebuild (BEESCABR_REBUILD_HOLWAY_REF=1).
  holway_reference = "data/reference/holway_sd_bee_reference_table_v3_generated.csv",
  # Your verify/reject decisions. They live in curated/ because a human wrote them:
  # the pipeline appends to them, it does not derive them.
  verified_taxa = "data/reference/hand_curated/verified_taxa.csv",
  rejected_taxa = "data/reference/hand_curated/rejected_taxa.csv",
  specimen_additions = "data/reference/hand_curated/specimen_additions.csv",   # curated specimen-only species merged into the lookup
  plant_taxonomy_lookup = "data/reference/cabr_plant_taxonomy_lookup_generated.csv",       # basic-rank plant lookup (obs + specimen flowers)
  plant_specimen_overrides = "data/reference/hand_curated/plant_specimen_overrides.csv",      # curated expert corrections for specimen-label plants
  plant_not_in_park = "data/reference/generated/cabr_plant_specimen_not_in_park.csv",       # worklist: specimen-label plants not confirmed in park
  plant_name_cache = "data/reference/generated/plant_name_resolution_cache.csv",           # name -> iNat taxon resolution cache
  taxon_changes_review = "data/reference/generated/qc_review_taxon_changes_generated.csv", # yearly sweep: taxa iNaturalist has revised
  holway_answers_version = "data/reference/generated/holway_answers_version.txt",          # which checklist version the saved answers were made against
  plant_all_taxa = "data/inat_observations/reference/cabr_inat_plant_all_taxa_generated.csv",   # ALL in-park plant taxa (any observer) -- in-park truth
  plant_park_confirmed = "data/reference/hand_curated/plant_park_confirmed.csv",               # curated: species the botanist confirms are in the park (e.g. obscured threatened taxa)
  inat_bee_forage = "data/inat_observations/reference/cabr_inat_bee_forage_generated.csv",       # plants bees were recorded foraging on in-park (bee-obs flower_visited) -- in-park truth
# Checklists. These names are the CURRENT ones on disk (the earlier
  # cabr_inat_bee_checklist / *_combined_* / *_v2 keys pointed at filenames that no
  # longer exist and nothing read them -- removed 2026-08-25).
  checklist_cabr_official   = "data/checklists/cabr/cabr_official_native_bee_checklist_generated.csv",
  checklist_cabr_inat   = "data/checklists/cabr/cabr_inat_native_bee_checklist_generated.csv",
  checklist_cabr_specimen   = "data/checklists/cabr/cabr_specimen_native_bee_checklist_generated.csv",
  checklist_pl_inat     = "data/checklists/point_loma/pl_inat_native_bee_checklist_generated.csv",
  checklist_sd_holway       = "data/checklists/sd_county/sd_holway_native_bee_checklist_generated.csv",
  checklist_sd_inat     = "data/checklists/sd_county/sd_inat_native_bee_checklist_generated.csv",
  checklist_sd_holway_inat  = "data/checklists/sd_county/sd_holway_and_inat_native_bee_checklist_generated.csv",
  inat_clean = "data/inat_observations/inat_clean/cabr_inat_bee_clean_generated.csv",
  inat_plant_clean = "data/inat_observations/inat_clean/cabr_inat_plant_clean_generated.csv",
  specimen_clean = "data/specimens/specimens_clean/cabr_specimen_bee_clean_generated.csv",
  holway_combined = "data/reference/source/holway_2026/holway_v3_combined.csv",
  # Project effort + roster. per_survey is the trip-level log (one row per survey
  # trip) -- use it for EFFORT metrics only (trip counts, days, method split), never
  # for headcounts: it stores netters by first name and iNat folks by handle, so the
  # same person appears many times and can't be deduped. surveyor_roster is the
  # canonical people list (one row per person-year, full name + role + method) and is
  # the SOLE authority for WHO surveyed -- count distinct people from here.
  per_survey = "data/project_info/surveys/master_per_survey_info_generated.csv",
  surveyor_roster = "data/project_info/rosters/surveyor_roster.csv",
  # identifier roster: WHO determined each specimen. The raw "determination" code (initials + surname)
  # maps to a person's iNaturalist username here (read-only; the pipeline never writes this file).
  identifier_roster = "data/project_info/rosters/identifier_roster.csv",
  # ONE row per human: identity, every written form of their name, and what they do.
  # Replaces the three rosters above, which each re-stated the same people.
  people = "data/project_info/rosters/people_manual.csv",
  # who was in the field, which year, in what capacity -- GENERATED from the survey record
  participation = "data/project_info/rosters/participation_generated.csv",
  # identifications each person made ON CABRILLO RECORDS -- GENERATED, replaced a
  # hand-typed id_count that drifted. Orders the identifier list on the site.
  identification_counts = "data/project_info/rosters/identification_counts_generated.csv",
  # the brain's per-observation lookup (carries the in_cabr flag)
  inat_raw_membership = "data/inat_observations/inat_raw/cabr_inat_raw_generated.csv",

  # project_info is organised into three jobs, each with its own review/ folder, the same
  # shape inat_observations/ and specimens/ already use: rosters (who), surveys (when and
  # where), crosswalk (the shared vocabulary). master_crosswalk_manual.csv spans BOTH methods --
  # it carries specimen label variants as well as iNat fields -- so it is not an iNat file.
  crosswalk                = "data/project_info/crosswalk/master_crosswalk_manual.csv",
  qc_inat_unknown_tags     = "data/project_info/crosswalk/review/qc_review_mastercrosswalk_inat_unknown_tags_generated.csv",
  qc_inat_unknown_fields   = "data/project_info/crosswalk/review/qc_review_mastercrosswalk_inat_unknown_fields_generated.csv",
  qc_inat_plant_names      = "data/project_info/crosswalk/review/qc_review_mastercrosswalk_plant_names.csv",
  qc_survey_date_windows   = "data/project_info/surveys/review/qc_review_survey_beeple_date_windows_generated.csv",
  qc_survey_transect_ties  = "data/project_info/surveys/review/qc_review_survey_transect_overlap_generated.csv"
)

# Standard ranked-name columns produced from the iNat taxon ancestry, in the
# exact order and naming the retired CSV export used (subtribe added 2026-07;
# subphylum/subclass/suborder/infraorder/epifamily added 2026-07 -- all sourced
# from the iNat ancestry walk, never guessed).
TAXON_RANK_COLUMNS <- c(
  "taxon_kingdom_name", "taxon_phylum_name", "taxon_subphylum_name",
  "taxon_class_name", "taxon_subclass_name",
  "taxon_order_name", "taxon_suborder_name", "taxon_infraorder_name",
  "taxon_superfamily_name", "taxon_family_name", "taxon_epifamily_name",
  "taxon_subfamily_name", "taxon_tribe_name", "taxon_subtribe_name",
  "taxon_genus_name", "taxon_species_name", "taxon_subspecies_name"
)

# The taxonomic hierarchy column order requested for the reference/lookup
# outputs, with metadata columns first. 19 levels (the full sub-rank set).
TAXONOMY_LEVELS <- c(
  "kingdom", "phylum", "subphylum", "class", "subclass",
  "order", "suborder", "infraorder", "superfamily",
  "family", "epifamily", "subfamily", "tribe", "subtribe",
  "genus", "subgenus", "complex", "species", "subspecies"
)

# ---- What each PATHS entry IS (the contract) ---------------------------------
# input    -- a human maintains it by hand (or drops it in). MUST exist before a run;
#             if it is missing the pipeline should stop immediately, not an hour in.
# output   -- the pipeline writes it. MUST exist after a full run; missing means a
#             stage silently produced nothing.
# optional -- a cache or an on-demand reviewer output. Absent is normal.
#
# Why this exists: until 2026-08-25, config.R carried seven checklist entries pointing
# at filenames that had been renamed away. Nothing read them, so nothing complained.
# check_paths() below makes that impossible to repeat.
PATH_KIND <- list(
  # --- inputs: hand-maintained, must exist up front ---
  people                   = "input",   # the one hand-maintained people file
  # RETIRED 2026-09-02: folded into people_manual.csv by build_people_roster.R. Kept optional so
  # the migration can be re-run, but nothing in the pipeline reads them any more.
  surveyor_roster          = "optional",
  identifier_roster        = "optional",
  crosswalk                = "input",
  qc_inat_unknown_tags     = "output",
  qc_inat_unknown_fields   = "output",
  qc_inat_plant_names      = "output",
  qc_survey_date_windows   = "output",
  qc_survey_transect_ties  = "output",
  specimen_additions       = "input",
  plant_specimen_overrides = "input",
  plant_park_confirmed     = "input",
  verified_taxa            = "input",
  rejected_taxa            = "input",
  holway_combined          = "input",
  # --- outputs: the pipeline writes these ---
  per_survey               = "output",   # finding_project_info.R rebuilds it every run
  participation            = "output",   # derived from per_survey: declared identity, derived activity
  identification_counts    = "output",
  inat_raw_membership      = "output",
  taxonomy_lookup          = "output",
  holway_reference         = "output",
  plant_taxonomy_lookup    = "output",
  plant_all_taxa           = "output",
  inat_bee_forage          = "output",
  inat_clean               = "output",
  inat_plant_clean         = "output",
  specimen_clean           = "output",
  checklist_cabr_official  = "output",
  checklist_cabr_inat  = "output",
  checklist_cabr_specimen  = "output",
  checklist_pl_inat    = "output",
  checklist_sd_holway      = "output",
  checklist_sd_inat    = "output",
  checklist_sd_holway_inat = "output",
  # --- optional: caches + on-demand reviewer worklists ---
  complex_map              = "optional",
  plant_not_in_park        = "optional",
  plant_name_cache         = "optional",
  taxon_changes_review     = "output",
  holway_answers_version   = "output"
)

# Report every PATHS entry whose file is missing when the contract says it should be
# there. Returns a data.frame (0 rows = clean) so a caller can stop, warn, or print.
# stage = "input" | "output" | NULL (both). paths/kinds are injectable for testing.
check_paths <- function(paths = PATHS, kinds = PATH_KIND, stage = NULL) {
  keys <- intersect(names(kinds), names(paths))
  if (!is.null(stage)) keys <- keys[vapply(keys, function(k) kinds[[k]] == stage, TRUE)]
  keys <- keys[vapply(keys, function(k) kinds[[k]] != "optional", TRUE)]
  bad  <- keys[!vapply(keys, function(k) file.exists(paths[[k]]), TRUE)]
  data.frame(key  = as.character(bad),
             kind = vapply(bad, function(k) kinds[[k]], ""),
             path = vapply(bad, function(k) paths[[k]], ""),
             row.names = NULL, stringsAsFactors = FALSE)
}
