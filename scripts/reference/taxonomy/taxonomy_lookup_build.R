# =============================================================
# reference/taxonomy/taxonomy_lookup_build.R
# beescabr pipeline -- taxonomy LOOKUP builder (sd_bee_taxonomy_lookup_generated.csv)
# Renamed 2026-07-15 from native_bee_checklist.R.
#
# The old orchestrator (build_all_checklists) built the taxonomy lookup AND the
# Tier 1 / Tier 2 checklists in one pass. The checklist writes were split off to
# legacy_checklists.R (DELETED from the repo; was parked until the new per-source checklist architecture is
# built). This file now builds ONLY the lookup + its internal artifacts.
#
# Exposes build_taxonomy_lookup(con): from a populated observation cache builds
#   - sd_bee_taxonomy_lookup_generated.csv   the taxonomy reference table (the deliverable)
#   - the internal complex map     bare genus/species/complex + complex_taxon_id;
#                                  specimen_bee_clean.R reads it to roll specimen
#                                  IDs up to iNat's complex-level observations
#
# It still BUILDS the SD County checklist in memory -- that checklist is the
# lookup's input, and the complex map is carved from it -- but it does NOT write
# any checklist file. Ingest is the CALLER's job (run_data_cleaning_pipeline.R ingests once for
# the whole run). Running this file directly still works: it ingests, builds, done.
#
# Modules wired here:
#   config.R, pipelines/read_inat.R, reference/holway.R,
#   checklists/checklist_build.R, reference/taxonomy_reference.R,
#   reference/specimen_id_prompt.R (the specimen-addition merge is WIRED IN -- see below)
#
# SPECIMEN ADDITIONS (WIRED IN): the lookup is Holway + iNat + curated specimen-only
# species -- NOT Holway + iNat only. STEP 5 below merges them every build.
#   * WHY  : specimen_bee_clean.R nets real bees the Holway checklist AND the iNat SD obs
#            both miss (species no one photographed). They surface in
#            data/specimens/specimens_clean/review/qc_review_specimen_taxonomy_flags_generated.csv as
#            "genus+species combo not in taxonomy lookup".
#   * WHERE: a curated specimen_additions.csv, MERGED here at build time (STEP 5 via
#            load_specimen_additions + specimen_additions_to_lookup). Do NOT hand-edit
#            sd_bee_taxonomy_lookup_generated.csv -- it is rewritten every run and any manual row is
#            wiped; edit specimen_additions.csv instead.
#   * IDS  : the interactive pass-1 prompt (specimen_id_prompt.R, run_pipeline stage 6c)
#            resolves each addition's iNat taxon_id with full parent lineage and writes it to
#            specimen_additions.csv, which folds in on the NEXT build.
#   * MERGE: specimen_additions_to_lookup() (reference/taxonomy_reference.R) appends leaf taxa
#            and LINKS parents that exist but NEVER fabricates a missing parent -- it returns
#            those in $missing_parents (a missing GENUS is warned at build time).
#
# Run standalone: source("scripts/reference/taxonomy/taxonomy_lookup_build.R")
#   BEESCABR_SKIP_INGEST=1 reuses the cache without hitting the API.
# =============================================================

library(dplyr)
library(stringr)
library(sf)
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("PATHS",                    "config.R")
  need("write_fresh",              "utils/utils.R")
  need("decorate_complex",         "utils/utils.R")
  need("require_columns",          "utils/utils.R")
  need("store_connect",            "inat_observations/engine/db/store_conn.R")
  need("count_observations",       "inat_observations/engine/db/observations_store.R")
  need("taxon_cache_get",          "inat_observations/engine/db/taxon_store.R")
  need("inat_request",             "inat_observations/engine/api/inat_http.R")
  need("flatten_observation",      "inat_observations/engine/api/inat_flatten.R")
  need("resolve_taxonomy",         "inat_observations/engine/api/inat_cache.R")
  need("ingest_observations",      "inat_observations/engine/pipelines/ingest_inat.R")
  need("read_observations_export", "inat_observations/engine/pipelines/read_inat.R")
  need("load_holway",              "reference/taxonomy/holway.R")
  need("build_checklist",          "checklists/checklist_build.R")
  need("build_bee_taxonomy_lookup","reference/taxonomy/taxonomy_reference.R")
  need("resolve_missing_taxon_ids","reference/taxonomy/resolve_missing_ids.R")
  need("apply_manual_overrides",   "reference/prompts/manual_overrides.R")
  need("load_verified_taxa",       "reference/taxonomy/verify.R")
})

# ------------------------------------------------------------
# build_taxonomy_lookup(con): build sd_bee_taxonomy_lookup_generated.csv (+ the internal
# complex map) against a populated cache. Returns an invisible summary list.
# backfill_parent_taxonomy() lives in reference/taxonomy_reference.R (sourced above via need()).
# ------------------------------------------------------------


# ------------------------------------------------------------
# The progress lines this stage prints. PURE, so they can be read and tested without
# building anything. They were written in the project's own shorthand -- "phantoms",
# "ancestor rows", "id-bearing taxa", "obs missing coords -- excluded" -- which tells
# the person who wrote it what happened and tells a supervisor nothing. The only job
# these lines have is letting someone judge a normal run from a bad one.
# ------------------------------------------------------------
.tlb_no_coords <- function(n)
  sprintf(paste("%s record%s had no location on them, so they are left out of anything",
                "mapped. They still count everywhere else."),
          format(n, big.mark = ","), if (n == 1L) "" else "s")

.tlb_complexes <- function(n)
  sprintf(paste("%s look-alike groups: bees iNaturalist cannot tell apart from a photo,",
                "so it files them under a group name instead of a species."),
          format(n, big.mark = ","))

.tlb_reference_base <- function(n_entries, n_ancestors)
  sprintf(paste("%s bees from the county checklist, plus %s rows for the genus, family and",
                "order each one sits in."),
          format(n_entries, big.mark = ","), format(n_ancestors, big.mark = ","))

.tlb_phantoms <- function(n)
  sprintf(paste("%s hand-added bee%s left out: no specimen and no photo backs %s up any",
                "longer. %s come back on their own if one turns up again."),
          n, if (n == 1L) "" else "s", if (n == 1L) "it" else "them",
          if (n == 1L) "It will" else "They will")

.tlb_additions <- function(n)
  sprintf(paste("%s bee%s added by hand from data/reference/hand_curated/specimen_additions.csv",
                "-- collected here but never photographed."),
          n, if (n == 1L) "" else "s")

.tlb_orphan_genus <- function(names) c(
  if (length(names) == 1L)
    "  One hand-added bee cannot be filed: its genus is not in the lookup."
  else
    sprintf("  %s hand-added bees cannot be filed: their genus is not in the lookup.",
            length(names)),
  paste0("    ", names),
  "  Add a row for the genus itself to data/reference/hand_curated/specimen_additions.csv,",
  "  then run the cleaning pipeline again.")

.tlb_subspecies <- function(n) {
  if (n == 0L) return("none to add -- every bee is already at the rank iNaturalist has it at")
  sprintf(paste("%s bee%s moved from species to subspecies, using records already",
                "downloaded -- no extra look-ups"),
          n, if (n == 1L) "" else "s")
}

.tlb_lookup_done <- function(n_rows, n_waiting)
  sprintf("%s bees%s", format(n_rows, big.mark = ","),
          if (n_waiting > 0L)
            sprintf(", %s waiting for someone to confirm they belong in San Diego County",
                    format(n_waiting, big.mark = ",")) else "")

.tlb_offline <- function(n_obs)
  sprintf(paste("Nothing new was downloaded from iNaturalist this run -- the %s records",
                "already on this computer were used. (BEESCABR_SKIP_INGEST=1)"),
          format(n_obs, big.mark = ","))

.tlb_no_reference <- function(f) paste0(
  "The county checklist has not been matched to iNaturalist yet, so there is nothing\n",
  "  to build this lookup on. That happens earlier in the cleaning pipeline, so run\n",
  "  the whole thing rather than this step on its own:\n",
  '    source("scripts/run_data_cleaning_pipeline.R")\n',
  "  It writes ", f)

#' Build the bee taxonomy lookup from the cache and the reference tables
#'
#' Boundaries are sourced lazily, so merely sourcing this file to define the
#' function does not require the spatial data to be on disk.
#'
#' @param con An open DuckDB connection to the observation cache.
#' @return The bee taxonomy lookup, one row per taxon, keyed by `taxon_id`.
build_taxonomy_lookup <- function(con) {
  # Boundaries (reads shapefiles) loaded lazily so merely sourcing this file
  # to define the function does not require the spatial data on disk.
  if (!exists("sd_county_boundary")) source("scripts/spatial/spatial_utils.R")

  bees <- read_observations_export(con)

  # STEP 1: ranked-name columns -> bare names.
  bees <- bees |>
    rename(
      kingdom = taxon_kingdom_name, phylum = taxon_phylum_name,
      class = taxon_class_name, order = taxon_order_name,
      superfamily = taxon_superfamily_name, family = taxon_family_name,
      subfamily = taxon_subfamily_name, tribe = taxon_tribe_name,
      subtribe = taxon_subtribe_name,
      genus = taxon_genus_name, species = taxon_species_name,
      subspecies = taxon_subspecies_name
    )

  # STEP 2: Holway backfill of blank family/subfamily/tribe.
  holway_df <- load_holway(PATHS$holway_combined)
  bees <- backfill_taxonomy(bees, holway_genus_taxonomy(holway_df))

  # STEP 3: SD County spatial subset. SD County is the widest boundary (it
  # contains Point Loma, which contains CABR), so its checklist holds the most
  # taxa and is the natural base for the San Diego taxonomy lookup. Only this
  # tier is needed here -- PL / CABR tiers lived in legacy_checklists.R (since deleted).
  n_missing_coords <- sum(is.na(bees$latitude) | is.na(bees$longitude))
  if (n_missing_coords > 0)
    bx_note(.tlb_no_coords(n_missing_coords))

  bees_sf <- bees |>
    filter(!is.na(latitude), !is.na(longitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
    st_transform(PROJECT_CRS)

  bx_kv("Bee lookup", "picking out the San Diego County bees…")
  bees_sd_county <- spatial_split(bees_sf, sd_county_boundary, "SD County")

  # STEP 4: SD County checklist IN MEMORY -- the lookup's input (never written).
  cl_sd <- build_checklist(bees_sd_county, "SD County")
  taxonomy_lookup <- taxonomy_lookup_from_bees(bees)
  cl_sd <- finalize_checklist(cl_sd, taxonomy_lookup)
  run_qc(cl_sd, "SD County")

  # Internal complex map (bare names + complex_taxon_id) from cl_sd, for the
  # specimen complex-match step. This is the ONLY place complex_taxon_id is
  # persisted -- the public checklists drop it. Written BEFORE decoration so it
  # keeps the bare complex + taxon_id that match_specimen_complex() needs.
  complex_map <- cl_sd |>
    filter(!is.na(complex), complex != "") |>
    distinct(genus, species, complex, complex_taxon_id)
  dir.create(dirname(PATHS$complex_map), recursive = TRUE, showWarnings = FALSE)
  write_fresh(complex_map, PATHS$complex_map, na = "")
  bx_cont(.tlb_complexes(nrow(complex_map)))
  bx_out(PATHS$complex_map)

  # NOTE: the lookup base is Holway + iNat; curated SPECIMEN-ONLY species are then merged
  # in at STEP 5 below (specimen_additions_to_lookup). Broader CABR specimen evidence still
  # lives downstream in the checklists (built from the cleaned specimen record), not here.

  # STEP 5: sd_bee_taxonomy_lookup_generated.csv (with source-membership columns).
  # The enriched Holway reference table (holway_sd_bee_reference_table.csv, built
  # by holway_reference_build.R earlier in run_data_cleaning_pipeline.R) is the BASE of the
  # lookup -- it supplies iNat taxon_ids + scientific names for Holway species,
  # including ones never observed in SD County. The lookup NEVER reads the raw
  # Holway sheet for names.
  verified_ids <- load_verified_taxa(PATHS$verified_taxa)
  if (!file.exists(PATHS$holway_reference))
    stop(.tlb_no_reference(PATHS$holway_reference), call. = FALSE)
  holway_resolved <- readr::read_csv(PATHS$holway_reference, show_col_types = FALSE)
  bx_cont(sum(!is.na(holway_resolved$taxon_id)),
          " of the county checklist's bees already have an iNaturalist number.")
  # The reference table now CONTAINS the ancestor taxa as their own rows (tagged
  # source_sheet == "iNat ancestry"). Split them out: the Holway ENTRIES are the
  # lookup's base (unchanged behavior); the ancestor rows are the id source that
  # gives each parent taxon its own iNat id. The id map is derived from every
  # id-bearing reference row (name at each row's own rank).
  is_ancestry    <- !is.na(holway_resolved$source_sheet) &
                    holway_resolved$source_sheet == "iNat ancestry"
  holway_entries <- holway_resolved[!is_ancestry, , drop = FALSE]
  ancestry_ids   <- ancestry_ids_from_reference(holway_resolved)
  bx_cont(.tlb_reference_base(nrow(holway_entries), sum(is_ancestry)))
  bee_taxonomy_lookup <- build_bee_taxonomy_lookup(holway_entries, cl_sd, bees,
                                                   verified_ids = verified_ids,
                                                   ancestry_ids = ancestry_ids)
  # Merge curated SPECIMEN-ONLY species (data/reference/specimen_additions.csv, e.g. Colletes
  # phaceliae, Lasioglossum daggetti) as new leaf rows -- linked to EXISTING parents, never
  # fabricating a parent. Runs BEFORE id-resolution so the new species' ids/names get filled by the
  # resolver + your overrides just like everything else. A missing parent is reported, not created.
  .adds <- load_specimen_additions(PATHS$specimen_additions)
  # Option A: skip curated additions whose taxon no longer has ANY current specimen or iNat evidence
  # (e.g. a specimen re-ID'd away). specimen_additions.csv stays the master list -- the row silently
  # reactivates if the bee is collected/photographed again. (drop_phantom_additions, taxonomy_reference.R)
  .rd_ev <- function(p) tryCatch(if (!is.null(p) && file.exists(p))
                                   utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE) else NULL,
                                 error = function(e) NULL)
  .n0 <- nrow(.adds)
  .adds <- drop_phantom_additions(.adds, .rd_ev(PATHS$specimen_clean), .rd_ev(PATHS$inat_clean))
  if (nrow(.adds) < .n0)
    bx_cont(.tlb_phantoms(.n0 - nrow(.adds)))
  if (nrow(.adds)) {
    .merged <- specimen_additions_to_lookup(bee_taxonomy_lookup, .adds)
    bee_taxonomy_lookup <- .merged$lookup
    bx_cont(.tlb_additions(nrow(.merged$added)))
    # Only a MISSING GENUS orphans a species; higher lineage ranks lacking a standalone lookup row
    # is normal (the lookup stores genus-and-below), so those are not flagged.
    .mp_g <- .merged$missing_parents[.merged$missing_parents$missing_parent_rank == "genus", , drop = FALSE]
    if (nrow(.mp_g)) {
      for (ln in .tlb_orphan_genus(.mp_g$scientific_name)) message(ln)
    }
  }
  # Fill any STILL-missing taxon_ids by iNaturalist name-search (Holway-only taxa never observed in
  # SD, so no observation ancestry carried their id). Safe: assigns only unambiguous rank+name+parent
  # matches, everything else stays blank; cached to resolved_missing_ids.csv (auditable, no re-hits).
  bee_taxonomy_lookup <- tryCatch(resolve_missing_taxon_ids(bee_taxonomy_lookup),
    error = function(e) { message("  (resolve_missing_taxon_ids skipped: ", conditionMessage(e), ")"); bee_taxonomy_lookup })
  # Your recorded answers (manual_taxon_overrides.csv) win over the automated search: fill the id +
  # correct the name for renamed / synonym taxa. Then write the review worklist -- the prompt listing
  # whatever is STILL missing an id (one list; it already includes the Holway taxa) for you to look up.
  bee_taxonomy_lookup <- apply_manual_overrides(bee_taxonomy_lookup)
  # INTERACTIVE: ask you for the ids the auto-search couldn't find (appends to manual_taxon_overrides.csv),
  # then re-apply so anything you enter fills THIS run's lookup. Non-interactive (Rscript) -> skipped,
  # and the worklist file below is the fallback.
  prompt_missing_taxon_ids()
  bee_taxonomy_lookup <- apply_manual_overrides(bee_taxonomy_lookup)
  write_review_worklist()   # the still-open not_found set (after your answers) -> the "look these up" file
  # re-apply the verified memory: rows appended AFTER build_bee_taxonomy_lookup (the specimen-additions
  # merge) missed the verified_ids pass, so a taxon you verified would otherwise re-ask every run.
  bee_taxonomy_lookup <- apply_verified_ids(bee_taxonomy_lookup, verified_ids)
  # in_cabr_specimens is appended last by the specimen-additions merge; move it up beside in_holway.
  if (all(c("in_cabr_specimens", "in_holway") %in% names(bee_taxonomy_lookup)))
    bee_taxonomy_lookup <- dplyr::relocate(bee_taxonomy_lookup, in_cabr_specimens, .after = in_holway)
  # LAST STEP: backfill blank ancestor ranks (specimen-only leaves can arrive missing intermediate
  # ranks their genus/family rows already carry). Adds no taxa; only fills gaps.
  # Promote the subspecies iNaturalist already gave us. Every cached taxon record
  # carries its children, so this reads what is on disk -- no API call, and a species
  # with no cache entry is skipped. Before this, a specimen keyed to a subspecies had
  # no node to match even when iNat had published one (Colletes hyalinus gaudialis,
  # 345235, sat in the cache unused while 60 specimens went unresolved).
  bee_taxonomy_lookup <- tryCatch({
    if (!exists("subspecies_from_cache")) source("scripts/reference/taxonomy/subspecies_from_cache.R")
    if (!exists("taxon_cache_get"))       source("scripts/inat_observations/engine/db/taxon_store.R")
    if (!exists("store_connect"))         source("scripts/inat_observations/engine/db/store_conn.R")
    con <- store_connect(read_only = TRUE)          # DB_CACHE_PATH, the bee cache
    on.exit(try(store_disconnect(con), silent = TRUE), add = TRUE)
    fetch <- function(id) {
      rec <- taxon_cache_get(con, paste0("id:", id))
      if (is.null(rec)) NULL else if (!is.null(rec$results)) rec$results[[1]] else rec
    }
    before <- nrow(bee_taxonomy_lookup)
    out <- subspecies_from_cache(bee_taxonomy_lookup, fetch)
    bx_kv("Subspecies", .tlb_subspecies(nrow(out) - before))
    out
  }, error = function(e) {
    bx_note("Could not fill in subspecies from the records already downloaded, so any ",
            "subspecies stay at species level. The rest of the run is unaffected. (",
            conditionMessage(e), ")")
    bee_taxonomy_lookup })

  bee_taxonomy_lookup <- backfill_parent_taxonomy(bee_taxonomy_lookup)
  write_fresh(decorate_complex_name(decorate_complex(bee_taxonomy_lookup)), PATHS$taxonomy_lookup, na = "")
  bx_kv("Bee lookup", .tlb_lookup_done(nrow(bee_taxonomy_lookup),
                                      sum(!bee_taxonomy_lookup$verified)))
  bx_out(PATHS$taxonomy_lookup)

  invisible(list(lookup = nrow(bee_taxonomy_lookup)))
}

# ------------------------------------------------------------
# Standalone entrypoint (skipped when sourced by run_data_cleaning_pipeline.R).
# ------------------------------------------------------------
if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) {
  main <- function() {
    con <- store_connect()
    on.exit(store_disconnect(con), add = TRUE)
    if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") != "1") ingest_observations(con)
    else bx_note(.tlb_offline(count_observations(con)))
    build_taxonomy_lookup(con)
  }
  main()
}
