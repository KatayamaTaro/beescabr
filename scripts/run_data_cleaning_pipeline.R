# =============================================================
# run_data_cleaning_pipeline.R
# beescabr -- STAGE 1 of 3: ingest + clean.  source() this file; it asks the rest.
#
#   1.  INGEST    iNat API -> DuckDB cache (incremental)
#   2.  EXPORT    flatten the cache -> export_flat.rds
#   2b. PLANTS    same again for vascular plants -> export_flat_plant.rds
#   3.  BRAIN     finding_project_info(): who surveyed, when, on which transect
#   3b. REVIEW    walk unknown tags + fields, survey-date windows, transect ties
#                 (interactive; skipped on an unattended run)
#   4.  CLEAN     cabr_inat_bee_clean_generated.csv
#   5.  CHECKLISTS Holway reference -> taxonomy lookup -> per-source checklists
#
# Ingest runs ONCE; every later stage reads that same cache.
#
# HOW TO RUN
#   source("scripts/run_data_cleaning_pipeline.R")
#
# It opens a menu -- normal / bees only / offline / full rebuild -- and sets every
# flag itself from your answer.
#
# DO NOT set BEESCABR_SKIP_INGEST or BEESCABR_FULL_INGEST by hand. A flag already
# set is read as "the operator knows what they are doing" and SKIPS the menu without
# saying so, and an env var survives the whole R session -- which is how a leftover
# FULL_INGEST=1 turned every later run into a 40-minute rebuild. The menu exists to
# end that. To clear a stuck flag: Sys.unsetenv("BEESCABR_FULL_INGEST").
#
# RECOVERY HATCHES -- deliberately NOT in the menu, each does damage if used casually:
#   BEESCABR_SKIP_PLANTS=1        bees only, plants left STALE. A failing plant pull
#                                 stops the run on purpose; re-run first, most failures
#                                 are transient. Every forage result uses old plants.
#   BEESCABR_REFRESH=1            re-check IUCN status + plant common names against the
#                                 live APIs. Normally unnecessary: new taxa are kept
#                                 current incrementally, and anything a year old is
#                                 refreshed automatically.
#   BEESCABR_REBUILD_HOLWAY_REF=1 re-resolve 700+ Holway names to iNat taxa. Slow and
#                                 interactive. The result is versioned and reused every
#                                 run; rebuild only when Holway ships a new checklist.
#   BEESCABR_NONINTERACTIVE=1     answer no prompts (for a scripted run).
# =============================================================

# Prevent the stage scripts' own standalone entrypoints from firing when we
# source them -- this runner owns the ingest and the connection.
BEESCABR_SOURCED_BY_RUNNER <- TRUE
# We are a runner: the optional refresh tools (below) should NOT trigger their own
# standalone field-guide rebuild -- the analysis run handles that.
RUNNING_ALL <- TRUE

source("scripts/utils/ingest_mode.R")   # the run-mode menu (asks instead of requiring flag names)
source("scripts/utils/refresh_due.R")   # "is the cached IUCN / plant-name reference stale?"
source("scripts/config.R")
source("scripts/utils/utils.R")
source("scripts/utils/console.R")                # phase banners + key/value reporter + NEEDS-YOU rollup
source("scripts/inat_observations/engine/db/store_conn.R")
source("scripts/inat_observations/engine/db/observations_store.R")
source("scripts/inat_observations/engine/db/taxon_store.R")
source("scripts/inat_observations/engine/db/decision_store.R")
source("scripts/inat_observations/engine/db/compact_store.R")
source("scripts/inat_observations/engine/api/inat_http.R")
source("scripts/inat_observations/engine/api/inat_flatten.R")
source("scripts/inat_observations/engine/api/inat_cache.R")
source("scripts/inat_observations/engine/pipelines/ingest_inat.R")
source("scripts/inat_observations/engine/pipelines/read_inat.R")
source("scripts/inat_observations/engine/pipelines/ingest_plants.R")  # plant pull -> separate cache -> export_flat_plant.rds
source("scripts/inat_observations/build_field_id_map.R")             # defines build_field_id_map() -- stage 2c
# reference/ (holway.R, taxonomy_reference.R, verify.R) is pulled in by taxonomy_lookup_build.R (stage 5).
source("scripts/spatial/spatial_utils.R")          # boundaries, PROJECT_CRS (once)
source("scripts/project_info/surveys/finding_beeple_calendar.R")  # defines finding_beeple_calendar() -- stage 2d
source("scripts/project_info/finding_project_info.R")     # THE brain: provenance + unknowns + survey_dates
source("scripts/project_info/review/qc_review_mastercrosswalk.R")         # interactive review of unknown tags + fields
source("scripts/project_info/review/qc_review_survey_windows.R")           # interactive review of survey-date windows
source("scripts/inat_observations/clean/inat_bee_clean.R")           # defines inat_bee_clean() -- stage 7 (clean, taxonomy-filled)
source("scripts/inat_observations/clean/inat_plant_clean.R")         # defines inat_plant_clean() -- stage 8 (surveyors' plant table)
source("scripts/inat_observations/review/qc_review_inat_location_maps.R") # defines build_location_review_maps() -- stage 7d (per-observer maps)
source("scripts/inat_observations/clean/bee_forage.R")               # defines write_bee_forage() -- stage 5b2 (bee-obs forage plants)
source("scripts/inat_observations/review/qc_review_inat_misid.R")         # defines inat_misid_qc() -- stage 10 (misID review queue)
# ---- TAXONOMY + SPECIMENS + CHECKLISTS ----
# Both the interactive Holway->iNat resolver AND the non-interactive lookup builder run in
# the pipeline now (stages 4 + 5); they pull their own deps (holway.R, taxonomy_reference.R,
# verify.R, checklist_build.R) via need().
source("scripts/reference/prompts/manual_overrides.R")        # apply_manual_overrides / write_review_worklist (name-change fixes)
source("scripts/reference/taxonomy/holway_reference_build.R")  # defines build_holway_reference() -- stage 4 (interactive)
source("scripts/reference/taxonomy/holway_version.R")          # "is there a newer checklist, and what changed?"
source("scripts/reference/taxonomy/taxonomy_lookup_build.R")   # defines build_taxonomy_lookup() -- stage 5
source("scripts/reference/prompts/verify_prompt.R")           # defines prompt_verify_taxa() -- pass-2 verification
source("scripts/reference/taxonomy/plant_lookup_join.R")           # attach_flower_ids() -- flower taxon_id + in_park
source("scripts/reference/taxonomy/plant_taxonomy_lookup_build.R") # defines build_plant_taxonomy_lookup() -- stage 5c
source("scripts/project_info/collect_plant_names.R")      # defines review_plant_names() -- stage 7b
source("scripts/specimens/specimen_clean_helpers.R")          # pure specimen-cleaning helpers
source("scripts/specimens/specimen_bee_clean.R")      # defines clean_specimens() -- stage 6b
source("scripts/specimens/specimen_raw_worklist.R")      # defines tidy_raw_specimens() -- stage 6a raw worklist
source("scripts/reference/prompts/specimen_id_prompt.R")      # defines resolve_specimen_taxa() -- stage 6c interactive taxon-id resolve
# CHECKLISTS (stage 9): normalized-tree builder + the 3 per-scope orchestrators. Sourced here so
# the whole pipeline runs end-to-end with NO manual steps (cabr_inat / cabr_specimen / cabr_official /
# pl_inat / sd_holway / sd_inat / sd_holway_and_inat -- parent taxa as their own rows).
source("scripts/checklists/checklist_build.R")        # spatial_split / lookup_subtree / combine_checklists
source("scripts/checklists/cabr_bee_checklist.R")     # defines build_cabr_bee_checklists()
source("scripts/checklists/pl_bee_checklist.R")       # defines build_pl_bee_checklists()
source("scripts/checklists/sd_bee_checklist.R")       # defines build_sd_bee_checklists()
source("scripts/analysis/shared/not_on_holway.R")            # not_on_holway_bees() + format_new_bees() -- stage 11

main <- function() {
  # ---- ASK, don't make anyone remember flag names. Interactive runs get a plain
  # menu; scripted runs and deliberately-preset flags skip it untouched. Every flag
  # is rewritten from the answer, so a value left over from an earlier session in
  # this same R process cannot leak into this run. ----
  ingest_mode_apply(ingest_mode_flags())

  # ---- Whose iNaturalist account is this run pulling as? ----
  # Credentials are PERSONAL and are never shipped with the code or the data. Ask once,
  # here, rather than deep inside the pull. Unauthenticated is allowed but returns
  # OBSCURED coordinates for sensitive taxa, so the operator is told which they are getting.
  if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") != "1") {
    if (!exists("inat_auth_prompt")) source("scripts/inat_observations/engine/api/inat_auth.R")
    inat_auth_prompt()
    .who <- tryCatch(inat_auth_login(), error = function(e) "")
    message("")
    if (nzchar(.who)) message("  iNaturalist: signed in as @", .who,
                              " -- true coordinates for taxa this account is trusted with.")
    else message("  iNaturalist: not signed in -- sensitive taxa will have OBSCURED coordinates.")
  }

  # ---- Is the cached reference data (IUCN status, plant common names) stale? Age comes
  # from each cache's own retrieved_on dates, oldest entry first. Anything past a year is
  # refreshed AUTOMATICALLY at phase 0 below -- nobody should have to remember a flag once
  # a year. An offline run cannot, and says so instead. ----
  .ages    <- refresh_ages()
  .overdue <- Filter(function(x) isTRUE(x$due), .ages)
  .offline <- Sys.getenv("BEESCABR_SKIP_INGEST", "0") == "1"
  # Say how old every cache is, every run -- not only once something has already been
  # stale for a year. "checked 8 months ago" is information; the same fact withheld
  # until it becomes a prompt is a surprise.
  message("")
  bx_cont("Reference data, last checked against the live sources:")
  for (ln in refresh_age_lines(.ages)) message(ln)
  if (length(.overdue)) {
    message("")
    for (o in .overdue) bx_note("stale reference: ", o$key, " -- ", o$reason)
    if (.offline) {
      bx_cont("this is an offline run, so they cannot be refreshed now.")
      bx_cont("re-run online (menu option 1) and you will be asked about refreshing.")
      bx_need(sprintf("%d reference cache(s) over a year old", length(.overdue)),
              "run online to refresh")
    }
    message("")
  }
  # Ask before spending minutes online. Declining keeps the cache and the run continues
  # normally -- new taxa are still picked up incrementally, as on every run.
  .refresh_ok <- if (.offline) FALSE else refresh_confirm(.overdue)
  if (length(.overdue) && !.offline && !.refresh_ok)
    bx_need(sprintf("%d reference cache(s) over a year old (you chose not to refresh)",
                    length(.overdue)), "BEESCABR_REFRESH=1 when ready")

  t0 <- Sys.time()

  # ---- PRE-FLIGHT: every hand-maintained input must exist BEFORE we spend an hour
  # on the API. PATH_KIND in config.R declares which entries are inputs. ----
  .missing_in <- check_paths(stage = "input")
  if (nrow(.missing_in)) {
    message("")
    bx_rule()
    message("  STOPPING: ", nrow(.missing_in), " required input file(s) are missing.")
    for (i in seq_len(nrow(.missing_in)))
      message(sprintf("    %-24s %s", .missing_in$key[i], .missing_in$path[i]))
    message("\n  These are maintained by hand -- see dev-docs/MANUAL_INPUTS.md.")
    bx_rule()
    stop("missing required input(s); nothing was run", call. = FALSE)
  }
  con <- store_connect()
  on.exit(if (!is.null(con)) store_disconnect(con), add = TRUE)

  bx_need_reset()

  # ---- 0. OPTIONAL REFRESH (flag-gated, ONLINE) ----
  # Force a full re-check of IUCN status + plant common names against the live APIs and rewrite
  # their caches, so the CLEAN TABLES phase below re-bakes the fresh values. OFF by default (a
  # normal run stays offline). Reads the species/genus universe from the existing cleaned tables,
  # so on a first-ever run it is a harmless no-op. Wrapped so an API/network failure never kills
  # the run. See scripts/reference/refresh_{iucn_status,plant_common_names}.R.
  # Runs when FORCED (BEESCABR_REFRESH=1) or when a cache has simply aged out. Never on an
  # offline run -- there is no network to ask. A failure keeps the existing cache and the
  # run continues: stale reference values beat no run at all.
  .forced <- Sys.getenv("BEESCABR_REFRESH", "0") == "1"
  .aged   <- isTRUE(.refresh_ok)          # aged out AND the operator said yes
  if (.forced || .aged) {
    bx_phase(0, "REFRESH IUCN + PLANT NAMES (online)")
    bx_kv("Refresh", if (.forced) "forcing a full re-check against the live APIs (BEESCABR_REFRESH=1)…"
                     else sprintf("%d cache(s) past %d days — re-checking against the live APIs…",
                                  length(.overdue), REFRESH_MAX_AGE_DAYS))
    tryCatch(source("scripts/reference/refresh/refresh_iucn_status.R"),
             error = function(e) bx_note("IUCN refresh failed (", conditionMessage(e), ") — kept the existing cache."))
    tryCatch(source("scripts/reference/refresh/refresh_plant_common_names.R"),
             error = function(e) bx_note("plant-name refresh failed (", conditionMessage(e), ") — kept the existing cache."))
  }

  bx_phase(1, "SETUP & FETCH")

  # ---- 1. INGEST (once) ----
  if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") == "1") {
    bx_kv("Fetch bees", "cache: ", count_observations(con), " observations (API pull skipped)")
  } else {
    bx_kv("Fetch bees", "pulling new + edited observations from iNaturalist…")
    ingest_observations(con, incremental = Sys.getenv("BEESCABR_FULL_INGEST", "0") != "1")
  }

  # ---- 2. EXPORT: refresh export_flat.rds (the brain reads it directly) ----
  invisible(read_observations_export(con))

  # ---- 2b. PLANTS: ingest vascular plants -> export_flat_plant.rds (separate cache) ----
  # Pulls Point Loma vascular plants into their OWN DuckDB cache and refreshes
  # export_flat_plant.rds, which the brain reads ALONGSIDE the bee export. Serves
  # BOTH survey-day confirmation (a surveyor's plant obs on a plant-only day) and
  # flower-resource analysis. Own connection; the bee `con` above is untouched.
  # Gated by the same ingest flags as bees (+ BEESCABR_SKIP_PLANTS to skip only plants).
  if (Sys.getenv("BEESCABR_SKIP_PLANTS", "0") == "1") {
    # NOT a normal way to run. Bees pull fresh while plants stay stale, so survey-day
    # confirmation and every forage/flower result rest on old plant data. Deliberately
    # LOUD -- this is a recovery hatch for a broken plant pull, not a speed option, and
    # it is not offered in the run menu.
    bx_kv("Fetch plants", "SKIPPED (BEESCABR_SKIP_PLANTS=1)")
    bx_note("plant data is now STALE while bee data is fresh -- this run is NOT a")
    bx_cont("complete refresh. Survey-day confirmation and all forage/flower results")
    bx_cont("use old plant records. Unset the flag and re-run for a consistent result.")
    bx_need("Plant pull skipped -- results rest on stale plants", "BEESCABR_SKIP_PLANTS=1")
  } else {
    # Free the bee export frame first: building the plant export (flatten + taxonomy
    # of 40k obs) while the 77k-bee frame was still resident OOM'd R on the first
    # plant run (2026-07-17). The on-disk export_flat.rds is untouched; the later
    # checklist stage re-reads it from disk (cache hit) if it needs it.
    clear_export_cache()
    # A plant failure STOPS the run, on purpose. Catching it here would keep going and
    # produce figures, checklists and eventually a published site built on stale plant
    # data -- and the warning would only ever exist in terminal scrollback, never on the
    # outputs themselves. Most plant failures are transient (503, timeout), so the right
    # response is usually just to re-run. If the pull is genuinely broken and the bee
    # side is needed now, that is a DELIBERATE call: set BEESCABR_SKIP_PLANTS=1, which
    # warns loudly about exactly what becomes stale. Same reasoning as the bee ingest.
    ingest_plants(
      incremental = Sys.getenv("BEESCABR_FULL_INGEST", "0") != "1",
      do_ingest   = Sys.getenv("BEESCABR_SKIP_INGEST", "0") != "1"
    )
  }

  # ---- 2c. FIELD MAP: refresh the obs-field name -> iNat id map from the cache ----
  # Keeps data/inat_observations/reference/inat_field_id_map_generated.csv (the crosswalk's stable-id
  # reference) in step with freshly-ingested fields. Cheap DuckDB query; skipped when ingest
  # was skipped and the map already exists. Reuses the bee `con` (2b used its own plant con).
  FIELD_MAP_PATH <- "data/inat_observations/reference/inat_field_id_map_generated.csv"
  if (Sys.getenv("BEESCABR_SKIP_INGEST", "0") != "1" || !file.exists(FIELD_MAP_PATH)) {
    build_field_id_map(con = con)
  }  # else: silently reused (ingest skipped and the map already exists)

  # ---- 2d. COMPACT: hand dead space back to the filesystem ----
  # DuckDB never shrinks a file on its own. Every incremental ingest rewrites rows
  # and leaves the old copies behind, so the cache only grows -- the bee cache had
  # reached 31 GB holding 78,578 observations, and rewriting it gave 7.9 GB with
  # every row intact. Asking how much dead space a file carries is instant, so this
  # checks after every ingest and only rewrites when it is worth it (most runs do
  # nothing). NOTHING IS PRUNED: it copies every row and value, just compactly.
  # Both connections must be closed first -- the copy needs the file to itself.
  store_disconnect(con); con <- NULL
  for (.p in c(DB_CACHE_PATH, DB_CACHE_PATH_PLANT))
    db_compact_if_needed(.p, say = function(...) bx_cont(...))
  con <- store_connect(DB_CACHE_PATH)

  # ---- 2d. BEEPLE CALENDARS: (re)build beeple_calendar_windows_generated.csv from the PDFs ----
  # Re-parses every "YYYY Cabrillo Bee Survey Calendar.pdf" in
  # data/project_info/surveys/survey_date_sources/beeple_calendar_windows/ each run, so a newly-added year
  # (e.g. 2027) is picked up automatically. The brain reads the resulting windows CSV.
  # Wrapped so a missing pdftools / malformed PDF warns and keeps the existing CSV
  # rather than killing the run.
  tryCatch(finding_beeple_calendar(), error = function(e)
    bx_note("calendar parse failed (", conditionMessage(e),
            ") — kept the existing beeple_calendar_windows_generated.csv."))

  # ---- 3. BRAIN: provenance + unknown tags/fields/notes + survey_dates ----
  # finding_project_info() decides survey membership from master_crosswalk_manual.csv, writes
  # cabr_inat_raw_generated.csv, the THREE unknown reports (review them by
  # hand in order -> update crosswalk_master -> re-run), and builds master_per_survey_info_generated.csv
  # (+ the beeple review queue). Taxonomy-blind, so it needs no Holway/lookup.
  bx_phase(2, "SURVEY BRAIN — who surveyed, when, which transect")
  finding_project_info()

  # ---- 3b. REVIEW: sort unknown tags + fields into crosswalk_master (interactive) ----
  # The brain just wrote the unknown reports; walk through tags then fields (the
  # same review you'd run by hand), then re-run the brain so survey membership +
  # survey_dates reflect what you filed. Auto-skips in non-interactive / scheduled
  # runs (or force-skip in RStudio with BEESCABR_NONINTERACTIVE=1).
  .n_rows <- function(p) if (file.exists(p)) nrow(readr::read_csv(p, show_col_types = FALSE)) else 0L
  .n_windows <- function(p) {
    if (!file.exists(p)) return(0L)
    d <- readr::read_csv(p, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
    if (!"decision" %in% names(d)) return(nrow(d))
    sum(is.na(d$decision) | trimws(d$decision) == "" | tolower(trimws(d$decision)) == "unsure")
  }
  if (interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1") {
    n_tags <- .n_rows(FPI_UNKNOWN_TAGS); n_fields <- .n_rows(FPI_UNKNOWN_FIELDS)
    bx_kv("Review", n_tags, " unknown tags · ", n_fields, " unknown fields to sort")
    if (n_tags   > 0) review_unknowns("tags")
    if (n_fields > 0) review_unknowns("fields")
    if (n_tags > 0 || n_fields > 0) {
      bx_cont("re-running the brain to apply your crosswalk edits…")
      finding_project_info()
    }

    # [3d] SURVEY-DATE WINDOWS -- heads-up queue of planned windows with no tagged
    # survey nearby (possible missed surveys). Runs LAST so it sees every tag + field.
    # Ruling a window is for YOUR records only -- it never adds a survey date (nothing
    # is hand-added; no tag = not a survey day), so there is NO brain re-run after it.
    n_win <- .n_windows(FPI_REVIEW)
    if (n_win > 0) {
      bx_kv("Windows", n_win, " planned window(s) with no tagged survey nearby — rule them")
      review_windows()
    }

    # [3e] TRANSECT TIES -- equal-split survey days the resolver couldn't call by
    # majority (a beeple's obs tagged evenly across two transects). Rule which transect
    # the day really was, or "both". If you rule any, the brain re-runs right below so the
    # chosen transect lands in master_per_survey_info_generated.csv THIS run (your master truth).
    n_ties <- .n_windows(FPI_TIES)   # reuse the blank/unsure "still to rule" counter
    if (n_ties > 0) {
      bx_kv("Ties", n_ties, " equal-split day(s) to rule")
      review_transect_ties()
      if (.n_windows(FPI_TIES) < n_ties) {   # a tie was ruled -> apply it now, not next run
        bx_cont("re-running the brain to apply your tie ruling(s)…")
        finding_project_info()
      }
    } else {
      bx_kv("Ties", "none to rule")
    }
  } else {
    bx_kv("Review", "skipped (non-interactive) — run qc_review_mastercrosswalk.R / qc_review_survey_windows.R by hand")
  }

  # ---- 4. HOLWAY REFERENCE (interactive: resolves Holway -> iNat; prompts as needed) ----
  # Rebuilds holway_sd_bee_reference_table_v3_generated.csv EVERY run so any Holway change is caught.
  # Decisions are cached (holway_decisions) so a normal run mostly replays them -- you're only
  # prompted for the unresolved-"Described" second pass + anything new. Wrapped so an abort or
  # failure keeps the existing table and never kills the run.
  bx_phase(3, "TAXONOMY")
  bx_kv("Holway table", "matching the SD bee checklist names to iNaturalist…")
  # A newer checklist dropped into data/reference/source/ is NOT picked up on its own:
  # the version is written into two config.R keys, and moving to it changes which bees
  # the whole county tier is built from. Silently reading v3 while a v4 sits beside it
  # is the failure this catches -- nothing else in the pipeline would ever mention it.
  local({
    files <- list.files(HOLWAY_SOURCE_DIR, recursive = TRUE, full.names = TRUE)
    notice <- holway_version_notice(holway_newer_than(files, PATHS$holway_combined),
                                    PATHS$holway_combined)
    if (length(notice)) for (ln in notice) message(ln)
  })
  # A CHECKLIST VERSION BUMP WIPES THE SAVED ANSWERS.
  #
  # Not for tidiness. A surviving NAME is not evidence the bee survived unchanged:
  # Holway can keep a spelling and change what it applies to -- a split where the old
  # name stays with one of the pieces -- and nothing in the string reveals that. The
  # answer is silently reused and is now about a different bee.
  #
  # Re-deriving is cheap and loud: ~700 names are re-searched (the 40 minutes the
  # rebuild menu already warns about) and only the couple of dozen the matcher cannot
  # settle come back as questions -- which are exactly the ambiguous bees where a
  # concept shift is most likely. Keeping them is free and silent, and silent is the
  # one this project does not accept: "a wrong taxon_id is worse than none."
  #
  # Every answer is written out first. A version bump is triggered by a person editing
  # two paths in config.R, and doing that by mistake must not cost a year of work.
  local({
    v_now <- holway_version_of(PATHS$holway_combined)
    vf    <- PATHS$holway_answers_version
    v_was <- if (file.exists(vf))
      suppressWarnings(as.integer(readLines(vf, warn = FALSE)[1])) else NA_integer_
    if (holway_version_bumped(v_now, v_was)) {
      backup <- file.path(dirname(vf),
                          sprintf("holway_answers_v%d_%s.csv", v_was, format(Sys.Date())))
      n_saved <- tryCatch(decision_export(con, backup), error = function(e) NA_integer_)
      n_gone  <- tryCatch(decision_forget_all(con), error = function(e) 0L)
      for (ln in holway_version_bump_notice(v_was, v_now, n_gone,
                                            if (is.na(n_saved)) NA_character_ else backup))
        message(ln)
    }
    if (!is.na(v_now)) {
      dir.create(dirname(vf), recursive = TRUE, showWarnings = FALSE)
      writeLines(as.character(v_now), vf)
    }
  })

  # Answers for bees that have left the checklist are never read again -- the build
  # below loops over the names in the CURRENT sheet -- but they pile up across
  # versions. Dropping them changes no output and costs no re-resolution: nothing was
  # going to look them up. Guarded on the checklist being readable, because this is a
  # DELETE driven by a file read.
  local({
    hdf <- tryCatch(load_holway(PATHS$holway_combined), error = function(e) NULL)
    if (is.null(hdf)) return(invisible(NULL))
    terms <- tryCatch(mapply(holway_search_term, hdf$source_sheet, hdf$genus, hdf$species_raw,
                             USE.NAMES = FALSE),
                      error = function(e) character(0))
    gone <- tryCatch(forget_orphan_decisions(con, terms), error = function(e) character(0))
    if (length(gone)) for (ln in orphan_decisions_note(gone)) bx_cont(ln)
  })
  tryCatch({
    .hdf <- load_holway(PATHS$holway_combined)
    .ref <- build_holway_reference(con, .hdf,
              interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1")
    write.csv(.ref, PATHS$holway_reference, row.names = FALSE, na = "")
    bx_out(basename(PATHS$holway_reference), " (", nrow(.ref), " rows)")
  }, error = function(e) bx_note("Holway reference failed (", conditionMessage(e),
                                 ") — kept the existing table."))

  # ---- 5. TAXONOMY LOOKUP ----
  # Reads the Holway reference table (step 4) + the cache, writes sd_bee_taxonomy_lookup_generated.csv
  # (+ the internal complex map). Wrapped so a taxonomy failure never kills the run.
  if (file.exists(PATHS$holway_reference)) {
    tryCatch(build_taxonomy_lookup(con),
             error = function(e) bx_note("taxonomy lookup failed: ", conditionMessage(e)))
    # PASS 2 -- VERIFY new-to-Holway taxa: prompt to confirm each is a real ID (not a misID) and
    # record it in verified_taxa.csv so it stops being flagged. Interactive only; never kills the run.
    tryCatch({
      if (interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1" && file.exists(PATHS$taxonomy_lookup)) {
        .lkv <- suppressMessages(utils::read.csv(PATHS$taxonomy_lookup, stringsAsFactors = FALSE, check.names = FALSE))
        prompt_verify_taxa(.lkv, interactive_ok = TRUE)
      }
    }, error = function(e) bx_note("verification prompt failed: ", conditionMessage(e)))
  } else {
    bx_kv("Bee lookup", "skipped — no Holway reference table")
  }

  # ---- 5b. PLANT CLEAN (moved up so the plant lookup + flower-id joins below can use it) ----
  bx_phase(4, "CLEAN TABLES")
  tryCatch(inat_plant_clean(),
           error = function(e) bx_note("plant clean failed: ", conditionMessage(e)))

  # ---- 5b2. BEE FORAGE (plants bees were recorded on in-park -> in-park truth for the lookup) ----
  tryCatch(write_bee_forage(),
           error = function(e) bx_note("bee forage failed: ", conditionMessage(e)))

  # ---- 5c. PLANT TAXONOMY LOOKUP (genus+species tree: crosswalk canonicals + broad obs + bee forage) ----
  tryCatch(build_plant_taxonomy_lookup(verbose = TRUE),
           error = function(e) bx_note("plant lookup failed: ", conditionMessage(e)))

  # ---- 6. SPECIMENS (lethal-survey record) ----
  # 6a. Raw hygiene worklist (non-ID'd / missing / duplicate rows to fix by hand).
  # 6b. Clean -- taxon_id + taxonomy from the lookup (step 5), transect, visited plant ->
  #     cabr_specimen_bee_clean_generated.csv (mirrors the iNat bee schema).
  tryCatch(tidy_raw_specimens(),
           error = function(e) bx_note("raw specimen worklist failed: ", conditionMessage(e)))
  tryCatch(clean_specimens(interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"),
           error = function(e) bx_note("specimen clean failed: ", conditionMessage(e)))

  # 6c. RESOLVE unknown specimen taxa -- for each bee flagged as "not in the taxonomy lookup",
  #     prompt for its iNaturalist id (with a suggested match) and fold it into specimen_additions.csv
  #     WITH full parent lineage, so it enters the lookup on the NEXT build and stops re-flagging.
  #     Interactive only; wrapped so it never kills the run.
  tryCatch({
    if (interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1") {
      .sprec <- suppressMessages(readxl::read_excel(read_latest(SBC_RECORDS_DIR, SBC_RECORDS_PATTERN)))
      resolve_specimen_taxa(.sprec, interactive_ok = TRUE)
    }
  }, error = function(e) bx_note("specimen taxon-id resolve failed: ", conditionMessage(e)))

  # ---- 7. CLEAN: labeled iNat BEE table (taxonomy filled from the lookup) ----
  # Reads the brain's cabr_inat_raw_generated.csv, joins coords + taxon_id, fills taxonomy from the
  # lookup (step 5) by taxon_id, re-marks Humphreys Rd walk-ins. AFTER the lookup so its
  # taxonomy columns are populated.
  tryCatch(inat_bee_clean(),
           error = function(e) bx_note("bee clean failed: ", conditionMessage(e)))

  # ---- 7b. PLANT NAMES: review any NEW plant name not yet in the crosswalk ----
  # After the cleaners so specimen flower labels exist; files your decisions into master_crosswalk
  # for the next run (non-interactive runs just drop a worklist).
  tryCatch(review_plant_names(interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"),
           error = function(e) bx_note("plant-name review failed: ", conditionMessage(e)))

  # ---- 7c. OBSERVATION REVIEW: prompt for the iNat obs that need fixing ON iNaturalist ----
  # cabr_inat_bee_fix_behavior.csv (wrong/missing flower field) + qc_review_inat_mistagged_transects_generated.csv
  # (stray transect tag) + the bee/plant location_review files (survey pins far from any transect).
  # Each row carries the observation's url, so you open it and fix it there; the next iNat pull picks
  # up your fix. Non-blocking: surfaces + prompts, then continues.
  # (The off-transect LOCATION pins are NOT listed here -- they are the whole subject of
  # stage 7d below, which surfaces them AND builds the per-surveyor maps in one place, so
  # they aren't split across two prompts.)
  tryCatch({
    obs_rev   <- "data/inat_observations/review"
    obs_items <- data.frame(
      label = c(.review_labels_inat(), "stray transect tags"),
      count = c(.n_rows(file.path(obs_rev, "qc_review_inat_bee_behavior_survey_generated.csv")),
                .n_rows(file.path(obs_rev, "qc_review_inat_bee_behavior_nonsurvey_generated.csv")),
                .n_rows(file.path(obs_rev, "qc_review_inat_mistagged_transects_generated.csv"))),
      file  = c("qc_review_inat_bee_behavior_survey_generated.csv", "qc_review_inat_bee_behavior_nonsurvey_generated.csv", "qc_review_inat_mistagged_transects_generated.csv"),
      what  = c(.review_what_inat(),
                paste("The observation carries a transect tag that disagrees with the",
                      "transect the rest of that surveyor's day was on. Usually a typo in",
                      "the tag; open the URL and correct it.")),
      stringsAsFactors = FALSE)
    resolve_review_gate(obs_items, obs_rev,
                        interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1",
                        fix_hint = "iNaturalist", blocking = FALSE)
  }, error = function(e) message("  [7c] observation review FAILED (non-fatal): ", conditionMessage(e)))

  # ---- 7d. LOCATION REVIEW: the off-transect survey pins (bee + plant) that sit >50 m from
  # any transect. This is the SINGLE place they surface: it reports the counts, builds one
  # self-contained iNaturalist "pins to fix" map per surveyor (next to the two location_review
  # CSVs + the shared instruction page, under review/location/), and prompts you to send them.
  # The per-pin survey-log annotation is computed from the master, so tag-only intern days
  # (e.g. 2024-05-05) label correctly. ----
  bx_phase(5, "SURVEYOR MAPS")
  tryCatch(build_location_review_maps(),
           error = function(e) bx_note("location maps failed: ", conditionMessage(e)))

  # ---- 9. CHECKLISTS: cabr / pl / sd native-bee checklists (normalized tree from the lookup) ----
  # Each checklist carries parent taxa as their own rows (taxon_id/taxon_rank/names/taxonomy from the
  # lookup), like Holway. iNat lists clip the RAW bee export to each boundary; the specimen + Holway
  # subtrees come from cabr_specimen_bee_clean_generated.csv + the Holway reference. Runs LAST (needs stages 5-8).
  bx_phase(6, "CHECKLISTS & QC")
  tryCatch({
    .lk <- suppressMessages(readr::read_csv(PATHS$taxonomy_lookup, show_col_types = FALSE)) |>
             dplyr::mutate(taxon_id = as.character(taxon_id))
    .bees_sf <- readRDS(EXPORT_FLAT_CACHE) |>
             dplyr::filter(!is.na(latitude), !is.na(longitude)) |>
             sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
             sf::st_transform(PROJECT_CRS)
    .spec <- if (file.exists(PATHS$specimen_clean))   suppressMessages(readr::read_csv(PATHS$specimen_clean,   show_col_types = FALSE)) else NULL
    .href <- if (file.exists(PATHS$holway_reference)) suppressMessages(readr::read_csv(PATHS$holway_reference, show_col_types = FALSE)) else NULL
    .hsub <- if (!is.null(.href)) lookup_subtree(.lk, .href, "SD Holway") else NULL   # built once, shared
    build_cabr_bee_checklists(.bees_sf, .lk, specimens = .spec, holway_sub = .hsub)
    build_pl_bee_checklists(.bees_sf, .lk)
    build_sd_bee_checklists(.bees_sf, .lk, holway_sub = .hsub)
    bx_out("data/checklists/{cabr, point_loma, sd_county}/  (7 files)")
  }, error = function(e) bx_note("checklists failed: ", conditionMessage(e)))

  # ---- 10. MISID QC: flag likely-misidentified iNat bee obs for review (advisory) ----
  # Species-level iNat IDs that are non-research-grade, unvouchered by a specimen, AND
  # absent from Holway -> a review queue for a human to verify on iNaturalist. Needs the
  # Holway reference + specimen table (built above), so it runs LAST. Changes nothing else.
  tryCatch(inat_misid_qc(),
           error = function(e) bx_note("misID QC failed: ", conditionMessage(e)))

  # ---- 11. NEW BEES NOT ON HOLWAY: review prompt (any rank) ----
  # CABR official-checklist taxa with holway == FALSE that ALSO have iNat records -> "new" bees
  # (genuine park/county additions OR misIDs). Advisory: a human opens each on iNaturalist to confirm
  # a trusted scientist ID'd it. Unlike stage 10 (species-rank only), this catches complex/genus-rank
  # finds too. Same set the analysis script reports to the park; here it's the "double-check" prompt.
  tryCatch({
    .chk_cabr <- PATHS$checklist_cabr_official
    if (file.exists(.chk_cabr) && file.exists(PATHS$specimen_clean) && file.exists(PATHS$inat_clean)) {
      .spec_nb <- utils::read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
      .inat_nb <- utils::read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
      .noh <- not_on_holway_bees(.chk_cabr, .spec_nb, .inat_nb, holway_path = PATHS$holway_reference)
      .newbees <- .noh[.noh$group %in% c("inat_only", "inat_and_collected"), , drop = FALSE]
      if (nrow(.newbees)) {
        writeLines(format_new_bees(.noh, mode = "review"))
        dir.create("data/inat_observations/review", showWarnings = FALSE, recursive = TRUE)
        write.csv(.newbees[, c("scientific_name", "taxon_rank", "group", "n_inat_records",
                               "n_inat_research_grade", "n_specimen_records", "taxon_id")],
                  "data/inat_observations/review/qc_review_inat_new_bees_not_on_holway_generated.csv", row.names = FALSE, na = "")
        bx_out("data/inat_observations/review/qc_review_inat_new_bees_not_on_holway_generated.csv")
      } else bx_kv("New bees", "none — every CABR bee with iNat records is on Holway")
    } else bx_kv("New bees", "skipped (need the CABR checklist + cleaned specimen/iNat tables)")
  }, error = function(e) bx_note("new-bees review failed: ", conditionMessage(e)))

  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  # ---- NEEDS-YOU rollup: collect the run's action items from the review artifacts on disk,
  # so a returning user sees everything that wants their attention in ONE place at the end. ----
  .maps_dir <- "data/inat_observations/review/location/by_surveyors"
  .n_maps <- if (dir.exists(.maps_dir)) length(list.files(.maps_dir, pattern = "^cabr_pins_to_fix_.*\\.html$")) else 0L
  if (.n_maps > 0) bx_need(sprintf("Send %d surveyors their maps", .n_maps), "review/location/by_surveyors/")
  .n_tax <- .n_rows("data/reference/generated/cabr_taxon_ids_needs_review.csv")
  if (.n_tax > 0) bx_need(sprintf("%d bee names need an iNat id", .n_tax), "cabr_taxon_ids_needs_review.csv")
  .n_dupe <- .n_rows("data/specimens/specimens_clean/review/qc_review_specimen_duplicates_generated.csv")
  if (.n_dupe > 0) bx_need(sprintf("%d duplicate specimen IDs", .n_dupe), "qc_review_specimen_duplicates_generated.csv")

  message("")
  bx_rule()
  message(sprintf("  ✓ DONE in %s min      cache: %s observations", dt, count_observations(con)))
  message("")
  bx_need_print()
  bx_analysis_files()
  message("\n  Everything else is written under  data/")
  bx_rule()
}

# ---- analysis-ready file manifest (printed at the end of every run) -----------
# The curated set of outputs meant for downstream analysis, grouped by kind.
# Any that aren't on disk are flagged (not found), so a failed/skipped stage
# shows up at a glance. Paths are the config.R / stage constants, verbatim.
bx_analysis_files <- function() {
  grp  <- function(label) message("    ", label, ":")
  item <- function(path) message("      · ", path, if (!file.exists(path)) "   (not found)" else "")
  message("")
  message("  FILES FOR ANALYSIS")
  grp("checklists")
  item(PATHS$checklist_cabr_official)
  item(PATHS$checklist_cabr_inat)
  item(PATHS$checklist_cabr_specimen)
  grp("cleaned observations")
  item(PATHS$inat_clean)
  item(PATHS$inat_plant_clean)
  grp("survey record")
  item(PATHS$per_survey)
  item(PATHS$participation)
  item(PATHS$people)
  grp("reference / lookups")
  item(PATHS$taxonomy_lookup)
  item(PATHS$plant_taxonomy_lookup)
  item(PATHS$holway_reference)
  grp("specimens")
  item(PATHS$specimen_clean)
  grp("spatial / boundaries")
  item("data/spatial/shapefiles/boundaries/cabr/cabr_survey_box.shp")
  item("data/spatial/shapefiles/boundaries/cabr/nps_official/cabr_boundary_nps_official.shp")
  item("data/spatial/shapefiles/boundaries/point_loma/point_loma_boundary.shp")
  item("data/spatial/shapefiles/boundaries/san_diego_county/sd_county_boundary.shp")
  item("data/spatial/shapefiles/transects/cabr_bee_transects.shp")
  item("data/spatial/shapefiles/access_routes_to_transects/cabr_survey_access_routes.shp")
}

main()
