# =============================================================
# scripts/run_all_analysis_pipeline.R
# Regenerate EVERY analysis figure + table with the current house palette,
# WITHOUT re-running ingest/cleaning (which is the slow part). It just reads the
# already-cleaned tables and rewrites the outputs in data/analysis/.
#
# Run from the repo ROOT:
#   source("scripts/run_all_analysis_pipeline.R")     # in an R console
#   source("scripts/run_all_analysis_pipeline.R")        # or from a terminal
#
# Any single script that errors is reported and skipped -- the rest still run,
# and the failures are listed at the end.
# =============================================================

# shared modules first: paths, palette, helpers (the figure scripts also
# self-source these, but loading them once up front keeps things tidy + fast).
source("scripts/config.R")
source("scripts/analysis/shared/theme_beescabr.R")
source("scripts/analysis/shared/utils_analysis.R")
source("scripts/analysis/shared/not_on_holway.R")
source("scripts/analysis/shared/conservation_status.R")   # shared IUCN / conservation lookups (one source)
source("scripts/analysis/shared/plant_names.R")           # shared plant-genus common-name labels (one source)
source("scripts/analysis/shared/forage_selectivity.R")    # shared bee-genus forage selectivity (one source)

# IUCN Red List status (per bee species) and plant-genus common names are no longer refreshed
# here -- they are now BAKED INTO THE CLEANED TABLES + CHECKLISTS at data-cleaning time
# (scripts/reference/refresh/enrich_lookups.R, called by the *_clean.R scripts + cabr_bee_checklist.R,
# fetched once and cached). This run stays fully OFFLINE and just reads those columns/caches.
# To force-refresh existing IUCN assessments or common names, run the pipeline with the refresh
# flag (Sys.setenv(BEESCABR_REFRESH = "1"); source("scripts/run_data_cleaning_pipeline.R")) -- or the two tools directly:
#   source("scripts/reference/refresh/refresh_iucn_status.R")  |  source("scripts/reference/refresh/refresh_plant_common_names.R")
RUNNING_ALL <- TRUE

# HELPERS, not analyses: other scripts source them, so the auto-loop must not run
# them as if they wrote figures. test-analysis-modules.R derives this list from the
# actual source() calls and fails if the two disagree -- it had already drifted by
# three files, which is why the check exists.
.modules <- c("theme_beescabr.R", "utils_analysis.R", "not_on_holway.R",
              "conservation_status.R", "plant_names.R", "forage_selectivity.R",
              "explorer_photo_helpers.R", "inat_taxon_links.R", "transect_years.R", "inext_estimates.R", "rarefaction_names.R", "folder_readmes.R",
              "rarefaction_combined.R",   # runs AFTER both rarefaction scripts, not in the loop
              "findings_summaries.R")   # runs LAST (after every analysis) -- excluded from the auto-loop
# analysis/ is foldered by topic, mirroring data/analysis/. Recurse, and compare
# against .modules by BASENAME so the module list stays a list of file names
# rather than a second copy of the folder layout.
.scripts <- sort(list.files("scripts/analysis", pattern = "\\.R$", recursive = TRUE))
.scripts <- .scripts[!basename(.scripts) %in% .modules]

# source each in the global env; a formal-arg closure keeps `nm` safe even if a
# sourced script reuses common variable names. run_analysis_script() attributes each
# script's warnings TO that script (R would otherwise pool them all and print them,
# nameless, after the run) and fails the script on an unknown-column warning, which
# always means a real bug rather than a style nit. See scripts/utils/analysis_run.R.
if (!exists("run_analysis_script")) source('scripts/utils/analysis_run.R')
.res <- lapply(.scripts, function(nm) {
  message("\n===== ", nm, " =====")
  r <- run_analysis_script(nm)
  if (!r$ok) message("  !! FAILED: ", r$error)
  if (length(r$warnings))
    message("  ! ", length(r$warnings), " warning(s): ",
            paste(unique(r$warnings)[1:min(2, length(unique(r$warnings)))], collapse = " | "))
  r
})
.ok     <- lapply(.res, function(r) r$ok)
.failed <- .scripts[!unlist(.ok)]

# LAST: roll up every analysis into plain-language <name>_findings.csv tables +
# a master findings_index.csv (data/analysis/findings_generated/). Runs after the loop so it
# can read the fresh per-analysis outputs it summarises. Best-effort like the rest.
message("\n===== rarefaction_combined.R (one figure per comparison, both estimators) =====")
tryCatch(source("scripts/analysis/richness/rarefaction_combined.R"),
         error = function(e) message("  rarefaction_combined failed: ", conditionMessage(e)))

message("\n===== findings_summaries.R (plain-language finding rollups) =====")
tryCatch(source("scripts/analysis/findings_summaries.R"),
         error = function(e) message("  !! findings rollup skipped: ", conditionMessage(e)))

message("\n---------------------------------------------")
message(analysis_tally(.scripts, .res))
# The tally names the scripts that warned; this says what they actually said.
for (.l in analysis_warning_report(.scripts, .res)) message(.l)
# The season folder, not data/analysis/ -- that holds only this folder and two notes.
.out <- normalizePath(DIR_REPORT, mustWork = FALSE)
message("")
message("WHERE THE RESULTS ARE")
message("  Every figure, table and page this run wrote is under:")
message("    ", .out)
message("")
message("  Start here -- it is the map to all of them:")
message("    ", file.path(.out, "findings_index.csv"))
message("  One row per analysis: what it asked, the answer in plain English, and the")
message("  file holding the numbers. Opens in Excel.")
message("")
message("  Each topic folder also has a WHAT_THESE_FILES_ARE.txt saying what is in it")
message("  and which statistical method is behind its numbers.")
message("")
message("  Nothing has been published. These files are on this computer only until")
message("  you run the publishing stage.")
# The project is run from R, not a shell: every stage is source()d from the console,
# so the hint is written the way it will actually be typed.
message("Next stage: publish the public site with")
message('    source("scripts/run_publishing_materials_pipeline.R")')
