# =============================================================
# run_publishing_materials_pipeline.R
# beescabr pipeline -- STAGE 3: PUBLISHING MATERIALS (the public GitHub Pages site)
#
# The three pipeline stages, each its own runner:
#   1. run_data_cleaning_pipeline.R      ingest iNat -> DuckDB cache -> cleaned tables
#   2. run_all_analysis_pipeline.R       every figure + table + report/journal HTML
#   3. run_publishing_materials_pipeline.R   <-- THIS: build + publish the public site
#
# This stage re-renders ONLY the pages that go PUBLIC (field guides, park summary,
# least-sampled, the transect + bee-bounty maps, and the occurrence explorer), then
# copies them into docs/ and rebuilds the landing page (via scripts/website/). The
# publish modules live in scripts/website/, the way cleaning lives in scripts/clean/
# and analysis in scripts/analysis/.
#
#   source("scripts/run_publishing_materials_pipeline.R")                     # rebuild docs/ (then git push to deploy)
#   Sys.setenv(BEESCABR_DEPLOY = "1"); source("scripts/run_publishing_materials_pipeline.R")   # also commit + push docs/ (auto-deploy)
#
# Assumes the analysis data is current -- run stages 1 + 2 first.
# =============================================================

# shared modules the public HTML scripts rely on (they also self-source these; loading
# once up front keeps things tidy + fast).
source("scripts/config.R")
source("scripts/analysis/shared/theme_beescabr.R")
source("scripts/analysis/shared/utils_analysis.R")
source("scripts/analysis/shared/plant_names.R")
source("scripts/analysis/shared/conservation_status.R")
source("scripts/analysis/shared/forage_selectivity.R")
source("scripts/analysis/shared/not_on_holway.R")

# the analysis scripts that emit a PUBLIC html page. ADD NEW PUBLIC PAGES HERE
# (and add the matching row to PUBLISH_PAGES in scripts/website/publish_pages.R).
PUBLIC_PAGES <- c(
  "bee_field_guide.R", "bee_field_guide_genus.R", "nps_summary_tables.R",
  "least_sampled_bees.R", "bee_bounties.R", "transect_map.R",
  "bee_occurrence_explorer.R")

# refresh basemap: clear the cached tiles so the static transect map redraws with CURRENT
# tiles on every publish (the interactive Leaflet maps load theirs live in the browser)
unlink("data/spatial/basemap_tiles", recursive = TRUE)

# FIRST BUILD: say what GitHub Pages needs before spending five minutes rendering.
# Shown only when docs/ holds no site yet, so an established setup never sees it.
if (!exists("website_setup_notice")) source("scripts/website/setup_notice.R")
local({
  remote <- tryCatch(system2("git", c("remote", "get-url", "origin"),
                             stdout = TRUE, stderr = FALSE)[1], error = function(e) "")
  if (is.na(remote)) remote <- ""
  notice <- website_setup_notice(has_site = file.exists("docs/index.html"), remote = remote)
  if (!is.null(notice)) message(paste(notice, collapse = "\n"))
})

message("==> STAGE 3: regenerating public report HTML")
ok <- vapply(PUBLIC_PAGES, function(nm) {
  message("    ", nm)
  # PUBLIC_PAGES holds FILE NAMES, not paths: scripts/analysis/ is foldered by
  # topic, and a page script moving between topics must not break this list.
  hit <- list.files("scripts/analysis", pattern = paste0("^", nm, "$"),
                    recursive = TRUE, full.names = TRUE)
  if (length(hit) != 1L) {
    message("      !! ", if (!length(hit)) "NOT FOUND" else "AMBIGUOUS", " under scripts/analysis/")
    return(FALSE)
  }
  tryCatch({ source(hit); TRUE },
           error = function(e) { message("      !! FAILED: ", conditionMessage(e)); FALSE })
}, logical(1))
# A page that did not rebuild would be published STALE, silently. Stop instead:
# the site is the one output the public sees, and a half-built one is worse than
# none. (This is the same swallow-and-continue that hid a broken specimen stage
# for a week.)
if (any(!ok))
  stop("These public pages failed to rebuild: ", paste(PUBLIC_PAGES[!ok], collapse = ", "),
       "\nFix them before publishing -- docs/ would otherwise go out with stale pages.",
       call. = FALSE)

message("\n==> Publishing into docs/")
pub <- tryCatch(system2("Rscript", "scripts/website/publish_pages.R", stdout = TRUE, stderr = TRUE),
                error = function(e) conditionMessage(e))
message(paste(pub, collapse = "\n"))

# publish_pages.R refuses an empty or a stale docs/ by printing "STOPPING: ..." and
# returning FALSE -- it does not stop(), so Rscript exits 0 either way. Nothing here
# used to check: the refusal scrolled past, "Site rebuilt in docs/" was announced on
# top of it, and with BEESCABR_DEPLOY=1 the half-built folder was committed and pushed
# live. Same reasoning as the page-rebuild stop above -- a half-built public site is
# worse than none. Both refusals already print the commands that fix them, so this
# only has to be loud and stop.
if (!exists("publish_run_failed")) source("scripts/website/publish_pages.R")
if (publish_run_failed(pub))
  stop("The site was NOT published -- docs/ and the live site are unchanged.",
       "\n  The reason is in the output just above, with the commands that fix it.",
       call. = FALSE)

if (identical(Sys.getenv("BEESCABR_DEPLOY"), "1")) {
  message("\n==> Deploying (BEESCABR_DEPLOY=1)")
  system2("git", c("add", "docs/"))
  if (system2("git", c("diff", "--cached", "--quiet")) != 0L) {   # non-zero = there ARE staged changes
    system2("git", c("commit", "-m", "Rebuild published site (docs/)"))
    system2("git", c("push", "origin", "main"))
    message("    deployed -- GitHub Pages will update in ~1 minute.")
  } else message("    no site changes to deploy.")
} else {
  message("\nSite rebuilt in docs/. To deploy:")
  message("    git add docs/ && git commit -m 'Rebuild site' && git push")
  message("  (or re-run with BEESCABR_DEPLOY=1 to do that automatically).")
}
