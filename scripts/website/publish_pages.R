# =============================================================
# website/publish_pages.R
# beescabr pipeline -- PUBLISH: copy the PUBLIC report HTML pages into docs/
# (tracked) so GitHub Pages can serve them, build the Acknowledgements page, and
# (re)build the docs/index.html landing page.
#
# The generated report HTML lives under data/analysis/ (gitignored), so this
# script copies the chosen public pages into docs/ with clean, stable filenames.
# Run it after the pipeline whenever the pages change:
#     source("scripts/website/publish_pages.R")
# then commit docs/ and push. GitHub Pages source = main branch, /docs folder.
#
# This is the R replacement for the old publish_pages.sh -- same docs/ output,
# no shell. Only the PUBLIC report set is published here; internal QA tools
# (per-surveyor pins-to-fix pages, reviewer drafts) are deliberately excluded.
# =============================================================

# Follow the season year from config.R so a new season needs no edit here. Set
# BEESCABR_SEASON_YEAR=2026 to publish an older season.
if (!exists("DIR_REPORT")) source('scripts/config.R')
if (!exists("beescabr_fill_colors")) source('scripts/analysis/shared/theme_beescabr.R')  # palette is the only colour source
IUCN_VERSION_FILE <- "data/checklists/iucn/iucn_redlist_version_generated.txt"  # written by the IUCN refresh
SRC_DIR  <- DIR_REPORT
DOCS_DIR <- "docs"

# One manifest drives both the file copy and the landing-page cards.
# ADD NEW PUBLIC PAGES HERE (src is relative to SRC_DIR; out is the docs/ name).
PUBLISH_PAGES <- list(
  list(src = "reference/occurrence_map/website/bee_occurrence_explorer.html", out = "occurrence_explorer.html",
       title = "Bee Occurrence Explorer", icon = "\U0001F50E", tag = "Field guide",
       blurb = "Pick a bee and see every place in the park it has been recorded, on a map you can filter by year, transect, and method, along with the months the bee is active."),
  list(src = "reference/field_guide/website/bee_field_guide_species.html", out = "field_guide_species.html",
       title = "Bee Field Guide (Species)", icon = "\U0001F41D", tag = "Field guide",
       blurb = "Every bee species recorded at Cabrillo, with conservation status, how often we find it, the flowers it visits, and a link to its photos on iNaturalist."),
  list(src = "reference/field_guide/website/bee_field_guide_genus.html", out = "field_guide_genus.html",
       title = "Bee Field Guide (Genus)", icon = "\U0001F41D", tag = "Field guide",
       blurb = "The same guide organized by genus. Useful for every bee, and especially for the ones that can only be identified to genus."),
  list(src = "reference/bee_plant/website/bee_plant_explorer.html", out = "bee_plant_explorer.html",
       title = "Bee and Plant Explorer", icon = "\U0001F33C", tag = "Field guide",
       blurb = "Which flowers each bee has been recorded on, and which bees have been recorded on each plant. Read it from either end, and check the record count before drawing a conclusion."),
  list(src = "coverage/least_sampled/website/least_sampled_bees.html", out = "least_sampled_bees.html",
       title = "Least-Sampled Bees", icon = "❗", tag = "Priorities",
       blurb = "The bees we know the least about across both survey methods. New records of these help the most."),
  list(src = "coverage/bee_bounties/website/specimen_bee_bounty_map.html", out = "specimen_bounty_map.html",
       title = "Specimen Bee Bounty Map", icon = "\U0001F52C", tag = "Priorities",
       blurb = "Where to net the specimens the park's collection is still missing, bee by bee."),
  list(src = "coverage/bee_bounties/website/inaturalist_bee_bounty_map.html", out = "inaturalist_bounty_map.html",
       title = "iNaturalist Bee Bounty Map", icon = "\U0001F4F7", tag = "Priorities",
       blurb = "Where to photograph the bees that iNaturalist is still missing for the park."),
  list(src = "reference/transects/website/cabr_bee_transects_map.html", out = "transects_map.html",
       title = "Survey Transect Map", icon = "\U0001F5FA\UFE0F", tag = "Monitoring program",
       blurb = "The four fixed trails our surveyors walk, known as the transects, mapped across the park."),
  list(src = "reference/nps_summary/website/nps_summary_tables.html", out = "summary_tables.html",
       title = "Park Summary Tables", icon = "\U0001F4CA", tag = "Monitoring program",
       blurb = "The program at a glance. How many species, genera, and plants we have recorded, and the number of surveys and surveyors behind them."),
  list(src = "phenology/website/bee_trends_explorer.html", out = "bee_trends.html",
       title = "Bee Trends Explorer", icon = "\U0001F4C8", tag = "Monitoring program",
       blurb = "An early look at how each bee's share of the records moves year to year. Six seasons in, so treat every line as a first look, not a conclusion.")
)

# ---- guard: never publish an EMPTY site --------------------------------------
# The landing page is rebuilt from the pages found on THIS run, not from what is
# already in docs/. So if the season folder is missing or empty -- which is exactly
# the state on 1 January, before the new year's analysis has been run -- a publish
# would copy nothing and then rebuild index.html with zero cards. Every page would
# still be live, but the front door would be blank.
#
# So: found nothing -> change nothing. The existing site keeps working until there is
# genuinely something new to replace it with.
publish_would_empty_site <- function(found) length(found) == 0L

publish_empty_message <- function(src_dir) paste0(
  "No published pages were found in ", src_dir, ".\n",
  "  That folder is this season's analysis output, and it looks like the analysis\n",
  "  has not been run for this year yet.\n\n",
  "  Nothing was changed -- docs/ is left untouched and the live site is unchanged.\n\n",
  "  Run the \"run_all_analysis_pipeline.R\" first, then publish:\n",
  "    source('scripts/run_all_analysis_pipeline.R')\n",
  "    source('scripts/run_publishing_materials_pipeline.R')\n\n",
  "  (To publish an EARLIER season instead, set BEESCABR_SEASON_YEAR, e.g.\n",
  "   Sys.setenv(BEESCABR_SEASON_YEAR = '2026'); source('scripts/run_publishing_materials_pipeline.R'))")

# ---- freshness guard --------------------------------------------------------
# run_all_analysis_pipeline.R runs all 36 analysis scripts best-effort: one that fails
# is reported in a tally at the END of the run and the loop continues -- leaving its
# PREVIOUS output on disk. Publishing is a separate command, so that tally can scroll
# away unread and last season's page goes live under this season's heading, with nothing
# on the site to show it. Compare times rather than trusting the tally was read.
# PURE: takes mtimes (seconds), not paths, so it is testable without touching a clock.
publish_stale_pages <- function(out_times, src_time, tol_secs = 0) {
  if (!length(out_times) || is.null(names(out_times))) return(character(0))
  if (length(src_time) != 1L || is.na(src_time)) return(character(0))   # unknown source: never block
  stale <- !is.na(out_times) & out_times < (src_time - tol_secs)
  names(out_times)[stale]
}

publish_stale_message <- function(stale) paste0(
  "These pages are OLDER than the cleaned data they are built from:\n",
  paste0("    - ", stale, collapse = "\n"), "\n\n",
  "  That usually means the analysis script that writes one of them FAILED on the\n",
  "  last run, so the file on disk is from an earlier run. Publishing now would put\n",
  "  outdated numbers on the live site.\n\n",
  "  Nothing was changed -- docs/ is left untouched and the live site is unchanged.\n\n",
  "  Re-run the analysis, check the failure tally it prints at the end, then publish:\n",
  "    source('scripts/run_all_analysis_pipeline.R')\n",
  "    source('scripts/run_publishing_materials_pipeline.R')\n\n",
  "  (To publish anyway -- you know these pages are fine as they are:\n",
  "   Sys.setenv(BEESCABR_ALLOW_STALE = '1'); source('scripts/run_publishing_materials_pipeline.R'))")

# mtime in seconds, or NA when the file is absent -- keeps the pure function clock-free.
publish_mtime <- function(path) {
  if (!length(path) || is.na(path) || !file.exists(path)) return(NA_real_)
  as.numeric(file.info(path)$mtime)
}

# Landing cards grouped by section: pages sharing a tag sit together, sections in
# first-appearance order, manifest order kept within a section -- so new pages can
# be appended to PUBLISH_PAGES without disordering the landing grid.
group_pages_by_tag <- function(pages) {
  if (!length(pages)) return(pages)
  tags <- vapply(pages, `[[`, "", "tag")
  pages[order(match(tags, unique(tags)))]
}

# Fill {{token}} placeholders in a template. gsub(fixed=TRUE) so literal % in the
# CSS is left alone (sprintf would choke on it).
.fill <- function(tpl, ...) {
  v <- list(...)
  for (nm in names(v)) tpl <- gsub(paste0("{{", nm, "}}"), v[[nm]], tpl, fixed = TRUE)
  tpl
}

# One landing-page card. Leading newline + indent matches the old heredoc exactly.
.CARD_TPL <- beescabr_fill_colors('
      <a class="card" href="./{{out}}{{ver}}">
        <div class="card-head"><span class="icon">{{icon}}</span><span class="tag">{{tag}}</span></div>
        <h2>{{title}}</h2>
        <p>{{blurb}}</p>
        <span class="go">Open<span class="arrow">&rarr;</span></span>
      </a>')

# Full landing page. {{cards}} / {{herov}} / {{date}} are filled in; everything
# else is verbatim the page the pipeline has always shipped.
.PAGE_TPL <- beescabr_fill_colors('<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Cabrillo National Monument &mdash; Native Bee Monitoring Program</title>
<style>
  :root {
    --bg:__W_BG__; --bg2:__W_BG2__; --fg:__W_FG__; --muted:__W_MUTED__;
    --card:__W_CARD__; --border:__W_BORDER__; --accent:__W_ACCENT__; --accent-deep:__W_ACCENTD__; --accent-soft:__W_ACCENTS__;
    --shadow:0 1px 2px rgba(24,50,26,.05), 0 8px 24px rgba(24,50,26,.07);
    --shadow-hover:0 2px 6px rgba(24,50,26,.1), 0 16px 40px rgba(24,50,26,.14);
  }
  @media (prefers-color-scheme: dark) {
    :root {
      --bg:__D_BG__; --bg2:__D_BG2__; --fg:__D_FG__; --muted:__D_MUTED__;
      --card:__D_CARD__; --border:__D_BORDER__; --accent:__D_ACCENT__; --accent-deep:__D_ACCENTD__; --accent-soft:__D_ACCENTS__;
      --shadow:0 1px 2px rgba(0,0,0,.3), 0 10px 30px rgba(0,0,0,.35);
      --shadow-hover:0 2px 8px rgba(0,0,0,.4), 0 20px 50px rgba(0,0,0,.5);
    }
  }
  * { box-sizing:border-box; }
  body { margin:0; color:var(--fg); line-height:1.55;
         background:linear-gradient(180deg,var(--bg) 0%,var(--bg2) 100%);
         background-attachment:fixed; min-height:100vh;
         font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
         -webkit-font-smoothing:antialiased; }
  .hero { position:relative; overflow:hidden; background:__W_HEAD__; border-bottom:1px solid var(--border); }
  .hero-bg { position:absolute; inset:0; z-index:0; width:100%; height:100%;
             object-fit:cover; object-position:52% 46%; transform:none; }
  .hero::after { content:""; position:absolute; inset:0; z-index:1; pointer-events:none;
                 background:linear-gradient(90deg, rgba(12,24,14,.74) 0%, rgba(12,24,14,.5) 42%, rgba(12,24,14,.32) 100%),
                            linear-gradient(180deg, rgba(12,24,14,.12) 0%, rgba(12,24,14,.55) 100%); }
  .hero-inner { position:relative; z-index:2; max-width:960px; margin:0 auto; padding:6rem 1.5rem 3.5rem; }
  .eyebrow { display:inline-block; font-size:.72rem; letter-spacing:.14em; text-transform:uppercase;
             font-weight:700; color:#fff; margin:0 0 .7rem;
             padding:.3rem .7rem; background:rgba(255,255,255,.16); backdrop-filter:blur(4px);
             border:1px solid rgba(255,255,255,.22); border-radius:999px; }
  h1 { margin:0 0 .5rem; font-size:2.5rem; line-height:1.12; letter-spacing:-.02em; font-weight:800;
       color:#fff; text-shadow:0 2px 24px rgba(0,0,0,.4); }
  .lead { color:rgba(255,255,255,.92); margin:0; max-width:58ch; font-size:1.08rem;
          text-shadow:0 1px 12px rgba(0,0,0,.35); }
  .credit { position:absolute; z-index:2; right:.9rem; bottom:.6rem; margin:0; font-size:.7rem;
            color:rgba(255,255,255,.8); text-shadow:0 1px 6px rgba(0,0,0,.7); max-width:70%; text-align:right; }
  .credit a { color:inherit; text-decoration:underline; text-underline-offset:2px; }
  .credit a.inat { text-decoration:none; }
  .credit img { width:15px; height:15px; vertical-align:-3px; margin-right:.1rem; border-radius:3px; }
  main { max-width:960px; margin:0 auto; padding:1.5rem 1.5rem 3.5rem;
         display:grid; grid-template-columns:repeat(auto-fill,minmax(280px,1fr)); gap:1.15rem; }
  .card { position:relative; display:flex; flex-direction:column; background:var(--card);
          border:1px solid var(--border); border-radius:16px; padding:1.35rem 1.4rem 1.25rem;
          text-decoration:none; color:inherit; box-shadow:var(--shadow); overflow:hidden;
          transition:transform .18s ease, box-shadow .18s ease, border-color .18s ease; }
  .card::before { content:""; position:absolute; inset:0 auto 0 0; width:3px; background:var(--accent);
                  opacity:0; transition:opacity .18s ease; }
  .card:hover { transform:translateY(-4px); box-shadow:var(--shadow-hover); border-color:var(--accent); }
  .card:hover::before { opacity:1; }
  .card-head { display:flex; align-items:center; justify-content:space-between; margin-bottom:.85rem; }
  .icon { font-size:1.7rem; line-height:1; }
  .tag { font-size:.68rem; font-weight:700; letter-spacing:.08em; text-transform:uppercase;
         color:var(--accent); background:var(--accent-soft); padding:.28rem .6rem; border-radius:999px; }
  .card h2 { margin:0 0 .4rem; font-size:1.18rem; letter-spacing:-.01em; color:var(--accent-deep); }
  .card p  { margin:0 0 1.1rem; color:var(--muted); font-size:.93rem; flex:1; }
  .go { display:inline-flex; align-items:center; gap:.35rem; font-size:.88rem; font-weight:600; color:var(--accent); }
  .arrow { transition:transform .18s ease; }
  .card:hover .arrow { transform:translateX(4px); }
  footer { max-width:960px; margin:0 auto; padding:0 1.5rem 3.5rem; color:var(--muted); font-size:.85rem;
           border-top:1px solid var(--border); padding-top:1.5rem; }
  footer p { margin:0 0 .55rem; line-height:1.5; }
  footer a { color:var(--accent-deep); text-decoration:underline; text-underline-offset:2px; }
  footer p.ack { font-size:.95rem; margin-bottom:1rem; }
  footer p.ack a { font-weight:600; }
</style>
</head>
<body>
  <div class="hero">
    <img class="hero-bg" src="./hero.jpg?v={{herov}}" alt="">
    <div class="hero-inner">
      <span class="eyebrow">&#127963;&#65039; Cabrillo National Monument</span>
      <h1>Native Bee Monitoring Program</h1>
      <p class="lead">Native bee field guides, checklists, and interactive maps from the Cabrillo native-bee monitoring program. Pick a page to explore.</p>
    </div>
    <p class="credit"><a class="inat" href="https://www.inaturalist.org/observations/248210427" title="View on iNaturalist"><img src="./inat-logo.png" alt="iNaturalist" width="15" height="15"></a> Peridot Sweat Bee (<i>Augochlorella pomoniella</i>) on Wirelettuce (<i>Stephanomeria</i>) &middot; Brandi Sanchez &middot; <a href="https://creativecommons.org/licenses/by-nc/4.0/">CC BY-NC</a></p>
  </div>
  <main>{{cards}}
  </main>
  <footer>
    <p class="ack"><a href="./acknowledgements.html">Acknowledgements, learn about the team behind this work &rarr;</a></p>
    <p><a href="https://www.nps.gov/cabr/">Cabrillo National Monument</a> and the <a href="https://www.nps.gov/">National Park Service</a> support this monitoring program, which is funded in large part by the <a href="https://www.nps.gov/rlc/southerncal/index.htm">Southern California Research Learning Center</a>, with additional support from <a href="https://conservationlegacy.org/">Conservation Legacy</a> and the <a href="https://www.cnmf.org/">Cabrillo National Monument Foundation</a>. The observations are shared by the <a href="https://www.inaturalist.org/">iNaturalist</a> community.</p>
    <p class="cite">{{inat_cite}}<br>{{iucn_cite}}</p>
    <p>Generated by the beescabr pipeline, Brandi Sanchez. Data as of {{date}}.</p>
  </footer>
</body>
</html>')

# Pure builder: given the cards to show (list of page specs), the data-as-of date,
# and the hero cache-buster, return the complete index.html as a string.
build_landing_html <- function(cards, date, hero_v) {
  cards_html <- paste0(vapply(cards, function(c)
    .fill(.CARD_TPL, out = c$out, icon = c$icon, tag = c$tag, title = c$title, blurb = c$blurb,
          ver = if (!is.null(c$v) && nzchar(c$v)) paste0("?v=", c$v) else ""),
    character(1)), collapse = "")
  # Data-source credits. IUCN REQUIRES a citation carrying the Red List version, which the
  # status refresh records to disk; iNaturalist asks for none, so it is credited instead.
  # An unknown version prints nothing rather than a citation that could be wrong.
  if (!exists("iucn_citation")) source('scripts/website/citations.R')
  .fill(.PAGE_TPL, herov = hero_v, cards = cards_html, date = date,
        inat_cite = inat_citation(date),
        iucn_cite = iucn_citation(citation_read_version(IUCN_VERSION_FILE), date))
}

# data-as-of date: read the same "data as of YYYY-MM-DD" the summary page prints,
# so the landing footer matches. Falls back to a phrase if the page is absent.
landing_date <- function() {
  f <- file.path(SRC_DIR, "reference/nps_summary/website/nps_summary_tables.html")
  if (file.exists(f)) {
    txt <- readLines(f, warn = FALSE)
    m <- regmatches(txt, regexpr("data as of [0-9]{4}-[0-9]{2}-[0-9]{2}", txt))
    if (length(m)) return(sub("data as of ", "", m[[1]]))
  }
  "the latest survey export"
}

# cache-buster: a short content hash of a published file, so browsers re-fetch it
# whenever the content changes but still cache it while it does not. Used for the
# hero photo AND for every card link (a bare link let browsers serve a stale page
# for hours after a republish, which is the recurring "the site looks old" bug).
page_version <- function(path) {
  if (file.exists(path)) {
    h <- unname(tools::md5sum(path))
    if (!is.na(h) && nzchar(h)) return(substr(h, 1, 8))
  }
  "1"
}
hero_version <- function() page_version(file.path(DOCS_DIR, "hero.jpg"))

# A small fixed "Back to main page" pill, wrapped in sentinels so re-runs replace
# rather than stack it. Injected into every copied report page so no page is a
# dead-end (they are full-screen maps/tables with no header of their own).
BACKLINK <- paste0(
  '<!--bx-back--><style>.bx-backlink{position:fixed;top:16px;right:16px;z-index:2147483000;',
  'display:inline-flex;align-items:center;gap:.35rem;padding:.4rem .8rem;border-radius:999px;',
  'background:rgba(22,48,43,.92);color:#fff;font:600 13px/1.1 -apple-system,BlinkMacSystemFont,',
  '"Segoe UI",Roboto,Helvetica,Arial,sans-serif;text-decoration:none;',
  'box-shadow:0 2px 8px rgba(0,0,0,.28);border:1px solid rgba(255,255,255,.2)}',
  '.bx-backlink:hover{background:rgba(28,92,40,.96)}</style>',
  '<a href="./index.html" class="bx-backlink">&larr; Back to main page</a><!--/bx-back-->')

# ---- favicon -------------------------------------------------------------------
# No page used to set one, so browsers fell back to whatever icon they already had for
# the github.io host, and the tab showed the NPS arrowhead from a different site of the
# same account. The park's logo is not ours to display, so the site declares its own.
#
# A PNG, not an inline SVG data URI: Safari would not render the SVG and kept using the
# cached host icon, which is the whole problem this is meant to fix. docs/favicon.png is
# a COMMITTED file (docs/*.png is allow-listed in .gitignore), so publishing needs no
# emoji font on the machine doing the build. To change the icon, replace that file.
FAVICON <- '<link rel="icon" type="image/png" href="favicon.png">'

# PURE, so the injection is testable without writing a file. A page with no <head> is
# returned untouched rather than being handed a stray <link>.
add_favicon_html <- function(html) {
  if (grepl('rel="icon"', html, fixed = TRUE)) return(html)   # idempotent
  if (!grepl("<head[^>]*>", html)) return(html)
  sub("(<head[^>]*>)", paste0("\\1", FAVICON), html)
}

add_favicon <- function(path) {
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  writeLines(add_favicon_html(html), path)
}

inject_backlink <- function(path) {
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  html <- gsub("(?s)<!--bx-back-->.*?<!--/bx-back-->", "", html, perl = TRUE)  # idempotent
  if (grepl("<body[^>]*>", html)) html <- sub("(<body[^>]*>)", paste0("\\1", BACKLINK), html)
  else html <- paste0(BACKLINK, html)
  writeLines(html, path)
}

# Orchestrator: copy pages, build the content + landing pages, write docs/.
# The one line publish_pages() always prints when it has finished, and the only
# reliable signal that it did. It refuses an empty or stale docs/ with
# return(invisible(FALSE)) rather than stop(), so Rscript still exits 0 -- the exit
# status alone cannot tell a refusal from a success.
PUBLISH_DONE_MARKER <- "Done. Review docs/"

#' Did a `Rscript publish_pages.R` run actually publish?
#'
#' The runner used to check nothing at all: a refusal printed "  STOPPING: ...",
#' the runner announced "Site rebuilt in docs/" on top of it, and with
#' BEESCABR_DEPLOY=1 it committed and pushed the half-built folder live. Absence
#' of the finish line covers every way the run can end early -- the two refusals,
#' an error part way through the copy loop, and the command not running at all.
#'
#' @param out What system2(..., stdout = TRUE, stderr = TRUE) returned.
#' @return TRUE if the site was NOT published.
publish_run_failed <- function(out) {
  st <- attr(out, "status")
  if (!is.null(st) && !identical(as.integer(st), 0L)) return(TRUE)
  !any(grepl(PUBLISH_DONE_MARKER, out, fixed = TRUE))
}

publish_pages <- function() {
  # locate repo root from the script path so it runs from anywhere (mirrors the
  # old `cd "$(dirname "$0")/../.."`). When sourced (tests) there is no --file=.
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) setwd(normalizePath(file.path(dirname(sub("^--file=", "", fa[1])), "..", "..")))

  dir.create(DOCS_DIR, showWarnings = FALSE, recursive = TRUE)
  nojekyll <- file.path(DOCS_DIR, ".nojekyll")            # serve files as-is, no Jekyll processing
  if (!file.exists(nojekyll)) file.create(nojekyll)

  # ---- stale-output guard (runs BEFORE anything is written) ----
  # Must come before the copy loop: once a page is copied into docs/ the live site has
  # already changed, so a guard that fires afterwards cannot honestly say it stopped.
  # run_all_analysis_pipeline.R is best-effort -- a failed script leaves its PREVIOUS
  # output on disk and only prints a tally at the end, which is easy to miss when
  # publishing happens later as a separate command.
  if (!nzchar(Sys.getenv("BEESCABR_ALLOW_STALE"))) {
    src_time <- suppressWarnings(max(c(publish_mtime(PATHS$inat_clean),
                                       publish_mtime(PATHS$specimen_clean)), na.rm = TRUE))
    if (!is.finite(src_time)) src_time <- NA_real_
    times <- vapply(PUBLISH_PAGES, function(c) publish_mtime(file.path(SRC_DIR, c$src)), 0)
    names(times) <- vapply(PUBLISH_PAGES, `[[`, "", "out")
    stale <- publish_stale_pages(times, src_time, tol_secs = 60)
    if (length(stale)) {
      message("")
      message("  STOPPING: ", publish_stale_message(stale))
      message("")
      return(invisible(FALSE))
    }
  }

  # ---- copy each public page into docs/ ----
  present <- list()
  for (p in PUBLISH_PAGES) {
    src <- file.path(SRC_DIR, p$src)
    if (file.exists(src)) {
      file.copy(src, file.path(DOCS_DIR, p$out), overwrite = TRUE)
      inject_backlink(file.path(DOCS_DIR, p$out))     # add the "Back to main page" pill
      add_favicon(file.path(DOCS_DIR, p$out))        # one bee in every tab
      message("published  ", p$out)
      present[[length(present) + 1L]] <- p
    } else {
      message("SKIP (missing)  ", src)
    }
  }

  # ---- build the content pages (Acknowledgements, etc.) ----
  # build_content_pages.R writes docs/acknowledgements.html directly from the
  # rosters (self-contained; emails are never output). Sourced in its own
  # environment so its helpers do not leak into this one.
  source(file.path("scripts", "website", "build_content_pages.R"), local = new.env())
  add_favicon(file.path(DOCS_DIR, "acknowledgements.html"))
  message("published  acknowledgements.html")

  # ---- landing page (docs/index.html) ----
  # Found nothing? Stop before touching index.html (see publish_would_empty_site).
  if (publish_would_empty_site(present)) {
    message("")
    message("  STOPPING: ", publish_empty_message(SRC_DIR))
    message("")
    return(invisible(FALSE))
  }

  present <- lapply(present, function(c) { c$v <- page_version(file.path(DOCS_DIR, c$out)); c })
  writeLines(build_landing_html(group_pages_by_tag(present), landing_date(), hero_version()),
             file.path(DOCS_DIR, "index.html"))
  add_favicon(file.path(DOCS_DIR, "index.html"))
  message("built      index.html")
  message(PUBLISH_DONE_MARKER,
          ", then commit and push. Enable GitHub Pages: Settings -> Pages -> main /docs.")
}

# Run only when executed as a script (Rscript), not when sourced by a test.
if (sys.nframe() == 0L) publish_pages()
