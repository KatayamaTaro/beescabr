library(testthat)

# Landing-page builder lives in the R publish module. Sourcing it must NOT run
# publish_pages() (that does file I/O + network-free copies) -- it is guarded so
# build_landing_html() can be unit-tested in isolation.
src("publish/publish_pages.R")

test_that("build_landing_html renders one card per page with its fields", {
  cards <- list(
    list(out = "foo.html", title = "Foo Guide", blurb = "All about foo.", icon = "\U0001F41D", tag = "Field guide"),
    list(out = "bar.html", title = "Bar Map",   blurb = "Where the bars are.", icon = "\U0001F5FA", tag = "Map"))
  h <- build_landing_html(cards, "2026-08-13", "abc12345")
  expect_true(grepl('href="./foo.html"', h, fixed = TRUE))
  expect_true(grepl("Foo Guide", h, fixed = TRUE))
  expect_true(grepl("All about foo.", h, fixed = TRUE))
  expect_true(grepl('href="./bar.html"', h, fixed = TRUE))
  expect_true(grepl("Bar Map", h, fixed = TRUE))
  # both cards present
  expect_equal(length(gregexpr('class="card"', h, fixed = TRUE)[[1]]), 2L)
})

test_that("build_landing_html interpolates the date and hero cache-buster", {
  h <- build_landing_html(list(), "2026-08-13", "deadbeef")
  expect_true(grepl("hero.jpg?v=deadbeef", h, fixed = TRUE))
  expect_true(grepl("Data as of 2026-08-13.", h, fixed = TRUE))
})

test_that("build_landing_html links the Acknowledgements page and is a full document", {
  h <- build_landing_html(list(), "x", "1")
  expect_true(grepl("./acknowledgements.html", h, fixed = TRUE))
  expect_true(startsWith(h, "<!doctype html>"))
  expect_true(grepl("</html>", h, fixed = TRUE))
})

test_that("build_landing_html credits the funders with their official links", {
  h <- build_landing_html(list(), "x", "1")
  # Southern California Research Learning Center -- major funder -> its NPS RLC page
  expect_true(grepl("Southern California Research Learning Center", h, fixed = TRUE))
  expect_true(grepl('href="https://www.nps.gov/rlc/southerncal/index.htm"', h, fixed = TRUE))
  # Conservation Legacy + Cabrillo National Monument Foundation
  expect_true(grepl("Conservation Legacy", h, fixed = TRUE))
  expect_true(grepl('href="https://conservationlegacy.org/"', h, fixed = TRUE))
  expect_true(grepl("Cabrillo National Monument Foundation", h, fixed = TRUE))
  expect_true(grepl('href="https://www.cnmf.org/"', h, fixed = TRUE))
})

test_that("group_pages_by_tag groups cards by tag, sections in first-appearance order", {
  pages <- list(
    list(out = "a.html", tag = "Field guide"),
    list(out = "b.html", tag = "Map"),
    list(out = "c.html", tag = "Explore"),
    list(out = "d.html", tag = "Map"),
    list(out = "e.html", tag = "Field guide"))
  g <- group_pages_by_tag(pages)
  expect_equal(vapply(g, `[[`, "", "out"), c("a.html", "e.html", "b.html", "d.html", "c.html"))
  expect_equal(vapply(g, `[[`, "", "tag"),
               c("Field guide", "Field guide", "Map", "Map", "Explore"))
})

test_that("group_pages_by_tag keeps within-section manifest order and handles edge cases", {
  pages <- list(
    list(out = "1.html", tag = "Map"),
    list(out = "2.html", tag = "Map"),
    list(out = "3.html", tag = "Map"))
  expect_equal(vapply(group_pages_by_tag(pages), `[[`, "", "out"),
               c("1.html", "2.html", "3.html"))         # stable within a section
  expect_equal(group_pages_by_tag(list()), list())
  one <- list(list(out = "x.html", tag = "Summary"))
  expect_equal(group_pages_by_tag(one), one)
})

test_that("page_version is a short stable content hash that changes with content", {
  a <- tempfile(fileext = ".html"); b <- tempfile(fileext = ".html")
  writeLines("<h1>one</h1>", a); writeLines("<h1>two</h1>", b)
  va <- page_version(a)
  expect_true(nchar(va) == 8)
  expect_equal(va, page_version(a))          # stable: same content -> same version
  expect_false(identical(va, page_version(b)))
  expect_equal(page_version(tempfile()), "1")   # missing file -> harmless fallback
})

test_that("build_landing_html cache-busts each card link with its page version", {
  cards <- list(list(out = "foo.html", title = "Foo", blurb = "b", icon = "x", tag = "t", v = "abc12345"))
  h <- build_landing_html(cards, "2026-08-22", "hero1")
  expect_true(grepl('href="./foo.html?v=abc12345"', h, fixed = TRUE))
})

test_that("a card with no version still renders a plain working link", {
  cards <- list(list(out = "bar.html", title = "Bar", blurb = "b", icon = "x", tag = "t"))
  h <- build_landing_html(cards, "2026-08-22", "hero1")
  expect_true(grepl('href="./bar.html"', h, fixed = TRUE))
  # only the CARD link must be bare; the hero image carries its own ?v= legitimately
  expect_false(grepl('href="./bar.html?v=', h, fixed = TRUE))
})

# ---- the empty-season guard --------------------------------------------------
# The landing page is rebuilt from the pages found THIS run, not from what is
# already in docs/. On 1 January the season folder (nps_report_<year>) does not
# exist yet, so a publish would find nothing and rebuild the homepage with zero
# cards -- every page still live, but the front door blank. Refuse instead.

test_that("publishing nothing is refused rather than emptying the homepage", {
  expect_true(publish_would_empty_site(found = list()))
})

test_that("a normal publish with pages is allowed", {
  expect_false(publish_would_empty_site(found = list(list(out = "a.html"))))
})

test_that("the refusal message names the season and what to run", {
  msg <- publish_empty_message(src_dir = "data/analysis/nps_report_2027_generated")
  expect_true(grepl("2027", msg, fixed = TRUE))
  expect_true(grepl("analysis", msg, ignore.case = TRUE))
  expect_true(grepl("run_all_analysis_pipeline", msg, fixed = TRUE))
})

test_that("the message says the existing site was left alone", {
  msg <- publish_empty_message(src_dir = "data/analysis/nps_report_2027_generated")
  expect_true(grepl("unchanged|left|untouched", msg, ignore.case = TRUE))
})

# ---- freshness guard ---------------------------------------------------------
# The analysis loop is best-effort: a script that fails leaves its PREVIOUS output on
# disk and the run only prints a tally at the end. Publishing is a separate command, so
# that tally can scroll away unread and last season's file goes live silently. Compare
# each page's mtime against the cleaned tables it derives from instead of trusting it.
test_that("a page older than its source data is reported stale", {
  out <- c(fresh = 200, stale = 50)
  expect_equal(publish_stale_pages(out, src_time = 100), "stale")
})

test_that("a page newer than the data is fine, and ties are not stale", {
  expect_equal(publish_stale_pages(c(a = 150), src_time = 100), character(0))
  expect_equal(publish_stale_pages(c(a = 100), src_time = 100), character(0))
})

test_that("tolerance absorbs same-run clock jitter", {
  # a page written seconds before the source file in the same run is not stale
  expect_equal(publish_stale_pages(c(a = 95), src_time = 100, tol_secs = 10), character(0))
  expect_equal(publish_stale_pages(c(a = 80), src_time = 100, tol_secs = 10), "a")
})

test_that("missing times and an unknown source time never block publishing", {
  expect_equal(publish_stale_pages(c(a = NA_real_), src_time = 100), character(0))
  expect_equal(publish_stale_pages(c(a = 50), src_time = NA_real_), character(0))
  expect_equal(publish_stale_pages(numeric(0), src_time = 100), character(0))
})

test_that("the stale message names every stale page and how to fix it", {
  m <- publish_stale_message(c("bee_plant_matrix.html", "park_summary.html"))
  expect_true(grepl("bee_plant_matrix.html", m, fixed = TRUE))
  expect_true(grepl("park_summary.html", m, fixed = TRUE))
  expect_true(grepl("run_all_analysis_pipeline.R", m, fixed = TRUE))
})

# ---- favicon ------------------------------------------------------------------
# No page set a favicon, so browsers fell back to whatever icon they had for the
# github.io host: the site showed a classical-building emoji that belongs to a
# different page. One bee, injected at publish time so all pages match.
# It was an inline SVG data URI first. Safari would not render it and kept falling back
# to the icon it had cached for the github.io host, which was the NPS arrowhead. A PNG
# is understood by every browser, so the icon is a committed file rather than a data URI.
test_that("the favicon points at the committed PNG, not an SVG data URI", {
  expect_match(FAVICON, "rel=\"icon\"")
  expect_match(FAVICON, "favicon.png", fixed = TRUE)
  expect_false(grepl("data:image/svg", FAVICON, fixed = TRUE))
})

test_that("the favicon file exists and is a real PNG", {
  # testthat runs from tests/testthat/, so resolve against the repo root
  f <- file.path(.beescabr_root(), "docs", "favicon.png")
  expect_true(file.exists(f))
  expect_gt(file.info(f)$size, 1000)
  expect_equal(readBin(f, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))  # PNG magic bytes
})

test_that("a page with a <head> gets the favicon inside it", {
  h <- add_favicon_html("<html><head><title>x</title></head><body>y</body></html>")
  expect_true(grepl("<head>.*rel=\"icon\".*</head>", h))
})

test_that("injecting twice does not duplicate it", {
  h1 <- add_favicon_html("<html><head><title>x</title></head><body>y</body></html>")
  h2 <- add_favicon_html(h1)
  expect_equal(lengths(regmatches(h2, gregexpr("rel=\"icon\"", h2))), 1L)
})

test_that("a page with no <head> is left alone rather than corrupted", {
  h <- add_favicon_html("<div>fragment</div>")
  expect_equal(h, "<div>fragment</div>")
})

# publish_pages() refuses an empty or stale docs/ with return(invisible(FALSE)) --
# it does not stop(), so Rscript still exits 0. The runner read neither the exit
# status nor the output, so a refusal was printed, "Site rebuilt in docs/" was
# announced on top of it, and with BEESCABR_DEPLOY=1 the half-built folder was
# committed and pushed live. The success line is the only reliable signal, so it
# is a constant that both the message and the check read.
test_that("a finished publish is recognized", {
  expect_false(publish_run_failed(c("built      bee_field_guide.html",
                                    paste0(PUBLISH_DONE_MARKER, ", then commit and push."))))
})

test_that("a refusal is a failure even though the exit code is 0", {
  expect_true(publish_run_failed(c("built      bee_field_guide.html",
                                   "  STOPPING: docs/ would be emptied")))
})

test_that("a non-zero exit is a failure", {
  out <- paste0(PUBLISH_DONE_MARKER, ", then commit and push.")
  attr(out, "status") <- 1L
  expect_true(publish_run_failed(out))
})

test_that("a crash part way through is a failure, not a silent success", {
  expect_true(publish_run_failed(c("built      bee_field_guide.html",
                                   "Error in build_content_pages() : object 'x' not found")))
  expect_true(publish_run_failed(character(0)))       # never ran at all
})
