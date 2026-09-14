# IUCN status and plant common names are cached and only refreshed when someone
# remembers BEESCABR_REFRESH=1 -- which is easy to forget for a whole year. The
# pipeline now checks how old the cached values actually are and says so.
#
# Age comes from each cache's own retrieved_on column, not the file's mtime: a file
# rewritten for an unrelated reason must not look freshly checked.

src("utils/refresh_due.R")

mk <- function(dates) {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(genus = "X", retrieved_on = dates), f, row.names = FALSE)
  f
}

test_that("a freshly retrieved cache is not due", {
  r <- refresh_due(mk(Sys.Date() - 10), max_age_days = 365)
  expect_false(r$due)
  expect_equal(r$age_days, 10L)
})

test_that("a cache older than the limit IS due", {
  r <- refresh_due(mk(Sys.Date() - 400), max_age_days = 365)
  expect_true(r$due)
  expect_equal(r$age_days, 400L)
})

test_that("age is judged by the OLDEST entry, not the newest", {
  # one taxon rechecked yesterday must not make a years-old table look current
  r <- refresh_due(mk(c(Sys.Date() - 1, Sys.Date() - 500)), max_age_days = 365)
  expect_true(r$due)
  expect_equal(r$age_days, 500L)
})

test_that("a missing cache is due, and says so rather than erroring", {
  r <- refresh_due(file.path(tempdir(), "nope.csv"), max_age_days = 365)
  expect_true(r$due)
  expect_true(is.na(r$age_days))
  expect_match(r$reason, "never|missing", ignore.case = TRUE)
})

test_that("a cache with no usable dates is due", {
  f <- tempfile(fileext = ".csv")
  write.csv(data.frame(genus = "X", retrieved_on = NA), f, row.names = FALSE)
  r <- refresh_due(f, max_age_days = 365)
  expect_true(r$due)
})

test_that("the file's mtime is ignored -- only retrieved_on counts", {
  f <- mk(Sys.Date() - 400)
  Sys.setFileTime(f, Sys.time())        # touched just now, but the DATA is old
  expect_true(refresh_due(f, max_age_days = 365)$due)
})

# ---- the pipeline-facing roll-up --------------------------------------------

test_that("refresh_overdue lists only the caches past their limit", {
  fresh <- mk(Sys.Date() - 5); stale <- mk(Sys.Date() - 500)
  caches <- list(list(key = "fresh one", path = fresh, tool = "a.R", needs = "internet"),
                 list(key = "stale one", path = stale, tool = "b.R", needs = "internet"))
  out <- refresh_overdue(caches, max_age_days = 365)
  expect_equal(length(out), 1L)
  expect_equal(out[[1]]$key, "stale one")
  expect_true(grepl("500 days ago", out[[1]]$reason))
})

test_that("refresh_overdue is empty when everything is current", {
  caches <- list(list(key = "a", path = mk(Sys.Date() - 5), tool = "a.R", needs = "internet"))
  expect_equal(length(refresh_overdue(caches, max_age_days = 365)), 0L)
})

# Every real cache must be CHECKABLE -- refresh_ages() has to return an answer for
# each, using that entry's own date source. A cache that has genuinely never been
# dated reports "due" rather than erroring; that is the correct answer, not a failure.
# (It used to call refresh_due(c$path) directly, ignoring each entry's date_col and
# dates_fn, which worked only while every cache happened to be a CSV with the same
# column name.)
test_that("every real cache reports an age or a reason, using its own date source", {
  old <- setwd(.beescabr_root()); on.exit(setwd(old), add = TRUE)
  ages <- refresh_ages()
  expect_equal(length(ages), length(REFRESH_CACHES))
  for (a in ages) {
    expect_true(nzchar(a$reason %||% ""), info = a$key)
    if (is.na(a$age_days)) expect_true(a$due, info = a$key)   # undatable => due
  }
})

# ---- the yearly confirm ------------------------------------------------------
# Nothing is actually overdue today and testthat runs non-interactively, so these
# supply a fake overdue cache and say so explicitly.
fake_overdue <- list(list(key = "IUCN Red List status", reason = "oldest entry checked 2024-01-01 (600 days ago)"))
confirm <- function(read_fn, say = function(...) invisible())
  refresh_confirm(overdue = fake_overdue, read_fn = read_fn, is_interactive = TRUE, say = say)
# A refresh is online, takes minutes, and needs an IUCN token. It is not a name-
# judgement task (that is the full rebuild), so the prompt says what it really does.

test_that("Y runs the refresh", {
  expect_true(confirm(function(...) "y"))
  expect_true(confirm(function(...) "Y"))
})

test_that("N keeps the cache and the run continues normally", {
  expect_false(confirm(function(...) "n"))
})

test_that("Enter defaults to NOT refreshing -- the safe, offline-ish choice", {
  expect_false(confirm(function(...) ""))
})

test_that("anything unrecognised is treated as no", {
  expect_false(confirm(function(...) "banana"))
})

test_that("a non-interactive run never prompts and never refreshes on its own", {
  called <- FALSE
  r <- refresh_confirm(overdue = fake_overdue, read_fn = function(...) { called <<- TRUE; "y" },
                       is_interactive = FALSE, say = function(...) invisible())
  expect_false(called)
  expect_false(r)
})

test_that("the prompt states what the refresh needs and what it does NOT need", {
  said <- character(0)
  confirm(function(...) "n", say = function(...) said <<- c(said, paste0(...)))
  blob <- paste(said, collapse = " ")
  expect_true(grepl("internet", blob, ignore.case = TRUE))
  expect_true(grepl("token",    blob, ignore.case = TRUE))
  expect_true(grepl("keep|cache", blob, ignore.case = TRUE))
})

# The age of the reference caches was printed ONLY when something was already over a
# year old. So for 364 days the run said nothing, and the first word anyone got was
# the prompt itself -- by which point the data had been stale for a year and the
# operator had no sense of how long it had been drifting. Print the age every run,
# so "checked 8 months ago" is visible long before it becomes a decision.
test_that("every cache is reported, not just the overdue ones", {
  ages <- list(list(key = "IUCN Red List status", age_days = 40L,  due = FALSE),
               list(key = "plant common names",   age_days = 400L, due = TRUE))
  txt <- paste(refresh_age_lines(ages), collapse = "\n")
  expect_match(txt, "IUCN Red List status", fixed = TRUE)
  expect_match(txt, "plant common names", fixed = TRUE)
})

test_that("an age reads in months, not a raw day count", {
  txt <- paste(refresh_age_lines(list(list(key = "IUCN Red List status",
                                           age_days = 250L, due = FALSE))), collapse = " ")
  expect_match(txt, "8 months", fixed = TRUE)
  expect_false(grepl("250", txt, fixed = TRUE))
})

test_that("a recent check reads in days", {
  txt <- paste(refresh_age_lines(list(list(key = "x", age_days = 3L, due = FALSE))),
               collapse = " ")
  expect_match(txt, "3 days", fixed = TRUE)
})

test_that("an overdue cache is marked so it stands out", {
  txt <- paste(refresh_age_lines(list(list(key = "x", age_days = 400L, due = TRUE))),
               collapse = " ")
  expect_match(txt, "due", ignore.case = TRUE)
})

test_that("a cache that was never fetched says so instead of printing NA", {
  txt <- paste(refresh_age_lines(list(list(key = "x", age_days = NA_integer_, due = TRUE))),
               collapse = " ")
  expect_false(grepl("NA", txt, fixed = TRUE))
  expect_match(txt, "never", ignore.case = TRUE)
})

test_that("refresh_ages reports every cache, overdue or not", {
  caches <- list(list(key = "a", path = tempfile(), tool = "t", needs = "n"),
                 list(key = "b", path = tempfile(), tool = "t", needs = "n"))
  a <- refresh_ages(caches)
  expect_length(a, 2L)
  expect_equal(vapply(a, function(x) x$key, ""), c("a", "b"))
})

# REFRESH_CACHES named two caches, so the yearly prompt only ever mentioned IUCN and
# plant common names -- which is exactly why the other three drifted for years without
# anyone hearing about it. Two things blocked listing them: the plant name cache
# carries no date at all, and the bee taxon cache is a DuckDB table, not a CSV.
test_that("a cache can supply its dates some other way than a CSV column", {
  r <- refresh_due("ignored.csv", max_age_days = 365, today = as.Date("2026-09-11"),
                   dates_fn = function() as.Date(c("2025-01-01", "2026-08-01")))
  expect_true(r$due)                                   # oldest is 2025-01-01
  expect_equal(r$oldest, as.Date("2025-01-01"))
})

test_that("a date source that returns nothing is due, not an error", {
  r <- refresh_due("ignored.csv", dates_fn = function() as.Date(character(0)))
  expect_true(r$due)
  expect_match(r$reason, "no usable", fixed = TRUE)
})

test_that("a date source that fails is due, not a crash", {
  r <- refresh_due("ignored.csv", dates_fn = function() stop("database is locked"))
  expect_true(r$due)
  expect_true(is.na(r$age_days))
})

test_that("a fresh other-source cache is not due", {
  r <- refresh_due("ignored.csv", max_age_days = 365, today = as.Date("2026-09-11"),
                   dates_fn = function() as.Date("2026-09-01"))
  expect_false(r$due)
  expect_equal(r$age_days, 10L)
})

test_that("every cache the pipeline depends on is listed, with a tool that exists", {
  keys <- vapply(REFRESH_CACHES, function(c) c$key, "")
  expect_true(any(grepl("IUCN", keys)))
  expect_true(any(grepl("plant common", keys)))
  expect_true(any(grepl("plant taxon", keys)))
  expect_true(any(grepl("bee taxon", keys)))
  for (c in REFRESH_CACHES) {
    expect_true(nzchar(c$tool %||% ""), info = c$key)
    expect_true(file.exists(file.path(.beescabr_root(), c$tool)),
                info = paste(c$key, "->", c$tool))       # a tool you can actually run
  }
})

# The yearly refresh auto-runs two tools and promises "It does NOT ask you to judge
# any bee names". The plant taxon cache was added to REFRESH_CACHES -- so a run
# reports it stale -- but nothing refreshes it, and folding it into that same yes
# would break the promise: it runs for about an hour and ends in questions.
#
# So it is asked for separately, the way the ingest menu asks: say what it costs
# before the person commits, not after.
test_that("the slow refresh is separated from the quick ones", {
  overdue <- list(list(key = "IUCN Red List status"), list(key = "plant common names"),
                  list(key = "plant taxon numbers"))
  s <- refresh_split(overdue)
  expect_equal(vapply(s$quick, function(x) x$key, ""),
               c("IUCN Red List status", "plant common names"))
  expect_equal(vapply(s$slow, function(x) x$key, ""), "plant taxon numbers")
})

test_that("the slow prompt says how long and that it asks questions", {
  txt <- paste(refresh_slow_lines(), collapse = " ")
  expect_match(txt, "hour", ignore.case = TRUE)
  expect_match(txt, "question", ignore.case = TRUE)
  expect_match(txt, "saved as it goes|stop it", ignore.case = TRUE)
})

test_that("it says what declining costs, which is nothing urgent", {
  txt <- paste(refresh_slow_lines(), collapse = " ")
  expect_match(txt, "Latin", ignore.case = TRUE)
})

test_that("a scheduled run never starts an hour-long job unasked", {
  expect_false(refresh_confirm_slow(is_interactive = FALSE, read_fn = function(...) "y",
                                    say = function(...) NULL))
})

test_that("yes means yes, anything else means no", {
  yn <- function(a) refresh_confirm_slow(is_interactive = TRUE,   # testthat is not
                                         read_fn = function(...) a, say = function(...) NULL)
  expect_true(yn("y")); expect_true(yn("yes")); expect_true(yn(" Y "))
  expect_false(yn("")); expect_false(yn("n")); expect_false(yn("later"))
})
