# =============================================================
# utils/refresh_due.R
# beescabr -- "is this cached reference data stale?"
#
# IUCN Red List status and plant common names are cached so a normal run stays OFFLINE.
# They only refresh when someone remembers BEESCABR_REFRESH=1, which is easy to forget
# for an entire season. This module lets the pipeline check how old the cached values
# ACTUALLY are and say so, instead of relying on memory.
#
# Age comes from each cache's own retrieved_on column, never the file's mtime: these
# files get rewritten for unrelated reasons (a new taxon appended, a column added), and
# a rewrite must not make year-old values look freshly checked.
#
# Judged by the OLDEST entry, because one taxon rechecked yesterday says nothing about
# the other 200. Pure + injectable so it is unit-tested offline.
# =============================================================

`%||%` <- if (exists("%||%")) `%||%` else function(a, b) if (is.null(a) || length(a) == 0) b else a

REFRESH_MAX_AGE_DAYS <- 365L   # IUCN reassesses a handful of taxa per year; annual is plenty

refresh_due <- function(path, max_age_days = REFRESH_MAX_AGE_DAYS,
                        date_col = "retrieved_on", today = Sys.Date(),
                        dates_fn = NULL) {
  none <- function(reason) list(due = TRUE, age_days = NA_integer_, oldest = NA,
                                reason = reason, path = path)
  # Not every cache is a CSV with a date column. The bee taxon cache is a DuckDB
  # table, and listing it here is the whole point of this file -- a cache nothing can
  # age is a cache nothing ever prompts about, which is how three of them drifted for
  # years. A cache that cannot say how old it is counts as due.
  dates <- if (!is.null(dates_fn)) {
    tryCatch(suppressWarnings(as.Date(dates_fn())), error = function(e) as.Date(character(0)))
  } else {
    if (!file.exists(path)) return(none("never fetched (no cache file yet)"))
    d <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(d) || !date_col %in% names(d)) return(none("cache has no retrieval dates"))
    suppressWarnings(as.Date(d[[date_col]]))
  }
  dates <- dates[!is.na(dates)]
  if (!length(dates)) return(none("cache has no usable retrieval dates"))
  oldest <- min(dates)
  age    <- as.integer(today - oldest)
  list(due      = age > max_age_days,
       age_days = age,
       oldest   = oldest,
       reason   = sprintf("oldest entry checked %s (%d days ago)", oldest, age),
       path     = path)
}

# The reference caches a normal run depends on, in the order they matter.
REFRESH_CACHES <- list(
  list(key = "IUCN Red List status", path = "data/checklists/iucn/iucn_status_generated.csv",
       tool = "scripts/reference/refresh/refresh_iucn_status.R", needs = "internet + a free IUCN token"),
  list(key = "plant common names",   path = "data/checklists/plants/plant_genus_common_generated.csv",
       tool = "scripts/reference/refresh/refresh_plant_common_names.R", needs = "internet"),
  list(key = "plant taxon numbers",  path = "data/reference/generated/plant_name_resolution_cache.csv",
       date_col = "resolved_on",
       tool = "scripts/reference/refresh/refresh_plant_taxon_ids.R", needs = "internet"),
  # a DuckDB table, not a CSV -- aged by its own fetched_at
  list(key = "bee taxon numbers",    path = "data/inat_observations/cache/inat_cache.duckdb",
       dates_fn = function() .refresh_taxon_cache_dates(),
       tool = "scripts/reference/refresh/refresh_taxon_ids.R", needs = "internet")
)

# When was each cached bee taxon last fetched? Read-only and failure-tolerant: a
# locked or absent database means "cannot say", which refresh_due() counts as due.
.refresh_taxon_cache_dates <- function(db = "data/inat_observations/cache/inat_cache.duckdb") {
  if (!file.exists(db) || !requireNamespace("duckdb", quietly = TRUE)) return(as.Date(character(0)))
  con <- tryCatch(DBI::dbConnect(duckdb::duckdb(), db, read_only = TRUE), error = function(e) NULL)
  if (is.null(con)) return(as.Date(character(0)))
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  d <- tryCatch(DBI::dbGetQuery(con, "SELECT fetched_at FROM taxon_cache")$fetched_at,
                error = function(e) NULL)
  if (is.null(d)) as.Date(character(0)) else as.Date(d)
}

# Report any cache past its age limit. Returns a list (empty = all current) so the
# caller decides whether to warn, prompt, or ignore.
refresh_overdue <- function(caches = REFRESH_CACHES, max_age_days = REFRESH_MAX_AGE_DAYS,
                            today = Sys.Date()) {
  out <- lapply(caches, function(c) c(c, refresh_due(
    c$path, max_age_days, date_col = c$date_col %||% "retrieved_on",
    today = today, dates_fn = c$dates_fn)))
  Filter(function(x) isTRUE(x$due), out)
}

#' Every reference cache with its age, overdue or not
#'
#' `refresh_overdue()` returns only what is already past its limit, which is why a run
#' said nothing at all for 364 days and then opened with a prompt. This returns all of
#' them so the age can be shown while it is still just information.
#'
#' @param caches The cache list to check.
#' @param max_age_days The age limit.
#' @param today Injection point for the date.
#' @return A list, one entry per cache, each carrying key / age_days / due.
refresh_ages <- function(caches = REFRESH_CACHES, max_age_days = REFRESH_MAX_AGE_DAYS,
                         today = Sys.Date()) {
  lapply(caches, function(c) c(c, refresh_due(
    c$path, max_age_days, date_col = c$date_col %||% "retrieved_on",
    today = today, dates_fn = c$dates_fn)))
}

#' Those ages as lines a person can read
#'
#' Months rather than a raw day count: "checked 8 months ago" lands, "250 days" has to
#' be divided first. Under two months stays in days, where days are still the natural
#' unit.
#'
#' @param ages What `refresh_ages()` returned.
#' @return A character vector of lines.
refresh_age_lines <- function(ages) {
  vapply(ages, function(a) {
    d <- suppressWarnings(as.integer(a$age_days %||% NA))
    when <- if (is.na(d)) "never checked"
            else if (d < 60L) sprintf("checked %d days ago", d)
            else sprintf("checked %d months ago", round(d / 30.44))
    sprintf("    %-24s %s%s", a$key, when, if (isTRUE(a$due)) "  -- due a refresh" else "")
  }, character(1))
}

# Ask before spending a few minutes online re-checking a year-old cache. Defaults to
# NO: declining keeps the existing cache and the run carries on normally, still picking
# up NEW taxa incrementally the way every run does. Nothing is lost by saying no except
# re-checking taxa that were already looked up.
#
# Note on scope: this is NOT the name-judgement step. Both refresh tools query the APIs
# by scientific name and rewrite their cache with no prompts. Judging renames/synonyms
# is the FULL REBUILD (run-menu option 4), which carries its own expertise warning.
refresh_confirm <- function(overdue = refresh_overdue(),
                            read_fn = function(prompt) readline(prompt),
                            is_interactive = interactive(),
                            say = message) {
  if (!length(overdue)) return(FALSE)
  if (!is_interactive)  return(FALSE)   # scheduled runs never phone out unasked
  say("")
  say("  It has been over a year since this reference data was last checked:")
  for (o in overdue) say("    - ", o$key, " (", o$reason, ")")
  say("")
  say("  Refreshing re-queries every entry against the live APIs. It needs INTERNET")
  say("  and a free IUCN token, and takes a few minutes. It does NOT ask you to judge")
  say("  any bee names -- that is the full rebuild, which is a separate choice.")
  say("")
  say("  Say no and the run keeps the existing cache and continues normally; new taxa")
  say("  are still picked up as they always are.")
  ans <- tolower(trimws(read_fn("  Refresh the reference data now? [y/N]: ")))
  isTRUE(ans %in% c("y", "yes"))
}
