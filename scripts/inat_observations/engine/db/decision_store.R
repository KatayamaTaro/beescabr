# =============================================================
# inat_observations/engine/db/decision_store.R
# beescabr pipeline -- persisted manual disambiguation decisions (DuckDB)
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# The Holway reference builder is interactive: when an iNat name search is
# ambiguous, a human picks the right taxon (or skips). Those decisions are
# recorded here keyed by the exact search term, so re-running the builder
# is reproducible and never re-prompts for an already-decided term. This is
# what makes "manual intervention" safe to bake into a batch pipeline.
#
# Depends on: DBI, duckdb.
# =============================================================

library(DBI)

# Return the recorded decision for a search term, or NULL if undecided.
# A decision is list(action, chosen_taxon_id). action is one of:
#   "pick"      -> resolved to chosen_taxon_id
#   "keep"      -> valid in ITIS but not on iNat (blank id, itis_valid TRUE)
#   "skip"      -> checked ITIS, not valid / unpublished (blank id, itis_valid FALSE)
#   "tentative" -> two-word name the user said is NOT a subspecies; provisional
#                  (blank id, itis_valid blank)
decision_get <- function(con, search_term) {
  res <- DBI::dbGetQuery(
    con,
    "SELECT action, chosen_taxon_id, decided_at FROM holway_decisions WHERE search_term = ?",
    params = list(search_term)
  )
  if (nrow(res) == 0) return(NULL)
  list(action = res$action[1],
       chosen_taxon_id = if (is.na(res$chosen_taxon_id[1])) NA_integer_ else as.integer(res$chosen_taxon_id[1]),
       # WHEN matters now: the second pass re-asks about a "no iNaturalist page"
       # answer every run, and the date is what tells the operator whether enough
       # time has passed to be worth looking again.
       decided_at = res$decided_at[1])
}

#' Record what a human decided about an ambiguous name
#'
#' Decisions are stored so the same question is never asked twice.
#'
#' @param con An open cache connection.
#' @param search_term The name that was ambiguous.
#' @param action One of `"pick"`, `"skip"`, `"keep"`, `"tentative"`.
#' @param chosen_taxon_id The id chosen, for `"pick"`.
#' @return Invisibly, nothing.
decision_put <- function(con, search_term, action, chosen_taxon_id = NA_integer_) {
  # "no_inat_id" = a person confirmed iNaturalist has no page for this bee. It is a
  # permanent answer, not a skip: holway_reference_build.R reads it back to stop
  # re-asking, and treats it as itis_valid. It was missing from this list, so the
  # write threw, the caller's tryCatch swallowed it, and the answer was lost.
  stopifnot(action %in% c("pick", "skip", "keep", "tentative", "no_inat_id"))
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO holway_decisions (search_term, chosen_taxon_id, action, decided_at)
     VALUES (?, ?, ?, now())",
    params = list(search_term,
                  if (is.na(chosen_taxon_id)) NA_integer_ else as.integer(chosen_taxon_id),
                  action)
  )
  invisible(search_term)
}

#' Which decisions chose one of these taxa?
#'
#' A pick is replayed forever -- `resolve_holway_row()` returns the stored
#' chosen_taxon_id before any API call -- so when iNaturalist retires that taxon the
#' decision keeps handing back a dead number. The yearly sweep knows which taxa moved;
#' this turns that into the list of answers worth asking about again. Decisions with no
#' chosen taxon ("no_inat_id", "skip") are never matched: there is no id to go stale.
#'
#' @param con An open cache connection.
#' @param taxon_ids The taxa that changed.
#' @return The search terms of the affected decisions.
decisions_for_taxa <- function(con, taxon_ids) {
  ids <- unique(suppressWarnings(as.integer(taxon_ids)))
  ids <- ids[!is.na(ids)]
  if (!length(ids)) return(character(0))
  res <- DBI::dbGetQuery(
    con,
    sprintf("SELECT search_term FROM holway_decisions WHERE chosen_taxon_id IN (%s)",
            paste(ids, collapse = ",")))
  as.character(res$search_term)
}

#' Every checklist name we hold an answer for
#'
#' Used to tell which answers a new checklist version has orphaned: a name the new
#' version dropped, that somebody had already ruled on, now applies to nothing.
#'
#' @param con An open cache connection.
#' @return The search terms.
decisions_all_terms <- function(con)
  as.character(DBI::dbGetQuery(con, "SELECT search_term FROM holway_decisions")$search_term)

#' Forget a decision, so the next build asks about it again
#'
#' The alternative to wiping the whole table. Nearly every saved answer is still
#' right; re-asking all of them to catch the handful that are not is how a rebuild
#' becomes a day of work nobody does.
#'
#' @param con An open cache connection.
#' @param search_terms The decisions to drop.
#' @return How many rows were removed.
decision_forget <- function(con, search_terms) {
  st <- unique(as.character(search_terms))
  st <- st[!is.na(st) & nzchar(st)]
  if (!length(st)) return(0L)
  n <- 0L
  for (s in st)
    n <- n + DBI::dbExecute(con, "DELETE FROM holway_decisions WHERE search_term = ?",
                            params = list(s))
  as.integer(n)
}

#' Write every saved answer out to a CSV
#'
#' Called before a checklist version bump wipes them. "Recoverable" has to mean a file
#' you can open, not a theory about re-answering: a version bump is triggered by a
#' person editing two paths in config.R, and editing them by mistake should not cost
#' anyone a year of judgements.
#'
#' @param con An open cache connection.
#' @param path Where to write.
#' @return How many answers were written.
decision_export <- function(con, path) {
  d <- DBI::dbGetQuery(con, "SELECT search_term, chosen_taxon_id, action, decided_at
                             FROM holway_decisions ORDER BY search_term")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(d, path, row.names = FALSE, na = "")
  nrow(d)
}

#' Drop every saved answer
#'
#' What a checklist version bump means. A surviving NAME is not evidence the bee
#' survived unchanged -- Holway can keep a spelling and change what it applies to --
#' and nothing in the string reveals it. Re-deriving is cheap and loud; reusing is
#' free and silent.
#'
#' @param con An open cache connection.
#' @return How many were dropped.
decision_forget_all <- function(con)
  as.integer(DBI::dbExecute(con, "DELETE FROM holway_decisions"))

#' Has the checklist version changed since these answers were made?
#'
#' PURE. Both versions must be known. An absent marker means "never recorded", which
#' is exactly the state of every machine the first time this ships -- treating that as
#' a change would wipe every answer on the strength of a file that never existed.
#'
#' @param now The version being read.
#' @param was The version the answers were made against.
#' @return TRUE only when both are known and the checklist moved forward.
holway_version_bumped <- function(now, was) {
  now <- suppressWarnings(as.integer(now)); was <- suppressWarnings(as.integer(was))
  !is.na(now) && !is.na(was) && now > was
}

# The smallest checklist we will believe. A saved answer is only an orphan because the
# name is absent from the CURRENT sheet -- so a sheet that failed to load, or loaded
# half a file, would make every answer in the store look orphaned. The real checklist
# is ~1000 names; anything under this is a bad read, not a shrunken checklist.
DECISION_MIN_CHECKLIST <- 100L

#' Drop answers for bees that have left the checklist
#'
#' They are never read again -- the Holway build loops over the names in the current
#' sheet -- but they accumulate across versions, so a table read years later is full of
#' answers for bees that left two versions ago. Deleting them changes no output and
#' costs no re-resolution: nothing was going to look them up.
#'
#' @param con An open cache connection.
#' @param current_terms Every search term the current checklist produces.
#' @return How many answers were dropped; 0 when the checklist looks unreadable.
forget_orphan_decisions <- function(con, current_terms) {
  cur <- unique(trimws(as.character(current_terms)))
  cur <- cur[!is.na(cur) & nzchar(cur)]
  if (length(cur) < DECISION_MIN_CHECKLIST) return(0L)
  decision_forget(con, setdiff(trimws(decisions_all_terms(con)), cur))
}

decision_count <- function(con) {
  DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM holway_decisions")$n[1]
}
