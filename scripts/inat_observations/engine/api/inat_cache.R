# =============================================================
# inat_observations/engine/api/inat_cache.R
# beescabr pipeline -- cache-first taxon access + taxonomy resolution
# Created: 2026-07-13 (API + DuckDB rewrite)
#
# The only module that knows about BOTH the API transport and the DuckDB
# taxon cache. Everything above it (checklist build, export read) asks for
# a taxon or a taxonomy map and neither knows nor cares whether it came
# from the network or the cache. Fetches on miss, writes through, returns.
#
# resolve_taxonomy() unifies what used to be two separate things in the old
# code: STEP 1's ranked-name hierarchy AND STEP 4's subgenus/complex fetch.
# Both now come from one cached /taxa/{id} call per taxon via parse_taxon_ranks().
#
# Depends on: api/inat_http.R, api/inat_flatten.R, db/taxon_store.R.
# =============================================================

if (!exists("inat_fetch_taxon_by_id")) source("scripts/inat_observations/engine/api/inat_http.R")
if (!exists("parse_taxon_ranks"))       source("scripts/inat_observations/engine/api/inat_flatten.R")
if (!exists("taxon_cache_get"))          source("scripts/inat_observations/engine/db/taxon_store.R")

# ------------------------------------------------------------
# get_taxon_by_id(): return one taxon object (nested list), cache-first.
# ------------------------------------------------------------
#' One taxon by its id, from the cache or the API
#'
#' @param con An open cache connection.
#' @param id iNaturalist taxon id.
#' @param request_fn Injection point for the API call.
#' @param verbose Report cache hits.
#' @return The parsed taxon, including its ancestors and children.
get_taxon_by_id <- function(con, id, request_fn = inat_request, verbose = FALSE) {
  key <- taxon_cache_key_id(id)
  hit <- taxon_cache_get(con, key)
  if (!is.null(hit)) {
    if (verbose) message("  cache hit: taxon ", id)
    return(hit)
  }
  results <- inat_fetch_taxon_by_id(id, request_fn = request_fn)
  taxon <- if (length(results) > 0) results[[1]] else NULL
  if (!is.null(taxon)) taxon_cache_put(con, key, taxon$id %||% id, taxon)
  if (verbose) message("  fetched taxon ", id)
  taxon
}

# ------------------------------------------------------------
# get_taxa_by_name(): return the list of candidate taxa for a name search,
# cache-first. Opportunistically caches each candidate by its own id too,
# so a later get_taxon_by_id() for one of them is a hit.
# ------------------------------------------------------------
#' Taxa matching a name, from the cache or the API
#'
#' @param con An open cache connection.
#' @param name The name to search for.
#' @param request_fn Injection point for the API call; tests pass a fake so no
#'   test ever reaches the network.
#'
#' @param verbose Report cache hits.
#' @return The parsed `results` list. Several matches means the name is
#'   ambiguous -- the caller decides, it is not resolved here.
get_taxa_by_name <- function(con, name, request_fn = inat_request, verbose = FALSE) {
  key <- taxon_cache_key_name(name)
  hit <- taxon_cache_get(con, key)
  if (!is.null(hit)) {
    if (verbose) message("  cache hit: name '", name, "'")
    return(hit)
  }
  results <- inat_fetch_taxa_by_name(name, request_fn = request_fn)
  taxon_cache_put(con, key, NA_integer_, results)
  # Cache a candidate by id ONLY if it carries ancestors and isn't already
  # cached. The /taxa?q= search endpoint omits ancestors, so caching those by id
  # would clobber the full ancestor-bearing objects the observation flatten
  # (resolve_taxonomy -> parse_taxon_ranks) needs, blanking out genus/family.
  for (t in results) {
    if (!is.null(t$id) && !is.null(t$ancestors) && length(t$ancestors) > 0 &&
        is.null(taxon_cache_get(con, taxon_cache_key_id(t$id)))) {
      taxon_cache_put(con, taxon_cache_key_id(t$id), t$id, t)
    }
  }
  if (verbose) message("  fetched name search '", name, "' (", length(results), " results)")
  results
}

# iNat /taxa/{ids} accepts at most this many ids per request.
TAXA_BATCH_SIZE <- 30L

# ------------------------------------------------------------
# prefetch_taxa(): fill the taxon cache for a set of ids using BATCHED
# requests (up to TAXA_BATCH_SIZE ids each) with a throttle between batches.
# Only ids not already cached are fetched. This replaces the old one-request-
# per-taxon behavior that was getting rate limited -- ~30x fewer requests,
# plus a courteous pause between them. Returns the number of taxa fetched.
# ------------------------------------------------------------
#' Warm the cache for many taxa in a few throttled requests
#'
#' @param con An open cache connection.
#' @param taxon_ids Ids to fetch. Already-cached ids are skipped.
#' @param request_fn Injection point for the API call.
#' @param throttle Seconds to wait between requests.
#' @param sleep_fn Injection point for the wait, so tests do not sleep.
#' @param verbose Print progress.
#' @param force Re-fetch ids already in the cache. The cache has no age of its own,
#'   so this is how the yearly rebuild asks iNaturalist again.
#' @return Invisibly, how many were fetched. Call this before a loop of
#'   `get_taxon_by_id()`, which then costs nothing.
prefetch_taxa <- function(con, taxon_ids, request_fn = inat_request,
                          throttle = INAT_THROTTLE_SEC, sleep_fn = Sys.sleep,
                          verbose = TRUE, force = FALSE) {
  taxon_ids <- unique(taxon_ids[!is.na(taxon_ids)])
  # force: re-fetch taxa we already hold. The cache has no age of its own -- a row
  # written in 2024 is served in 2026 with the name iNaturalist used then -- so the
  # yearly rebuild needs a way to ask again. Everything else still fetches misses only.
  uncached <- if (isTRUE(force)) taxon_ids else taxon_ids[vapply(taxon_ids,
    function(id) is.null(taxon_cache_get(con, taxon_cache_key_id(id))), logical(1))]
  if (length(uncached) == 0) return(invisible(0L))

  batches <- split(uncached, ceiling(seq_along(uncached) / TAXA_BATCH_SIZE))
  if (verbose) message(sprintf("  taxonomy: fetching %d uncached taxa in %d batch(es) of <=%d",
                               length(uncached), length(batches), TAXA_BATCH_SIZE))
  n <- 0L
  for (i in seq_along(batches)) {
    results <- inat_fetch_taxa_by_ids(batches[[i]], request_fn = request_fn)
    for (t in results) if (!is.null(t$id)) taxon_cache_put(con, taxon_cache_key_id(t$id), t$id, t)
    n <- n + length(results)
    if (verbose && (i %% 5 == 0 || i == length(batches)))
      message(sprintf("    batch %d/%d (%d taxa cached)", i, length(batches), n))
    if (i < length(batches) && !is.null(throttle) && throttle > 0) sleep_fn(throttle)
  }
  invisible(n)
}


# the shared answer vocabulary -- one set of words for every prompt in the pipeline
if (!exists("answer_is")) source("scripts/utils/answers.R")

# ------------------------------------------------------------
# sweep_taxon_changes(): ask iNaturalist again about ids we already hold, and report
# what moved. This is the bee half of the once-a-year rebuild.
#
# It does NOT read the cache back to find out what happened, and that is the whole
# point. prefetch_taxa() writes only what the API RETURNS, so an id iNaturalist has
# retired keeps its stale cache row untouched -- read the cache afterwards and a dead
# taxon looks perfectly healthy. The comparison has to be against the API's actual
# answer, which is why this batches the fetch itself instead of delegating it.
#
# A taxon that came back is written over its cached row. One that did NOT come back is
# left alone on purpose: the bee is still on our checklist, and dropping its name would
# empty every label and make the review file unreadable.
# ------------------------------------------------------------
#' Re-ask iNaturalist about cached taxa and report what changed
#'
#' @param con An open cache connection.
#' @param taxon_ids The ids to re-check. Ids with no cached row are skipped -- there is
#'   nothing to compare them against.
#' @param request_fn Injection point for the API call.
#' @param throttle Seconds between batches.
#' @param sleep_fn Injection point for the wait, so tests do not sleep.
#' @param verbose Print progress.
#' @return What `taxon_changes()` returns: the changed taxa only, carrying `n_full`
#'   (compared against a stored record, so a rename is detectable) and `n_partial`
#'   (asked about, but only retirement and disappearance are provable).
sweep_taxon_changes <- function(con, taxon_ids, request_fn = inat_request,
                                throttle = INAT_THROTTLE_SEC, sleep_fn = Sys.sleep,
                                verbose = TRUE) {
  ids <- unique(suppressWarnings(as.integer(taxon_ids)))
  ids <- ids[!is.na(ids)]
  before <- Filter(Negate(is.null),
                   lapply(ids, function(i) taxon_cache_get(con, taxon_cache_key_id(i))))
  cached_ids <- vapply(before, function(t) suppressWarnings(as.integer(t$id %||% NA)), integer(1))
  # An id with no cached record has no "before", so a RENAME cannot be detected for it.
  # But RETIRED and GONE are facts about the answer iNaturalist gives now -- is_active
  # is false, or the id returns nothing -- and need no before at all. Skipping those ids
  # entirely left 297 of 1184 bees unchecked for the two categories that actually break
  # joins. Every id is asked about; only the name comparison needs history.
  # Every id is asked about, so "skipped" would be the wrong word. What differs is how
  # much can be PROVEN: with history, a rename is detectable too; without it, only
  # retirement and disappearance are.
  coverage <- function(x) {
    attr(x, "n_full")    <- length(before)                    # compared against history
    attr(x, "n_partial") <- length(ids) - length(before)      # retired/gone only
    x
  }
  if (!length(ids)) return(coverage(taxon_changes(list(), list())))

  batches <- split(ids, ceiling(seq_along(ids) / TAXA_BATCH_SIZE))
  if (verbose) message(sprintf("  re-checking %d taxa with iNaturalist in %d batch(es) of <=%d",
                               length(ids), length(batches), TAXA_BATCH_SIZE))
  after <- list()
  for (i in seq_along(batches)) {
    got <- tryCatch(inat_fetch_taxa_by_ids(batches[[i]], request_fn = request_fn),
                    error = function(e) list())
    after <- c(after, got)
    # write back only what came back; a retired id keeps the name we already have
    for (t in got) if (!is.null(t$id)) taxon_cache_put(con, taxon_cache_key_id(t$id), t$id, t)
    if (i < length(batches) && !is.null(throttle) && throttle > 0) sleep_fn(throttle)
  }
  # the ids we hold history for: full comparison
  out <- taxon_changes(before, after)
  # the rest: only what the fresh answer proves on its own
  fresh <- setdiff(ids, cached_ids)
  if (length(fresh)) {
    got <- stats::setNames(after, vapply(after, function(t)
      as.character(suppressWarnings(as.integer(t$id %||% NA))), character(1)))
    add <- lapply(fresh, function(id) {
      t <- got[[as.character(id)]]
      if (is.null(t))
        return(data.frame(taxon_id = id, change = "gone", was = NA_character_,
                          now = NA_character_, rank_was = NA_character_,
                          rank_now = NA_character_, replaced_by = NA_character_,
                          stringsAsFactors = FALSE))
      if (isTRUE(t$is_active %||% TRUE)) return(NULL)   # alive: nothing provable
      syn <- unlist(t$current_synonymous_taxon_ids %||% list())
      data.frame(taxon_id = id, change = "retired", was = NA_character_,
                 now = as.character(t$name %||% NA), rank_was = NA_character_,
                 rank_now = as.character(t$rank %||% NA),
                 replaced_by = if (length(syn)) paste(syn, collapse = ",") else NA_character_,
                 stringsAsFactors = FALSE)
    })
    add <- Filter(Negate(is.null), add)
    if (length(add)) out <- rbind(out, do.call(rbind, add))
  }
  coverage(out)
}

# ------------------------------------------------------------
# taxon_changes(): PURE. What does iNaturalist now say differently?
#
# iNaturalist does not edit a taxon in place. A taxon that is split, lumped or
# swapped is RETIRED (is_active = FALSE) and a new taxon with a new id takes over its
# records. Our observations follow that automatically -- they are re-pulled every run
# -- but every reference-side id is a permanent memo, so the checklist row and the
# records drift apart and the bee turns up in the county-additions list looking like
# a discovery. Retired is therefore the serious finding, and it is reported ahead of
# a mere rename when both happened at once.
# ------------------------------------------------------------
#' Compare what we stored against what iNaturalist returns now
#'
#' @param before Taxa as cached: a list of taxon objects (`id`, `name`, `rank`,
#'   `is_active`).
#' @param after The same ids fetched again; an id iNaturalist no longer returns is
#'   simply absent, which is itself a finding.
#' @return A data.frame of the CHANGED ones only: taxon_id, change, was, now, rank_was,
#'   rank_now. Empty when nothing moved.
taxon_changes <- function(before, after) {
  by_id <- function(x) {
    ids <- vapply(x, function(t) suppressWarnings(as.integer(t$id %||% NA)), integer(1))
    stats::setNames(x, as.character(ids))
  }
  a <- by_id(after)
  out <- lapply(before, function(t) {
    id <- suppressWarnings(as.integer(t$id %||% NA))
    if (is.na(id)) return(NULL)
    n <- a[[as.character(id)]]
    was <- as.character(t$name %||% NA); rk_was <- as.character(t$rank %||% NA)
    if (is.null(n))
      return(data.frame(taxon_id = id, change = "gone", was = was, now = NA_character_,
                        rank_was = rk_was, rank_now = NA_character_,
                        replaced_by = NA_character_, stringsAsFactors = FALSE))
    now <- as.character(n$name %||% NA); rk_now <- as.character(n$rank %||% NA)
    # retired first: a live rename is a label change, a retirement moves the records
    change <- if (!isTRUE(n$is_active %||% TRUE)) "retired"
              else if (!identical(was, now)) "renamed"
              else if (!identical(rk_was, rk_now)) "rank changed"
              else NA_character_
    if (is.na(change)) return(NULL)
    # iNaturalist says what a retired taxon became, in current_synonymous_taxon_ids.
    # ONE replacement is a swap or a lump and can be offered as a straight choice.
    # SEVERAL is a split -- the bee became two, and which of our records belong to
    # which is a per-record judgement, so those are reported rather than asked.
    syn <- unlist(n$current_synonymous_taxon_ids %||% list())
    data.frame(taxon_id = id, change = change, was = was, now = now,
               rank_was = rk_was, rank_now = rk_now,
               replaced_by = if (length(syn)) paste(syn, collapse = ",") else NA_character_,
               stringsAsFactors = FALSE)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(data.frame(taxon_id = integer(0), change = character(0),
                                      was = character(0), now = character(0),
                                      rank_was = character(0), rank_now = character(0),
                                      replaced_by = character(0), stringsAsFactors = FALSE))
  do.call(rbind, out)
}

#' Read one answer at the retired-bee prompt
#'
#' The same four answers whatever kind of retirement it is, so there is one thing to
#' learn: type the new number, `keep` to leave it, `quit` to stop. `take` is a
#' shorthand for "the one number iNaturalist offered", so it only means anything when
#' there IS one -- offering it for a split would be asking which of two, with no way
#' to say which.
#'
#' A typed number is read as a taxon_id, never as a menu position: iNaturalist ids are
#' the numbers in its URLs, and a prompt where "2" might mean either is a prompt that
#' will eventually be answered wrong.
#'
#' @param raw What the operator typed.
#' @param has_offer TRUE when iNaturalist named exactly one replacement.
#' @return A list of `action` ("id", "take", "keep", "quit", "unclear") and `id`.
taxon_change_choice <- function(raw, has_offer = TRUE) {
  a <- answer_norm(raw)
  no <- function(act) list(action = act, id = NA_integer_)
  if (!nzchar(a))                          return(no("keep"))   # Enter never moves an id
  if (grepl("^[0-9]+$", a)) {
    id <- suppressWarnings(as.integer(a))
    return(if (!is.na(id) && id > 0L) list(action = "id", id = id) else no("unclear"))
  }
  if (a %in% c("keep", "k"))               return(no("keep"))
  if (answer_is(raw, c("no", "skip")))     return(no("keep"))
  if (answer_is(raw, "quit"))              return(no("quit"))
  if (isTRUE(has_offer) && (a %in% c("take", "t") || answer_is(raw, "yes")))
                                           return(no("take"))
  no("unclear")
}

#' The retirements that can be offered as a straight choice
#'
#' Exactly one replacement: a swap or a lump, so both bees can go on screen and the
#' answer is take-it-or-keep-it.
#'
#' @param changed What `taxon_changes()` returned.
#' @return The askable subset.
taxon_changes_askable <- function(changed) {
  if (!nrow(changed)) return(changed)
  one <- !is.na(changed$replaced_by) & !grepl(",", changed$replaced_by, fixed = TRUE)
  changed[changed$change == "retired" & one, , drop = FALSE]
}

#' The retirements only a person can settle
#'
#' Every retirement that is not askable: a split (several replacements), or one where
#' iNaturalist named no replacement at all. Both are reported with links rather than
#' asked about, because there is no single right answer to put on screen. Defined as
#' the complement of `taxon_changes_askable()` so a retirement cannot fall between the
#' two -- it did once, and the unnamed case printed with no guidance whatsoever.
#'
#' @param changed What `taxon_changes()` returned.
#' @return The subset needing a human.
taxon_changes_manual <- function(changed) {
  if (!nrow(changed)) return(changed)
  one <- !is.na(changed$replaced_by) & !grepl(",", changed$replaced_by, fixed = TRUE)
  changed[changed$change == "retired" & !one, , drop = FALSE]
}

# ------------------------------------------------------------
# resolve_taxonomy(): build the taxon_id -> ranked-name (+ subgenus /
# complex / complex_taxon_id) map for a set of taxon ids. It first BATCH-
# prefetches every uncached taxon (few requests), then reads each taxon
# straight from the cache -- so this makes NO per-taxon API calls. A taxon
# the batch didn't return (inactive/invalid id) is left with NA ranks rather
# than triggering an extra single request.
# ------------------------------------------------------------
#' Full rank columns for a set of taxon ids
#'
#' @param con An open cache connection.
#' @param taxon_ids Ids to resolve.
#' @param request_fn Injection point for the API call.
#' @param throttle Seconds to wait between requests.
#' @param verbose Print progress.
#' @return One row per id with every rank column filled from the ancestry.
resolve_taxonomy <- function(con, taxon_ids, request_fn = inat_request,
                             throttle = INAT_THROTTLE_SEC, verbose = TRUE) {
  taxon_ids <- unique(taxon_ids[!is.na(taxon_ids)])
  if (length(taxon_ids) == 0) {
    return(parse_taxon_ranks(list(id = NA_integer_))[0, ])
  }
  prefetch_taxa(con, taxon_ids, request_fn = request_fn, throttle = throttle, verbose = verbose)

  rows <- vector("list", length(taxon_ids))
  for (i in seq_along(taxon_ids)) {
    taxon <- taxon_cache_get(con, taxon_cache_key_id(taxon_ids[[i]]))  # cache-only
    rows[[i]] <- if (is.null(taxon)) parse_taxon_ranks(list(id = taxon_ids[[i]]))
                 else parse_taxon_ranks(taxon)
  }
  dplyr::bind_rows(rows)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
