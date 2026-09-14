# =============================================================
# reference/refresh/enrich_lookups.R
# beescabr -- SHARED enrichment resolvers, run at DATA-CLEANING time (not analysis).
#
# One place that turns online reference lookups into cached, bakeable columns:
#   * IUCN Red List category  per bee species   (rredlist, needs a free token)
#   * common name             per plant genus   (public iNaturalist taxa API, no token)
#
# The clean scripts + checklist builders call the enrich_*() helpers to BAKE these
# straight into their output tables, so the analysis layer just reads columns and never
# touches the network. Both resolvers are:
#   * INCREMENTAL -- only species/genera missing from the cache are fetched (fast reruns);
#     pass force = TRUE to re-check everything (the standalone refresh_* tools do this).
#   * OFFLINE-SAFE -- no token / no internet -> the cache is used as-is and NEVER clobbered;
#     unresolved rows fall back to "NE" / the Latin genus, so cleaning always completes.
#
# Caches (also the single fetch ledger, shared across the two bee tables):
#   data/checklists/iucn/iucn_status_generated.csv           (scientific_name, iucn_code, ...)
#   data/checklists/plants/plant_genus_common_generated.csv  (genus, common_name, source, ...)
#
# Depends on: dplyr, stringr; httr2 (common names); rredlist (IUCN, optional). + config.R.
# =============================================================

# rredlist (rOpenSci) is the IUCN Red List client. Same auto-install pattern the analysis
# scripts use, so a fresh machine does not have to discover the requirement from a skipped
# refresh. It is on CRAN; the official IUCN package is GitHub-only and we deliberately
# stay on rredlist (see dev-docs/TODO.md).
# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source('scripts/config.R')
beescabr_require()

suppressPackageStartupMessages({ library(dplyr); library(stringr) })
if (!exists("PATHS")) source('scripts/config.R')
if (!exists("cred_get")) source('scripts/utils/credentials.R')

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a)) ||
                            (is.character(a) && length(a) == 1 && !nzchar(a))) b else a

# =====================================================================================
# IUCN Red List status  (per bee species)
# =====================================================================================
IUCN_DIR        <- "data/checklists/iucn"
IUCN_CACHE_FILE <- file.path(IUCN_DIR, "iucn_status_generated.csv")
IUCN_VERSION_FILE <- file.path(IUCN_DIR, "iucn_redlist_version_generated.txt")  # the edition we cited
if (!exists("citation_save_version")) source('scripts/website/citations.R')
IUCN_SECRET     <- "data/secrets/iucn_api.env"          # gitignored -- paste token here
IUCN_CODE_NAME  <- c(EX = "Extinct", EW = "Extinct in the Wild", RE = "Regionally Extinct",
                     CR = "Critically Endangered", EN = "Endangered", VU = "Vulnerable",
                     NT = "Near Threatened", LC = "Least Concern", DD = "Data Deficient",
                     NE = "Not Evaluated", LR = "Lower Risk")
# a few species IUCN assesses under a different accepted name (synonyms)
IUCN_SYNONYM    <- c("Bombus sonorus"      = "Bombus pensylvanicus",
                     "Bombus californicus" = "Bombus fervidus")
.iucn_name_of <- function(code) { n <- unname(IUCN_CODE_NAME[toupper(code)]); ifelse(is.na(n), code, n) }

.iucn_token <- function() {
  # A free personal key. Ask rather than silently skipping the conservation status: a blank
  # IUCN column looks the same as "nothing is threatened", which is the wrong impression.
  if (exists("cred_get")) {
    if (!nzchar(Sys.getenv("IUCN_REDLIST_KEY")) && !file.exists(IUCN_SECRET)) {
      message("")
      message("  The IUCN Red List key is not set on this machine.")
      message("  It fills the conservation status column on the field guides (the codes")
      message("  NE, LC, NT, VU, EN, CR). Without it that column is blank, which reads as")
      message("  'nothing is threatened' rather than 'we did not check'.")
      message("  The key is FREE: request one at https://api.iucnredlist.org")
      message("  (approval usually arrives by email within a day or two).")
    }
    return(cred_get("IUCN_REDLIST_KEY", IUCN_SECRET, what = "IUCN Red List API key"))
  }
  k <- Sys.getenv("IUCN_REDLIST_KEY")
  if (nzchar(k)) return(k)
  if (!file.exists(IUCN_SECRET)) return("")
  ln <- readLines(IUCN_SECRET, warn = FALSE); ln <- ln[grepl("^\\s*IUCN_REDLIST_KEY\\s*=", ln)]
  if (!length(ln)) return("")
  trimws(gsub('^["\']|["\']$', "", trimws(sub("^\\s*IUCN_REDLIST_KEY\\s*=\\s*", "", ln[length(ln)]))))
}

.iucn_read_cache <- function() {
  if (!file.exists(IUCN_CACHE_FILE))
    return(data.frame(scientific_name = character(0), iucn_code = character(0),
                      iucn_category = character(0), assessment_year = character(0),
                      source = character(0), retrieved_on = character(0), stringsAsFactors = FALSE))
  # colClasses="character" so a numeric-looking assessment_year isn't read back as <integer>
  # (that mismatched the fetched rows' type and broke bind_rows during an incremental refresh)
  d <- read.csv(IUCN_CACHE_FILE, stringsAsFactors = FALSE, check.names = FALSE, colClasses = "character")
  for (c in c("scientific_name","iucn_code","iucn_category","assessment_year","source","retrieved_on"))
    if (!c %in% names(d)) d[[c]] <- NA_character_
  d
}

# Is this failure an authentication problem (bad/expired/revoked key) rather than a
# network blip? Worth naming separately: the fix is different and the operator can act on it.
.iucn_is_auth_error <- function(msg) {
  if (!length(msg) || is.na(msg)) return(FALSE)
  grepl("401|403|not valid|unauthor|forbidden", msg, ignore.case = TRUE)
}

# Latest global category for one species.
#
# CRITICAL: a FAILED call must not look like an answer. This used to return "NE" when the
# request errored, which is the same value IUCN returns for a species it has genuinely not
# assessed. An expired key would therefore have relabelled every bee "Not Evaluated" --
# quietly erasing Bombus crotchii (Endangered) from the public pages. A failure now returns
# ok = FALSE with code = NA, and the caller keeps whatever the cache already held.
# fetch_fn is injectable so the failure paths are testable without touching the network.
# rredlist 1.1.1 cannot report "this species has no assessments". rl_species_latest()
# warns "Returning the latest assessment across all scopes" and then throws
# "incorrect number of dimensions" sorting a result that is empty. Most native bees
# have never been assessed, so this fires constantly: the first live run called it 74
# failures out of 79 and advised trying again later, which would never have helped.
#
# rl_species() handles the same species fine and reports zero assessments, so it is
# used to tell the two apart. Zero assessments is Not Evaluated -- a real answer.
.IUCN_EMPTY_ERR <- "incorrect number of dimensions"

.iucn_fetch_one <- function(binom, key, fetch_fn = NULL, probe_fn = NULL) {
  q  <- if (binom %in% names(IUCN_SYNONYM)) unname(IUCN_SYNONYM[binom]) else binom
  fn <- if (is.null(fetch_fn))
    function(...) rredlist::rl_species_latest(genus = word(q, 1), species = word(q, 2),
                                              key = key, parse = TRUE)
  else fetch_fn
  probe <- if (is.null(probe_fn))
    function(...) rredlist::rl_species(genus = word(q, 1), species = word(q, 2),
                                       key = key, parse = FALSE)
  else probe_fn
  err <- NA_character_
  res <- tryCatch(fn(), error = function(e) { err <<- conditionMessage(e); NULL })
  if (is.null(res) && !is.na(err) && grepl(.IUCN_EMPTY_ERR, err, fixed = TRUE)) {
    # crash-on-empty, or a genuine parse problem? Only the count settles it.
    n <- tryCatch(length(probe()$assessments), error = function(e) NA_integer_)
    if (!is.na(n) && n == 0L)
      return(list(ok = TRUE, error = NA_character_, code = "NE", year = "", note = ""))
  }
  if (is.null(res))
    return(list(code = NA_character_, year = NA_character_, note = "", ok = FALSE, error = err))
  code <- tryCatch(res$red_list_category$code, error = function(e) NULL)
  year <- tryCatch(res$year_published %||% res$assessment_date, error = function(e) NA)
  code <- toupper(code %||% "NE")
  list(ok = TRUE, error = NA_character_, code = code, year = as.character(year %||% ""),
       note = if (q != binom && code != "NE") sprintf(" (as %s)", q) else "")
}

# What to print when the Red List cannot be reached. A pure function because the
# braces here were wrong once: `if (!nzchar(key))` guarded only the first of the
# token block's two lines, so the second printed even when a token WAS set, and an
# operator missing only an R package was sent hunting for credentials they had.
# The two causes have completely different fixes, so each is reported on its own.
.iucn_skip_note <- function(need_n, has_pkg, has_key) {
  c(sprintf(paste("  Conservation status: %d bee%s have never been looked up, and could not",
                  "be this run."),
            need_n, if (need_n == 1L) "" else "s"),
    "  They will read \"Not Evaluated\", which is not the same as \"not threatened\".",
    if (!has_pkg)
      "  The rredlist package is missing. Run: source('scripts/utils/install_requirements.R')",
    if (!has_key) c(
      "  There is no IUCN token on this computer. One is free from",
      "  https://api.iucnredlist.org -- put it in data/secrets/iucn_api.env,",
      "  then run the pipeline again."))
}

#' The status pass could not run at all
#'
#' enrich_iucn_columns() writes "NE" / "Not Evaluated" onto EVERY bee when the lookup
#' returns nothing, so a genuinely Endangered bee reaches the public field guide
#' looking unassessed. The old message was two words and an R error.
#'
#' @param err Why it failed.
#' @return Lines to print.
.iucn_skipped_note <- function(err) c(
  paste0("  !! Conservation status could not be fetched: ", err),
  "  Every bee in this run reads \"Not Evaluated\" as a result -- including any that",
  "  are genuinely threatened. Do not publish the field guides from this run.",
  "  Fix the problem above and run the pipeline again.")

#' Some lookups worked, some did not
#' @param n_fail,n_total Counts.
#' @param auth TRUE when the key itself was rejected.
#' @return Lines to print.
.iucn_partial_note <- function(n_fail, n_total, auth) c(
  sprintf("  Conservation status: %d of %d bees came back; %d did not.",
          n_total - n_fail, n_total, n_fail),
  "  The ones that failed keep whatever status they already had -- unchanged, not",
  "  reset. Nothing is wrong with the ones that worked.",
  if (auth)
    "  The token was rejected. Check data/secrets/iucn_api.env, then run again."
  else
    "  Usually the Red List was briefly unreachable. Try again later; nothing is lost.")

#' Where the status cache lives
#' @param n Species in it.
#' @param path The file.
#' @return One line.
.iucn_cache_note <- function(n, path)
  sprintf("  Conservation status on file for %s bees -> %s", format(n, big.mark = ","), path)

#' Plant common names that could not be fetched
#' @param n How many genera.
#' @return Lines to print.
.pgc_unresolved_note <- function(n) c(
  sprintf("  Common names: %d plant genus name%s could not be fetched from iNaturalist.",
          n, if (n == 1L) "" else "s"),
  "  Those plants show their Latin name only. Nothing else is affected.")

# Resolve IUCN status for a vector of "Genus species" names. Returns the FULL cache-backed
# table (data.frame). Incremental + offline-safe; writes the cache when it learns something.
resolve_iucn <- function(species, force = FALSE, verbose = TRUE) {
  species <- unique(str_squish(species))
  species <- species[!is.na(species) & grepl(" ", species)]          # species-rank binomials only
  cache   <- .iucn_read_cache()
  need <- if (force) species else setdiff(species, cache$scientific_name)
  key  <- .iucn_token()

  has_pkg <- requireNamespace("rredlist", quietly = TRUE)   # installed at load; see top of file
  can_net <- length(need) > 0 && nzchar(key) && has_pkg

  # Say WHICH thing is missing. "no token / rredlist missing" left the operator guessing,
  # and the two have completely different fixes.
  if (length(need) > 0 && !can_net && verbose)
    for (ln in .iucn_skip_note(length(need), has_pkg, nzchar(key))) message(ln)

  if (can_net) {
    if (verbose) message(sprintf("  IUCN: fetching %d species from the Red List (v4)...", length(need)))
    fetched <- lapply(seq_along(need), function(i) {
      r <- .iucn_fetch_one(need[i], key)
      if (verbose && i %% 10 == 0) message(sprintf("    ...%d/%d", i, length(need)))
      # IUCN's own client package (IUCN-UK/iucnredlist) recommends wait_time >= 0.5s to
      # avoid rate limiting; they warn that heavy use can get a token throttled or revoked.
      # 0.34s was faster than their guidance.
      Sys.sleep(0.5)
      d <- data.frame(scientific_name = need[i], iucn_code = r$code,
                 iucn_category = paste0(.iucn_name_of(r$code), r$note),
                 assessment_year = as.character(r$year), source = "IUCN Red List API v4 (rredlist)",
                 retrieved_on = as.character(Sys.Date()), stringsAsFactors = FALSE)
      attr(d, "error") <- r$error %||% NA_character_
      d
    })
    new <- do.call(rbind, fetched)

    # Drop lookups that FAILED. Writing them would cache a network or auth problem as a
    # scientific claim ("Not Evaluated"), and the cache is what the public pages read.
    failed <- is.na(new$iucn_code)
    if (any(failed)) {
      auth <- any(vapply(fetched[failed], function(x) .iucn_is_auth_error(attr(x, "error")), TRUE))
      if (verbose) {
        for (ln in .iucn_partial_note(sum(failed), nrow(new), auth)) message(ln)
        if (all(failed) && auth) {
          message("  Get a new token at https://api.iucnredlist.org (Your account -> Cycle token)")
          message("  and put it in data/secrets/iucn_api.env as IUCN_REDLIST_KEY=...")
        }
      }
      new <- new[!failed, , drop = FALSE]
    }
    if (!nrow(new)) {
      if (verbose) message("  IUCN: nothing new to cache.")
      return(invisible(cache))
    }
    old_assessed <- sum(toupper(cache$iucn_code) != "NE", na.rm = TRUE)
    # offline guard applies only to a FULL (force) re-pull: if EVERY species came back NE
    # yet the cache held real assessments, the API was almost certainly unreachable, so keep
    # the cache rather than wipe it. In INCREMENTAL mode we always write -- existing assessed
    # rows are preserved by the merge, and caching the genuinely-NE new species stops them
    # from being re-queried on every run (a single unassessed species used to re-fetch forever).
    if (force && sum(new$iucn_code != "NE") == 0 && old_assessed > 0) {
      if (verbose) for (ln in .iucn_partial_note(length(need), length(need), auth = FALSE)) message(ln)
    } else {
      merged <- if (force) new else bind_rows(cache[!cache$scientific_name %in% new$scientific_name, , drop = FALSE], new)
      merged <- merged[order(merged$scientific_name), , drop = FALSE]
      dir.create(IUCN_DIR, recursive = TRUE, showWarnings = FALSE)
      write.csv(merged, IUCN_CACHE_FILE, row.names = FALSE)
      cache <- merged
      if (verbose) message(.iucn_cache_note(nrow(cache), IUCN_CACHE_FILE))
      # Record the Red List EDITION these statuses came from. IUCN requires the version in
      # any citation, and the public pages must not have to make a live call to print it.
      v <- tryCatch(rredlist::rl_version(key = key), error = function(e) NULL)
      v <- if (is.list(v)) as.character(v[[1]] %||% "") else as.character(v %||% "")
      if (nzchar(v)) {
        citation_save_version(IUCN_VERSION_FILE, v)
        if (verbose) message("  IUCN: Red List version ", v, " recorded for citation.")
      }
    }
  }
  cache
}

# Add iucn_code / iucn_category / assessment_year columns to a cleaned bee table.
# Only species/subspecies rows are looked up; coarser rows get "NE". Never errors out the
# caller: any failure leaves the table unchanged-but-columned (all NE).
enrich_iucn_columns <- function(df, species_col = "scientific_name", rank_col = "taxon_rank") {
  if (!species_col %in% names(df)) return(df)
  sp_rows <- if (rank_col %in% names(df)) tolower(str_squish(df[[rank_col]])) %in% c("species", "subspecies")
             else rep(TRUE, nrow(df))
  binoms  <- ifelse(sp_rows, str_squish(df[[species_col]]), NA_character_)
  tab <- tryCatch(resolve_iucn(binoms[!is.na(binoms)]),
                  error = function(e) {
                    for (ln in .iucn_skipped_note(conditionMessage(e))) message(ln)
                    NULL })
  code_map <- if (is.null(tab)) character(0) else setNames(toupper(tab$iucn_code), tab$scientific_name)
  cat_map  <- if (is.null(tab)) character(0) else setNames(tab$iucn_category,        tab$scientific_name)
  yr_map   <- if (is.null(tab)) character(0) else setNames(tab$assessment_year,      tab$scientific_name)
  df$iucn_code       <- ifelse(is.na(binoms), "NE", unname(code_map[binoms] %||% NA)); df$iucn_code[is.na(df$iucn_code)] <- "NE"
  df$iucn_category   <- ifelse(is.na(binoms), "Not Evaluated", unname(cat_map[binoms]));  df$iucn_category[is.na(df$iucn_category)] <- "Not Evaluated"
  df$iucn_assessment_year <- ifelse(is.na(binoms), NA_character_, unname(yr_map[binoms]))
  df
}

# =====================================================================================
# Plant-genus common names  (public iNaturalist taxa API, no token)
# =====================================================================================
PGC_DIR        <- "data/checklists/plants"
PGC_CACHE_FILE <- file.path(PGC_DIR, "plant_genus_common_generated.csv")

.pgc_read_cache <- function() {
  if (!file.exists(PGC_CACHE_FILE))
    return(data.frame(genus = character(0), common_name = character(0),
                      source = character(0), retrieved_on = character(0), stringsAsFactors = FALSE))
  read.csv(PGC_CACHE_FILE, stringsAsFactors = FALSE, check.names = FALSE)
}

# local seed: names the in-park plant taxonomy files already carry (via plant_names.R)
.pgc_local_map <- function() {
  if (!exists(".plant_common_map")) {
    pn <- "scripts/analysis/shared/plant_names.R"
    if (file.exists(pn)) suppressWarnings(try(source(pn), silent = TRUE))
  }
  if (exists(".plant_common_map")) tryCatch(.plant_common_map(), error = function(e) character(0)) else character(0)
}

.pgc_api_reachable <- function() {
  isTRUE(tryCatch({
    httr2::request(paste0(INAT_BASE_URL, "taxa")) |>
      httr2::req_url_query(q = "Salvia", rank = "genus", per_page = 1) |>
      httr2::req_user_agent(INAT_USER_AGENT) |> httr2::req_timeout(12) |>
      httr2::req_error(is_error = function(resp) FALSE) |> httr2::req_perform()
    TRUE
  }, error = function(e) FALSE))
}

.pgc_fetch_one <- function(g) {
  r <- tryCatch(
    httr2::request(paste0(INAT_BASE_URL, "taxa")) |>
      httr2::req_url_query(q = g, rank = "genus", per_page = 5) |>
      httr2::req_user_agent(INAT_USER_AGENT) |> httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 4,
                       is_transient = function(resp) httr2::resp_status(resp) %in% c(429L,500L,502L,503L,504L),
                       backoff = function(i) min(30, 3 * 2^(i - 1))) |>
      httr2::req_perform() |> httr2::resp_body_json(),
    error = function(e) NULL)
  if (is.null(r) || is.null(r$results)) return(NA_character_)
  for (t in r$results) if (identical(t$name %||% "", g) && identical(t$rank %||% "", "genus"))
    return(t$preferred_common_name %||% NA_character_)
  NA_character_
}

# Resolve a common name for each plant genus. Returns a named vector (genus -> common name,
# Title-cased; NA where none). Incremental + offline-safe (never shrinks a good cache).
resolve_plant_common <- function(genera, force = FALSE, verbose = TRUE) {
  genera  <- unique(str_squish(genera)); genera <- genera[!is.na(genera) & genera != ""]
  local   <- .pgc_local_map()                                   # RAW-spelling local seed
  cache   <- .pgc_read_cache()
  cached_map <- setNames(str_squish(cache$common_name), str_squish(cache$genus))
  cached_map <- cached_map[!is.na(cached_map) & nzchar(cached_map)]
  have    <- union(names(cached_map), names(local)[!is.na(local) & nzchar(local %||% "")])
  need    <- if (force) genera else setdiff(genera, have)

  fetched <- character(0)
  if (length(need) > 0 && requireNamespace("httr2", quietly = TRUE) && .pgc_api_reachable()) {
    if (verbose) message(sprintf("  plant common names: fetching %d genera from iNaturalist...", length(need)))
    for (i in seq_along(need)) {
      cn <- .pgc_fetch_one(need[i]); if (!is.na(cn) && nzchar(cn)) fetched[need[i]] <- cn
      if (verbose && i %% 10 == 0) message(sprintf("    ...%d/%d", i, length(need)))
      Sys.sleep(0.5)
    }
    if (verbose) message(sprintf("  plant common names: fetched %d new.", length(fetched)))
  } else if (length(need) > 0 && verbose) {
    for (ln in .pgc_unresolved_note(length(need))) message(ln)
  }

  # assemble & persist the cache: prior cache + local seed + newly fetched (fetched wins on refresh)
  acc <- list()
  for (g in names(cached_map)) acc[[g]] <- list(v = unname(cached_map[g]), s = "cache (prior fetch)")
  for (g in names(local)) if (!is.na(local[g]) && nzchar(local[g] %||% "")) acc[[g]] <- list(v = unname(local[g]), s = "in-park plant taxonomy")
  for (g in names(fetched)) acc[[g]] <- list(v = unname(fetched[g]), s = "iNaturalist taxa API")
  if (length(acc) > 0) {
    out <- data.frame(genus = names(acc),
                      common_name = vapply(acc, function(x) x$v, character(1)),
                      source = vapply(acc, function(x) x$s, character(1)),
                      retrieved_on = as.character(Sys.Date()), stringsAsFactors = FALSE)
    out <- out[order(out$genus), , drop = FALSE]
    if (nrow(out) >= nrow(cache)) {                              # never shrink a good cache
      dir.create(PGC_DIR, recursive = TRUE, showWarnings = FALSE)
      write.csv(out, PGC_CACHE_FILE, row.names = FALSE)
    }
  }

  # final map for the requested genera (Title-cased), local + cache + fetched
  full <- c(setNames(unname(local[!is.na(local)]), names(local)[!is.na(local)]),
            cached_map, fetched)
  full <- full[!duplicated(names(full), fromLast = TRUE)]        # fetched/cache override earlier
  tc <- function(x) str_to_title(str_squish(x))
  setNames(ifelse(genera %in% names(full), tc(full[genera]), NA_character_), genera)
}

# Add a plant_genus_common column (genus-level common name) to a cleaned plant table.
enrich_plant_common_column <- function(df, genus_col = "plant_genus") {
  if (!genus_col %in% names(df)) { gc <- if ("genus" %in% names(df)) "genus" else return(df); genus_col <- gc }
  m <- tryCatch(resolve_plant_common(df[[genus_col]]),
                error = function(e) { message("  !! common-name enrichment skipped: ", conditionMessage(e)); NULL })
  df$plant_genus_common <- if (is.null(m)) NA_character_ else unname(m[str_squish(df[[genus_col]])])
  df
}
