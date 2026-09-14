# =============================================================
# reference/prompts/manual_overrides.R
# beescabr -- user-curated taxon-id overrides for NAME CHANGES / synonyms the automated
# iNat name-search can't bridge (e.g. Holway 'Holcopasites minima' == iNat 'minimus').
#
# THE LOOP (per Brandi):
#   1. Each run, whatever the resolvers still can't find is written to a REVIEW WORKLIST
#      (cabr_taxon_ids_needs_review.csv) with an iNaturalist search link -- the pipeline
#      asking you to look those few up by hand. (One list, at the lookup stage, which by
#      then already contains the Holway taxa too.)
#   2. You find the taxon on iNat and record rank + name + the correct taxon_id (+ its
#      current iNat name) in manual_taxon_overrides.csv.
#   3. Every future run READS that file and applies your answer at BOTH the Holway reference
#      build AND the taxonomy lookup build -- so the id (and corrected name) land in both
#      tables, and survive the from-scratch rebuild that would wipe a hand-edit.
#
# The Holway reference table and the taxonomy lookup share the same rank/genus/species/...
# shape, so ONE apply function serves both. Pure + unit-tested in test-manual-overrides.R.
# Depends on: dplyr, readr, stringr (+ stringi if present, for diacritics).
# =============================================================
suppressWarnings(suppressMessages({ library(dplyr); library(readr); library(stringr) }))

`%||%` <- if (exists("%||%")) `%||%` else function(a, b) if (is.null(a) || length(a) == 0) b else a

# the shared answer vocabulary -- one set of words for every prompt in the pipeline
if (!exists("answer_is")) source("scripts/utils/answers.R")

MANUAL_OVERRIDES_PATH <- "data/reference/hand_curated/manual_taxon_overrides.csv"   # user-curated answers
TAXON_REVIEW_PATH     <- "data/reference/generated/cabr_taxon_ids_needs_review.csv"  # the prompt / worklist
RMI_CACHE_PATH        <- "data/reference/generated/resolved_missing_ids.csv"     # the resolver's verdict cache
MO_REVIEW_RANKS       <- c("genus", "subgenus", "complex", "species", "subspecies")

.mo_titlecase1 <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", x, paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))
}

# .mo_norm(): diacritic-insensitive, lowercase, letters+spaces only ("Schönnherria" -> "schonnherria").
.mo_norm <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- if (requireNamespace("stringi", quietly = TRUE))
    stringi::stri_trans_general(x, "Latin-ASCII") else x
  x[is.na(x)] <- ""
  x <- tolower(x); x <- gsub("[^a-z ]", " ", x); trimws(gsub("\\s+", " ", x))
}

# .mo_name_at_rank(): each row's OWN name at its rank -- genus for genus, "genus species" for a
# species, "genus species subspecies" for a subspecies, the subgenus/complex value otherwise.
# This is the key the override + worklist match on (same string the user sees in the worklist).
.mo_name_at_rank <- function(df) {
  col <- function(n) if (n %in% names(df)) as.character(df[[n]]) else rep(NA_character_, nrow(df))
  g <- col("genus"); sp <- col("species"); ss <- col("subspecies")
  sg <- str_remove_all(col("subgenus"), "[()]"); cx <- col("complex")
  rk <- tolower(trimws(col("rank")))
  nz <- function(x) !is.na(x) & trimws(x) != ""   # incomplete names -> NA (never match junk rows)
  dplyr::case_when(
    rk == "genus"      & nz(g)                   ~ trimws(g),
    rk == "subgenus"   & nz(sg)                  ~ trimws(sg),
    rk == "complex"    & nz(cx)                  ~ trimws(cx),
    rk == "species"    & nz(g) & nz(sp)          ~ trimws(paste(g, sp)),
    rk == "subspecies" & nz(g) & nz(sp) & nz(ss) ~ trimws(paste(g, sp, ss)),
    TRUE ~ NA_character_)
}

# load_manual_overrides(): read the curated CSV (rank,name,taxon_id,correct_name,note). Missing file
# -> empty. Rows with a blank taxon_id are dropped (no answer yet -> nothing to apply).
load_manual_overrides <- function(path = MANUAL_OVERRIDES_PATH) {
  empty <- tibble(rank = character(), name = character(), taxon_id = integer(),
                  correct_name = character(), note = character())
  if (!file.exists(path)) return(empty)
  ov <- tryCatch(suppressWarnings(read_csv(path, show_col_types = FALSE)), error = function(e) NULL)
  if (is.null(ov) || !nrow(ov)) return(empty)
  for (c in c("rank", "name", "taxon_id", "correct_name", "note"))
    if (!c %in% names(ov)) ov[[c]] <- if (c == "taxon_id") NA_integer_ else NA_character_
  ov$taxon_id <- suppressWarnings(as.integer(ov$taxon_id))
  ov |> filter(!is.na(taxon_id), !is.na(name), trimws(name) != "")
}

# apply_manual_overrides(): PURE. For each df row whose (rank, name-at-rank) matches an override,
# set taxon_id and -- when correct_name is given -- set scientific_name to it and correct the rank's
# epithet columns to match (Brandi: correct to iNat's current name). A resolved override also marks
# the row resolved / verified when those columns exist. Works on the Holway reference table AND the
# taxonomy lookup. Returns df with an attribute "n_applied".
apply_manual_overrides <- function(df, overrides = NULL) {
  if (is.null(overrides)) overrides <- load_manual_overrides()
  if (is.null(overrides) || !nrow(overrides) || !"rank" %in% names(df)) {
    attr(df, "n_applied") <- 0L; return(df)
  }
  if (!"taxon_id" %in% names(df))        df$taxon_id <- NA_integer_
  if (!"scientific_name" %in% names(df)) df$scientific_name <- NA_character_
  df$taxon_id <- suppressWarnings(as.integer(df$taxon_id))
  key  <- paste(tolower(trimws(as.character(df$rank))),        .mo_norm(.mo_name_at_rank(df)))
  okey <- paste(tolower(trimws(as.character(overrides$rank))), .mo_norm(overrides$name))
  n_applied <- 0L
  for (j in seq_len(nrow(overrides))) {
    for (i in which(key == okey[j])) {
      df$taxon_id[i] <- overrides$taxon_id[j]
      cn <- overrides$correct_name[j]
      if (!is.na(cn) && trimws(cn) != "") {
        df$scientific_name[i] <- trimws(cn)
        w <- strsplit(trimws(cn), "\\s+")[[1]]
        r <- tolower(trimws(as.character(df$rank[i])))
        if (r == "genus"      && length(w) >= 1 && "genus"   %in% names(df)) df$genus[i] <- w[1]
        if (r == "subgenus"   && length(w) >= 1 && "subgenus"%in% names(df)) df$subgenus[i] <- w[length(w)]
        if (r == "complex"    && "complex" %in% names(df))                   df$complex[i] <- trimws(cn)
        if (r == "species"    && length(w) >= 2) {
          if ("genus"   %in% names(df)) df$genus[i]   <- w[1]
          if ("species" %in% names(df)) df$species[i] <- w[2]
        }
        if (r == "subspecies" && length(w) >= 3) {
          if ("genus"      %in% names(df)) df$genus[i]      <- w[1]
          if ("species"    %in% names(df)) df$species[i]    <- w[2]
          if ("subspecies" %in% names(df)) df$subspecies[i] <- w[3]
        }
      }
      if ("resolved" %in% names(df)) df$resolved[i] <- TRUE
      if ("verified" %in% names(df)) df$verified[i] <- TRUE
      n_applied <- n_applied + 1L
    }
  }
  attr(df, "n_applied") <- n_applied
  df
}

# .mo_open_worklist(): the resolver's not_found taxa (from resolved_missing_ids.csv) that are NOT
# yet answered in the overrides -> tibble(rank, name, inat_search_url), title-cased + sorted. Shared
# by the worklist writer and the interactive prompt so both show the exact same open set.
.mo_open_worklist <- function(cache_path = RMI_CACHE_PATH, overrides = NULL) {
  empty <- tibble(rank = character(), name = character(), inat_search_url = character())
  cache <- if (file.exists(cache_path))
    tryCatch(suppressWarnings(read_csv(cache_path, show_col_types = FALSE)), error = function(e) NULL) else NULL
  if (is.null(cache) || !nrow(cache) || !all(c("key", "status") %in% names(cache))) return(empty)
  nf <- cache[grepl("not_found|ambiguous", cache$status, ignore.case = TRUE), , drop = FALSE]
  if (!nrow(nf)) return(empty)
  # key format from resolve_missing_ids.R: "rank|name|parent". Recover rank + name.
  parts <- strsplit(as.character(nf$key), "|", fixed = TRUE)
  rank  <- vapply(parts, function(p) if (length(p) >= 1) p[1] else NA_character_, character(1))
  name  <- vapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))
  if (is.null(overrides)) overrides <- load_manual_overrides()
  answered <- if (nrow(overrides)) paste(tolower(trimws(overrides$rank)), .mo_norm(overrides$name)) else character(0)
  key  <- paste(tolower(trimws(rank)), .mo_norm(name))
  keep <- !is.na(name) & trimws(name) != "" & !(key %in% answered)   # drop already-answered taxa
  if (!any(keep)) return(empty)
  # Bind the filtered vectors ONCE up front. Inlining name[keep] a second time inside
  # tibble() would bind to the freshly-built (already-filtered) `name` column and re-index
  # it with the full-length `keep`, shifting the URL off its name (and tacking on an NA).
  rk <- tolower(trimws(rank[keep]))
  nm <- trimws(name[keep])
  tibble(
    rank = rk,
    name = .mo_titlecase1(nm),                           # "holcopasites minima" -> "Holcopasites minima"
    inat_search_url = paste0("https://www.inaturalist.org/taxa/search?q=",
                             vapply(nm, function(s) utils::URLencode(s, reserved = TRUE),
                                    character(1), USE.NAMES = FALSE))
  ) |> distinct(rank, name, .keep_all = TRUE) |> arrange(rank, name)
}

# write_review_worklist(): the file version of the prompt -- writes the open not_found set to
# cabr_taxon_ids_needs_review.csv with blank taxon_id / correct_name columns to fill in. Always runs
# (the non-interactive fallback for the interactive prompt below).
write_review_worklist <- function(cache_path = RMI_CACHE_PATH, overrides = NULL, path = TAXON_REVIEW_PATH) {
  open <- .mo_open_worklist(cache_path, overrides)
  wl <- tibble(rank = open$rank, name = open$name, taxon_id = NA_integer_,
               correct_name = NA_character_, note = NA_character_, inat_search_url = open$inat_search_url)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(readr::write_csv(wl, path, na = ""))
  message(sprintf("  taxon review worklist: %d bee names need an iNat taxon_id -> %s",
                  nrow(wl), basename(path)))
  invisible(wl)
}

# prompt_missing_taxon_ids(): INTERACTIVE. Walk the open not_found taxa and ASK the user for each
# one's iNaturalist taxon_id (+ optional current name), appending answers to manual_taxon_overrides.csv
# so they apply at BOTH levels on the next build. Mirrors the Holway second-pass prompt; a no-op when
# non-interactive (then the worklist file is the fallback). Returns the count of ids recorded.
#' Explain the fill-missing-ids pass before asking about the first name
#'
#' These names have already been searched for automatically and not found -- the
#' verdict is cached in resolved_missing_ids.csv. Presenting them as open work
#' invites someone to hunt for an id that does not exist. For most, `n` is correct
#' and the only real question is whether the NAME is still valid, which ITIS answers.
#'
#' @param n How many names are being asked about.
#' @return Invisibly NULL; prints.
MO_COLS <- c("rank", "name", "taxon_id", "correct_name", "note")

#' Fold new answers into the overrides file's existing ones
#'
#' PURE. One rule for which answer wins: a newer answer replaces an older one for the
#' same bee, where "the same bee" is rank plus normalized name -- so the same name at
#' two ranks stays two rows. Pulled out of `prompt_missing_taxon_ids()` because the
#' yearly taxon sweep records corrections too, and two copies of this rule would drift.
#'
#' @param new The answers just given.
#' @param existing The overrides file's current rows, or NULL.
#' @return The merged rows, ordered by rank then name.
merge_manual_overrides <- function(new, existing = NULL) {
  cols <- MO_COLS
  if (!is.null(existing)) for (c in cols) if (!c %in% names(existing))
    existing[[c]] <- if (c == "taxon_id") NA_integer_ else NA_character_
  existing <- if (is.null(existing)) new[0, cols] else existing[, cols]
  existing$taxon_id <- suppressWarnings(as.integer(existing$taxon_id))
  bind_rows(new[, cols], existing) |>
    mutate(.k = paste(tolower(trimws(rank)), .mo_norm(name))) |>
    distinct(.k, .keep_all = TRUE) |> select(-.k) |> arrange(rank, name)
}

.mo_banner <- function(n) {
  message("")
  message("  ", n, " checklist bees still have no iNaturalist id.")
  message("")
  message("  These were ALREADY searched for automatically and not found, so this is")
  message("  a second opinion rather than a first look. Some genuinely have no page on")
  message("  iNaturalist -- that is expected and documented, and 'none' is the right answer.")
  message("")
  message("  For each one, two questions:")
  message("    1. Is it on iNaturalist under another name?  The search link is printed")
  message("       with each name; the taxon_id is the number in the address bar.")
  message("    2. Is the name still valid at all?  ITIS, the US government taxonomy")
  message("       database, says whether it is accepted or was renamed:")
  message("       https://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=Scientific_Name")
  message("")
  message("  WHAT TO TYPE")
  message("    345235             a taxon_id you found")
  message("    345235 New name    the id AND a corrected name")
  message("    none               there is no iNaturalist page for this bee")
  message("    skip               not sure. Pressing Enter does the same.")
  message("    quit               stop here, keeping everything entered so far")
  message("")
  # none and skip both fall through to `next` -- nothing is written for either. This
  # sat indented under `skip`, which read as though only skip came back, leaving
  # "none" looking like a permanent answer. It is not attached to either option now.
  message("  Neither none nor skip records anything, so you are asked again next run,")
  message("  in case iNaturalist adds a page for the bee.")
  invisible(NULL)
}

#' Ask a person about the checklist names that still have no iNaturalist id
#'
#' These were already searched for automatically and not found, so this is a second
#' opinion rather than a first look. `.mo_banner()` says so before the first name,
#' and points at ITIS for the separate question of whether the name is still valid.
#'
#' @param cache_path The resolver's verdict cache.
#' @param overrides_path Where answers are written.
#' @param interactive_ok FALSE skips the prompts entirely, for an unattended run.
#' @param prompt_fn Injection point for reading an answer.
#' @return Invisibly, the answers recorded.
prompt_missing_taxon_ids <- function(cache_path = RMI_CACHE_PATH, overrides_path = MANUAL_OVERRIDES_PATH,
                                     interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1",
                                     prompt_fn = readline) {
  if (!isTRUE(interactive_ok)) return(0L)
  open <- .mo_open_worklist(cache_path, load_manual_overrides(overrides_path))
  if (!nrow(open)) return(0L)
  .mo_banner(nrow(open))
  cols <- c("rank", "name", "taxon_id", "correct_name", "note")
  answers <- list()
  for (i in seq_len(nrow(open))) {
    o <- open[i, ]
    message(sprintf("\n[%d/%d] %s  (%s)\n   %s", i, nrow(open), o$name, o$rank, o$inat_search_url))
    raw <- trimws(prompt_fn("   taxon_id (or 'id Current name'), none, or skip: "))
    if (answer_is(raw, "quit")) break
    if (raw == "" || answer_is(raw, c("none", "skip"))) next
    toks <- strsplit(raw, "\\s+")[[1]]
    id <- suppressWarnings(as.integer(toks[1]))
    if (is.na(id)) { message("   (not a number -- skipped)"); next }
    cn <- if (length(toks) > 1) paste(toks[-1], collapse = " ") else NA_character_
    answers[[length(answers) + 1L]] <- tibble(rank = o$rank, name = o$name, taxon_id = id,
                                              correct_name = cn, note = "added via prompt")
  }
  if (!length(answers)) { message("   no ids entered."); return(0L) }
  new <- bind_rows(answers)
  existing <- if (file.exists(overrides_path))
    tryCatch(suppressWarnings(read_csv(overrides_path, show_col_types = FALSE)), error = function(e) NULL) else NULL
  combined <- merge_manual_overrides(new, existing)
  dir.create(dirname(overrides_path), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(readr::write_csv(combined, overrides_path, na = ""))
  message(sprintf("   recorded %d id(s) -> %s (applies at both levels next build)",
                  nrow(new), basename(overrides_path)))
  nrow(new)
}
