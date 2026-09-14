# =============================================================
# reference/taxonomy/resolve_missing_ids.R
# beescabr -- fill MISSING taxon_ids in the taxonomy lookup by looking the taxon up on iNaturalist.
#
# THE RULE (per Brandi): a lookup row that's identified to some rank but has no taxon_id -- take the
# name at that rank, search iNaturalist for it, and take the taxon_id. This catches Holway-only taxa
# that were never OBSERVED on iNat (so no observation ancestry carried their id): genera like
# Megandrena, subgenera like Xylocopa (Schonnherria), etc. -- they exist on iNat, just unobserved in SD.
#
# SAFE BY DESIGN: it only assigns an id on an UNAMBIGUOUS match -- right rank, diacritic-insensitive
# name match, AND the taxon's known parent (genus for infra-generic ranks, family for a genus) present
# in the candidate's iNat ancestry. Anything ambiguous or unfound is LEFT BLANK (a wrong id is worse
# than none). Every lookup is cached to resolved_missing_ids.csv so reruns don't re-hit the API and
# you can audit exactly what was filled (status = filled / not_found / ambiguous).
#
# Called by taxonomy_lookup_build.R (stage 5) on the assembled lookup, before it is written.
# =============================================================
suppressWarnings(suppressMessages({library(dplyr); library(readr)}))

`%||%` <- if (exists("%||%")) `%||%` else function(a, b) if (is.null(a) || length(a) == 0) b else a
RMI_CACHE <- "data/reference/generated/resolved_missing_ids.csv"

# .rmi_norm(): diacritic-insensitive, lowercase, letters+spaces only ("Schönnherria" -> "schonnherria").
# Maps accented letters to their BASE letter (o, not oe) to match Holway's diacritic-stripped names.
.rmi_norm <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- if (requireNamespace("stringi", quietly = TRUE))
    stringi::stri_trans_general(x, "Latin-ASCII")   # ö->o, é->e ... locale-robust
  else suppressWarnings(iconv(x, to = "ASCII//TRANSLIT"))
  x[is.na(x)] <- ""
  x <- tolower(x); x <- gsub("[^a-z ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}
.rmi_lastword <- function(s) { w <- strsplit(s, " ", fixed = TRUE)[[1]]; if (length(w)) w[length(w)] else s }

# pick_taxon_id_by_rank(): PURE. cands = list of list(id, rank, name, ancestor_ids). Return the id
# ONLY on a single unambiguous match at `rank` whose (normalized) name matches and whose ancestry
# contains `parent_id` (skipped when parent_id is NA). Otherwise NA -- never guess.
pick_taxon_id_by_rank <- function(cands, rank, name, parent_id = NA) {
  if (is.null(cands) || !length(cands)) return(NA_integer_)
  want_rk <- tolower(trimws(as.character(rank)))
  want_nm <- .rmi_norm(name); want_lw <- .rmi_lastword(want_nm)
  ok <- vapply(cands, function(c) {
    if (tolower(trimws(as.character(c$rank %||% ""))) != want_rk) return(FALSE)
    cnm <- .rmi_norm(c$name %||% "")
    if (!(cnm == want_nm || .rmi_lastword(cnm) == want_lw)) return(FALSE)
    if (is.na(parent_id)) return(TRUE)
    anc <- suppressWarnings(as.integer(c$ancestor_ids %||% integer(0)))
    as.integer(parent_id) %in% anc
  }, logical(1))
  hits <- cands[ok]
  if (length(hits) == 1) suppressWarnings(as.integer(hits[[1]]$id)) else NA_integer_
}

# normalize one raw iNat /taxa result into the (id, rank, name, ancestor_ids) shape the picker wants
.rmi_cand <- function(t) list(
  id = suppressWarnings(as.integer(t$id %||% NA)),
  rank = as.character(t$rank %||% NA),
  name = as.character(t$name %||% NA),
  ancestor_ids = suppressWarnings(as.integer(unlist(t$ancestor_ids %||% integer(0)))))

#' Is a cached verdict due to be searched again?
#'
#' PURE. The cache used to decide this, and it was permanent: a name searched once and
#' not found was written as "not_found_or_ambiguous" and never looked at again, so a
#' bee iNaturalist published afterwards could never be picked up automatically and the
#' operator's "still have no id" list could only shrink by someone typing an id in by
#' hand. A not-found answer is only true as of the moment it was asked, so it is asked
#' again every run -- it is a handful of names, and it is the only thing that shrinks
#' that list on its own. The cache still records what was last seen; it just no longer
#' decides whether to look.
#'
#' A FOUND id is the exception: it is never re-searched here. iNaturalist does not
#' un-publish a taxon, and a fresh search could only replace a good id with an
#' ambiguous one. (Whether a found id can go stale a DIFFERENT way -- iNaturalist
#' splitting or moving that taxon later -- is not this function's job and is not
#' currently checked anywhere.)
#'
#' @param status The cached verdict ("filled" / "not_found_or_ambiguous").
#' @param season Unused; kept so callers reading a season-stamped cache still work.
#' @return Logical, vectorized: TRUE for rows to search again.
.rmi_stale <- function(status, season = beescabr_season_year()) {
  !(!is.na(status) & trimws(as.character(status)) == "filled")
}

# resolve_missing_taxon_ids(): fill df$taxon_id for rows missing it, by iNat name search. Ranks
# genus/subgenus/complex/species/subspecies. fetch_fn(name) -> raw iNat results (default: live API).
resolve_missing_taxon_ids <- function(df, cache_path = RMI_CACHE, fetch_fn = NULL, verbose = TRUE,
                                      season = beescabr_season_year()) {
  if (is.null(fetch_fn)) fetch_fn <- function(nm) inat_fetch_taxa_by_name(nm)
  df$taxon_id <- suppressWarnings(as.integer(df$taxon_id))
  rk <- as.character(df$rank)
  nm_at <- function(col) if (col %in% names(df)) as.character(df[[col]]) else rep(NA_character_, nrow(df))
  gcol <- nm_at("genus"); spcol <- nm_at("species"); fcol <- nm_at("family")

  # parent-id maps from the ALREADY-resolved rows (genus name -> id ; family name -> id)
  gmap <- df |> filter(rank == "genus",  !is.na(taxon_id)) |> distinct(genus,  .keep_all = TRUE) |>
    transmute(k = tolower(trimws(genus)),  id = taxon_id)
  fmap <- df |> filter(rank == "family", !is.na(taxon_id)) |> distinct(family, .keep_all = TRUE) |>
    transmute(k = tolower(trimws(family)), id = taxon_id)
  gid <- function(g) { i <- gmap$id[match(tolower(trimws(g)), gmap$k)]; if (length(i)) i else NA_integer_ }
  fid <- function(f) { i <- fmap$id[match(tolower(trimws(f)), fmap$k)]; if (length(i)) i else NA_integer_ }

  # search term + parent id per row (only the resolvable ranks)
  term <- rep(NA_character_, nrow(df)); parent <- rep(NA_integer_, nrow(df))
  for (i in seq_len(nrow(df))) {
    r <- rk[i]
    if (r == "genus")           { term[i] <- gcol[i];                       parent[i] <- fid(fcol[i]) }
    else if (r == "subgenus")   { term[i] <- nm_at("subgenus")[i];          parent[i] <- gid(gcol[i]) }
    else if (r == "complex")    { term[i] <- nm_at("complex")[i];           parent[i] <- gid(gcol[i]) }
    else if (r == "species")    { term[i] <- paste(gcol[i], spcol[i]);      parent[i] <- gid(gcol[i]) }
    else if (r == "subspecies") { term[i] <- paste(gcol[i], spcol[i], nm_at("subspecies")[i]); parent[i] <- gid(gcol[i]) }
  }
  term <- gsub("\\(Complex\\)\\s*", "", term)   # complex column may be decorated
  need <- which(is.na(df$taxon_id) & !is.na(term) & trimws(term) != "" & trimws(term) != "NA")
  if (!length(need)) return(df)

  cache <- if (file.exists(cache_path))
    suppressWarnings(read_csv(cache_path, show_col_types = FALSE)) else
    tibble(key = character(), taxon_id = integer(), status = character(),
           checked_season = integer())
  # a cache written before the season stamp existed: every not-found row is due
  if (!"checked_season" %in% names(cache)) cache$checked_season <- NA_integer_
  ck <- function(rank, term, parent) paste(rank, tolower(trimws(term)), parent %||% "", sep = "|")

  n_new <- 0L; n_hit <- 0L; n_again <- 0L
  for (i in need) {
    key <- ck(rk[i], term[i], parent[i])
    c_row <- cache[cache$key == key, ]
    fresh <- nrow(c_row) && !.rmi_stale(c_row$status[1])
    if (fresh) { id <- suppressWarnings(as.integer(c_row$taxon_id[1])) }
    else {
      if (nrow(c_row)) n_again <- n_again + 1L else n_new <- n_new + 1L
      # searched again every run, so the stamp records when it was LAST looked for
      cands <- tryCatch(lapply(fetch_fn(term[i]), .rmi_cand), error = function(e) list())
      id <- pick_taxon_id_by_rank(cands, rk[i], term[i], parent[i])
      # drop the old verdict FIRST: distinct() below keeps the first row for a key, so
      # appending beside a stale row would quietly preserve the stale one.
      cache <- bind_rows(cache[cache$key != key, ],
                         tibble(key = key, taxon_id = id,
                                status = if (is.na(id)) "not_found_or_ambiguous" else "filled",
                                checked_season = as.integer(season)))
    }
    if (!is.na(id)) { df$taxon_id[i] <- id; n_hit <- n_hit + 1L }
  }
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  suppressWarnings(write_csv(distinct(cache, key, .keep_all = TRUE), cache_path))
  if (verbose) {
    one <- length(need) == 1L
    message(sprintf("  Searching iNaturalist for the %d bee%s that still %s no number:",
                    length(need), if (one) "" else "s", if (one) "has" else "have"))
    if (n_new)   message(sprintf("    %d looked up for the first time", n_new))
    if (n_again) message(sprintf("    %d looked up again -- they had no page last time, and", n_again),
                         "\n      iNaturalist may have published them since")
    message(sprintf("    %d now have a number. The rest are looked for again every run.", n_hit))
    message("    ", cache_path)
  }
  df
}
