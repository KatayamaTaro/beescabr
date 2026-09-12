# =============================================================
# reference/taxonomy/plant_taxonomy_lookup_build.R
# beescabr pipeline -- PLANT taxonomy lookup builder
# Created 2026-07-21  |  crosswalk-driven + broad truth + genus normalization
#
# Builds data/reference/cabr_plant_taxonomy_lookup_generated.csv: a NORMALIZED plant tree
# where EVERY genus and EVERY species (from iNat obs + specimen labels) gets its
# own row, its own iNat taxon_id, and an in_cabr_park_at_all (T/F) flag. Basic
# ranks only (kingdom..species). Plant analogue of sd_bee_taxonomy_lookup_generated.csv
# (in_holway -> in_cabr_park_at_all).
#
# Sources:
#   * IN-PARK TRUTH  cabr_inat_plant_all_taxa_generated.csv -- every plant taxon observed
#     anywhere in the CABR boundary by ANY observer (not just surveyors), with
#     iNat taxon_id + ranks.  -> in_observations = TRUE, in_park = TRUE.
#   * SPECIMEN PLANTS  master_crosswalk_manual.csv (what_for == "plant_taxon") -- the
#     curated CANONICAL names of plants on specimen labels.  -> in_specimens.
#     A specimen species is in_park only if it's actually observed; otherwise
#     its clean name is resolved once via iNat (reliable -- no "Madia sp." moth).
#
# GENUS NORMALIZATION (roll up to genus only): every genus referenced by any row
# gets its own genus row with its iNat taxon_id (resolved when not already
# observed at genus level). A genus is in_park if it's observed in the park.
# So a photo ID'd only to genus, or a specimen species we can't confirm, still
# lands as an in-park genus row -- honest when species can't be told from a photo.
#
# rows with in_specimens = TRUE AND in_park = FALSE = specimen plants not observed
# in the park -> worklist (with genus_in_park so genus-absent suspects stand out).
# =============================================================

suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(stringr)
}))
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

local({
  sdir <- "scripts"
  for (cand in c("scripts", "../scripts", "../../scripts", "../../../scripts"))
    if (dir.exists(cand)) { sdir <- cand; break }
  need <- function(sym, file) if (!exists(sym)) source(file.path(sdir, file))
  need("PATHS",       "config.R")
  need("write_fresh", "utils/utils.R")
})

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---- constants / paths ------------------------------------------------------
PLANT_BASIC_RANKS <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")

.plt_path <- function(key, default) if (!is.null(PATHS[[key]])) PATHS[[key]] else default
PLT_ALL_TAXA     <- .plt_path("plant_all_taxa",         PATHS$plant_all_taxa)
PLT_CROSSWALK    <- "data/project_info/crosswalk/master_crosswalk_manual.csv"
PLT_CACHE        <- .plt_path("plant_name_cache",       PATHS$plant_name_cache)
PLT_LOOKUP_OUT   <- .plt_path("plant_taxonomy_lookup",  PATHS$plant_taxonomy_lookup)
PLT_WORKLIST_OUT <- .plt_path("plant_not_in_park",      PATHS$plant_not_in_park)
PLT_CONFIRMED    <- .plt_path("plant_park_confirmed",   PATHS$plant_park_confirmed)
PLT_FORAGE       <- .plt_path("inat_bee_forage",        PATHS$inat_bee_forage)

PLT_LOOKUP_COLS <- c("taxon_id", "scientific_name", "common_name", "rank",
                     "in_cabr_park_at_all", "in_specimens", "in_observations", "in_bee_forage",
                     PLANT_BASIC_RANKS)

# =============================================================
# PURE HELPERS (unit-tested, no I/O)
# =============================================================
plt_norm <- function(x) { x <- trimws(as.character(x)); tolower(gsub("\\s+", " ", x)) }

plt_label_rank <- function(name) {
  n <- plt_norm(name)
  if (is.na(n) || n == "") return(NA_character_)
  w <- strsplit(n, " ", fixed = TRUE)[[1]]
  if (length(w) == 1 && grepl("aceae$", w[1])) return("family")
  if (length(w) >= 2 && w[2] %in% c("sp", "sp.", "spp", "spp.")) return("genus")
  if (length(w) == 1) return("genus")
  "species"
}
plt_genus_token <- function(name) strsplit(plt_norm(name), " ", fixed = TRUE)[[1]][1]

plt_basic_ranks <- function(taxon) {
  out <- setNames(rep(NA_character_, length(PLANT_BASIC_RANKS)), PLANT_BASIC_RANKS)
  entries <- c(list(taxon), taxon$ancestors %||% list())
  for (a in entries) {
    if (!is.list(a)) next
    rk <- a$rank %||% NA_character_; nm <- a$name %||% NA_character_
    if (!is.na(rk) && rk %in% PLANT_BASIC_RANKS && is.na(out[[rk]])) out[[rk]] <- nm
  }
  out
}

# Best PLANT candidate from a /taxa?q= list; NULL if no plant (never a non-plant
# fallback -- that's what turned "Madia" into a moth).
plt_pick_plant <- function(results, name) {
  if (is.null(results) || length(results) == 0) return(NULL)
  target <- plt_norm(name)
  is_plant <- function(r) isTRUE(tolower(r$iconic_taxon_name %||% "") == "plantae")
  plants <- Filter(function(r) is.list(r) && is_plant(r), results)
  if (!length(plants)) return(NULL)
  exact_active <- Filter(function(r) isTRUE(r$is_active %||% TRUE) && plt_norm(r$name %||% "") == target, plants)
  if (length(exact_active)) return(exact_active[[1]])
  active <- Filter(function(r) isTRUE(r$is_active %||% TRUE), plants)
  if (length(active)) return(active[[1]])
  plants[[1]]
}

plt_park_sets <- function(obs_df) {
  g <- function(col) if (col %in% names(obs_df)) as.character(obs_df[[col]]) else character(0)
  list(taxon_ids = unique(g("taxon_id")[!is.na(g("taxon_id")) & g("taxon_id") != ""]),
       sci = unique(plt_norm(g("scientific_name"))), genus = unique(plt_norm(g("genus"))),
       family = unique(plt_norm(g("family"))))
}

# species -> its taxon_id OR binomial observed (a congener is NOT enough);
# genus -> genus observed; family -> family observed.
plt_in_park <- function(resolved, park) {
  tid <- as.character(resolved$taxon_id %||% NA_character_)
  if (!is.na(tid) && tid != "" && tid %in% park$taxon_ids) return(TRUE)
  rk  <- resolved$rank %||% NA_character_
  sci <- plt_norm(resolved$scientific_name %||% NA_character_)
  gen <- plt_norm(resolved$genus %||% NA_character_)
  fam <- plt_norm(resolved$family %||% NA_character_)
  if (!is.na(rk) && rk == "family")                 return(isTRUE(fam %in% park$family) || isTRUE(sci %in% park$family))
  if (!is.na(rk) && rk %in% c("genus", "subgenus")) return(isTRUE(gen %in% park$genus)  || isTRUE(sci %in% park$genus))
  isTRUE(sci != "" && sci %in% park$sci)
}

# Reduce a STACKED frame (obs + specimen + genus rows) to one row per taxon
# (key = taxon_id else normalized name): OR the flags, first non-NA taxonomy.
plt_reduce_lookup <- function(allrows) {
  for (col in PLT_LOOKUP_COLS) if (!col %in% names(allrows)) allrows[[col]] <- NA
  allrows$.key <- ifelse(!is.na(allrows$taxon_id) & allrows$taxon_id != "",
                         paste0("id:", allrows$taxon_id), paste0("nm:", plt_norm(allrows$scientific_name)))
  fna  <- function(x) { x <- x[!is.na(x) & x != ""]; if (length(x)) x[1] else NA_character_ }
  anyT <- function(x) any(x %in% TRUE)
  allrows %>% group_by(.key) %>% summarise(
    taxon_id = fna(taxon_id), scientific_name = fna(scientific_name),
    common_name = fna(common_name), rank = fna(rank),
    in_cabr_park_at_all = anyT(in_cabr_park_at_all),
    in_specimens = anyT(in_specimens), in_observations = anyT(in_observations),
    in_bee_forage = anyT(in_bee_forage),
    kingdom = fna(kingdom), phylum = fna(phylum), class = fna(class), order = fna(order),
    family = fna(family), genus = fna(genus), species = fna(species), .groups = "drop") %>%
    select(all_of(PLT_LOOKUP_COLS)) %>%
    arrange(desc(in_cabr_park_at_all), kingdom, family, genus, species, scientific_name)
}

# canonical plant names from the crosswalk (what_for == "plant_taxon")
plt_crosswalk_canonicals <- function(crosswalk_path = PLT_CROSSWALK) {
  if (!file.exists(crosswalk_path)) return(character(0))
  cw <- suppressWarnings(suppressMessages(read_csv(crosswalk_path, show_col_types = FALSE, col_types = cols(.default = "c"))))
  if (!all(c("name", "what_for") %in% names(cw))) return(character(0))
  keep <- !is.na(cw$what_for) & tolower(cw$what_for) == "plant_taxon" & !is.na(cw$name) & cw$name != ""
  unique(cw$name[keep])
}

# curated confirmed-in-park species (obscured threatened taxa iNat won't place):
# scientific_name + optional taxon_id (a manual id hint) + a free-text note.
plt_load_confirmed <- function(path = PLT_CONFIRMED) {
  blank <- tibble(scientific_name = character(), taxon_id = character())
  if (!file.exists(path)) return(blank)
  cf <- suppressWarnings(suppressMessages(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))))
  if (!"scientific_name" %in% names(cf)) return(blank)
  if (!"taxon_id" %in% names(cf)) cf$taxon_id <- NA_character_
  cf %>% filter(!is.na(scientific_name), trimws(scientific_name) != "") %>%
    transmute(scientific_name = trimws(scientific_name), taxon_id = as.character(taxon_id))
}

# roll a plant name up to AT MOST species: a trinomial+ (subspecies/variety) folds
# to its binomial; a binomial or genus is left as-is; "Genus sp." is left alone.
# Keeps the lookup at the genus+species granularity the design asks for, so e.g.
# "Isocoma menziesii sedoides" (136 bee obs) confirms the species "Isocoma menziesii".
plt_roll_to_species <- function(name) {
  x <- trimws(gsub("\\s+", " ", as.character(name)))
  if (is.na(x) || x == "") return(NA_character_)
  w <- strsplit(x, " ", fixed = TRUE)[[1]]
  if (length(w) >= 3 && !(tolower(w[2]) %in% c("sp", "sp.", "spp", "spp.", "x", "×"))) return(paste(w[1], w[2]))
  x
}

# distinct bee-forage plant names (from cabr_inat_bee_forage_generated.csv): the plants bees
# were recorded foraging on inside the park -> a second in-park truth source.
plt_load_forage <- function(forage_path = PLT_FORAGE) {
  if (!file.exists(forage_path)) return(character(0))
  fg <- suppressWarnings(suppressMessages(read_csv(forage_path, show_col_types = FALSE, col_types = cols(.default = "c"))))
  if (!"scientific_name" %in% names(fg)) return(character(0))
  unique(fg$scientific_name[!is.na(fg$scientific_name) & trimws(fg$scientific_name) != ""])
}

# =============================================================
# RESOLUTION SHELL (network -- injected, cached)
# =============================================================
plt_resolve_one <- function(name, request_fn = NULL, fetch_by_id_fn = NULL) {
  if (is.null(request_fn) || is.null(fetch_by_id_fn)) {
    if (!exists("inat_fetch_taxa_by_name")) source("scripts/inat_observations/engine/api/inat_http.R")
    request_fn     <- request_fn     %||% inat_fetch_taxa_by_name
    fetch_by_id_fn <- fetch_by_id_fn %||% inat_fetch_taxon_by_id
  }
  blank <- tibble(input_name = name, taxon_id = NA_character_, scientific_name = NA_character_,
                  common_name = NA_character_, rank = NA_character_, kingdom = NA_character_,
                  phylum = NA_character_, class = NA_character_, order = NA_character_,
                  family = NA_character_, genus = NA_character_, species = NA_character_, resolved = FALSE)
  results <- tryCatch(request_fn(name), error = function(e) NULL)
  best <- plt_pick_plant(results, name)
  if (is.null(best)) return(blank)
  full <- tryCatch(fetch_by_id_fn(best$id), error = function(e) NULL)
  taxon <- if (!is.null(full) && length(full)) full[[1]] else best
  rk <- plt_basic_ranks(taxon)
  tibble(input_name = name, taxon_id = as.character(taxon$id %||% NA_character_),
         scientific_name = taxon$name %||% NA_character_, common_name = taxon$preferred_common_name %||% NA_character_,
         rank = taxon$rank %||% NA_character_,
         kingdom = rk[["kingdom"]], phylum = rk[["phylum"]], class = rk[["class"]],
         order = rk[["order"]], family = rk[["family"]], genus = rk[["genus"]],
         species = rk[["species"]], resolved = TRUE)
}

# resolved_on: the date this name was last asked about. Without it nothing can tell
# how old the plant cache is, so refresh_due() could never age it and the yearly
# prompt could never mention it -- which is exactly how it drifted unnoticed.
.plt_cache_cols <- c("input_name", "taxon_id", "scientific_name", "common_name", "rank", PLANT_BASIC_RANKS, "resolved", "resolved_on")

#' Read the plant name -> iNaturalist taxon cache
#'
#' @param path The cache CSV.
#' @return Its rows, all columns character; an empty tibble when it does not exist yet.
plt_load_cache <- function(path = PLT_CACHE) {
  if (!file.exists(path)) return(tibble())
  suppressWarnings(suppressMessages(read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))))
}


# ------------------------------------------------------------
# plant_cache_changes(): PURE. What does iNaturalist now say differently about the
# plant names we already hold?
#
# Plants are resolved BY NAME, not by id, so the change that matters here is not a
# rename -- it is the SAME name coming back with a DIFFERENT taxon_id. That happens
# when iNaturalist retires a taxon and the name now matches its replacement, and it
# silently re-points every bee-plant join that used the old id. Reported ahead of a
# rename when both happened.
# ------------------------------------------------------------
#' Compare a plant name cache against a freshly resolved one
#'
#' @param before The cache as it was.
#' @param after The cache after a forced re-resolution. A name absent from it was not
#'   re-resolved this run, so nothing is known about it and it is not reported.
#' @return A data.frame of changed rows only: input_name, change, id_was, id_now,
#'   name_was, name_now.
plant_cache_changes <- function(before, after) {
  key <- function(d) plt_norm(d$input_name)
  bk <- key(before); ak <- key(after)
  out <- lapply(seq_len(nrow(before)), function(i) {
    j <- match(bk[i], ak)
    if (is.na(j)) return(NULL)                       # not re-resolved: nothing known
    chr <- function(d, col, r) { v <- if (col %in% names(d)) d[[col]][r] else NA; as.character(v) }
    id_was <- chr(before, "taxon_id", i); id_now <- chr(after, "taxon_id", j)
    nm_was <- chr(before, "scientific_name", i); nm_now <- chr(after, "scientific_name", j)
    res_now <- tolower(chr(after, "resolved", j))
    change <- if (identical(res_now, "false") || identical(res_now, "f")) "no longer resolves"
              else if (!identical(id_was, id_now)) "different taxon"
              else if (!identical(nm_was, nm_now)) "renamed"
              else NA_character_
    if (is.na(change)) return(NULL)
    data.frame(input_name = before$input_name[i], change = change,
               id_was = id_was, id_now = id_now, name_was = nm_was, name_now = nm_now,
               stringsAsFactors = FALSE)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(data.frame(input_name = character(0), change = character(0),
                                      id_was = character(0), id_now = character(0),
                                      name_was = character(0), name_now = character(0),
                                      stringsAsFactors = FALSE))
  do.call(rbind, out)
}

# ------------------------------------------------------------
# Which changes need a person, and what they answered.
#
# A rename is cosmetic: same taxon_id, new spelling, every join still holds, so it is
# taken without asking. An id change is not -- the plant name now matches a DIFFERENT
# iNaturalist taxon, and every bee-plant record using it silently follows. Only a
# person can say whether that is the same plant. Same for a name that stopped
# resolving. The tool used to write the new cache and print the change afterwards,
# which meant the id had already moved by the time anyone read about it.
# ------------------------------------------------------------
#' The changed rows a person has to rule on
#'
#' @param changed What `plant_cache_changes()` returned.
#' @return The subset that moves or loses a taxon_id.
plant_changes_to_ask <- function(changed) {
  if (!nrow(changed)) return(changed)
  changed[changed$change %in% c("different taxon", "no longer resolves"), , drop = FALSE]
}

#' Read one answer to "is this the same plant?"
#'
#' @param raw What the operator typed.
#' @return "take", "keep", "quit", or "unclear". Blank is keep: doing nothing must
#'   never be the answer that moves an id.
plant_change_choice <- function(raw) {
  a <- answer_norm(raw)
  if (!nzchar(a))                          return("keep")   # Enter never moves an id
  if (a %in% c("take", "t"))               return("take")
  if (a %in% c("keep", "k"))               return("keep")
  if (answer_is(raw, "yes"))               return("take")
  if (answer_is(raw, c("no", "skip")))     return("keep")
  if (answer_is(raw, "quit"))              return("quit")
  "unclear"
}

#' Put the old row back for the names the operator kept
#'
#' @param after The re-resolved cache.
#' @param before The cache as it was.
#' @param kept_names Input names to restore.
#' @return `after` with those rows replaced by their `before` versions.
plant_apply_keep <- function(after, before, kept_names) {
  if (!length(kept_names)) return(after)
  k <- plt_norm(kept_names)
  after <- after[!(plt_norm(after$input_name) %in% k), , drop = FALSE]
  dplyr::bind_rows(after, before[plt_norm(before$input_name) %in% k, , drop = FALSE])
}

#' Resolve plant names to iNaturalist taxa, cache-first
#'
#' @param names_vec The names to resolve.
#' @param cache An already-loaded cache, or NULL to read it from `cache_path`.
#' @param resolve_fn Injection point for the per-name resolution.
#' @param cache_path Where the cache lives.
#' @param verbose Print each name as it is resolved.
#' @param force Ask iNaturalist again about names already in the cache. The cache has
#'   no age of its own, so this is the only way a revised plant is ever noticed.
#' @return A list of `rows` (one per requested name) and the updated `cache`.
plt_resolve_names <- function(names_vec, cache = NULL, resolve_fn = plt_resolve_one,
                              cache_path = PLT_CACHE, verbose = TRUE, force = FALSE) {
  cache <- cache %||% plt_load_cache(cache_path)
  # Keep everything character: a disk-loaded cache is all-character, a fresh
  # resolution has a logical `resolved`; without this bind_rows() refuses to
  # combine them once new names are appended to an existing cache.
  if (nrow(cache)) cache <- mutate(cache, across(everything(), as.character))
  have  <- if (nrow(cache)) plt_norm(cache$input_name) else character(0)
  out <- list()
  for (nm in names_vec) {
    # force: ask iNaturalist again about a name we already hold. The cache has no age,
    # so without this a name resolved once keeps its first taxon_id forever -- and
    # plants are resolved BY NAME, so a retired taxon quietly re-points the name at a
    # different id that nothing would ever look for.
    hit <- if (!isTRUE(force) && length(have)) which(have == plt_norm(nm)) else integer(0)
    if (length(hit)) out[[nm]] <- cache[hit[1], intersect(.plt_cache_cols, names(cache)), drop = FALSE]
    else {
      if (verbose) message("  resolving plant name: ", nm)
      r <- resolve_fn(nm); r$input_name <- nm; r$resolved_on <- as.character(Sys.Date())
      r <- mutate(r, across(everything(), as.character))
      out[[nm]] <- r[, intersect(.plt_cache_cols, names(r)), drop = FALSE]
      # a forced re-resolution REPLACES its row; appending beside the old one would
      # leave two rows for one name and the stale one first. An empty cache has no
      # input_name column yet, so only filter when there is something to filter.
      if (nrow(cache) && "input_name" %in% names(cache))
        cache <- cache[plt_norm(cache$input_name) != plt_norm(nm), , drop = FALSE]
      cache <- bind_rows(cache, out[[nm]])
      have  <- if ("input_name" %in% names(cache)) plt_norm(cache$input_name) else character(0)
    }
  }
  list(rows = if (length(out)) bind_rows(out) else tibble(), cache = cache)
}

# build the specimen leaf rows (obs taxonomy if observed, else resolved)
.plt_spec_rows <- function(canon, obs, obs_n, reslookup, park) {
  ridx <- function(rn) { i <- if (nrow(reslookup)) which(plt_norm(reslookup$input_name) == plt_norm(rn)) else integer(0); if (length(i)) i[1] else NA_integer_ }
  rows <- lapply(canon, function(nm) {
    j <- which(obs_n == plt_norm(nm))
    if (length(j)) { r <- obs[j[1], , drop = FALSE]
      tibble(taxon_id = as.character(r$taxon_id), scientific_name = r$scientific_name, common_name = r$common_name,
             rank = r$rank, kingdom = r$kingdom, phylum = r$phylum, class = r$class, order = r$order,
             family = r$family, genus = r$genus, species = r$species, in_cabr_park_at_all = TRUE)
    } else { i <- ridx(nm); r <- if (!is.na(i)) reslookup[i, , drop = FALSE] else NULL
      tibble(taxon_id = if (!is.null(r)) as.character(r$taxon_id) else NA_character_,
             scientific_name = if (!is.null(r) && !is.na(r$scientific_name)) r$scientific_name else nm,
             common_name = if (!is.null(r)) r$common_name else NA_character_,
             rank = if (!is.null(r) && !is.na(r$rank)) r$rank else plt_label_rank(nm),
             kingdom = if (!is.null(r)) r$kingdom else NA_character_, phylum = if (!is.null(r)) r$phylum else NA_character_,
             class = if (!is.null(r)) r$class else NA_character_, order = if (!is.null(r)) r$order else NA_character_,
             family = if (!is.null(r)) r$family else NA_character_,
             genus = if (!is.null(r) && !is.na(r$genus)) r$genus else plt_genus_token(nm),
             species = if (!is.null(r)) r$species else NA_character_,
             in_cabr_park_at_all = if (!is.null(r)) plt_in_park(r, park) else FALSE)
    }
  })
  if (length(rows)) bind_rows(rows) else
    tibble(taxon_id = character(), scientific_name = character(), common_name = character(), rank = character(),
           kingdom = character(), phylum = character(), class = character(), order = character(),
           family = character(), genus = character(), species = character(), in_cabr_park_at_all = logical())
}

# =============================================================
# ORCHESTRATOR
# =============================================================
#' Build the plant taxonomy lookup -- the authority on what a plant taxon is
#'
#' @param all_taxa_path Every plant name seen anywhere in the project.
#' @param crosswalk_path Hand-maintained name corrections.
#' @param cache_path Cached iNaturalist resolutions, so a rebuild is offline.
#' @param confirmed_path Names a human has confirmed.
#' @param forage_path Forage-plant list.
#' @param forage_fn Injection point for reading forage data; `NULL` uses the file.
#' @param resolve_fn Injection point for resolving one name; tests pass a fake.
#' @param write Write the lookup to disk. `FALSE` returns it only.
#' @param verbose Print progress.
#' @return The lookup, one row per plant taxon, keyed by `taxon_id`.
build_plant_taxonomy_lookup <- function(all_taxa_path  = PLT_ALL_TAXA,
                                        crosswalk_path = PLT_CROSSWALK,
                                        cache_path     = PLT_CACHE,
                                        confirmed_path = PLT_CONFIRMED,
                                        forage_path    = PLT_FORAGE,
                                        forage_fn      = NULL,
                                        resolve_fn     = plt_resolve_one,
                                        write = TRUE, verbose = TRUE) {
  stopifnot(file.exists(all_taxa_path))
  rc <- function(p) suppressWarnings(suppressMessages(read_csv(p, show_col_types = FALSE, col_types = cols(.default = "c"))))
  at <- rc(all_taxa_path)
  park <- plt_park_sets(at)

  # ---- observed leaf taxa (every taxon anyone recorded in the park) ----
  obs <- at %>%
    filter(!is.na(scientific_name), scientific_name != "") %>%
    transmute(taxon_id = as.character(taxon_id), scientific_name,
              common_name = if ("common_name" %in% names(at)) common_name else NA_character_,
              rank = if ("taxon_rank" %in% names(at)) taxon_rank else NA_character_,
              kingdom, phylum, class, order, family, genus, species,
              in_observations = TRUE, in_specimens = FALSE, in_cabr_park_at_all = TRUE)
  obs_n <- plt_norm(obs$scientific_name)

  # ---- confirmed-in-park override (obscured threatened taxa) ----
  # iNat auto-obscures the coordinates of vulnerable species (cliff spurge, coast
  # barrel cactus, sea dahlia, ...) so a park as small as CABR never earns their
  # place-membership -- they can't be confirmed from public obs. A curated list
  # (the botanist's word) names them; we resolve each to its taxon_id, force
  # in_park = TRUE, and fold it into the park truth so its genus and any matching
  # specimen row read in-park too. in_observations = FALSE + in_specimens = FALSE
  # (unless also on a specimen) is the fingerprint of one of these overrides.
  confirmed <- plt_load_confirmed(confirmed_path)
  cache <- NULL
  confirmed_leaves <- NULL
  if (nrow(confirmed)) {
    cres  <- plt_resolve_names(confirmed$scientific_name, resolve_fn = resolve_fn,
                               cache_path = cache_path, verbose = verbose)
    cache <- cres$cache
    if (nrow(cres$rows)) {
      idmap <- confirmed %>% transmute(.n = plt_norm(scientific_name), taxon_id_curated = taxon_id)
      confirmed_leaves <- cres$rows %>%
        transmute(taxon_id = as.character(taxon_id),
                  scientific_name = coalesce(scientific_name, input_name),
                  common_name = common_name,
                  rank = coalesce(rank, vapply(input_name, plt_label_rank, character(1))),
                  kingdom, phylum, class, order, family,
                  genus = coalesce(genus, vapply(input_name, plt_genus_token, character(1))),
                  species = species, .n = plt_norm(input_name)) %>%
        left_join(idmap, by = ".n") %>%
        mutate(taxon_id = coalesce(taxon_id, taxon_id_curated),
               in_observations = FALSE, in_specimens = FALSE, in_cabr_park_at_all = TRUE) %>%
        select(-.n, -taxon_id_curated)
      park$taxon_ids <- unique(c(park$taxon_ids,
                                 confirmed_leaves$taxon_id[!is.na(confirmed_leaves$taxon_id) & confirmed_leaves$taxon_id != ""]))
      park$sci   <- unique(c(park$sci,   plt_norm(confirmed_leaves$scientific_name)))
      park$genus <- unique(c(park$genus, plt_norm(confirmed_leaves$genus)))
    }
  }

  # ---- bee-forage in-park source (plants bees were recorded on, in-park) ----
  # A bee photographed ON a flower inside the park is direct proof the plant is
  # here -- and it names plants the standalone plant pull misses, incl. the
  # obscured threatened taxa. Forage names roll to species; the resolver keeps
  # only Plantae (a non-plant flower tag -> NA taxon_id -> dropped here, and the
  # bee cleaner flags that obs for a fix). Observed plants that are ALSO foraged
  # just get in_bee_forage = TRUE for provenance.
  forage_names <- if (!is.null(forage_fn)) forage_fn() else {
    if (!file.exists(forage_path)) {
      if (!exists("write_bee_forage")) try(source("scripts/inat_observations/clean/bee_forage.R"), silent = TRUE)
      if (exists("write_bee_forage")) try(write_bee_forage(out_path = forage_path, verbose = verbose), silent = TRUE)
    }
    plt_load_forage(forage_path)
  }
  obs$in_bee_forage <- FALSE
  forage_leaves <- NULL
  if (length(forage_names)) {
    froll   <- vapply(forage_names, plt_roll_to_species, character(1))
    froll_n <- plt_norm(froll)
    obs_roll_n <- plt_norm(vapply(obs$scientific_name, plt_roll_to_species, character(1)))
    obs$in_bee_forage <- obs_roll_n %in% froll_n                 # observed plants that are also foraged
    fkeep <- unique(froll[!(froll_n %in% obs_roll_n)])           # foraged plants missing from the plant-obs pull
    if (length(fkeep)) {
      fres  <- plt_resolve_names(fkeep, cache = cache, resolve_fn = resolve_fn, cache_path = cache_path, verbose = verbose)
      cache <- fres$cache
      forage_leaves <- fres$rows %>%
        filter(!is.na(taxon_id), taxon_id != "") %>%            # resolver returns no taxon_id for non-plants -> dropped
        transmute(taxon_id = as.character(taxon_id),
                  scientific_name = coalesce(scientific_name, input_name),
                  common_name = common_name,
                  rank = coalesce(rank, vapply(input_name, plt_label_rank, character(1))),
                  kingdom, phylum, class, order, family,
                  genus = coalesce(genus, vapply(input_name, plt_genus_token, character(1))),
                  species = species,
                  in_observations = FALSE, in_specimens = FALSE, in_bee_forage = TRUE,
                  in_cabr_park_at_all = TRUE)
      if (nrow(forage_leaves)) {
        park$taxon_ids <- unique(c(park$taxon_ids, forage_leaves$taxon_id))
        park$sci   <- unique(c(park$sci,   plt_norm(forage_leaves$scientific_name)))
        park$genus <- unique(c(park$genus, plt_norm(forage_leaves$genus)))
      }
    }
  }

  # ---- specimen leaf taxa (crosswalk canonicals) ----
  canon <- plt_crosswalk_canonicals(crosswalk_path)
  need_resolve <- unique(canon[!(plt_norm(canon) %in% obs_n)])
  res <- plt_resolve_names(need_resolve, cache = cache, resolve_fn = resolve_fn, cache_path = cache_path, verbose = verbose)
  cache <- res$cache
  spec <- .plt_spec_rows(canon, obs, obs_n, res$rows, park)
  if (nrow(spec)) spec <- spec %>% mutate(in_observations = FALSE, in_specimens = TRUE)
  spec_genera <- unique(plt_norm(spec$genus))

  # ---- genus normalization: every genus its own row + taxon_id ----
  gd <- bind_rows(obs %>% transmute(genus), spec %>% transmute(genus),
                  if (!is.null(confirmed_leaves)) confirmed_leaves %>% transmute(genus),
                  if (!is.null(forage_leaves))    forage_leaves %>% transmute(genus)) %>%
    filter(!is.na(genus), genus != "") %>% mutate(.n = plt_norm(genus)) %>% distinct(.n, .keep_all = TRUE)
  have_genus <- plt_norm(c(obs$scientific_name[!is.na(obs$rank) & tolower(obs$rank) == "genus"],
                           spec$scientific_name[!is.na(spec$rank) & tolower(spec$rank) == "genus"]))
  need_g <- gd$genus[!(gd$.n %in% have_genus)]
  gres <- plt_resolve_names(unique(need_g), cache = cache, resolve_fn = resolve_fn, cache_path = cache_path, verbose = verbose)
  cache <- gres$cache
  genus_rows <- if (nrow(gres$rows)) gres$rows %>% transmute(
      taxon_id = as.character(taxon_id), scientific_name = coalesce(scientific_name, input_name),
      common_name = common_name, rank = "genus", kingdom, phylum, class, order, family,
      genus = coalesce(genus, input_name), species = NA_character_,
      in_observations = TRUE, in_specimens = FALSE,
      in_cabr_park_at_all = plt_norm(coalesce(genus, input_name)) %in% park$genus) else NULL

  if (write) { dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE); write_fresh(cache, cache_path, na = "") }

  lookup <- plt_reduce_lookup(bind_rows(obs, spec, genus_rows, confirmed_leaves, forage_leaves))

  worklist <- lookup %>%
    filter(in_specimens %in% TRUE, in_cabr_park_at_all %in% FALSE) %>%
    mutate(genus_in_park = plt_norm(genus) %in% park$genus) %>%
    transmute(scientific_name, rank, genus, family, in_cabr_park_at_all, genus_in_park) %>%
    arrange(genus_in_park, scientific_name)

  if (write) {
    dir.create(dirname(PLT_LOOKUP_OUT), recursive = TRUE, showWarnings = FALSE)
    write_fresh(lookup, PLT_LOOKUP_OUT, na = "")
    write_fresh(worklist, PLT_WORKLIST_OUT, na = "")
  }
  if (verbose) {
    ng <- sum(tolower(lookup$rank) == "genus", na.rm = TRUE); ns <- sum(tolower(lookup$rank) == "species", na.rm = TRUE)
    bx_kv("Plant lookup", format(nrow(lookup), big.mark = ","), " taxa (", ng, " genus, ", ns, " species)")
    bx_cont("in park: ", sum(lookup$in_cabr_park_at_all, na.rm = TRUE),
            " · on specimens: ", sum(lookup$in_specimens, na.rm = TRUE),
            " · not in park: ", nrow(worklist), " (genus-absent: ", sum(!worklist$genus_in_park), ")")
    bx_out(basename(PLT_LOOKUP_OUT))
    bx_out(basename(PLT_WORKLIST_OUT))
    if (!is.null(confirmed_leaves) && nrow(confirmed_leaves))
      bx_cont("confirmed-in-park overrides applied: ", nrow(confirmed_leaves))
    nf <- sum(lookup$in_bee_forage %in% TRUE)
    if (nf) bx_cont("in-park via bee forage: ", nf, " plants (",
                    if (is.null(forage_leaves)) 0L else nrow(forage_leaves), " added by forage alone)")
  }
  invisible(list(lookup = lookup, worklist = worklist, cache = cache))
}

# ---- standalone entrypoint --------------------------------------------------
if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) {
  build_plant_taxonomy_lookup()
}
