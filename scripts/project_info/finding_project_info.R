# =============================================================
# project_info/finding_project_info.R
# beescabr pipeline -- THE provenance layer ("the brain")
# Created 2026-07-14. See docs/ARCHITECTURE_project_info.md.
#
# Decides, ONCE, for every observation (bee now; plant when ingested): is this
# part of our project, whose is it, when, which transect. Taxonomy-blind on
# purpose. Everything downstream (inat_bee_clean.R / inat_plant_clean.R) just
# LOOKS UP the answer by obs_id -- no more ping-ponging, no tag logic in the
# clean scripts.
#
# The simple, validated model (Brandi's design):
#   * Is it a survey? -> ANY Cabrillo tag/field (all survey variants collapsed,
#     incl. the general 10-minute + metadata tags). Exclude tags win.
#   * What year?      -> the observation's own date.
#   * Beeple/intern?  -> the ROSTER (observer + year); missing -> "unknown".
#   * Transect?       -> the transect tag OR the transect obs-field.
#   * Flags is_10min / is_metadata ride along.
#   * in_cabr         -> the real CABR boundary (a LABEL; nothing is dropped).
#
# INPUTS
#   data/inat_observations/cache/export_flat.rds                 (bee obs; plant export added later)
#   data/project_info/crosswalk/master_crosswalk_manual.csv  (tag/field crosswalk)
#   data/project_info/rosters/people_manual.csv      (who exists; per-year role is derived)
#   data/project_info/surveys/survey_date_sources/beeple_calendar_windows/beeple_calendar_windows_generated.csv (from finding_beeple_calendar.R, stage 2d)
#   data/spatial/shapefiles/boundaries/cabr/cabr_survey_box.shp
#
# The intern schedule is a CURATED INPUT file -- data/project_info/surveys/survey_date_sources/master_intern_survey_log_manual.csv
# (FPI_INTERN_LOG) -- holding the `source == "intern-log"` rows for interns (BOTH lethal net
# days AND non-lethal iNat days). Each run READS those rows from the log and rebuilds the
# beeple rows around them; the master is pure generated OUTPUT. This replaced the old design
# where intern rows lived only in the generated master and were silently wiped on regeneration
# (a tagged intern iNat day like 2024-05-05 -- not in the beeple tag-rebuild, not a specimen
# date -- fell through every crack). Edit intern dates in master_intern_survey_log_manual.csv, NOT the master.
#
# OUTPUTS
#   data/inat_observations/inat_raw/cabr_inat_raw_generated.csv  <- NEW: the per-obs lookup
#   data/project_info/surveys/master_per_survey_info_generated.csv                      <- built upon in place
#   data/project_info/crosswalk/review/qc_review_mastercrosswalk_inat_unknown_tags_generated.csv        <- unrecognized hashtags
#   data/project_info/crosswalk/review/qc_review_mastercrosswalk_inat_unknown_fields_generated.csv      <- unrecognized obs-field names
#
# Run: source("scripts/project_info/finding_project_info.R"); finding_project_info()
# =============================================================

if (!exists("PATHS")) source("scripts/config.R")   # centralized paths (see PATHS in config.R)
if (!exists("build_participation")) source("scripts/project_info/rosters/build_participation.R")
if (!exists("build_identification_counts")) source("scripts/project_info/rosters/build_identification_counts.R")
if (!exists("roster_view")) source("scripts/project_info/rosters/roster_view.R")
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(sf)

if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

# transect resolver (majority rule) -- defines resolve_transects(); guarded so the
# brain still runs if the file isn't present.
if (file.exists("scripts/project_info/surveys/resolve_beeple_transects_per_survey.R")) source("scripts/project_info/surveys/resolve_beeple_transects_per_survey.R")
if (file.exists("scripts/project_info/surveys/finding_survey_dates.R")) source("scripts/project_info/surveys/finding_survey_dates.R")
if (file.exists("scripts/project_info/surveys/finding_specimen_dates.R")) source("scripts/project_info/surveys/finding_specimen_dates.R")
if (file.exists("scripts/project_info/surveys/rescue_on_transect_surveys.R")) source("scripts/project_info/surveys/rescue_on_transect_surveys.R")

# ---- paths ----
# Every export listed here is pooled through the SAME membership + survey-date
# logic, tagged by `kind`. The plant export is built by ingest_plants.R; it's
# safe to list before the first plant pull -- an absent path is skipped below.
FPI_EXPORTS   <- list(
  list(path = "data/inat_observations/cache/export_flat.rds",       kind = "bee"),
  list(path = "data/inat_observations/cache/export_flat_plant.rds", kind = "plant")
)
FPI_CROSSWALK <- "data/project_info/crosswalk/master_crosswalk_manual.csv"
FPI_YEARS     <- 2021:as.integer(format(Sys.Date(), "%Y"))   # seasons the derived roster spans
FPI_WINDOWS   <- "data/project_info/surveys/survey_date_sources/beeple_calendar_windows/beeple_calendar_windows_generated.csv"
FPI_BOUNDARY  <- "data/spatial/shapefiles/boundaries/cabr/cabr_survey_box.shp"
FPI_TRANSECTS <- "data/spatial/shapefiles/transects/cabr_bee_transects.shp"  # rescue: on-transect untagged obs

FPI_MEMBERSHIP     <- "data/inat_observations/inat_raw/cabr_inat_raw_generated.csv"  # the per-obs lookup
FPI_SURVEY_DATES   <- PATHS$per_survey
FPI_INTERN_LOG     <- "data/project_info/surveys/survey_date_sources/master_intern_survey_log_manual.csv"  # curated intern survey-day log (SOURCE OF TRUTH -- edit intern days HERE, not in the generated master)
#' The review-queue summary, with somewhere to go for anything outstanding
#'
#' It used to print three counts and nothing else -- "0 unknown tags . 0 fields .
#' 8 windows" -- which tells someone there is work without telling them where it
#' is or what it means. A count is only useful with the file beside it.
#'
#' @param tags,fields,windows Counts outstanding in each queue.
#' @return Invisibly NULL; prints.
.fpi_review_summary <- function(tags, fields, windows) {
  if (!tags && !fields && !windows) { bx_kv("Review queue", "nothing waiting on you"); return(invisible(NULL)) }
  bx_kv("Review queue", tags, " unknown tags \U00B7 ", fields, " fields \U00B7 ", windows, " survey-date windows")
  if (tags || fields)
    bx_cont("tags/fields: words on an observation the project has not seen before. ",
            "Sorted in the REVIEW step below -- no file to open.")
  if (windows) {
    bx_cont("windows: a survey day on the beeple calendar with no tagged survey near it.")
    bx_cont("Rule on each in  ", FPI_REVIEW)
    bx_cont("Nothing is dropped meanwhile; unruled days stay out of the survey counts")
    bx_cont("and come back next run.")
  }
  invisible(NULL)
}

FPI_REVIEW         <- "data/project_info/surveys/review/qc_review_survey_beeple_date_windows_generated.csv"   # beeple windows to rule on (persistent)
FPI_MISTAGS        <- "data/inat_observations/review/qc_review_inat_mistagged_transects_generated.csv"  # stray transect tags outvoted by the day's majority
FPI_TIES           <- "data/project_info/surveys/review/qc_review_survey_transect_overlap_generated.csv"  # equal-split days to rule (review_transect_ties)
FPI_UNKNOWN_TAGS   <- "data/project_info/crosswalk/review/qc_review_mastercrosswalk_inat_unknown_tags_generated.csv"    # unknown hashtags
FPI_UNKNOWN_FIELDS <- "data/project_info/crosswalk/review/qc_review_mastercrosswalk_inat_unknown_fields_generated.csv"  # unknown obs-field NAMES
SD_COLUMNS <- c("year", "role", "source", "date",
                "transects", "surveyors", "inat_username", "specimen_collector",
                "method", "technique",
                "confirmed", "confirmed_by", "n_obs", "n_speci", "n_days", "note")

# survey-date tuning:
#   * tol_days -- a planned survey may drift this many days from its calendar window and
#     still count as "covered" by a nearby tagged survey (people survey a week early/late).
SD_WINDOW_TOL_DAYS <- 10L

fpi_norm <- function(s) tolower(gsub("^#", "", trimws(s)))

# Split a ";"/"," -delimited variant list WITHOUT splitting on a delimiter that
# sits inside parentheses. FOR FIELD VARIANTS ONLY -- some iNat field NAMES embed
# their allowed values, e.g. "soil type (sandy; loam; clay)" is ONE name, not three,
# so a plain split would shred them. TAGS must NOT use this: their parenthetical
# common names ("Sweat Bee (Lasioglossum sp)") sit beside standalone tags and must
# stay split, or tags like bokeh/ceratina resurface as "unknown".
fpi_split_variants <- function(s) {
  s <- as.character(s)
  if (length(s) != 1L) return(unlist(lapply(s, fpi_split_variants), use.names = FALSE))
  if (is.na(s) || !nzchar(trimws(s))) return(character(0))
  out <- character(0); buf <- ""; depth <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") depth <- if (depth > 0L) depth - 1L else 0L
    if (depth == 0L && (ch == ";" || ch == ",")) { out <- c(out, buf); buf <- "" }
    else buf <- paste0(buf, ch)
  }
  out <- trimws(c(out, buf)); out[nzchar(out)]
}

# ------------------------------------------------------------
# crosswalk -> normalized variant -> canonical/category (survey/transect/exclude)
# ------------------------------------------------------------
fpi_build_tagmap <- function(crosswalk) {
  # New concept-per-row crosswalk: tag spellings live in `inat_tag_variants`
  # (and the `name` itself, for the exclude/location rows whose name IS the tag).
  # Variants may be separated by ; OR , so split on both. what_for is the group.
  crosswalk |>
    filter(!is.na(name), trimws(name) != "") |>
    filter(tolower(trimws(what_for)) != "plant_taxon") |>   # plant taxa are flower names, not survey tags
    transmute(concept = name, what_for = tolower(trimws(what_for)),
              variants = paste(name, coalesce(inat_tag_variants, ""), sep = "; ")) |>
    separate_rows(variants, sep = "[;,]\\s*") |>   # TAGS: plain split (NOT paren-aware)
    mutate(key = fpi_norm(variants)) |>
    filter(nchar(key) > 0, !grepl("^\\(", key), !(key %in% c("n/a", "na"))) |>
    distinct(key, concept, what_for)
}

# ------------------------------------------------------------
# Per-obs signals from BOTH the tag list AND the obs-fields (a survey tag typed
# into any field counts too -- that's the manual "tags" override). Returns one
# row per obs_id with membership signals, plus the long unmatched-token table
# for the unknown-tags report.
# ------------------------------------------------------------
fpi_signals <- function(df, tagmap) {
  # tokens from tag_list
  from_tags <- df |>
    select(obs_id, tag_list) |>
    filter(!is.na(tag_list), tag_list != "") |>
    mutate(tok = str_split(tag_list, ",\\s*")) |> unnest(tok) |>
    transmute(obs_id, tok = trimws(tok), src = "tag")
  # tokens from non-empty obs-fields
  fld <- names(df)[startsWith(names(df), "field:")]
  fld <- fld[vapply(df[fld], function(c) any(!is.na(c) & c != ""), logical(1))]
  from_fields <- df |>
    select(obs_id, all_of(fld)) |>
    pivot_longer(-obs_id, values_to = "tok") |>
    filter(!is.na(tok), tok != "") |>
    transmute(obs_id, tok = trimws(tok), src = "field")

  toks <- bind_rows(from_tags, from_fields) |>
    mutate(key = fpi_norm(tok)) |> filter(nchar(key) > 0) |>
    left_join(tagmap, by = "key", relationship = "many-to-many")

  signals <- toks |>
    group_by(obs_id) |>
    summarise(
      has_survey  = any(what_for %in% c("survey", "meta_data", "beeple", "intern"), na.rm = TRUE),
      is_exclude  = any(what_for == "exclude", na.rm = TRUE),
      exclude_tag = paste(sort(unique(na.omit(concept[what_for == "exclude"]))), collapse = "; "),
      is_10min    = any(concept == "cabr_bee_10_min_survey", na.rm = TRUE),
      is_metadata = any(concept == "cabr_bee_meta_data", na.rm = TRUE),
      transect    = { tr <- sort(unique(na.omit(concept[what_for == "transect"])))
                      if (length(tr)) paste(tr, collapse = "; ") else NA_character_ },
      .groups = "drop"
    )

  # Free-text notes: old records sometimes wrote "10 min" in the description
  # instead of using the field/tag. Fold that into is_10min so we don't miss it.
  notes_flag <- df |>
    transmute(obs_id, note = tolower(coalesce(description, ""))) |>
    mutate(note_10min = str_detect(note, "10\\s*-?\\s*min")) |>
    filter(note_10min) |> select(obs_id, note_10min)
  signals <- signals |>
    left_join(notes_flag, by = "obs_id") |>
    mutate(is_10min = is_10min | coalesce(note_10min, FALSE)) |>
    select(-any_of("note_10min"))

  # unknown HASHTAGS only (field names/notes handled by their own reports).
  unknown_tags <- toks |> filter(src == "tag", is.na(concept)) |>
    select(obs_id, tok)
  list(signals = signals, unknown_tags = unknown_tags)
}

# ------------------------------------------------------------
# Unknown obs-FIELD NAMES: field:* columns present in the data whose name isn't
# in the crosswalk's obs_field entries. Scoped to fields OUR surveyors actually
# filled, so a stranger's exotic field isn't noise. For step-2a categorizing.
# ------------------------------------------------------------
fpi_unknown_fields <- function(df, crosswalk, our_users) {
  known <- crosswalk |>
    filter(!is.na(inat_field_variants), trimws(inat_field_variants) != "") |>
    transmute(v = inat_field_variants) |>
    mutate(v = lapply(v, fpi_split_variants)) |>
    tidyr::unnest(v) |>
    mutate(k = tolower(trimws(v))) |>
    filter(k != "", !(k %in% c("n/a", "na"))) |> pull(k) |> unique()
  fld <- names(df)[startsWith(names(df), "field:")]
  unknown_cols <- fld[!(tolower(trimws(sub("^field:", "", fld))) %in% known)]
  if (!length(unknown_cols))
    return(tibble(field = character(), n_obs = integer(), n_our = integer()))
  our <- df$observer %in% our_users
  res <- purrr::map_dfr(unknown_cols, function(col) {
    ne <- !is.na(df[[col]]) & df[[col]] != ""
    tibble(field = sub("^field:", "", col), n_obs = sum(ne), n_our = sum(ne & our))
  }) |> filter(n_our > 0) |> arrange(desc(n_our), desc(n_obs))

  # attach real iNat field IDs if the map has been built (build_field_id_map.R)
  idmap_path <- "data/inat_observations/reference/inat_field_id_map_generated.csv"
  if (nrow(res) && file.exists(idmap_path)) {
    idmap <- read_csv(idmap_path, show_col_types = FALSE) |>
      transmute(k = tolower(trimws(field_name)), inat_field_id)
    res <- res |> mutate(k = tolower(trimws(field))) |>
      left_join(idmap, by = "k") |> select(-k) |>
      relocate(inat_field_id, .after = field)
  }
  res
}

# ------------------------------------------------------------
# Notes to review: our-surveyor observations whose free-text description holds a
# survey keyword (10-min, times, weather, transect, "bee survey"...). These may
# carry survey info that isn't structured into a tag/field yet.
# ------------------------------------------------------------

# ------------------------------------------------------------
# in_cabr via the real boundary (point-in-polygon). Rows with missing coords
# get in_cabr = NA (can't place them).
# ------------------------------------------------------------
fpi_in_cabr <- function(base, boundary_path) {
  boundary <- st_read(boundary_path, quiet = TRUE)
  has_xy <- !is.na(base$latitude) & !is.na(base$longitude)
  res <- rep(NA, nrow(base))
  pts <- st_as_sf(base[has_xy, ], coords = c("longitude", "latitude"), crs = 4326) |>
    st_transform(st_crs(boundary))
  inside <- lengths(st_within(pts, boundary)) > 0
  res[has_xy] <- inside
  as.logical(res)
}

# ------------------------------------------------------------
# Assemble the membership table.
# ------------------------------------------------------------
fpi_membership <- function(base, signals, roster, boundary_path) {
  roster_lk <- roster |>
    transmute(year = as.integer(year), observer = inaturalist_username, role) |>
    filter(!is.na(observer), observer != "") |>
    distinct(observer, year, .keep_all = TRUE)

  base |>
    left_join(signals, by = "obs_id") |>
    mutate(across(c(has_survey, is_exclude, is_10min, is_metadata), ~ coalesce(., FALSE)),
           in_cabr = fpi_in_cabr(base, boundary_path),
           year = as.integer(format(observed_on, "%Y"))) |>
    left_join(roster_lk, by = c("observer", "year")) |>
    mutate(
      status = case_when(is_exclude ~ "exclude",
                         has_survey ~ "keep",
                         coalesce(in_cabr, FALSE) ~ "flag",
                         TRUE ~ "not_survey"),
      surveyor_type  = if_else(status == "keep", coalesce(role, "unknown"), NA_character_),
      survey_year  = if_else(status == "keep", as.character(year), NA_character_),
      status_reason = case_when(
        status == "exclude" ~ paste0("exclude tag: ", exclude_tag),
        status == "keep" & surveyor_type == "unknown" ~ "tagged, but observer not in roster for this year -> onboard",
        status == "keep" ~ "valid Cabrillo survey tag",
        status == "flag" ~ "in CABR, no survey tag -- review",
        TRUE ~ "outside CABR, no survey tag")
    ) |>
    transmute(obs_id, kind, observer, observed_on, surveyor_type, survey_year,
              transect, is_10min, is_metadata, status, status_reason, in_cabr, obscured) |>
    arrange(observed_on, obs_id)
}

# ------------------------------------------------------------
# survey_dates + review_windows live in scripts/project_info/surveys/finding_survey_dates.R
#   fpi_survey_dates() + fpi_norm_transect() -- sourced at the top of this file.
# ------------------------------------------------------------

# ------------------------------------------------------------
# ORCHESTRATOR
# ------------------------------------------------------------
#' Rebuild the per-survey information table
#'
#' The per-year roster is derived, not typed: `people_manual.csv` says who
#' exists, the intern log says who was an intern that year, and everyone else
#' is a beeple.
#'
#' @param write Write the result.
#' @return The per-survey table.
finding_project_info <- function(write = TRUE) {
  crosswalk <- read_csv(FPI_CROSSWALK, show_col_types = FALSE)
  windows   <- read_csv(FPI_WINDOWS, show_col_types = FALSE)
  # The per-year roster the rebuild needs is DERIVED now: people_manual.csv says who exists,
  # the intern log says who was an intern that year, everyone else is a beeple. The
  # old surveyor_roster.csv is retired (see rosters/WHAT_THESE_FILES_ARE.txt).
  people    <- read.csv(PATHS$people, stringsAsFactors = FALSE, check.names = FALSE)
  intern_lg <- if (file.exists(FPI_INTERN_LOG))
                 read.csv(FPI_INTERN_LOG, stringsAsFactors = FALSE, check.names = FALSE) else NULL
  roster    <- roster_view(people, intern_lg, years = FPI_YEARS)
  tagmap    <- fpi_build_tagmap(crosswalk)

  # load + normalize each export (bee + plant). A listed-but-absent export (e.g.
  # plant, before the first plant pull) is skipped -- map_dfr drops the NULL.
  base <- purrr::map_dfr(FPI_EXPORTS, function(s) {
    if (!file.exists(s$path)) {
      bx_note("(export absent, skipping ", s$kind, ": ", s$path, ")")
      return(NULL)
    }
    x <- readRDS(s$path)
    x$obs_id <- x$id; x$kind <- s$kind
    if (!"coordinates_obscured" %in% names(x)) x$coordinates_obscured <- FALSE
    x |> mutate(observer = user_login, observed_on = as.Date(observed_on),
                obscured = coalesce(as.logical(coordinates_obscured), FALSE)) |>
      select(obs_id, kind, observer, observed_on, obscured, latitude, longitude, url,
             tag_list, description, starts_with("field:"))
  })

  sig <- fpi_signals(base, tagmap)
  membership <- fpi_membership(base, sig$signals, roster, FPI_BOUNDARY)
  # resolve each beeple survey day to its MAJORITY transect (stamps every obs of that
  # surveyor+day; interns untouched). Feeds survey_dates a single, clean transect.
  mistags <- NULL; ties <- NULL
  if (exists("resolve_transects")) {
    rt <- resolve_transects(membership); membership <- rt$membership
    mistags <- rt$mistags; ties <- rt$ties
  }
  # RESCUE: an untagged obs on its surveyor's survey-day transect IS a survey (they just missed the
  # tag). Upgrade flag->keep, marked survey_source="inferred_on_transect". Runs AFTER the majority-
  # transect resolve and BEFORE survey_dates, so it flows to n_obs + both cleaned tables at once.
  if (exists("fpi_rescue_on_transect") && file.exists(FPI_TRANSECTS)) {
    .tsf <- suppressWarnings(sf::st_read(FPI_TRANSECTS, quiet = TRUE))
    names(.tsf)[tolower(names(.tsf)) == "name"] <- "Name"
    .tsf$T <- fpi_norm_transect(.tsf$Name)
    membership <- fpi_rescue_on_transect(membership, dplyr::select(base, obs_id, latitude, longitude), .tsf)
  }
  sd_out         <- fpi_survey_dates(membership, windows, roster)
  survey_dates   <- sd_out$survey_dates
  review_windows <- sd_out$review

  # Step-2a review reports -- all scoped to OUR surveyors (a stranger's tag/field
  # isn't ours to categorize). Three separate files: hashtags, obs-field names, notes.
  our_users <- roster$inaturalist_username |> na.omit() |> unique()
  our_users <- our_users[our_users != ""]
  unknown_tags <- sig$unknown_tags |>
    left_join(base |> select(obs_id, observer), by = "obs_id") |>
    filter(observer %in% our_users) |>
    group_by(tag = tok) |>
    summarise(n_obs = dplyr::n(),
              surveyors = paste(sort(unique(observer)), collapse = "; "), .groups = "drop") |>
    mutate(cabrillo_ish = grepl("cabrill|beesurvey|beemonitor|monument|upmon|bst|transect|10min|beeple",
                                tolower(tag))) |>
    arrange(desc(cabrillo_ish), desc(n_obs))
  unknown_fields <- fpi_unknown_fields(base, crosswalk, our_users)

  if (write) {
    dir.create(dirname(FPI_MEMBERSHIP), recursive = TRUE, showWarnings = FALSE)
    write.csv(membership,     FPI_MEMBERSHIP,     row.names = FALSE, na = "")
    write.csv(survey_dates,   FPI_SURVEY_DATES,   row.names = FALSE, na = "")
    write.csv(review_windows, FPI_REVIEW,         row.names = FALSE, na = "")
    if (!is.null(mistags)) write.csv(mistags, FPI_MISTAGS, row.names = FALSE, na = "")
    if (!is.null(ties))    write.csv(ties,    FPI_TIES,    row.names = FALSE, na = "")
    write.csv(unknown_tags,   FPI_UNKNOWN_TAGS,   row.names = FALSE, na = "")
    write.csv(unknown_fields, FPI_UNKNOWN_FIELDS, row.names = FALSE, na = "")
    # participation falls out of the survey record: declared identity (people_manual.csv),
    # derived activity. Written after the master because it reads it back.
    .part <- tryCatch(build_participation(), error = function(e) { bx_note("participation: ", conditionMessage(e)); NULL })
    # identifications per person, CABR records only -- the identifier list orders on it
    invisible(tryCatch(build_identification_counts(), error = function(e) bx_note("id counts: ", conditionMessage(e))))
    bx_kv("Classified", format(nrow(membership), big.mark = ","), " observations (bees + plants)")
    bx_kv("Surveys", nrow(survey_dates), " confirmed")
    if (!is.null(.part)) bx_kv("Participation", nrow(.part), paste0(" person-years \U00B7 ",
                                length(unique(.part$person_id)), " people"))
    .fpi_review_summary(nrow(unknown_tags), nrow(unknown_fields), nrow(review_windows))
    if (!is.null(mistags)) bx_cont(nrow(mistags), " stray transect tags to fix")
    if (!is.null(ties) && nrow(ties)) bx_cont(nrow(ties), " tie day(s) to rule")
    bx_out(FPI_SURVEY_DATES)   # data/project_info/surveys/master_per_survey_info_generated.csv
    bx_out(FPI_MEMBERSHIP)     # data/inat_observations/inat_raw/cabr_inat_raw_generated.csv
    bx_cont("review files, if any, are listed above with their full paths")
  }
  invisible(list(membership = membership, survey_dates = survey_dates, review_windows = review_windows,
                 mistags = mistags, ties = ties, unknown_tags = unknown_tags, unknown_fields = unknown_fields))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0) finding_project_info()
