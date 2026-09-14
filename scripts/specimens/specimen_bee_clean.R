# =============================================================
# specimens/specimen_bee_clean.R
# beescabr -- turn the lethal-survey specimen record into an ANALYSIS-ready table
# that MIRRORS inat_bee_clean.R's schema (so lethal vs non-lethal line up column-
# for-column), swapping obs_id -> ucsd_id + sdnhm_id and keeping `sex`.
#
# WHAT IT'S FOR
#   Reads the newest specimen .xlsx (data/specimens/records/), cleans names/dates,
#   attaches taxon_id + full taxonomy from the taxonomy lookup (by name), resolves
#   the transect from the `plot` text (via the master_crosswalk specimen_label_variants,
#   with a lat/long spatial fallback for plain "Cabrillo NM"), pulls the visited
#   plant out of method_or_plant, and writes cabr_specimen_bee_clean_generated.csv plus
#   QC side files (taxonomy flags, missing, duplicates).
#
# COLUMN MAP (iNat -> specimen)
#   obs_id            -> ucsd_id + sdnhm_id
#   observer          -> collector          observed_on -> date       survey_year -> year
#   is_survey         -> TRUE (all)         surveyor_type -> "intern"   survey_note -> blank
#   survey_method     -> "lethal" (all)     (lethal/non-lethal axis; surveyor_type is the surveyor)
#   transect          -> from `plot` (crosswalk variants) + spatial fallback
#   flower_visited    -> the "ex. <plant>" value of method_or_plant (methods -> blank)
#   cabr_bee_lethal_collection -> TRUE      the 8 other behavior flags -> blank
#   (specimens with missing lat/long are NOT a column -- they go to the review folder:
#    data/specimens/specimens_clean/review/qc_review_specimen_location_missing_generated.csv)
#   taxon_id/taxon_rank + taxonomy -> from the lookup by name       sex -> kept
#   is_10min / is_metadata / quality_grade / positional_accuracy / url -> blank
#
# Runs AFTER the taxonomy lookup (stage 5) -- it reads the lookup + complex map.
# In the pipeline it's non-interactive: flags are logged and the run continues.
#
# Run: source("scripts/specimens/specimen_bee_clean.R"); clean_specimens()
# =============================================================
suppressWarnings(suppressMessages({ library(dplyr); library(stringr); library(readr); library(sf); library(readxl) }))
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

local({
  need <- function(sym, file) if (!exists(sym)) source(file.path("scripts", file))
  need("PATHS",                     "config.R")
  need("read_latest",               "utils/utils.R")
  need("require_columns",           "utils/utils.R")
  need("write_fresh",               "utils/utils.R")
  need("standardize_specimen_names","specimens/specimen_clean_helpers.R")
  need("keep_bee_specimens",        "specimens/specimen_clean_helpers.R")
})

SBC_RECORDS_DIR     <- "data/specimens/records"
SBC_RECORDS_PATTERN <- "^cabr_bee_specimens_record_V"
SBC_TRANSECTS       <- "data/spatial/shapefiles/transects/cabr_bee_transects.shp"
SBC_CROSSWALK       <- "data/project_info/crosswalk/master_crosswalk_manual.csv"
SBC_OFF_TRANSECT_M  <- 50   # a plot-less specimen within this of a transect line is assigned to it

# blank behavior flags (iNat-only annotations); cabr_bee_lethal_collection is set TRUE
SBC_BLANK_BOOL <- c("bee_on_flower", "pollen_on_bee", "feeding", "mating",
                    "bee_on_ground", "bee_nest", "bee_in_nest", "mark_recapture")
SBC_TAXONOMY_COLS <- c("scientific_name", "common_name",
                       "kingdom", "phylum", "subphylum", "class", "subclass", "order",
                       "suborder", "infraorder", "superfamily", "family", "epifamily",
                       "subfamily", "tribe", "subtribe", "genus", "subgenus", "complex",
                       "species", "subspecies")
# final column order -- inat_bee_clean's IBC_COLUMN_ORDER, obs_id -> ucsd_id+sdnhm_id, + sex
# mask_out_of_park_flowers(): PURE. For specimen rows whose resolved flower is NOT in the park
# (flower_in_park == FALSE -- an expert-flagged label misID / plant absent from CABR), hide the
# specific plant so scientists aren't shown an out-of-park identification: flower_visited becomes
# "flower - angiosperm" and flower_taxon_id / plant_genus / plant_species are cleared. The raw label
# stays in flower_visited_raw for provenance; flower_in_park stays FALSE. In-park or unresolved
# flowers (TRUE / NA) are left untouched.
SBC_ANGIOSPERM <- "flower - angiosperm"
mask_out_of_park_flowers <- function(df) {
  if (!all(c("flower_in_park", "flower_visited") %in% names(df))) return(df)
  out <- !is.na(df$flower_in_park) & tolower(trimws(as.character(df$flower_in_park))) == "false"
  if (any(out)) {
    df$flower_visited[out] <- SBC_ANGIOSPERM
    for (cc in c("flower_taxon_id", "plant_genus", "plant_species"))
      if (cc %in% names(df)) df[[cc]][out] <- NA
  }
  df
}

SBC_COLUMN_ORDER <- c("ucsd_id", "sdnhm_id", "observer", "observed_on", "is_survey", "survey_note",
                      "surveyor_type", "survey_method", "survey_year", "transect", "is_10min", "is_metadata",
                      "flower_visited", "flower_visited_raw", "flower_taxon_id", "flower_in_park", "plant_genus", "plant_species", "bee_situation", SBC_BLANK_BOOL, "cabr_bee_lethal_collection",
                      "taxon_id", "taxon_rank", "quality_grade",
                      SBC_TAXONOMY_COLS, "latitude", "longitude", "positional_accuracy",
                      "url", "sex", "determiner")

# TP/TP1 -> TP, etc. (same normalization the iNat side uses).
.sbc_norm_transect <- function(x) {
  u <- toupper(gsub("^#", "", trimws(as.character(x))))
  dplyr::case_when(u %in% c("", "NA", "N/A") ~ NA_character_,
                   startsWith(u, "TP") ~ "TP", startsWith(u, "UPMON") ~ "UPMON",
                   startsWith(u, "BST") ~ "BST", u == "OT" ~ "OT", TRUE ~ u)
}

# the plant out of method_or_plant: "ex. Encelia californica" -> "Encelia californica";
# a method ("Aerial Net", "ground") -> NA.
.sbc_flower_from_method <- function(mop) {
  x <- trimws(as.character(mop))
  ifelse(grepl("^ex\\.?\\s+", x, ignore.case = TRUE),
         trimws(sub("^ex\\.?\\s+", "", x, ignore.case = TRUE)), NA_character_)
}

# spatial transect: for rows still missing a transect, the nearest transect line
# within SBC_OFF_TRANSECT_M of the specimen's coordinates (else NA). Impure (reads
# the shapefile); returns a character vector aligned to df's rows.
sbc_transect_spatial <- function(df, transect_path = SBC_TRANSECTS, off_m = SBC_OFF_TRANSECT_M) {
  out <- rep(NA_character_, nrow(df))
  if (!file.exists(transect_path)) {
    bx_note("(transect spatial: no shapefile -- plot-less rows left blank)"); return(out)
  }
  tl <- suppressWarnings(sf::st_read(transect_path, quiet = TRUE))
  names(tl)[tolower(names(tl)) == "name"] <- "Name"
  tl$tr <- .sbc_norm_transect(tl$Name)
  groups <- tl |> dplyr::filter(!is.na(tr)) |> dplyr::group_by(tr) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  if (!nrow(groups)) return(out)
  idx <- which(!is.na(df$latitude) & !is.na(df$longitude))
  if (!length(idx)) return(out)
  pts <- sf::st_as_sf(df[idx, ], coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(tl))
  dmat <- sf::st_distance(pts, groups)
  for (k in seq_along(idx)) {
    d <- as.numeric(dmat[k, ]); m <- which.min(d)
    if (length(m) && is.finite(d[m]) && d[m] <= off_m) out[idx[k]] <- as.character(groups$tr[m])
  }
  out
}

# ------------------------------------------------------------
# clean_specimens(): read -> clean -> write. Non-interactive in the pipeline
# (interactive_ok = FALSE logs flags and continues; never hard-stops the run).
# ------------------------------------------------------------
#' Clean the specimen spreadsheet into the analysis table
#'
#' @param interactive_ok Allow prompting for a taxon id the lookup cannot find.
#'   Set `BEESCABR_NONINTERACTIVE=1` for an unattended run.
#'
#' @param prompt_fn Injection point for reading an answer.
#' @param verbose Print progress.
#' @return The cleaned specimen table, plus QC files under
#'   `specimens_clean/review/` for a human to look over.
clean_specimens <- function(interactive_ok = (Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1"),
                            prompt_fn = readline, verbose = TRUE) {
  cleaned_dir <- "data/specimens/specimens_clean"
  review_dir  <- file.path(cleaned_dir, "review")   # QC side files, grouped for someone to review
  flags_out    <- file.path(review_dir, "qc_review_specimen_taxonomy_flags_generated.csv")
  missing_out  <- file.path(review_dir, "qc_review_specimen_missing_generated.csv")
  dupes_out    <- file.path(review_dir, "qc_review_specimen_duplicates_generated.csv")
  locmiss_out  <- file.path(review_dir, "qc_review_specimen_location_missing_generated.csv")
  clean_out    <- PATHS$specimen_clean
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

  specimens_path <- read_latest(SBC_RECORDS_DIR, SBC_RECORDS_PATTERN)
  bx_kv("Specimens", "reading ", basename(specimens_path))
  raw <- suppressMessages(readxl::read_excel(specimens_path))
  # missing_specimen is OPTIONAL: a record with nothing missing may drop the column entirely.
  # Default it to NA (= none missing) so its absence never hard-stops the clean (line ~204 already
  # treats NA as not-missing). Everything else is genuinely required.
  if (!"missing_specimen" %in% names(raw)) raw$missing_specimen <- NA_character_
  require_columns(raw, c("date", "latitude", "longitude", "sdnhm_id", "ucsd_id", "collector",
                         "plot", "method_or_plant", "genus", "subgenus", "complex", "species",
                         "subspecies", "sex"), "raw specimens")
  bx_cont(nrow(raw), " rows")

  df <- raw |> parse_specimen_dates() |> standardize_specimen_names()

  # BEES ONLY (per Brandi: "only include bees, not wasps or dipterans, or anything
  # not bee"): drop wasp/fly bycatch AND fully-unidentified rows up front, so the
  # entire downstream -- taxonomy attach, spell-check flags, transect, complex match,
  # QC side files, output -- is bee-scoped. FAMILY test, not superfamily (apoid wasps
  # like Crabronidae share Apoidea with bees). The record carries `family` on every
  # ID'd row, so no real bee is lost.
  n_pre_bee <- nrow(df)
  df <- keep_bee_specimens(df)
  bx_cont("bee filter: kept ", nrow(df), " bee-family rows, dropped ",
          n_pre_bee - nrow(df), " non-bee / unidentified")

  # NATIVE bees only -- drop the honey bee (Apis mellifera), mirroring the iNat ingest.
  n_pre_apis <- nrow(df)
  df <- drop_non_native_apis(df)
  if (n_pre_apis - nrow(df) > 0)
    bx_cont("honey-bee filter: dropped ", n_pre_apis - nrow(df), " Apis mellifera (non-native) specimen(s)")

  # --- complex match (needs the internal complex map from stage 5) ---
  # BEFORE the taxonomy attach, deliberately. The complex is taxonomic identity, and
  # attach_lookup_taxonomy() resolves identity -- so a complex the MAP supplies (rather
  # than one typed into the workbook) has to exist by the time that runs, or it cannot
  # be used to place the specimen. It used to run 49 lines later, which worked only
  # because every complex in the record happens to be hand-typed.
  if (file.exists(PATHS$complex_map)) {
    cmap <- read.csv(PATHS$complex_map)
    require_columns(cmap, c("genus", "species", "complex", "complex_taxon_id"), "complex_map")
    df <- match_specimen_complex(df, build_complex_lookup(cmap))
  } else if (!"complex" %in% names(df)) df$complex <- NA_character_

  # --- taxon_id + full taxonomy + spell-check (needs the lookup) ---
  n_taxonomy <- 0L
  if (file.exists(PATHS$taxonomy_lookup)) {
    lookup <- suppressMessages(read_csv(PATHS$taxonomy_lookup, show_col_types = FALSE))
    df <- attach_lookup_taxonomy(df, lookup)

    tax_check <- lookup |> filter(!is.na(genus), genus != "") |>
      mutate(genus = str_to_title(genus), species = str_to_lower(species))
    inat_species <- if (file.exists(PATHS$checklist_sd_inat)) {
      suppressMessages(read_csv(PATHS$checklist_sd_inat, show_col_types = FALSE)) |>
        filter(!is.na(genus), genus != "", !is.na(species), species != "") |>
        mutate(genus = str_to_title(genus), species = str_to_lower(species)) |> distinct(genus, species)
    } else tibble(genus = character(), species = character())
    known <- build_known_names(tax_check, inat_species)
    flags <- compute_taxonomy_flags(df, known$genera, known$genus_species,
                                    known$subgenera, known$complexes, known$subspecies)
    write_fresh(flags, flags_out, row.names = FALSE)
    n_taxonomy <- nrow(flags)
    bx_cont("taxonomy spell-check: ", n_taxonomy, " flag(s) -> ", basename(flags_out))
    if (n_taxonomy > 0 && verbose) {
      id_col <- intersect(c("ucsd_id", "sdnhm_id"), names(flags))[1]
      show_n <- min(nrow(flags), 20L)
      for (i in seq_len(show_n)) {
        r     <- flags[i, , drop = FALSE]
        parts <- c(r$genus, r$species, r$subspecies)
        binom <- paste(parts[!is.na(parts) & nzchar(parts)], collapse = " ")
        idtag <- if (!is.na(id_col) && !is.na(r[[id_col]])) paste0("  (", r[[id_col]], ")") else ""
        bx_cont("· ", binom, " — ", r$flag_reason, idtag)
      }
      if (nrow(flags) > show_n)
        bx_cont("… and ", nrow(flags) - show_n, " more — see ", basename(flags_out))
    }
  } else {
    message("WARNING: taxonomy lookup not found (", PATHS$taxonomy_lookup,
            ") -- taxon_id + taxonomy left blank. Run the lookup (stage 5) first.")
    for (col in SBC_TAXONOMY_COLS) if (!col %in% names(df)) df[[col]] <- NA_character_
    df$taxon_id <- NA; df$taxon_rank <- NA_character_
  }

  # --- transect: crosswalk plot-variant match, spatial fallback for the rest ---
  cw <- if (file.exists(SBC_CROSSWALK)) suppressMessages(read_csv(SBC_CROSSWALK, show_col_types = FALSE)) else NULL
  by_plot <- match_plot_transect(df$plot, transect_variant_map(cw))
  need_sp <- is.na(by_plot)
  by_spatial <- if (any(need_sp)) sbc_transect_spatial(df) else rep(NA_character_, nrow(df))
  df$transect <- ifelse(is.na(by_plot), by_spatial, by_plot)
  bx_cont("transect: ", sum(!is.na(by_plot)), " from plot, ",
          sum(need_sp & !is.na(by_spatial)), " from lat/long, ",
          sum(is.na(df$transect)), " unresolved")

  # --- schema columns ---
  df$observer   <- df$collector
  df$observed_on <- df$date_clean
  # Off-transect collections (no resolvable transect -- e.g. S O'Dell's targeted
  # off-transect netting) count toward the park total but are NOT survey events,
  # so a specimen with no transect is marked is_survey = FALSE.
  has_transect   <- !is.na(df$transect) & nzchar(trimws(as.character(df$transect)))
  df$is_survey   <- has_transect
  df$survey_note <- ifelse(has_transect, NA_character_,
                           "off-transect collection (counts to park total, not a survey)")
  df$surveyor_type <- "intern"     # surveyor category (interns collect the specimens)
  df$survey_method <- "lethal"   # specimens are the LETHAL survey
  df$survey_year <- df$year
  df$is_10min <- NA; df$is_metadata <- NA
  df$flower_visited     <- .sbc_flower_from_method(df$method_or_plant)
  df$flower_visited_raw <- df$flower_visited                                              # the intern's label, verbatim
  df$flower_visited     <- normalize_flower_name(df$flower_visited, plant_variant_map(cw)) # -> canonical name via master_crosswalk
  if (!exists("attach_flower_ids")) source("scripts/reference/taxonomy/plant_lookup_join.R")
  df <- attach_flower_ids(df)                                                             # flower_taxon_id + flower_in_park from the plant lookup
  df <- mask_out_of_park_flowers(df)                                                      # not-in-park plants -> "flower - angiosperm" (don't show scientists an out-of-park ID)
  df$bee_situation  <- sbc_bee_situation(df)   # on_flower / on_ground / aerial (mirrors inat_bee_clean)
  for (b in SBC_BLANK_BOOL) df[[b]] <- NA
  df$cabr_bee_lethal_collection <- TRUE
  df$location_needs_fix <- is.na(df$latitude) | is.na(df$longitude)
  df$quality_grade <- NA_character_
  df$positional_accuracy <- NA
  df$url <- NA_character_
  for (col in SBC_TAXONOMY_COLS) if (!col %in% names(df)) df[[col]] <- NA_character_

  # --- QC side files (data/specimens/specimens_clean/review/ -- for someone to review + fix in the raw .xlsx) ---
  qc <- add_qc_flags(df)
  miss_rows   <- qc |> filter(!is.na(missing_specimen) & missing_specimen == "Y")
  dupe_rows   <- detect_duplicate_ids(df)
  loc_missing <- df |> filter(is.na(latitude) | is.na(longitude)) |>
    select(any_of(c("ucsd_id", "sdnhm_id", "collector", "observed_on", "plot", "transect", "latitude", "longitude")))
  write_fresh(miss_rows,   missing_out, row.names = FALSE)
  write_fresh(dupe_rows,   dupes_out,   row.names = FALSE)
  write_fresh(loc_missing, locmiss_out, row.names = FALSE)

  # --- ONE review checkpoint: surface every review-folder issue so none is silently missed ---
  review_items <- data.frame(
    label = c("unknown names (typo or new taxon)", "duplicate IDs", "missing lat/long", "missing specimens"),
    count = c(n_taxonomy, nrow(dupe_rows), nrow(loc_missing), nrow(miss_rows)),
    file  = basename(c(flags_out, dupes_out, locmiss_out, missing_out)),
    what  = .review_what_specimens(),
    stringsAsFactors = FALSE)
  if (resolve_review_gate(review_items, review_dir, interactive_ok, prompt_fn,
                          fix_hint = .specimen_fix_hint()) == "stop")
    stop("Stopping so you can review/fix the flagged rows in the raw .xlsx, then re-run. Review files: ", review_dir)

  # determiner provenance: map the raw "determination" code (initials + surname) to the identifier
  # roster's iNaturalist username (READ-ONLY -- never writes the roster). Any code the roster does not
  # cover is left blank on the row and listed in a review file for a human to reconcile.
  if ("determination" %in% names(df)) {
    det_roster <- tryCatch(read.csv(PATHS$people, stringsAsFactors = FALSE, check.names = FALSE),
                           error = function(e) NULL)
    det <- resolve_determiners(df$determination, det_roster)
    df$determiner <- det$determiner
    det_bad <- which(det$status %in% c("unknown", "ambiguous"))
    if (length(det_bad)) {
      det_rev <- unique(data.frame(ucsd_id = df$ucsd_id[det_bad], sdnhm_id = df$sdnhm_id[det_bad],
                                   determination = det$code[det_bad], determiner_status = det$status[det_bad],
                                   stringsAsFactors = FALSE))
      write_fresh(det_rev, file.path(review_dir, "cabr_specimen_bee_determiner_unmatched.csv"), row.names = FALSE)
    }
    bx_cont("determiner: ", sum(!is.na(df$determiner)), " matched to roster",
            if (length(det_bad)) paste0(" \U00B7 ", length(det_bad), " unmatched") else "")
  }
  clean <- df |> strip_control_chars() |> select(any_of(SBC_COLUMN_ORDER))

  # bake the current IUCN Red List status onto each species (fetched once, cache-backed,
  # offline-safe) so the analysis layer reads a column instead of hitting the network.
  if (!exists("enrich_iucn_columns")) source("scripts/reference/refresh/enrich_lookups.R")
  clean <- tryCatch(enrich_iucn_columns(clean),
                    error = function(e) { message("  !! IUCN enrichment skipped: ", conditionMessage(e)); clean })

  dir.create(dirname(clean_out), recursive = TRUE, showWarnings = FALSE)
  write.csv(clean, clean_out, row.names = FALSE, na = "")

  bx_kv("Specimens", format(nrow(clean), big.mark = ","), " cleaned rows")
  bx_out(basename(clean_out))
  bx_cont(sum(!is.na(clean$taxon_id)), " with taxon_id | ",
          sum(!is.na(clean$flower_visited)), " with flower_visited | ",
          sum(df$location_needs_fix), " missing lat/long -> review/", basename(locmiss_out))
  invisible(list(clean = clean))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message("Sourced specimen_bee_clean.R -- clean_specimens() writes the labeled CABR specimen table (mirrors inat_bee_clean).")
