# =============================================================
# inat_observations/clean/inat_bee_clean.R
# beescabr -- turn the brain's per-obs answer key into an ANALYSIS-ready iNaturalist BEE table.
#
# WHAT IT'S FOR
#   The brain (finding_project_info.R) writes data/inat_observations/inat_raw/cabr_inat_raw_generated.csv: one row per
#   iNat bee obs, already stamped (by the master_crosswalk) with a status, whose survey it is, the
#   resolved transect, and an in_cabr label -- but taxonomy-blind (no taxon_id / names) and with no
#   coordinates. This script pulls every bee obs INSIDE the CABR survey box (in_cabr == TRUE), labels
#   each survey-or-not, joins on identity + coordinates + crosswalk ANNOTATIONS from the iNat export.
#
#   "In CABR" uses cabr_survey_box.shp (the generous hand-drawn box), on purpose: it keeps ocean /
#   misplaced pins IN so they aren't lost.
#
# CROSSWALK ANNOTATIONS (per obs, from its iNat obs-fields + tags via master_crosswalk_manual.csv)
#   * flower_visited -- the PLANT SPECIES the bee visited (a real value, e.g. "Encelia californica"),
#     coalesced from the visited-plant obs-fields (most-populated first).
#   * yes/no flags: bee_on_flower, pollen_on_bee, feeding, mating, bee_on_ground, bee_nest,
#     bee_in_nest, mark_recapture, cabr_bee_lethal_collection.
#   (flower_flowering is a PLANT concept -- excluded here; cabr_bee_lethal_collection is specimen-side,
#   so it's blank in the iNat bee table but kept for schema consistency.)
#
# SPATIAL SURVEY REFINEMENT (Humphreys Rd walk-in) + location_needs_fix
#   A tagged (keep) obs whose PIN sits off EVERY transect yet ON the access road to BST (Humphreys Rd)
#   is a WALK-IN: logged on the way to the transect, not a transect survey -> is_survey = FALSE (with a
#   survey_note). A tagged obs off every transect AND off the road stays a survey (likely a bad GPS pin)
#   and gets location_needs_fix = TRUE -- an extra-precaution "go fix this pin" marker. Shapefiles are
#   optional; absent either one, nothing is re-marked. (The pin-map visualising this is kept as a
#   reference artifact next to the road layer, not run in the pipeline.)
#
# TAXONOMY (2026-07-20) -- now FILLED, not a scaffold:
#   * scientific_name, common_name, kingdom..subspecies are filled from the taxonomy lookup by
#     taxon_id (ibc_fill_taxonomy) -- the lookup is built EARLIER in the run. A taxon_id absent
#     from the lookup leaves that row's taxonomy blank.
#   * is_10min / is_metadata are STILL BLANK -- the crosswalk has no NOTE variants yet.
#   taxon_id is the real identity we compare bees on; taxon_rank rides along from the iNat export.
#
# INPUTS   data/inat_observations/inat_raw/cabr_inat_raw_generated.csv                         (the brain's per-obs lookup)
#          data/inat_observations/cache/export_flat.rds                     (taxon_id + coords + fields/tags)
#          data/project_info/crosswalk/master_crosswalk_manual.csv                      (field/tag -> annotation concept)
#          data/spatial/shapefiles/transects/cabr_bee_transects.shp               (off-transect test)
#          data/spatial/shapefiles/access_routes_to_transects/cabr_survey_access_routes.shp (Humphreys Rd walk-in)
# OUTPUT   data/inat_observations/inat_clean/cabr_inat_bee_clean_generated.csv        (one labeled CABR table)
#
# Run: source("scripts/inat_observations/clean/inat_bee_clean.R"); inat_bee_clean()
# =============================================================
if (!exists("PATHS")) source("scripts/config.R")   # centralized paths (see PATHS in config.R)
suppressWarnings(suppressMessages({library(dplyr); library(readr); library(sf)}))
if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

IBC_MEMBERSHIP     <- "data/inat_observations/inat_raw/cabr_inat_raw_generated.csv"
IBC_EXPORT         <- "data/inat_observations/cache/export_flat.rds"
IBC_CROSSWALK      <- "data/project_info/crosswalk/master_crosswalk_manual.csv"
IBC_TRANSECTS      <- "data/spatial/shapefiles/transects/cabr_bee_transects.shp"   # Name: TP/UPMON/BST/OT
IBC_ROAD           <- "data/spatial/shapefiles/access_routes_to_transects/cabr_survey_access_routes.shp"  # Humphreys Rd
IBC_OUT_CLEAN      <- PATHS$inat_clean
IBC_FIX_SURVEY     <- "data/inat_observations/review/qc_review_inat_bee_behavior_survey_generated.csv"     # behavior fields to fix -- SURVEY obs
IBC_FIX_NONSURVEY  <- "data/inat_observations/review/qc_review_inat_bee_behavior_nonsurvey_generated.csv"  # behavior fields to fix -- CASUAL (non-survey) obs
IBC_LOCATION_REVIEW <- "data/inat_observations/review/location/qc_review_inat_bee_location_generated.csv"  # heads-up worklist: survey pins to re-check on iNat (lives with the per-observer maps)
IBC_LOOKUP         <- PATHS$taxonomy_lookup   # taxon_id -> taxonomy fill
IBC_OFF_TRANSECT_M <- 50   # a pin farther than this from EVERY transect line is "off transect"
IBC_ROAD_BUFFER_M  <- 10   # off-transect AND within this of the access road = walk-in (not a survey)

# crosswalk annotation concepts carried onto each obs
IBC_BOOL_ANNOT <- c("bee_on_flower", "pollen_on_bee", "feeding", "mating",
                    "bee_on_ground", "bee_nest", "bee_in_nest", "mark_recapture",
                    "cabr_bee_lethal_collection")             # yes/no
IBC_ANNOT_COLS <- c("flower_visited", IBC_BOOL_ANNOT)          # flower_visited (value) first

# taxonomy columns -- filled from the taxonomy lookup by taxon_id (ibc_fill_taxonomy)
IBC_TAXONOMY_COLS <- c("scientific_name", "common_name",
                       "kingdom", "phylum", "subphylum", "class", "subclass", "order",
                       "suborder", "infraorder", "superfamily", "family", "epifamily",
                       "subfamily", "tribe", "subtribe", "genus", "subgenus", "complex",
                       "species", "subspecies")

# final column order for the clean table
IBC_COLUMN_ORDER <- c("obs_id", "observer", "observed_on", "is_survey", "survey_note", "survey_source",
                      "surveyor_type", "survey_method", "survey_year", "transect", "is_10min", "is_metadata",
                      IBC_ANNOT_COLS, "flower_answered_no", "flower_taxon_id", "flower_in_park", "plant_genus", "plant_species", "bee_situation",
                      "taxon_id", "taxon_rank", "quality_grade",
                      IBC_TAXONOMY_COLS,
                      "latitude", "longitude", "positional_accuracy", "url")

# ---- taxonomy fill ---------------------------------------------------------
# ibc_fill_taxonomy(): PURE. Fill scientific_name / common_name / kingdom..subspecies from the
# taxonomy lookup, joined by taxon_id (the lookup is the source of truth for the names). A taxon_id
# absent from the lookup leaves that row's taxonomy blank. Every IBC_TAXONOMY_COLS column is
# guaranteed present (blank when the lookup is NULL/empty or lacks it) so the schema stays stable,
# and any stale taxonomy columns already on df are dropped first so a re-run never duplicates one.
ibc_fill_taxonomy <- function(df, lookup) {
  df$taxon_id <- as.character(df$taxon_id)
  ensure_blank <- function(d) {
    for (col in IBC_TAXONOMY_COLS) if (!col %in% names(d)) d[[col]] <- NA_character_
    d
  }
  if (is.null(lookup) || !nrow(lookup) || !"taxon_id" %in% names(lookup))
    return(ensure_blank(df))
  tcols <- intersect(IBC_TAXONOMY_COLS, names(lookup))
  if (!length(tcols)) return(ensure_blank(df))
  lk2 <- lookup |>
    mutate(taxon_id = as.character(taxon_id)) |>
    filter(!is.na(taxon_id), taxon_id != "") |>
    select(taxon_id, all_of(tcols)) |>
    distinct(taxon_id, .keep_all = TRUE)
  df |> select(-any_of(IBC_TAXONOMY_COLS)) |> left_join(lk2, by = "taxon_id") |> ensure_blank()
}

# TP / TP1 / TP2 -> TP, etc. (same rule the brain + resolver use)
ibc_norm_transect <- function(x) {
  u <- toupper(gsub("^#", "", trimws(as.character(x))))
  dplyr::case_when(
    u %in% c("", "NA", "N/A") ~ NA_character_,
    startsWith(u, "TP")    ~ "TP",
    startsWith(u, "UPMON") ~ "UPMON",
    startsWith(u, "BST")   ~ "BST",
    u == "OT"              ~ "OT",
    TRUE                   ~ u)
}

# ---- crosswalk annotations -------------------------------------------------
# From each obs's iNat obs-fields (field:* columns) + tags, per master_crosswalk_manual.csv:
#   flower_visited -> the plant value (coalesced, most-populated field first)
#   the IBC_BOOL_ANNOT concepts -> TRUE if any of the concept's field variants is populated or any
#   of its tag variants is present. Returns tibble(obs_id, flower_visited, <bool annot...>).
ibc_annotations <- function(ex_full, crosswalk_path) {
  ids <- as.character(ex_full$id)
  out <- tibble(obs_id = ids, flower_visited = NA_character_)
  for (cc in IBC_BOOL_ANNOT) out[[cc]] <- FALSE
  if (!file.exists(crosswalk_path)) { bx_note("annotations: no crosswalk -- skipped"); return(out) }
  cw <- suppressWarnings(read_csv(crosswalk_path, show_col_types = FALSE))
  splitv <- function(s) { s <- s[!is.na(s)]; if (!length(s)) return(character(0))
                          tolower(trimws(unlist(strsplit(s, "[;,]")))) }
  fcols <- grep("^field:", names(ex_full), value = TRUE); names(fcols) <- tolower(sub("^field:", "", fcols))
  tags  <- lapply(strsplit(tolower(ifelse(is.na(ex_full$tag_list), "", as.character(ex_full$tag_list))), "[;,|]"), trimws)
  nonempty <- function(x) !is.na(x) & trimws(as.character(x)) != ""

  for (cc in IBC_BOOL_ANNOT) {
    fv <- splitv(cw$inat_field_variants[cw$name == cc]); tv <- splitv(cw$inat_tag_variants[cw$name == cc])
    fc <- fcols[intersect(fv, names(fcols))]
    fhit <- if (length(fc)) Reduce(`|`, lapply(as.data.frame(ex_full[, fc, drop = FALSE]), nonempty)) else rep(FALSE, length(ids))
    thit <- if (length(tv)) vapply(tags, function(t) any(t %in% tv), logical(1)) else rep(FALSE, length(ids))
    out[[cc]] <- fhit | thit
  }

  # bee_on_flower is a YES/NO field, so being value-aware matters: the loop above set it TRUE
  # whenever the "insect on flower" field was populated, which wrongly marks a recorded "No"
  # (bee NOT on a flower, e.g. a Nomada on the ground) as on-a-flower. Redo it: TRUE only for an
  # affirmative value (or an affirmative tag). Also REMEMBER an explicit "No" in flower_answered_no
  # so the review step never asks a reviewer to add a flower to a bee the observer said wasn't on one.
  out$flower_answered_no <- FALSE
  yes_val <- function(x) { x <- trimws(tolower(as.character(x))); x %in% c("yes", "true", "1", "y", "t") }
  no_val  <- function(x) { x <- trimws(tolower(as.character(x))); x %in% c("no", "false", "0", "n", "f") }
  of_fv <- splitv(cw$inat_field_variants[cw$name == "bee_on_flower"]); of_fc <- fcols[intersect(of_fv, names(fcols))]
  of_tv <- splitv(cw$inat_tag_variants[cw$name == "bee_on_flower"])
  of_yes <- rep(FALSE, length(ids)); of_no <- rep(FALSE, length(ids))
  for (c in of_fc) { of_yes <- of_yes | yes_val(ex_full[[c]]); of_no <- of_no | no_val(ex_full[[c]]) }
  of_tag <- if (length(of_tv)) vapply(tags, function(t) any(t %in% of_tv), logical(1)) else rep(FALSE, length(ids))
  out$bee_on_flower      <- of_yes | of_tag                 # affirmative only -> a recorded "No" is FALSE
  out$flower_answered_no <- of_no & !out$bee_on_flower       # observer explicitly said NOT on a flower

  # flower_visited value: coalesce the visited-plant fields, most-populated field first
  fvf <- splitv(cw$inat_field_variants[cw$name == "flower_visited"])
  fvc <- fcols[intersect(fvf, names(fcols))]
  if (length(fvc)) {
    fvc <- fvc[order(-vapply(fvc, function(c) sum(nonempty(ex_full[[c]])), integer(1)))]
    val <- rep(NA_character_, length(ids))
    for (c in fvc) { v <- trimws(as.character(ex_full[[c]])); take <- is.na(val) & nonempty(v); val[take] <- v[take] }
    out$flower_visited <- val
  }
  out
}

# ---- bee situation ---------------------------------------------------------
# ibc_bee_situation(): PURE. Each bee obs's recorded situation, by PRIORITY
#   on_flower > on_ground > nest > missing.
# A recorded flower_visited PLANT counts as on_flower (most surveyors log the plant,
# not the bee_on_flower yes/no -- 145 checked the flag, 8,705 recorded a plant).
# "missing" = the obs carries NONE of these -- the surveyor left off the behavioral
# observation field; the SURVEY obs among these are handed back for manual annotation.
# NA-safe; returns a character vector aligned to df's rows.
ibc_bee_situation <- function(df) {
  n  <- nrow(df)
  tf <- function(c) { x <- if (c %in% names(df)) df[[c]] else rep(NA, n); as.logical(x) %in% TRUE }
  has_plant <- if ("flower_visited" %in% names(df))
    !is.na(df$flower_visited) & trimws(as.character(df$flower_visited)) != "" else rep(FALSE, n)
  on_flower <- tf("bee_on_flower") | has_plant
  on_ground <- tf("bee_on_ground")
  nest      <- tf("bee_nest") | tf("bee_in_nest")
  dplyr::case_when(on_flower ~ "on_flower",
                   on_ground ~ "on_ground",
                   nest      ~ "nest",
                   TRUE      ~ "missing")
}

# ---- fix-behavior worklist -------------------------------------------------
# ibc_fix_behavior(): PURE. From the clean table, the hand-back worklist of obs whose
# behavioral fields need FIXING (someone opens each on iNat and corrects it). Applied to
# ALL obs; clean() then SPLITS the result into a survey and a non-survey file (same
# columns, same reasons) so each audience gets its own list:
#   * flower_not_a_plant_or_unresolved -- a flower_visited that carries no flower_taxon_id,
#     i.e. it isn't a plant in the lookup (a wrong taxon in the flower field, or a typo).
#   * flower_not_to_genus -- a flower_visited that DOES resolve to a real plant taxon (it has a
#     flower_taxon_id) but only at a rank ABOVE genus (family/order, e.g. "Cactaceae",
#     "Asteraceae"), so plant_genus is blank. The plant is too coarse to use as forage evidence
#     -> refine it to at least genus. (This is the Diadasia-on-"cactus" case.)
#   * on_flower_but_no_plant -- the bee_on_flower flag is ticked (a foraging interaction IS
#     recorded) but no plant was entered. The visit is confirmed; only the plant is missing
#     -> add it. A bee on the ground or at a nest is NOT on a flower, so those are excluded.
#   * missing_all_behavior_fields -- the obs recorded no behavior at all (bee_situation ==
#     "missing"): no plant, no on-flower flag, not on-ground, not at a nest. For a survey obs
#     the surveyor left the field off; for a casual obs it's a public photo nobody annotated
#     -> in both cases, check the photo and add the plant if the bee is on one.
# The `action` prompt is tailored per reason (and, for "missing", per survey vs casual).
# `is_survey` is carried through so clean() can split; NA-safe; priority bad-flower >
# not-to-genus > on-flower-no-plant > missing.
ibc_fix_behavior <- function(clean) {
  n   <- nrow(clean)
  col <- function(c) if (c %in% names(clean)) clean[[c]] else rep(NA, n)
  tf  <- function(c) as.logical(col(c)) %in% TRUE
  fv   <- col("flower_visited"); ftid <- col("flower_taxon_id"); pg <- col("plant_genus")
  has_flower <- !is.na(fv) & trimws(as.character(fv)) != ""
  bad_flower <- has_flower & (is.na(ftid) | trimws(as.character(ftid)) == "")
  has_genus  <- !is.na(pg) & trimws(as.character(pg)) != ""                 # flower resolved to a PLANT GENUS
  # flower resolves to a real plant taxon (has a taxon_id) but only ABOVE genus rank
  # (family/order, e.g. "Cactaceae", "Asteraceae") -> plant_genus is blank. These used to
  # fall through every reason (they carry a plant, so not bad/blank/missing); now flagged
  # so the reviewer refines the plant ID to at least genus (Diadasia's "cactus" problem).
  not_to_genus <- has_flower & !bad_flower & !has_genus
  is_surv <- as.logical(col("is_survey")) %in% TRUE
  sit     <- as.character(col("bee_situation"))
  answered_no <- as.logical(col("flower_answered_no")) %in% TRUE            # observer said "insect on flower = No"
  ground_nest <- tf("bee_on_ground") | tf("bee_nest") | tf("bee_in_nest")  # NOT on a flower -> exclude
  on_flower_no_plant <- tf("bee_on_flower") & !has_flower & !ground_nest    # flower confirmed, plant blank
  reason  <- dplyr::case_when(
    bad_flower                       ~ "flower_not_a_plant_or_unresolved",
    not_to_genus                     ~ "flower_not_to_genus",
    on_flower_no_plant               ~ "on_flower_but_no_plant",
    sit == "missing" & !answered_no  ~ "missing_all_behavior_fields",   # NOT flagged if observer answered "No"
    TRUE                             ~ NA_character_)
  # plain-language "do this" prompt so the worklist tells the reviewer what to fix, not just why
  action <- dplyr::case_when(
    reason == "flower_not_a_plant_or_unresolved"    ~ "Open on iNaturalist: the flower entry is not a known plant (typo or wrong taxon) -- correct the plant name or clear it.",
    reason == "flower_not_to_genus"                 ~ sprintf("Open on iNaturalist: the flower is only identified to %s -- refine the plant ID to at least genus.",
                                                              ifelse(has_flower, paste0("\"", trimws(as.character(fv)), "\" (above genus rank)"), "family/order")),
    reason == "on_flower_but_no_plant"              ~ "Open on iNaturalist: the bee is marked on a flower but the plant is blank -- add the plant it is visiting.",
    reason == "missing_all_behavior_fields" &  is_surv ~ "Open on iNaturalist: survey obs with no behavior recorded -- add the plant it is on (or mark on-ground / at-nest).",
    reason == "missing_all_behavior_fields" & !is_surv ~ "Open on iNaturalist: no behavior recorded -- if the bee is on a flower, add the plant it is visiting (and tick bee-on-flower).",
    TRUE                                            ~ NA_character_)
  clean |>
    mutate(fix_reason = reason, action = action, is_survey = is_surv) |>
    filter(!is.na(fix_reason)) |>
    select(any_of(c("obs_id", "observer", "observed_on", "transect", "taxon_id",
                    "scientific_name", "flower_visited", "plant_genus", "fix_reason", "action", "url", "is_survey")))
}

# ---- pins that landed in the water ----------------------------------------
# A known beeple/intern problem: an observation's GPS lands offshore. Those pins are
# unusable for anything spatial, and only the observer can fix them on iNaturalist, so
# they join the same per-surveyor "pins to fix" worklist as other bad pins.
#
# TOLERANCE, and why it is not zero: cabr_boundary (NPS) is digitized slightly seaward
# of the City/County coastline (see dev-docs/SPATIAL_MAPPING.md), and GPS itself drifts
# a few metres. A pin standing on the shore can therefore sit just outside a land
# polygon while being perfectly correct. Only pins more than tol_m beyond the land are
# called water. Missing coords, or no land layer at all, never flag -- we do not guess.
IBC_WATER_TOL_M <- 50
ibc_water_flag <- function(lat, lon, land, tol_m = IBC_WATER_TOL_M) {
  n <- max(length(lat), length(lon))
  out <- rep(FALSE, n)
  if (is.null(land) || !requireNamespace("sf", quietly = TRUE)) return(out)
  ok <- which(!is.na(lat) & !is.na(lon))
  if (!length(ok)) return(out)
  pts <- sf::st_as_sf(data.frame(lat = lat[ok], lon = lon[ok]),
                      coords = c("lon", "lat"), crs = 4326)
  # metric CRS so tol_m is genuinely metres (CA Albers, same as PROJECT_CRS)
  pts_m  <- sf::st_transform(pts, 3310)
  land_m <- sf::st_transform(sf::st_union(sf::st_geometry(land)), 3310)
  d <- as.numeric(sf::st_distance(pts_m, land_m))   # 0 when inside the polygon
  out[ok] <- d > tol_m
  out
}

# ---- location-fix worklist ------------------------------------------------
# ibc_location_review(): PURE. Survey obs flagged location_needs_fix (a GPS pin far from every
# transect -- likely a bad pin) -> a heads-up worklist so a scientist re-checks the pin on
# iNaturalist. Kept OUT of the clean table (a review artifact, not a data column), mirroring the
# specimen side. Each row carries its url so the reviewer can open it. Takes the pre-select `df`
# (which still has location_needs_fix). NA-safe.
IBC_LOCATION_COLS <- c("obs_id", "observer", "observed_on", "transect", "taxon_id",
                       "scientific_name", "latitude", "longitude", "url")
ibc_location_review <- function(df, land = NULL) {
  cols <- intersect(IBC_LOCATION_COLS, names(df))
  has_flag <- "location_needs_fix" %in% names(df)
  if (!has_flag && is.null(land)) return(df[0, cols, drop = FALSE])

  far   <- if (has_flag) as.logical(df$location_needs_fix) %in% TRUE else rep(FALSE, nrow(df))
  water <- if (!is.null(land) && all(c("latitude", "longitude") %in% names(df)))
             ibc_water_flag(df$latitude, df$longitude, land) else rep(FALSE, nrow(df))

  keep <- which(far | water)
  out  <- df[keep, cols, drop = FALSE]
  if (nrow(out)) {
    # water wins when both apply: it is the more specific problem and the pin cannot be
    # anywhere near a transect if it is offshore.
    out$fix_reason <- ifelse(
      water[keep],
      "in_water: this pin landed in the ocean -- move it to where you actually saw the bee",
      "bad_coord: survey pin far from any transect -- check the pin on iNaturalist")
  }
  out
}

# ---- spatial survey flags --------------------------------------------------
# on_transect / walk_in (off transect but on Humphreys Rd) / bad_coord (off transect AND off road).
# Missing shapefile / coords -> "on_transect" (nothing re-marked).
ibc_spatial_flags <- function(keep_rows, transect_path, road_path,
                              off_m = IBC_OFF_TRANSECT_M, road_m = IBC_ROAD_BUFFER_M) {
  base <- tibble(obs_id = as.character(keep_rows$obs_id), spatial_cat = "on_transect")
  if (!file.exists(transect_path)) {
    bx_note("spatial flags: no transect shapefile -- keep rows left as survey"); return(base)
  }
  tl <- suppressWarnings(sf::st_read(transect_path, quiet = TRUE))
  names(tl)[tolower(names(tl)) == "name"] <- "Name"
  tl$tr <- ibc_norm_transect(tl$Name)
  lines <- tl |> dplyr::filter(!is.na(tr)) |> dplyr::summarise(geometry = sf::st_union(geometry))
  if (!nrow(lines)) return(base)

  cand <- keep_rows |>
    transmute(obs_id = as.character(obs_id), latitude, longitude,
              obsc = coalesce(as.logical(coordinates_obscured), FALSE)) |>
    filter(!is.na(latitude), !is.na(longitude), !obsc)
  if (!nrow(cand)) return(base)

  pts <- sf::st_as_sf(cand, coords = c("longitude", "latitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(tl))
  d_near <- as.numeric(sf::st_distance(pts, lines))

  if (file.exists(road_path)) {
    rd <- suppressWarnings(sf::st_read(road_path, quiet = TRUE)) |> sf::st_transform(sf::st_crs(tl))
    d_road <- as.numeric(sf::st_distance(pts, sf::st_union(rd)))
  } else {
    bx_note("spatial flags: no access-road shapefile -- walk-in not identified")
    d_road <- rep(Inf, nrow(pts))
  }

  cand$spatial_cat <- dplyr::case_when(
    d_near <= off_m  ~ "on_transect",
    d_road <= road_m ~ "walk_in",
    TRUE             ~ "bad_coord")

  base |>
    dplyr::left_join(cand |> dplyr::select(obs_id, sc = spatial_cat), by = "obs_id") |>
    dplyr::mutate(spatial_cat = dplyr::coalesce(sc, spatial_cat)) |>
    dplyr::select(obs_id, spatial_cat)
}

# ---- main ------------------------------------------------------------------
#' Clean the raw iNaturalist bee observations into the analysis table
#'
#' @param membership_path Which observations belong to the project.
#' @param export_path The flattened observation export.
#' @param crosswalk_path Hand-maintained tag and field corrections.
#' @param transect_path Transect lines, for assigning a record to a transect.
#' @param road_path Road layer, used to keep the coastal buffer honest.
#' @param out_clean Where to write the cleaned table.
#' @param write Write the result. `FALSE` returns it without touching disk.
#' @return The cleaned bee table, carrying `taxon_id` on every row.
inat_bee_clean <- function(membership_path = IBC_MEMBERSHIP,
                           export_path     = IBC_EXPORT,
                           crosswalk_path  = IBC_CROSSWALK,
                           transect_path   = IBC_TRANSECTS,
                           road_path       = IBC_ROAD,
                           out_clean       = IBC_OUT_CLEAN,
                           write = TRUE) {
  stopifnot(file.exists(membership_path), file.exists(export_path))

  mem <- suppressWarnings(read_csv(membership_path, show_col_types = FALSE))
  if ("kind" %in% names(mem)) mem <- mem |> filter(kind == "bee")
  # whole-CABR = everything in the survey box that isn't a hard exclude (keep + flag)
  mem <- mem |> filter(status %in% c("keep", "flag"))
  mem$obs_id <- as.character(mem$obs_id)

  ex_full <- readRDS(export_path)
  ex <- tibble(
    obs_id               = as.character(ex_full$id),
    taxon_id             = as.character(ex_full$taxon_id),
    taxon_rank           = if ("rank" %in% names(ex_full)) as.character(ex_full$rank) else NA_character_,
    quality_grade        = ex_full$quality_grade,
    latitude             = ex_full$latitude,
    longitude            = ex_full$longitude,
    positional_accuracy  = ex_full$positional_accuracy,
    coordinates_obscured = if ("coordinates_obscured" %in% names(ex_full)) ex_full$coordinates_obscured else NA,
    url                  = ex_full$url) |>
    distinct(obs_id, .keep_all = TRUE)

  annot <- ibc_annotations(ex_full, crosswalk_path) |> distinct(obs_id, .keep_all = TRUE)

  df <- mem |> left_join(ex, by = "obs_id") |> left_join(annot, by = "obs_id")
  for (cc in IBC_BOOL_ANNOT) df[[cc]] <- coalesce(df[[cc]], FALSE)

  # spatial refinement: re-mark walk-in (off-transect but on Humphreys Rd) keep rows as NOT survey
  keep_full <- df |> filter(status == "keep")
  flags <- ibc_spatial_flags(keep_full, transect_path, road_path)
  df <- df |> left_join(flags, by = "obs_id")
  df$spatial_cat <- coalesce(df$spatial_cat, "on_transect")

  df$is_survey <- df$status == "keep" & df$spatial_cat != "walk_in"
  df$survey_note <- dplyr::case_when(
    df$status == "keep" & df$spatial_cat == "walk_in"   ~ "walk-in: off-transect on Humphreys Rd (not a transect survey)",
    df$status == "keep" & df$spatial_cat == "bad_coord" ~ "off-transect: GPS pin far from any transect -- check pin",
    TRUE ~ NA_character_)
  df$location_needs_fix <- df$is_survey & df$spatial_cat == "bad_coord"

  # taxonomy: fill from the lookup by taxon_id (the lookup is built EARLIER in the run now).
  # A taxon_id not in the lookup stays blank; is_10min/is_metadata blank until note variants.
  if (file.exists(IBC_LOOKUP)) {
    lk <- suppressMessages(read_csv(IBC_LOOKUP, show_col_types = FALSE))
    df <- ibc_fill_taxonomy(df, lk)
    bx_cont("taxonomy: filled ", format(sum(!is.na(df$scientific_name)), big.mark = ","),
            "/", format(nrow(df), big.mark = ","), " rows from the lookup")
  } else {
    df <- ibc_fill_taxonomy(df, NULL)
    bx_note("taxonomy: lookup not found (", IBC_LOOKUP, ") -- columns left blank")
  }
  df$is_10min    <- NA
  df$is_metadata <- NA
  if (!"survey_source" %in% names(df)) df$survey_source <- NA_character_   # tag / inferred_on_transect
  df$survey_method <- "nonlethal"   # iNaturalist observations are the NON-lethal survey
  df$bee_situation <- ibc_bee_situation(df)   # on_flower / on_ground / nest / missing
  if (!exists("attach_flower_ids")) source("scripts/reference/taxonomy/plant_lookup_join.R")
  df <- attach_flower_ids(df)                 # flower_taxon_id + flower_in_park from the plant lookup

  clean <- df |> select(any_of(IBC_COLUMN_ORDER))

  # hand-back worklists (review artifacts, kept OUT of the clean table):
  #   * fix_behavior    -- behavioral fields to fix; SPLIT into a survey + a non-survey file
  #   * location_review -- survey pins to re-check (location_needs_fix, see ibc_location_review)
  fix_behavior    <- ibc_fix_behavior(clean)
  drop_flag       <- function(d) d[, setdiff(names(d), "is_survey"), drop = FALSE]
  fix_survey      <- drop_flag(fix_behavior[fix_behavior$is_survey, , drop = FALSE])
  fix_nonsurvey   <- drop_flag(fix_behavior[!fix_behavior$is_survey, , drop = FALSE])
  # Land layer for the in-water check: the Point Loma peninsula polygon (City open
  # data, an authoritative coastline). Absent shapefile -> the check simply does not
  # run and the worklist behaves exactly as it did before.
  .land <- tryCatch(
    { f <- "data/spatial/shapefiles/boundaries/point_loma/point_loma_boundary.shp"
      if (file.exists(f) && requireNamespace("sf", quietly = TRUE))
        sf::st_transform(sf::st_read(f, quiet = TRUE), 4326) else NULL },
    error = function(e) NULL)
  location_review <- ibc_location_review(df, land = .land)
  if (write) {
    dir.create(dirname(out_clean), recursive = TRUE, showWarnings = FALSE)
    write.csv(clean, out_clean, row.names = FALSE, na = "")
    dir.create(dirname(IBC_FIX_SURVEY), recursive = TRUE, showWarnings = FALSE)
    write.csv(fix_survey,    IBC_FIX_SURVEY,    row.names = FALSE, na = "")
    write.csv(fix_nonsurvey, IBC_FIX_NONSURVEY, row.names = FALSE, na = "")
    dir.create(dirname(IBC_LOCATION_REVIEW), recursive = TRUE, showWarnings = FALSE)
    write.csv(location_review, IBC_LOCATION_REVIEW, row.names = FALSE, na = "")
  }
  n_walk    <- sum(df$status == "keep" & df$spatial_cat == "walk_in")
  n_bad     <- sum(df$location_needs_fix)
  n_fv      <- sum(!is.na(clean$flower_visited))
  n_missing <- sum(fix_behavior$fix_reason == "missing_all_behavior_fields")
  n_badflow <- sum(fix_behavior$fix_reason == "flower_not_a_plant_or_unresolved")
  n_noplant <- sum(fix_behavior$fix_reason == "on_flower_but_no_plant")
  n_notgen  <- sum(fix_behavior$fix_reason == "flower_not_to_genus")
  bx_kv("Bees", format(nrow(clean), big.mark = ","), " rows — ",
        format(sum(clean$is_survey), big.mark = ","), " survey / ",
        format(sum(!clean$is_survey), big.mark = ","), " other")
  bx_out(basename(out_clean))
  bx_cont("spatial: ", format(n_walk, big.mark = ","), " walk-in re-marked NOT survey · ", format(n_bad, big.mark = ","), " pins off-transect → review")
  bx_cont("annotations: ", format(n_fv, big.mark = ","), " obs with flower_visited")
  bx_cont("fix_behavior: ", format(n_noplant, big.mark = ","), " on-flower but no plant · ", format(n_missing, big.mark = ","), " missing all fields · ", format(n_badflow, big.mark = ","), " non-plant/unresolved flower · ", format(n_notgen, big.mark = ","), " flower not to genus (family-level)")
  bx_out(IBC_FIX_SURVEY); bx_out(IBC_FIX_NONSURVEY)   # the path, not just the name
  invisible(list(clean = clean, fix_behavior = fix_behavior, location_review = location_review))
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message("Sourced inat_bee_clean.R -- inat_bee_clean() writes the labeled CABR bee table (annotations + walk-in marking).")
