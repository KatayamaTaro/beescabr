# =============================================================
# specimens/specimen_clean_helpers.R
# beescabr pipeline -- CABR bee specimen cleaning (pure helpers)
# (formerly specimen_clean.R; renamed for clarity -- these are the pure transforms,
#  NOT the runnable stage. The orchestrator specimen_bee_clean.R calls them.)
#
# The testable, side-effect-free transforms behind specimen cleaning. The
# orchestrator (specimen_bee_clean.R) does the I/O (read the .xlsx, read the
# lookup/complex-map CSVs, run the review gate, write outputs) and calls these.
# Every function here is df-in / df-out (or a small decision), so the QC logic is
# unit-tested in tests/testthat/test-specimen.R.
#
# Depends on: dplyr, stringr, lubridate.
# =============================================================

suppressWarnings(suppressMessages({ library(dplyr); library(stringr); library(lubridate) }))

# .parse_specimen_date_vec(): PURE. Parse a specimen date column robustly. A cell that is a bare
# number (e.g. "44310") is an EXCEL SERIAL date that lost its formatting -- convert it via the Excel
# 1900 origin; a normal "YYYY-MM-DD" string parses as a date; blanks / "NA" -> NA. Genuinely
# unparseable text still yields NA and warns (a useful heads-up, not a crash). Serials are recognised
# only in a plausible collection-date window (~1954-2064) so a real number is never mistaken for a date.
.parse_specimen_date_vec <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | toupper(x) == "NA"] <- NA_character_
  out <- rep(as.Date(NA), length(x))
  ser <- suppressWarnings(as.numeric(x))                      # non-numeric -> NA (coercion warning muffled)
  is_serial <- !is.na(ser) & grepl("^[0-9]+(\\.0+)?$", x) & ser >= 20000 & ser <= 60000
  out[is_serial] <- as.Date(ser[is_serial], origin = "1899-12-30")   # Excel 1900 date system
  rest <- !is_serial & !is.na(x)
  out[rest] <- as_date(x[rest])                              # real dates parse; true typos -> NA + warn
  out
}

# Parse the specimen date column into date_clean + month/day/year, and turn
# empty strings into NA across character columns. Excel serials (mis-formatted date cells) are
# auto-converted so one bad cell never derails the run.
parse_specimen_dates <- function(df) {
  df |>
    mutate(
      date_clean = .parse_specimen_date_vec(date),
      month = month(date_clean),
      day   = day(date_clean),
      year  = year(date_clean),
      across(where(is.character), ~ na_if(.x, ""))
    )
}

# Standard scientific-name casing: Genus title-case, species/subspecies lower.
# Fixes a real source-data problem (ANDRENA / andrena) that broke downstream
# case-sensitive joins.
standardize_specimen_names <- function(df) {
  df |>
    mutate(
      genus      = str_to_title(genus),
      species    = str_to_lower(species),
      subspecies = str_to_lower(subspecies)
    )
}

# BEE_FAMILIES: the seven true bee families (clade Anthophila). Apoid WASPS
# (Crabronidae, Sphecidae, ...) share SUPERFAMILY Apoidea with bees, so bee-ness is
# a FAMILY test, never a superfamily one -- filtering by Apoidea would keep the
# apoid wasps too.
BEE_FAMILIES <- c("Andrenidae", "Apidae", "Colletidae", "Halictidae",
                  "Megachilidae", "Melittidae", "Stenotritidae")

# keep_bee_specimens(): PURE. Keep only rows whose `family` is one of the seven bee
# families (case-/whitespace-insensitive). Drops identified non-bees (wasp families
# Tiphiidae/Crabronidae/Vespidae/Pompilidae/Scoliidae, Diptera) AND fully-
# unidentified rows (blank family) -- specimen_bee_clean is bees only (per Brandi:
# "only include bees, not wasps or dipterans, or anything not bee"). The specimen
# record carries `family` on every ID'd row, so this is lossless for real bees.
# No `family` column -> we CANNOT guarantee a bees-only table, so STOP loudly rather
# than fail open: silently letting wasps/flies through is worse than halting, and a
# missing/renamed family column is a schema error the caller should fix. The COLUMN
# name is also matched case-insensitively so a `Family` rename is handled, not fatal.
keep_bee_specimens <- function(df) {
  fam_col <- names(df)[tolower(names(df)) == "family"][1]
  if (is.na(fam_col)) {
    stop("keep_bee_specimens: no `family` column -- cannot guarantee a bees-only table. ",
         "The specimen record carries family on every ID'd row; a missing or renamed column ",
         "is a schema error -- fix the input rather than let non-bees through.")
  }
  fam <- tolower(trimws(as.character(df[[fam_col]])))
  df[fam %in% tolower(BEE_FAMILIES), , drop = FALSE]
}

# drop_non_native_apis(): PURE. Remove the non-native honey bee (Apis mellifera + any
# subspecies). The project is NATIVE bees only, and the iNat side already excludes Apis
# via without_taxon_id in the ingest -- this keeps the specimen table consistent. Matches
# on genus so every honey-bee form goes; NA / absent genus is kept (it isn't a honey bee).
drop_non_native_apis <- function(df) {
  if (!"genus" %in% names(df)) return(df)
  gen <- tolower(trimws(as.character(df$genus)))
  df[is.na(gen) | gen != "apis", , drop = FALSE]
}

# sbc_bee_situation(): PURE. The specimen's collection situation, mirroring inat_bee_clean's
# bee_situation so the lethal and non-lethal tables line up. A specimen with a visited plant
# (method "ex. <plant>", already parsed into flower_visited) is on_flower; a "ground" method is
# on_ground; an aerial-net / in-air method is aerial (caught in flight); a blank method -> NA.
# Specimens carry no nest concept. Uses the already-set flower_visited + the raw method_or_plant.
sbc_bee_situation <- function(df) {
  n   <- nrow(df)
  fv  <- if ("flower_visited"  %in% names(df)) trimws(as.character(df$flower_visited))  else rep(NA_character_, n)
  mth <- if ("method_or_plant" %in% names(df)) tolower(trimws(as.character(df$method_or_plant))) else rep(NA_character_, n)
  on_flower <- !is.na(fv)  & fv  != ""
  has_mth   <- !is.na(mth) & mth != ""
  on_ground <- has_mth & grepl("ground", mth)
  dplyr::case_when(
    on_flower ~ "on_flower",
    on_ground ~ "on_ground",
    has_mth   ~ "aerial",     # aerial net / in air -> caught in flight, not on a substrate
    TRUE      ~ NA_character_)
}

# Fill blank order/family/subfamily/tribe from the taxonomy lookup (Holway-derived
# authority). Source values win when present; the lookup only fills blanks. ROBUST
# to a lookup that omits some of those columns (e.g. no `order`): a rank whose
# `*_lookup` column is absent after the join is simply left as-is (bugfix vs the
# old version, which referenced order_lookup unconditionally and errored).
fill_specimen_taxonomy <- function(df, tax_lookup) {
  if (!"order" %in% names(df)) df$order <- NA_character_   # bees are all Hymenoptera
  joined <- df |>
    left_join(tax_lookup, by = c("genus", "species", "subspecies"), suffix = c("", "_lookup"))
  fill_from <- function(col) {
    lk <- paste0(col, "_lookup")
    base <- na_if(joined[[col]], "")
    if (lk %in% names(joined)) coalesce(base, joined[[lk]]) else base
  }
  joined |>
    mutate(order = fill_from("order"), family = fill_from("family"),
           subfamily = fill_from("subfamily"), tribe = fill_from("tribe")) |>
    select(-ends_with("_lookup"))
}

# Build the "known names" sets used by the spell-check + new-taxon detection, from the
# taxonomy lookup + the SD County iNat checklist (second authority for valid names not
# yet in Holway). Returns list(genera, genus_species, subgenera, complexes). The coarse
# sets (subgenera, complexes) let a subgenus/complex-only ID that IS already represented
# in the lookup avoid being re-flagged as new -- complex names are held WITHOUT the
# "(Complex) " display tag (bare), the same form a specimen carries.
build_known_names <- function(tax_check, inat_species) {
  known_genera <- unique(c(tax_check$genus, inat_species$genus))
  known_genus_species <- bind_rows(
    tax_check |> filter(!is.na(species), species != "") |> distinct(genus, species),
    inat_species
  ) |> distinct(genus, species)
  .strip_cx <- function(x) trimws(sub("^\\s*\\([^)]*\\)\\s*", "", ifelse(is.na(x), "", as.character(x))))
  known_subgenera <- if ("subgenus" %in% names(tax_check))
    tax_check |> filter(!is.na(subgenus), subgenus != "") |> distinct(genus, subgenus)
  else tibble(genus = character(), subgenus = character())
  known_complexes <- if ("complex" %in% names(tax_check))
    tax_check |> mutate(complex = .strip_cx(complex)) |> filter(complex != "") |> distinct(genus, complex)
  else tibble(genus = character(), complex = character())
  # the subspecies the lookup carries, keyed by its full genus+species+subspecies:
  # a subspecies epithet alone is not an identity (several species have a "californica")
  known_subspecies <- if ("subspecies" %in% names(tax_check))
    tax_check |> filter(!is.na(subspecies), subspecies != "") |> distinct(genus, species, subspecies)
  else tibble(genus = character(), species = character(), subspecies = character())
  list(genera = known_genera, genus_species = known_genus_species,
       subgenera = known_subgenera, complexes = known_complexes,
       subspecies = known_subspecies)
}

# Spell-check + NEW-taxon detection. Flags, per specimen, the FINEST-rank determination
# that isn't yet in the lookup: (1) unknown genus, (2) known genus but unknown
# genus+species, (3) a complex-only ID (blank species) whose complex isn't a known
# complex, (4) a subgenus-only ID (blank species+complex) whose subgenus isn't known.
# The coarse cases (3,4) feed the specimen-additions loop so a subgenus/complex-only bee
# gets a taxon_id + a lookup row (was species-only before). Carries subgenus + complex on
# the output so seeding can build a lookup-shaped row. Coarse flags require a KNOWN genus
# (an unknown genus is reported as such, not as an unknown subgenus). Returns one row per
# flagged specimen. known_subgenera / known_complexes default to none (species-only mode).
compute_taxonomy_flags <- function(df, known_genera, known_genus_species,
                                   known_subgenera = NULL, known_complexes = NULL,
                                   known_subspecies = NULL) {
  .strip_cx <- function(x) trimws(sub("^\\s*\\([^)]*\\)\\s*", "", ifelse(is.na(x), "", as.character(x))))
  ksub <- if (!is.null(known_subgenera) && nrow(known_subgenera))
    tolower(paste(known_subgenera$genus, known_subgenera$subgenus)) else character(0)
  kcx  <- if (!is.null(known_complexes)  && nrow(known_complexes))
    tolower(paste(known_complexes$genus,  known_complexes$complex))  else character(0)
  kss  <- if (!is.null(known_subspecies) && nrow(known_subspecies))
    tolower(paste(known_subspecies$genus, known_subspecies$species, known_subspecies$subspecies)) else character(0)
  d  <- df |> filter(!is.na(genus), genus != "")
  sg <- if ("subgenus" %in% names(d)) ifelse(is.na(d$subgenus), "", trimws(as.character(d$subgenus))) else rep("", nrow(d))
  cx <- if ("complex"  %in% names(d)) .strip_cx(d$complex) else rep("", nrow(d))
  sp <- if ("species"  %in% names(d)) ifelse(is.na(d$species),  "", trimws(as.character(d$species)))  else rep("", nrow(d))
  ss <- if ("subspecies" %in% names(d)) ifelse(is.na(d$subspecies), "", trimws(as.character(d$subspecies))) else rep("", nrow(d))
  unk_g  <- !(d$genus %in% known_genera)
  unk_sp <- !unk_g & sp != "" & !(paste(d$genus, sp) %in% paste(known_genus_species$genus, known_genus_species$species))
  unk_cx <- !unk_g & sp == "" & cx != "" & !(tolower(paste(d$genus, cx)) %in% kcx)   # complex-only, complex not known
  unk_sg <- !unk_g & sp == "" & cx == "" & sg != "" & !(tolower(paste(d$genus, sg)) %in% ksub)  # subgenus-only, subgenus not known
  # A SUBSPECIES the lookup does not carry. Without this the row passes every check --
  # its genus and its species are both known -- so nothing is flagged, nobody is asked,
  # and the subspecies id is never captured. That is how 60 Colletes hyalinus gaudialis
  # specimens went unnoticed while their iNat id sat unused in the local cache.
  unk_ss <- !unk_g & !unk_sp & sp != "" & ss != "" &
            !(tolower(paste(d$genus, sp, ss)) %in% kss)
  keep <- unk_g | unk_sp | unk_cx | unk_sg | unk_ss
  d <- d[keep, , drop = FALSE]
  d$flag_reason <- dplyr::case_when(
    unk_g[keep]  ~ "genus not in taxonomy lookup",
    unk_sp[keep] ~ "genus+species combo not in taxonomy lookup",
    unk_cx[keep] ~ "complex not in taxonomy lookup",
    unk_sg[keep] ~ "subgenus not in taxonomy lookup",
    unk_ss[keep] ~ "subspecies not in taxonomy lookup")
  d |>
    select(any_of(c("ucsd_id", "sdnhm_id")), genus, any_of(c("subgenus", "complex")),
           any_of("species"), any_of("subspecies"), flag_reason) |>
    distinct()
}

# ------------------------------------------------------------------------------
# STANDARD review-gate input. ONE convention for every STOP / CONTINUE decision in
# the pipeline: y = pause & review now; Enter (the default) or n = continue. A few
# synonyms are accepted; only genuine garbage RE-ASKS -- a bare Enter continues, so
# a stray keystroke can't silently halt the run.
#   stop (pause & review):  y | yes | stop | fix | halt | x
#   continue (default):  <Enter> | n | no | skip | continue | c | go | ok
# Heads-up prompts that have NO decision just say "[Enter] to continue".
# ------------------------------------------------------------------------------
REVIEW_STOP_WORDS     <- c("y", "yes", "stop", "fix", "halt", "x")               # pause & fix
REVIEW_CONTINUE_WORDS <- c("", "n", "no", "skip", "continue", "c", "go", "ok")   # keep going ("" = Enter default)
.review_ask <- function(prompt_fn, lead) {
  repeat {
    # the lead is printed above now, wrapped; gluing it into the prompt is what made
    # the line run off the screen in the first place
    if (nzchar(lead)) message(lead)
    ans <- tolower(trimws(prompt_fn("     Pause to review now?  [y/N]: ")))
    if (ans %in% REVIEW_STOP_WORDS)     return("stop")       # check stop first
    if (ans %in% REVIEW_CONTINUE_WORDS) return("continue")   # "" (Enter) lands here -> default continue
    message("     (y = pause & review · Enter = continue)")
  }
}

# Decide what to do about spell-check flags. PURE (prompt injected):
#   0 flags            -> "clean"
#   flags, batch mode  -> "continue" (log & proceed; the automated pipeline)
#   flags, interactive -> STANDARD skip/stop prompt -> "continue" / "stop"
resolve_flag_gate <- function(n_flags, interactive_ok, prompt_fn = readline) {
  if (n_flags == 0) return("clean")
  if (!interactive_ok) return("continue")
  .review_ask(prompt_fn, "  Names above aren't in your taxonomy list yet (a typo to fix, or a new taxon to add).")
}

# ONE consolidated review checkpoint so nothing in the review folder gets silently
# missed. PURE (prompt injected). `items` is a data.frame(label, count, file):
#   0 issues total       -> "clean" (silent)
#   issues, batch mode    -> print the summary loudly, return "continue" (never blocks automation)
#   issues, interactive   -> heads-up (blocking = FALSE) is Enter-to-continue; a blocking
#                            checkpoint uses the STANDARD skip/stop prompt.
#' The iNaturalist review-gate labels
#'
#' "bee behavior to fix" and "bee flowers to add" were two names for one thing, and
#' neither said what was actually missing. On iNaturalist the flower lives in an
#' observation field, so if it is absent either the bee was not on a flower or
#' nobody filled that field in. Naming the field is what makes it fixable.
#'
#' @return The two labels, survey first.
.review_labels_inat <- function()
  c("flower not recorded on the observation (survey)",
    "flower not recorded on the observation (non-survey)")

.review_what_inat <- function() {
  fix <- paste(
    "HOW TO FIX: open each row's URL on iNaturalist and set both fields --",
    "\n     Insect on flower                -> Yes / No",
    "\n     Interaction->Visited flower of  -> the plant",
    "\n  No flower is a real answer: set the first to No and leave the plant blank.",
    "\n  Do the survey rows first; the non-survey ones can wait.")
  c(fix, fix)   # one shared block; the gate prints it once
}

#' What each specimen review row means, and how to fix it
#'
#' A label like "duplicate IDs" names a symptom. These say what went wrong and what
#' to change, in the order the review_items table lists them.
#'
#' @return Four strings: unknown names, duplicate IDs, missing lat/long, missing specimens.
.review_what_specimens <- function() c(
  paste("A name on the sheet is not in the taxonomy lookup. Usually a typo to correct;",
        "sometimes a real bee that is simply new, in which case it gets added on the",
        "next rebuild and needs nothing from you."),
  paste("Two rows carry the same museum number, so one specimen's records would be",
        "credited to the other. Give one of them its own number, or delete the row if",
        "it is a duplicate entry rather than a duplicate specimen."),
  paste("No coordinates, so the record cannot be placed on a transect and drops out of",
        "every map and per-transect count. Fill latitude and longitude from the",
        "collection plot."),
  paste("Marked as missing from the physical collection. Nothing to fix in the data --",
        "either find the specimen or leave the flag as the record that it is gone."))

#' Name the workbook to edit, rather than "the raw .xlsx"
#'
#' There are nineteen versions on disk. Naming the newest, and saying to save a new
#' one rather than edit it, is the difference between a fix and a lost correction.
#'
#' @param files Workbook filenames; the highest V number is taken as current.
#' @return A sentence naming the file and the rule.
.specimen_fix_hint <- function(files = NULL) {
  if (is.null(files))
    files <- list.files("data/specimens/records", pattern = "^cabr_bee_specimens_record_V[0-9]+_.*[.]xlsx$")
  if (!length(files)) return("the specimen workbook in data/specimens/records/")
  v <- suppressWarnings(as.integer(sub(".*_V([0-9]+)_.*", "\\1", files)))
  newest <- files[which.max(v)]
  paste0("data/specimens/records/", newest,
         " -- find each row by ucsd_id / sdnhm_id. Save a NEW version when done ",
         "(next number, today's date); never edit an old one.")
}

#' Wrap a review instruction so it fits a terminal
#'
#' The gate printed its explanation and its fix instruction as single unwrapped lines,
#' so both ran off the right edge and the operator could not read the end of either.
#' Continuations are indented past the first line, so a wrapped sentence still reads as
#' one block rather than as new bullets.
#'
#' @param text The sentence to wrap.
#' @param first Indent for the first line.
#' @param rest Indent for the continuations.
#' @param width Column to wrap at.
#' @return Character vector of lines.
.review_wrap <- function(text, first = "     ", rest = "       ", width = 84)
  strwrap(text, width = width, initial = first, prefix = rest)

#' The "where to fix it" instruction, wrapped
#'
#' @param fix_hint The workbook sentence from `.specimen_fix_hint()`.
#' @return Character vector of lines.
.review_fix_lines <- function(fix_hint)
  .review_wrap(paste("Review these in", sub("[.]+$", ".", fix_hint)),
               first = "     ", rest = "       ")

#' One review checkpoint, so nothing in a review folder is silently missed
#'
#' Prints each outstanding issue with a path you can open and, where given, a
#' sentence saying what it means and how to fix it. A folder name plus a bare
#' filename is not a path a newcomer can act on.
#'
#' @param items Data frame of `label`, `count`, `file`, and optionally `what` --
#'   the plain-language explanation shown under the label.
#' @param review_dir Folder the files sit in; joined to `file` for the full path.
#' @param interactive_ok FALSE prints the summary and continues, never blocking
#'   an unattended run.
#' @param prompt_fn Injection point for reading the answer.
#' @param fix_hint Where the fix is made, named in the prompt.
#' @param blocking TRUE offers to stop so the fix can happen now; FALSE is a
#'   heads-up that continues on Enter.
#' @return `"clean"` when nothing is outstanding, `"continue"`, or `"stop"`.
#' What to actually do about a record with no flower recorded
#'
#' "Add the observation field" is not actionable -- iNaturalist has thousands of
#' them. These are the two this project reads, spelled the way they appear on the
#' site, taken from the crosswalk (`bee_on_flower`, `flower_visited`).
#'
#' @return Two strings: the survey explanation, then the non-survey one.
resolve_review_gate <- function(items, review_dir, interactive_ok, prompt_fn = readline,
                                fix_hint = "the raw .xlsx", blocking = TRUE) {
  items <- items[!is.na(items$count) & items$count > 0, , drop = FALSE]
  if (!nrow(items)) return("clean")
  message("\n  ⚠ REVIEW NEEDED")
  for (i in seq_len(nrow(items))) {
    message(sprintf("     %-52s (%d)", items$label[i], items$count[i]))
    # a path you can open, not a folder plus a filename to join up yourself
    message("        ", file.path(review_dir, items$file[i]))
  }
  # Two rows often share one fix. Printing it per row turns a short instruction
  # into a wall of text, so identical explanations collapse to one block.
  if ("what" %in% names(items)) {
    for (w in unique(items$what[!is.na(items$what) & nzchar(items$what)])) {
      message("")
      for (para in strsplit(w, "\n", fixed = TRUE)[[1]])
        for (ln in .review_wrap(para)) message(ln)
    }
  }
  if (!interactive_ok) { message("     (batch mode: logged above, continuing)"); return("continue") }
  if (!blocking) {   # heads-up only -- the run never stops here; the fix happens elsewhere, later
    for (ln in .review_fix_lines(fix_hint)) message(ln)
    message("     Each row carries its url.")
    prompt_fn("     [Enter] to continue: ")
    return("continue")
  }
  # "Review", not "fix": some flags are genuine mistakes to correct, others are just new taxa
  # (a real name not in the lookup yet) that need no raw-data change -- they get added on rebuild.
  message("")
  for (ln in .review_fix_lines(fix_hint)) message(ln)
  .review_ask(prompt_fn, "")
}

# QC flags: which required-data fields are missing. genus is the one rank expected
# on every specimen; species is deliberately NOT flagged.
add_qc_flags <- function(df) {
  df |>
    mutate(
      missing_latlong  = is.na(latitude) | is.na(longitude),
      missing_date     = is.na(date),
      missing_sdnhm_id = is.na(sdnhm_id) | sdnhm_id == "",
      missing_ucsd_id  = is.na(ucsd_id) | ucsd_id == "",
      missing_genus    = is.na(genus) | genus == ""
    )
}

# Duplicate ID detection: any repeated ucsd_id (should be unique), or repeated
# sdnhm_id excluding 0/NA (0 is the intentional "needs new tag" sentinel).
detect_duplicate_ids <- function(df) {
  # A blank / NA / 0 id means "not assigned yet", not a duplicate. The sdnhm_id branch
  # already excluded those, but the ucsd_id branch did not -- so multiple un-tagged rows
  # were all false-flagged as "duplicate ucsd_id". Exclude blanks on BOTH sides.
  nz <- function(x) { x <- as.character(x); !is.na(x) & trimws(x) != "" & trimws(x) != "0" }
  df$.row <- seq_len(nrow(df))
  dup_ucsd <- df |>
    filter(nz(ucsd_id)) |>
    filter(duplicated(ucsd_id) | duplicated(ucsd_id, fromLast = TRUE)) |>
    mutate(duplicate_reason = "duplicate ucsd_id")
  dup_sdnhm <- df |>
    filter(nz(sdnhm_id)) |>
    filter(duplicated(sdnhm_id) | duplicated(sdnhm_id, fromLast = TRUE)) |>
    mutate(duplicate_reason = "duplicate sdnhm_id")
  # Dedupe on ROW identity, not ucsd_id: a row can be flagged for both reasons, and
  # keying on ucsd_id would collapse different blank-ucsd_id rows -- dropping a genuine
  # sdnhm_id duplicate from the report.
  bind_rows(dup_ucsd, dup_sdnhm) |>
    distinct(.row, .keep_all = TRUE) |>
    arrange(sdnhm_id, ucsd_id) |>
    select(-.row)
}

# Build the species-level complex match lookup from the SD County iNat checklist
# (only species rows that belong to a complex are valid targets).
build_complex_lookup <- function(checklist) {
  checklist |>
    filter(!is.na(species), species != "", !is.na(complex), complex != "") |>
    transmute(
      genus   = str_to_lower(genus),
      species = str_to_lower(species),
      complex_match          = complex,
      complex_taxon_id_match = complex_taxon_id
    ) |>
    distinct()
}

# Apply the species-level complex match, gated on BOTH genus and species present. A hit
# is prefixed "(Complex) " so a complex-level id isn't misread as a confirmed species.
# IMPORTANT: a row with NO species-level hit KEEPS a hand-entered complex rather than
# being wiped to NA -- a complex-only ID (blank species, e.g. "Colletes simulans") is a
# real determination the collector made and must survive to the output. Only the
# complex_taxon_id (from the species map) is left blank when there's no genus+species hit;
# we don't invent an id for a hand-typed complex. Existing tags are kept idempotently.
match_specimen_complex <- function(df, complex_lookup) {
  as_complex_tag <- function(x) {                    # "(Complex) X", idempotent; blank -> NA
    x <- trimws(as.character(x))
    ifelse(is.na(x) | x == "", NA_character_,
           ifelse(grepl("^\\(Complex\\)", x, ignore.case = TRUE), x, paste0("(Complex) ", x)))
  }
  df |>
    mutate(.mg = str_to_lower(genus), .ms = str_to_lower(species),
           .kept_complex = as_complex_tag(if ("complex" %in% names(df)) complex else NA_character_)) |>
    left_join(complex_lookup, by = c(".mg" = "genus", ".ms" = "species")) |>
    mutate(
      .has_gs = !is.na(genus) & genus != "" & !is.na(species) & species != "",
      # a genus+species map hit wins; otherwise fall back to the collector's own complex
      complex = coalesce(
        ifelse(.has_gs & !is.na(complex_match), paste0("(Complex) ", complex_match), NA_character_),
        .kept_complex),
      complex_taxon_id = ifelse(.has_gs, complex_taxon_id_match, NA)
    ) |>
    select(-.mg, -.ms, -.has_gs, -.kept_complex, -complex_match, -complex_taxon_id_match)
}

# Build old_scientific_name from old_genus_name + old_species_name (blank/genus-
# only/binomial cases), for advisor-facing name-change tracking.
build_old_scientific_name <- function(df) {
  df |>
    mutate(old_scientific_name = case_when(
      (is.na(old_genus_name) | old_genus_name == "") &
        (is.na(old_species_name) | old_species_name == "") ~ NA_character_,
      (is.na(old_species_name) | old_species_name == "")   ~ old_genus_name,
      TRUE                                                  ~ paste(old_genus_name, old_species_name)
    ))
}

# Defensive: strip embedded control/null bytes from character columns so the saved
# CSV re-reads cleanly.
strip_control_chars <- function(df) {
  df |> mutate(across(where(is.character), ~ str_replace_all(.x, "[\\x00-\\x1F]", "")))
}

# flag_raw_clutter(): PURE. The RAW-record hygiene worklist -- rows that clutter the
# source .xlsx and need a human decision: non-ID'd (blank genus) and physically
# missing (missing_specimen == "Y"). Returns the flagged rows + a clutter_reason.
# (Duplicates are a separate axis -- see detect_duplicate_ids.)
flag_raw_clutter <- function(df) {
  g  <- if ("genus" %in% names(df)) as.character(df$genus) else rep(NA_character_, nrow(df))
  needs_id <- is.na(g) | trimws(g) == ""
  ms <- if ("missing_specimen" %in% names(df))
    toupper(trimws(as.character(df$missing_specimen))) else rep(NA_character_, nrow(df))
  missing <- !is.na(ms) & ms == "Y"
  keep    <- needs_id | missing
  reason  <- ifelse(needs_id & missing, "needs_id; missing",
             ifelse(needs_id, "needs_id",
             ifelse(missing, "missing", NA_character_)))
  out <- df[keep, , drop = FALSE]
  out$clutter_reason <- reason[keep]
  out
}

# ------------------------------------------------------------
# TRANSECT from the specimen `plot` text, via the master_crosswalk transect rows.
# The crosswalk already carries the plot strings under specimen_label_variants
# (e.g. tp: "Cabrillo NM: Tide Pool Trail; TPT1; ..."), so the mapping is
# data-driven -- no hardcoded plot list. Plots that match no variant (plain
# "Cabrillo NM") are left NA here and resolved spatially by the orchestrator.
# ------------------------------------------------------------
# transect_variant_map(): PURE. Long (transect, variant) table from the crosswalk's
# transect rows, transect UPPER-cased (TP/UPMON/BST/OT) and variants lowercased,
# longest-variant-first so the most specific match wins.
transect_variant_map <- function(crosswalk) {
  empty <- tibble(transect = character(0), variant = character(0))
  if (is.null(crosswalk) || nrow(crosswalk) == 0 ||
      !all(c("name", "what_for", "specimen_label_variants") %in% names(crosswalk))) return(empty)
  tr <- crosswalk |>
    filter(tolower(what_for) == "transect",
           !is.na(specimen_label_variants), specimen_label_variants != "")
  if (nrow(tr) == 0) return(empty)
  rows <- lapply(seq_len(nrow(tr)), function(i) {
    vars <- tolower(trimws(unlist(strsplit(tr$specimen_label_variants[i], ";"))))
    vars <- vars[vars != ""]
    if (length(vars)) tibble(transect = toupper(trimws(tr$name[i])), variant = vars) else NULL
  })
  bind_rows(rows) |> arrange(desc(nchar(variant)))
}

# plant_variant_map(): from the crosswalk's plant rows (what_for == "plant_taxon"),
# a variant(lowercased) -> canonical-name lookup, so a raw flower label folds to the
# accepted plant name. Mirrors transect_variant_map. Lets specimen_bee_clean make
# flower_visited uniform with the plant observations + plant taxonomy lookup.
plant_variant_map <- function(crosswalk) {
  empty <- tibble(canonical = character(0), variant = character(0))
  if (is.null(crosswalk) || nrow(crosswalk) == 0 ||
      !all(c("name", "what_for", "specimen_label_variants") %in% names(crosswalk))) return(empty)
  pl <- crosswalk |>
    filter(tolower(what_for) == "plant_taxon",
           !is.na(specimen_label_variants), specimen_label_variants != "")
  if (nrow(pl) == 0) return(empty)
  rows <- lapply(seq_len(nrow(pl)), function(i) {
    vars <- tolower(trimws(unlist(strsplit(pl$specimen_label_variants[i], ";"))))
    vars <- vars[vars != ""]
    if (length(vars)) tibble(canonical = trimws(pl$name[i]), variant = vars) else NULL
  })
  bind_rows(rows)
}

# normalize_flower_name(): map each raw flower label to its canonical plant name via
# the crosswalk plant-variant map; unmapped names pass through unchanged. PURE + vectorized.
normalize_flower_name <- function(x, variant_map) {
  if (is.null(variant_map) || nrow(variant_map) == 0) return(x)
  hit <- variant_map$canonical[match(tolower(trimws(x)), variant_map$variant)]
  ifelse(is.na(hit), x, hit)
}

# match_plot_transect(): PURE. For each plot string, the transect whose (longest)
# variant is a substring of it, else NA. Vectorized.
match_plot_transect <- function(plot, variant_map) {
  if (is.null(variant_map) || nrow(variant_map) == 0) return(rep(NA_character_, length(plot)))
  p <- tolower(trimws(as.character(plot)))
  out <- rep(NA_character_, length(p))
  for (i in seq_along(p)) {
    if (is.na(p[i]) || p[i] == "") next
    for (j in seq_len(nrow(variant_map))) {
      if (grepl(variant_map$variant[j], p[i], fixed = TRUE)) { out[i] <- variant_map$transect[j]; break }
    }
  }
  out
}

# ABOVE_GENUS_RANKS: identification ranks coarser than genus, FINEST first. A
# specimen keyed only to one of these (blank genus -- e.g. ID'd to tribe Halictini)
# can't match the genus-and-below join, so its id/rank/name come from the lookup ROW
# AT THAT RANK instead. Bee specimen records only ever reach family/subfamily/tribe
# above genus, but the ladder is general.
ABOVE_GENUS_RANKS <- c("subtribe", "tribe", "subfamily", "epifamily", "family",
                       "superfamily", "infraorder", "suborder", "order",
                       "subclass", "class", "subphylum", "phylum", "kingdom")

# ---- rolling an unresolvable ID back to its parent taxon ---------------------
#
# A specimen is identified to whatever rank the determiner could reach: a subspecies,
# a species, a species complex, a subgenus, a genus, or only a tribe. Whichever it is,
# if the reference lookup has no node at that exact rank the record must still be
# placed as precisely as the taxonomy allows -- otherwise it comes out with no id AND
# no lineage, which is what happened to 60 Colletes rows.
#
# THE LADDER, finest to coarsest. This is the whole ordering, in one place: adding a
# rung is a line here, not a new branch in a function.
#
#   keyed_by_genus  below the genus, a rank value only means something WITH its genus
#                   (Dialictus is a subgenus of Lasioglossum; the name alone is not an
#                   identity). At and above the genus, the rank column stands alone.
#   strip_prefix    the record writes a complex as "(Complex) Colletes hyalinus".
TAXON_LADDER <- c(
  list(list(rank = "species",  keyed_by_genus = TRUE),
       list(rank = "complex",  keyed_by_genus = TRUE, strip_prefix = TRUE),
       list(rank = "subgenus", keyed_by_genus = TRUE),
       list(rank = "genus",    keyed_by_genus = FALSE)),
  lapply(ABOVE_GENUS_RANKS, function(r) list(rank = r, keyed_by_genus = FALSE))
)

# rollback_to_parent_taxon(): PURE. For row `i`, try each rung of `rungs` (an ordered
# subset of TAXON_LADDER) and take the FIRST that resolves to exactly one taxon in the
# lookup. Ambiguity resolves nothing -- two candidate ids means we do not know which,
# and a wrong id is worse than none. Fills taxon_id, taxon_rank, the names, and any
# ancestor rank the specimen itself left blank. Returns df unchanged if nothing hits.
rollback_to_parent_taxon <- function(df, lookup, i, rungs, lk = NULL) {
  b2na <- function(x) { x <- as.character(x); ifelse(is.na(x) | trimws(x) == "", NA_character_, trimws(x)) }
  norm <- function(x) tolower(b2na(x))
  strip <- function(x) { x <- b2na(x); tolower(trimws(sub("^\\s*\\([^)]*\\)\\s*", "", x))) }
  col  <- function(d, n) if (n %in% names(d)) d[[n]] else rep(NA_character_, nrow(d))
  if (is.null(lk)) lk <- list(rank = norm(lookup$rank),
                              id   = suppressWarnings(as.integer(lookup$taxon_id)),
                              genus = norm(col(lookup, "genus")))
  fill_cols <- intersect(intersect(names(df), names(lookup)), SPECIMEN_LOOKUP_RANKS)
  gi <- norm(col(df, "genus")[i])[1]

  for (rg in rungs) {
    r <- rg$rank
    if (!r %in% names(lookup)) next
    want <- if (isTRUE(rg$strip_prefix)) strip(col(df, r)[i])[1] else norm(col(df, r)[i])[1]
    if (r == "genus") want <- gi                       # the genus rung keys on the genus itself
    if (is.na(want)) next
    have <- if (isTRUE(rg$strip_prefix)) strip(lookup[[r]]) else norm(lookup[[r]])
    m <- lk$rank == r & have == want
    if (isTRUE(rg$keyed_by_genus)) m <- m & lk$genus == gi
    hit <- which(m & !is.na(lk$id))
    ids <- unique(lk$id[hit])
    if (length(ids) != 1L) next                        # 0 = no node, >1 = ambiguous
    j1 <- hit[1]
    df$taxon_id[i]   <- ids
    df$taxon_rank[i] <- r
    if ("scientific_name" %in% names(lookup)) df$scientific_name[i] <- as.character(lookup$scientific_name[j1])
    if ("common_name"     %in% names(lookup)) df$common_name[i]     <- as.character(lookup$common_name[j1])
    for (fc in fill_cols) if (is.na(b2na(df[[fc]][i]))) df[[fc]][i] <- as.character(lookup[[fc]][j1])
    break                                              # finest match wins
  }
  df
}

# the rungs at or below the genus, and the rungs above it
.ladder_below_genus <- function() Filter(function(x) x$rank %in% c("species","complex","subgenus","genus"), TAXON_LADDER)
.ladder_above_genus <- function() Filter(function(x) x$rank %in% ABOVE_GENUS_RANKS, TAXON_LADDER)


# fill_above_genus_ids(): PURE. For rows STILL missing taxon_id AND with no genus,
# resolve taxon_id / taxon_rank / scientific_name / common_name from the lookup's row
# at the specimen's FINEST filled above-genus rank (e.g. tribe Halictini -> 335597).
# Fills only on an UNAMBIGUOUS match (a single distinct taxon_id for that rank+name);
# ambiguous or absent -> left blank (never guess). Genus-or-finer IDs are handled by
# the genus join and are left untouched here -- a species not in the lookup stays
# blank, it is NOT coarsened up to its family.
fill_above_genus_ids <- function(df, lookup) {
  # Rows keyed ABOVE the genus (family / subfamily / tribe only). The walk itself is
  # rollback_to_parent_taxon(); this decides only WHICH rows and WHICH rungs.
  if (!"taxon_id"        %in% names(df)) df$taxon_id        <- NA_integer_
  if (!"taxon_rank"      %in% names(df)) df$taxon_rank      <- NA_character_
  if (!"scientific_name" %in% names(df)) df$scientific_name <- NA_character_
  if (!"common_name"     %in% names(df)) df$common_name     <- NA_character_
  df$taxon_id <- suppressWarnings(as.integer(df$taxon_id))
  if (is.null(lookup) || !nrow(lookup) || !"rank" %in% names(lookup)) return(df)
  g <- if ("genus" %in% names(df)) as.character(df$genus) else rep(NA_character_, nrow(df))
  for (i in which(is.na(df$taxon_id) & (is.na(g) | trimws(g) == "")))
    df <- rollback_to_parent_taxon(df, lookup, i, .ladder_above_genus())
  df
}

# rank+name (>1 distinct id) or no match at any rank leaves the row unresolved.
fill_coarse_ids <- function(df, lookup) {
  # Rows keyed AT OR BELOW the genus. Two shapes, and they walk differently:
  #
  #   keyed BELOW species (a subspecies the lookup lacks) -> may claim its own SPECIES
  #        and nothing coarser. Rolling a species-level record back to the genus would
  #        overstate what is unknown; test-specimen.R protects that rule.
  #   species BLANK (complex / subgenus / genus only) -> may walk complex -> subgenus
  #        -> genus, all honest statements of the same coarse ID.
  #
  # A row with a species and NO subspecies is left alone: the exact join already had
  # its chance, and a miss there means the NAME is wrong, not the rank.
  if (!"taxon_id"        %in% names(df)) df$taxon_id        <- NA_integer_
  if (!"taxon_rank"      %in% names(df)) df$taxon_rank      <- NA_character_
  if (!"scientific_name" %in% names(df)) df$scientific_name <- NA_character_
  if (!"common_name"     %in% names(df)) df$common_name     <- NA_character_
  df$taxon_id <- suppressWarnings(as.integer(df$taxon_id))
  if (is.null(lookup) || !nrow(lookup) || !"rank" %in% names(lookup)) return(df)

  b2na <- function(x) { x <- as.character(x); ifelse(is.na(x) | trimws(x) == "", NA_character_, trimws(x)) }
  col  <- function(d, n) if (n %in% names(d)) d[[n]] else rep(NA_character_, nrow(d))
  g <- b2na(col(df, "genus")); sp <- b2na(col(df, "species")); ss <- b2na(col(df, "subspecies"))

  species_rung <- Filter(function(x) x$rank == "species", TAXON_LADDER)
  coarse_rungs <- Filter(function(x) x$rank %in% c("complex", "subgenus", "genus"), TAXON_LADDER)

  for (i in which(is.na(df$taxon_id) & !is.na(g) & !is.na(sp) & !is.na(ss)))
    df <- rollback_to_parent_taxon(df, lookup, i, species_rung)
  for (i in which(is.na(df$taxon_id) & !is.na(g) & is.na(sp)))
    df <- rollback_to_parent_taxon(df, lookup, i, coarse_rungs)
  df
}

# ------------------------------------------------------------
# attach_lookup_taxonomy(): PURE. Give each specimen the lookup's taxon_id at the rank
# it was actually identified to. Resolution is rank-scoped, coarsest handled last:
#   1. EXACT join on (genus, species, subspecies) against the lookup's SPECIES/SUBSPECIES
#      rows -- the confident, name-based match for fully-identified specimens.
#   2. fill_coarse_ids() -- genus/subgenus/complex-only IDs (blank species) match the
#      lookup node AT THAT RANK, rolling an unknown complex back to its parent rather
#      than being fabricated onto a sibling complex.
#   3. fill_above_genus_ids() -- IDs coarser than genus (blank genus, e.g. tribe only).
# Pulls taxon_id, taxon_rank, scientific_name, common_name, and every rank name -- the
# specimen's own genus/subgenus/complex/species/subspecies win; the lookup fills the
# higher ranks and any blanks. This is what gives specimens the taxon_id the iNat
# comparison joins on. NB the step-1 join is deliberately species-only: matching a
# blank-species coarse ID here would collide with same-genus children (the old bug).
# ------------------------------------------------------------
SPECIMEN_LOOKUP_RANKS <- c("kingdom", "phylum", "subphylum", "class", "subclass", "order",
                           "suborder", "infraorder", "superfamily", "family", "epifamily",
                           "subfamily", "tribe", "subtribe", "genus", "subgenus", "complex",
                           "species", "subspecies")
#' Attach full taxonomy to specimen rows from the lookup
#'
#' @param df Specimen rows, with whatever ranks the recorder filled in.
#' @param lookup The bee taxonomy lookup -- the authority on what a taxon is.
#' @return `df` with `taxon_id`, `rank`, `scientific_name` and every ancestor
#'   rank attached. A row identified only to tribe still gets its family and
#'   order, via the rollback ladder.
attach_lookup_taxonomy <- function(df, lookup) {
  b2na <- function(x) { x <- as.character(x); ifelse(is.na(x) | x == "", NA_character_, x) }
  d <- df |> mutate(.g = b2na(genus), .s = b2na(species), .ss = b2na(subspecies))
  keep <- intersect(c("taxon_id", "rank", "scientific_name", "common_name", SPECIMEN_LOOKUP_RANKS),
                    names(lookup))
  # Exact join uses ONLY the lookup's species-bearing rows (a real species/subspecies).
  # A blank-species coarse ID (genus/subgenus/complex-only) therefore matches NOTHING
  # here -- it can't collide with, and inherit the id of, a same-genus child complex or
  # the genus node. Those coarse rows are resolved rank-aware by fill_coarse_ids() below;
  # blank-genus rows by fill_above_genus_ids(). (Before, this join included blank-species
  # rows, so a genus-only Colletes grabbed whichever Colletes complex sorted first.)
  lk <- lookup |>
    filter(!is.na(genus), genus != "", !is.na(species), species != "") |>
    mutate(.g = b2na(genus), .s = b2na(species), .ss = b2na(subspecies)) |>
    select(.g, .s, .ss, all_of(keep)) |>
    distinct(.g, .s, .ss, .keep_all = TRUE)
  j <- d |> left_join(lk, by = c(".g", ".s", ".ss"), suffix = c("", "_lk"))
  j$taxon_id        <- if ("taxon_id" %in% names(j))        j$taxon_id        else NA
  j$taxon_rank      <- if ("rank" %in% names(j))            j$rank            else NA_character_
  j$scientific_name <- if ("scientific_name" %in% names(j)) j$scientific_name else NA_character_
  j$common_name     <- if ("common_name" %in% names(j))     j$common_name     else NA_character_
  for (rc in SPECIMEN_LOOKUP_RANKS) {
    lkc <- paste0(rc, "_lk")
    if (rc %in% names(j) && lkc %in% names(j)) j[[rc]] <- coalesce(b2na(j[[rc]]), j[[lkc]])
    else if (lkc %in% names(j))                j[[rc]] <- j[[lkc]]
  }
  # Coarse below-genus fallback: genus/subgenus/complex-only IDs (blank species) that the
  # species-only exact join left unresolved -- match the lookup node at the specimen's own
  # rank (complex -> subgenus -> genus), never a finer child.
  j <- fill_coarse_ids(j, lookup)
  # Above-genus fallback: a specimen keyed only to family/subfamily/tribe (blank
  # genus) matched nothing above -- resolve its id/rank/name from the lookup's row at
  # that rank (uses the higher-rank columns just coalesced).
  j <- fill_above_genus_ids(j, lookup)
  j |> select(-.g, -.s, -.ss, -any_of("rank"), -ends_with("_lk"))
}

# resolve_determiners(): PURE. Map each specimen determination CODE ("initials + surname", e.g.
# "JL Mullins") to a determiner's iNaturalist username, via the identifier roster.
#
# The roster's `determiner_code` column IS the join key: it holds the determination string exactly as
# it is written on the specimen label, so a label matches a stored value rather than a name we
# assembled -- the same rule taxon joins follow (CLAUDE.md). A determiner who determines specimens
# needs that cell filled; a blank cell never matches anything, including a blank label.
#
# FALLBACK, for a roster row whose code is not filled in yet: surname + first initial. It is a guess,
# and it loses multi-word surnames ("D de Pedro" reads its surname as "Pedro"), which is exactly why
# the code column exists. Fill the code and the fallback stops mattering.
#
# Returns a data frame (code, determiner, status): status is "matched", "unknown" (no roster row),
# "ambiguous" (>1 roster row claims it -- a human must resolve), or NA (blank code = no determination
# recorded). READ-ONLY: never writes the roster. `roster` needs columns first_name, last_name,
# inaturalist_username, and ideally determiner_code.
resolve_determiners <- function(codes, roster) {
  low  <- function(x) tolower(trimws(as.character(x)))
  code     <- trimws(as.character(codes))
  surname  <- sub(".*\\s", "", code)                 # last whitespace-delimited token
  first_in <- substr(sub("\\s.*", "", code), 1, 1)   # first char of the leading initial(s)
  r_last <- low(roster$last_name)
  r_fi   <- substr(low(roster$first_name), 1, 1)
  r_user <- as.character(roster$inaturalist_username)
  r_code <- if ("determiner_code" %in% names(roster)) low(roster$determiner_code) else character(nrow(roster))
  r_code[is.na(r_code)] <- ""                         # an unfilled cell is not a key: it matches nothing
  out <- data.frame(code = code, determiner = NA_character_, status = NA_character_,
                    stringsAsFactors = FALSE)
  for (i in seq_along(code)) {
    if (is.na(code[i]) || !nzchar(code[i])) next      # blank -> no determination; status stays NA
    hit <- which(nzchar(r_code) & r_code == low(code[i]))                  # the stored key wins
    if (!length(hit)) hit <- which(r_last == low(surname[i]) & r_fi == low(first_in[i]))
    if (length(hit) == 1L) { out$determiner[i] <- r_user[hit]; out$status[i] <- "matched" }
    else if (length(hit) > 1L) out$status[i] <- "ambiguous"
    else out$status[i] <- "unknown"
  }
  out
}
