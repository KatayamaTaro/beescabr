# =============================================================
# project_info/collect_plant_names.R
# beescabr -- collect CABR plant names + review the unknown ones
# Created: 2026-07-21
#
# After the API ingest (plant observations) and the specimen ingest (bee
# flower labels), this gathers EVERY plant name seen in CABR and checks each
# against master_crosswalk_manual.csv's plant rows (what_for == "plant_taxon"):
#
#   * a name the crosswalk already knows (a canonical or one of its label
#     variants) -> recognized, nothing to do.
#   * a name it does NOT know -> UNKNOWN. You decide where it belongs, exactly
#     like the tag/field reviewer: file it as a spelling variant of an existing
#     canonical, or add it as a brand-new canonical plant.
#
# Your decision is written back into master_crosswalk_manual.csv, so it's remembered
# and the plant lookup + specimen_bee_clean pick up the canonical name on the
# next run. Non-interactive runs just drop a worklist and move on.
#
# Feels like qc_review_mastercrosswalk.R (it reuses cw_append / cw_get_or_add):
#   <Enter>   accept the * suggestion (file under it as a variant)
#   <number>  file under that canonical (a spelling variant)
#   a         ADD as a brand-new canonical plant (the name itself)
#   s         skip     q  save & quit
#
# Run: source("scripts/project_info/collect_plant_names.R"); review_plant_names()
# =============================================================

suppressWarnings(suppressMessages({library(dplyr); library(readr); library(stringr)}))

if (!exists("bx_kv") && file.exists("scripts/utils/console.R")) source("scripts/utils/console.R")

local({
  sdir <- "scripts"
  for (cand in c("scripts", "../scripts", "../../scripts", "../../../scripts"))
    if (dir.exists(cand)) { sdir <- cand; break }
  need <- function(sym, file) if (!exists(sym)) source(file.path(sdir, file))
  need("PATHS",             "config.R")
  need("plant_variant_map", "specimens/specimen_clean_helpers.R")
  need("cw_append",         "project_info/review/qc_review_mastercrosswalk.R")
})

CPN_CW       <- "data/project_info/crosswalk/master_crosswalk_manual.csv"
CPN_WORKLIST <- "data/project_info/crosswalk/review/qc_review_mastercrosswalk_plant_names.csv"

pcn_norm <- function(x) tolower(gsub("\\s+", " ", trimws(as.character(x))))

# ------------------------------------------------------------
# collect_plant_names(): distinct plant names seen in CABR, tagged by source.
#   specimen -> flower_visited_raw (the intern's label, verbatim)
#   observation -> plant-obs scientific_name (already canonical from iNat)
# PURE-ish (only reads the two clean files).
# ------------------------------------------------------------
collect_plant_names <- function(specimen_clean_path = PATHS$specimen_clean,
                                plant_clean_path    = PATHS$inat_plant_clean) {
  out <- tibble(name = character(), source = character())
  if (!is.null(specimen_clean_path) && file.exists(specimen_clean_path)) {
    s <- suppressMessages(read_csv(specimen_clean_path, show_col_types = FALSE, col_types = cols(.default = "c")))
    lab <- if ("flower_visited_raw" %in% names(s)) s$flower_visited_raw
           else if ("flower_visited" %in% names(s)) s$flower_visited else character(0)
    lab <- unique(trimws(lab)); lab <- lab[!is.na(lab) & lab != "" & tolower(lab) != "na"]
    if (length(lab)) out <- bind_rows(out, tibble(name = lab, source = "specimen"))
  }
  if (!is.null(plant_clean_path) && file.exists(plant_clean_path)) {
    p <- suppressMessages(read_csv(plant_clean_path, show_col_types = FALSE, col_types = cols(.default = "c")))
    if ("scientific_name" %in% names(p)) {
      sn <- unique(trimws(p$scientific_name)); sn <- sn[!is.na(sn) & sn != ""]
      if (length(sn)) out <- bind_rows(out, tibble(name = sn, source = "observation"))
    }
  }
  if (!nrow(out)) return(out)
  out |> group_by(name) |>
    summarise(source = paste(sort(unique(source)), collapse = "+"), .groups = "drop") |>
    arrange(name)
}

# canonical plant names currently in the crosswalk (what_for == "plant_taxon")
plant_canonicals <- function(crosswalk) {
  if (is.null(crosswalk) || !all(c("name", "what_for") %in% names(crosswalk))) return(character(0))
  keep <- !is.na(crosswalk$what_for) & tolower(crosswalk$what_for) == "plant_taxon" &
          !is.na(crosswalk$name) & crosswalk$name != ""
  unique(crosswalk$name[keep])
}

# collected names the crosswalk does NOT recognize. iNat plant-obs names are
# already canonical, so they DEFINE what's recognized and are never themselves
# "unknown" -- only specimen labels (the messy source) can need a decision.
unknown_plant_names <- function(collected, crosswalk) {
  if (is.null(collected) || nrow(collected) == 0) return(collected)
  vm <- plant_variant_map(crosswalk)
  known <- unique(c(if (nrow(vm)) vm$variant else character(0),
                    pcn_norm(plant_canonicals(crosswalk))))
  if ("source" %in% names(collected)) {
    known <- unique(c(known, pcn_norm(collected$name[grepl("observation", collected$source)])))
    cand  <- collected[grepl("specimen", collected$source), , drop = FALSE]
  } else cand <- collected
  cand[!(pcn_norm(cand$name) %in% known), , drop = FALSE]
}

# closest existing canonical by edit distance; suggested only if it's a near-miss
# (a likely typo), never an exact 0 (that would already be "known").
plant_suggest <- function(name, canonicals, max_dist = 2L) {
  if (!length(canonicals)) return(NA_character_)
  d <- as.integer(adist(pcn_norm(name), pcn_norm(canonicals)))
  b <- which.min(d)
  if (length(b) && !is.na(d[b]) && d[b] > 0 && d[b] <= max_dist) canonicals[b] else NA_character_
}

# top-N closest canonicals (for the pick list)
plant_closest <- function(name, canonicals, n = 6L) {
  if (!length(canonicals)) return(character(0))
  d <- as.integer(adist(pcn_norm(name), pcn_norm(canonicals)))
  canonicals[order(d)][seq_len(min(n, length(canonicals)))]
}

#' How alike two plant names are, as a percentage
#'
#' The option list offered a "nearest" name with no idea how near, so a plant sharing
#' nothing with the one being reviewed sat in the list looking like a candidate --
#' "Salvia apiana" was offered "1=Isocoma menziesii". A tired operator typing 1 files
#' a sage under a goldenbush. The figure is what keeps a bad match visibly bad.
#'
#' @param a,b Names to compare.
#' @return 0-100, where 100 is identical after normalization.
plant_similarity <- function(a, b) {
  x <- pcn_norm(a); y <- pcn_norm(b)
  w <- pmax(nchar(x), nchar(y))
  ifelse(w == 0, 0L, as.integer(round(100 * (1 - as.integer(adist(x, y)) / w))))
}

.cpn_write_worklist <- function(unk, crosswalk, path = CPN_WORKLIST) {
  canon <- plant_canonicals(crosswalk)
  wl <- unk |>
    rowwise() |>
    mutate(nearest_canonical = { s <- plant_suggest(name, canon, max_dist = 3L); if (is.na(s)) "" else s }) |>
    ungroup()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(wl, path, row.names = FALSE, na = "")
  path
}

# add a brand-new canonical plant row (what_for = plant_taxon, applies_to = plant)
cw_add_plant_canonical <- function(cw, canonical, variant) {
  g <- cw_get_or_add(cw, canonical, what_for = "plant_taxon"); cw <- g$cw
  if ("applies_to_plant_bee_or_both" %in% names(cw)) cw$applies_to_plant_bee_or_both[g$i] <- "plant"
  cw_append(cw, g$i, "specimen_label_variants", variant)
}

# On-demand ("?") help -- plain-language key legend (same control keys as the other reviewers).
.cpn_help <- function() {
  cat("\n  WHAT THIS IS\n")
  cat("    Plant names arrive two ways: typed on a specimen label, and picked on\n")
  cat("    iNaturalist. The same plant is often spelled differently in the two, and\n")
  cat("    unless they are tied together the bee-plant records split in half. This is\n")
  cat("    where you say which names mean the same plant.\n\n")
  cat("    Your answers are saved in ", CPN_CW, "\n\n", sep = "")
  cat("  WHAT TO TYPE\n")
  cat("   <Enter>   yes, it is the one shown as the guess -- file this spelling under it.\n")
  cat("             When no guess is offered, Enter does nothing; use a or s.\n")
  cat("   <number>  a different plant from the list -- file it under that one\n")
  cat("   a         none of those. It is a plant we have not recorded before.\n")
  cat("   s         not sure -- you are asked again next run\n")
  cat("   q         save what you have answered and stop      ?  show this again\n\n")
}

#' What to type when this file is sourced by hand
#'
#' "Sourced collect_plant_names.R -- run: review_plant_names()" named the file and the
#' function and said nothing about what either does, so there was no reason to type it.
#' @return Lines to print.
.cpn_sourced_hint <- function() c(
  "Run review_plant_names() to sort out plant names that are spelled more than one",
  "way -- so a plant written on a specimen label and the same plant picked on",
  "iNaturalist count as one plant and not two.")

# ------------------------------------------------------------
# review_plant_names(): the interactive loop (mirrors review_unknowns).
# ------------------------------------------------------------
#' Walk a human through the plant names the pipeline could not resolve
#'
#' @param cw_path The plant-name crosswalk, which this updates.
#' @param specimen_clean_path The cleaned specimen table.
#' @param plant_clean_path The cleaned plant table.
#' @param prompt_fn Injection point for reading an answer; tests pass a fake.
#' @param write Save the answers.
#' @param interactive_ok Whether prompting is allowed at all.
#' @param worklist_path Where the outstanding names are listed.
#' @return Invisibly, how many names were resolved.
review_plant_names <- function(cw_path = CPN_CW,
                               specimen_clean_path = PATHS$specimen_clean,
                               plant_clean_path    = PATHS$inat_plant_clean,
                               prompt_fn = readline, write = TRUE,
                               interactive_ok = interactive() && Sys.getenv("BEESCABR_NONINTERACTIVE", "0") != "1",
                               worklist_path = CPN_WORKLIST) {
  cw <- read_csv(cw_path, show_col_types = FALSE, col_types = cols(.default = "c"))
  cw <- cw[!(is.na(cw$name) & is.na(cw$what_for)), ]

  collected <- collect_plant_names(specimen_clean_path, plant_clean_path)
  unk <- unknown_plant_names(collected, cw)
  if (!nrow(unk)) { bx_kv("Plant names", "all ", nrow(collected), " recognized — nothing to review"); return(invisible(cw)) }

  if (!interactive_ok) {
    p <- .cpn_write_worklist(unk, cw, worklist_path)
    bx_kv("Plant names", nrow(unk), " unknown name(s) to review")
    bx_out(p)
    return(invisible(cw))
  }

  bx_kv("Plant names", nrow(unk), " name",
        if (nrow(unk) == 1L) "" else "s", " nobody has sorted yet")
  cat("\n  These plant names turned up on specimen labels or iNaturalist records and do\n")
  cat("  not match any plant already on file. For each one: is it another spelling of\n")
  cat("  a plant we have, or a plant we have not recorded before?\n")
  cat("  Getting it wrong splits one plant's bee records in two, so 's' (not sure) is\n")
  cat("  always safe -- you are asked again next run.\n")
  cat("  Answers are saved in ", CPN_CW, "\n", sep = "")
  cat("  Type ? at any point for the full explanation.\n")
  changed <- FALSE
  for (k in seq_len(nrow(unk))) {
    it <- unk$name[k]; src <- unk$source[k]
    canon <- plant_canonicals(cw)
    near  <- plant_closest(it, canon, n = 6L)
    sugg  <- plant_suggest(it, canon)
    cat(sprintf("\n[%d/%d] unknown plant: \"%s\"  (%s)\n", k, nrow(unk), it, src))
    if (!is.na(sugg))
      cat(sprintf("  Looks like a misspelling of  %s  -- press Enter if it is.\n", sugg))
    if (length(near)) {
      cat("  Plants we already have, closest first:\n")
      for (j in seq_along(near))
        cat(sprintf("    %d  %-38s %3d%% alike%s\n", j, near[j],
                    plant_similarity(it, near[j]),
                    if (!is.na(sugg) && near[j] == sugg) "  <- the guess above" else ""))
    }
    if (!is.na(sugg)) cat("    Enter=yes, the guess above    number=one of the others\n")
    else              cat("    number=one of the list above   (no guess, so Enter does nothing)\n")
    cat("    a=a plant we have not recorded    s=not sure    q=save and stop    ?=help\n")
    repeat { ans <- trimws(prompt_fn("> ")); low <- tolower(ans); if (low != "?") break; .cpn_help() }
    if (low == "q") break
    if (low == "s") next
    if (low == "a") { cw <- cw_add_plant_canonical(cw, it, it); changed <- TRUE; next }
    if (ans == "") {
      if (!is.na(sugg)) { cw <- cw_append(cw, which(cw$name == sugg)[1], "specimen_label_variants", it); changed <- TRUE }
      else cat("  Nothing on the list is close enough to guess at, so Enter does nothing here.\n",
               "  Type a to add it as a new plant, or s if you are not sure.\n", sep = "")
      next
    }
    num <- suppressWarnings(as.integer(ans))
    if (!is.na(num) && num >= 1 && num <= length(near)) {
      cw <- cw_append(cw, which(cw$name == near[num])[1], "specimen_label_variants", it); changed <- TRUE
    } else cat("  That is not one of the choices (a number from the list, a, s or q),\n",
               "  so nothing was recorded -- you will be asked about it again next run.\n", sep = "")
  }

  if (changed && write) {
    write.csv(cw, cw_path, row.names = FALSE, na = "")
    cat("\nSaved your answers -> ", cw_path, "\n", sep = "")
  } else if (!changed) {
    cat("\nNothing to save -- no name was filed.\n")
  } else {
    cat("\nNothing was saved: this run was asked not to write to the file.\n")
  }
  invisible(cw)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  for (ln in .cpn_sourced_hint()) message(ln)
