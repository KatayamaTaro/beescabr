# =============================================================
# analysis/coverage/transect_effort.R
# Per-transect sampling effort  (companion to rarefaction_{vegan,inext}.R)
# beescabr / Cabrillo National Monument (CABR) native bees
#
# WHAT THIS IS:
#   Sampling-effort bar charts by transect -- how many bee records each transect
#   has produced, split by method (lethal netting vs non-lethal photos) and as a
#   plain total. Specimen coordinates are transect centroids, so effort is
#   summarised BY TRANSECT (their reliable spatial unit): all-records for the
#   report, the fair survey window for the journal.
#
#   (A fine iNaturalist grid / whole-park spatial richness map lived here before;
#   it was retired -- the basemap-less, UTM-axis cell maps were unreadable, and the
#   effort-standardized richness story is told by rarefaction_{vegan,inext}.R by
#   transect. The dead grid + map code was removed rather than left commented.)
#
# Run from the repo root:  source("scripts/analysis/coverage/transect_effort.R")
# Depends on: dplyr, stringr, ggplot2 (+ config.R).
# =============================================================

suppressPackageStartupMessages({
  library(dplyr); library(stringr); library(ggplot2)
})

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
OUT_JOURNAL   <- file.path(DIR_JOURNAL, "richness/diversity/fair_method_2021_2023")           # fair-window per-transect effort
COV_EFFORT    <- file.path(DIR_REPORT,  "coverage/transect_effort")     # sampling effort is a COVERAGE concept, not diversity
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")   # named transects; OT = off-transect

#' What this figure's scope actually is
#'
#' It said "all records". It is not: `filter(transect %in% TRANSECTS)` drops every
#' record with no transect on it -- 4,106 of 12,471, almost all of them public
#' observations rather than project surveys. Dropping them is right (a visitor's
#' photograph is not sampling effort, however close to a route they stood), but a
#' caption claiming a scope the figure does not have is how the figure's 8,365 and
#' Table 2's 12,471 could only be reconciled by email.
#'
#' @param kept Records the figure shows.
#' @param total Records in the cleaned tables.
#' @return The scope string for `scope_cap()`.
.te_scope <- function(kept, total) {
  left <- total - kept
  paste0("records with a transect recorded, by transect (OT = off-transect)",
         if (left > 0) paste0("; the park's other ", format(left, big.mark = ","),
                              " bee records are public observations, not surveys, ",
                              "and carry no transect") else "")
}
dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE)
dir.create(COV_EFFORT,  recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
# scope_cap(): shared helper from theme_beescabr.R (adds Source + data-as-of, one canonical order).

spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

base_theme <- theme_beescabr(11) +
  theme(axis.text = element_text(size = 7, colour = BEE_INK$muted))

# ---- 1. per-transect richness (BOTH methods) -- specimens' reliable unit ------
# Specimen coordinates are transect centroids, so specimens are summarised BY
# TRANSECT. Both methods pooled, all-records; the named transects plus OT
# (off-transect) as its own bar -- see TRANSECTS.
tr_key <- function(df, method) df %>% transmute(
  method = method, transect = toupper(str_squish(transect)),
  surveyor  = str_squish(tolower(as.character(surveyor_type))),
  is_survey = is_true(is_survey),
  month = suppressWarnings(as.integer(substr(observed_on, 6, 7))),
  year  = suppressWarnings(as.integer(substr(observed_on, 1, 4))),
  species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                         !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
  genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
recs2 <- bind_rows(tr_key(spec, "lethal"), tr_key(inat, "nonlethal")) %>% filter(transect %in% TRANSECTS)

summ <- function(dat) dat %>% group_by(transect) %>%
  summarise(n_records         = n(),
            genus_richness    = n_distinct(genus_key[!is.na(genus_key)]),
            species_richness  = n_distinct(species_key[!is.na(species_key)]),
            records_lethal    = sum(method == "lethal"),
            records_nonlethal = sum(method == "nonlethal"),
            .groups = "drop") %>%
  arrange(desc(species_richness))

tr_tbl <- summ(recs2)                                    # REPORT: all records, all transects
# JOURNAL fair window: survey-only, Mar-Oct 2021-2023, lethal = intern nets (all specimens),
# non-lethal = beeple photos. OT drops out naturally (no 2021-2023 surveys).
tr_tbl_fair <- summ(recs2 %>% filter(is_survey, month %in% FAIR_MONTHS, year %in% FAIR_YEARS,
                                     method == "lethal" | (method == "nonlethal" & surveyor == "beeple")))
write.csv(tr_tbl,      file.path(COV_EFFORT,  "survey_effort_by_transect_richness.csv"),       row.names = FALSE)
write.csv(tr_tbl_fair, file.path(OUT_JOURNAL, "transect_effort_journal.csv"), row.names = FALSE)
message("Per-transect richness (report, all records): ",
        paste(sprintf("%s=%dsp", tr_tbl$transect, tr_tbl$species_richness), collapse = "  "))
lab_col <- BEE_INK$secondary

# NOTE: a raw observed-richness-per-transect bar chart is deliberately NOT drawn -- comparing raw
# richness across transects with unequal effort is biased. The effort-standardized version lives in
# rarefaction_{vegan,inext}.R -> by_transect. The per-transect counts are kept in survey_effort_by_transect_richness.csv.

# ---- 2. transect_effort -- records per transect, lethal vs non-lethal ---------
effort_chart <- function(tbl, file, scope_lab) {
  tbl$transect <- factor(tbl$transect, levels = as.character(tbl$transect[order(-tbl$n_records)]))
  eff_long <- bind_rows(
    data.frame(transect = tbl$transect, method = "non-lethal", value = tbl$records_nonlethal),
    data.frame(transect = tbl$transect, method = "lethal",     value = tbl$records_lethal))
  eff_long$method <- factor(eff_long$method, levels = c("non-lethal", "lethal"))
  g <- ggplot(eff_long, aes(transect, value, fill = method)) +
    geom_col(width = 0.66) +
    geom_text(aes(label = ifelse(value > 0, value, "")), position = position_stack(vjust = 0.5),   # per-method count inside its segment
              colour = "white", fontface = "bold", size = 2.9, show.legend = FALSE) +
    geom_text(data = tbl, aes(transect, n_records, label = n_records), vjust = -0.35, size = 3, colour = lab_col, inherit.aes = FALSE) +
    scale_fill_manual(values = setNames(unname(BEE_METHOD_COL[c("nonlethal", "lethal")]),
                                        c("non-lethal", "lethal")), name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
    labs(title = "Is survey effort even across transects?",
         subtitle = "TP carries the most records (surveyed as two transects); OT the fewest -- effort is uneven across transects.",
         caption = str_wrap(scope_lab, 74),
         x = "transect", y = "records") +
    base_theme + theme(legend.position = "top", plot.subtitle = element_text(size = 8.5),
                       plot.title = element_text(hjust = 0.5))
  bee_ggsave(file, g, width = 6.4, height = 5, bg = "white")
}
effort_chart(tr_tbl, file.path(COV_EFFORT, "survey_effort_by_transect.png"),
             scope_cap(.te_scope(nrow(recs2), nrow(spec) + nrow(inat)),
                       "lethal vs non-lethal", "records", n = nrow(recs2)))
effort_chart(tr_tbl_fair, file.path(OUT_JOURNAL, "transect_effort_journal.png"),
             scope_cap(scope  = "fair window: survey-only, Mar-Oct 2021-2023 (OT excluded -- added 2024)",
                       method = "lethal vs non-lethal",
                       rank   = "records (by transect)"))

# ---- 3. transect_effort TOTAL -- companion slide: total records per transect, colored by TRANSECT,
# no lethal/non-lethal split (same transect order as the split version, so the two slides line up). ----
effort_total_chart <- function(tbl, file, scope_lab) {
  tbl$transect <- factor(tbl$transect, levels = as.character(tbl$transect[order(-tbl$n_records)]))
  g <- ggplot(tbl, aes(transect, n_records, fill = transect)) +
    geom_col(width = 0.66) +
    geom_text(aes(label = n_records), vjust = -0.35, size = 3, colour = lab_col) +
    scale_fill_manual(values = BEE_TRANSECT, name = NULL) +   # transect legend occupies the same top space as the split slide's method legend, so the two slides line up
    scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
    labs(title = "Is survey effort even across transects?",
         subtitle = "TP carries the most records (surveyed as two transects); OT the fewest -- effort is uneven across transects.",
         caption = str_wrap(scope_lab, 74),
         x = "transect", y = "records") +
    base_theme + theme(legend.position = "top", plot.subtitle = element_text(size = 8.5),
                       plot.title = element_text(hjust = 0.5))
  bee_ggsave(file, g, width = 6.4, height = 5, bg = "white")
}
effort_total_chart(tr_tbl, file.path(COV_EFFORT, "survey_effort_by_transect_total.png"),
                   scope_cap(.te_scope(nrow(recs2), nrow(spec) + nrow(inat)),
                             "lethal + non-lethal pooled", "records", n = nrow(recs2)))
effort_total_chart(tr_tbl_fair, file.path(OUT_JOURNAL, "transect_effort_total_journal.png"),
                   scope_cap(scope  = "fair window: survey-only, Mar-Oct 2021-2023 (OT excluded -- added 2024)",
                             method = "lethal + non-lethal pooled", rank = "records (by transect)"))

message("Wrote survey_effort_by_transect.png + survey_effort_by_transect_richness.csv to ", COV_EFFORT,
        " | transect_effort_journal.png/.csv to ", OUT_JOURNAL)
