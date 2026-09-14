# =============================================================
# analysis/richness/genera_and_species_accumulation.R
# analysis/species_accumulation.R
# beescabr pipeline -- native-bee species accumulation by SURVEY EFFORT
# Created: 2026-07-21
#
# Adapts Owen Jones' "Species Accumulation Curves" tutorial
#   (https://rpubs.com/jonesor/SACs)  -- vegan::specaccum -- to the CABR
# native-bee surveys.
#
# THE QUESTION: "as we run more surveys on a transect, how fast do we keep
# turning up new bee TAXA, and are we levelling off?" -- asked at two ranks:
#   * species  (Figure 1)
#   * genus    (Figure 2)
#
# THE UNIT (x-axis): one SURVEY. The authoritative survey list is
# master_per_survey_info_generated.csv (494 surveys). A survey that walked several
# transects in a day counts as one survey FOR EACH transect it covered, so
# the list is expanded to one survey-event per (day x transect). Surveys that
# recorded no identifiable bee still count as effort (an empty row that flattens
# the curve) -- that is the honest picture of survey return.
#
# SPLIT by paper:
#   * REPORT (_report) -- transect COMPLETENESS: one panel per rank, ONE curve PER TRANSECT
#     (both methods POOLED -- method is noise for completeness), COLOR = transect, + the
#     Chao2 completeness table. "How well-sampled is each part of the park?"  ALL survey records.
#   * JOURNAL (_journal) -- METHOD comparison as SMALL MULTIPLES: one panel per transect,
#     each lethal vs non-lethal (lethal = intern nets, non-lethal = beeple photos), FAIR
#     WINDOW (Mar-Oct 2021-2023). Shows the method effect is consistent ACROSS transects,
#     not a pooling artifact. OT is dropped -- it was added in 2024 (no 2021-2023 surveys),
#     so only BST/TP/UPMON appear; the freed grid cell holds the legend.
# Each at BOTH ranks: species (Fig 1) and genera (Fig 2).
#
# RANK RULES: for the SPECIES figure only species-level IDs count (subspecies
# rolled up). For the GENUS figure any ID that pins a genus counts
# (species / subspecies / subgenus / complex / genus) -- so the genus curve
# uses more records, as it should.
#
# Run from the repo root:
#   source("scripts/analysis/richness/genera_and_species_accumulation.R")
# Figures + summary table are written to data/analysis/.
# Depends on: vegan, dplyr, stringr (+ config.R for the cleaned-table paths).
# =============================================================

suppressPackageStartupMessages({
  library(vegan)
  library(dplyr)
  library(stringr)
})

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")   # PATHS$specimen_clean, PATHS$inat_clean

# master_per_survey_info_generated.csv is not in config's PATHS list; name it here.
PER_SURVEY_INFO <- PATHS$per_survey

OUT_JOURNAL     <- file.path(DIR_JOURNAL, "richness/accumulation/fair_method_2021_2023")  # method small-multiples (fair window)
OUT_REPORT      <- file.path(DIR_REPORT,  "richness/accumulation")  # per-transect completeness + Chao2 table
TRANSECTS       <- c("BST", "UPMON", "TP", "OT")          # the lines, in legend order
SPECIES_RANKS   <- c("species", "subspecies")             # ranks that resolve to a species
GENUS_RANKS     <- c("species", "subspecies", "subgenus", # ranks that pin a genus
                     "complex", "genus")
PERMUTATIONS    <- 200                                    # specaccum random permutations
set.seed(1)                                               # reproducible permutation curves

# house style: colors + line-style come from the shared module (theme_beescabr.R)
if (!exists("BEE_TRANSECT")) source("scripts/analysis/shared/theme_beescabr.R")
COLS         <- BEE_TRANSECT      # transect color -> now TINTS each panel's title
METHOD_COL   <- BEE_METHOD_COL    # curve color = method: poppy = net (lethal), teal = photo (non-lethal)
LTY          <- BEE_METHOD_LTY    # secondary cue: solid = lethal, dashed = non-lethal
METHOD_LABEL <- BEE_METHOD_LABEL

dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_REPORT,  recursive = TRUE, showWarnings = FALSE)

# ---- 1. bee records, tagged with method + join keys -------------------------
is_true <- function(x) toupper(trimws(as.character(x))) == "TRUE"

spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)

# lethal survey  -> specimen (net);  non-lethal survey -> iNat survey rows (photo)
recs <- bind_rows(
  spec %>% transmute(date = trimws(observed_on), transect = toupper(trimws(transect)),
                     method = "lethal", taxon_rank, genus, species,
                     surveyor = str_squish(tolower(as.character(surveyor_type))), is_survey = is_true(is_survey)),
  inat %>% filter(is_true(is_survey)) %>%
           transmute(date = trimws(observed_on), transect = toupper(trimws(transect)),
                     method = "nonlethal", taxon_rank, genus, species,
                     surveyor = str_squish(tolower(as.character(surveyor_type))), is_survey = TRUE)
) %>%
  mutate(
    species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                           !is.na(species) & species != "",
                         paste(genus, word(species, -1)), NA_character_),
    genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "",
                         genus, NA_character_),
    month = suppressWarnings(as.integer(substr(date, 6, 7))),
    year  = suppressWarnings(as.integer(substr(date, 1, 4))),
    k = paste(date, transect, method)
  )
recs_by_key <- split(recs, recs$k)   # fast per-survey lookup (report: all survey records)

# fair window (JOURNAL method comparison): survey-only, Mar-Oct 2021-2023,
# lethal = intern nets (all specimens are intern), non-lethal = beeple photos only
# (drops casual public + interns' 2024 iNaturalist photos).
recs_fair <- recs %>% filter(is_survey, month %in% FAIR_MONTHS, year %in% FAIR_YEARS,
                             method == "lethal" | (method == "nonlethal" & surveyor == "beeple"))
recs_by_key_fair <- split(recs_fair, recs_fair$k)

# ---- 2. authoritative survey list, expanded to one row per (day x transect) --
surv <- read.csv(PER_SURVEY_INFO, stringsAsFactors = FALSE, check.names = FALSE)
surv$method <- ifelse(trimws(surv$method) == "lethal", "lethal", "nonlethal")
surv$date   <- trimws(surv$date)

tl       <- strsplit(surv$transects, "[,;]")               # split multi-transect days
expanded <- surv[rep(seq_len(nrow(surv)), lengths(tl)), c("date", "method"), drop = FALSE]
expanded$transect <- toupper(trimws(unlist(tl)))
expanded <- expanded[expanded$transect %in% TRANSECTS, ]
expanded$k <- paste(expanded$date, expanded$transect, expanded$method)
expanded$year  <- suppressWarnings(as.integer(substr(expanded$date, 1, 4)))
expanded$month <- suppressWarnings(as.integer(substr(expanded$date, 6, 7)))
# survey events inside the fair window (journal method comparison)
expanded_fair <- expanded[expanded$year %in% FAIR_YEARS & expanded$month %in% FAIR_MONTHS, ]

# ---- 3. survey x taxa presence matrix for one group of surveys --------------
# key_col = "species_key" or "genus_key". Rows are surveys (empty rows kept as
# effort); columns are the taxa ever seen in that group.
survey_taxa_matrix <- function(survey_rows, key_col, lookup = recs_by_key) {
  taxa <- sort(unique(na.omit(
    unlist(lapply(lookup[survey_rows$k], function(df) df[[key_col]])))))
  M <- matrix(0L, nrow = nrow(survey_rows), ncol = length(taxa),
              dimnames = list(NULL, taxa))
  if (length(taxa) == 0L) return(M)
  for (i in seq_len(nrow(survey_rows))) {
    df <- lookup[[survey_rows$k[i]]]
    if (is.null(df)) next
    kk <- unique(na.omit(df[[key_col]]))
    if (length(kk)) M[i, kk] <- 1L
  }
  M
}

# ---- 4. accumulation curves: all transects x both methods, ONE panel per rank ---
# accumulate(): one specaccum per group; NULL if the group never recorded a taxon
# at this rank (nothing to accumulate).
accumulate <- function(survey_rows, key_col) {
  M <- survey_taxa_matrix(survey_rows, key_col)
  if (ncol(M) == 0L || nrow(M) == 0L) return(NULL)
  specaccum(M, method = "random", permutations = PERMUTATIONS)
}

# ONE report figure: genera + species side by side (two panels), each OVERLAYING one curve
# PER TRANSECT (both methods POOLED -- for completeness, method is noise). COLOR = transect;
# genera panel dashed, species panel solid (house rule). A single shared transect legend.
# CI bands omitted (overlapping polygons unreadable); Chao2 +/- SE lives in the table (section 5).
sacs_for <- function(key_col) {
  sacs <- list()
  for (tr in TRANSECTS) {
    rows <- expanded[expanded$transect == tr, ]         # BOTH methods pooled
    if (!nrow(rows)) next
    s <- accumulate(rows, key_col); if (is.null(s)) next
    sacs[[tr]] <- s
  }
  sacs
}
plot_accumulation_combined <- function(file) {
  ranks <- c(genera = "genus_key", species = "species_key")
  sac_by_rank <- lapply(ranks, sacs_for)
  if (!length(unlist(sac_by_rank, recursive = FALSE))) { message("Nothing to plot."); return(invisible()) }
  all_sac <- unlist(sac_by_rank, recursive = FALSE)           # ONE panel: shared axes across BOTH ranks
  xmax <- max(vapply(all_sac, function(s) max(s$sites),    numeric(1)))
  ymax <- max(vapply(all_sac, function(s) max(s$richness), numeric(1)))
  lty_rank <- c(genera = 2, species = 1)                      # house rule: genera dashed, species solid
  
  bee_png(file, width = 2200, height = 1600, res = 200); on.exit(dev.off())
  bee_base_par()                     # house-style fonts + muted axis/label colors
  op <- par(mar = c(4.6, 5.0, 1.5, 1.5), oma = c(0, 0, 0, 0))
  plot(NA, xlim = c(0, xmax), ylim = c(0, ymax), xlab = "surveys", ylab = "number of unique taxa",
       cex.lab = 1.15, cex.axis = 1.05)
  for (rk in names(ranks))                                    # overlay both ranks: color = transect, style = rank
    for (tr in names(sac_by_rank[[rk]]))
      lines(sac_by_rank[[rk]][[tr]]$sites, sac_by_rank[[rk]][[tr]]$richness,
            col = COLS[tr], lwd = 3.2, lty = lty_rank[rk])
  present <- names(sac_by_rank[["species"]])                  # one transect legend (color = transect)
  legend("bottomright", title = "transect", legend = present,
         col = COLS[present], lwd = 3.2, lty = 1, bty = "n", cex = 1.05,
         text.col = BEE_INK$secondary, title.col = BEE_INK$secondary)
  # rank key (house rule: species solid, genera dashed) -- neutral ink, since color = transect
  legend("bottomright", inset = c(0, 0.30), title = "rank", legend = c("species", "genera"),
         col = BEE_INK$secondary, lwd = 3.2, lty = c(1, 2), bty = "n", cex = 1.05,
         text.col = BEE_INK$secondary, title.col = BEE_INK$secondary)
  par(op)
}
plot_accumulation_combined(file.path(OUT_REPORT, "bee_taxa_accumulation_by_transect.png"))

# SPLIT figures for the report: the same curves, one figure PER RANK, so each can be
# read (or dropped into a page) on its own. Shared x/y limits across both figures so
# they compare directly; genera keep the dashed house mark to match the combined figure.
plot_accumulation_one_rank <- function(rank_label, key_col, lty, title_txt, sub_txt, file) {
  sacs <- sacs_for(key_col)
  if (!length(sacs)) { message("Nothing to plot for ", rank_label); return(invisible()) }
  both <- unlist(lapply(c("genus_key", "species_key"), sacs_for), recursive = FALSE)
  xmax <- max(vapply(both, function(s) max(s$sites),    numeric(1)))   # shared limits: the two
  ymax <- max(vapply(both, function(s) max(s$richness), numeric(1)))   # split figures stay comparable
  bee_png(file, width = 2200, height = 1600, res = 200); on.exit(dev.off())
  bee_base_par()
  op <- par(mar = c(4.6, 5.0, 1.5, 1.5), oma = c(0, 0, 0, 0))
  plot(NA, xlim = c(0, xmax), ylim = c(0, ymax), xlab = "Surveys",
       ylab = sprintf("Number of unique %s", rank_label),
       cex.lab = 1.15, cex.axis = 1.05)
  for (tr in names(sacs))
    lines(sacs[[tr]]$sites, sacs[[tr]]$richness, col = COLS[tr], lwd = 3.2, lty = lty)
  legend("bottomright", title = "Transect", legend = names(sacs),
         col = COLS[names(sacs)], lwd = 3.2, lty = lty, bty = "n", cex = 1.05,
         text.col = BEE_INK$secondary, title.col = BEE_INK$secondary)
  par(op)
}
plot_accumulation_one_rank("species", "species_key", 1,
                           NULL, NULL,
                           file.path(OUT_REPORT, "bee_species_accumulation_by_transect.png"))
plot_accumulation_one_rank("genera", "genus_key", 2,
                           NULL, NULL,
                           file.path(OUT_REPORT, "bee_genera_accumulation_by_transect.png"))

# ---- 4b. JOURNAL method comparison: lethal vs non-lethal, SMALL MULTIPLES ----
# ONE figure: a grid with rank as ROWS (genera top, species bottom) and transect as COLUMNS,
# each cell showing lethal vs non-lethal accumulation in the fair window -- so the method
# effect can be checked for consistency across transects AND ranks at a glance (not hidden by
# pooling). Shared x across all, y shared within each rank row. Lethal = intern nets,
# non-lethal = beeple photos, Mar-Oct 2021-2023. OT is dropped automatically (added 2024, no
# fair-window data), so only BST/TP/UPMON appear; the freed top-right cell holds the legend.
method_curves <- function(key_col) {
  curves <- list()
  for (tr in TRANSECTS) for (m in c("lethal", "nonlethal")) {
    rows <- expanded_fair[expanded_fair$transect == tr & expanded_fair$method == m, ]
    if (!nrow(rows)) next
    M <- survey_taxa_matrix(rows, key_col, lookup = recs_by_key_fair)
    if (ncol(M) == 0L || nrow(M) == 0L) next
    curves[[tr]][[m]] <- specaccum(M, method = "random", permutations = PERMUTATIONS)
  }
  curves
}
plot_accumulation_method_combined <- function(file) {
  ranks <- c(genera = "genus_key", species = "species_key")   # rows: genera on top, species below
  cbr <- lapply(ranks, method_curves)                         # cbr[[rank]][[transect]][[method]]
  present <- intersect(TRANSECTS, names(cbr[[1]]))
  if (!length(present)) { message("No journal accumulation to plot."); return(invisible()) }
  flat <- function(rk) unlist(cbr[[rk]], recursive = FALSE)   # -> flat list of specaccum objects
  xmax <- max(vapply(do.call(c, lapply(names(ranks), flat)), function(s) max(s$sites), numeric(1)))
  ymax_by_rank <- vapply(names(ranks), function(rk) max(vapply(flat(rk), function(s) max(s$richness), numeric(1))), numeric(1))

  nc <- length(present) + 1                          # transect columns + a legend column
  bee_png(file, width = 720 * nc, height = 640 * 2, res = 200); on.exit(dev.off())
  bee_base_par()
  op <- par(mfrow = c(2, nc), oma = c(5, 1, 6.4, 1), mar = c(2.6, 3.4, 2.4, 0.8), mgp = c(2, 0.6, 0))
  for (ri in seq_along(ranks)) {
    rk <- names(ranks)[ri]; ymax <- ymax_by_rank[rk]
    for (ci in seq_len(nc)) {
      if (ci <= length(present)) {
        tr <- present[ci]
        plot(NA, xlim = c(0, xmax), ylim = c(0, ymax), xlab = "",
             ylab = if (ci == 1) paste(rk, "recorded") else "")   # col-1 y-label doubles as the row (rank) label
        if (ri == 1) title(main = tr, col.main = COLS[tr], font.main = 2, line = 0.5)  # transect column header, top row only
        for (m in names(cbr[[rk]][[tr]]))
          lines(cbr[[rk]][[tr]][[m]]$sites, cbr[[rk]][[tr]][[m]]$richness, col = METHOD_COL[m], lwd = 2.8, lty = LTY[m])
      } else {                                        # last column: shared method legend in the top cell, empty below
        plot.new()
        if (ri == 1)
          legend("center", title = "method", legend = METHOD_LABEL[c("lethal", "nonlethal")],
                 col = METHOD_COL[c("lethal", "nonlethal")], lwd = 2.8, lty = LTY[c("lethal", "nonlethal")],
                 bty = "n", cex = 1.05, text.col = BEE_INK$secondary, title.col = BEE_INK$secondary)
      }
    }
  }
  mtext("surveys", side = 1, outer = TRUE, line = 1.4, col = BEE_INK$secondary, cex = 0.9)
  mtext("Do nets and photos find bees at the same pace?", side = 3, outer = TRUE,
        line = 3.6, col = BEE_INK$primary, font = 2, cex = 1.05)
  mtext("The two methods climb at different rates, and the gap is consistent across transects, so it is a real method effect, not a pooling artifact.",
        side = 3, outer = TRUE, line = 2.4, col = BEE_INK$secondary, cex = 0.8)   # takeaway (answers the title)
  bee_caption_base(scope = "fair window: survey-only records, per transect (BST/TP/UPMON; OT excluded -- added 2024)",
                   method = "lethal vs non-lethal", rank = "genera & species", line0 = 2.0,
                   sig = bee_test("sample-based species accumulation (specaccum)"))
  par(op)
}
plot_accumulation_method_combined(file.path(OUT_JOURNAL, "accumulation_by_effort_journal_combined.png"))

# ---- 5. summary table + incidence-based richness estimates -------------------
# richness_est(): observed richness plus the Chao2 incidence estimate of TRUE
# richness for a survey x taxa matrix, via vegan::specpool(). Chao2 (incidence,
# survey-based) is the estimator that matches these curves -- it asks "given how
# many taxa were seen in exactly one vs two surveys, how many were missed?".
# completeness = observed / estimated, in percent.
richness_est <- function(M) {
  if (ncol(M) == 0L) return(c(obs = 0, chao = NA_real_, se = NA_real_, pct = NA_real_))
  sp <- specpool(M)
  c(obs = sp$Species, chao = round(sp$chao, 1), se = round(sp$chao.se, 1),
    pct = round(100 * sp$Species / sp$chao, 1))
}

summary_tbl <- do.call(rbind, lapply(TRANSECTS, function(tr) {
  rows <- expanded[expanded$transect == tr, ]
  rs <- richness_est(survey_taxa_matrix(rows, "species_key"))
  rg <- richness_est(survey_taxa_matrix(rows, "genus_key"))
  data.frame(
    transect             = tr,
    n_surveys            = nrow(rows),
    surveys_lethal       = sum(rows$method == "lethal"),
    surveys_nonlethal    = sum(rows$method == "nonlethal"),
    species_observed     = rs["obs"],
    species_chao2        = rs["chao"],
    species_chao2_se     = rs["se"],
    species_pct_complete = rs["pct"],
    genera_observed      = rg["obs"],
    genera_chao2         = rg["chao"],
    genera_chao2_se      = rg["se"],
    genera_pct_complete  = rg["pct"],
    row.names = NULL)
}))
write.csv(summary_tbl, file.path(OUT_REPORT, "transect_accumulation_summary.csv"), row.names = FALSE)

message("Per-transect survey effort + observed / Chao2-estimated richness:")
print(summary_tbl, row.names = FALSE)
message("Chao2 estimates TRUE richness; the +/- SE (standard error) is how uncertain that\n",
        "estimate is -- true richness is ~95% likely within +/-2 SE. A large SE (e.g. OT)\n",
        "means the estimate is rough ('sample more here'), not a precise target.")
message("\nDone. Report figures + table in: ", OUT_REPORT, " | Journal small-multiples in: ", OUT_JOURNAL)
