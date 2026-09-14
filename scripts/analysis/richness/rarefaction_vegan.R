# =============================================================
# analysis/richness/rarefaction_vegan.R
# Rarefaction (vegan) -- fair diversity by standardizing sampling effort
# beescabr / Cabrillo National Monument (CABR) native bees
#
# WHY: raw species counts are unfair to compare because more sampling almost
# always finds more species. Rarefaction asks "if every group had been sampled
# the SAME amount, how many species would each have?" -- it sub-samples the better-
# sampled groups down to the smallest group's size and reports expected richness
# there, so leftover differences are real, not effort artifacts.
#
# FOUR COMPARISONS, each run at BOTH genus and species rank (survey records only,
# both methods pooled unless the comparison IS the method split):
#   1. per TRANSECT  -- rarefy all transects to the smallest transect's total
#                       ("is OT really poorer, or just under-sampled?")
#   2. per YEAR      -- Mar-Sep window; rarefy all years to the lowest-count year
#   3. beeple vs intern -- rarefy the two observer groups to a common effort
#   4. observations vs specimens -- non-lethal iNaturalist vs lethal specimens
# Genus rank = robust (every ID'd record counts); species rank = finer but sparser.
#
# METHOD = classic individual-based rarefaction to the LOWEST sample size
# (vegan::rarefy, interpolation only). See the folder README for how this compares
# to iNEXT's size-based and coverage-based standardization, and which to trust.
#
# Run from the repo root:  source("scripts/analysis/richness/rarefaction_vegan.R")
# Depends on: dplyr, stringr, vegan, ggplot2 (+ config.R).
# =============================================================

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(vegan); library(ggplot2) })

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_TRANSECT")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
OUT_JOURNAL   <- function(dim) file.path(DIR_JOURNAL, "richness/rarefaction", rare_window_dir(dim))  # the rarefaction family, split out of accumulation (44 files in one folder);
OUT_REPORT    <- file.path(DIR_REPORT,  "richness/rarefaction")  # the dimension is baked into each filename, no by_<dim>/ subfolders
SPECIES_RANKS <- c("species", "subspecies")
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
WINDOW_MONTHS <- 3:9
# which paper each rarefaction dimension belongs to
if (!exists("rare_out_name")) source("scripts/analysis/shared/rarefaction_names.R")
JOURNAL_DIMS  <- names(RARE_WINDOWS)
# journal-dimension rarefy tables, both ranks stacked; written once after the rank loop.
VEG_ACC       <- new.env(parent = emptyenv())
# ALL rarefaction is a richness-by-effort method, so every dimension (by_transect, by_year,
# and the journal by_method/by_observer) lives together in richness/accumulation -- next to the
# accumulation curves + Chao2. (It is NOT filed under richness/diversity, which holds the
# community-structure analyses: evenness, NMDS/PERMANOVA, rank-abundance.)
rare_base <- function(dimdir) if (dimdir %in% JOURNAL_DIMS) OUT_JOURNAL(dimdir) else OUT_REPORT
for (.d in JOURNAL_DIMS) dir.create(OUT_JOURNAL(.d), recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_REPORT,  recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"
# scope_cap(): use the SHARED helper from theme_beescabr.R -- adds Source + data-as-of, one canonical order (no local override).

# ---- 1. survey-only bee records with keys + grouping vars --------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) {
  df %>% filter(is_true(is_survey)) %>%
    transmute(method = method,
      surveyor = ifelse(is.na(surveyor_type) | str_squish(surveyor_type) == "",
                        "unattributed", str_squish(tolower(surveyor_type))),
      transect = toupper(str_squish(transect)),
      year  = suppressWarnings(as.integer(ifelse(!is.na(survey_year) & survey_year != "",
                                                 survey_year, substr(observed_on, 1, 4)))),
      month = suppressWarnings(as.integer(substr(observed_on, 6, 7))),
      taxon_rank, genus, species) %>%
    mutate(obs_type = ifelse(method == "lethal", "specimen", "observation"),
           species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                                  !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
           genus_key   = ifelse(!is.na(genus) & genus != "", genus, NA))
}
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal"))

# community matrix: rows = groups, cols = taxa at the chosen rank (key_col)
comm <- function(df, group_col, key_col) {
  d <- df[!is.na(df[[group_col]]) & !is.na(df[[key_col]]), ]
  t <- table(d[[group_col]], d[[key_col]])
  matrix(as.integer(t), nrow = nrow(t), dimnames = dimnames(t))
}

# ---- 2. helpers: rarefy-to-lowest table + curves -----------------------------
rarefy_table <- function(M) {
  M <- M[rowSums(M) > 0, , drop = FALSE]
  minN <- min(rowSums(M))
  rr <- vegan::rarefy(M, sample = minN, se = TRUE)      # rows: S (expected) and se
  data.frame(group = rownames(M),
             n_records = as.integer(rowSums(M)),
             observed_richness = as.integer(vegan::specnumber(M)),
             rarefied_to = minN,
             rarefied_richness = round(as.numeric(rr["S", ]), 1),
             rarefied_se = round(as.numeric(rr["se", ]), 2),
             row.names = NULL) %>%
    arrange(desc(rarefied_richness))
}
UNIT <- function(rank) if (rank == "genus") "genera" else "species"
curve_df <- function(M) {
  M <- M[rowSums(M) > 0, , drop = FALSE]
  do.call(rbind, lapply(rownames(M), function(g) {
    v <- M[g, ]; N <- sum(v); ns <- unique(pmax(1, round(seq(1, N, length.out = 40))))
    data.frame(group = g, n = ns, S = sapply(ns, function(n) as.numeric(vegan::rarefy(v, n))))
  }))
}
#' Draw and write one rarefaction figure
#'
#' @param M Site-by-taxon matrix; all-zero rows are dropped first.
#' @param key Output name stem, from `rare_out_name()`.
#' @param title Figure title.
#' @param rank `"genus"` or `"species"`. Genus figures are hatched and species
#'   solid, so the two read as different charts at a glance.
#'
#' @param cols Fill colors, one per group; `NULL` uses the theme default.
#' @param group_fill Color by group rather than by rank.
#' @return Invisibly, nothing. Skips with a message when fewer than 2 groups have data.
draw <- function(M, key, title, rank, cols = NULL, group_fill = FALSE) {
  M <- M[rowSums(M) > 0, , drop = FALSE]
  if (nrow(M) < 2) { message("  ", key, ": <2 groups with data, skipped"); return(invisible()) }
  unit <- UNIT(rank)
  # HOUSE RULE: genus-rank figures are hatched, species-rank solid (so a genus chart reads distinct from
  # its species twin). col_geom() draws bars either way -- hatched stripes for genus, plain for species.
  col_geom <- function(...) ggpattern::geom_col_pattern(...,
    pattern = if (rank == "genus") "stripe" else "none",
    pattern_fill = "white", pattern_colour = NA, pattern_angle = 45,
    pattern_density = 0.10, pattern_spacing = 0.028, pattern_key_scale_factor = 0.5)
  # write straight into the accumulation folder; the dimension (+ method label, journal only)
  # is baked into the filename, e.g. rarefaction_by_transect_species_curves.png. The report has
  # only vegan so it drops the "_vegan" tag; the journal keeps it (it also carries iNEXT files).
  dimdir <- sub(paste0("_", rank, "$"), "", key)   # "by_transect_species" -> "by_transect"
  outdir <- rare_base(dimdir); dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  vlab   <- if (dimdir %in% JOURNAL_DIMS) "_vegan" else ""
  pre    <- paste0("rarefaction_", dimdir)   # rank appended LAST, e.g. rarefaction_by_transect_bars_species
  # A report dimension's table is the data behind its figure, so it stays paired with it,
  # one file per rank. A JOURNAL dimension has no per-rank figure of its own (the combined
  # figure carries both), so its ranks stack into one table -- four files become two.
  tab <- rarefy_table(M)
  if (dimdir %in% JOURNAL_DIMS) {
    VEG_ACC[[dimdir]] <- rbind(VEG_ACC[[dimdir]], cbind(rank = rank, tab))
  } else {
    write.csv(tab, file.path(outdir, paste0(pre, vlab, "_", rank, ".csv")), row.names = FALSE)
  }
  minN <- min(rowSums(M)); cdf <- curve_df(M)
  cap  <- scope_cap("survey records only", "lethal + non-lethal pooled", rank,
                    sig = bee_test("individual-based rarefaction to a common record count"))
  cols <- if (is.null(cols)) setNames(grDevices::colorRampPalette(BEE_SEQ)(nrow(M)), rownames(M)) else cols  # ordinal groups (years) -> blue sequential
  g1 <- ggplot(cdf, aes(n, S, color = group)) +
    geom_vline(xintercept = minN, linetype = "dashed", color = "grey50") +
    annotate("text", x = minN, y = 0, label = sprintf(" rarefy to %d", minN),
             hjust = 0, vjust = 0, size = 3, color = "grey40") +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = cols, name = NULL) +
    labs(title = title, subtitle = "Richness rarefied to a common sampling effort -- differences between groups are real, not just who-was-sampled-more.",
         caption = cap,
         x = "records sampled", y = paste0(unit, " (adjusted for effort)")) +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  # curves: JOURNAL only. The REPORT dropped its rarefaction curves -- the bars (observed vs
  # rarefied) are the report's rarefaction figure (Taro's call). Journal keeps both for review.
  if (dimdir %in% JOURNAL_DIMS) {
  # SUPERSEDED in the fair window by rarefaction_combined.R: bee_ggsave(file.path(outdir, paste0(pre, vlab, "_curves_", rank, ".png")), g1, width = 8, height = 5.4, bg = "white")
  }
  # RAREFIED-ONLY bars (raw observed counts dropped for clarity, per project decision): the raw
  # vs rarefied pairing cluttered the figure, and the rarefied bar is the one that carries the
  # claim ("genuinely richer, not just more-sampled"). Applies to every dimension (year/transect/
  # method/observer). Flip back to the grouped raw+rarefied version via git history if ever needed.
  bdr <- data.frame(group = tab$group, S = tab$rarefied_richness)
  # transect bars sort ALPHABETICALLY (consistent BST/OT/TP/UPMON across every transect figure); YEAR bars
  # sort CHRONOLOGICALLY; other dimensions keep the rarefy_table order (descending rarefied richness).
  lev <- if (group_fill)               sort(unique(as.character(bdr$group)))
         else if (dimdir == "by_year") as.character(sort(as.numeric(unique(as.character(bdr$group)))))
         else                          tab$group
  bdr$group <- factor(bdr$group, levels = lev)
  # transect version colors each bar by its transect hue; every other dimension uses one neutral ink.
  g2 <- if (group_fill)
    ggplot(bdr, aes(group, S, fill = group)) + col_geom(width = 0.62) +
      scale_fill_manual(values = cols, guide = "none")
  else
    ggplot(bdr, aes(group, S)) + col_geom(width = 0.62, fill = BEE_TEAL[[5]])
  g2 <- g2 +
    geom_text(aes(label = round(S)), vjust = -0.35, size = 3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(title = title, subtitle = sprintf("Rarefied to %d records -- taller bar = genuinely richer, not just more-sampled.", minN),
         x = switch(dimdir, by_observer = "observer", by_method = "method", by_transect = "transect", by_year = "year", "group"),
         y = unit, caption = cap) +
    theme_beescabr(11) +
    theme(panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  # SUPERSEDED in the fair window by rarefaction_combined.R: bee_ggsave(file.path(outdir, paste0(pre, vlab, "_bars_", rank, ".png")), g2, width = 8, height = 5, bg = "white")
  message(sprintf("  %-22s: rarefied to %d records; %s",
                  key, minN, paste(sprintf("%s=%.0f", tab$group, tab$rarefied_richness), collapse = "  ")))
}

# ---- 3. run every comparison at BOTH ranks (genus + species) -----------------
# Every comparison is run twice -- once at genus rank (robust: every ID'd record
# counts) and once at species rank (finer, but only species-resolved records).
RANKS <- c(species = "species_key", genus = "genus_key")
TCOLS <- BEE_TRANSECT   # house transect palette
rec_win  <- rec %>% filter(month %in% WINDOW_MONTHS, !is.na(year))
rec_fair <- rec %>% filter(month %in% 3:10, year %in% 2021:2023)   # FAIR WINDOW for the lethal-vs-non-lethal comparisons (matches yield_by_method/efficiency/Venn; drops 2024 intern photos)

for (rk in names(RANKS)) {
  kc <- RANKS[[rk]]
  message(sprintf("Vegan rarefaction, %s rank:", rk))
  # 1. transect + 2. year (Mar-Sep) -- both drawn ONCE, combined across ranks, AFTER this loop
  #    (see below); no per-rank transect/year bar figures are produced, only the combined charts.
  # 3. observer: beeple vs intern -- its OWN window (May-Sep 2024, non-lethal only), where
  #    both groups photographed. On the 2021-2023 fair window this was a duplicate of the
  #    method comparison, since only interns netted and only beeple photographed there.
  w_obs <- rare_window_records(rec, "by_observer")
  draw(comm(w_obs, "surveyor", kc), paste0("by_observer_", rk),
       rare_window("by_observer")$title, rk, BEE_OBSERVER_COL)
  # 4. method: observations (iNaturalist) vs specimens (fair window)
  w_met <- rare_window_records(rec, "by_method")
  draw(comm(w_met, rare_window("by_method")$group, kc), paste0("by_method_", rk),
       rare_window("by_method")$title, rk,
       BEE_METHOD_COL)   # keys are lethal / nonlethal, matching the window levels
}

# ---- combined BY-TRANSECT figure: genus + species in ONE horizontal, faceted chart ----
# Same treatment as the by-year figure. Transect on the y-axis, each bar in its transect hue;
# genera and species are separate panels (different metrics, each rarefied to its own effort).
tr <- do.call(rbind, lapply(names(RANKS), function(rk) {
  M <- comm(filter(rec, transect %in% TRANSECTS), "transect", RANKS[[rk]])
  M <- M[intersect(TRANSECTS, rownames(M)), , drop = FALSE]; M <- M[rowSums(M) > 0, , drop = FALSE]
  t <- rarefy_table(M)
  write.csv(t, file.path(rare_base("by_transect"), paste0("bee_richness_by_transect_rarefaction_", rk, ".csv")), row.names = FALSE)
  data.frame(rank = rk, unit = UNIT(rk), transect = as.character(t$group),
             S = t$rarefied_richness, minN = min(rowSums(M)), stringsAsFactors = FALSE)
}))
tr$rank     <- factor(tr$rank, levels = c("genus", "species"))                          # genera left, species right
tr$transect <- factor(tr$transect, levels = sort(unique(as.character(tr$transect))))  # alphabetical (house rule): BST, OT, TP, UPMON
# genus + species share ONE panel, OVERLAID per transect (not dodged): the solid species
# bar sits behind, the hatched genus bar in front. Species richness always >= genus, so the
# genus bar is shorter and the species-only band shows above it -- both stay readable in one bar.
# Legend just names the ranks; the per-rank rarefaction depths go in the scope caption instead.
n_gen <- tr$minN[tr$rank == "genus"][1]; n_sp <- tr$minN[tr$rank == "species"][1]
tr$rank <- factor(tr$rank, levels = c("species", "genus"))  # species drawn first (behind), genus on top
tr <- tr[order(tr$rank), ]
gtr <- ggplot(tr, aes(transect, S, fill = transect, pattern = rank)) +
  ggpattern::geom_col_pattern(width = 0.68, position = position_identity(), colour = NA,
    pattern_fill = "white", pattern_colour = NA, pattern_angle = 45,
    pattern_density = 0.10, pattern_spacing = 0.028, pattern_key_scale_factor = 0.5) +
  ggpattern::scale_pattern_manual(values = c(genus = "stripe", species = "none"), name = NULL,
    breaks = c("genus", "species"), labels = c(species = "species", genus = "genera")) +
  scale_fill_manual(values = TCOLS, guide = "none") +
  guides(pattern = guide_legend(override.aes = list(fill = "grey75"))) +   # legend keys show the hatch clearly
  geom_text(data = subset(tr, rank == "species"), aes(label = round(S)), vjust = -0.35, size = 3) +
  geom_text(data = subset(tr, rank == "genus"), aes(label = round(S)),
            vjust = -0.35, size = 3, colour = "white", fontface = "bold") +   # genus count sits just above the hatch top (on the solid species band)
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(title = "Richness",
       subtitle = "Richness rarefied to a common sampling effort within each rank",
       x = "Transect", y = "Number of unique taxa",
       caption = scope_cap("survey records only", "lethal + non-lethal pooled", "genus & species",
                           control = sprintf("rarefied to a common effort (genera %d, species %d records)", n_gen, n_sp),
                           sig = bee_test("individual-based rarefaction"))) +
  theme_beescabr(11) +
  theme(panel.grid.major.x = element_blank(), legend.position = "right",
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

bee_ggsave(file.path(rare_base("by_transect"), "bee_richness_by_transect_rarefaction.png"), gtr,
           width = 9.2, height = 5, bg = "white")
message("  by_transect combined (genus + species): bee_richness_by_transect_rarefaction.png")

# ---- combined BY-YEAR figure: genus + species OVERLAID in ONE horizontal panel ----
# The public "richer years?" figure. Year is the y-axis; the solid species bar sits behind and
# the hatched genus bar in front (species richness >= genus in every year, so the species-only
# band always shows past the hatch). Genus count sits just past the hatch end, on the solid band.
# Both ranks share one teal fill; the hatch is the only genus/species cue. Depths in the caption.
yr <- do.call(rbind, lapply(names(RANKS), function(rk) {
  M <- comm(rec_win, "year", RANKS[[rk]]); M <- M[rowSums(M) > 0, , drop = FALSE]
  t <- rarefy_table(M)
  # keep the per-rank machine-readable table (the individual bar FIGURES are retired, the data is not)
  write.csv(t, file.path(rare_base("by_year"), paste0("bee_richness_by_year_rarefaction_", rk, ".csv")), row.names = FALSE)
  data.frame(rank = rk, unit = UNIT(rk), year = as.character(t$group),
             S = t$rarefied_richness, minN = min(rowSums(M)), stringsAsFactors = FALSE)
}))
n_gy <- yr$minN[yr$rank == "genus"][1]; n_sy <- yr$minN[yr$rank == "species"][1]
yr$rank <- factor(yr$rank, levels = c("species", "genus"))          # species drawn first (behind), genus on top
yr$year <- factor(yr$year, levels = rev(sort(unique(yr$year))))     # chronological, 2021 at top
yr <- yr[order(yr$rank), ]
gyr <- ggplot(yr, aes(S, year, pattern = rank)) +
  ggpattern::geom_col_pattern(fill = BEE_TEAL[[5]], width = 0.68, position = position_identity(), colour = NA,
    pattern_fill = "white", pattern_colour = NA, pattern_angle = 45,
    pattern_density = 0.10, pattern_spacing = 0.028, pattern_key_scale_factor = 0.5) +
  ggpattern::scale_pattern_manual(values = c(genus = "stripe", species = "none"), name = NULL,
    breaks = c("genus", "species"), labels = c(genus = "genera", species = "species")) +
  guides(pattern = guide_legend(override.aes = list(fill = "grey75"))) +
  geom_text(data = subset(yr, rank == "species"), aes(label = round(S)), hjust = -0.35, size = 3) +
  geom_text(data = subset(yr, rank == "genus"), aes(label = round(S)),
            hjust = -0.35, size = 3, colour = "white", fontface = "bold") +   # genus count just past the hatch end, on the solid band
  scale_x_continuous(expand = expansion(mult = c(0, 0.10))) +
  labs(title = "Did some years have more richness of bees than others?",
       subtitle = "Richness rarefied to a common sampling effort within each rank, so a longer bar means genuinely richer, not just more-sampled.",
       x = "number of unique taxa", y = "year",
       caption = scope_cap("survey records only", "lethal + non-lethal pooled", "genus & species",
                           control = sprintf("rarefied to a common effort (genera %d, species %d records)", n_gy, n_sy),
                           sig = bee_test("individual-based rarefaction"))) +
  theme_beescabr(11) +
  theme(panel.grid.major.y = element_blank(), legend.position = "right",
        plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
bee_ggsave(file.path(rare_base("by_year"), "bee_richness_by_year_rarefaction.png"), gyr,
           width = 9, height = 4.8, bg = "white")
message("  by_year combined (genus + species): bee_richness_by_year_rarefaction.png")

# one vegan table per journal dimension, both ranks stacked
for (dimdir in ls(VEG_ACC)) {
  f <- file.path(rare_base(dimdir), rare_out_name(dimdir, kind = "rarefied"))
  write.csv(VEG_ACC[[dimdir]], f, row.names = FALSE)
  message(sprintf("  %-11s: %d rarefy rows -> %s", dimdir, nrow(VEG_ACC[[dimdir]]), basename(f)))
}

message("Wrote rarefaction_by_{transect,year}_bars_combined (report) + ",
        "bee_richness_photos_vs_specimens_rarefied_to_smallest_group.csv (journal)\n",
        "  into richness/rarefaction/ of each paper.")
