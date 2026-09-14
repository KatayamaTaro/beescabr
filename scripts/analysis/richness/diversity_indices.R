# =============================================================
# analysis/richness/diversity_indices.R
# beescabr pipeline -- bee community diversity (Q8)
# Created: 2026-07-22
#
# Q8: "How diverse is the bee community -- by transect, by year, at genus vs
# species rank -- and do the assemblages differ?" Alpha-diversity indices,
# rank-abundance structure, and an NMDS + PERMANOVA composition test, following
# the vegan methods in Stevens' Primer of Ecology (ch. 14).
#
# SCOPE (read this before reading any figure):
#   * DIVERSITY INDICES + NMDS/PERMANOVA use SURVEY RECORDS ONLY (is_survey ==
#     TRUE), because abundance-/composition-based measures only compare fairly
#     across standardized effort. Both methods (lethal specimens + non-lethal
#     survey iNat) are POOLED into one community per group -- that is the bee
#     community found on that transect/year.
#   * The RANK-ABUNDANCE figure additionally SPLITS by method (lethal vs
#     non-lethal, survey-only) because the method contrast is the whole point there.
#   * Every figure carries a caption stating its exact scope + method + rank.
#   * (A whole-park richness MAP that uses ALL records is a separate script.)
#
# Note: TP's larger survey count is legitimate effort, not oversampling. The
# transect is split into two halves (TP1 and TP2) and only one half is walked per
# trip, logged as a single "TP" survey-event -- so each TP record is one
# independent survey walk, the same unit of effort as BST/UPMON/OT; TP's larger
# total just reflects being surveyed on more days. Year comparisons are restricted
# to the intern survey window (Mar-Sep) so seasonal coverage is comparable.
#
# Run from the repo root:  source("scripts/analysis/richness/diversity_indices.R")
# Depends on: dplyr, stringr, vegan, ggplot2 (+ config.R).
# =============================================================

# Dependencies are CHECKED here, not installed: see beescabr_require() in config.R.
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(vegan); library(ggplot2) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_TRANSECT")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
OUT_JOURNAL   <- file.path(DIR_JOURNAL, "richness/diversity/fair_method_2021_2023")  # fair-window rank-abundance (method contrast)
OUT_REPORT    <- file.path(DIR_REPORT,  "richness/diversity")  # evenness, NMDS/PERMANOVA, park-shape rank-abundance, backing CSVs
SPECIES_RANKS <- c("species", "subspecies")
GENUS_RANKS   <- c("species", "subspecies", "subgenus", "complex", "genus")
TRANSECTS     <- c("BST", "UPMON", "TP", "OT")
WINDOW_MONTHS <- 3:9                 # intern survey window (Mar-Sep) for year comparisons
MIN_SITE_REC  <- 50                  # REPORT floor: a site (transect x year) needs this many records to enter NMDS/PERMANOVA
dir.create(OUT_JOURNAL, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_REPORT,  recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# consistent scope caption stamped on every figure -- the SHARED helper from theme_beescabr.R
# (adds Source + data-as-of and one canonical order; no local override).

# ---- 1. SURVEY-ONLY bee records (both methods), with keys + grouping ----------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
prep <- function(df, method) {
  df %>%
    filter(is_true(is_survey)) %>%
    transmute(
      method   = method,
      transect = toupper(str_squish(transect)),
      year     = suppressWarnings(as.integer(ifelse(!is.na(survey_year) & survey_year != "",
                                                    survey_year, substr(observed_on, 1, 4)))),
      month    = suppressWarnings(as.integer(substr(observed_on, 6, 7))),
      taxon_rank, genus, species) %>%
    mutate(
      species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                             !is.na(species) & species != "", paste(genus, word(species, -1)), NA),
      genus_key   = ifelse(taxon_rank %in% GENUS_RANKS & !is.na(genus) & genus != "", genus, NA))
}
rec <- bind_rows(prep(spec, "lethal"), prep(inat, "nonlethal")) %>%
  filter(transect %in% TRANSECTS)
message(sprintf("Survey-only bee records: %d (lethal %d, non-lethal %d)",
                nrow(rec), sum(rec$method == "lethal"), sum(rec$method == "nonlethal")))

# ---- helpers: community matrix + alpha indices -------------------------------
comm_matrix <- function(df, group_col, key_col) {
  d <- df[!is.na(df[[key_col]]) & !is.na(df[[group_col]]), ]
  t <- table(d[[group_col]], d[[key_col]])
  matrix(as.integer(t), nrow = nrow(t), dimnames = dimnames(t))
}
# NOTE: fully-qualify vegan functions -- igraph also exports diversity(), and if
# the network script loaded igraph earlier in the session it would mask vegan's.
alpha_indices <- function(M) {
  S <- vegan::specnumber(M); H <- vegan::diversity(M, "shannon")
  data.frame(group = rownames(M), n_records = as.integer(rowSums(M)),
             richness = as.integer(S),
             hill_q1_expShannon = round(exp(H), 1),      # effective # of common species
             invSimpson_q2      = round(vegan::diversity(M, "invsimpson"), 1),
             pielou_evenness    = ifelse(S > 1, round(H / log(S), 3), NA_real_), row.names = NULL)
}

# ---- 2. ALPHA DIVERSITY BY TRANSECT (survey-only, both methods, species+genus) --
Mt_sp <- comm_matrix(rec, "transect", "species_key")[TRANSECTS, , drop = FALSE]
Mt_gn <- comm_matrix(rec, "transect", "genus_key")[TRANSECTS, , drop = FALSE]
div_tr <- rbind(cbind(rank = "species", alpha_indices(Mt_sp)),
                cbind(rank = "genus",   alpha_indices(Mt_gn)))
write.csv(div_tr, file.path(OUT_REPORT, "diversity_by_transect.csv"), row.names = FALSE)

# tiny long-pivot helper (avoid tidyr dep) -- still used by the by-year section below
tidyr_pivot <- function(df, cols) {
  do.call(rbind, lapply(cols, function(c)
    data.frame(rank = df$rank, group = df$group, metric = c, value = df[[c]])))
}
# EVENNESS ONLY. The raw richness / Shannon / Simpson panels were RETIRED -- comparing them
# across groups of unequal sampling effort is biased; the effort-standardized versions live in
# rarefaction_{vegan,inext}.R (iNEXT Hill q0/q1/q2). Pielou EVENNESS (J = H / log S) is a ratio
# and is fairly effort-robust, so it stays as its own figure (kept at Lauren's request). The full
# index table (all four measures) is still written to diversity_by_transect.csv as backing data.
plot_evenness <- function(dfin, file, title, cap, group_lab) {
  d <- dfin[!is.na(dfin$pielou_evenness), ]
  d$group <- factor(d$group, levels = sort(unique(as.character(d$group))))   # transects alphabetical (BST/OT/TP/UPMON), consistent across figures
  d$rank  <- factor(d$rank, levels = c("species", "genus"))
  has_pat <- requireNamespace("ggpattern", quietly = TRUE)
  # transect bars: hue = transect identity (BEE_TRANSECT), so the color is "free" for the transect.
  # The species-vs-genus split is carried by PATTERN instead: species = solid, genus = hatched
  # (house rule for genus-vs-species comparisons). Fill legend is dropped (hue is named on the x-axis).
  # NOTE: evenness stays DODGED (side-by-side), not overlaid like the transect/year RICHNESS bars.
  # An overlap needs one rank consistently taller; here genus evenness can EXCEED species
  # (e.g. OT: genus 0.78 > species 0.61) and they tie elsewhere (BST), so an overlap flips the
  # hatch to the top on some transects and reads inconsistently. Dodged keeps both ranks clear.
  base <- list(
    geom_text(data = d, aes(x = group, y = pielou_evenness, label = sprintf("%.2f", pielou_evenness),
              group = rank), position = position_dodge(0.8), vjust = -0.35, size = 3,
              colour = BEE_INK$secondary, inherit.aes = FALSE),
    scale_fill_manual(values = BEE_TRANSECT, guide = "none"),
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.08))),
    labs(title = title,
         caption = cap,   # rationale (why only evenness) moved to the report text -- caption stays scope_cap only
         x = group_lab, y = "Evenness"),
    theme_beescabr(11),
    theme(panel.grid.major.x = element_blank(), plot.title = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0, size = 7.5)))
  if (has_pat) {
    g <- ggplot(d, aes(x = group, y = pielou_evenness, fill = group, pattern = rank)) +
      ggpattern::geom_col_pattern(position = position_dodge(0.8), width = 0.7, colour = NA,
        pattern_fill = "white", pattern_colour = NA, pattern_angle = 45,
        pattern_density = 0.10, pattern_spacing = 0.028, pattern_key_scale_factor = 0.5) +
      ggpattern::scale_pattern_manual(values = c(species = "none", genus = "stripe"), name = "rank",
        breaks = c("genus", "species"), labels = c(genus = "genera", species = "species"),
        guide = guide_legend(override.aes = list(fill = "grey75", pattern_fill = "white"))) +
      base
  } else {
    # fallback (ggpattern absent): keep the transect hue, split rank by shade instead of hatch.
    g <- ggplot(d, aes(x = group, y = pielou_evenness, fill = group, alpha = rank)) +
      geom_col(position = position_dodge(0.8), width = 0.7) +
      scale_alpha_manual(values = c(species = 1, genus = 0.45), name = "rank",
        guide = guide_legend(override.aes = list(fill = BEE_INK$secondary))) +
      base
  }
  bee_ggsave(file, g, width = 7.5, height = 5.4, bg = "white")
}
plot_evenness(div_tr, file.path(OUT_REPORT, "bee_evenness_by_transect_pielou.png"),
             "Evenness",
             scope_cap("survey records only", "lethal + non-lethal pooled", "species vs genus", sig = bee_test("Pielou's evenness (J')")),
             "Transect")

# ---- 3. ALPHA DIVERSITY BY YEAR (survey-only, both methods, Mar-Sep, species) --
rec_win <- rec %>% filter(month %in% WINDOW_MONTHS, !is.na(year))
My_sp <- comm_matrix(rec_win, "year", "species_key")
div_yr <- cbind(rank = "species", alpha_indices(My_sp))
write.csv(div_yr, file.path(OUT_REPORT, "diversity_by_year.csv"), row.names = FALSE)
# EVENNESS ONLY (why: see caption). Raw richness/Shannon/Simpson across years of unequal effort
# are biased -> those live effort-standardized in rarefaction (iNEXT by_year). Evenness stays.
WHY_EVEN <- paste("Only evenness is shown: richness / Shannon / Simpson are effort-biased across unequal",
                  "sampling (they are effort-standardized in the rarefaction figures). Pielou evenness is a",
                  "ratio, so it is effort-robust.")
{
  d <- div_yr[!is.na(div_yr$pielou_evenness), ]
  g <- ggplot(d, aes(x = factor(group), y = pielou_evenness, group = 1)) +
    geom_line(color = BEE_TEAL[[5]]) + geom_point(color = BEE_TEAL[[5]], size = 2.4) +
    geom_text(aes(label = sprintf("%.2f", pielou_evenness)), vjust = -0.9, size = 3, colour = BEE_INK$secondary) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.02, 0.1))) +
    labs(title = "Does one bee dominate in any year?",
         subtitle = sprintf("Communities stay fairly balanced across years. Evenness runs %.2f to %.2f, so no single species dominates.",
                            min(d$pielou_evenness), max(d$pielou_evenness)),
         caption = scope_cap("survey records only, Mar-Sep window", "lethal + non-lethal pooled", "species", sig = bee_test("Pielou's evenness (J')")),
         x = "year", y = "how evenly bees are spread   (0 = one dominates, 1 = all equal)") +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0, size = 7.5))
  bee_ggsave(file.path(OUT_REPORT, "bee_evenness_by_year_pielou.png"), g, width = 8.5, height = 5.4, bg = "white")
}

# ---- 4. RANK-ABUNDANCE (Whittaker) -- SPLIT by paper --------------------------
# JOURNAL: lethal-vs-non-lethal comparison, FAIR WINDOW (survey-only, Mar-Oct 2021-2023)
#          -- same scope as yield_by_method / efficiency / the Venn.
# REPORT : the park's community SHAPE, ALL records -- ONE combined figure: the pooled
#          whole-park line (bold) over the per-transect curves (thin, nearly overlapping).
rad_df <- function(counts, lab) {
  counts <- sort(counts[counts > 0], decreasing = TRUE)
  data.frame(grp = lab, rank = seq_along(counts), rel_abund = counts / sum(counts))
}

# -- JOURNAL: 3 lines (pooled / lethal / non-lethal), fair window ----------------
rad_rec <- rec %>% filter(month %in% FAIR_MONTHS, year %in% FAIR_YEARS)
radJ <- rbind(rad_df(as.integer(table(rad_rec$species_key)),                              "both pooled"),
              rad_df(as.integer(table(rad_rec$species_key[rad_rec$method == "lethal"])),    "lethal"),
              rad_df(as.integer(table(rad_rec$species_key[rad_rec$method == "nonlethal"])), "non-lethal"))
gJ <- ggplot(radJ, aes(rank, rel_abund, color = grp)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1) + scale_y_log10(labels = function(x) paste0(format(signif(x * 100, 2), trim = TRUE, scientific = FALSE), "%")) +
  scale_color_manual(values = c("both pooled" = BEE_INK$primary,
                                "lethal" = unname(BEE_METHOD_COL["lethal"]),
                                "non-lethal" = unname(BEE_METHOD_COL["nonlethal"])), name = "method") +
  labs(title = "Are a few bees common and the rest rare?",
       subtitle = "A few species dominate the records and most are uncommon. That long tail is the signature of a diverse community.",
       caption = scope_cap("fair window: survey-only, Mar-Oct 2021-2023", "lethal vs non-lethal", "species", sig = bee_test("rank-abundance distribution")),
       x = "bee species, from most common (left) to rarest (right)", y = "% of all records (log scale)") +
  theme_beescabr(11) + theme(plot.title = element_text(hjust = 0.5))
bee_ggsave(file.path(OUT_JOURNAL, "diversity_rank_abundance_journal.png"), gJ, width = 9, height = 6, bg = "white")

# -- REPORT: ALL records (park community shape) ----------------------------------
key_all <- function(df, method) df %>% transmute(
  method = method, transect = toupper(str_squish(transect)),
  species_key = ifelse(taxon_rank %in% SPECIES_RANKS & !is.na(genus) & genus != "" &
                         !is.na(species) & species != "", paste(genus, word(species, -1)), NA))
rec_all <- bind_rows(key_all(spec, "lethal"), key_all(inat, "nonlethal")) %>% filter(!is.na(species_key))
# ONE combined figure: pooled whole-park line (bold) OVER the per-transect curves (thin).
# The transect curves nearly overlap the pooled line -- a small, homogeneous park.
radR <- rad_df(as.integer(table(rec_all$species_key)), "all bees (pooled)")
radT <- do.call(rbind, lapply(TRANSECTS, function(tr) {
  cc <- table(rec_all$species_key[rec_all$transect == tr]); if (!length(cc)) return(NULL)
  rad_df(as.integer(cc), tr)
}))
comb <- rbind(radT, radR)                                  # pooled last => drawn on top
comb$grp <- factor(comb$grp, levels = c("all bees (pooled)", TRANSECTS))
rad_cols <- c("all bees (pooled)" = BEE_INK$primary, setNames(unname(BEE_TRANSECT[TRANSECTS]), TRANSECTS))
rad_lwd  <- c("all bees (pooled)" = 1.7, setNames(rep(0.8, length(TRANSECTS)), TRANSECTS))
gR <- ggplot(comb, aes(rank, rel_abund, color = grp, linewidth = grp)) +
  geom_line() + scale_y_log10(labels = function(x) paste0(format(signif(x * 100, 2), trim = TRUE, scientific = FALSE), "%")) +
  scale_color_manual(values = rad_cols, name = NULL) +
  scale_linewidth_manual(values = rad_lwd, guide = "none") +
  labs(title = "Are a few bees common and the rest rare?",
       subtitle = str_wrap(paste("The single most common bee is about a quarter of all records and the top three are more than half,",
                                  "yet 79% of species each make up under 1% and 20 have been seen only once.",
                                  "That long tail of uncommon bees is what a diverse community looks like, and it is where continued surveying keeps finding more."), 118),
       caption = scope_cap("all records", "lethal + non-lethal pooled", "species", sig = bee_test("rank-abundance distribution")),
       x = "bee species, from most common (left) to rarest (right)", y = "% of all records (log scale)") +
  theme_beescabr(11) + theme(plot.title = element_text(hjust = 0.5))
bee_ggsave(file.path(OUT_REPORT, "bee_species_commonness_rank_abundance.png"), gR, width = 9, height = 6, bg = "white")

# ---- 5. NMDS + PERMANOVA: does composition differ by transect/year? ----------
# sites = transect x year (survey-only, both methods pooled), species community.
site <- rec %>% filter(!is.na(species_key), !is.na(year)) %>%
  mutate(site = paste(transect, year, sep = "_"))
Msite <- comm_matrix(site, "site", "species_key")
Msite <- Msite[rowSums(Msite) >= MIN_SITE_REC, , drop = FALSE]
Msite <- Msite[, colSums(Msite) > 0, drop = FALSE]
meta <- data.frame(site = rownames(Msite),
                   transect = sub("_.*", "", rownames(Msite)),
                   year     = sub(".*_", "", rownames(Msite)))
set.seed(1)
perm <- vegan::adonis2(Msite ~ transect + year, data = meta, method = "bray", permutations = 999)
capture.output(print(perm), file = file.path(OUT_REPORT, "diversity_permanova.txt"))
message("\nPERMANOVA (Bray-Curtis, transect + year):")
print(perm)

mds <- tryCatch(vegan::metaMDS(Msite, distance = "bray", autotransform = FALSE, trace = 0),
                error = function(e) NULL)
if (!is.null(mds)) {
  sc <- as.data.frame(vegan::scores(mds, display = "sites")); sc$transect <- meta$transect
  sig <- bee_test("PERMANOVA (Bray-Curtis)", with(perm, sprintf("transect R2=%.2f, p=%.3f", R2[1], `Pr(>F)`[1])))   # standardized Analysis: slot
  # plain-language TAKEAWAY (the "so what"), shown as a subtitle under the title
  takeaway <- sprintf("Transects host distinct bee communities. Transect explains %.0f%% of the compositional variation (PERMANOVA, p = %.3f).",
                      100 * perm$R2[1], perm$`Pr(>F)`[1])
  g <- ggplot(sc, aes(NMDS1, NMDS2, color = transect, label = meta$site)) +
    geom_point(size = 3) +
    ggrepel::geom_text_repel(size = 2.6, show.legend = FALSE, seed = 1, max.overlaps = Inf,   # repel labels off the dots + each other
                             min.segment.length = 0.15, box.padding = 0.45, point.padding = 0.3,
                             segment.color = "grey75", segment.size = 0.3) +
    scale_color_manual(values = BEE_TRANSECT, name = "transect") +   # transect owns color (house palette)
    labs(title = "Do the transects share the same bees?",
         subtitle = str_wrap(takeaway, 96),
         caption = scope_cap(sprintf("survey records only; sites = transect x year, >= %d records each (NMDS stress %.2f)", MIN_SITE_REC, mds$stress),
                             "lethal + non-lethal pooled", "species",
                             control = "year (999-permutation null)", sig = sig),
         x = "bee community composition (axis 1)", y = "bee community composition (axis 2)") +
    theme_beescabr(11) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5, size = 9.5, colour = BEE_INK$secondary))
  bee_ggsave(file.path(OUT_REPORT, "bee_community_composition_nmds.png"), g, width = 8.5, height = 6.5, bg = "white")
}

message("\nDone. Report diversity outputs in: ", OUT_REPORT, " | Journal rank-abundance in: ", OUT_JOURNAL)
