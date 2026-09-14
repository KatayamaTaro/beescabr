# =============================================================
# analysis/interactions/interactions_top_plants.R
# beescabr pipeline -- top plants bees visit (Q3)
# Created: 2026-07-21
#
# Q3: "Which plants do bees visit the most -- across the year, and per month --
# so we know what to plant more of?" Counts of bee records per visited plant
# genus (the resolved `plant_genus`). CABR only.
#
# THREE SCOPES (per the stakeholder decision):
#   * whole-park  -- every bee record with a plant genus
#   * survey-only -- is_survey == TRUE
#   * by method   -- lethal (specimen/net) vs non-lethal (iNat/photo)
# Plus a per-MONTH breakdown of the top plants (forage seasonality).
#
# "Just by counts." NOTE: counts reflect sampling effort + detectability, not a
# bee's preference -- a common plant can top the list because it's everywhere and
# easy to photograph. And the season window matters: interns survey ~Mar-Sep,
# beeple (non-lethal) run year-round, so month coverage is uneven.
#
# Run from the repo root:  source("scripts/analysis/interactions/interactions_top_plants.R")
# Depends on: dplyr, stringr (+ config.R). Base-R plots -- no extra packages.
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(stringr) })

# ---- config -----------------------------------------------------------------
if (!exists("PATHS")) source("scripts/config.R")
if (!exists("BEE_METHOD_COL")) source("scripts/analysis/shared/theme_beescabr.R")   # shared house style
if (!exists("plant_label")) source("scripts/analysis/shared/plant_names.R")          # shared plant common-name labels
OUT_DIR   <- file.path(DIR_REPORT, "interactions/top_plants")
TOP_N     <- 10          # top plants for the headline table/figure
TOP_MONTH <- 10          # plants shown in the month heatmap (matched to TOP_N)
COL_LETHAL <- unname(BEE_METHOD_COL["lethal"]); COL_NONLETHAL <- unname(BEE_METHOD_COL["nonlethal"])  # purple net / vermillion photo
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
is_true <- function(x) toupper(str_squish(as.character(x))) == "TRUE"

# ---- 1. bee records that name a plant genus (both methods) -------------------
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
rec <- bind_rows(
  spec %>% transmute(plant_genus, method = "lethal",
                     is_survey = is_true(is_survey), date = observed_on),
  inat %>% transmute(plant_genus, method = "nonlethal",
                     is_survey = is_true(is_survey), date = observed_on)
) %>% filter(!is.na(plant_genus), plant_genus != "")
rec$month <- suppressWarnings(as.integer(substr(rec$date, 6, 7)))
message(sprintf("Bee-visit records with a plant genus: %d", nrow(rec)))

# ---- 2. top plants, three scopes --------------------------------------------
tally <- function(df, nm) df %>% count(plant_genus, name = nm)
tbl <- tally(rec, "whole_park") %>%
  full_join(tally(filter(rec, is_survey),            "survey_only"),  by = "plant_genus") %>%
  full_join(tally(filter(rec, method == "lethal"),   "lethal"),       by = "plant_genus") %>%
  full_join(tally(filter(rec, method == "nonlethal"),"nonlethal"),    by = "plant_genus") %>%
  mutate(across(-plant_genus, ~ replace(., is.na(.), 0L))) %>%
  arrange(desc(whole_park))
tbl$rank_whole_park <- seq_len(nrow(tbl))
write.csv(tbl, file.path(OUT_DIR, "interactions_top_plants.csv"), row.names = FALSE)

top <- head(tbl, TOP_N)
message(sprintf("\nTop %d plant genera (whole-park visits):", TOP_N))
print(top[, c("plant_genus", "whole_park", "survey_only", "lethal", "nonlethal")],
      row.names = FALSE)

# ---- 3. figure A: top-N plants, method-split bars ---------------------------
bee_png(file.path(OUT_DIR, "interactions_top_plants.png"),
    width = 2050, height = 1150, res = 200)
bee_base_par()                                    # house-style fonts + muted axis colors
op <- par(mar = c(5.5, 16, 3.5, 1), oma = c(3.6, 0, 0, 0))  # wide left margin for labels; bottom oma fits the 3-line scope caption
M <- rbind(nonlethal = top$nonlethal, lethal = top$lethal)   # stacked
colnames(M) <- top$plant_genus                               # keep raw genus (indexing/order); labels drawn as plotmath below
M <- M[, ncol(M):1, drop = FALSE]                            # #1 at top
.stars <- setNames(bee_low_n_mark(top$whole_park), top$plant_genus)   # '*' on thinly-sampled plants
.plab  <- as.expression(lapply(colnames(M), function(g) {    # common name upright, Latin italic, '*' appended
  cn <- plant_common_name(g); s <- .stars[[g]]
  if (is.na(cn)) bquote(italic(.(g)) * .(s)) else bquote(.(cn) ~ "(" * italic(.(g)) * ")" * .(s))
}))
bp <- barplot(M, horiz = TRUE, names.arg = rep("", ncol(M)), border = NA,
        col = c(nonlethal = COL_NONLETHAL, lethal = COL_LETHAL),
        xlab = "bee-visit records",
        main = "Which plants do the park's bees visit most?")
axis(2, at = bp, labels = .plab, las = 1, tick = FALSE, cex.axis = 0.82, col.axis = BEE_INK$muted)
mtext("A few plant genera pull most of the bee visits -- the park's keystone forage.", side = 3, line = 0.3, cex = 0.8, col = BEE_INK$secondary)   # takeaway
legend("bottomright", bty = "n", fill = c(COL_NONLETHAL, COL_LETHAL), text.col = BEE_INK$primary,
       legend = c("non-lethal", "lethal"))
if (any(bee_low_n(top$whole_park)))
  mtext(BEE_LOW_N_NOTE, side = 1, line = 4.2, cex = 0.7, adj = 0, col = BEE_INK$secondary)
bee_caption_base(scope = "all records, whole park; counts = bee-visit records (reflect sampling effort, not preference)",
                 method = "lethal + non-lethal pooled", rank = "plant genus")
par(op); dev.off()

# ---- 4. per-month breakdown of the top plants -------------------------------
top_m   <- head(tbl$plant_genus, TOP_MONTH)
top_tot <- head(tbl$whole_park, TOP_MONTH)                  # total bee-visit records = the "favorite" ranking
mm <- rec %>% filter(!is.na(month), plant_genus %in% top_m) %>%
  count(plant_genus, month)
Mmon <- matrix(0L, nrow = length(top_m), ncol = 12,
               dimnames = list(top_m, month.abb))
for (i in seq_len(nrow(mm)))
  Mmon[mm$plant_genus[i], mm$month[i]] <- mm$n[i]
write.csv(data.frame(favourite_rank = seq_along(top_m), plant_genus = rownames(Mmon),
                     total_visits = top_tot, Mmon, check.names = FALSE),
          file.path(OUT_DIR, "interactions_top_plants_by_month.csv"), row.names = FALSE)

# y-axis numbered by the bees' FAVORITE: rank 1 = most total visit records. Labels carry rank + count.
# plant genus shown as its common name (Latin) via the shared label helper; names() stay the raw
# genus so the matrix-rowname indexing below still lines up.
# rank + common name upright, Latin italic, + total count -- as plotmath so only the Latin is italicised
rank_lab <- setNames(lapply(seq_along(top_m), function(i) {
  g <- top_m[i]; cn <- plant_common_name(g)
  rk <- paste0(i, ". "); ct <- paste0(" (", format(top_tot[i], big.mark = ",", trim = TRUE), ")")
  if (is.na(cn)) bquote(.(rk) * italic(.(g)) * .(ct)) else bquote(.(rk) * .(cn) ~ "(" * italic(.(g)) * ")" * .(ct))
}), top_m)

bee_png(file.path(OUT_DIR, "interactions_top_plants_by_month.png"),
        width = 1900, height = 950, res = 200)
bee_base_par()
op <- par(mar = c(4, 17, 1, 6), oma = c(0, 0, 0, 0))   # shrunk: no title/caption reserved anymore
Mplot  <- Mmon[nrow(Mmon):1, , drop = FALSE]
ramp_m <- grDevices::colorRampPalette(BEE_SEQ)(24)
image(x = 1:12, y = seq_len(nrow(Mplot)), z = t(log1p(Mplot)),
      col = ramp_m, axes = FALSE, xlab = "", ylab = "",
      main = "")
axis(1, 1:12, month.abb, las = 2, cex.axis = 1.0)
axis(2, seq_len(nrow(Mplot)), labels = as.expression(rank_lab[rownames(Mplot)]), las = 1, cex.axis = 0.85)
mtext("month", side = 1, line = 2.6, cex = 1.0, col = BEE_INK$secondary)

# color-scale legend
lx0 <- grconvertX(0.90, "ndc", "user"); lx1 <- grconvertX(0.925, "ndc", "user")
ly0 <- grconvertY(0.30, "ndc", "user");  ly1 <- grconvertY(0.70, "ndc", "user")
nb  <- length(ramp_m); ys <- seq(ly0, ly1, length.out = nb + 1)
rect(lx0, ys[-(nb + 1)], lx1, ys[-1], col = ramp_m, border = NA, xpd = NA)
rect(lx0, ly0, lx1, ly1, border = BEE_INK$secondary, lwd = 0.8, xpd = NA)
text(lx1, ly1, sprintf(" %s", format(max(Mmon), big.mark = ",")), pos = 4, offset = 0.15, xpd = NA, cex = 0.75, col = BEE_INK$secondary)
text(lx1, ly0, " 0", pos = 4, offset = 0.15, xpd = NA, cex = 0.75, col = BEE_INK$secondary)
text((lx0 + lx1) / 2, ly1, "records/month", pos = 3, offset = 0.5, xpd = NA, cex = 0.75, col = BEE_INK$secondary)
par(op); dev.off()
