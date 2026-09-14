# =============================================================
# analysis/coverage/records_near_transect.R
# beescabr -- every bee record in the park, by the transect it was recorded NEAR.
#
# NOT the same figure as transect_effort.R, and not a replacement for it.
#   transect_effort.R  counts SURVEY records, by the transect the surveyor tagged.
#                      That is sampling effort: somebody walked that transect.
#   this               counts EVERY record, by the transect it is physically closest
#                      to. That is spatial coverage: where in the park bees get
#                      recorded, by anyone, including the public.
#
# The two do not add up and are not meant to. 4,106 records carry no transect tag --
# almost all of them public observations rather than project surveys -- and the effort
# figure drops them, correctly. This one places them by location instead.
#
# The buffer is the whole figure: a record more than BUFFER_M from every route is left
# out rather than pushed to the nearest one. At 10 m the routes barely overlap, so
# almost nothing is ambiguous; widen it and the OT/TP junction starts claiming records
# that could belong to either.
#
# Run from the repo root:  source("scripts/analysis/coverage/records_near_transect.R")
# =============================================================

#' Which route is a record near, if any?
#'
#' PURE, taking a distance matrix so the rule is testable with no spatial data.
#' Nearest-within-the-buffer wins; a record equidistant from two routes is left
#' unassigned rather than picked arbitrarily, because there is no fact to pick on.
#'
#' @param dmat Rows = records, columns = routes (named), values = metres.
#' @param buffer_m How close counts as near.
#' @param tie_breaker Which route wins when two are exactly equidistant. TP, because
#'   the ties happen at the OT/TP junction and TP is the route actually walked there --
#'   it is surveyed as two transects and carries roughly double the effort of any other.
#'   Named rather than implicit: a tie-break is a decision, and a silent one is the kind
#'   of thing nobody finds for years.
#' @return A list of `route` (character, NA when out of range, or tied with the
#'   tie-breaker not among the tied) and `ambiguous` (TRUE when more than one route
#'   was within the buffer).
nearest_within <- function(dmat, buffer_m, tie_breaker = "TP") {
  labs <- colnames(dmat)
  route <- character(nrow(dmat)); amb <- logical(nrow(dmat))
  for (i in seq_len(nrow(dmat))) {
    d <- dmat[i, ]
    inb <- which(d <= buffer_m)
    amb[i] <- length(inb) > 1L
    if (!length(inb)) { route[i] <- NA_character_; next }
    best <- inb[d[inb] == min(d[inb])]
    route[i] <- if (length(best) == 1L) labs[best]
                else if (tie_breaker %in% labs[best]) tie_breaker
                else NA_character_
  }
  list(route = route, ambiguous = amb)
}

#' The caption, which has to carry what the bars cannot
#'
#' The bars are labelled by transect, so nothing on the axis distinguishes this from
#' the sampling-effort figure. The caption is where that has to happen.
#'
#' @param buffer_m The buffer used.
#' @param assigned,ambiguous,unassigned Counts.
#' @return One string.
near_transect_caption <- function(buffer_m, assigned, ambiguous, unassigned, tagged) {
  f <- function(x) format(x, big.mark = ",", trim = TRUE)
  paste0("Every bee record placed on a transect, from two kinds of evidence. ",
         f(tagged), " carry a transect the surveyor recorded, and keep it. A further ",
         f(assigned), " carry none and are placed by falling within ", buffer_m,
         " m of a route -- mostly public observations rather than surveys. ",
         f(unassigned), " were further than ", buffer_m,
         " m from every route and are not shown. ", f(ambiguous),
         " lay within reach of two routes and went to the nearer one, or to TP where ",
         "the two were exactly equidistant.")
}

#' A recorded transect always beats a geometric guess
#'
#' Geometry never overrules the person who walked it: a tag is evidence of what
#' happened, a distance is an inference about where somebody stood. So a record that
#' already has a transect keeps it, and only the ones with none are placed by distance.
#'
#' @param tagged The transect on the record; blank or NA when there is none.
#' @param near What proximity suggests; NA when nothing was within the buffer.
#' @return A list of `transect` and `source` ("tagged" / "nearby" / NA).
combine_transect_source <- function(tagged, near) {
  t <- toupper(trimws(as.character(tagged)))
  has <- !is.na(t) & nzchar(t)
  list(transect = ifelse(has, t, near),
       source   = ifelse(has, "tagged", ifelse(is.na(near), NA_character_, "nearby")))
}

# ---- everything below runs only when this file is run on purpose ----
if (!exists("RNT_SOURCED_FOR_HELPERS")) {

if (!exists("PATHS")) source("scripts/config.R")
if (!exists("bee_ggsave")) source("scripts/analysis/shared/theme_beescabr.R")
suppressPackageStartupMessages({ library(sf); library(dplyr); library(ggplot2); library(stringr) })

RNT_BUFFER_M <- 10
TRANSECT_LEVELS <- c("BST", "UPMON", "TP", "OT")
RNT_OUT      <- file.path(DIR_REPORT, "coverage/records_near_transect")
dir.create(RNT_OUT, recursive = TRUE, showWarnings = FALSE)

# ONLY the records whose transect is not already known. A survey record already has
# its transect -- tagged on the observation, or resolved from the majority of that
# surveyor's tags for the day -- and re-deriving it from geometry could only disagree
# with the person who walked it. This figure is about the records nothing else places.
inat <- read.csv(PATHS$inat_clean,     stringsAsFactors = FALSE, check.names = FALSE)
spec <- read.csv(PATHS$specimen_clean, stringsAsFactors = FALSE, check.names = FALSE)
pts  <- inat[!is.na(inat$latitude) & !is.na(inat$longitude), ]

shp <- list.files("data/spatial/shapefiles/transects", pattern = "[.]shp$",
                  full.names = TRUE, recursive = TRUE)[1]
tr  <- st_transform(st_read(shp, quiet = TRUE), 32611)
p   <- st_transform(st_as_sf(pts, coords = c("longitude", "latitude"), crs = 4326), 32611)

dmat <- matrix(as.numeric(st_distance(p, tr)), nrow = nrow(p),
               dimnames = list(NULL, as.character(tr$Name)))
res <- nearest_within(dmat, RNT_BUFFER_M)
cmb <- combine_transect_source(pts$transect, res$route)
pts$near_transect  <- cmb$transect
pts$transect_from  <- cmb$source
pts$near_ambiguous <- res$ambiguous

n_tagged     <- sum(pts$transect_from == "tagged", na.rm = TRUE)
n_assigned   <- sum(pts$transect_from == "nearby", na.rm = TRUE)
n_ambiguous  <- sum(pts$near_ambiguous & pts$transect_from == "nearby", na.rm = TRUE)
n_unassigned <- sum(is.na(pts$near_transect))

write.csv(pts[, c("obs_id", "observed_on", "observer", "is_survey", "transect",
                  "near_transect", "transect_from", "near_ambiguous",
                  "scientific_name", "url")],
          file.path(RNT_OUT, "records_near_transect.csv"), row.names = FALSE, na = "")

# Specimens keep the transect on the record -- their coordinates are transect
# centroids, so placing them by distance would only rediscover the centroid.
.sp_tr <- toupper(trimws(spec$transect))
sp_ok  <- .sp_tr %in% TRANSECT_LEVELS
n_spec <- sum(sp_ok)
# specimens with no transect are not shown either, and were being left out of the
# "not shown" count -- so the caption's numbers did not add up to the total.
n_unassigned <- n_unassigned + sum(!sp_ok)

tbl <- rbind(
  data.frame(transect = pts$near_transect[!is.na(pts$near_transect)],
             method = "non-lethal", stringsAsFactors = FALSE),
  data.frame(transect = .sp_tr[sp_ok], method = "lethal", stringsAsFactors = FALSE)) |>
  count(transect, method, name = "value")
tot <- tbl |> group_by(transect) |> summarise(n_records = sum(value)) |> arrange(-n_records)
tbl$transect <- factor(tbl$transect, levels = tot$transect)
tot$transect <- factor(tot$transect, levels = tot$transect)
tbl$method   <- factor(tbl$method, levels = c("non-lethal", "lethal"))

# same legend and colours as survey_effort_by_transect.png, so the two read as a pair
g <- ggplot(tbl, aes(transect, value, fill = method)) +
  geom_col(width = 0.66) +
  geom_text(aes(label = ifelse(value > 0, format(value, big.mark = ","), "")),
            position = position_stack(vjust = 0.5), colour = "white",
            fontface = "bold", size = 2.9, show.legend = FALSE) +
  geom_text(data = tot, aes(transect, n_records, label = format(n_records, big.mark = ",")),
            vjust = -0.35, size = 3, colour = BEE_INK$secondary, inherit.aes = FALSE) +
  scale_fill_manual(values = setNames(unname(BEE_METHOD_COL[c("nonlethal", "lethal")]),
                                      c("non-lethal", "lethal")), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(title = "Bee records by transect",
       subtitle = "Every record placed on a transect, not only the ones tagged as surveys.",
       x = "transect", y = "records",
       caption = str_wrap(near_transect_caption(RNT_BUFFER_M, n_assigned, n_ambiguous,
                                                n_unassigned, n_tagged + n_spec), 74)) +
  theme_beescabr(11) +
  theme(axis.text = element_text(size = 7, colour = BEE_INK$muted),
        legend.position = "top", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 8.5))
bee_ggsave(file.path(RNT_OUT, "records_near_transect.png"), g, width = 6.4, height = 5, bg = "white")

# Second version: one bar per transect in the house transect colours, pooled across
# method. Same figure, same numbers, different question -- "how much is on each
# transect" rather than "how was it collected". BEE_TRANSECT is the house colour per
# transect, so a transect is the same colour here as on every other transect figure,
# and the legend sits in the same top strip as the method legend so the two line up
# side by side on a slide.
g_total <- ggplot(tot, aes(transect, n_records, fill = transect)) +
  geom_col(width = 0.66) +
  geom_text(aes(label = format(n_records, big.mark = ",")), vjust = -0.35, size = 3,
            colour = BEE_INK$secondary) +
  scale_fill_manual(values = BEE_TRANSECT, name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
  labs(title = "Bee records by transect",
       subtitle = "Every record placed on a transect, not only the ones tagged as surveys.",
       x = "transect", y = "records",
       caption = str_wrap(near_transect_caption(RNT_BUFFER_M, n_assigned, n_ambiguous,
                                                n_unassigned, n_tagged + n_spec), 74)) +
  theme_beescabr(11) +
  theme(axis.text = element_text(size = 7, colour = BEE_INK$muted),
        legend.position = "top", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 8.5))
bee_ggsave(file.path(RNT_OUT, "records_near_transect_total.png"), g_total,
           width = 6.4, height = 5, bg = "white")

message(sprintf("  %s tagged + %s placed within %d m = %s on a transect; %s further away.",
                format(n_tagged, big.mark = ","), format(n_assigned, big.mark = ","),
                RNT_BUFFER_M, format(n_tagged + n_assigned, big.mark = ","),
                format(n_unassigned, big.mark = ",")))
message("  ", file.path(RNT_OUT, "records_near_transect.csv"))

}
