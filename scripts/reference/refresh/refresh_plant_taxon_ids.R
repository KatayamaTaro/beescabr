# =============================================================
# scripts/reference/refresh/refresh_plant_taxon_ids.R
# beescabr  [iNaturalist REST API v1 -- see INAT_API_VERSION in config.R]
# "Ask iNaturalist about every plant name again" -- part of the once-a-year rebuild.
#
#   From the repo root:  source("scripts/reference/refresh/refresh_plant_taxon_ids.R")
#
# WHY THIS EXISTS. Every plant name is resolved to an iNaturalist taxon ONCE, and the
# answer is kept in data/reference/generated/plant_name_resolution_cache.csv forever.
# Nothing ages that file and, until now, nothing could re-check it -- so a plant name
# resolved in 2024 still carries its 2024 taxon_id.
#
# WHY IT MATTERS MORE FOR PLANTS THAN FOR BEES. Plants are resolved by NAME. When
# iNaturalist retires a taxon, the same name starts matching its replacement, which has
# a DIFFERENT id. Nothing breaks loudly: the bee-plant joins simply start pointing at a
# taxon nobody chose. That is what "different taxon" below means, and it is the row to
# read first.
#
# Nothing is changed on the strength of this run alone -- it rewrites the cache with
# what iNaturalist says today and PRINTS what moved, so a person can judge it.
#
# Needs internet; no token. Takes a few minutes for a few hundred names.
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(readr) })
if (!exists("PATHS"))            source("scripts/config.R")
if (!exists("plt_resolve_names")) source("scripts/reference/taxonomy/plant_taxonomy_lookup_build.R")

.rpt_cache <- PATHS$plant_name_cache
before <- plt_load_cache(.rpt_cache)

if (!nrow(before)) {
  message("")
  message("  Nothing to re-check yet.")
  message("  ", .rpt_cache)
  message("  does not exist, which means no plant name has been resolved on this")
  message("  computer yet. Run the cleaning pipeline first:")
  message("    source(\"scripts/run_data_cleaning_pipeline.R\")")
} else {
  names_vec <- before$input_name
  message("")
  message("  Asking iNaturalist about all ", length(names_vec), " plant names again.")
  message("  These were each looked up once and the answer kept ever since, so this is")
  message("  the only thing that notices a plant iNaturalist has since revised.")
  message("  iNaturalist rate-limits this, so expect about an hour and long pauses")
  message("  between names -- that is the wait, not a hang. Progress is printed as it")
  message("  goes, and answers are saved along the way, so stopping it loses only the")
  message("  name in flight. Nothing is decided here -- anything that moved is printed")
  message("  for you to judge.")
  message("")

  res <- plt_resolve_names(names_vec, cache = before, force = TRUE, verbose = TRUE)
  after <- res$cache
  changed <- plant_cache_changes(before, after)

  # A rename keeps the id, so it is taken silently. An id that MOVED is a judgement:
  # only a person can say whether iNaturalist's new match is the same plant, and
  # writing it first and reporting it after meant the joins had already followed.
  ask <- plant_changes_to_ask(changed)
  kept <- character(0)
  if (nrow(ask) && interactive()) {
    message("")
    message("  ", nrow(ask), " of these need you to decide, because the number changed.")
    message("  Every bee-plant record on that plant currently points at the OLD number.")
    message("  Open both links: if they are the same plant, take the new one.")
    message("")
    message("    take     use the new number")
    message("    keep     leave it on the old number. Pressing Enter does the same.")
    message("    quit     stop here, keeping what you have answered")
    for (i in seq_len(nrow(ask))) {
      r <- ask[i, ]
      message("")
      message(sprintf("  [%d/%d] %s", i, nrow(ask), r$input_name))
      message(sprintf("     was  %-38s https://www.inaturalist.org/taxa/%s",
                      paste0(r$name_was, " (", r$id_was, ")"), r$id_was))
      if (identical(r$change, "no longer resolves")) {
        message("     now  iNaturalist returns nothing for this name.")
        message("          Keeping the old number is usually right -- the plant did not")
        message("          vanish, the name just stopped matching.")
      } else {
        message(sprintf("     now  %-38s https://www.inaturalist.org/taxa/%s",
                        paste0(r$name_now, " (", r$id_now, ")"), r$id_now))
      }
      repeat {
        ans <- plant_change_choice(readline("     take / keep / quit: "))
        if (ans == "unclear") { message("     Not one of the three -- type take, keep, or quit."); next }
        break
      }
      if (ans == "quit") { kept <- c(kept, ask$input_name[i:nrow(ask)]); break }
      if (ans == "keep") kept <- c(kept, r$input_name)
    }
    after <- plant_apply_keep(after, before, kept)
  } else if (nrow(ask)) {
    message("")
    message("  ", nrow(ask), " name(s) changed number. This is not an interactive session,")
    message("  so nothing was changed -- the old numbers are kept. Re-run this in R to")
    message("  decide: source(\"scripts/reference/refresh/refresh_plant_taxon_ids.R\")")
    after <- plant_apply_keep(after, before, ask$input_name)
  }

  dir.create(dirname(.rpt_cache), recursive = TRUE, showWarnings = FALSE)
  write_csv(after, .rpt_cache)

  message("")
  if (!nrow(changed)) {
    message("  Nothing moved. All ", length(names_vec), " names still resolve to the same")
    message("  iNaturalist taxon they did before.")
  } else {
    .n <- function(k) sum(changed$change == k)
    message("  ", nrow(changed), " of ", length(names_vec), " plant names changed:")
    message("")
    for (k in c("different taxon", "no longer resolves", "renamed")) {
      rows <- changed[changed$change == k, , drop = FALSE]
      if (!nrow(rows)) next
      message("  ", toupper(k), "  (", nrow(rows), ")")
      if (k == "different taxon")
        message("    iNaturalist now matches this name to a different taxon. Every bee-plant",
                "\n    record using it now points somewhere new. Check these first.")
      if (k == "no longer resolves")
        message("    iNaturalist returns nothing for this name now. Records keep the name",
                "\n    but lose the taxonomy behind it.")
      if (k == "renamed")
        message("    Same taxon, new spelling. Cosmetic -- the joins still hold.")
      .shown <- function(nm, id) if (is.na(nm) || !nzchar(nm)) "nothing"
                                 else paste0(nm, " (", id, ")")
      for (i in seq_len(nrow(rows)))
        message(sprintf("      %-34s %s -> %s", rows$input_name[i],
                        .shown(rows$name_was[i], rows$id_was[i]),
                        .shown(rows$name_now[i], rows$id_now[i])))
      message("")
    }
    message("  Look each one up to confirm:  https://www.inaturalist.org/taxa/<the new number>")
  }
  message("  ", .rpt_cache)
  message("")
}
