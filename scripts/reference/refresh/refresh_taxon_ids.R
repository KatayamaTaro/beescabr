# =============================================================
# scripts/reference/refresh/refresh_taxon_ids.R
# beescabr  [iNaturalist REST API v1 -- see INAT_API_VERSION in config.R]
# "Ask iNaturalist about every bee number again" -- part of the once-a-year rebuild.
#
#   From the repo root:  source("scripts/reference/refresh/refresh_taxon_ids.R")
#
# WHY THIS EXISTS. Every bee in the reference tables is pinned to an iNaturalist
# number, and that number was looked up ONCE. Nothing ages it and, until now, nothing
# could ask again -- so a bee resolved in 2024 still carries its 2024 number and its
# 2024 name.
#
# WHAT GOES WRONG WITHOUT IT. iNaturalist never edits a taxon in place. When a bee is
# split, lumped or swapped, that taxon is RETIRED and a new one with a NEW number takes
# over its records. Our observations follow automatically -- they are re-downloaded
# every run -- but the reference tables do not. So the records move to the new number
# while the checklist keeps the old one, they stop matching, and the bee turns up in
# the park/county-additions list looking like a brand-new discovery.
#
# Nothing is changed on the strength of this run. It writes a review file and prints
# what moved, because deciding what a split bee should now be called is a judgement.
#
# Needs internet; no token. A few minutes.
# =============================================================

suppressPackageStartupMessages({ library(dplyr); library(readr) })
if (!exists("PATHS"))               source("scripts/config.R")
if (!exists("store_connect"))       source("scripts/inat_observations/engine/db/store_conn.R")
if (!exists("taxon_cache_get"))     source("scripts/inat_observations/engine/db/taxon_store.R")
if (!exists("decisions_for_taxa"))  source("scripts/inat_observations/engine/db/decision_store.R")
if (!exists("sweep_taxon_changes")) source("scripts/inat_observations/engine/api/inat_cache.R")
if (!exists("merge_manual_overrides")) source("scripts/reference/prompts/manual_overrides.R")
if (!exists("plant_change_choice")) source("scripts/reference/taxonomy/plant_taxonomy_lookup_build.R")

# every bee number our reference tables hold
.rti_ids <- function(path, col = "taxon_id") {
  if (!file.exists(path)) return(integer(0))
  d <- suppressWarnings(read_csv(path, show_col_types = FALSE, progress = FALSE))
  if (!col %in% names(d)) return(integer(0))
  v <- suppressWarnings(as.integer(d[[col]]))
  unique(v[!is.na(v)])
}
.rti_all <- sort(unique(c(.rti_ids(PATHS$holway_reference), .rti_ids(PATHS$taxonomy_lookup))))

# an override is recorded against a bee's NAME and RANK, not its number, so a
# correction needs the reference row behind the id
.rti_ref_row <- function(id) {
  for (p in c(PATHS$taxonomy_lookup, PATHS$holway_reference)) {
    if (!file.exists(p)) next
    d <- suppressWarnings(read_csv(p, show_col_types = FALSE, progress = FALSE))
    if (!all(c("taxon_id", "scientific_name", "rank") %in% names(d))) next
    hit <- which(suppressWarnings(as.integer(d$taxon_id)) == id)
    if (length(hit)) return(as.list(d[hit[1], c("rank", "scientific_name")]))
  }
  NULL
}

message("")
if (!length(.rti_all)) {
  message("  Nothing to re-check yet -- the reference tables have no bee numbers.")
  message("  Run the cleaning pipeline first:")
  message("    source(\"scripts/run_data_cleaning_pipeline.R\")")
} else {
  message("  Asking iNaturalist about all ", length(.rti_all), " bee numbers again.")
  message("  Each was looked up once and the answer kept ever since, so this is the only")
  message("  thing that notices a bee iNaturalist has revised. A few minutes. Nothing is")
  message("  decided here -- anything that moved is written to a review file.")
  message("")

  .rti_con <- store_connect(DB_CACHE_PATH)
  on.exit(try(store_disconnect(.rti_con), silent = TRUE), add = TRUE)
  changed <- sweep_taxon_changes(.rti_con, .rti_all)

  message("")
  if (!nrow(changed)) {
    message("  Nothing moved. All ", length(.rti_all), " bees are still the taxon they were.")
  } else {
    dir.create(dirname(PATHS$taxon_changes_review), recursive = TRUE, showWarnings = FALSE)
    write_csv(changed, PATHS$taxon_changes_review)

    message("  ", nrow(changed), " of ", length(.rti_all), " bees changed on iNaturalist:")
    message("")
    for (k in c("retired", "gone", "renamed", "rank changed")) {
      rows <- changed[changed$change == k, , drop = FALSE]
      if (!nrow(rows)) next
      message("  ", toupper(k), "  (", nrow(rows), ")")
      if (k == "retired")
        message("    iNaturalist split, lumped or swapped these. Their records have moved to a",
                "\n    NEW number, so the checklist row and the records no longer match, and the",
                "\n    bee will show up as a new county record. Read these first.")
      if (k == "gone")
        message("    iNaturalist returns nothing for this number at all. The bee keeps the name",
                "\n    we have, but has no taxonomy behind it.")
      if (k == "renamed")
        message("    Same taxon, new spelling. The number still matches, so nothing breaks --",
                "\n    but the checklist now prints an out-of-date name.")
      if (k == "rank changed")
        message("    Same taxon, moved rank (a genus became a subgenus, or the reverse).")
      for (i in seq_len(nrow(rows))) {
        r <- rows[i, ]
        message(sprintf("      %-32s %s", r$was %||% "?",
                        sprintf("https://www.inaturalist.org/taxa/%s", r$taxon_id)))
        if (!is.na(r$now) && !identical(r$now, r$was))
          message(sprintf("      %-32s now reads: %s", "", r$now))
      }
      message("")
    }
    # Every retirement gets the SAME prompt. What differs is only how much help the
    # tool can give: one named replacement means it can offer "take"; a split or an
    # unnamed retirement means the operator reads the page and types the number. The
    # old shape -- prompt for one flavor, print a list for the other two -- sent a
    # person to a spreadsheet for exactly the cases that were hardest to work out.
    retired <- changed[changed$change == "retired", , drop = FALSE]
    recorded <- 0L

    if (nrow(retired) && interactive()) {
      message("  ", nrow(retired), " bee", if (nrow(retired) == 1L) "" else "s",
              " need", if (nrow(retired) == 1L) "s" else "", " a decision.")
      message("  Open the old link. iNaturalist's page says what the bee became -- look for")
      message("  its Taxonomy section. Then:")
      message("")
      message("    901455   the new number, typed in (it is the number in the iNat address)")
      message("    take     accept the replacement shown below, where there is one")
      message("    keep     leave it on the old number. Pressing Enter does the same.")
      message("    quit     stop here, keeping what you have answered")
      message("")
      message("  Not sure? Keep it. A wrong number silently files this bee's records under")
      message("  another bee; leaving it alone only means being asked again next year.")

      answers <- list()
      for (i in seq_len(nrow(retired))) {
        r <- retired[i, ]
        offers <- if (is.na(r$replaced_by)) integer(0) else
          suppressWarnings(as.integer(strsplit(r$replaced_by, ",", fixed = TRUE)[[1]]))
        offers <- offers[!is.na(offers)]
        message("")
        message(sprintf("  [%d/%d] %s", i, nrow(retired), r$was))
        message(sprintf("     was      %-30s https://www.inaturalist.org/taxa/%s",
                        paste0(r$was, " (", r$taxon_id, ")"), r$taxon_id))
        if (length(offers) == 1L) {
          nt <- tryCatch(get_taxon_by_id(.rti_con, offers[1]), error = function(e) NULL)
          message(sprintf("     replaced by  %-26s https://www.inaturalist.org/taxa/%s",
                          paste0(nt$name %||% "?", " (", offers[1], ")"), offers[1]))
        } else if (length(offers) > 1L) {
          message("     SPLIT into ", length(offers), " taxa -- iNaturalist cannot say which one")
          message("     this bee's records belong to, and nor can this tool. Open them, decide")
          message("     which fits the Cabrillo material, and type that number:")
          for (o in offers) {
            nt <- tryCatch(get_taxon_by_id(.rti_con, o), error = function(e) NULL)
            message(sprintf("       %-30s https://www.inaturalist.org/taxa/%s",
                            paste0(nt$name %||% "?", " (", o, ")"), o))
          }
        } else {
          message("     iNaturalist named no replacement. Its page still says what happened --")
          message("     read the Taxonomy section there and type the number you find.")
        }
        repeat {
          ans <- taxon_change_choice(readline("     number / take / keep / quit: "),
                                     has_offer = length(offers) == 1L)
          if (ans$action == "unclear") {
            message("     Not understood. Type a number from an iNaturalist address,",
                    if (length(offers) == 1L) " or take," else "", " or keep, or quit.")
            next
          }
          break
        }
        if (ans$action == "quit") break
        if (ans$action == "keep") next
        pick <- if (ans$action == "take") offers[1] else ans$id
        ref  <- .rti_ref_row(r$taxon_id)
        if (is.null(ref)) {
          message("     This bee is not in the reference table, so there is no name to record")
          message("     the correction against. Left alone; it is in the review file.")
          next
        }
        nt <- tryCatch(get_taxon_by_id(.rti_con, pick), error = function(e) NULL)
        if (is.null(nt)) {
          message("     iNaturalist has no taxon ", pick, ". Check the number and try again.")
          next
        }
        message("     -> ", pick, " = ", nt$name %||% "?", " (", nt$rank %||% "?", ")")
        answers[[length(answers) + 1L]] <- tibble(
          rank = ref$rank, name = ref$scientific_name, taxon_id = pick,
          correct_name = nt$name %||% NA_character_,
          note = paste0("taxon sweep: ", r$taxon_id, " retired"))
      }
      if (length(answers)) {
        new_rows <- bind_rows(answers)
        prev <- if (file.exists(MANUAL_OVERRIDES_PATH))
          tryCatch(suppressWarnings(read_csv(MANUAL_OVERRIDES_PATH, show_col_types = FALSE)),
                   error = function(e) NULL) else NULL
        dir.create(dirname(MANUAL_OVERRIDES_PATH), recursive = TRUE, showWarnings = FALSE)
        write_csv(merge_manual_overrides(new_rows, prev), MANUAL_OVERRIDES_PATH, na = "")
        recorded <- nrow(new_rows)
      }
      message("")
      message("  Recorded ", recorded, " correction", if (recorded == 1L) "" else "s",
              if (recorded) paste0(" -> ", MANUAL_OVERRIDES_PATH) else
                " -- everything left on its old number.")
    } else if (nrow(retired)) {
      message("  ", nrow(retired), " bee", if (nrow(retired) == 1L) "" else "s",
              " retired and need a decision, but this is not an interactive")
      message("  session so nothing was changed. Re-run it in R to settle them:")
      message("    source(\"scripts/reference/refresh/refresh_taxon_ids.R\")")
    }

    # A saved answer at the Holway prompt is replayed before any API call, so a pick
    # that chose a now-retired taxon would keep handing back the dead number however
    # many corrections were recorded above. Drop ONLY those answers: the next build
    # re-resolves them properly, and the hundreds that are still right are untouched.
    # (Wiping the table is the other option, and it is why "full rebuild" is a job
    # nobody does -- it re-asks everything to catch the handful that moved.)
    moved <- changed$taxon_id[changed$change %in% c("retired", "gone")]
    stale_answers <- decisions_for_taxa(.rti_con, moved)
    if (length(stale_answers)) {
      n_forgot <- decision_forget(.rti_con, stale_answers)
      message("")
      message("  ", n_forgot, " saved answer", if (n_forgot == 1L) "" else "s",
              " pointed at ", if (n_forgot == 1L) "a bee" else "bees",
              " iNaturalist has retired, so ",
              if (n_forgot == 1L) "it was" else "they were", " cleared.")
      message("  The next cleaning run looks ", if (n_forgot == 1L) "that name" else "those names",
              " up again rather than replaying a dead number.")
      message("  Every other saved answer is untouched.")
      for (a in stale_answers) message("      ", a)
    }

    message("")
    message("  Everything that moved, including what you settled:")
    message("  ", PATHS$taxon_changes_review)
  }
  message("")
}
