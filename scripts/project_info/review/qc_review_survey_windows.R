# =============================================================
# project_info/review/qc_review_survey_windows.R
# beescabr -- interactive review of SURVEY-DATE windows
# Created 2026-07-16.
#
# The brain (finding_project_info) auto-confirms a beeple survey window when the
# assigned surveyor has a Cabrillo-tagged obs (or an in-CABR untagged obs) inside
# it. Windows it CAN'T auto-confirm -- empty / off-site / excluded / no-username --
# get written to qc_review_survey_beeple_date_windows_generated.csv for a human ruling. This walks them.
#
# Runs LAST in the review chain (after tags + fields), so it sees the fullest
# picture of membership. For each un-ruled window you say whether the survey
# actually happened:
#   y  survey     -- yes, it happened (recorded for review ONLY; NOT added to survey_dates -- no tag = not a survey day)
#   n  no         -- it did not happen -> stays out
#   u  unsure     -- revisit next run
#   s  skip       -- leave blank, revisit next run       q  save & quit    ? help
#
# Decisions persist in qc_review_survey_beeple_date_windows_generated.csv (the `decision` column), so a
# window you've ruled y/n never comes back; only blank/unsure resurface.
#
# This file ALSO holds review_transect_ties() -- for equal-split days where a beeple's
# obs are tagged evenly across two transects (looks like two transects in one day). The
# brain can't pick a majority, so it lists the tag counts and you rule which transect the
# whole day really was (or "both" to keep it a genuine two-transect day). Same persist model.
#
# Run: source("scripts/project_info/review/qc_review_survey_windows.R"); review_windows()   # missing/off-site windows
#      review_transect_ties()                                        # equal-split transect days
# =============================================================

library(dplyr); library(readr)

RW_PATH <- "data/project_info/surveys/review/qc_review_survey_beeple_date_windows_generated.csv"

.rw_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
# a window still needs a ruling if its decision is blank OR "unsure"
.rw_todo  <- function(dec) .rw_blank(dec) | tolower(trimws(as.character(dec))) == "unsure"

.rw_help <- function() {
  bar <- strrep("-", 60)
  cat("\n", bar, "\n", sep = "")
  cat(" DID THIS SURVEY HAPPEN?\n\n")
  cat("   Someone was scheduled to walk a transect on these dates, and no photos were\n")
  cat("   tagged as a survey anywhere near them. That is all this is asking: did the\n")
  cat("   survey happen and go untagged, or did it not happen at all?\n\n")
  cat("   y   yes, it happened\n")
  cat("   n   no, it did not\n")
  cat("   u   unsure    -- recorded as unsure; you are asked again next run\n")
  cat("   s   skip      -- nothing recorded; you are asked again next run\n")
  cat("   l   look      -- where to check before you answer\n")
  cat("   q   save and stop                                 ?  show this again\n\n")
  cat("   Answering yes records your answer and nothing more. It does NOT add a survey\n")
  cat("   day to\n")
  cat("     data/project_info/surveys/master_per_survey_info_generated.csv\n")
  cat("   because that file is built only from photos that carry a survey tag. Your\n")
  cat("   answer is the record that somebody looked and decided.\n\n")
  cat("   Each one comes with a suggested answer. SUGGEST NO means nothing was found\n")
  cat("   near those dates anywhere; LOOK means a few photos were taken inside the park\n")
  cat("   around then, so it is worth pressing l first.\n")
  cat(bar, "\n", sep = "")
}

# "l" -- where to look before ruling on a window.
#
# The windows file has no obs_urls column: that column is written by
# resolve_beeple_transects_per_survey.R, into the TRANSECT-TIE file, not this one. So
# the blank branch here is not an edge case, it is every row -- and it used to print
# "(no observations near this window)" and stop, which is the opposite of the help's
# "press l first to look". Blank is also not a surprise: a window is being asked about
# BECAUSE nothing tagged was found near it. What the row does carry is who the
# surveyor is and which dates are in question, which is enough to go and look.
.rw_list_urls <- function(rv, i) {
  urls <- if ("obs_urls" %in% names(rv)) rv$obs_urls[i] else NA_character_
  if (.rw_blank(urls)) {
    cat("   No tagged survey by anyone was recorded near these dates -- that is why\n")
    cat("   this window is being asked about. Untagged photos can still exist, so to\n")
    cat(sprintf("   check for yourself, look for photos dated %s to %s:\n",
                rv$window_start[i], rv$window_end[i]))
    u <- if ("inat_username" %in% names(rv)) rv$inat_username[i] else NA_character_
    if (.rw_blank(u)) {
      cat(sprintf("     %s has no iNaturalist name on file -- add one in\n",
                  if ("first_name" %in% names(rv)) rv$first_name[i] else "This surveyor"))
      cat("     data/project_info/rosters/people_manual.csv\n")
    } else {
      cat("     https://www.inaturalist.org/people/", trimws(as.character(u)), "\n", sep = "")
    }
    return(invisible())
  }
  parts <- trimws(strsplit(as.character(urls), ";\\s*")[[1]]); parts <- parts[nzchar(parts)]
  cat(sprintf("   %d observation(s) near this window (the ones inside the park first):\n",
              length(parts)))
  for (u in parts) cat("     ", u, "\n")
}

#' Walk a human through records that fell outside any survey window
#'
#' @param path The review file, written by `finding_project_info()`.
#' @param prompt_fn Injection point for reading an answer.
#' @param write Save the answers.
#' @param max_items Stop after this many.
#' @return Invisibly, how many were resolved.
review_windows <- function(path = RW_PATH, prompt_fn = readline, write = TRUE, max_items = Inf) {
  if (!file.exists(path)) {
    message("Nothing to review: ", path)
    message("does not exist yet. It is written by the cleaning pipeline, so run that first:")
    message('  source("scripts/run_data_cleaning_pipeline.R")')
    return(invisible(NULL))
  }
  rv <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  if (!nrow(rv)) { message("Nothing to review -- every scheduled survey was accounted for.")
                   return(invisible(rv)) }
  if (!"decision" %in% names(rv))      rv$decision <- NA_character_
  if (!"decision_note" %in% names(rv)) rv$decision_note <- NA_character_

  todo <- which(.rw_todo(rv$decision))
  done <- nrow(rv) - length(todo)
  if (!length(todo)) { message(sprintf("All %d windows already ruled -- nothing to do.", nrow(rv))); return(invisible(rv)) }

  message(sprintf("%d scheduled survey%s to decide about (%d already answered).",
                  length(todo), if (length(todo) == 1L) "" else "s", done))
  message("Your answers are saved in ", path)
  .rw_help()
  changed <- FALSE
  n_show <- min(length(todo), max_items)
  for (k in seq_len(n_show)) {
    i <- todo[k]
    cat(sprintf("\n[%d/%d] %s (%s)\n", k, n_show,
                rv$first_name[i], if (.rw_blank(rv$inat_username[i])) "no iNat user" else rv$inat_username[i]))
    cat(sprintf("   scheduled to walk transect %s, %s to %s\n",
                rv$transect[i], rv$window_start[i], rv$window_end[i]))
    # The suggestion column already reads as a sentence; the raw review_reason is an
    # internal code ("no-survey-near") that means nothing to anyone but this script.
    if ("suggestion" %in% names(rv) && !.rw_blank(rv$suggestion[i]))
      cat(sprintf("   %s\n", rv$suggestion[i]))
    else
      cat(sprintf("   %s photo%s were taken inside the park near those dates.\n",
                  rv$n_obs_in_window[i], if (identical(rv$n_obs_in_window[i], 1L)) "" else "s"))
    cat("   Did this survey happen?\n")
    cat("     y=yes  n=no  u=unsure  s=skip  l=where to look  q=save and stop  ?=help\n")

    repeat {
      ans <- tolower(trimws(prompt_fn("> ")))
      if (ans == "?") { .rw_help(); next }
      if (ans == "l") { .rw_list_urls(rv, i); next }
      break
    }
    if (ans == "q") break
    if (ans %in% c("s", "")) next
    dec <- switch(ans, y = "survey", n = "no", u = "unsure", NA_character_)
    if (is.na(dec)) { cat("   ? didn't understand -- skipped\n"); next }
    rv$decision[i] <- dec
    changed <- TRUE
  }

  if (changed && write) {
    write.csv(rv, path, row.names = FALSE, na = "")
    cat(sprintf("\nSaved your answers -> %s\n", path))
    cat("These are a record that somebody looked and decided. They do not add survey\n")
    cat("days to data/project_info/surveys/master_per_survey_info_generated.csv, which\n")
    cat("is built only from photos carrying a survey tag.\n")
  } else cat("\nNo new rulings written.\n")
  invisible(rv)
}

# =============================================================
# review_transect_ties() -- rule equal-split transect days
# -------------------------------------------------------------
# resolve_beeple_transects_per_survey.R (called by the brain) resolves each beeple survey day to the
# transect the MAJORITY of that day's obs are tagged with. When it's an exact tie
# (e.g. TP:3 | UPMON:3 -- looks like two transects walked in one day) it does NOT
# guess; it writes the day to qc_review_survey_transect_overlap_generated.csv with the per-transect
# tag counts. This walks those ties so you can rule each one:
#   <TP|UPMON|...>  the whole day was really this ONE transect -> stamped on every obs,
#                   the other tag's obs go to qc_review_inat_mistagged_transects_generated.csv
#   b  both         a genuine two-transect day -> obs keep their own tags (stays split)
# Your ruling persists in the file's `decision` column and is applied on the next brain
# run; blank/unsure ties resurface, ruled ones don't.
# =============================================================
RTT_PATH <- "data/project_info/surveys/review/qc_review_survey_transect_overlap_generated.csv"

# a tie still needs a ruling if its decision is blank OR "unsure"
.rtt_todo <- function(dec) .rw_blank(dec) | tolower(trimws(as.character(dec))) == "unsure"

# "TP:3 | UPMON:3" -> c("TP","UPMON") : the transect codes the surveyor actually tagged
.rtt_options <- function(tag_counts) {
  parts <- trimws(strsplit(as.character(tag_counts), "\\|")[[1]])
  opts  <- toupper(trimws(sub(":.*$", "", parts)))
  opts[nzchar(opts)]
}

.rtt_help <- function() {
  bar <- strrep("-", 60)
  cat("\n", bar, "\n", sep = "")
  cat(" WHICH TRANSECT DID THEY WALK?\n\n")
  cat("   On this day the surveyor split their photos evenly between two transects, as\n")
  cat("   if they walked both. Usually they walked one and tagged some photos wrongly.\n")
  cat("   The counts below are exactly what they tagged.\n\n")
  cat("   TP        type a transect code to say that is the one they walked. The whole\n")
  cat("             day is recorded as that transect, and the photos tagged with the\n")
  cat("             other one are listed for a second look in\n")
  cat("             data/inat_observations/review/qc_review_inat_mistagged_transects_generated.csv\n")
  cat("   b  both   they really did walk both. Every photo keeps the transect it has.\n")
  cat("   u  unsure recorded as unsure; you are asked again next run\n")
  cat("   s  skip   nothing recorded; you are asked again next run\n")
  cat("   l  look   where to check before you answer\n")
  cat("   q  save and stop                                  ?  show this again\n")
  cat(bar, "\n", sep = "")
}

# print the obs URLs stored on a tie row, on demand ("l").
.rtt_list_urls <- function(tv, i) {
  urls <- if ("obs_urls" %in% names(tv)) tv$obs_urls[i] else NA_character_
  if (.rw_blank(urls)) { cat("   (no observation URLs recorded for this day)\n"); return(invisible()) }
  parts <- trimws(strsplit(as.character(urls), ";\\s*")[[1]]); parts <- parts[nzchar(parts)]
  cat(sprintf("   %d observation(s) on this tie day:\n", length(parts)))
  for (u in parts) cat("     ", u, "\n")
}

#' Walk a human through records that matched two transects equally well
#'
#' @param path The tie file, written by `finding_project_info()`.
#' @param prompt_fn Injection point for reading an answer.
#' @param write Save the answers.
#' @param max_items Stop after this many.
#' @return Invisibly, how many were ruled on. No tie file means no ties.
review_transect_ties <- function(path = RTT_PATH, prompt_fn = readline, write = TRUE, max_items = Inf) {
  if (!file.exists(path)) {
    message("Nothing to review here -- no survey day looks split between two transects.")
    message("(If you expected some, the file the cleaning pipeline writes is ", path, ")")
    return(invisible(NULL))
  }
  tv <- read_csv(path, show_col_types = FALSE, col_types = cols(.default = "c"))
  if (!nrow(tv)) { message("No transect ties to review -- every survey day had a clear majority."); return(invisible(tv)) }
  if (!"decision" %in% names(tv))      tv$decision <- NA_character_
  if (!"decision_note" %in% names(tv)) tv$decision_note <- NA_character_

  todo <- which(.rtt_todo(tv$decision))
  done <- nrow(tv) - length(todo)
  if (!length(todo)) { message(sprintf("All %d tie day(s) already ruled -- nothing to do.", nrow(tv))); return(invisible(tv)) }

  message(sprintf("%d equal-split transect day(s) to rule (%d already done).", length(todo), done))
  .rtt_help()
  changed <- FALSE
  n_show <- min(length(todo), max_items)
  for (k in seq_len(n_show)) {
    i <- todo[k]
    opts <- .rtt_options(tv$tag_counts[i])
    cat(sprintf("\n[%d/%d] %s  on %s\n", k, n_show, tv$inat_username[i], tv$date[i]))
    cat(sprintf("   they tagged their photos:  %s\n", tv$tag_counts[i]))
    cat("   Which transect did they really walk?\n")
    cat(sprintf("     type %s, or b=both if they genuinely walked both\n",
                paste(opts, collapse = " or ")))
    cat("     u=unsure  s=skip  l=where to look  q=save and stop  ?=help\n")

    action <- NULL   # resolves to: __quit__ / __skip__ / unsure / both / a transect code
    repeat {
      ans <- trimws(prompt_fn("> ")); low <- tolower(ans)
      if (low == "?") { .rtt_help(); next }
      if (low == "l") { .rtt_list_urls(tv, i); next }
      if (low == "q")            { action <- "__quit__"; break }
      if (low %in% c("s", ""))   { action <- "__skip__"; break }
      if (low == "u")            { action <- "unsure";   break }
      if (low %in% c("b", "both")) { action <- "both";   break }
      pick <- toupper(ans)
      if (pick %in% opts)        { action <- pick;       break }
      cat(sprintf("   ? '%s' isn't one of %s (or b/u/s/l/q) -- try again\n", ans, paste(opts, collapse = "/")))
    }
    if (identical(action, "__quit__")) break
    if (identical(action, "__skip__")) next
    if (identical(action, "both")) {
      tv$decision[i] <- "both"; tv$decision_note[i] <- "kept as a genuine two-transect day"
    } else if (identical(action, "unsure")) {
      tv$decision[i] <- "unsure"
    } else {
      tv$decision[i] <- action; tv$decision_note[i] <- sprintf("whole day ruled %s", action)
    }
    changed <- TRUE
  }

  if (changed && write) {
    write.csv(tv, path, row.names = FALSE, na = "")
    cat(sprintf("\nSaved your answers -> %s\n", path))
    cat("The next cleaning-pipeline run stamps each of those days with the transect you\n")
    cat("chose. If you are running this from inside the pipeline, that happens on its own.\n")
  } else cat("\nNo new rulings written.\n")
  invisible(tv)
}

# What to type, and what each question answers. "missing windows" and "equal-split
# days" named the code's own categories, so neither told you which one to run.
.rw_sourced_hint <- function() c(
  "Two things to review here. Run whichever you were sent for:",
  "  review_windows()         did a scheduled survey happen? (no photos were tagged",
  "                           for it, so somebody has to say)",
  "  review_transect_ties()   a surveyor tagged one day evenly across two transects.",
  "                           Which one did they really walk?")

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  for (ln in .rw_sourced_hint()) message(ln)
