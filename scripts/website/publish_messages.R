# =============================================================
# website/publish_messages.R
# beescabr -- what stage 3 says while it builds and publishes the public site.
#
# Stage 3 is the only thing in this project that makes anything public, and its own
# words had that backwards: "Publishing into docs/" merely copies files into a local
# folder, while "Deploying" -- the step that actually puts the park's site on the
# internet -- said nothing about being public and never showed the URL.
#
# Kept short on purpose. This stage runs for several minutes and nobody reads a
# paragraph in the middle of it.
# =============================================================

#' Copying the built pages into docs/ -- local, not public
#' @return One line.
.pub_copying <- function() "Copying the finished pages into docs/ on this computer."

#' Pushing docs/ to GitHub Pages -- this IS public
#' @param url Where the site will be.
#' @return Lines to print.
.pub_deploying <- function(url) c(
  "Putting the site on the internet. This is the public one:",
  paste0("  ", url),
  "  Live in about a minute.")

#' A page script that could not be located
#' @param nm The script name.
#' @param found How many files matched.
#' @return Lines to print.
.pub_page_missing <- function(nm, found) {
  if (found == 0L) c(
    paste0("  ", nm, " is not under scripts/analysis/, so its page cannot be rebuilt."),
    "  Either it was renamed, or PUBLIC_PAGES lists a name that no longer exists.")
  else c(
    paste0("  two files are named ", nm, " under scripts/analysis/, so it is not clear"),
    "  which one builds the page. Rename one of them.")
}

#' A page script that errored
#' @param nm The script name.
#' @param err The R error.
#' @return Lines to print.
.pub_page_failed <- function(nm, err) c(
  paste0("  ", nm, " stopped with an error, so its page was not rebuilt:"),
  paste0("    ", err))

#' The hard stop before anything is copied
#' @param failed Scripts that did not rebuild.
#' @return Lines to print.
.pub_stop <- function(failed) c(
  paste0("These pages did not rebuild: ", paste(failed, collapse = ", ")),
  "Nothing was copied, so docs/ and the live site are unchanged.",
  "Fix the errors above, then run this stage again.")

#' What to do once docs/ is built
#' @return Lines to print.
.pub_next_steps <- function() c(
  "Built, but not public yet. Look at docs/ first.",
  "To put it on the internet, run this stage again with:",
  '  Sys.setenv(BEESCABR_DEPLOY = "1"); source("scripts/run_publishing_materials_pipeline.R")')
