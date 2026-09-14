# =============================================================
# reference/taxonomy/holway_version.R
# beescabr -- "is there a newer Holway checklist, and what changed in it?"
#
# The checklist version is written into config.R in two places (the source CSV and the
# generated reference table), so a v4 file dropped into data/reference/source/ does
# NOTHING: the pipeline keeps reading v3 and never mentions it. Whoever inherits this
# project has no way to find that out except by reading config.R.
#
# These are pure. The pipeline uses them to say, before it starts asking questions,
# that the checklist itself has changed and roughly what that means for the answers
# already on file.
# =============================================================

HOLWAY_SOURCE_DIR <- "data/reference/source"
.HOLWAY_FILE_RE   <- "holway_v([0-9]+)_combined\\.csv$"

#' The version number in a Holway source filename
#'
#' Only the combined CSV the pipeline actually reads is matched. The .xlsx and the
#' README that sit beside it carry a version in their names too, and treating those as
#' the source would point the pipeline at a file it cannot parse.
#'
#' @param path A file path.
#' @return The version as an integer, or NA if the name is not a combined checklist.
holway_version_of <- function(path) {
  m <- regmatches(basename(path), regexec(.HOLWAY_FILE_RE, basename(path)))[[1]]
  if (length(m) < 2) return(NA_integer_) else as.integer(m[2])
}

#' The newest combined checklist among some files
#'
#' @param files Paths to consider.
#' @return A list of `path` and `version`, or NULL when none of them is a checklist.
holway_newest_source <- function(files) {
  v <- vapply(files, holway_version_of, integer(1))
  ok <- !is.na(v)
  if (!any(ok)) return(NULL)
  i <- which(ok)[which.max(v[ok])]
  list(path = unname(files[i]), version = unname(v[i]))
}

#' Is there a checklist newer than the one the pipeline is configured to read?
#'
#' @param files Paths to consider.
#' @param configured The path config.R points at.
#' @return The newer one as `holway_newest_source()` returns it, or NULL.
holway_newer_than <- function(files, configured) {
  newest <- holway_newest_source(files)
  if (is.null(newest)) return(NULL)
  cur <- holway_version_of(configured)
  if (!is.na(cur) && newest$version <= cur) return(NULL)
  newest
}

#' Say that a newer checklist is sitting on disk, and what to do about it
#'
#' Naming both config keys is the point. The version lives in two places -- the source
#' CSV and the generated reference table -- and editing only one leaves the pipeline
#' reading a v4 sheet while writing a file still called v3, which is worse than not
#' upgrading at all.
#'
#' @param newer What `holway_newer_than()` returned, or NULL.
#' @param configured The path config.R currently points at.
#' @return Lines to print; empty when there is nothing newer.
holway_version_notice <- function(newer, configured) {
  if (is.null(newer)) return(character(0))
  cur <- holway_version_of(configured)
  c("",
    sprintf("  A NEWER CHECKLIST IS ON DISK -- v%d. This run is still using v%s.",
            newer$version, if (is.na(cur)) "?" else as.character(cur)),
    sprintf("    found:  %s", newer$path),
    sprintf("    using:  %s", configured),
    "",
    "  The version is not picked up automatically, because moving to a new checklist",
    "  changes which bees the whole county tier is built from. Two lines in",
    "  scripts/config.R point at it, and BOTH have to change together:",
    sprintf("    PATHS$holway_combined   -> %s", newer$path),
    sprintf("    PATHS$holway_reference  -> ...reference_table_v%d_generated.csv", newer$version),
    "",
    "  Editing only one leaves the pipeline reading the new sheet and writing a file",
    "  still named for the old one.")
}

#' Announce that a new checklist version has cleared the saved answers
#'
#' The loudest thing the pipeline prints, and it has to carry one fact beyond the
#' numbers: the questions that follow are TAXONOMIC judgements, not clerical ones.
#' The rebuild menu already warns "this needs BEE EXPERTISE, not just patience" --
#' the same warning belongs here, because a version bump reaches the same prompts
#' without anyone having chosen them from a menu.
#'
#' @param was The version the answers were made against.
#' @param now The version being read.
#' @param n_gone How many answers were cleared.
#' @param backup Where they were written first; NA if that failed.
#' @return Lines to print.
holway_version_bump_notice <- function(was, now, n_gone, backup) {
  c("",
    sprintf("  NEW CHECKLIST VERSION -- v%s to v%s.", was, now),
    "",
    sprintf("    Every saved answer was made against v%s, and a name surviving into", was),
    sprintf("    v%s does not prove the bee did: a checklist can keep a spelling and", now),
    sprintf("    change which bee it means. So all %s answers were cleared and every", n_gone),
    sprintf("    name is being resolved against v%s from scratch.", now),
    "",
    "    THIS NEEDS BEE EXPERTISE, not just patience.",
    "",
    "    About 40 minutes of searching, then questions about the names iNaturalist",
    "    cannot settle on its own -- a couple of dozen, not hundreds. Each one asks",
    "    whether a checklist name and an iNaturalist taxon are the same bee. You",
    "    answer it by reading two pages, both linked for you at the prompt:",
    "      iNaturalist  -- is this bee published there, and under what name",
    "      ITIS         -- the US government taxonomy database: is the name still",
    "                      accepted, and what was it renamed to",
    "",
    "    A wrong answer is worse than no answer: it silently files one bee's records",
    "    under another. Every prompt offers a skip, and skipping costs nothing.",
    "",
    if (is.na(backup))
      "    WARNING: the old answers could NOT be written out first."
    else
      "    Your old answers were saved first, nothing is lost:",
    paste0("      ", if (is.na(backup)) "(no backup file)" else backup),
    "")
}
