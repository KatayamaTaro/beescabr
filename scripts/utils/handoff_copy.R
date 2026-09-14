# =============================================================
# utils/handoff_copy.R -- make a copy of data/ that is safe to hand over.
#
# The last handoff shipped data/secrets/ by accident: the folder was forgotten in
# the moment of copying, and the recipient's pipeline then ran on somebody else's
# API key. Deleting it by hand is the step a person skips when they are in a
# hurry, so the copy leaves it behind and then CHECKS that it did.
#
#   source("scripts/utils/handoff_copy.R")
#   handoff_copy("data", "~/Desktop/beescabr_data_for_taro")
#
# Nothing in your own data/ is touched. This only writes the copy.
# =============================================================

# Never copied: keys are personal, and .DS_Store is Finder noise that regenerates.
HANDOFF_EXCLUDE_DIRS  <- c("secrets")
HANDOFF_EXCLUDE_FILES <- c(".DS_Store")

#' Copy a data folder for handover, without the credentials
#'
#' @param from The data folder to copy.
#' @param to Destination. Must be outside the repository and empty.
#' @param verbose Print what was copied and what was left behind.
#' @return Invisibly, a list of `files` copied, `skipped`, and `secrets_found`
#'   (a count of credential files that reached the copy -- must be zero).
handoff_copy <- function(from = "data", to, verbose = TRUE) {
  if (!dir.exists(from)) stop("no such folder: ", from, call. = FALSE)

  # A copy inside the repo would sit next to the gitignored original with no
  # ignore rule of its own, which is how a secret reaches a commit.
  # Find the repo by its marker rather than by the working directory: this is
  # called from the console (root) and from tests (tests/testthat), and getting
  # the root wrong would let the check pass on a destination inside the repo.
  dest <- normalizePath(to, mustWork = FALSE)
  d <- dest
  repeat {
    if (file.exists(file.path(d, "beescabr.Rproj")))
      stop("refusing: ", to, " is inside the repo. Copy somewhere else -- a folder ",
           "here is not covered by .gitignore.", call. = FALSE)
    parent <- dirname(d)
    if (parent == d) break
    d <- parent
  }
  if (dir.exists(to) && length(list.files(to, all.files = TRUE, no.. = TRUE)))
    stop("refusing: ", to, " is not empty. Give an empty or new folder.", call. = FALSE)

  all_files <- list.files(from, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  parts <- strsplit(all_files, .Platform$file.sep)
  drop  <- vapply(seq_along(all_files), function(i)
    any(parts[[i]] %in% HANDOFF_EXCLUDE_DIRS) ||
    basename(all_files[i]) %in% HANDOFF_EXCLUDE_FILES, logical(1))
  keep <- all_files[!drop]

  for (f in keep) {
    out <- file.path(to, f)
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(from, f), out, overwrite = FALSE, copy.date = TRUE)
  }

  # Verify rather than trust: check the COPY, not the exclusion list.
  landed <- list.files(to, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  secrets_found <- sum(grepl("(^|/)secrets(/|$)", landed) | grepl("[.]env$", landed))

  if (verbose) {
    message("")
    message("  Copied ", length(keep), " files to ", to)
    message("  Left behind ", sum(drop), ": ", paste(HANDOFF_EXCLUDE_DIRS, collapse = ", "),
            " and ", paste(HANDOFF_EXCLUDE_FILES, collapse = ", "))
    message("")
    if (secrets_found)
      message("  !!! ", secrets_found, " credential file(s) reached the copy. DELETE THEM before sending.")
    else
      message("  No credential file reached the copy. Safe to hand over.")
    message("")
    message("  The recipient makes their own OAuth app under the park's iNaturalist")
    message("  account; the pipeline asks for the values on their first run.")
  }
  invisible(list(files = keep, skipped = all_files[drop], secrets_found = as.integer(secrets_found)))
}
