# dev-docs/WHAT_THESE_FILES_ARE.txt is the index of that folder. It drifted twice
# in one session: it still listed VERIFICATION.md and SCRIPTS_GUIDE.md after both
# were deleted, and it did not mention PACKAGES.md after it was added. An index
# that lies is worse than no index -- someone goes looking for a file that is gone.
.root <- .beescabr_root()
.note <- file.path(.root, "dev-docs", "WHAT_THESE_FILES_ARE.txt")

test_that("every file in dev-docs is named in the note", {
  txt <- paste(readLines(.note, warn = FALSE), collapse = "\n")
  files <- setdiff(basename(list.files(file.path(.root, "dev-docs"))),
                   "WHAT_THESE_FILES_ARE.txt")
  missing <- files[!vapply(files, function(f) grepl(f, txt, fixed = TRUE), logical(1))]
  expect_equal(sort(missing), character(0),
               info = paste("in dev-docs but not in the note:", paste(missing, collapse = ", ")))
})

test_that("every file the note names still exists", {
  txt <- readLines(.note, warn = FALSE)
  named <- unique(unlist(regmatches(txt, gregexpr("[A-Z_]+[.](md|png)", txt))))
  gone <- named[!file.exists(file.path(.root, "dev-docs", named))]
  expect_equal(sort(gone), character(0),
               info = paste("named in the note but deleted:", paste(gone, collapse = ", ")))
})
