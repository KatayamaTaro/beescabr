# The last handoff shipped data/secrets/ by accident -- the folder was simply
# forgotten. Deleting keys by hand at the moment of transfer is exactly the step a
# person skips when they are in a hurry, so this makes the copy do it, and then
# CHECKS the copy rather than trusting that it did.
src("utils/handoff_copy.R")

.tree <- function() {
  root <- file.path(tempdir(), paste0("d", as.integer(runif(1, 1e6, 9e6))))
  for (d in c("secrets", "specimens", "project_info/rosters"))
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
  writeLines("INAT_CLIENT_SECRET=shh", file.path(root, "secrets", "inat_api.env"))
  writeLines("IUCN=shh",               file.path(root, "secrets", "iucn_api.env"))
  writeLines("a,b",                    file.path(root, "specimens", "rec.csv"))
  writeLines("name",                   file.path(root, "project_info/rosters", "people_manual.csv"))
  writeLines("junk",                   file.path(root, "specimens", ".DS_Store"))
  root
}

test_that("the copy carries the data but not the secrets", {
  src_dir <- .tree(); dest <- file.path(tempdir(), "out1")
  on.exit(unlink(c(src_dir, dest), recursive = TRUE), add = TRUE)
  res <- handoff_copy(src_dir, dest, verbose = FALSE)

  expect_true(file.exists(file.path(dest, "specimens", "rec.csv")))
  expect_true(file.exists(file.path(dest, "project_info/rosters", "people_manual.csv")))
  expect_false(dir.exists(file.path(dest, "secrets")))
  expect_equal(res$secrets_found, 0L)
})

test_that(".DS_Store files are left behind too", {
  src_dir <- .tree(); dest <- file.path(tempdir(), "out2")
  on.exit(unlink(c(src_dir, dest), recursive = TRUE), add = TRUE)
  handoff_copy(src_dir, dest, verbose = FALSE)
  expect_equal(length(list.files(dest, pattern = "^[.]DS_Store$",
                                 recursive = TRUE, all.files = TRUE)), 0L)
})

test_that("it refuses to write inside the repo", {
  src_dir <- .tree(); on.exit(unlink(src_dir, recursive = TRUE), add = TRUE)
  expect_error(handoff_copy(src_dir, file.path(.beescabr_root(), "handoff"),
                            verbose = FALSE), "inside the repo")
})

test_that("it refuses a destination that already has files", {
  src_dir <- .tree(); dest <- file.path(tempdir(), "out3")
  dir.create(dest, showWarnings = FALSE); writeLines("x", file.path(dest, "old.csv"))
  on.exit(unlink(c(src_dir, dest), recursive = TRUE), add = TRUE)
  expect_error(handoff_copy(src_dir, dest, verbose = FALSE), "not empty")
})
