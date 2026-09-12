# A generated REFERENCES_CITED.txt for the whole analysis folder. The folder notes
# carry a short citation; this is where the full reference lives, plus the software
# versions the numbers were actually produced with.
source(file.path("..", "..", "scripts", "analysis", "shared", "folder_readmes.R"))

test_that("every method source carries a full reference, not just a short form", {
  for (x in STAT_SOURCES) {
    expect_true(!is.null(x$full) && nzchar(x$full), info = x$src)
    expect_gt(nchar(x$full), nchar(x$src))
  }
})

test_that("a reference says whether it came from the package or from a person", {
  # anything marked verified was read out of the installed package's own Rd or
  # citation(); everything else is typed by hand and should be spot-checked
  for (x in STAT_SOURCES) expect_true(is.logical(x$verified), info = x$src)
  expect_true(any(vapply(STAT_SOURCES, function(x) isTRUE(x$verified), logical(1))))
  expect_true(any(vapply(STAT_SOURCES, function(x) !isTRUE(x$verified), logical(1))))
})

test_that("the file lists the software versions the numbers came from", {
  txt <- references_text()
  expect_match(txt, "vegan", fixed = TRUE)
  expect_match(txt, "iNEXT", fixed = TRUE)
  expect_match(txt, as.character(getRversion()), fixed = TRUE)
  expect_match(txt, as.character(utils::packageVersion("vegan")), fixed = TRUE)
})

test_that("the verified ones are marked, and so is the reason it matters", {
  txt <- references_text()
  expect_match(txt, "Almeida-Neto", fixed = TRUE)
  expect_match(txt, "Oikos", fixed = TRUE)             # a full journal reference
  expect_match(txt, "checked against", ignore.case = TRUE)
})

test_that("references are sorted by author, the way a reference list is read", {
  txt <- references_text()
  lines <- strsplit(txt, "\n")[[1]]
  # an entry starts at the margin, optionally behind the [pkg] marker; its
  # continuation lines are indented, which is what makes the list readable
  ent <- grep("^(\\[pkg\\] )?[A-Z][a-zA-Z-]+, ", lines, value = TRUE)
  who <- sub("^(\\[pkg\\] )?", "", ent)
  expect_gt(length(who), 8)
  expect_equal(who, sort(who))
})

test_that("no line wraps past a terminal width", {
  expect_lt(max(nchar(strsplit(references_text(), "\n")[[1]])), 90)
})

# REFERENCES_CITED.txt listed only statistical methods, and was missing two of
# those: Bray-Curtis (the dissimilarity behind EVERY PERMANOVA and NMDS result)
# and Patefield 1981 (the null the H2' p-value is tested against). A p-value with
# no stated null is not reportable.
#
# It also cited no DATA source at all -- not iNaturalist, whose observers produced
# every record, not the IUCN Red List, not the Holway checklist the whole county
# tier is built from, not the boundary layers. citations.R already knew how to
# write the first two; the references file simply never asked it.
test_that("the two missing methods are cited", {
  txt <- paste(references_text(), collapse = " ")
  expect_match(txt, "Bray", fixed = TRUE)
  expect_match(txt, "Patefield", fixed = TRUE)
})

test_that("the data sources are cited, not just the statistics", {
  txt <- paste(references_text(), collapse = " ")
  for (src in c("iNaturalist", "IUCN", "Holway", "National Park Service"))
    expect_match(txt, src, fixed = TRUE, info = src)
})

# Every other data source in the list is reachable -- iNaturalist, IUCN and ITIS
# all carry a URL. The checklist the whole county tier is built from did not, so
# a reader was told the name of a document and left to go find it. It is also a
# nine-author work: citing it as "Holway, D.A." alone drops eight of them.
test_that("the checklist is cited in full, with its DOI", {
  txt <- paste(references_text(), collapse = " ")
  expect_match(txt, "https://doi.org/10.6075/J0NZ88MD", fixed = TRUE)
  expect_match(txt, "Hung", fixed = TRUE)          # first author
  expect_match(txt, "Holway", fixed = TRUE)        # last author, and what we call it
  expect_match(txt, "Version 3", fixed = TRUE)
})

test_that("the file separates data sources from methods", {
  txt <- paste(references_text(), collapse = "\n")
  expect_match(txt, "DATA SOURCES", fixed = TRUE)
  expect_match(txt, "METHODS", fixed = TRUE)
  expect_lt(regexpr("DATA SOURCES", txt, fixed = TRUE),
            regexpr("METHODS", txt, fixed = TRUE))   # the data comes first
})
