library(testthat)

# 31 interactive prompts across 13 files, and every one grew its own answer parser.
# Most already accept synonyms nobody is told about ("yes", "stop", "none", "halt")
# while a few compare the raw string, so "None" or "Y" is not recognized at all. One
# vocabulary, so a word that works at one prompt works at every prompt that offers
# the same choice.
src("utils/answers.R")

test_that("capitalization and stray spaces never matter", {
  for (raw in c("y", "Y", "YES", " yes ", "\tYes\t", "yes"))
    expect_true(answer_is(raw, "yes"), info = raw)
  for (raw in c("none", "None", "NONE", "  none  "))
    expect_true(answer_is(raw, "none"), info = raw)
})

test_that("a multi-word answer is matched however it is spaced", {
  for (raw in c("not sure", "Not  Sure", " not   sure "))
    expect_true(answer_is(raw, "unsure"), info = raw)
})

# "n" is the collision: at a yes/no gate it means no, and at a taxon prompt it means
# "iNaturalist has no page for this bee". One global word->meaning map cannot express
# that, so the CALLER says which choices it is offering and the word is read in that
# context. A gate asks about yes/no and never sees the taxon meaning.
test_that("n reads as no at a gate and as none at a taxon prompt", {
  expect_true(answer_is("n", "no"))
  expect_true(answer_is("n", "none"))
  expect_false(answer_is("n", "yes"))
  expect_false(answer_is("n", "skip"))
})

test_that("several choices can be tested at once", {
  expect_true(answer_is("halt", c("skip", "stop")))
  expect_false(answer_is("halt", c("skip", "quit")))
})

test_that("blank is never a word -- callers decide what Enter means", {
  for (raw in c("", "   ", NA, NULL))
    for (m in names(ANSWER_WORDS))
      expect_false(answer_is(raw, m), info = m)
})

test_that("an unrecognized answer matches nothing", {
  expect_false(answer_is("banana", "yes"))
  expect_false(answer_is("345235", "none"))
})

test_that("a misspelled choice name is an error, not a silent FALSE", {
  expect_error(answer_is("y", "yess"), "yess")
})

test_that("every choice offers the single letter the prompts print", {
  for (m in c("yes", "no", "skip", "quit", "stop", "help", "list", "add", "both", "unsure"))
    expect_true(any(nchar(ANSWER_WORDS[[m]]) == 1L), info = m)
})

test_that("the words for a choice can be shown to the operator", {
  expect_match(answer_words("skip"), "skip", fixed = TRUE)
  expect_match(answer_words("skip"), "s", fixed = TRUE)
})
