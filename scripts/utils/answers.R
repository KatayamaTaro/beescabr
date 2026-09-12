# =============================================================
# utils/answers.R
# beescabr pipeline -- one vocabulary for every interactive prompt.
#
# The pipeline asks 31 questions across 13 files, and each one grew its own answer
# parser. Most already accept synonyms nobody is ever told about -- "yes", "stop",
# "halt", "none", "noid" -- while a few compare the raw string, so "None" or "Y" is
# not recognized at all and silently becomes something else. A supervisor running
# this for the first time should be able to type the obvious word and have it work,
# at every prompt, in any capitalization.
#
#   answer_is(raw, "skip")            TRUE for s, skip, later, pass, next
#   answer_is(raw, c("skip", "stop")) TRUE for either
#   answer_words("skip")              "s / skip / later / pass / next", to print
#
# Blank (bare Enter) is deliberately NOT a word here. It means different things at
# different prompts -- accept the suggestion, skip, continue -- so each caller tests
# it itself rather than inheriting a guess.
# =============================================================

# The words, per choice. A word may appear under more than one choice: "n" is *no*
# at a yes/no gate and *none* ("iNaturalist has no page for this bee") at a taxon
# prompt. That is why the caller names the choices it is offering instead of asking
# what a word means in the abstract -- a gate that offers yes/no never sees the
# taxon meaning, so the two cannot collide.
#
# The first entry of each is the single letter the prompts print; the rest are the
# spellings an operator reaches for without being told.
ANSWER_WORDS <- list(
  yes    = c("y", "yes", "yeah", "yep", "yup", "ok", "okay", "sure"),
  no     = c("n", "no", "nope", "nah"),
  skip   = c("s", "skip", "later", "pass", "next"),
  none   = c("n", "no", "none", "noid", "no id", "no page", "not found", "nothing"),
  quit   = c("q", "quit", "exit", "done", "save and quit", "save & quit"),
  stop   = c("x", "stop", "halt", "abort", "cancel", "fix"),
  help   = c("?", "h", "help", "keys"),
  list   = c("l", "list", "links", "show"),
  add    = c("a", "add", "new"),
  both   = c("b", "both"),
  unsure = c("u", "unsure", "not sure", "dunno", "idk", "maybe")
)

#' Normalize a typed answer
#'
#' Lowercase, trimmed, and with runs of whitespace collapsed, so "Not  Sure" and
#' "not sure" are the same answer. Punctuation is left alone on purpose: "?" is
#' itself a valid answer at every prompt that offers help.
#'
#' @param raw What the operator typed.
#' @return A single lowercase string; "" for NULL, NA, or whitespace.
answer_norm <- function(raw) {
  a <- suppressWarnings(as.character(raw)[1])
  if (is.null(a) || length(a) == 0L || is.na(a)) return("")
  gsub("[[:space:]]+", " ", trimws(tolower(a)))
}

#' Does this answer mean one of these choices?
#'
#' @param raw What the operator typed.
#' @param meaning One or more names from `ANSWER_WORDS` -- the choices THIS prompt
#'   is offering. Naming them is what keeps "n" from meaning two things at once.
#' @return TRUE if the answer is any spelling of any of those choices. Blank is
#'   always FALSE; the caller decides what Enter does.
answer_is <- function(raw, meaning) {
  bad <- setdiff(meaning, names(ANSWER_WORDS))
  if (length(bad))
    stop("answer_is(): no such choice: ", paste(bad, collapse = ", "),
         ". Known: ", paste(names(ANSWER_WORDS), collapse = ", "), call. = FALSE)
  a <- answer_norm(raw)
  if (!nzchar(a)) return(FALSE)
  any(vapply(meaning, function(m) a %in% ANSWER_WORDS[[m]], logical(1)))
}

#' The spellings of a choice, for printing in a help line
#'
#' @param meaning One name from `ANSWER_WORDS`.
#' @return The words, slash-separated, shortest first.
answer_words <- function(meaning) paste(ANSWER_WORDS[[meaning]], collapse = " / ")
