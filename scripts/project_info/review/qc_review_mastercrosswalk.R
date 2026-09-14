# =============================================================
# project_info/review/qc_review_mastercrosswalk.R
# beescabr -- interactive review of unknown tags & fields
# Rewritten 2026-07-14 for the concept-per-row master_crosswalk_manual.csv.
# Auto-suggest + multi-file + help block added 2026-07-15.
#
# Feels like the bee-name resolver. It shows each unknown tag/field one at a
# time, PRE-FILLED with its best-guess concept, and you confirm or redirect:
#
#   <Enter>   accept the suggested concept (shown with a *)
#   <number>  file it into that concept instead
#   <n1,n2>   file into MORE THAN ONE concept (the "2 meanings" case)
#   i         ignore (catch-all "ignore" row) -- use for junk with no value
#   n         brand-new concept (asks name + what_for)
#   s         skip this one       q  save & quit       ?  show help again
#
# The list is sorted Cabrillo-ish first, then noise -- so once you hit the junk
# you can just press q and you're done (no grinding through all of them).
#
# Filing appends the item to that concept's variant cell (deduped):
#   a tag   -> inat_tag_variants     a field -> inat_field_variants
# After you finish, re-run finding_project_info() -- everything you filed drops
# out of the unknown reports because it's now recognized in master_crosswalk_manual.csv.
#
# Run: source("scripts/project_info/review/qc_review_mastercrosswalk.R"); review_unknowns("tags")
#      then                                        review_unknowns("fields")
# =============================================================

library(dplyr); library(readr); library(stringr)

CW_PATH    <- "data/project_info/crosswalk/master_crosswalk_manual.csv"
UNK_TAGS   <- "data/project_info/crosswalk/review/qc_review_mastercrosswalk_inat_unknown_tags_generated.csv"
UNK_FIELDS <- "data/project_info/crosswalk/review/qc_review_mastercrosswalk_inat_unknown_fields_generated.csv"

# paren-aware split: don't break on ";" inside "(...)" -- some field NAMES embed
# their allowed values, e.g. "soil type (sandy; loam; clay)" is ONE variant.
.split <- function(s) {
  if (is.na(s) || s == "") return(character(0))
  out <- character(0); buf <- ""; depth <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    if (ch == "(") depth <- depth + 1L
    else if (ch == ")") depth <- if (depth > 0L) depth - 1L else 0L
    if (depth == 0L && ch == ";") { out <- c(out, buf); buf <- "" } else buf <- paste0(buf, ch)
  }
  out <- str_trim(c(out, buf)); out[nzchar(out)]
}
.join  <- function(v) { v <- unique(v[!is.na(v) & v != "" & !(tolower(v) %in% c("n/a", "na"))])
                        if (length(v)) paste(v, collapse = "; ") else NA_character_ }

# append one value to row i's column `col`, deduped
cw_append <- function(cw, i, col, val) {
  cw[[col]][i] <- .join(c(.split(cw[[col]][i]), val)); cw
}

# ------------------------------------------------------------
# Auto-suggest: score an unknown item against every concept and return the best
# guess. Word-ANCHORED (a keyword matches a token that STARTS WITH it, so
# "visit" -> "visited"; short codes tp/ot/bst match EXACTLY, so "tpsnr" != "tp").
# Keywords per concept = the concept name + the seed hints below + whatever's
# already filed into it (so suggestions sharpen as you go).
# ------------------------------------------------------------
.RK_STOP <- c("bee","on","in","of","the","a","to","with","for","or","and","by",
              "is","was","survey","cabrillo","cabr","de","por","con")
# Seed keywords per concept -- EDIT THESE to tune the guesses. Anchored on the
# field groupings config.R already uses (FLOWER_VISITED_SOURCES / NESTING_SOURCES).
RK_HINTS <- list(
  flower_visited         = c("flower","floral","nectar","plant","forage","visit","host","associated","bloom","proboscis","pollinat","foraging"),
  flower_flowering       = c("flowering","bloom","phenolog","inflorescence","bud"),
  bee_on_ground          = c("ground","soil","dirt","sand","substrate"),
  bee_nest               = c("nest","burrow","cavity","tunnel","brood","construction"),
  bee_in_nest            = c("nesting"),
  bee_on_flower          = c("flower"),
  cabr_bee_meta_data     = c("weather","wind","temp","time","humidity","cloud","metadata","habitat","elevation"),
  cabr_bee_10_min_survey = c("minute","10 min","10-min")
)
.rk_toks <- function(s) { t <- str_split(tolower(s), "[^a-z0-9]+")[[1]]; t[nchar(t) > 1] }
.rk_hits <- function(item, words) {
  if (!length(words)) return(0L)
  ft <- .rk_toks(item); fs <- tolower(item)
  sum(vapply(words, function(w)
    if (grepl("[ -]", w)) grepl(w, fs, fixed = TRUE)   # phrase ("10 min") -> substring
    else if (nchar(w) <= 3) w %in% ft                  # short code (tp/ot/bst) -> exact token, so "tpsnr" != "tp"
    else any(startsWith(ft, w)),                       # longer -> prefix, so "visit" -> "visited"
    logical(1)))
}
rk_keywords <- function(cw) setNames(lapply(seq_len(nrow(cw)), function(i) {
  nm <- cw$name[i]
  name_toks <- setdiff(.rk_toks(nm), .RK_STOP)
  fvar <- if ("inat_field_variants" %in% names(cw) && !is.na(cw$inat_field_variants[i]))
            tolower(str_split(cw$inat_field_variants[i], "[;,]\\s*")[[1]]) else character(0)
  tvar <- if ("inat_tag_variants" %in% names(cw) && !is.na(cw$inat_tag_variants[i]))
            tolower(str_split(cw$inat_tag_variants[i], "[;,]\\s*")[[1]]) else character(0)
  unique(c(name_toks, RK_HINTS[[nm]], fvar, tvar))
}), cw$name)
rk_suggest <- function(item, kw, valid) {
  sc <- vapply(names(kw), function(nm) if (nm %in% valid) .rk_hits(item, kw[[nm]]) else 0L, integer(1))
  b <- which.max(sc)
  if (!length(b) || sc[b] == 0) return(NA_character_)
  names(kw)[b]
}

# make sure an "ignore" catch-all row exists; return list(cw, i)
cw_ensure_ignore <- function(cw) {
  i <- which(cw$name == "ignore")
  if (length(i)) return(list(cw = cw, i = i[1]))
  new <- cw[1, ]; new[] <- NA
  new$name <- "ignore"; new$what_for <- "ignore"
  new$applies_to_plant_bee_or_both <- "both"; new$nonlethal_or_lethal <- "both"
  new$notes <- "catch-all: tags/fields reviewed and dismissed as not relevant"
  cw <- bind_rows(cw, new)
  list(cw = cw, i = nrow(cw))
}

# find-or-create a concept row by name; returns list(cw, i)
cw_get_or_add <- function(cw, name, what_for = NA) {
  i <- which(cw$name == name)
  if (length(i)) return(list(cw = cw, i = i[1]))
  new <- cw[1, ]; new[] <- NA
  new$name <- name; new$what_for <- what_for
  if ("applies_to_plant_bee_or_both" %in% names(new)) new$applies_to_plant_bee_or_both <- "both"
  cw <- bind_rows(cw, new)
  list(cw = cw, i = nrow(cw))
}

# One-time (and on-demand via "?") help block, in plain language.
.rk_help <- function(kind) {
  bar <- strrep("─", 60)
  cat("\n", bar, "\n", sep = "")
  cat(sprintf(" Reviewing unknown %s\n\n", toupper(kind)))
  if (kind == "fields") {
    cat(" An observation FIELD is a structured answer somebody attached to an\n")
    cat(" observation on iNaturalist -- \"Host plant\", \"Sex\", that sort of thing.\n")
    cat(" These are ones this project has never seen, so the pipeline does not know\n")
    cat(" whether they matter.\n\n")
    cat(" Not sure what one is? Search it here:\n")
    cat("   https://www.inaturalist.org/observation_fields?utf8=%E2%9C%93&q=\n\n")
  } else {
    cat(" A TAG is free text somebody typed on an observation -- a survey hashtag,\n")
    cat(" but also camera models, place names and species names. These are ones this\n")
    cat(" project has never seen. Most are harmless; you are looking for a real\n")
    cat(" project tag that was mistyped.\n\n")
  }
  cat(" Your answer decides what happens to it:\n\n")
  cat("   Enter    accept the highlighted (*) guess -- if there is no *, this skips\n")
  cat("   3        file under a concept, e.g. 3 for concept #3 -- the pipeline reads it\n")
  cat("   3,5      file under two concepts at once\n")
  cat("   i        ignore it. THE OBSERVATION IS KEPT, only this ")
  cat(if (kind == "fields") "field" else "tag")
  cat(" is skipped\n")
  cat("   n        make a brand-new concept, if none of the above fit\n")
  cat("   s        skip for now -- it comes back next run\n")
  cat("   q        save everything and quit        ?  = show this help again\n\n")
  cat(" Nothing here is dropped or deleted. A whole observation is only dropped when a\n")
  cat(" tag is marked \"exclude\" in the crosswalk by hand, which this reviewer never does.\n\n")
  cat(" Sorted Cabrillo-first, so once you hit random junk just press q.\n")
  cat(bar, "\n", sep = "")
}

#' Walk a human through unrecognised tags or observation fields
#'
#' @param kind `"tags"` or `"fields"`.
#' @param cw_path The crosswalk, which this updates.
#' @param prompt_fn Injection point for reading an answer.
#' @param write Save the answers.
#' @param max_items Stop after this many, so a long queue can be done in sittings.
#' @return Invisibly, how many were resolved.
review_unknowns <- function(kind = c("tags", "fields"), cw_path = CW_PATH,
                            prompt_fn = readline, write = TRUE, max_items = Inf) {
  kind <- match.arg(kind)
  cw <- read_csv(cw_path, show_col_types = FALSE, col_types = cols(.default = "c"))
  cw <- cw[!(is.na(cw$name) & is.na(cw$what_for)), ]        # drop blank rows

  unk <- read_csv(if (kind == "tags") UNK_TAGS else UNK_FIELDS, show_col_types = FALSE)
  item_col   <- if (kind == "tags") "tag" else "field"
  target_col <- if (kind == "tags") "inat_tag_variants" else "inat_field_variants"
  if ("cabrillo_ish" %in% names(unk)) unk <- arrange(unk, desc(cabrillo_ish), desc(n_obs))
  else if ("n_obs" %in% names(unk))   unk <- arrange(unk, desc(n_obs))
  unk <- head(unk, max_items)
  if (!nrow(unk)) { message("Nothing to review for ", kind, "."); return(invisible(cw)) }

  cw <- cw_ensure_ignore(cw)$cw
  items <- unk[[item_col]]

  message(sprintf("%d unknown %s to review.", length(items), kind))
  .rk_help(kind)
  changed <- FALSE
  for (k in seq_along(items)) {
    it <- items[k]

    # (re)compute the concept list, valid suggestion targets, and keyword map --
    # cheap, and stays correct after n adds a new row.
    concept_i <- which(!is.na(cw$name) & cw$name != "")
    nm <- cw$name[concept_i]
    valid <- if (kind == "fields")                       # a field NAME isn't a specific transect
      nm[!(tolower(cw$what_for[concept_i]) %in% "transect")] else nm
    kw   <- rk_keywords(cw[concept_i, , drop = FALSE])
    sugg <- rk_suggest(it, kw, valid)

    n_obs <- if ("n_obs" %in% names(unk)) unk$n_obs[k] else NA
    cat(sprintf("\n[%d/%d] %s: \"%s\"%s\n", k, length(items), sub("s$", "", kind), it,
                if (!is.na(n_obs)) sprintf("  (%s obs)", n_obs) else ""))
    if (!is.na(sugg)) cat(sprintf("  suggestion: %s   [Enter = accept]\n", sugg))
    labs <- sprintf("%d=%s%s", seq_along(nm), nm, ifelse(!is.na(sugg) & nm == sugg, "*", ""))
    cat("  file into:  ", paste(labs, collapse = "   "), "\n")
    cat("  <Enter>=accept*   n1,n2=multi   i=ignore   n=new   s=skip   q=save & quit   ?=help\n")

    repeat { ans <- trimws(prompt_fn("> ")); low <- tolower(ans)   # ? re-shows help, then re-asks
             if (low != "?") break
             .rk_help(kind) }

    if (low == "q") break
    if (low == "s") next
    if (ans == "") {                                        # accept the suggestion
      if (!is.na(sugg)) { cw <- cw_append(cw, which(cw$name == sugg)[1], target_col, it); changed <- TRUE }
      else cat("  (no suggestion -- skipped)\n")
      next
    }
    if (low == "i") { cw <- cw_append(cw, which(cw$name == "ignore")[1], target_col, it); changed <- TRUE; next }
    if (low == "n") {
      nn <- trimws(prompt_fn("  new concept name: "))
      wf <- trimws(prompt_fn("  what_for: "))
      g <- cw_get_or_add(cw, nn, what_for = wf); cw <- g$cw
      cw <- cw_append(cw, g$i, target_col, it); changed <- TRUE; next
    }
    # otherwise: one or more concept numbers ("3" or "3,5") -- the 2-meanings case
    nums <- suppressWarnings(as.integer(str_split(ans, "[,\\s]+")[[1]]))
    if (length(nums) && all(!is.na(nums)) && all(nums >= 1 & nums <= length(nm))) {
      for (ix in nums) cw <- cw_append(cw, concept_i[ix], target_col, it)
      changed <- TRUE
    } else cat("  ? didn't understand -- skipped\n")
  }

  if (changed && write) { write.csv(cw, cw_path, row.names = FALSE, na = ""); cat("\nSaved master_crosswalk_manual.csv\n") }
  else cat("\nNo changes written.\n")
  invisible(cw)
}

if (!exists("BEESCABR_SOURCED_BY_RUNNER") && sys.nframe() == 0)
  message('Sourced. Run: review_unknowns("tags")  or  review_unknowns("fields")')
