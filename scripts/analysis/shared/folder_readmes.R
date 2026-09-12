# =============================================================
# analysis/shared/folder_readmes.R
# beescabr -- per-folder WHAT_THESE_FILES_ARE.txt for the analysis outputs
# =============================================================

# Per-folder WHAT_THESE_FILES_ARE.txt ------------------------------------------
#
# data/analysis/WHAT_THESE_FILES_ARE.txt explains the tree as a whole. It cannot
# explain a folder that holds eight things at once, and a stranger opening
# phenology/ or interactions/networks/ directly never sees it.
#
# So: a short note IN the folders that hold several different analyses. NOT in
# every folder -- a folder with one analysis already explains itself through the
# <analysis>_findings.csv sitting beside its outputs, and a second file there is
# noise. FOLDER_NOTES below is the whole "where it matters" decision.
#
# GENERATED, not hand-written, because these live inside 2026_generated/ where
# nothing is hand-maintained. The blurb is declared here; the list of analyses and
# their headline results is read back from the findings files, so it cannot go
# stale when an analysis is added, renamed or retired.
#
# Pure text builder tested in tests/testthat/test-folder-readmes.R.

WRAP <- 78

# Where each statistic comes from. Keyed on a pattern matched against the method
# text the findings file already carries, so a folder only ever cites what it used.
#
# Two of these are computed in this repo rather than taken from a package, and the
# note says so. A reader who sees "Rayleigh test" and assumes a vetted library call
# would be wrong, and that is exactly the kind of thing a reviewer asks about.
# `full` is the complete reference; `verified = TRUE` means it was read out of the
# installed package's own Rd or citation() rather than typed from memory. That flag
# is not decoration: an unverified reference is one to spot-check before publication,
# and checking these turned up a wrong year and a missing null-model citation.
STAT_SOURCES <- list(
  list(pat = "iNEXT|Hill",
       src = "Chao et al. 2014, Hsieh, Ma & Chao 2016", verified = FALSE,
       full = paste("Chao, A., Gotelli, N.J., Hsieh, T.C., Sander, E.L., Ma, K.H., Colwell, R.K.",
                    "& Ellison, A.M. (2014). Rarefaction and extrapolation with Hill numbers: a",
                    "framework for sampling and estimation in species diversity studies.",
                    "Ecological Monographs 84, 45-67. -- Hsieh, T.C., Ma, K.H. & Chao, A. (2016).",
                    "iNEXT: an R package for rarefaction and extrapolation of species diversity",
                    "(Hill numbers). Methods in Ecology and Evolution 7, 1451-1456."),
       use = "Hill-number rarefaction and extrapolation; iNEXT R package."),
  list(pat = "individual-based rarefaction|rarefaction to (a |the )?common|rarefaction to equal|vegan rarefaction|rarefied to",
       src = "Hurlbert 1971, Heck, van Belle & Simberloff 1975", verified = TRUE,
       full = paste("Hurlbert, S.H. (1971). The nonconcept of species diversity: a critique and",
                    "alternative parameters. Ecology 52, 577-586. -- Heck, K.L., van Belle, G. &",
                    "Simberloff, D. (1975). Explicit calculation of the rarefaction diversity",
                    "measurement and the determination of sufficient sample size. Ecology 56,",
                    "1459-1461."),
       use = paste("individual-based rarefaction to a common record count;",
                   "vegan::rarefy, whose standard errors follow Heck et al.")),
  list(pat = "Chao2",
       src = "Chao 1987", verified = FALSE,
       full = paste("Chao, A. (1987). Estimating the population size for capture-recapture data",
                    "with unequal catchability. Biometrics 43, 783-791."),
       use = "Chao2 asymptotic richness estimator; vegan::specpool and specaccum."),
  list(pat = "Shannon",
       src = "Shannon 1948", verified = FALSE,
       full = paste("Shannon, C.E. (1948). A mathematical theory of communication.",
                    "Bell System Technical Journal 27, 379-423."),
       use = "Shannon entropy; vegan::diversity."),
  list(pat = "Simpson",
       src = "Simpson 1949", verified = FALSE,
       full = "Simpson, E.H. (1949). Measurement of diversity. Nature 163, 688.",
       use = "Simpson index; vegan::diversity."),
  list(pat = "Pielou|evenness",
       src = "Pielou 1966", verified = FALSE,
       full = paste("Pielou, E.C. (1966). The measurement of diversity in different types of",
                    "biological collections. Journal of Theoretical Biology 13, 131-144."),
       use = "evenness J = H / log(S), computed in this repo from vegan's Shannon H."),
  list(pat = "PERMANOVA",
       src = "Anderson 2001", verified = FALSE,
       full = paste("Anderson, M.J. (2001). A new method for non-parametric multivariate",
                    "analysis of variance. Austral Ecology 26, 32-46."),
       use = "PERMANOVA on Bray-Curtis distances; vegan::adonis2."),
  list(pat = "NMDS|multidimensional scaling",
       src = "Kruskal 1964", verified = TRUE,
       full = paste("Kruskal, J.B. (1964). Multidimensional scaling by optimizing",
                    "goodness-of-fit to a nonmetric hypothesis. Psychometrika 29, 1-28."),
       use = "non-metric multidimensional scaling; vegan::metaMDS."),
  list(pat = "NODF|nested",
       src = "Almeida-Neto et al. 2008", verified = TRUE,
       full = paste("Almeida-Neto, M., Guimaraes, P., Guimaraes, P.R., Loyola, R.D. & Ulrich, W.",
                    "(2008). A consistent metric for nestedness analysis in ecological systems:",
                    "reconciling concept and measurement. Oikos 117, 1227-1239."),
       use = "NODF nestedness metric; vegan::nestednodf."),
  list(pat = "Bray|bray|PERMANOVA|NMDS|dissimilar",
       src = "Bray & Curtis 1957", verified = FALSE,
       full = paste("Bray, J.R. & Curtis, J.T. (1957). An ordination of the upland forest",
                    "communities of southern Wisconsin. Ecological Monographs 27, 325-349."),
       use = paste("the dissimilarity every PERMANOVA and NMDS result is computed on",
                   "(method/distance = \"bray\"); vegan::adonis2 and vegan::metaMDS.")),
  list(pat = "r2dtable|Patefield",
       src = "Patefield 1981", verified = FALSE,
       full = paste("Patefield, W.M. (1981). An efficient method of generating random R x C",
                    "tables with given row and column totals. Applied Statistics 30, 91-97."),
       use = paste("the null the H2' p-value is tested against: random tables with the same",
                   "row and column totals; stats::r2dtable.")),
  list(pat = "quasiswap|NODF|nested",
       src = "Miklos & Podani 2004", verified = TRUE,
       full = paste("Miklos, I. & Podani, J. (2004). Randomization of presence-absence matrices:",
                    "comments and new algorithms. Ecology 85, 86-92."),
       use = "the quasiswap null the NODF p-value is tested against; vegan::oecosimu."),
  list(pat = "H2",
       src = "Bluthgen, Menzel & Bluthgen 2006", verified = TRUE,
       full = paste("Bluthgen, N., Menzel, F. & Bluthgen, N. (2006). Measuring specialization in",
                    "species interaction networks. BMC Ecology 6, 9."),
       use = paste("H2' network specialization, computed in this repo against a null that",
                   "permutes species labels within month x method strata.")),
  list(pat = "Rayleigh",
       src = "standard circular statistics", verified = FALSE,
       full = paste("Rayleigh test of circular uniformity. No package call to cite: computed in",
                    "this repo from the resultant vector as Z = nR^2 with p ~ exp(-Z), the",
                    "standard large-sample approximation."),
       use = "Rayleigh test of seasonal concentration; computed in this repo."),
  list(pat = "chi-square|chisq",
       src = "base R", verified = TRUE,
       full = paste("R Core Team. stats::chisq.test -- Pearson chi-square test of independence.",
                    "Part of base R; see the R citation below."),
       use = "chi-square test of independence; stats::chisq.test."),
  list(pat = "bipartite|network diagram|plotweb",
       src = "Dormann, Fruend, Bluethgen & Gruber 2009", verified = TRUE,
       full = paste("Dormann, C.F., Fruend, J., Bluethgen, N. & Gruber, B. (2009). Indices,",
                    "graphs and null models: analyzing bipartite ecological networks.",
                    "The Open Ecology Journal 2, 7-24."),
       use = "bipartite::plotweb, for drawing the web only; no statistic taken from it.")
)

# ---- the references file -----------------------------------------------------
# Software versions come from the running session, so the file always says what
# the numbers were actually produced with rather than what was installed once.
REF_PACKAGES <- c("vegan", "iNEXT", "bipartite")

# Every source the numbers rest on, not only the statistics. iNaturalist and IUCN
# are built by citations.R (which the website already uses) so the version and
# access date are the real ones rather than a typed guess.
.data_sources <- function() {
  if (!exists("inat_citation")) {
    # the pipeline runs from the repo root, a test does not -- look upward rather
    # than assuming, or the data sources silently vanish from the file
    cand <- file.path(c(".", "..", "../..", "../../.."), "scripts/website/citations.R")
    f <- cand[file.exists(cand)][1]
    if (is.na(f)) return(character(0))
    source(f)
  }
  # the access date is the day the data was last pulled, not today -- these
  # references describe the numbers in this folder
  .ingest <- "data/inat_observations/cache/last_ingest.txt"
  asof <- if (file.exists(.ingest)) {
    v <- readLines(.ingest, warn = FALSE)[1]
    if (is.na(v) || !nzchar(v)) format(Sys.Date()) else substr(v, 1, 10)
  } else format(Sys.Date())
  .iucn <- "data/checklists/iucn/iucn_redlist_version_generated.txt"
  iucn_v <- if (file.exists(.iucn)) citation_read_version(.iucn) else ""

  c(inat_citation(asof),
    if (nzchar(iucn_v)) iucn_citation(iucn_v, asof) else
      "IUCN Red List of Threatened Species. https://www.iucnredlist.org -- not fetched for this run; the status column reads \"Not Evaluated\".",
    paste("Integrated Taxonomic Information System (ITIS). https://www.itis.gov.",
          "-- consulted when iNaturalist has no page for a checklist name, to tell a",
          "real-but-unpublished bee from a name that was renamed or withdrawn."),
    paste("Hung, K.-L.J., Mullins, J.L., Rightmyer, M.G., Wall, M., Berrian, J.,",
          "Ascher, J.S., Yanega, D., Davids, J.A. & Holway, D.A. (2026). Checklist,",
          "Version 3. In An Annotated Checklist of the Bees (Hymenoptera: Anthophila)",
          "of San Diego County, California. UC San Diego Library Digital Collections.",
          "https://doi.org/10.6075/J0NZ88MD -- the county species list every checklist",
          "tier is compared against."),
    paste("National Park Service, Land Resources Division. Cabrillo National Monument",
          "administrative boundary. -- the authoritative park outline."),
    paste("City of San Diego. Community Plan districts (Peninsula, CPCODE 30).",
          "-- the Point Loma tier boundary."),
    paste("San Diego Natural History Museum (SDNHM). -- accession numbers for the",
          "deposited specimens."))
}

references_text <- function(sources = STAT_SOURCES) {
  seen <- character(0); refs <- character(0); marks <- logical(0)
  for (x in sources) {
    if (x$full %in% seen) next
    seen <- c(seen, x$full); refs <- c(refs, x$full); marks <- c(marks, isTRUE(x$verified))
  }
  o <- order(refs); refs <- refs[o]; marks <- marks[o]

  out <- c("REFERENCES CITED", strrep("=", 16), "",
    .wrap(paste("Where the data came from, and where each statistical method comes from.",
                "The short form of a method appears in each folder's",
                "WHAT_THESE_FILES_ARE.txt; the full reference is here.")), "",
    .wrap(paste("A reference marked [pkg] was checked against the installed package's own",
                "documentation or citation(). The rest were typed by hand and are worth a",
                "spot-check before publication.")), "",
    "DATA SOURCES", strrep("-", 12), "")
  for (d in .data_sources())
    out <- c(out, .wrap(d, indent = "      ", exdent = "        "), "")

  out <- c(out, "METHODS", strrep("-", 7), "")
  for (i in seq_along(refs))
    out <- c(out, .wrap(paste0(if (marks[i]) "[pkg] " else "      ", refs[i]),
                        indent = "", exdent = "        "), "")

  out <- c(out, "SOFTWARE", strrep("-", 8), "",
           .wrap(paste0("R ", getRversion(), " -- ", R.version.string), indent = "  "), "")
  for (p in REF_PACKAGES) {
    if (!requireNamespace(p, quietly = TRUE)) next
    out <- c(out, .wrap(sprintf("%s %s", p, utils::packageVersion(p)), indent = "  "))
  }
  out <- c(out, "", .wrap(paste("Versions are read from the session that generated this file, so they",
                                "describe the numbers in this folder rather than whatever is installed",
                                "later.")))
  paste0(paste(out, collapse = "\n"), "\n")
}

# the sources for the methods this folder actually used, in declared order
.sources_for <- function(findings, what = "") {
  txt <- paste(c(what, vapply(findings, function(f) paste(f$name, f$finding,
                      if (!is.null(f$method)) f$method else ""), character(1))), collapse = " | ")
  hit <- vapply(STAT_SOURCES, function(x) grepl(x$pat, txt, ignore.case = TRUE), logical(1))
  lapply(STAT_SOURCES[hit], function(x) list(src = x$src, use = x$use))
}

# folder (relative to the season folder) -> what it is for, in one or two sentences
FOLDER_NOTES <- list(
  "coverage" = paste(
    "How well the park has been sampled, and where the holes are. Each subfolder",
    "asks one coverage question: which bees still need a photo or a voucher, which",
    "transects are under-visited, and how much of the county checklist we hold."),
  "interactions" = paste(
    "Which bees visit which plants. The networks subfolder holds the whole web and",
    "its statistics; top_plants ranks the plants that carry the most bee visits."),
  "method_comparison" = paste(
    "Lethal (netting) versus non-lethal (photographing). Everything here is",
    "restricted to a like-for-like",
    "window so the two methods are compared on equal footing rather than on how many",
    "records each happened to produce.",
    "\n\nTwo different comparisons run in this project and only one of them is here.",
    "METHOD is nets vs photos, and that is this folder. OBSERVER is beeple vs",
    "interns, and its rarefaction lives under",
    "richness/rarefaction/fair_observer_2024/ -- filed by what the analysis IS",
    "rather than by what it compares. The yield files here do split by contributor",
    "as well (bee_yield_by_contributor.csv).",
    "\n\nRead the two together. Before 2024 only interns netted and only beeple",
    "photographed, so method and observer were confounded: lethal beats non-lethal,",
    "but with method held constant beeple beat interns. The gap is the netting,",
    "not the people."),
  "phenology" = paste(
    "When things happen through the year: when each bee genus is active, when the",
    "plants bloom, and when we actually surveyed. The effort files matter because a",
    "bee can only look seasonal if somebody was out looking in that season."),
  "reference" = paste(
    "Lookups and deliverables rather than analyses: the field guide, the transect",
    "maps, the conservation write-ups, and the summary tables NPS asked for."),
  "richness" = paste(
    "How many kinds of bee, and how that count depends on effort. accumulation asks",
    "whether we are still finding new ones, rarefaction compares groups at equal",
    "effort, and diversity looks at how evenly the community is spread."),
  "richness/rarefaction" = paste(
    "Richness compared at EQUAL effort. Raw counts cannot compare two groups that",
    "were not sampled equally, so every number here is levelled first. The files in",
    "this folder cover transects and years; the fair_ subfolders hold the two method",
    "and observer comparisons."),
  "richness/diversity" = paste(
    "Community structure rather than a headcount: how evenly records are spread",
    "across species (Pielou evenness), which species dominate (rank-abundance), and",
    "whether transects or years differ in composition (NMDS + PERMANOVA)."),
  "interactions/networks" = paste(
    "The bee-plant web. Matrices are the raw who-visited-what; the network figures",
    "draw it; the specialization files say which bees are picky and which are not.",
    "Genus and species versions of the same thing sit side by side."),
  "reference/nps_summary" = paste(
    "The tables NPS asked for, formatted for the report rather than for analysis:",
    "checklists at each rank, headline counts, methods, who took part, and the",
    "bee-by-plant matrix of which bee was recorded on which plant."),
  "reference/conservation" = paste(
    "The IUCN-listed bees and the plants that carry them. The rare_flower_ figures",
    "are one per rare bee, each showing the plants it was recorded on."),
  "method_comparison/yield" = paste(
    "What each method actually produced: how many records, how many taxa, and how",
    "often a record could be identified to species."),
  "coverage/records_by_evidence" = paste(
    "How strong the evidence is behind each taxon: a specimen voucher, a",
    "research-grade photo, or a photo still waiting on an ID."),
  "coverage/bee_bounties" = paste(
    "The wanted list. Which bees are still missing the kind of evidence they need,",
    "so a surveyor knows what to look for and whether to net it or photograph it.")
)

# subfolder names that repeat across the tree, explained once
SUBFOLDER_NOTES <- c(
  "website" = paste("the HTML version of this analysis, published to the public site.",
                    "Same numbers as the CSV beside it."),
  "fair_method_2021_2023" = paste("the like-for-like window for comparing lethal and",
                    "non-lethal surveys: survey records only, Mar-Oct 2021-2023."),
  "fair_observer_2024" = paste("the like-for-like window for comparing beeple with",
                    "interns: May-Sep 2024, when both groups were photographing."),
  "genus_species_webs" = "one flower-web figure per bee genus."
)

# indent = first line, exdent = the continuation lines (a hanging indent). Without
# the hanging indent a wrapped reference's second line starts at the margin and
# reads as a new entry.
.wrap <- function(x, indent = "", exdent = NULL) {
  if (is.null(exdent)) exdent <- indent
  paste(strwrap(x, width = WRAP, initial = indent, prefix = exdent), collapse = "\n")
}

.first_sentence <- function(x) {
  if (!nzchar(x)) return("")
  # a findings headline can run several sentences; the folder note wants the first
  m <- regexpr("^.*?[.!?](\\s|$)", x)
  s <- if (m > 0) regmatches(x, m) else x
  trimws(s)
}

# findings: list of list(name =, finding =). subdirs: character vector of names.
# hints: named character, subfolder -> one line describing it. A hub folder like
# coverage/ is almost nothing BUT subfolders, so a bare list of names there is
# useless; the hint is read out of the findings files inside each one.
# src_findings: what the SOURCES block is computed from. Defaults to this folder's
# own findings, but a hub folder passes everything beneath it -- richness/ holds no
# analysis of its own, and a reader there should still see that the branch rests on
# Chao2 and iNEXT.
folder_readme_text <- function(rel, what, findings, subdirs, hints = character(0),
                               src_findings = NULL) {
  if (is.null(src_findings)) src_findings <- findings
  out <- c("WHAT THESE FILES ARE", strrep("=", 20), "",
           .wrap(paste0("Folder: ", rel)), "", .wrap(what), "")

  if (length(findings)) {
    out <- c(out, "THE ANALYSES IN HERE", strrep("-", 20), "")
    for (f in findings) {
      out <- c(out, .wrap(f$name, indent = "  "),
                    .wrap(.first_sentence(f$finding), indent = "      "))
      if (!is.null(f$method) && nzchar(f$method))
        out <- c(out, .wrap(paste0("Method: ", f$method), indent = "      "))
      out <- c(out, "")
    }
  }

  if (length(subdirs)) {
    out <- c(out, "SUBFOLDERS", strrep("-", 10), "")
    for (d in sort(subdirs)) {
      note <- if (d %in% names(SUBFOLDER_NOTES)) SUBFOLDER_NOTES[[d]]
              else if (d %in% names(hints)) unname(hints[[d]]) else ""
      out <- c(out, .wrap(paste0(d, "/"), indent = "  "))
      if (nzchar(note)) out <- c(out, .wrap(note, indent = "      "))
      out <- c(out, "")
    }
  }

  srcs <- .sources_for(src_findings, what)
  if (length(srcs)) {
    out <- c(out, "WHERE THESE METHODS COME FROM", strrep("-", 29), "")
    for (x in srcs)
      out <- c(out, .wrap(x$src, indent = "  "), .wrap(x$use, indent = "      "), "")
  }

  out <- c(out,
    .wrap(paste("Every file here is regenerated by",
                "source('scripts/run_all_analysis_pipeline.R') -- nothing in this folder is",
                "hand-maintained. If a number looks wrong, the fix is in the script that",
                "wrote it.")),
    "",
    .wrap("The whole tree is explained in data/analysis/WHAT_THESE_FILES_ARE.txt."),
    if (length(srcs))
      .wrap(paste("Full references for the methods above are in",
                  "data/analysis/REFERENCES_CITED.txt.")) else NULL)
  paste0(paste(out, collapse = "\n"), "\n")
}

# ---- writer ------------------------------------------------------------------
# reads each folder's findings CSVs for the headline results, then writes the note
.findings_in <- function(dir) {
  fs <- list.files(dir, pattern = "_findings\\.csv$", full.names = TRUE)
  lapply(sort(fs), function(f) {
    d <- try(read.csv(f, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(d, "try-error") || !all(c("field", "value") %in% names(d))) return(NULL)
    g <- function(k) { v <- d$value[d$field == k]; if (length(v)) v[1] else "" }
    list(name = g("analysis name"), finding = g("key_finding"), method = g("analysis"))
  }) |> Filter(f = Negate(is.null))
}

#' Write a WHAT_THESE_FILES_ARE.txt into every folder that has a note
#'
#' Only folders named in `FOLDER_NOTES` get one -- a note in every folder is
#' noise; a note where a reader would otherwise guess is worth having.
#'
#' @param root Repository root, so the function can be pointed at a temp copy
#'   in a test rather than the real tree.
#' @return The number of notes written, invisibly.
write_folder_readmes <- function(root) {
  n <- 0
  for (rel in names(FOLDER_NOTES)) {
    dir <- file.path(root, rel)
    if (!dir.exists(dir)) next
    subs <- basename(list.dirs(dir, recursive = FALSE))
    hints <- vapply(subs, function(d) {
      f <- .findings_in(file.path(dir, d))
      if (!length(f)) return("")
      # the analyses inside are what that subfolder IS
      paste(vapply(f, function(x) x$name, character(1)), collapse = "; ")
    }, character(1))
    deep <- unlist(lapply(c(dir, list.dirs(dir, recursive = TRUE)[-1]), .findings_in),
                  recursive = FALSE)
    txt  <- folder_readme_text(rel, FOLDER_NOTES[[rel]], .findings_in(dir), subs,
                               hints = hints[nzchar(hints)], src_findings = deep)
    writeLines(txt, file.path(dir, "WHAT_THESE_FILES_ARE.txt"))
    n <- n + 1
  }
  # Nothing matched means the root is wrong, not that there was nothing to do: the
  # loop skips a folder that does not exist, so a bad root wrote 0 notes and said
  # "wrote 0" as though that were a result. It cost a regeneration that looked fine.
  if (!n) message("  folder notes: none of the ", length(FOLDER_NOTES),
                  " folders exist under '", root, "' -- nothing written. ",
                  "Pass the season folder, e.g. data/analysis/<year>_generated.")

  # one full reference list for the whole analysis folder, beside the tree-level
  # note rather than inside a season folder: the references do not change per year
  # data/analysis/, NOT the season folder: the references do not change per year,
  # and they sit beside the tree-level WHAT_THESE_FILES_ARE.txt
  writeLines(references_text(), file.path(dirname(root), "REFERENCES_CITED.txt"))
  invisible(n)
}
