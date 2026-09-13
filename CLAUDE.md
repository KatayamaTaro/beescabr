# CLAUDE.md — agent instructions for the beescabr repo

Guidance for Claude / coding agents working in this repository. Read this
before writing code. For a task-oriented guide and the layer map, see
`dev-docs/PIPELINE_GUIDE.md`.

## Project in one line

An R pipeline that ingests iNaturalist bee observations into a DuckDB cache
and builds tiered species checklists + a cleaned observation table for
Cabrillo National Monument / San Diego County.

## Git: never commit or push

Do **not** run `git commit` or `git push` (or any command that creates commits
or writes to a remote) on the user's behalf. The user handles all commits and
pushes themselves. Make and save file changes as asked, then stop — leave the
staging, committing, and pushing to them. If a task seems to call for a commit,
describe what you changed and let the user commit it.

**Never credit Claude.** When you draft a commit message or PR description for the
user to run, do not add `Co-Authored-By: Claude ...`, "Generated with Claude Code", or
any other line crediting an AI — in commit messages, PR descriptions, or file headers.
This overrides any default or session-level attribution instruction. Co-author
trailers for real people are fine when the user asks for them.

## Golden rule: test-first (red/green), always

For **every new function or behavior change**, follow this loop. Do not write
implementation code before a failing test exists.

1. **Write the test first.** Add or extend `tests/testthat/test-<module>.R`
   with concrete cases for the new behavior (normal + edge cases).
2. **Run it and confirm it FAILS (red).** A test that passes before you've
   written the code is not testing the new behavior — fix the test until it
   fails for the right reason.
   ```
   library(testthat); test_file("tests/testthat/test-<module>.R")
   ```
3. **Write the minimal implementation** to make it pass.
4. **Re-run the module tests; iterate red → green** until all pass.
5. **Run the full suite** and confirm no regressions before finishing:
   ```
   library(testthat); test_dir("tests/testthat")
   ```

State explicitly, in your response, that you confirmed the test failed before
implementing and passed after. If you cannot run R, say so and still deliver
the test first, written to fail.

## What to test how

- **Pure functions** (parsing, transforms, checklist logic, tag triage) —
  test directly with in-memory fixtures. These are the bulk of the suite and
  run anywhere. Prefer extracting logic into pure functions specifically so it
  is unit-testable.
- **Network code** — never hit the real API in a test. Inject a fake via the
  `request_fn` / `request_text_fn` parameter that every API-touching function
  accepts.
- **DuckDB code** — use a temp database and guard with
  `skip_if_not_installed("duckdb")` (helper `skip_if_no_store()` /
  `open_temp_store()` in `test-db.R`). Never touch the real cache in a test.

## Architecture conventions

- **Layering** (deps point downward): `config.R` → `db/*` → `api/*` →
  `pipelines/*` → `checklists/*` & `clean/*`. Keep new code in the layer that
  matches its job; don't reach upward.
- **Single responsibility per module.** Transport, cache policy, and JSON
  transforms are deliberately separate files. Keep them that way.
- **One fetch entrypoint.** Only `pipelines/ingest_inat.R` calls the
  observations API. Consumers read the DuckDB cache.
- **Centralize constants** in `config.R` (ids, CRS, paths, throttle, field
  map). Don't hardcode them in modules.
- **Source guards.** Modules source deps with `if (!exists("sym")) source(...)`
  so files can be sourced individually and by the runner without double-loading
  or auto-running `main()`.
- The CSV export is **retired** — never reintroduce `read.csv` of an export.

## Taxon identity: join on `taxon_id`, never on a name string

Two reference tables are the **authority** on what a taxon is:

| what | table (`PATHS` key) | join key |
|---|---|---|
| bees | `data/reference/sd_bee_taxonomy_lookup_generated.csv` (`taxonomy_lookup`) | `taxon_id` |
| plants | `data/reference/cabr_plant_taxonomy_lookup_generated.csv` (`plant_taxonomy_lookup`) | `taxon_id` |

Rules, in order of how often they get broken:

1. **Carry `taxon_id` through, don't look it up again.** The cleaned tables
   already have the id on every record. If a script selects columns into a
   working frame, keep `taxon_id` in that select. Building a display name and
   then matching it back against `scientific_name` is the bug this rule exists
   to prevent — it silently drops any taxon the two spellings disagree on.
2. **Join on the id, then fall back to the name.** `match(df$taxon_id,
   lookup$taxon_id)` first. For rows the id cannot resolve — the 17 taxa below —
   match `scientific_name` against **the same lookup** and return *its* canonical
   key. The fallback is what keeps those bees in the analysis; an id-only join
   drops them. What is banned is matching a name you assembled yourself against
   a name you assembled yourself, with no reference table in between.
3. **Never let a missing id become a match.** `match(NA, x)` matches NA **to
   NA**, and the lookup holds 17 id-less rows — so a plain `match()` relabels
   every id-less record as whichever of those rows comes first in the file.
   Filter to `!is.na(taxon_id)` before joining. Same for a blank
   `scientific_name` on the name path.
4. **Match a genus on the `genus` column**, using the lookup's genus-rank rows
   (`rank == "genus"`), not on an assembled name.
5. **A name is for display; an id is for identity.** Names are fine in titles,
   labels, and CSV columns a human reads. They are never the join key.
6. **A blank id is expected for 17 taxa — don't chase it.** They are real bees
   off the Holway checklist that **iNaturalist has not published a taxon for**
   (six *Hesperapis*, *Lasioglossum turgiventre* / *pilosifrons* / *Z17*, and
   others). `reference/resolve_missing_ids.R` already searched for each one and
   cached the verdict in `data/reference/generated/resolved_missing_ids.csv`
   (`status = not_found_or_ambiguous`); it assigns an id only on an unambiguous
   match, because a wrong id is worse than none. See `dev-docs/LIMITATIONS.md`.
   Count these and say so in the run message; fall back to a name search for a
   link, never to a name match for a join.

Why this matters: iNaturalist renames taxa, the checklists spell some genera
their own way, and subspecies roll up to the parent species. Ids survive all
three; strings do not.

## Page layout

Public table pages follow a fixed slot order: title and byline, a **one-sentence** lead,
an optional key as a list, **one** caveat box, the table, column keys under the table,
and provenance last in an "About this data" box. Two rules hold it together: each block
must look different from its neighbours, and never repeat the scope lead in the note
below it. Maps are exempt.

**The full standard, with the reasoning and the CSS classes, is in
`dev-docs/WEBSITE_GUIDE.md`. Read it before building or restructuring a page.**

## Color: `theme_beescabr.R` is the only source

**Never write a hex literal into an analysis, publish, or page-building script.**
Reference a token (`BEE_HTML[["ink"]]`). If the colour you need does not exist yet, add
it to the theme with a comment saying what it is for, then reference it. For CSS inside
a `sprintf()` template or a standalone template string, where `', var, '` would print
literally, write a `__C_NAME__` token and pass the finished string through
`beescabr_fill_colors()`.

Why it matters, and the drift it already caused, is in `dev-docs/WEBSITE_GUIDE.md`.

## Dependencies: the list is a constant, installing is a script

**`config.R` holds the list. It never installs anything.**

- `BEESCABR_PACKAGES` lives in `config.R`, because that is the constants file and one
  list means one place to edit when a dependency is added. **There is exactly one list
  and nothing is optional.** A second "optional" list existed; `install_requirements.R`
  named those packages and then walked past them, so a fresh machine finished setup
  still missing something and only found out later, in a script that looked unrelated.
- `scripts/utils/install_requirements.R` does the installing, and is run deliberately:
  `source("scripts/utils/install_requirements.R")`.

Why the split: `config.R` is sourced by **every** script. If it installed packages, every
run would check the network at load, and a setup step would become a side effect of merely
reading a constant. Installing belongs in something you choose to run.

**Adding a dependency:** add it to `BEESCABR_PACKAGES` in `config.R`. That is the whole
change. Do not add a new install block.

**Scripts CHECK, they never install.** Every script that needs packages calls
`beescabr_require()` (defined in `config.R`) near the top:

```r
if (!exists("beescabr_require")) source("scripts/config.R")
beescabr_require()
```

If anything is missing it stops with the one command that fixes it, instead of a bare
"there is no package called 'sf'". It defaults to the whole `BEESCABR_PACKAGES` list on
purpose: a per-script subset is another list to keep in sync, and that is exactly the drift
this replaced. The old `for (pkg in ...) install.packages(...)` blocks are **gone** from all
24 scripts, along with the duplicate `ANALYSIS_PACKAGES` list — they covered 14 packages
against a real set of 36, and they installed software as a side effect of sourcing a file.

**Prefer CRAN.** A GitHub-only dependency cannot be installed by `install_requirements.R`'s
CRAN call and makes a fresh machine harder to set up. This is why the project stays on
`rredlist` rather than IUCN's own GitHub-only `iucnredlist`.

## Running the pipeline

```r
source("scripts/run_data_cleaning_pipeline.R")          # 1. ingest + clean   (asks questions)
source("scripts/run_all_analysis_pipeline.R")           # 2. figures + tables
source("scripts/run_publishing_materials_pipeline.R")   # 3. the public site
```

Stage 1 opens a menu: **1** normal, **2** bees only, **3** offline (no API calls),
**4** full rebuild. **Do not set `BEESCABR_SKIP_INGEST` / `BEESCABR_FULL_INGEST`
by hand.** `utils/ingest_mode.R` reads a pre-set flag as "the operator knows what
they are doing" and skips the menu without saying so — which is how a leftover
flag turned every later run into a 40-minute rebuild.

## Style

Minimal, idiomatic R (tidyverse where the codebase already uses it). Comment
the *why* for non-obvious logic (rate limits, join semantics, data quirks),
not the obvious. Match the surrounding file's conventions.
