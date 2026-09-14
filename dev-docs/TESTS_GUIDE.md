# Tests

**Not part of any pipeline** — delete this folder and every figure comes out
identical. They exist because this pipeline fails *quietly*: a bad change drops
records or mis-joins a name, and there is no error, no broken page, just wrong
numbers on a public site.

## Run them

```r
library(testthat); test_dir("tests/testthat")                   # all
library(testthat); test_file("tests/testthat/test-publish.R")   # one
```

The count is whatever `test_dir` reports — it is not written down here, because a
number in a document is a number that goes stale.

## What they actually caught

Real bugs, none of which raised an error at the time:

| | |
|---|---|
| The installer installed **nothing** | `sys.nframe() == 0` is never true under `source()` |
| Tests wrote into the **real `data/`** | a guard flag set in a test file is invisible to the script it sources |
| The verification link included **Casual** records | `verifiable=any`, while the doc said not to |
| Specimen cleaning silently skipped for a week | a renamed `PATHS` key |
| Sixty specimens with no taxonomy | a join with no name fallback |
| Pages published from stale analysis | a source path that no longer resolved |

## Rules

| | |
|---|---|
| **Write the test first** | Watch it fail, then write the code. `CLAUDE.md` has the loop. |
| **Never hit the real API** | Inject a fake through `request_fn` / `request_text_fn`. |
| **Never touch the real cache or `data/`** | DuckDB tests use a temp database. |

## helper.R

| | |
|---|---|
| `src("path/to/script.R")` | load a script; finds it by name if it has moved |
| `src_helpers("script.R", "FLAG")` | load a script **without running its build** |
| `fx("fixture.csv")` | reach a file in `fixtures/` |
| `have_duckdb()` | TRUE when the DuckDB tests can run |

**Use `src_helpers()`, not `src()` plus a flag.** A script that ends in
`if (!exists("X_SOURCED_FOR_HELPERS")) { ...run... }` cannot see a flag you set at
the top of a test file: `source()` evaluates in `globalenv()`, and the test file's
own environment is not on that search path. Setting it that way did nothing, the
guard opened, and two tests ran the full analysis against the real `data/` folder
on every suite run.
