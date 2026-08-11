# De-duplicating the `pbc` example setup block

**Date:** 2026-08-11
**Status:** Approved, implementation deferred (see Sequencing)
**Origin:** Item 5 of the Codex review of 3.5.1

## Problem

A ~35-line `pbc` data-preparation block is repeated across five locations:

| Location | Guarded? |
|---|---|
| `R/gg_error.R` `@examples` | no |
| `R/plot.gg_error.R` `@examples` | no |
| `R/gg_vimp.R` `@examples` | no |
| `R/plot.gg_rfsrc.R` `@examples` | inside `\donttest{}` |
| `tests/testthat/test_gg_vimp.R:198` | n/a |

The block loops over every column of `randomForestSRC::pbc`, coercing two-valued
0/1 columns to logical and columns with five or fewer levels to factors, then
converts `age` and `days` from days to years and relabels `treatment` to
`DPCA`/`placebo`.

The fifth copy, in the test file, was not part of the original Codex item.

## Findings that shaped the design

### The check-time argument does not hold

Measured on this machine:

| | elapsed |
|---|---|
| The munging block | 0.016 s |
| One `rfsrc` pbc fit as the examples call it | 0.84 s |

Three of the four example copies run unguarded, so the duplicated munging costs
roughly **0.05 s per `R CMD check`** — noise against the sub-10-minute CRAN
budget that archived 3.1.0.

The genuine check-time load is the forest fits: 86 `rfsrc()` calls across all
roxygen examples, 33 of them in these four files. **No de-duplication route
reduces that count.** Cutting example check time is a separate piece of work
(cache forests, or reduce `ntree`) and is explicitly out of scope here.

This change is justified on maintenance load alone.

### The copies have already drifted

The four blocks are not verbatim. The logic is identical; the surrounding prose
has diverged in three directions:

- `data(pbc, package = "randomForestSRC",)` vs `..., )` vs no trailing comma
- The editorial comment is one line in two files, split across two lines in
  `gg_vimp.R`, and already reworded to `# ...convert to years` in
  `plot.gg_rfsrc.R`
- `plot.gg_rfsrc.R` carries an extra `# Remove test-set patients` comment the
  others lack

This is the predicted drift, already underway, and is the strongest argument for
the change.

## Design

### Chosen route: one shared example file

`inst/examples/pbc-setup.R` holds the block exactly once. Each of the four
roxygen blocks replaces its literal copy with:

```r
#' @example inst/examples/pbc-setup.R
```

Roxygen inlines the file's contents into each `.Rd` at `document()` time, so the
generated documentation is byte-for-byte the kind of self-contained, runnable
example it is today. The change is invisible to readers and to CRAN; only the
maintainer's edit point moves.

`tests/testthat/test_gg_vimp.R` sources the same file via
`system.file("examples", "pbc-setup.R", package = "ggRandomForests")`, which
resolves both under `devtools::test()` (where `system.file` is shimmed to find
`inst/`) and against an installed package. The `source()` call must use
`local = TRUE` when invoked inside a `test_that()` block, so the created objects
land in the test's environment rather than the global one — writing to
`.GlobalEnv` from a test would itself be a CRAN issue.

#### Output contract

The shared file must leave exactly three objects behind:

| Object | Description |
|---|---|
| `pbc` | the munged full data frame, `days` dropped, `years` added |
| `dta_train` | rows with an assigned `treatment` |
| `pbc_test` | rows with `treatment` missing |

The callers currently disagree on this: four of the five create `pbc_test`, but
`plot.gg_rfsrc.R` does not. Standardising on all three means `plot.gg_rfsrc.R`
gains one unused assignment. That is deliberate — a single uniform contract is
worth more than avoiding one sub-millisecond subset, and an unused object in an
example is harmless.

Net effect: five copies become one. No new exported function, no `data/`
directory, no `DESCRIPTION` change.

### Why not a helper function

A non-exported `pbc_demo_data()` cannot be called from `@examples`. Examples run
with the package *attached*, not with its namespace exposed, so the block would
have to read `ggRandomForests:::pbc_demo_data()`. That draws CRAN attention to a
`:::` call against the package's own namespace, and it makes the examples
non-runnable for a reader who copies them — which matters in a package whose
examples are its teaching surface.

Exporting the helper instead would solve the runnability problem but adds public
surface (29 → 30 exports) plus an `.Rd` with `\value`, for a change whose only
benefit is maintenance.

### Why not a prepared dataset in `data/`

The package currently ships no datasets at all. Adding `data/` would require a
dataset `.Rd` with `\format`/`\source`, a `LazyData:` line in `DESCRIPTION`
(currently absent), and a decision about redistributing a derivative of
`randomForestSRC::pbc`. It would also hide the data-cleaning lesson the block
currently teaches — that `pbc` arrives with age in days and 0/1 integers that
ought to be factors.

## Verified mechanics

Both behaviours below were confirmed against roxygen2 8.1.0 in a throwaway
package before this spec was written. They are not assumptions.

1. `@example <path>` inlines the file's **contents** into the `\examples{}`
   section of the generated `.Rd`. The path is relative to package root.
2. `@examples` (inline) and `@example` (file) tags **concatenate in source
   order**, so a block can read: inline preamble → shared file → inline
   remainder.

### Constraint: `\donttest{}` may not straddle a tag boundary

Roxygen validates brace balance **per tag**. Opening `\donttest{` in one
`@examples` tag and closing it in another fails with:

```
✖ @examples has mismatched braces or quotes.
```

`plot.gg_rfsrc.R` is affected: its `\donttest{` at line 133 opens *before* the
munging and closes *after* the forest fit.

**Resolution:** move `\donttest{` to open *after* the shared block, so it guards
only the fit and the plots. The munging costs 16 ms, so guarding it buys
nothing, and the comment's stated intent — `## -------- pbc data (larger dataset
-- skipped on CRAN)` — refers to the forest, not the data preparation. Braces
then balance within a single tag.

This slightly *increases* what runs under `R CMD check` for that file (the 16 ms
munging), which is immaterial.

## Scope boundaries

Explicitly **not** included, per surgical-changes:

- **`R/plot.gg_variable.R:209`** contains a near-identical coercion loop, but it
  operates on `gg_dta` rather than `pbc` and omits the age/days/treatment half.
  Unifying them would be a behavioural change to package code, not a docs
  cleanup. Note it separately.
- **Example check time** (86 `rfsrc()` calls). Real and worth addressing, but
  orthogonal to this change.
- **The editorial comments** (`# For whatever reason... makes no sense to me`)
  are removed by commit `9e029b6f` on `fix/codex-review-3.5.1`. The shared file
  should be seeded from the post-merge text, not cleaned up a second time here.

## Sequencing

`fix/codex-review-3.5.1` is **not merged into `main`**. This worktree branched
from `8348409d`, so all four editorial comments are still present here, and that
branch edits all four of the same files.

**Implementation is deferred until `fix/codex-review-3.5.1` lands on `main`.**
Starting now would guarantee a four-file conflict and risk reintroducing the
comments that branch removes.

## Versioning

Per the versioning discipline in the global `CLAUDE.md`: this adds no public
surface, so it does not on its own warrant a minor bump. It should ride along
with whatever minor cycle it lands in, accumulating under the current minor
rather than inflating it.

## Implementation notes

Two deviations from the design surfaced during implementation. Both are
recorded here rather than silently absorbed.

### `data(..., envir = environment())`

The design assumed the shared file could be sourced into a scoped environment
for the test. It cannot as written: `data()` defaults to `envir = .GlobalEnv`,
so sourcing it would have written `pbc` to the global environment from inside a
test — itself a CRAN issue, and the very thing the existing test avoided with
its `new.env()` dance.

The shared file therefore passes `envir = environment()` explicitly. At the top
level of an example this resolves to the global environment, so example
behaviour is unchanged; under `sys.source(..., envir = env)` it resolves to the
scoped environment. One line, correct in both contexts.

### `&` → `&&`

Consolidating the block moved it from roxygen comment text into a real `.R`
file, so `lintr::lint_package()` sees it for the first time. It immediately
flagged `vector_logic_linter` on:

```r
if (!is.logical(pbc[, ind]) &
  length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
```

Both operands are length-1 scalars, so `&` and `&&` are behaviourally identical
here and the fix is safe. The defect has been present in all five copies since
they were written and was invisible because lintr does not lint example prose.

This is a second, unbudgeted benefit of consolidation: example code becomes
lintable. It also means the shared file is now subject to the 0-lint CI gate,
which is desirable but worth knowing.

### Equivalence verification

Before and after the `&&` change, the shared file was checked to produce objects
`identical()` to those from the original inline block for all three of `pbc`,
`dta_train`, and `pbc_test`. Both runs returned TRUE on all three.

## Definition of done

1. `inst/examples/pbc-setup.R` exists and contains the block once, seeded from
   the post-`9e029b6f` text.
2. All four roxygen blocks reference it via `@example`; no literal copy remains
   in `R/`.
3. `tests/testthat/test_gg_vimp.R` sources the shared file rather than inlining
   the block.
4. `\donttest{` in `plot.gg_rfsrc.R` opens after the shared block, and roxygen
   reports no brace-balance errors.
5. `devtools::document()` regenerates the four `.Rd` files. Reviewing
   `git diff man/*.Rd`, the only changes are:
   - comment/whitespace normalisation, as the three drifted variants converge on
     the shared text;
   - one added `pbc_test <- ...` line in `plot.gg_rfsrc.Rd` (see Output
     contract);
   - the relocated `\donttest{` in `plot.gg_rfsrc.Rd`.

   No change to the munging logic itself in any of the four.
6. `Rscript -e 'lintr::lint_package()'` reports 0 lints.
7. `NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'` passes,
   with `test_gg_vimp.R` genuinely running rather than skipping.
8. `R CMD check --as-cran` with the manual, built from a clean `git archive`
   export, is 0/0/0 and the examples step runs clean.
