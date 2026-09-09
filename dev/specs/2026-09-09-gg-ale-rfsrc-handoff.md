# Handoff: `gg_ale_rfsrc()`, accumulated local effects for `rfsrc` forests

**Date:** 2026-09-09
**Status:** 🔴 **blocked on code transfer.** The implementation described below is
not in this repository and has never been in it.
**Target version:** 4.0.x (patch line; a new extractor family is the maintainer's
call on the minor digit, see "Versioning" below)
**Issue:** none filed. ggRandomForests has zero open issues as of 2026-09-09.

## Why this file exists

The work was done in a sandbox session that could not push. This note is written
from that session's own status report plus a verification pass against this
checkout, so that whoever picks it up knows which claims were checked and which
were taken on trust.

⚠️ **Nothing in this repo has been touched.** No `R/gg_ale_rfsrc.R`, no NAMESPACE
entry, no test file, no branch. Verified 2026-09-09 across the working tree, every
branch's file list (`git rev-list --all --objects`), the full text of every clone
under `~/Documents/GitHub/`, the Obsidian vault, and the issue list. The string
`ALE` does not appear anywhere.

## The blocker, first

🔴 **The code sits at `/home/claude/ggRandomForests-main/` in a sandbox, and that
path does not exist on this host.** It is an unpushed local clone in an
environment with no CRAN access, which is also why it was never
`R CMD check`ed — `randomForestSRC`, `ggplot2` and `dplyr` could not be
installed there.

So the first step is not code, it is transfer. In order of preference:

1. **Push the sandbox branch** to `origin` if that session can still reach the
   network. Cheapest, preserves authorship, and nothing below needs re-deriving.
2. **Hand over a diff** (`git diff main`, or `git format-patch`) and apply it here.
3. **Re-implement from the design sketch below.** Only if the first two are gone.
   The prose here is deliberately detailed enough to support this, but it is a
   reconstruction from a status report, not a specification anyone reviewed.

⚠️ **Do not treat this note as the source of truth for the algorithm.** It records
what the sandbox session said it built. The accumulation and centering steps were
reportedly validated against synthetic ground truth in a standalone prototype
before transcription; that prototype is also in the sandbox and is also not here.
If the code is lost, the validation is lost with it and has to be redone.

## What was reportedly built

| File | Contents |
|---|---|
| `R/gg_ale_rfsrc.R` | `gg_ale_rfsrc()`, mirroring `gg_partial_rfsrc()`'s API. `xvar2.name` gives second-order interaction ALE, continuous pairs only |
| `R/plot.gg_ale_rfsrc.R` | `plot.gg_ale_rfsrc()` (line/bar, faceted) and `plot.gg_ale_interaction()` (heatmap) |
| the shared method files | `print`, `summary` and `autoplot` methods added to the existing files |
| `NAMESPACE` | `S3method` entries for `autoplot`, `plot` and `print` |

Two of its claims check out against this checkout:

- ✅ **The API mirror is a real one.** `gg_partial_rfsrc()` (`R/gg_partial_rfsrc.R:119`)
  takes `rf_model, xvar.names, xvar2.name, newx, partial.time, partial.type,
  cat_limit, n_eval`. An ALE extractor has no use for `partial.time` or
  `partial.type` — both are survival-only — so mirroring the API means mirroring
  the argument *names and order* that survive the scope limit, not the whole
  signature.
- ✅ **The scope limit matches `gg_shap`'s.** `R/gg_shap.R:96` rejects anything
  outside `c("regr", "class")` with a named-family error message. A new extractor
  refusing survival is consistent with what shipped, not a gap.

## What is not done

From the sandbox session's own list:

- `NAMESPACE`: the `summary` `S3method` entries and `export(gg_ale_rfsrc)`.
- `tests/testthat/test_gg_ale_rfsrc.R`: does not exist. Only the validated
  prototypes, which are in the sandbox.
- `devtools::document()` and `R CMD check`: never run, no R environment there.

⚠️ **Hand-editing `NAMESPACE` is the wrong fix and this repo will undo it.**
`NAMESPACE` and `man/` are roxygen output (`AGENTS.md`, "Generated files: never
hand-edit"). The missing lines are a symptom of missing or wrong roxygen tags —
`@export` on `gg_ale_rfsrc()`, `@method summary gg_ale_rfsrc` where the summary
methods live — and `devtools::document()` writes them. Adding the four lines by
hand makes the tests pass once and the next `document()` reverts them.

## What the sandbox session could not have known

This is the part worth reading before writing any code. A new `gg_*` family is not
just two files; the suite pins conventions across the whole extractor surface, and
three of those gates apply automatically.

- 🔴 **`test_determinism.R:154` globs every `test_*` file** and parses it for
  `test_that()` blocks that touch the RNG without a `set.seed()` *inside the
  block*. A file-level seed does not satisfy it. So `test_gg_ale_rfsrc.R` is in
  scope the moment it exists, and every block that fits a forest needs its own
  seed or the suite fails — in `test_determinism.R`, not in the new file, which
  makes it read like an unrelated breakage.
- ⚠️ **`test_autoplot_equivalence.R:100` is a hand-written list**, not reflection.
  Its `objects <- list(...)` enumerates the classes it compares, so a new
  `gg_ale_rfsrc` class is **silently uncovered** until someone adds it. That file
  exists because an `autoplot.gg_vimp()` carrying an extra `labs(caption = ...)`
  passed every other test in the suite; a thin delegator that drifts is the
  documented failure mode for exactly this kind of addition. Add both new classes.
- ⚠️ **Every `plot()`/`autoplot()` method wants a `vdiffr` baseline** in
  `test_snapshots.R` (`AGENTS.md`: 58 baselines against 38 methods today). Two new
  plot methods means new baselines — and **regenerate them last**, after the final
  full-suite run, because a later run deletes a baseline the run itself did not
  produce.
- 🔴 **Run the suite with both environment variables set**, always:
  `NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'`. An unset
  `VDIFFR_RUN_TESTS` prunes all 58 baselines as "unused". `.Renviron` now inverts
  the default and `.githooks/pre-commit` blocks a commit that deletes them, but
  the hook needs `git config core.hooksPath .githooks` once per clone.
- **`plot()` returns, never prints**, and importance-style plots put the most
  important variable *last* in factor order so it lands on top after
  `coord_flip()`. Neither obviously binds an ALE curve, but `plot.gg_ale_interaction()`
  is a heatmap and heatmaps have the same axis-order trap.

### Test file: mirror the right thing

The sandbox note says to mirror `test_gg_partial_rfsrc.R`'s style, which is right
for shape and structure. But shape assertions are explicitly *not* considered
sufficient in this suite — `test_extractor_contracts.R` opens by saying so, in
these terms: a shape assertion passes just as happily when an extractor reads the
wrong field, transposes a matrix or silently reorders rows, "which is the failure
mode that actually matters in a visualisation layer: the plot still renders, and it
shows the wrong number."

⭐ **So ALE needs a contract test, not only a shape test, and it is the one place
where ALE is genuinely harder to test than partial dependence.** A partial-dependence
value can be cross-checked against a field of the source forest. An ALE value
cannot — it is accumulated from local differences across bins and then centered, so
there is no field on the `rfsrc` object to compare it against. Two properties are
checkable without a stored constant, and the house rule in that file is that no
assertion may compare against a number pasted from a previous run:

1. **The centering is exact.** A 1D ALE curve is centered, so its weighted mean over
   the bin populations is 0 to floating-point tolerance. This catches a centering
   step dropped or applied before accumulation rather than after.
2. **A known-additive model has a recoverable effect.** On synthetic data where the
   response is built as a known monotone function of one predictor, the ALE curve
   for that predictor must be monotone in the same direction. This is the closest
   available stand-in for the sandbox's ground-truth prototype and it does not
   depend on the forest's exact numbers.

Both belong in `test_extractor_contracts.R` alongside the other cross-checks, with
the shape and error-path tests in `test_gg_ale_rfsrc.R`.

## Versioning

`DESCRIPTION` is at **4.0.0**, unreleased, and v4 is the methods-extension line, so
a new extractor family is in scope for it. **Do not roll the minor digit** to 4.1.0
for this — that is the maintainer's call, made when a feature set is consolidated.
A patch bump is fine and must update **both** `DESCRIPTION` and the `Version:` line
in `NEWS.md`; a test greps `NEWS.md` for the exact `DESCRIPTION` version.

## Definition of done

The repo's standard gate, in order, plus the two additions this change needs:

```bash
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'                                  # 0 lints
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'   # 0 failures
```

then, once, from a clean `git archive` export rather than the working tree:

```bash
R CMD check --as-cran   # with the manual; do not pass --no-manual
```

Additions: the new classes added to `test_autoplot_equivalence.R`'s object list,
and the ALE contract tests above. `R CMD check` runs with `NOT_CRAN` false and so
exercises none of the `skip_on_cran()` tests; a green check is not evidence they
pass.

⚠️ **Watch the check-time budget.** CRAN declines a package whose overall
`R CMD check` exceeds about 10 minutes, and the rule bites at the incoming
pretest. ALE accumulates over bins across the predictor range, so the 2D
interaction path is the one to profile before it lands — `partialpro()` already
costs 47 seconds of the suite's 110 for two tests, which is the shape of mistake to
avoid repeating. Keep new fits small and put anything slow behind `skip_on_cran()`.

## A note on where this file sits

ggRandomForests has no `*handoff*` file on any branch; it uses paired
`YYYY-MM-DD-<topic>-design.md` and `-plan.md` specs, while the hvtiR family uses
handoffs. This note is a handoff because it was written as one, and because the
first task it describes is a transfer rather than a design decision. If the code
arrives and the work turns into ordinary implementation, the design half of this
belongs in a `-design.md` and this file should be superseded rather than grown.
