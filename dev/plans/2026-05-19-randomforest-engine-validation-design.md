---
tags: [r-package, ggRandomForests, randomForest, design-spec, validation]
created: 2026-05-19
status: approved-design-pre-plan
related: ["[[ggRandomForests]]"]
repo: ehrlinger/ggRandomForests
issues: ["#82 (meta)", "#80", "#81"]
scope: v2.8 cycle — randomForest engine validation & repair (own branch/PR, not the varPro Phase plans)
branch: fix/randomforest-engine-validation (off origin/main@f3ffd38)
---

# ggRandomForests — randomForest Engine Validation & Repair — Design

> Design spec home note. ggRF `docs/` is pkgdown's render target; tracked
> plans/specs live in-repo under `dev/plans/` (Rbuildignored via `^dev$`)
> and are mirrored to the Obsidian vault — same convention as the v2.8.0
> Phase 0 plan.

- **Date:** 2026-05-19
- **Status:** Approved design (pre-implementation-plan)
- **Author:** John Ehrlinger (design partner: Claude)
- **Meta-issue:** #82 (groups #80, #81)
- **Base:** `origin/main` @ `f3ffd38` (v2.8.0 Phase 0 merged: Imports
  migration, roxygen2 8.0.0, testthat `setup.R`, namespace-hygiene guard).
  Clean baseline verified: `devtools::test()` = `[ FAIL 0 | WARN 0 | SKIP 3 | PASS 678 ]`.

## 1. Motivation & Context

ggRandomForests supports two forest engines — `randomForestSRC` (rfsrc)
and `randomForest`. Development and testing have historically centered on
rfsrc; the `randomForest` paths are under-exercised. A validation pass
across the six `randomForest`-dispatching extractors surfaced real defects
with **no regression coverage guarding them**:

- **#80** — `plot(gg_variable(rf))` renders an empty figure (0 ggplot
  layer rows) for `randomForest`, classification *and* regression.
  **Root cause (pinned):** `gg_variable.randomForest` returns a *correct*
  `gg_variable` data.frame (e.g. 150×6 for iris: 4 predictors + `yhat` +
  `yvar`); the defect is in **`plot.gg_variable`**, which returns a bare
  `list` (not a `ggplot`/`patchwork`) for this case, so `ggplot_build`/
  `layer_data` fails and nothing is drawn. This is the *same bug class*
  the v2.7.3 #77/#78 work fixed for `plot.gg_partial*` (collapsing a
  `list(continuous=, categorical=)` return into one `patchwork`).
- **#81** — `gg_roc()`/`calc_roc()` produce a degenerate ROC for
  `randomForest` classification: ~3 points and AUC ≈ 0.5 even on
  trivially separable data, even when `which_outcome` is passed
  correctly. **Root cause (pinned):** `gg_roc.randomForest()` calls
  `calc_roc(object, object$y, which_outcome=)`; the `calc_roc.randomForest`
  path thresholds *hard class labels* rather than OOB vote probabilities,
  so an n-class problem yields only n operating points and AUC collapses
  toward chance. Secondary: when `which_outcome` is omitted it becomes
  `"all"`, and `.validate_which_outcome("all")` (`R/calc_roc.R:16-22`)
  `warning("Must specify which_outcome for now.")` then silently returns
  `1L` — the default user call both warns and arbitrarily picks class 1.

These are bugs in documented, NAMESPACE-exported functionality →
appropriate for the **v2.8 cycle** as bug fixes. This is **not** the
varPro integration plan and **not** a rfsrc change.

## 2. Goals / Non-Goals

**Goals**
- Every supported `randomForest` extractor × applicable family is correct
  and **permanently guarded** by regression tests.
- Fix #80, #81, the minor `plot.gg_rfsrc` regression colour-label wart.
- Document the intentionally-rfsrc-only families.

**Non-Goals (scope guard)**
- Per-class faceted multi-class ROC and ROC confidence intervals — these
  stay with meta-issue **#72**. This plan delivers only a single honest
  default curve (macro-average) plus correct per-class ROC.
- Any `rfsrc`-path behavior change (the shared `plot.gg_variable`
  unification must *not* regress rfsrc — guarded by existing rfsrc tests).
- varPro / gg_* component work (the v2.8.0 varPro Phases).
- No new ROC CI/multi-class API.

## 3. Locked Decisions

| Topic | Decision |
|---|---|
| Scope | Full randomForest engine audit (all 6 extractors × class/regr) |
| Approach | **Coverage-first / TDD**: build the validation matrix first (red), then fix to green, then docs |
| Default `gg_roc(rf)` / `which_outcome="all"` | **Macro-average one-vs-rest ROC** over corrected per-class probabilities; no warning. Per-class faceting + CIs → #72 |
| Test infra | **Extend existing files**; reuse helpers — no parallel infra |
| Sequencing | Spec + plan now; **execute on this branch off `main`@`f3ffd38`** (Phase 0 already merged) |
| Versioning | Plan's first task bumps `DESCRIPTION` to the next unused `2.7.3.900x` *at execution time* (ordering vs varPro phases is nondeterministic — do not hardcode); NEWS bullet under the existing `v2.8.0 (development)` section |
| Storage | `dev/plans/` in-repo (Rbuildignored `^dev$`) + vault mirror |
| Execution model | subagent-driven-development, per-task spec + code-quality review (same rigor as Phase 0) |
| Issue tracking | Meta **#82** groups #80/#81; closes them when complete |

## 4. The Validation Matrix (durable deliverable)

Supported `randomForest` extractors: `gg_rfsrc`, `gg_error`, `gg_vimp`,
`gg_variable`, `gg_roc`, `calc_roc`. `gg_partial`/`gg_survival`/`gg_brier`
have **no** `randomForest` method by design.

**(a) Structural guards → `tests/testthat/test_plot_layer_data.R`**
(reuse the existing `expect_layer_nonempty()` / `expect_layer_has_variation()`
helpers), one `randomForest` case per supported cell:

| Extractor | classification (iris) | regression (mtcars / Boston) |
|---|---|---|
| `gg_rfsrc` | non-empty + variation | non-empty + variation; assert **no stray `colour="Outcome"`** label (ties the minor wart) |
| `gg_error` | non-empty | non-empty |
| `gg_vimp` | non-empty; also non-formula `randomForest(x, y)` interface | non-empty (fixture grown with `importance = TRUE`) |
| `gg_variable` | non-empty ← **#80 (red first)** | non-empty ← **#80 (red first)** |
| `gg_roc` | non-empty ← **#81 (red first)** | n/a (no probabilities for regression) |

**(b) ROC numeric-correctness → `tests/testthat/test_gg_roc.R`**
(correctness lives with the function's own tests, not the layer suite):
- `calc_auc(gg_roc(rf, which_outcome = <setosa>))` ≈ 1.0 on iris
  (separable; tolerance: AUC ≥ 0.98).
- ROC point count `> nlevels(response)` (catches the degenerate
  3-row case).
- Default `gg_roc(rf)` (macro-average) returns a many-point curve with
  AUC well above 0.5 **and emits no warning**.

**(c) vdiffr snapshots → `tests/testthat/test_snapshots.R`**, added inside
that file's *existing* opt-in + `requireNamespace("vdiffr")` guarded
block, naming `"<plot> <type> randomForest"` parallel to the rfsrc
snapshots, for every supported family × type.

**Coverage-first ordering nuance:** structural/correctness assertions for
#80/#81 are written **first and fail red**. vdiffr snapshots for the
currently-broken families (`gg_variable` RF, `gg_roc` RF) are baselined
**after** their fix (never snapshot a broken plot as "correct").
Snapshots for the already-OK RF families (`gg_rfsrc`/`gg_error`/`gg_vimp`)
may be baselined upfront. Step (a) is also a discovery pass — if it
surfaces any *sibling* RF defect beyond #80/#81, log it under meta #82.

## 5. The Fixes

**#80 — `plot.gg_variable` list return.** `plot.gg_variable` returns a
plain `list` instead of a single `ggplot`/`patchwork` for the
`randomForest` case (verify the regression branch too). Fix: unify
`plot.gg_variable` to always return one `ggplot`/`patchwork`
(combine continuous/categorical or per-variable panels via
`patchwork::wrap_plots()`), mirroring the Phase-0/v2.7.3
`plot.gg_partial*` unification. The extractor `gg_variable.randomForest`
is correct — **do not modify it**. `systematic-debugging` localizes the
exact list-returning branch before the fix. The shared rfsrc
`gg_variable` path must not regress — existing rfsrc layer-data and
snapshot tests are the guard.

**#81 — `calc_roc.randomForest` uses hard labels, not probabilities.**
Drive the curve from **OOB vote probabilities** (`object$votes` — OOB
class proportions for `randomForest` classification; fall back to
`predict(object, type = "prob")` only if `$votes` is absent), sweeping
thresholds over the continuous score for the selected class. The
default / `which_outcome = "all"` case returns a **macro-average
one-vs-rest** curve over the corrected per-class probabilities (replaces
the `.validate_which_outcome` warn-and-guess); no warning. Per-class
faceting + CIs are out of scope (#72). Document the macro-average
semantics in the `gg_roc` Rd.

**Minor wart — `plot.gg_rfsrc` regression.** A stray
`labs(colour = "Outcome")` is applied on the regression path where no
`colour` aesthetic is mapped → `Ignoring unknown labels`. Fix: only set
the colour label when a `colour` aesthetic exists (guard the regression
branch). Guarded by the §4(a) `gg_rfsrc` regression assertion.

**Rd docs — intentionally rfsrc-only families.** Add a one-line
`@details`/`@note` to `gg_partial`, `gg_survival`, `gg_brier` stating
`randomForest` objects are unsupported and why (no partial-dependence /
survival / Brier for `randomForest`). No code change. Regenerated `man/`
committed (roxygen2 8.0.0, matching `Config/roxygen2/version`).

## 6. Verification Gates

1. Write §4 matrix (structural + ROC-correctness) → run → confirm
   **#80/#81 fail red for the right reasons**; record any sibling defects.
2. `systematic-debugging` → fix `plot.gg_variable`,
   `calc_roc.randomForest`, the `gg_rfsrc` wart until the matrix is green.
3. Baseline vdiffr snapshots (fixed families + already-OK families).
4. Rd docs for rfsrc-only families; `devtools::document()` (roxygen2
   8.0.0 — `DESCRIPTION` must stay byte-identical except intended
   changes; `man/` regen committed).
5. **Gate:** fresh `R CMD check --as-cran` **0 errors / 0 warnings /
   0 notes** (capture a new baseline at this branch point —
   `f3ffd38`, *not* the stale Phase 0 Task 3 log) **and**
   `devtools::test()` `FAIL 0` with `PASS` increased by the new RF
   matrix cases relative to the 678 baseline. All Phase 0 invariants
   (namespace-hygiene guard, etc.) stay green.

## 7. Risks & Mitigations

- **Shared `plot.gg_variable`:** unifying the return must not regress the
  rfsrc path. Mitigation: existing rfsrc `gg_variable` layer-data +
  snapshot tests must stay green (built-in regression guard).
- **`object$votes` availability:** present by default for `randomForest`
  classification; handle absence gracefully (`predict(type="prob")`
  fallback); document macro-average semantics.
- **vdiffr device sensitivity** (post-ggplot2 4.0.3): strictly follow
  `test_snapshots.R`'s existing opt-in + `requireNamespace` guard so CI
  is not flaked; broken-family snapshots baselined only after fix.
- **Regression fixtures:** `gg_vimp` regression requires the forest grown
  with `importance = TRUE`.
- **Scope creep into #72:** the macro-average default is the *only*
  multi-class behavior delivered here; per-class/CI explicitly deferred.

## 8. Out of Scope (reaffirmed)

No rfsrc behavior change; no #72 multi-class/CI work; no varPro work;
no new public API beyond the macro-average default behavior of an
existing function.
