---
tags: [r-package, ggRandomForests, randomForest, design-spec, validation]
created: 2026-05-19
revised: 2026-05-19 (root causes corrected against source before plan)
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
rfsrc; the `randomForest` paths are under-exercised, and a validation
pass surfaced defects with **no regression coverage guarding them**.
Root causes below were verified against source on `f3ffd38` (this
section was corrected after the initial draft mis-stated all three):

- **#80 — `plot.gg_variable` returns a bare `list` for multi-`xvar`
  (engine-agnostic API-consistency defect; surfaced via randomForest).**
  `plot.gg_variable` has **no engine branch** (`R/plot.gg_variable.R:119-127`);
  for the non-`panel` path it pre-allocates `gg_plt <- vector("list", ...)`
  (`:412-414`) and collapses to a single object **only when `lng == 1`**
  (`:604-607`), otherwise returns the raw `list`. This is *documented*
  (`@return` `:29-31`: "Otherwise a named list of ggplot objects") and
  *contract-tested* (`tests/testthat/test_gg_variable.R:38-41`:
  `expect_type(gg_plt, "list")`). The `gg_variable.randomForest`
  *extractor* is correct (e.g. iris → 150×6 data.frame). The "empty
  plot" symptom = calling `plot(gg_variable(rf))` with the default
  `xvar` (all predictors → `lng > 1`) → the documented list → `layer_data`
  cannot build a list. **This is engine-agnostic** (identical for rfsrc;
  existing rfsrc gg_variable tests pass only because they pass a single
  `xvar`/`panel=TRUE`). It is the *same bare-list API inconsistency*
  v2.7.3 #77/#78 deliberately removed from `plot.gg_partial*` (NEWS
  v2.7.3). Fixing it is therefore an **intentional breaking behavior
  change** on the established v2.7.x trajectory — not a randomForest bug
  and not a one-liner.
- **#81 — `calc_roc.randomForest` degenerate ROC: a matrix
  mis-indexing bug, not "hard labels vs probabilities".**
  `calc_roc.randomForest` *already* uses probabilities:
  `prd <- predict(object, type = "prob")` (`R/calc_roc.R:160`). The
  degeneracy is `:166` `pct <- sort(unique(prd[[which_outcome]]))` —
  `prd` is a **matrix**, so `prd[[1]]` extracts a single *scalar* (not
  the class column) → one threshold → ~3-row ROC → AUC ≈ 0.5. The
  `cbind(res = ..., prd = prd)` flattening (`:164`) and
  `dta_roc[, c(1, 1 + which_outcome)]` (`:176`) compound the column
  confusion. Correct index is `prd[, which_outcome]`. Secondary defects:
  (a) `gg_roc.randomForest` (`R/gg_roc.R:134,153-156`) **drops the `oob`
  argument** (not passed to `calc_roc`; signature has no default) and
  `calc_roc.randomForest` uses in-bag `predict(type="prob")`, contradicting
  the documented `oob` parameter and the rfsrc method's OOB semantics;
  (b) default / `which_outcome="all"` → `.validate_which_outcome("all")`
  (`R/calc_roc.R:16-22`) warns `"Must specify which_outcome for now."`
  then silently returns `1L`.
- **Minor wart — `plot.gg_error.R:244,248`** unconditionally apply
  `ggplot2::labs(..., color = "Outcome")` even on the regression /
  single-outcome path where no `colour` aesthetic is mapped →
  `Ignoring unknown labels: colour "Outcome"`. (Originally mis-located
  to `plot.gg_rfsrc.R`; corrected here.)

These are defects in documented, NAMESPACE-exported functionality →
appropriate for the **v2.8 cycle**. This is **not** the varPro
integration plan. #80's fix is engine-agnostic and *intentionally
changes rfsrc-visible behavior* (the list→single-object contract);
that is in scope and on the v2.7.x #77/#78 trajectory. No other rfsrc
behavior changes.

## 2. Goals / Non-Goals

**Goals**
- Every supported `randomForest` extractor × applicable family is correct
  and **permanently guarded** by regression tests.
- Fix #80 (`plot.gg_variable` list→single object), #81
  (`calc_roc.randomForest` indexing + `oob`/macro-average), and the
  minor `plot.gg_error` `color="Outcome"` unknown-label wart.
- Document the intentionally-rfsrc-only families.

**Non-Goals (scope guard)**
- Per-class faceted multi-class ROC and ROC confidence intervals — these
  stay with meta-issue **#72**. This plan delivers only a single honest
  default curve (macro-average) plus correct per-class ROC.
- Any rfsrc *analytical/statistical* behavior change, and any change to
  the rfsrc single-`xvar` / `panel=TRUE` plot paths. (The #80
  `plot.gg_variable` multi-`xvar` list→single-object change **is
  intentionally engine-agnostic** — it changes the documented multi-`xvar`
  *return type* for rfsrc too, on the v2.7.x #77/#78 trajectory; that is
  in scope. "No rfsrc change" applies to everything *except* this
  deliberate, documented return-contract unification.)
- varPro / gg_* component work (the v2.8.0 varPro Phases).
- No new ROC CI / multi-class API (→ #72).

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
| `gg_rfsrc` | non-empty + variation | non-empty + variation |
| `gg_error` | non-empty | non-empty; **`expect_no_warning` / no "Ignoring unknown labels"** (ties the `plot.gg_error` colour-label wart) |
| `gg_vimp` | non-empty; also non-formula `randomForest(x, y)` interface | non-empty (fixture grown with `importance = TRUE`) |
| `gg_variable` | `plot(gg_variable(rf))` with **default multi-`xvar`** is a single `ggplot`/`patchwork` (`expect_s3_class(p,"ggplot")` ∪ `"patchwork"`) + non-empty layer ← **#80 (red first: currently a bare `list`)** | same ← **#80 (red first)** |
| `gg_roc` | non-empty ← **#81 (red first)** | n/a (no probabilities for regression) |

The `gg_variable` row asserts the **new return contract** (#80 fix). The
currently-passing contract test `tests/testthat/test_gg_variable.R:38-41`
(`expect_type(gg_plt, "list")`) and the `plot.gg_variable` `@return`
roxygen (`:29-31`) both encode the *old* list contract and **must be
rewritten** as part of the #80 fix (single `ggplot`/`patchwork`), with a
NEWS bullet mirroring the v2.7.3 #77/#78 entry. rfsrc single-`xvar` /
`panel=TRUE` tests (`test_plot_layer_data.R:358-377`,
`test_gg_variable.R:32,46,116`) stay unchanged and are the
no-regression guard.

**(b) ROC numeric-correctness → `tests/testthat/test_gg_roc.R`**
(correctness lives with the function's own tests, not the layer suite):
- `calc_auc(gg_roc(rf, which_outcome = <setosa>))` ≈ 1.0 on iris
  (separable; tolerance: AUC ≥ 0.98).
- ROC point count `> nlevels(response)` (catches the degenerate
  3-row case).
- Default `gg_roc(rf)` (macro-average) returns a many-point curve with
  AUC well above 0.5 **and emits no warning**.

**(c) vdiffr snapshots → `tests/testthat/test_snapshots.R`**, added inside
that file's *existing* guarded block — literal condition
`if (requireNamespace("vdiffr", quietly = TRUE) &&
identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true"))` (`test_snapshots.R:19-20`),
using the existing `local({...})` fixture idiom and
`vdiffr::expect_doppelganger("<plot> <type> rf", plot(gg_dta))` naming
(parallel to the existing `"... rfsrc"` snapshots), for every supported
family × type.

**Coverage-first ordering nuance:** structural/correctness assertions for
#80/#81 are written **first and fail red**. vdiffr snapshots for the
currently-broken families (`gg_variable` RF, `gg_roc` RF) are baselined
**after** their fix (never snapshot a broken plot as "correct").
Snapshots for the already-OK RF families (`gg_rfsrc`/`gg_error`/`gg_vimp`)
may be baselined upfront. Step (a) is also a discovery pass — if it
surfaces any *sibling* RF defect beyond #80/#81, log it under meta #82.

## 5. The Fixes

**#80 — `plot.gg_variable` multi-`xvar` list → single object
(intentional, engine-agnostic API change).** In the non-`panel` branch
(`R/plot.gg_variable.R:412-607`) `plot.gg_variable` builds
`gg_plt <- vector("list", lng)` and returns it directly unless
`lng == 1`. Fix: when `lng > 1` and `!panel`, return a single
`patchwork` by `patchwork::wrap_plots(gg_plt, ncol = 1)` instead of the
bare list (single var → unchanged single `ggplot`; `panel=TRUE` →
unchanged faceted `ggplot`). This mirrors the v2.7.3 #77/#78
`plot.gg_partial`/`plot.gg_partial_rfsrc` pattern
(`R/plot.gg_partial.R:87-93`: three-way `if/else if/else` ending in
`wrap_plots(..., ncol = 1)`). `@importFrom patchwork wrap_plots` is
already declared in the package. **This is an intentional breaking
return-contract change for both engines** (the spec §1/§2 scope this
in). Required companion edits, all in the #80 task:
- Rewrite `plot.gg_variable` `@return` roxygen (`:29-31`) to state a
  single `ggplot`/`patchwork` is always returned.
- Rewrite the contract test `tests/testthat/test_gg_variable.R:38-41`
  from `expect_type(gg_plt, "list")` to
  `expect_s3_class(gg_plt, c("patchwork","ggplot"))` (and audit
  `:106` which calls `plot.gg_variable(gg_dta)` default-multi-xvar).
- NEWS bullet under `v2.8.0 (development)` mirroring the #77/#78 wording
  ("…now always returns a single ggplot/patchwork; previously a bare
  list, which surprised users / broke `autoplot` & `layer_data`").
- The extractor `gg_variable.randomForest` is **correct — do not
  modify it.** `systematic-debugging` confirms the exact branch before
  editing. rfsrc single-`xvar`/`panel` tests must stay green.

**#81 — `calc_roc.randomForest` matrix mis-indexing + `oob`
plumbing + macro-average default.** `calc_roc.R:160` already uses
probabilities (`prd <- predict(object, type = "prob")`). The degeneracy
is `:166` `sort(unique(prd[[which_outcome]]))` — `prd` is a **matrix**,
`prd[[k]]` returns one scalar → ~1 threshold → 3-row ROC → AUC ≈ 0.5.
Fixes (one task):
- Index the class **column**: derive the score vector as
  `prob[, which_outcome]` from a proper probability matrix; rebuild
  `dta_roc`/the threshold sweep so `:164,176-178` use the correct
  column (mirror the working `calc_roc.rfsrc` shape, `:97-140`).
- **OOB semantics:** use `object$votes` (randomForest OOB class
  proportions) when `oob = TRUE`, else `predict(object, type="prob")`;
  fall back to `predict` if `$votes` is absent. Fix `gg_roc.randomForest`
  (`R/gg_roc.R:134,153-156`) to give `oob = TRUE` a default (matching
  `gg_roc.rfsrc`) and **pass `oob` through** to `calc_roc` (currently
  dropped).
- **Default / `which_outcome="all"`:** replace the
  `.validate_which_outcome("all")` warn-and-return-`1L` (`calc_roc.R:16-22`)
  with a **macro-average one-vs-rest** curve over the corrected
  per-class probabilities; no warning. Per-class faceting + CIs are
  out of scope (#72). Document macro-average + `oob` semantics in the
  `gg_roc` Rd (`R/gg_roc.R:26-33`). **Definitive: do NOT modify the
  shared `.validate_which_outcome` (`calc_roc.R:16-22`) nor
  `calc_roc.rfsrc`.** The macro-average is implemented entirely within
  the randomForest path (`calc_roc.randomForest` /
  `gg_roc.randomForest`), e.g. by handling `which_outcome %in% c("all", 0)`
  there *before* delegating, so `calc_roc.rfsrc` and the shared helper
  are byte-unchanged and rfsrc behavior is provably untouched (a
  characterization test on `calc_roc.rfsrc` for an explicit
  `which_outcome` is still added as a guard).

**Minor wart — `plot.gg_error.R:244,248`.** Both `labs()` calls pass
`color = "Outcome"` unconditionally; on the regression / single-outcome
error plot no `colour` aesthetic is mapped → `Ignoring unknown labels`.
Fix: only include `color = "Outcome"` in `labs()` when a `colour`
aesthetic is actually mapped (i.e. the multi-outcome / classification
branch); omit it on the regression / single-outcome branch. Guarded by
the §4(a) `gg_error` regression `expect_no_warning` assertion.

**Rd docs — intentionally rfsrc-only families.** Add a one-line
`@details`/`@note` to `gg_partial`, `gg_survival`, `gg_brier` stating
`randomForest` objects are unsupported and why (no partial-dependence /
survival / Brier for `randomForest`). No code change. Regenerated `man/`
committed (roxygen2 8.0.0, matching `Config/roxygen2/version`).

## 6. Verification Gates

1. Write §4 matrix (structural + ROC-correctness) → run → confirm
   **#80/#81 fail red for the right reasons**; record any sibling defects.
2. `systematic-debugging` → fix `plot.gg_variable` (list→single object
   + `@return`/contract-test/NEWS), `calc_roc.randomForest`
   (indexing + `oob` + macro-average), the `plot.gg_error`
   `color="Outcome"` wart, until the matrix is green.
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

- **Shared `plot.gg_variable` (breaking return-contract change):** the
  multi-`xvar` list→single-object change is *intentional and engine-
  agnostic*. Risk is regressing the **single-`xvar`/`panel=TRUE`** paths
  or rfsrc *statistical* output. Mitigation: existing rfsrc single-`xvar`
  /panel layer-data + snapshot tests stay green; the only intentionally
  changed tests are the documented multi-`xvar` list-contract ones
  (`test_gg_variable.R:38-41`, `:106`) + `@return` + a NEWS bullet.
  Audit `autoplot.gg_variable` (`R/autoplot_methods.R:77`) and examples
  for any reliance on the old list return.
- **`calc_roc.rfsrc` must not change:** per §5 the #81 fix
  **does not modify** the shared `.validate_which_outcome` or
  `calc_roc.rfsrc` (macro-average lives only in the randomForest path).
  Defensive guard: a characterization test pinning `calc_roc.rfsrc`
  output for an explicit `which_outcome`, plus `git diff` confirming
  `calc_roc.R:16-22` and `calc_roc.rfsrc` are byte-unchanged.
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
