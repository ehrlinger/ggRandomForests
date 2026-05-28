# Fix: importance-plot vertical ordering + renumber to v3.0.0 — Design

**Date:** 2026-05-28
**Author:** John Ehrlinger (design via Claude brainstorming)
**Status:** Approved — ready for implementation planning
**Sequencing:** First of the two pre-RC housekeeping items. Followed by the CRAN audit cleanup (`\dontrun` → `\donttest`; chatty `message()`), then the v3.0.0 release candidate.

---

## Goal

Make every `gg_*` importance plot put the **most-important variable at the top** of the (post-`coord_flip`) horizontal layout, so the package's importance views read consistently. Fold in the agreed `v2.8.0 → v3.0.0` major-version renumber while the same documentation files are open.

## Problem (evidence-backed)

Verified locally against `varPro 3.1.0` + `randomForestSRC` on mtcars:

| Function | Top-of-plot variable | Most-important variable | Convention |
|---|---|---|---|
| `gg_vimp` | `wt` | `wt` | most-important **at top** ✓ |
| `gg_varpro` | `cyl` | `cyl` | most-important **at top** ✓ |
| `gg_beta_varpro` | `hp` (least) | `wt` | most-important **at bottom** ✗ |
| `gg_ivarpro` | (same construction as beta) | — | most-important **at bottom** ✗ |

Root cause: `gg_beta_varpro` and `gg_ivarpro` set the `variable` factor with **descending** levels (`ord_names <- names(sort(agg, decreasing = TRUE))`, then `factor(variable, levels = ord_names)`). With `coord_flip()`, the first factor level renders at the bottom, so the most-important variable (level 1) lands at the bottom. `gg_vimp` avoids this by reversing (`factor(vars, levels = rev(unique(vars)))`); `gg_varpro` builds ascending levels for the same reason.

This is a **vertical-direction** inconsistency, not a missing-ordering one. The per-facet alignment (all class facets sharing one row order) already works correctly in all four functions — only the direction is wrong in the two Phase 4 wrappers.

## Scope

One PR. Two code files (`gg_beta_varpro.R`, `gg_ivarpro.R`), their tests, their vdiffr snapshots, a new shared-convention test, the doc text describing the ordering, plus the v3.0.0 / v3.1.0 rename sweep.

---

## The fix

**Canonical convention (made explicit):** the `variable` factor's levels run **least-important first, most-important last**, so that after `coord_flip()` the most-important variable is at the top. This matches `gg_vimp` and `gg_varpro`.

### `R/gg_beta_varpro.R`

Both internal builders currently do:

```r
ord_names <- names(sort(agg, decreasing = TRUE))   # most-important first
... factor(variable, levels = ord_names) ...
```

Change the factor-level construction to reverse:

```r
ord_names <- names(sort(agg, decreasing = TRUE))    # keep: drives row sort + summary
lvl <- rev(ord_names)                               # least-important first → top after flip
... factor(variable, levels = lvl) ...
```

- `.gg_beta_varpro_regr`: reverse the levels on the `variable` factor.
- `.gg_beta_varpro_class`: reverse the **shared** levels (the unified total-|β| ordering) so every class facet flips together and stays aligned.
- Row sort order (`order(...)`) and the provenance / summary outputs keep using the descending `ord_names` so `summary()` still lists most-important first. Only the *factor levels* (which drive plot position) reverse.

### `R/gg_ivarpro.R`

Same change in `.gg_ivarpro_regr` and `.gg_ivarpro_class`: reverse the `variable` factor levels; keep the row sort and provenance descending.

### What does NOT change

- `gg_vimp`, `gg_varpro`, `plot.gg_varpro` conditional — already correct.
- The per-facet sharing logic — all class facets still derive from one ordering.
- The aggregation maths, cutoff resolution, provenance shape, `which_obs` / `which_class` filtering.
- `summary.*` ordering (still most-important first — it's a printed ranking, not a plot axis).

---

## Tests

### Updated per-function assertions

- `tests/testthat/test_gg_beta_varpro.R`: the test "variable factor levels ordered by descending mean|β|" currently asserts `levels(out$variable) == descending`. Flip to `levels(out$variable) == rev(descending)`. (For classification, same flip on the shared ordering.)
- `tests/testthat/test_gg_ivarpro.R`: the test "variable factor levels ordered by descending mean(|local_imp|)" — flip the same way.
- Aggregation-correctness, cutoff, shape-guard, which_obs/which_class tests are unaffected.

### New shared-convention test

`tests/testthat/test_plot_conventions.R` (new file):

- Build `gg_vimp` (rfsrc regr), `gg_varpro` (varpro regr), `gg_beta_varpro` (varpro regr), `gg_ivarpro` (varpro regr) on small fixed-seed fits (reuse the memoised varPro fixtures where possible; rfsrc fit is cheap).
- For each, assert the **last factor level** of the variable axis equals that function's own highest-importance variable (computed independently from the object). This pins "most-important is the last level → top after coord_flip" across the family and guards against a future wrapper reintroducing the bottom-heavy convention.
- Skip cleanly if `varPro` is unavailable.

### Snapshots

Re-record the affected vdiffr baselines under the `VDIFFR_RUN_TESTS` guard — the bars flip vertically:

- `gg-beta-varpro-default`
- `gg-beta-varpro-class-binary`, `gg-beta-varpro-class-multiclass`
- `gg-ivarpro-regr-distribution`, `gg-ivarpro-regr-which-obs`
- `gg-ivarpro-class-distribution`, `gg-ivarpro-class-which-obs`

No new snapshots; existing ones re-recorded.

---

## v3.0.0 / v3.1.0 rename sweep (folded in)

This release is renumbered from the working `v2.8.0` label to **`v3.0.0`** — varPro integration is a major scope expansion plus a soft-deprecation (`gg_partialpro`), which is major-version territory. Deferred-work references move from `v2.9.0` to `v3.1.0`.

**Dev version policy:** keep ticking `2.7.3.901x` through the housekeeping PRs (this PR: `2.7.3.9015`); cut `DESCRIPTION` straight to `3.0.0` at the RC. No `2.99.x` dev-suffix renumber.

**Text sweep (human-facing labels only — DESCRIPTION dev version stays `2.7.3.901x`):**

- `NEWS.md`: section heading `ggRandomForests v2.8.0 (development) — continued` → `ggRandomForests v3.0.0 (development) — continued`. Any in-bullet "v2.8.0" → "v3.0.0"; "tracked under Phase 4d / v2.9.0" style references → "v3.1.0".
- `vignettes/varpro.qmd`: "Headline document for v2.8.0" and any in-text version refs → v3.0.0; deferred "v2.9.0" refs in §5 / §6 → v3.1.0.
- Roxygen across the varPro wrappers: `@note` / `@section` lines that say "out of scope for this release … tracked … v2.9.0" → v3.1.0. Regenerate Rd via `devtools::document()`.
- `cran-comments.md`: the v2.8.0 draft header (if it's been committed by now) → v3.0.0. If `cran-comments.md` is still the v2.7.3 content, leave it — the v3.0.0 rewrite lands with the RC cut.

**Grep target:** `grep -rn "2\.8\.0\|2\.9\.0" R/ NEWS.md vignettes/*.qmd man/` to catch every reference; review each hit (some "2.8.0" may be inside historical NEWS entries that should NOT change — only the *current development* heading and forward-looking refs move).

---

## Files

- **Modify**: `R/gg_beta_varpro.R`, `R/gg_ivarpro.R` (factor-level reversal)
- **Modify**: `tests/testthat/test_gg_beta_varpro.R`, `tests/testthat/test_gg_ivarpro.R` (flip level assertions)
- **New**: `tests/testthat/test_plot_conventions.R` (shared-convention test)
- **Re-record**: 7 vdiffr SVGs under `tests/testthat/_snaps/snapshots/`
- **Modify**: `NEWS.md`, `vignettes/varpro.qmd`, roxygen in the varPro wrapper files (+ regenerated `man/*.Rd`), `DESCRIPTION` (version `2.7.3.9015`)

---

## Acceptance criteria

- `R CMD check --as-cran`: 0 / 0 / 0.
- `lintr::lint_package()`: 0 issues.
- `devtools::test()`: 0 failures. The new convention test passes for all four functions.
- `GG_IVARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true devtools::test()`: 0 failures; re-recorded snapshots stable.
- `grep -rn "v2\.8\.0\|v2\.9\.0"` over `R/ NEWS.md vignettes/ man/` returns only historical NEWS entries (no forward-looking or current-development references remain).
- One PR before the CRAN audit cleanup PR and the v3.0.0 RC.

## Out of scope

- `gg_vimp` / `gg_varpro` ordering (already correct).
- The DESCRIPTION cut to `3.0.0` (happens at RC, not here).
- The `cran-comments.md` v3.0.0 rewrite (lands with the RC).
- The CRAN audit cleanup (`\dontrun` → `\donttest`; chatty `message()`) — separate housekeeping PR.
