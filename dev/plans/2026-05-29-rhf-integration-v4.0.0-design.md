# ggRandomForests v4.0.0 — Random Hazard Forests (RHF) Integration Design

**Status:** Approved (brainstorm) — 2026-05-29
**Target release:** v4.0.0 (major)
**Integration branch:** `dev` (this doc is its first artifact)

## Goal

Add a `randomForestRHF` (Random Hazard Forests) visualization layer to
ggRandomForests, giving the package a **hazard-function** and
**time-varying-performance** story it currently lacks. RHF (Ishwaran &
Kogalur, CRAN, GPL ≥3) extends Random Survival Forests by directly
estimating the hazard and supporting time-dependent covariates (TDCs) via
counting-process input. It already builds on `varPro`, which ggRF
integrated in v3.0.0.

## Scope

Full RHF family, built in internal phases on the `dev` line, shipped as a
single major release **v4.0.0** — the same cadence by which the varPro
layer became v3.0.0.

## Architecture & object model

- **Suggests-gated, MIT-clean.** `randomForestRHF` (GPL ≥3) goes in
  `Suggests:` only. Every entry point gates on
  `requireNamespace("randomForestRHF", quietly = TRUE)` with an install
  hint (the `ggraph` pattern). ggRF's own source stays MIT; no GPL
  force-load at attach; RHF's OpenMP/compiled weight stays off ggRF's
  install path.
- **ggRF remains a visualization layer.** Extractors take an
  already-fitted `rhf` object — `gg_rhf(rhf_obj)` — exactly as
  `gg_rfsrc()` takes a fitted `rfsrc`. ggRF does **not** fit forests and
  does **not** re-export `convert.counting()`. Counting-process prep is
  the user's responsibility, documented in the vignette.
- **Hybrid, leaning new.** RHF's outputs mostly have no rfsrc analogue, so
  the bulk is new functions; reuse is at the level of *conventions and
  varPro idioms*, not literal S3-generic dispatch:
  - `gg_rhf` / `plot.gg_rhf` — per-case & ensemble hazard and
    cumulative-hazard curves.
  - `gg_auct` / `plot.gg_auct` — time-varying AUC + iAUC.
  - `gg_rhf_importance` / `plot.gg_rhf_importance` — time-windowed varPro
    importance (variable × time-window).
  - `gg_tune_rhf` / `plot.gg_tune_rhf` — tuning curves.
  - Reused idioms: provenance attributes, ribbon styling, the v3.0.0
    most-important-at-top ordering convention, `print`/`summary`/`autoplot`
    S3 companions, the memoised varPro-cache test fixture pattern,
    `.gg_header()` / `summary_methods.R` placement rules.
- **New tidy classes** `c("gg_rhf","data.frame")`, `c("gg_auct",
  "data.frame")`, etc., mirroring the existing family.

### Why importance is its own shape (not gg_vimp / gg_varpro reuse)

`importance.rhf` returns an `"importance.rhf"` object: a **variable ×
time-window z-matrix** (time-varying importance, hence RHF's own
`dotmatrix.importance.rhf`). It does not map onto `gg_vimp` (one VIMP per
variable) or `gg_varpro` (per-tree z boxplot). `varpro.cache.rhf` builds a
rule-membership cache with a `"regr"` working response that the importance
engine consumes. So `gg_rhf_importance` is a new wrapper with a
heatmap/dotmatrix default plot.

## Phase breakdown

All five phases land on `dev`; v4.0.0 ships them as a set.

- **Phase 0 — Prerequisite / dev-cycle.** Add `randomForestRHF` to
  `Suggests:`; open the dev version; capture a green baseline; add a
  namespace-hygiene / `requireNamespace`-gating regression guard. Version
  stays `3.0.0.9xxx` on `dev`; cut to `4.0.0` at the RC (the 2.7.3.9xxx →
  3.0.0 trick).
- **Phase 1 — `gg_rhf` (hazard/CHF curves).** Foundational headline.
  Establishes `rhf`-object handling + counting-process plumbing, the tidy
  frame, and `plot`/`print`/`summary`/`autoplot`. Per-case (`idx=`) and
  ensemble hazard & cumulative-hazard on `time.interest`, OOB vs inbag.
- **Phase 2 — `gg_auct` (time-varying performance).** Wraps `auct.rhf`
  AUC(t) + iAUC (Uno/standard), `marker = "chf"/"haz"`, bootstrap-SE
  ribbon.
- **Phase 3 — `gg_rhf_importance` (time-windowed importance).** Wraps
  `importance.rhf` / `varpro.cache.rhf`; heatmap of variable × time-window
  z-values.
- **Phase 4 — `gg_tune_rhf` (tuning).** Wraps `tune.treesize.rhf` /
  `tune.iAUC.rhf`.
- **Phase 5 — Vignette + RC.** Consolidated "Random Hazard Forests with
  ggRandomForests" vignette, family-support matrix, README, v4.0.0 NEWS
  finalization, full CRAN-Cookbook gate, cut to 4.0.0, submit.

Order rationale: Phase 1 is both the headline and the object-handling
foundation; importance (3) and tuning (4) are independent and could swap;
the data-model work is woven into Phase 1 and finalized in the vignette.

## Component tidy frames & default plots

### `gg_rhf` (Phase 1)
- **Input:** fitted `rhf` object.
- **Tidy frame** (long over `time.interest`): `id, time, hazard, chf,
  source` (`source` ∈ `oob`/`inbag`). Provenance attr: `ntime`, family,
  n.events, bootstrap.
- **Plot:** per-case curves via `idx=` (smoothed hazard and/or CHF,
  reusing RHF's `supsmu`/`scale.hazard` semantics in ggplot2) + optional
  ensemble mean; `hazard.only` / `chf.only` toggles; optional ribbon.
  Returns a single `ggplot`/`patchwork`.

### `gg_auct` (Phase 2)
- **Input:** `auct.rhf` object.
- **Tidy frame:** `time, auc, se, lower, upper, marker` (CI columns `NA`
  when no bootstrap). iAUC (`iAUC.uno`, `iAUC.std`, SEs) as a provenance
  attr.
- **Plot:** AUC(t) line + CI ribbon (existing ribbon styling), dashed 0.5
  reference, iAUC in the caption; `marker` drives colour when multiple
  markers are bound.

### `gg_rhf_importance` (Phase 3)
- **Input:** `importance.rhf` object (variable × time-window z-matrix)
  [+ optional `varpro.cache.rhf` reuse].
- **Tidy frame** (long): `variable, time_window, z, selected`.
- **Plot:** `geom_tile` heatmap (variable × time-window, fill = z),
  modeled on RHF's `dotmatrix.importance`; variable axis ordered
  most-important-at-top (v3.0.0 convention).

### `gg_tune_rhf` (Phase 4)
- **Input:** `tune.treesize.rhf` / `tune.iAUC.rhf` object.
- **Tidy frame:** `param (treesize), metric, value`.
- **Plot:** metric-vs-param line with the selected optimum marked.

All four ship `print` / `summary` / `autoplot` S3 companions and
provenance attributes.

## Cross-cutting

- **Data model.** Counting-process `Surv(id, start, stop, event)`,
  time ∈ [0,1], one event per id. Each extractor validates
  `inherits(x, "rhf"/"auct.rhf"/"importance.rhf")` and errors clearly if
  handed a standard `rfsrc` / `Surv(time, status)` object. The vignette
  shows `convert.counting()` + `rhf()` upstream.
- **Family-support matrix** (vignette): rows = the four `gg_*` functions;
  columns = time-static vs TDC, OOB vs inbag/test, `chf` vs `haz` marker.
- **Testing.** testthat + vdiffr snapshots (gated by `VDIFFR_RUN_TESTS`),
  `skip_on_cran()` for slow fits, `skip_if_not_installed("randomForestRHF")`,
  memoised helper fixtures (one cached `rhf` fit + `auct.rhf` +
  `varpro.cache.rhf`, à la `helper-beta_varpro`). Test data:
  `hazard.simulation()` for TDC, `pbc` / `peakVO2` for static.
- **Version mechanics.** Dev line `3.0.0.9xxx` → cut to `4.0.0` at the RC;
  DESCRIPTION + NEWS dual-version update; the standing release gate applies
  (full CRAN Cookbook audit + `R CMD check --as-cran` **with** manual
  build).

## Branching / workflow

- **`main`** = release line. Frozen at v3.0.0-submitted (`026f9e7c`) until
  CRAN accepts. CRAN-reviewer fixes: branch off `main` → PR to `main` →
  resubmit → tag `v3.0.0` on acceptance.
- **`dev`** = v4.0.0 integration line (off `main`). Each phase gets a
  feature branch → **PR into `dev`** (preserves PR + Copilot review + CI).
- At the v4.0.0 RC: one **release PR `dev` → `main`**, cut to `4.0.0`, run
  the full release gate, submit.
- CI must run on `dev`; periodically merge `main` → `dev` to absorb any
  CRAN hotfix; keep the snapshot-deletion discipline on the long-lived
  branch.
- Matches the established hazard `release/4.4` + `dev/v5.0-alpha` pattern.

## Deferred to v4.1.0

- **`gg_partial_rhf` (partial dependence).** RHF has **no native PDP
  function**. `predict.rhf(newdata=)` is the primitive (grid sweep →
  hazard/CHF per grid point), and `varPro::partialpro` is a possible path,
  but both would make ggRF own forest computation / counting-process grid
  construction — beyond its visualization-layer role. Revisit in v4.1.0:
  either wrap a native `partial.rhf` if upstream adds one, or verify the
  `partialpro`-on-`rhf` path. Pairs with the longstanding hazard-estimate
  issues (#4/#5/#71).

## References

- Lee, D.K., Chen, N., Ishwaran, H. (2021). Boosted nonparametric hazards
  with time-dependent covariates. *Annals of Statistics*, 49:2101–2128.
  <doi:10.1214/20-AOS2028>
- Ishwaran, H., Kogalur, U.B., Blackstone, E.H., Lauer, M.S. (2008).
  Random survival forests. *Ann. Appl. Statist.*, 2:841–860.
  <doi:10.1214/08-AOAS169>
- Ishwaran, H., Kogalur, U.B., Hsich, E.M., Lee, D.K. (2026). Random
  hazard forests.
