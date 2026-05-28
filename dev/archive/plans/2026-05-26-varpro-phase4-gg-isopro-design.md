# ggRandomForests v2.8.0 — varPro Phase 4: gg_isopro Design Spec

**Date:** 2026-05-26
**Author:** John Ehrlinger (design via Claude brainstorming)
**Status:** Approved — ready for implementation planning
**Sequencing:** First of three Phase 4 sub-projects (`gg_isopro` → `gg_beta_varpro` → `gg_ivarpro`). Lands as one PR before the v2.8.0 release candidate.

---

## Goal

Add a tidy-data wrapper and ggplot2 plot method for `varPro::isopro` —
isolation-forest anomaly detection — so users can extract per-observation
anomaly scores into a familiar `gg_*` object and visualise them with the
package's existing plot/print/summary conventions.

## Scope

A single sub-project. Three Phase 4 functions remain (`isopro`,
`beta.varpro`, `ivarpro`); each gets its own brainstorm → spec → plan → PR
cycle, matching the Phase 1–3 pattern. This spec covers `gg_isopro` only.
`shap.ivarpro` and `outpro` are out of scope; they may be picked up later
under `ivarpro` or in a follow-on release.

---

## Architecture

```
varPro::isopro fit  →  gg_isopro()       (extractor)
                          │
                          └── tidy data.frame
                              │   columns: obs, case.depth, howbad
                              │   class:   c("gg_isopro", "data.frame")
                              │   attr("provenance"): source/method/n/ntree
                              │
                          plot.gg_isopro(panel, threshold, top_n_pct, ...)
                          print.gg_isopro
                          summary.gg_isopro
                          autoplot.gg_isopro
```

All computation lives in the extractor. The plot method is pure rendering.

---

## Extractor

```r
gg_isopro(object)
```

- **Input:** an `isopro` fit from `varPro::isopro`. Method dispatch via
  `gg_isopro.isopro`; no other args.
- **Output:** a tidy `data.frame`, one row per observation, columns:

  | column | type | meaning |
  |--------|------|---------|
  | `obs` | integer | observation index, 1..n |
  | `case.depth` | numeric | average isolation depth (lower = more anomalous) |
  | `howbad` | numeric, [0, 1] | anomaly score (higher = more anomalous) |

- **Class:** `c("gg_isopro", "data.frame")`.
- **Provenance attribute** (consumed by print/summary):
  `source = "varPro::isopro"`, `method`, `n`, `ntree`. Method is extracted
  from the underlying `isoforest$call`.
- **Multi-method comparison.** Single fit per call, matching Phase 1–3.
  Users compare methods by calling `gg_isopro()` three times and
  `dplyr::bind_rows()` with a `method` label column, the same pattern as
  multi-model `gg_brier`. The plot method auto-detects the `method` column.

## Plot method

```r
plot(gg, panel = c("both", "elbow", "density"),
        threshold = NULL, top_n_pct = NULL, ...)
```

- `panel = "both"` (default) → a `patchwork` of two `ggplot` objects:
  - **Elbow** (left): observations sorted by `howbad` ascending; x = rank
    index, y = `howbad`, drawn as a line. The anomalous tail is the
    top-right corner. Multi-method: one coloured line per method.
  - **Density** (right): `geom_density(aes(x = howbad))`, sharing the
    `howbad` axis rotated. Multi-method: one filled density per method.
- `panel = "elbow"` → single `ggplot`, the elbow only.
- `panel = "density"` → single `ggplot`, the density only.
- `match.arg(panel)` enforces the three valid values.

**Threshold annotation.** Optional, opt-in:

- `threshold` (numeric in `[0, 1]`, score-space): draws a reference line at
  that `howbad` value — `geom_hline` on the elbow, `geom_vline` on the
  density. Points above the line on the elbow are shaded.
- `top_n_pct` (numeric in `(0, 100)`, quantile-space): resolves to the
  matching `howbad` quantile internally, then draws the same reference line.
- Both `NULL` → no annotation.
- Both set → `threshold` wins, with a `message()` saying so. Matches the
  `which_outcome` vs `per_class` precedent in `gg_roc`.

**Multi-method dispatch.** `"method" %in% names(gg)` decides whether
`colour`/`fill` aesthetics are wired up. No `method` column → single black
line and single grey density.

**Return contract.** Single-panel calls return a plain `ggplot`; the
two-panel call returns a `patchwork`. Same shape rule as `plot.gg_variable`
and `plot.gg_partial*`.

## S3 companions

- `print.gg_isopro(x)` — one-line header via `.gg_header()`:
  `gg_isopro: <n> observations, method = "<rnd|unsupv|auto>", ntree = <n>`.
  Header-only — use `head()` for rows. Appended to `R/print_methods.R`.
- `summary.gg_isopro(object)` — `.summary_skel()` body: rows, range of
  `howbad`, quantile snapshot (5th/50th/95th). Appended to
  `R/summary_methods.R`.
- `autoplot.gg_isopro(object)` — thin delegate to `plot.gg_isopro`.
  Appended to `R/autoplot_methods.R`.

## Dependencies

- `varPro` is already in `Imports:` (graduated in Phase 1). No new
  dependencies.
- `patchwork` is already in `Imports:`.
- No `DESCRIPTION` changes except the v2.7.3.9008 dev-version bump.

## Files

- **New:** `R/gg_isopro.R`, `R/plot.gg_isopro.R`,
  `tests/testthat/test_gg_isopro.R`.
- **Append:** `print.gg_isopro` to `R/print_methods.R`; `summary.gg_isopro`
  to `R/summary_methods.R`; `autoplot.gg_isopro` to `R/autoplot_methods.R`.
- **Edit:** `DESCRIPTION` (version), `NEWS.md` (entry), `tests/testthat/test_snapshots.R` (snapshots).

## Tests

Mirroring Phase 1–3 coverage:

1. Extractor: returns `c("gg_isopro", "data.frame")`; columns
   `obs`/`case.depth`/`howbad`; `howbad ∈ [0, 1]`; `nrow == n`; provenance
   attribute attached with the expected fields.
2. Multi-fit `bind_rows` with a `method` label column: a `method` column
   survives the extractor and reaches the plot method.
3. `plot(gg, panel = "both")` returns `patchwork` with 2 sub-plots; both
   `ggplot_build` cleanly.
4. `plot(gg, panel = "elbow")` returns `ggplot` (not `patchwork`); builds.
5. `plot(gg, panel = "density")` returns `ggplot` (not `patchwork`); builds.
6. `plot(gg, threshold = 0.8)` adds a reference layer on both sub-plots of
   the `panel = "both"` result.
7. `plot(gg, top_n_pct = 5)` adds the reference layer; the resolved
   threshold matches the 95th percentile of `howbad`.
8. `plot(gg, threshold = 0.8, top_n_pct = 5)` emits a `message()` and uses
   `threshold` for the annotation.
9. Multi-method plot: elbow layer has more than one distinct colour group.
10. `print` / `summary` return expected shapes (`summary.gg` class for
    summary; invisible return for print).

## Snapshots

Two `vdiffr::expect_doppelganger()` calls under `VDIFFR_RUN_TESTS = true`:

- `gg-isopro-default` — `plot(gg)` with default args.
- `gg-isopro-threshold` — `plot(gg, threshold = 0.8)`.

Both skip cleanly without the env var. Baselines are recorded on the first
CI run that opts in.

## Acceptance criteria

- `R CMD check --as-cran`: 0 errors / 0 warnings / 0 notes.
- Full `devtools::test()`: 0 failures.
- New tests (#1–10) pass; old tests still pass.
- Documentation: roxygen produces valid Rd; `pkgdown` builds.
- Voice: roxygen written against `dev/voice-fingerprint.md` from the start
  (narrative + terse registers), so no follow-on audit is needed.

## Out of scope

- Ground-truth evaluation (ROC, precision-recall via
  `varPro::get.iso.performance`). Deferred to a future `gg_iso_performance`
  sub-project, as discussed in brainstorming.
- `shap.ivarpro` and `outpro` integrations. Tracked as possible additions
  alongside `gg_ivarpro` (the next Phase 4 sub-project) or in v2.9.
- `predict.isopro` integration on new data. The extractor takes an existing
  fit; scoring new data is a separate workflow.
