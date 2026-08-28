# SHAP analysis for rfsrc models — design (v1)

**Date:** 2026-07-11
**Branch:** `feature/gg-shap`
**Status:** design approved in brainstorming; pending spec review

## Goal

Add a SHAP (Shapley additive explanations) analysis surface to
ggRandomForests, following the package's established
extract → arrange → plot seam. SHAP contributes the one thing the existing
VIMP and varPro tooling structurally cannot: an **exact, signed, additive
decomposition of an individual prediction** (base value + Σ SHAP = the model's
prediction). This is what powers beeswarm and dependence views where every
point is a real per-observation contribution.

## Scope (v1)

**In:**
- Regression and classification forests: `rfsrc` and `randomForest`.
- Three plots from one SHAP object: global importance, beeswarm, dependence.

**Out (deferred):**
- Survival forests (no single scalar target — needs a user-facing
  target choice: ensemble mortality, or survival/CHF at time `t`).
- Single-observation waterfall / force plots (different data shape and layout).

## Engine decision

**Wrap `kernelshap::kernelshap()`** (model-agnostic, prediction-function
driven). Rationale: matches the package philosophy of visualizing existing
methods rather than implementing new inference machinery; driven purely by
the model's `predict()`, so no coupling to rfsrc tree internals.

**Superseded choice:** the original design wrapped `fastshap::explain()`.
`fastshap` was removed from CRAN on 2026-05-27 ("issues were not corrected
despite reminders") and is not installable on current R — a hard blocker for
both local development and CRAN Suggests. `kernelshap` (CRAN 0.9.1, actively
maintained) is the drop-in replacement: same model-agnostic contract
(`object`, `X`, `pred_fun`), returns a result object with `$S` (a plain
observations × variables numeric matrix) and `$baseline` (scalar mean
prediction over the background sample) — confirmed by a live call against an
`rfsrc` fit during replanning.

**`kernelshap` goes in `Suggests`, not `Imports`** — keeps the dependency
footprint frozen and the CRAN `R CMD check` time budget safe. All examples and
tests guard with `requireNamespace("kernelshap")` / `skip_if_not_installed()`.

## Architecture

Follows the existing `gg_*` idiom exactly (cf. `gg_vimp`/`plot.gg_vimp`):
an S3 generic constructor returning a subclassed tidy `data.frame` with
provenance attached, plus a `plot.` method returning a ggplot.

### Extract layer — `R/gg_shap.R`

`gg_shap(object, newdata, bg_n = 50, which.class = 1, ...)` — S3 generic:

- `gg_shap.default` — error on unsupported class (mirrors `gg_vimp.default`).
- `gg_shap.rfsrc` — pulls training predictors `X` from `object$xvar`.
- `gg_shap.randomForest` — recovers `X` via the existing
  `.rf_recover_model_frame()` helper.

Behavior:

1. Determine `X`: `newdata` if supplied, else the model's own training
   predictors (`object$xvar` for rfsrc). No requirement that the user
   re-supply the training data — consistent with `gg_vimp`/`gg_partial`.
   The same `X` (or a sample of it, size `bg_n`) is used as kernelshap's
   background reference set (`bg_X`).
2. Build a `pred_fun(object, newdata)` returning a numeric vector:
   - regression → `predict(object, newdata)$predicted`
     (rfsrc) / `predict(object, newdata)` (randomForest).
   - classification → predicted probability for `which.class`
     (column of the predicted-probability matrix).
3. Call `kernelshap::kernelshap(object, X = X, bg_X = ..., pred_fun = ...,
   verbose = FALSE)`.
4. Reshape the returned `$S` matrix (observations × variables) into **long**
   tidy form: one row per (observation, variable). `$baseline` becomes the
   object's `baseline` attribute directly (no separate mean-prediction call
   needed — kernelshap computes it from the background sample).

Return value: a `data.frame` subclassed `c("gg_shap", "data.frame")` with columns:

| column  | meaning |
|---------|---------|
| `id`    | observation index (row of `X`) |
| `vars`  | variable name (factor, ranked by mean(\|shap\|) for plotting) |
| `shap`  | SHAP contribution of that variable for that observation |
| `value` | the feature's value for that observation, numeric — used for beeswarm coloring; `NA` for factor/character features (rendered uncolored) |
| `value_label` | the raw feature value as character (the factor level for categorical features); the label source for dependence x-axes |

Note: beeswarm colors on the continuous `value`, so factor features appear
uncolored there; `shap_dependence()` on a factor `xvar` uses `value_label` and
falls back to a discrete x-axis.

Attributes: `baseline` (kernelshap's `$baseline`, the background-sample mean
prediction), `bg_n`, `which.class`, family. Provenance attached via
`.set_provenance(gg_dta, object)`.

`...` in `gg_shap()` passes through to `kernelshap::kernelshap()` (e.g.
`seed`, `exact`, `max_iter`) for users who want to tune the underlying
computation; v1 does not wrap or rename kernelshap's own arguments beyond
`bg_n` (background sample size) and `which.class`.

### Plot layer — `R/plot.gg_shap.R`

Canonical, convention-compliant method that **routes** to three exported
builders (satisfies "one `plot` per object" while keeping each view
discoverable and independently callable):

```r
plot.gg_shap(x, type = c("beeswarm", "importance", "dependence"),
             xvar = NULL, ...)
```

Delegates to:

- `shap_importance(x, ...)` — horizontal bar of `mean(|shap|)` per variable,
  ranked, `coord_flip()` (visually parallel to `plot.gg_vimp`).
- `shap_beeswarm(x, ...)` — the signature SHAP summary: points jittered by
  `vars` (y, ranked), x = `shap`, color = scaled feature `value`. Shows
  magnitude and direction at once.
- `shap_dependence(x, xvar, ...)` — for a chosen `xvar`: x = feature `value`,
  y = `shap`. If `xvar` is `NULL`, default to the top-ranked variable.

Default `type = "beeswarm"` (the canonical SHAP plot).

All three are exported ggplot builders taking a `gg_shap` object, using the
`.data[[...]]` pronoun idiom. Add `autoplot.gg_shap` mirroring the existing
`autoplot` family (thin wrapper over `plot.gg_shap`).

## Data flow

```
rfsrc/randomForest fit
   │  object$xvar (or newdata)  +  pred_fun  +  bg_X (size bg_n)
   ▼
kernelshap::kernelshap()  ──►  $S matrix (obs × vars)  +  $baseline
   ▼  reshape to long
gg_shap object  (id, vars, shap, value)  [+ baseline/bg_n attrs, provenance]
   │
   ├─ shap_importance   → bar of mean(|shap|)
   ├─ shap_beeswarm     → jittered points, colored by value
   └─ shap_dependence   → shap vs feature value (per xvar)
        ▲
   plot.gg_shap(type=) routes here; autoplot.gg_shap wraps it
```

## Error handling

- `gg_shap.default` errors with the offending class, like `gg_vimp.default`.
- If `kernelshap` is not installed, `gg_shap()` errors with an actionable
  message ("install 'kernelshap'"); examples/tests skip rather than fail.
- `shap_dependence()` validates `xvar` is a known variable; on `NULL`, picks
  the top-ranked variable and messages the choice.
- Classification `which.class` out of range → informative error listing valid
  classes (mirrors the `which.outcome` handling in `gg_vimp`).

## Testing

`tests/testthat/` — all guarded by `skip_if_not_installed("kernelshap")` and
`skip_on_cran()` (kernelshap's sampling is slow and stochastic; use a small
`bg_n` and a fixed `seed`):

- `gg_shap()` returns a `gg_shap` data.frame with the documented columns and
  `nrow == n_obs * n_vars`, on iris (classification) and airquality/Boston
  (regression), for both `rfsrc` and `randomForest`.
- `gg_shap.default` errors on a non-forest object.
- `plot.gg_shap()` returns a `ggplot` for each `type`; `shap_dependence`
  honors `xvar` and defaults sensibly when `NULL`.
- vdiffr snapshots for importance / beeswarm / dependence (guarded; restore
  pruned snapshots before committing per project practice).

## Documentation

- Roxygen on all exported objects with `\value`/`@return` (CRAN requirement),
  `@examples` wrapped so slow SHAP calls use `\donttest` and
  `if (requireNamespace("kernelshap"))`.
- A short vignette section (or standalone `shap.qmd`) is a **follow-up**, not
  part of v1 code.

## Versioning

Patch-level bump only (`3.4.1 → 3.4.x`) with matching `DESCRIPTION` +
`NEWS.md` version lines. No minor/major roll.

## Explicitly not doing (YAGNI)

- No survival support, no waterfall/force, no interaction (SHAP-interaction)
  values, no TreeSHAP, no new hard dependency.

## Revision log

- 2026-07-11: engine switched from `fastshap` to `kernelshap` after
  discovering `fastshap` was archived from CRAN on 2026-05-27 and would not
  install; `gg_shap()`'s `nsim` argument became `bg_n` accordingly (see
  Engine decision above).
