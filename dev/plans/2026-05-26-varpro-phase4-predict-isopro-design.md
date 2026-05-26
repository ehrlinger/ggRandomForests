# ggRandomForests v2.8.0 — varPro Phase 4: predict.isopro Wrapper Design

**Date:** 2026-05-26
**Author:** John Ehrlinger (design via Claude brainstorming)
**Status:** Approved — ready for implementation planning
**Sequencing:** Second of the Phase 4 sub-projects. Builds on PR #94 (gg_isopro for the in-sample case). `gg_beta_varpro` and `gg_ivarpro` come after. Lands as one PR before the v2.8.0 release candidate.

---

## Goal

Let users score new observations against a fitted `varPro::isopro` model with the same tidy-data ergonomics as the in-sample `gg_isopro()` call: same return shape, same plot method, same threshold semantics.

## Scope

A single sub-project. Implemented as one new argument on the existing `gg_isopro()` extractor — no new exported function, no new plot method. Other Phase 4 functions (`gg_beta_varpro`, `gg_ivarpro`) are tracked separately.

---

## Architecture

```
varPro::isopro fit  ──┐
                      ├──► gg_isopro(object, newdata = NULL)
data.frame (newdata) ─┘                │
                                       └── tidy data.frame
                                           class: c("gg_isopro", "data.frame")
                                           cols : obs, case.depth, howbad
                                           attr : provenance (prediction flag)
                                                  │
                                            plot / print / summary / autoplot
                                                  (unchanged from PR #94)
```

The two input paths produce the same return shape. The plot/print/summary methods do not care which path produced the object.

---

## Extractor signature

```r
gg_isopro(object, newdata = NULL, ...)
```

- **`object`** — an `isopro` fit from `varPro::isopro`. Method dispatch via `gg_isopro.isopro`.
- **`newdata`** — `NULL` (default) or a `data.frame`. When `NULL`, returns the training-data tidy frame (PR #94 behaviour). When a `data.frame`, scores each row against the fit and returns the same tidy shape for the test data.
- **`...`** — currently unused.

## Internal flow when `newdata` is supplied

1. Validate: `newdata` must be a `data.frame`. Otherwise `stop()` with `"newdata must be a data.frame."`.
2. Call `predict(object, newdata = newdata, quantiles = FALSE)` → raw mean case-depth per row.
3. Call `predict(object, newdata = newdata, quantiles = TRUE)` → quantile per row (smaller = more anomalous, per varPro's convention).
4. **Flip polarity** for column consistency:
   ```r
   howbad <- 1 - quantile
   ```
   With the flip, `howbad` always means "higher = more anomalous", whether the row came from the training set or `newdata`. The plot method and any `threshold = ...` value the user picks from the training elbow apply unchanged.
5. Assemble the tidy frame:
   ```r
   data.frame(obs = seq_len(nrow(newdata)),
              case.depth = case_depth_vec,
              howbad     = howbad_vec)
   ```
6. Set class `c("gg_isopro", "data.frame")` and attach a provenance attribute:
   - `source = "varPro::isopro"`
   - `n = nrow(newdata)`
   - `ntree` (carried from `object$isoforest$ntree`)
   - `prediction = TRUE` (new — distinguishes test-data extractor from training)

## Plot / print / summary

Unchanged. The new tidy frame has the same class and columns as the training case, so every S3 companion from PR #94 works as-is.

## Overlay train + test (caller pattern)

No new machinery in the package; the existing `method`-column auto-detect in `plot.gg_isopro` is overloaded:

```r
gg_train <- gg_isopro(fit)
gg_test  <- gg_isopro(fit, newdata = test_df)
gg_both  <- rbind(cbind(gg_train, method = "train"),
                  cbind(gg_test,  method = "test"))
class(gg_both) <- c("gg_isopro", "data.frame")
plot(gg_both)
```

`method` is the existing special column used to colour-group rnd / unsupv / auto curves; reusing it for `train` / `test` works because the plot only cares about the column's existence, not its semantics. A `@section` in the `gg_isopro` roxygen documents this overload so it isn't a hidden trick.

## Polarity: how the wrapper presents both conventions

`varPro::predict.isopro(quantiles = TRUE)` returns quantiles where *smaller is more anomalous* (a row whose case depth sits in the lower tail of the training depth distribution). `gg_isopro`'s `howbad` is the opposite: *higher is more anomalous*. The wrapper is **not** trying to hide the conflict — it shows both polarities by keeping both columns:

- `case.depth` carries the raw mean depth from `predict(quantiles = FALSE)`. **Lower = more anomalous.** This is varPro's native scale, exposed directly, with no transformation. A user who wants to cross-reference against `varPro::predict.isopro()` output can do it on this column.
- `howbad` carries `1 - predict(quantiles = TRUE)`. **Higher = more anomalous.** This is the wrapper convention, and it matches what the training-path `gg_isopro()` already produces. The plot method's elbow shape, the `threshold` annotation, and the `top_n_pct` quantile all assume this polarity.

The roxygen must name this transformation explicitly. A user who reads only the `howbad` column should still come away understanding: (i) it isn't byte-identical to `predict.isopro(quantiles = TRUE)`, (ii) the relationship is `howbad = 1 - quantile`, and (iii) `case.depth` is the unmodified varPro number if you need the raw measure.

## Validation

- `newdata` is supplied but isn't a `data.frame` → `stop("newdata must be a data.frame.")`.
- `nrow(newdata) == 0` → empty `gg_isopro` frame with the same columns; downstream plot handles zero rows by erroring with a clear ggplot message. No special-case in the extractor.
- Unknown columns / NAs in `newdata` → pass through to `predict.isopro`; varPro decides.

## Tests (mirroring the Phase 1–4a coverage)

1. **Shape**: `gg_isopro(fit, newdata = test_df)` returns `c("gg_isopro", "data.frame")` with columns `obs / case.depth / howbad`, `nrow == nrow(newdata)`.
2. **Polarity flip**: synthetic check that `howbad` is in `[0, 1]` and corresponds to `1 - predict(..., quantiles = TRUE)` for the same rows.
3. **Sanity check**: scoring the training set as newdata produces `howbad` values close to (but not necessarily identical to) `fit$howbad`. Tolerance is loose because varPro may use a slightly different code path for `predict` vs the in-bag scoring; the test asserts the same range and the same per-row ordering for the top-5 most anomalous rows.
4. **Provenance**: returned object's provenance attribute has `prediction = TRUE` and `n == nrow(newdata)`.
5. **Validation error**: `gg_isopro(fit, newdata = "not a df")` errors with `"newdata must be a data.frame"`.
6. **Overlay smoke test**: rbind of train + test extractor outputs with a `method` label column plots without error; every patchwork sub-plot builds.

## Snapshots

One new `vdiffr::expect_doppelganger` inside the existing `VDIFFR_RUN_TESTS` guard: `gg-isopro-predict-overlay` — train + test bound with a `method` column, default `plot()`. Skip cleanly without the env var.

## Documentation

- Extend the existing `gg_isopro` roxygen with:
  - A new `@param newdata` line in the terse register.
  - A short `@section Scoring new data` block in the narrative register, written to make the polarity transformation explicit:
    - What `newdata` does.
    - The two `predict.isopro` calls and how their outputs map to the two columns (raw depth → `case.depth`, `1 - quantile` → `howbad`).
    - One sentence naming the transformation in code form (e.g. "`howbad = 1 - predict(fit, newdata, quantiles = TRUE)`") so a user diffing against raw `predict()` output sees exactly where the difference comes from.
    - The train/test overlay caller pattern.
- Update the existing "What you use this for" section to mention the new-data use case (a held-out cohort, a production scoring scenario).

## Files

- **Modify**: `R/gg_isopro.R` (signature + new internal path), `tests/testthat/test_gg_isopro.R` (six new tests), `tests/testthat/test_snapshots.R` (one snapshot), `NEWS.md`, `DESCRIPTION` (version bump to the next available `2.7.3.900x` increment — `.9010` if PR #95 has landed by then, otherwise the implementer picks the next free slot above `.9008`).
- **New**: none.

## Acceptance criteria

- `R CMD check --as-cran`: 0 errors / 0 warnings / 0 notes.
- Full `devtools::test()`: 0 failures. New tests pass; gg_isopro coverage from PR #94 (43 expectations) still green.
- Roxygen produced under markdown mode (PR #95 enables this; if #95 hasn't merged, write in Rd-style and document() will produce valid Rd either way).
- One PR before the v2.8.0 release candidate.

## Out of scope

- A new function (e.g. `gg_isopro_predict()`) — rejected in favour of one optional argument.
- An S3 `predict.gg_isopro()` method — rejected because gg_isopro is a data frame and doesn't carry the fit.
- Generalising the `method`-column auto-detect to *any* grouping column. Today we reuse `method`; if real friction emerges, generalise in a later release.
- Exposing `quantiles = FALSE` to the caller. Internally we call both; externally the user gets the unified columns.
