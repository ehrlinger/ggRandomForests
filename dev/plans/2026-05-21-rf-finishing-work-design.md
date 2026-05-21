---
date: 2026-05-21
status: approved
scope: v2.8.0 — randomForest engine finishing work
author: John Ehrlinger (design partner: Claude)
issues: "#81, #82 (close stale), #82 follow-up (gg_variable classification), #72 (multi-class ROC, no CIs)"
---

# randomForest Engine — Finishing Work Design

Two independent PRs targeting v2.8.0 completion. All work is on the
`randomForest` engine path; no rfsrc or varPro behavior changes.

---

## Scope

| PR | Content | Version bump |
|---|---|---|
| #87 | `gg_variable.randomForest` classification fix + close stale #81/#82 | `2.7.3.9005` |
| #88 | Multi-class `gg_roc` with `per_class = TRUE` (issue #72, no CIs) | `2.7.3.9006` |

CIs on ROC curves (#7, tracked under #72) are deferred to v2.9.0.

---

## PR #87 — `gg_variable.randomForest` Classification Fix

### Problem

`gg_variable.randomForest` for classification stores a single `yhat` column
containing `as.vector(object$predicted)` — a factor of predicted class labels,
not per-class probabilities. This mismatches what `gg_variable.rfsrc` produces
(`yhat.<classname>` probability columns from `object$predicted.oob`), so
`plot.gg_variable`'s multi-class pivot never fires. Two downstream bugs follow:

1. The multi-class numeric path in `plot.gg_variable` has no `smooth = TRUE`
   block, so smooth curves are silently skipped for 3+ class forests.
2. The binary path's `smooth = TRUE` block calls `geom_smooth(...)` with no
   `aes()`, so `stat_smooth()` fails ("requires missing aesthetics: x and y")
   when `layer_data()` is called on the result.

### Fix: Extractor (`R/gg_variable.R`)

In `gg_variable.randomForest`, replace the single-yhat classification branch:

```r
# Before (wrong — class labels, not probabilities):
gg_dta$yhat <- as.vector(object$predicted)
if (object$type == "classification") {
  gg_dta$yvar <- response
}

# After (correct — per-class OOB vote fractions, matching rfsrc shape):
if (object$type == "classification") {
  preds <- object$votes   # matrix: n × n_classes, OOB vote fractions
  colnames(preds) <- paste0("yhat.", colnames(preds))
  gg_dta <- cbind(gg_dta, preds)
  gg_dta$yvar <- response
} else {
  gg_dta$yhat <- as.vector(object$predicted)
}
```

`object$votes` is the n × n_classes matrix of OOB per-class vote fractions
(equivalent to `object$predicted.oob` for rfsrc classification). With this
change, `gg_variable.randomForest` produces `yhat.setosa`, `yhat.versicolor`,
`yhat.virginica` for iris — the same shape as the rfsrc path — so
`plot.gg_variable` dispatches identically for both engines.

### Fix: Plot method (`R/plot.gg_variable.R`)

**Fix 1 — Binary classification smooth missing aes (line ~519):**

```r
# Before:
if (smooth) {
  gg_plt[[ind]] <- gg_plt[[ind]] +
    ggplot2::geom_smooth(...)
}

# After:
if (smooth) {
  gg_plt[[ind]] <- gg_plt[[ind]] +
    ggplot2::geom_smooth(
      ggplot2::aes(x = .data$var, y = .data$yhat), ...
    )
}
```

**Fix 2 — Multi-class numeric path missing smooth block (after the `geom_point`
at line ~557):**

```r
# Add after the multi-class geom_point block:
if (smooth) {
  gg_plt[[ind]] <- gg_plt[[ind]] +
    ggplot2::geom_smooth(
      ggplot2::aes(x = .data$var, y = .data$yhat), ...
    )
}
```

### Tests

**`tests/testthat/test_gg_variable.R`** — add a `randomForest` classification
block:

```r
## ── randomForest classification ──────────────────────────────────────────────
test_that("gg_variable.randomForest classification: yhat.* columns present", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  expect_true(all(c("yhat.setosa", "yhat.versicolor", "yhat.virginica")
                  %in% names(gg)))
  expect_false("yhat" %in% names(gg))   # no bare yhat for multi-class
  expect_true("yvar" %in% names(gg))
})

test_that("gg_variable.randomForest classification: plot returns patchwork", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  p  <- plot(gg)
  expect_s3_class(p, "patchwork")
})

test_that("gg_variable.randomForest classification: layer_data smokeable", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  p  <- plot(gg, xvar = "Sepal.Length")
  expect_no_error(ggplot2::layer_data(p, 1L))
})
```

**`tests/testthat/test_snapshots.R`** — add inside the `VDIFFR_RUN_TESTS=true`
guard:

```r
if (requireNamespace("randomForest", quietly = TRUE)) {
  local({
    set.seed(42L)
    rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
    gg <- gg_variable(rf)
    test_that("snapshot: gg-variable-rf-classification-default", {
      vdiffr::expect_doppelganger("gg-variable-rf-classification-default",
                                  plot(gg))
    })
    test_that("snapshot: gg-variable-rf-classification-smooth", {
      vdiffr::expect_doppelganger("gg-variable-rf-classification-smooth",
                                  plot(gg, xvar = "Sepal.Length", smooth = TRUE))
    })
  })
}
```

### Housekeeping

Close issues #81 and #82 via the PR description (`Closes #81, Closes #82`).
Both were fully resolved by PR #83 but GitHub's auto-close never fired.

---

## PR #88 — Multi-class `gg_roc` with `per_class = TRUE`

### Goal

Extend `gg_roc()` to return per-class one-vs-rest ROC curves in a long-format
data frame when `per_class = TRUE`. `which_outcome = "all"` retains its current
macro-average meaning (backward compatible). No CI computation in this PR.

### Extractor changes (`R/gg_roc.R`, `R/calc_roc.R`)

**New signature:**

```r
gg_roc(object, which_outcome = "all", per_class = FALSE, ...)
```

**`per_class = TRUE` behaviour:**

- For forests with > 2 classes: compute one-vs-rest ROC for every class *k*
  (scores = `votes[, k]`, positive = `(y == k)`), stack into a long data frame
  with columns `class` (factor), `fpr`, `tpr`.
- `attr(gg, "auc")` becomes a named numeric vector: one entry per class,
  ordered by descending AUC. Class factor levels follow the same order.
- For binary forests: `per_class = TRUE` is a no-op — returns the single-curve
  result with no `class` column and a scalar `auc` attribute (same as
  `per_class = FALSE`).
- If `per_class = TRUE` AND `which_outcome != "all"` (i.e., a specific class
  integer): `per_class` wins, a `message()` informs the caller that
  `which_outcome` is ignored when `per_class = TRUE`.

**Internal helper `calc_roc_one_vs_rest(scores, y, k)`** — single class OvR
ROC, returns a data frame `(fpr, tpr)` plus scalar AUC. Extracted so it can be
reused for both per-class computation and macro-average.

### Plot method (`R/plot.gg_roc.R`)

Detection: `has_class <- "class" %in% names(x)`.

```r
plot.gg_roc(x, panel = c("overlay", "facet"), ...)
```

**When `has_class = FALSE`** (no `class` column): behaves exactly as today —
single-panel ROC curve with diagonal reference. No change.

**When `has_class = TRUE`:**

- `panel = "overlay"` (default): single panel, `aes(color = class)`, one curve
  per class, legend titled "Class". Diagonal reference line in grey.
- `panel = "facet"`: `facet_wrap(~ class)`, individual y-axis per class, no
  color legend. Diagonal reference line in each panel.

Both modes: `geom_step` for the ROC curve (same as current single-curve plot),
`geom_abline(slope = 1, intercept = 0, linetype = 2, color = "grey50")` for
reference.

Caption: `"OvR ROC — per_class = TRUE. AUC: <name>=<value>, ..."` (truncated
if > 5 classes, showing top 5 by AUC).

### Summary method (`R/summary_methods.R`)

`summary.gg_roc` already reads `attr(object, "auc")`. Extend to handle named
vector:

```r
auc_str <- if (length(auc) == 1L) {
  sprintf("AUC: %.4g", auc)
} else {
  paste("AUC:", paste(sprintf("%s=%.4g", names(auc), auc), collapse = ", "))
}
```

### Tests (`tests/testthat/test_gg_roc.R`)

```r
## ── per_class = TRUE ─────────────────────────────────────────────────────────
test_that("gg_roc per_class=TRUE: long format with class column", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  expect_true("class" %in% names(gg))
  expect_true(all(c("fpr", "tpr") %in% names(gg)))
  expect_equal(nlevels(gg$class), 3L)
})

test_that("gg_roc per_class=TRUE: auc attr is named vector length 3", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  auc <- attr(gg, "auc")
  expect_length(auc, 3L)
  expect_named(auc)
  # setosa should be near-perfect on iris
  expect_gt(auc[["setosa"]], 0.99)
})

test_that("gg_roc per_class=TRUE on binary: no class column (no-op)", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  bin_data <- iris[iris$Species != "virginica", ]
  bin_data$Species <- droplevels(bin_data$Species)
  rf <- randomForest::randomForest(Species ~ ., data = bin_data, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  expect_false("class" %in% names(gg))
  expect_length(attr(gg, "auc"), 1L)
})

test_that("gg_roc which_outcome='all' still returns macro-average", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, which_outcome = "all")
  expect_false("class" %in% names(gg))
})

test_that("gg_roc per_class=TRUE + which_outcome integer: message + per_class wins", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  expect_message(
    gg <- gg_roc(rf, per_class = TRUE, which_outcome = 1L),
    "which_outcome.*ignored.*per_class"
  )
  expect_true("class" %in% names(gg))
})

## ── plot.gg_roc multi-class ──────────────────────────────────────────────────
test_that("plot.gg_roc per_class: overlay returns ggplot", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  p  <- plot(gg, panel = "overlay")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_roc per_class: facet returns ggplot", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  p  <- plot(gg, panel = "facet")
  expect_s3_class(p, "ggplot")
})
```

**`tests/testthat/test_snapshots.R`** — add inside the `VDIFFR_RUN_TESTS=true`
guard:

```r
if (requireNamespace("randomForest", quietly = TRUE)) {
  local({
    set.seed(1L)
    rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
    gg <- gg_roc(rf, per_class = TRUE)
    test_that("snapshot: gg-roc-multiclass-overlay", {
      vdiffr::expect_doppelganger("gg-roc-multiclass-overlay",
                                  plot(gg, panel = "overlay"))
    })
    test_that("snapshot: gg-roc-multiclass-facet", {
      vdiffr::expect_doppelganger("gg-roc-multiclass-facet",
                                  plot(gg, panel = "facet"))
    })
  })
}
```

### `_pkgdown.yml`

No new exported functions. No change needed.

---

## Non-Goals (this spec)

- ROC confidence intervals (deferred to v2.9.0 as issue #7 / #72-CIs)
- Hazard estimates (#71 / #4 / #5) — post-v2.8.0
- Issue #15 (consistent yhat scale) — pre-2015 enhancement, separate planning
- Any rfsrc-path behavior changes
- Any varPro changes

---

## Files Touched

| PR | File | Action |
|---|---|---|
| #87 | `R/gg_variable.R` | Modify: fix classification yhat columns |
| #87 | `R/plot.gg_variable.R` | Modify: binary smooth aes + multi-class smooth block |
| #87 | `tests/testthat/test_gg_variable.R` | Modify: add RF classification tests |
| #87 | `tests/testthat/test_snapshots.R` | Modify: add 2 RF classification snapshots |
| #87 | `DESCRIPTION` | Modify: version `2.7.3.9004` → `2.7.3.9005` |
| #87 | `NEWS.md` | Modify: add fix entry |
| #88 | `R/gg_roc.R` | Modify: add `per_class` argument |
| #88 | `R/calc_roc.R` | Modify: extract `calc_roc_one_vs_rest` helper; per-class dispatch |
| #88 | `R/plot.gg_roc.R` | Modify: `panel` argument; multi-class overlay + facet paths |
| #88 | `R/summary_methods.R` | Modify: `summary.gg_roc` named-vector AUC display |
| #88 | `tests/testthat/test_gg_roc.R` | Modify: add per_class tests |
| #88 | `tests/testthat/test_snapshots.R` | Modify: add 2 multi-class ROC snapshots |
| #88 | `DESCRIPTION` | Modify: version `2.7.3.9005` → `2.7.3.9006` |
| #88 | `NEWS.md` | Modify: add feature entry |
