# PR #88: Multi-class gg_roc with per_class=TRUE Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend `gg_roc()` to return per-class one-vs-rest ROC curves in a long-format data frame (with a `class` column and named AUC vector) when `per_class = TRUE` is passed, and teach `plot.gg_roc` to render them as an overlay or faceted panel.

**Architecture:** Add `per_class = FALSE` to the `gg_roc` generic and the `randomForest` method. When `per_class = TRUE` and the forest has > 2 classes, the method reuses the existing `.rf_one_class_roc` and `.rf_prob_matrix` helpers already in `calc_roc.R` to build one OvR ROC curve per class, stacks them into a long-format data frame with a `class` factor column, and attaches a named AUC vector ordered by descending AUC. `plot.gg_roc` detects the `class` column and dispatches to a new overlay/facet path. `summary.gg_roc` is updated to print a named AUC vector when the `class` column is present. Binary forests with `per_class = TRUE` are a no-op (single curve returned unchanged).

**Tech Stack:** R, randomForest, ggplot2, testthat, vdiffr (snapshots only)

---

## File Map

| File | Change |
|---|---|
| `R/gg_roc.R` | Add `per_class` argument to generic + both methods; implement per_class path in `gg_roc.randomForest` |
| `R/plot.gg_roc.R` | Add `panel` argument; insert per_class detection branch before single-class path |
| `R/summary_methods.R` | Update `summary.gg_roc` to handle named AUC vector |
| `tests/testthat/test_gg_roc.R` | Add per_class tests (T1, T3, T4) |
| `tests/testthat/test_snapshots.R` | Add 2 vdiffr snapshots (T7) |
| `DESCRIPTION` | Version `2.7.3.9005` → `2.7.3.9006` |
| `NEWS.md` | Add feature entry |

---

## Task T0: Branch setup and version bump

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Create the feature branch from main**

```bash
git checkout -b feat/rf-88-multiclass-roc origin/main
```

Expected: `Switched to a new branch 'feat/rf-88-multiclass-roc'`

- [ ] **Step 2: Bump version in DESCRIPTION**

Open `DESCRIPTION`. Change:
```
Version: 2.7.3.9005
```
to:
```
Version: 2.7.3.9006
```

- [ ] **Step 3: Confirm package loads**

```r
devtools::load_all()
```

Expected: no errors.

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION
git commit -m "chore: open v2.7.3.9006 dev increment (PR #88)"
```

---

## Task T1: Write failing tests — per_class extractor

**Files:**
- Modify: `tests/testthat/test_gg_roc.R`

- [ ] **Step 1: Append the per_class extractor tests to `tests/testthat/test_gg_roc.R`**

```r
## ── per_class = TRUE (PR #88) ──────────────────────────────────────────────

test_that("gg_roc per_class=TRUE: long format with class column", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  expect_true("class" %in% names(gg))
  expect_true(all(c("sens", "spec", "pct") %in% names(gg)))  # pct = threshold; same 3-col contract as calc_roc
  expect_s3_class(gg$class, "factor")
  expect_equal(nlevels(gg$class), 3L)
})

test_that("gg_roc per_class=TRUE: auc attr is named numeric vector length 3", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf  <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg  <- gg_roc(rf, per_class = TRUE)
  auc <- attr(gg, "auc")
  expect_length(auc, 3L)
  expect_named(auc)
  # setosa is linearly separable in iris — AUC should be near-perfect
  expect_gt(auc[["setosa"]], 0.99)
  # AUC values must be sorted descending
  expect_true(all(diff(auc) <= 0))
})

test_that("gg_roc per_class=TRUE: class factor levels ordered by descending AUC", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf  <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg  <- gg_roc(rf, per_class = TRUE)
  auc <- attr(gg, "auc")
  expect_equal(levels(gg$class), names(auc))
})
```

- [ ] **Step 2: Run the new tests — expect all three to FAIL**

```bash
Rscript -e "devtools::test(filter='gg_roc')"
```

Expected: 3 FAILures — `gg_roc` does not accept `per_class` yet so it hits `...` and returns without a `class` column.

---

## Task T2: Implement gg_roc per_class path

**Files:**
- Modify: `R/gg_roc.R`

- [ ] **Step 1: Add `per_class = FALSE` to the generic**

Find:
```r
gg_roc <- function(object, which_outcome, oob = TRUE, ...) {
  UseMethod("gg_roc", object)
}
```

Replace with:
```r
gg_roc <- function(object, which_outcome, oob = TRUE, per_class = FALSE, ...) {
  UseMethod("gg_roc", object)
}
```

- [ ] **Step 2: Add `per_class = FALSE` to `gg_roc.rfsrc`**

Find:
```r
gg_roc.rfsrc <- function(object, which_outcome, oob = TRUE, ...) {
```

Replace with:
```r
gg_roc.rfsrc <- function(object, which_outcome, oob = TRUE, per_class = FALSE, ...) {
```

Note: the rfsrc per_class path is out of scope for this PR (tracked under issue #72).
`gg_roc.rfsrc` already accepts `...`, so `per_class = TRUE` would currently be
silently swallowed rather than erroring. The argument is added explicitly for API
discoverability and to keep the signature consistent with `gg_roc.randomForest`.
Leave the body unchanged — no per_class logic is wired up in the rfsrc method.

- [ ] **Step 3: Replace the entire `gg_roc.randomForest` body**

Find and replace the entire `gg_roc.randomForest` function:

```r
# BEFORE — existing body:
#' @export
gg_roc.randomForest <- function(object, which_outcome, oob = TRUE, ...) {
  # Validate that the object is a genuine randomForest instance.
  if (!inherits(object, "randomForest")) {
    stop(
      "gg_roc.randomForest only works for objects of class 'randomForest'."
    )
  }

  # Default to computing the ROC curve for all outcome classes.
  if (missing(which_outcome)) {
    which_outcome <- "all"
  }

  if (!(object$type == "classification")) {
    stop("gg_roc only works with classification forests")
  }

  # For randomForest objects the response is stored in $y (not $yvar).
  gg_dta <- # nolint: object_usage_linter
    calc_roc(object,
      object$y,
      which_outcome = which_outcome,
      oob = oob
    )
  class(gg_dta) <- c("gg_roc", class(gg_dta))
  gg_dta <- .set_provenance(gg_dta, object)

  invisible(gg_dta)
}
```

Replace with:

```r
#' @export
gg_roc.randomForest <- function(object, which_outcome, oob = TRUE,
                                per_class = FALSE, ...) {
  if (!inherits(object, "randomForest")) {
    stop("gg_roc.randomForest only works for objects of class 'randomForest'.")
  }
  if (missing(which_outcome)) {
    which_outcome <- "all"
  }
  if (!(object$type == "classification")) {
    stop("gg_roc only works with classification forests")
  }

  lvls    <- levels(object$y)
  n_class <- length(lvls)

  # ── per_class = TRUE path (multi-class only) ─────────────────────────────
  if (isTRUE(per_class) && n_class > 2L) {
    if (!missing(which_outcome) && !identical(which_outcome, "all")) {
      message("which_outcome is ignored when per_class = TRUE.")
    }
    prob   <- .rf_prob_matrix(object, oob, lvls)
    dta    <- object$y
    curves <- lapply(seq_along(lvls), function(k) {
      cv       <- .rf_one_class_roc(dta, prob, k, lvls)
      cv$class <- lvls[k]
      cv
    })
    auc_vals        <- vapply(curves, calc_auc, numeric(1L))
    names(auc_vals) <- lvls
    auc_ord         <- order(auc_vals, decreasing = TRUE)
    auc_vals        <- auc_vals[auc_ord]
    gg_dta          <- do.call(rbind, curves)
    gg_dta$class    <- factor(gg_dta$class, levels = lvls[auc_ord])
    class(gg_dta)   <- c("gg_roc", class(gg_dta))
    attr(gg_dta, "auc") <- auc_vals
    gg_dta <- .set_provenance(gg_dta, object)
    return(invisible(gg_dta))
  }

  # ── Standard path (binary, or per_class not requested) ──────────────────
  # For randomForest objects the response is stored in $y (not $yvar).
  gg_dta <- # nolint: object_usage_linter
    calc_roc(object, object$y, which_outcome = which_outcome, oob = oob)
  class(gg_dta)       <- c("gg_roc", class(gg_dta))
  attr(gg_dta, "auc") <- calc_auc(gg_dta)
  gg_dta              <- .set_provenance(gg_dta, object)
  invisible(gg_dta)
}
```

- [ ] **Step 4: Run the T1 tests — expect all three to PASS**

```bash
Rscript -e "devtools::test(filter='gg_roc')"
```

Expected: T1's 3 new tests green; all existing tests still passing.

- [ ] **Step 5: Commit**

```bash
git add R/gg_roc.R tests/testthat/test_gg_roc.R
git commit -m "feat: gg_roc.randomForest per_class=TRUE — per-class OvR ROC + named AUC"
```

---

## Task T3: Write failing tests — binary no-op and which_outcome conflict

**Files:**
- Modify: `tests/testthat/test_gg_roc.R`

- [ ] **Step 1: Append edge-case tests**

```r
test_that("gg_roc per_class=TRUE on binary forest: no class column (no-op)", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  bin_data         <- iris[iris$Species != "virginica", ]
  bin_data$Species <- droplevels(bin_data$Species)
  rf  <- randomForest::randomForest(Species ~ ., data = bin_data, ntree = 100L)
  gg  <- gg_roc(rf, per_class = TRUE)
  # Binary forest: per_class is a no-op — no class column, scalar AUC
  expect_false("class" %in% names(gg))
  expect_length(attr(gg, "auc"), 1L)
})

test_that("gg_roc per_class=TRUE + which_outcome integer: message then per_class wins", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  expect_message(
    gg <- gg_roc(rf, per_class = TRUE, which_outcome = 1L),
    "which_outcome.*ignored.*per_class"
  )
  expect_true("class" %in% names(gg))
})

test_that("gg_roc which_outcome='all' still returns macro-average (no class column)", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, which_outcome = "all")
  expect_false("class" %in% names(gg))
  # Macro-average returns a single data frame, not a class-faceted one
  expect_true(all(c("sens", "spec", "pct") %in% names(gg)))  # pct = threshold; same 3-col contract as calc_roc
})
```

- [ ] **Step 2: Run the new tests — expect all to PASS immediately (T2 already implements them)**

```bash
Rscript -e "devtools::test(filter='gg_roc')"
```

Expected: `[ FAIL 0 | ... ]` — all pass. (These tests verify correctness of the T2 implementation; they are written before T2 in the plan so the red-green cycle is explicit, but because they test the same function modified in T2 they green immediately.)

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_roc.R
git commit -m "test: gg_roc per_class binary no-op and which_outcome conflict tests"
```

---

## Task T4: Write failing tests — plot.gg_roc per_class paths

**Files:**
- Modify: `tests/testthat/test_gg_roc.R`

- [ ] **Step 1: Append plot tests**

```r
## ── plot.gg_roc per_class paths (PR #88) ─────────────────────────────────

test_that("plot.gg_roc per_class=TRUE: overlay returns ggplot", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  p  <- plot(gg, panel = "overlay")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_roc per_class=TRUE: facet returns ggplot", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  p  <- plot(gg, panel = "facet")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_roc per_class=TRUE: layer_data smokeable for overlay", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  p  <- plot(gg, panel = "overlay")
  expect_no_error(ggplot2::layer_data(p, 1L))
})

test_that("plot.gg_roc existing single-class path unchanged", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, which_outcome = 1L)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::layer_data(p, 1L))
})
```

- [ ] **Step 2: Run new tests — the per_class plot tests FAIL**

```bash
Rscript -e "devtools::test(filter='gg_roc')"
```

Expected: the 3 `plot.gg_roc per_class` tests FAIL because `plot.gg_roc` does not yet accept `panel` and has no per_class detection. The "existing single-class path unchanged" test should PASS.

---

## Task T5: Implement plot.gg_roc per_class detection

**Files:**
- Modify: `R/plot.gg_roc.R`

- [ ] **Step 1: Add `panel` argument to `plot.gg_roc`**

Find:
```r
plot.gg_roc <- function(x, which_outcome = NULL, ...) {
```

Replace with:
```r
plot.gg_roc <- function(x, which_outcome = NULL,
                        panel = c("overlay", "facet"), ...) {
  panel <- match.arg(panel)
```

- [ ] **Step 2: Insert per_class detection branch**

Find the comment and `if` block that begins the single-class ROC plot section:

```r
  ## ---- Single-class ROC plot ------------------------------------------
  if (inherits(gg_dta, "gg_roc")) {
    # Sort by specificity so the ROC curve is drawn left-to-right
    gg_dta <- gg_dta[order(gg_dta$spec), ]
```

Replace with:

```r
  ## ---- Single-class ROC plot (or per_class long-format) ----------------
  if (inherits(gg_dta, "gg_roc")) {

    # Per-class detection: gg_roc produced by gg_roc(..., per_class = TRUE)
    # carries a 'class' column (factor) + a named AUC vector attribute.
    if ("class" %in% names(gg_dta)) {
      gg_dta$fpr <- 1 - gg_dta$spec
      auc        <- attr(x, "auc")

      if (panel == "overlay") {
        gg_plt <- ggplot2::ggplot(gg_dta) +
          ggplot2::geom_line(ggplot2::aes(
            x = .data$fpr, y = .data$sens, color = .data$class
          )) +
          ggplot2::labs(
            x = "1 - Specificity (FPR)", y = "Sensitivity (TPR)",
            color = "Class"
          ) +
          ggplot2::geom_abline(
            slope = 1, intercept = 0,
            col = "red", linetype = 2, linewidth = .5
          ) +
          ggplot2::coord_fixed()
      } else {
        gg_plt <- ggplot2::ggplot(gg_dta) +
          ggplot2::geom_line(ggplot2::aes(
            x = .data$fpr, y = .data$sens
          )) +
          ggplot2::labs(
            x = "1 - Specificity (FPR)", y = "Sensitivity (TPR)"
          ) +
          ggplot2::geom_abline(
            slope = 1, intercept = 0,
            col = "red", linetype = 2, linewidth = .5
          ) +
          ggplot2::facet_wrap(~class) +
          ggplot2::coord_fixed()
      }

      # AUC caption — top 5 classes by descending AUC (already sorted)
      if (!is.null(auc) && length(auc) > 0L) {
        top_n   <- min(5L, length(auc))
        auc_str <- paste(
          sprintf("%s=%.3g", names(auc)[seq_len(top_n)], auc[seq_len(top_n)]),
          collapse = ", "
        )
        if (length(auc) > 5L) auc_str <- paste0(auc_str, ", ...")
        gg_plt <- gg_plt +
          ggplot2::labs(caption = paste("OvR ROC — per_class=TRUE. AUC:", auc_str))
      }
      return(gg_plt)
    }

    # Sort by specificity so the ROC curve is drawn left-to-right
    gg_dta <- gg_dta[order(gg_dta$spec), ]
```

- [ ] **Step 3: Run the T4 tests — expect all four to PASS**

```bash
Rscript -e "devtools::test(filter='gg_roc')"
```

Expected: `[ FAIL 0 | ... ]`

- [ ] **Step 4: Run full test suite — confirm no regressions**

```bash
Rscript -e "devtools::test()"
```

Expected: `[ FAIL 0 | WARN <n> | SKIP <n> | PASS <n> ]`

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_roc.R tests/testthat/test_gg_roc.R
git commit -m "feat: plot.gg_roc per_class=TRUE — overlay and facet panel paths"
```

---

## Task T6: Update summary.gg_roc for named AUC vector

**Files:**
- Modify: `R/summary_methods.R`

- [ ] **Step 1: Write a failing test for summary with per_class**

Append to `tests/testthat/test_gg_roc.R`:

```r
test_that("summary.gg_roc per_class=TRUE: prints named AUC, no error", {
  skip_if_not_installed("randomForest")
  set.seed(1L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  gg <- gg_roc(rf, per_class = TRUE)
  s  <- summary(gg)
  expect_s3_class(s, "summary.gg")
  # Body should mention all three class names
  expect_true(any(grepl("setosa", s$body)))
  expect_true(any(grepl("versicolor", s$body)))
  expect_true(any(grepl("virginica", s$body)))
})
```

- [ ] **Step 2: Run — expect FAIL**

```bash
Rscript -e "devtools::test(filter='gg_roc')"
```

Expected: the new summary test FAIL because `summary.gg_roc` currently calls `sprintf("AUC: %.4g", auc)` on a named numeric vector, producing a misformatted string rather than named class entries.

- [ ] **Step 3: Replace `summary.gg_roc` in `R/summary_methods.R`**

Find:
```r
#' @rdname summary.gg
#' @export
summary.gg_roc <- function(object, ...) {
  body <- c(
    sprintf("thresholds: %d", nrow(object)),
    sprintf("AUC: %.4g",
            attr(object, "auc") %||% .gg_auc_trap(object))
  )
  .summary_skel(object, "gg_roc", body)
}
```

Replace with:
```r
#' @rdname summary.gg
#' @export
summary.gg_roc <- function(object, ...) {
  auc <- attr(object, "auc") %||% .gg_auc_trap(object)
  if ("class" %in% names(object)) {
    # per_class = TRUE path: named AUC vector, one entry per class
    n_cls   <- nlevels(object$class)
    auc_str <- paste(sprintf("%s=%.4g", names(auc), auc), collapse = ", ")
    body <- c(sprintf("classes: %d", n_cls),
              sprintf("AUC: %s", auc_str))
  } else {
    body <- c(
      sprintf("thresholds: %d", nrow(object)),
      sprintf("AUC: %.4g", auc)
    )
  }
  .summary_skel(object, "gg_roc", body)
}
```

- [ ] **Step 4: Run all gg_roc tests — expect all to PASS**

```bash
Rscript -e "devtools::test(filter='gg_roc')"
```

Expected: `[ FAIL 0 | ... ]`

- [ ] **Step 5: Commit**

```bash
git add R/summary_methods.R tests/testthat/test_gg_roc.R
git commit -m "feat: summary.gg_roc handles named AUC vector for per_class=TRUE"
```

---

## Task T7: vdiffr snapshots

**Files:**
- Modify: `tests/testthat/test_snapshots.R`

- [ ] **Step 1: Append a per_class ROC section inside the `VDIFFR_RUN_TESTS` guard**

Find the closing `}` of the `if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true"))` block and add the following **inside** it, before the closing `}`:

```r
## ── per_class ROC snapshots (PR #88) ─────────────────────────────────────
if (requireNamespace("randomForest", quietly = TRUE)) {
  local({
    set.seed(1L)
    rf_iris    <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
    gg_pc_iris <- gg_roc(rf_iris, per_class = TRUE)

    test_that("snapshot: gg-roc-multiclass-overlay", {
      vdiffr::expect_doppelganger(
        "gg-roc-multiclass-overlay",
        plot(gg_pc_iris, panel = "overlay")
      )
    })

    test_that("snapshot: gg-roc-multiclass-facet", {
      vdiffr::expect_doppelganger(
        "gg-roc-multiclass-facet",
        plot(gg_pc_iris, panel = "facet")
      )
    })
  })
}
```

- [ ] **Step 2: Confirm snapshot tests skip cleanly without the env var**

```bash
Rscript -e "devtools::test(filter='snapshots')"
```

Expected: all snapshot tests SKIP (no `VDIFFR_RUN_TESTS=true`).

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_snapshots.R
git commit -m "test: add vdiffr snapshots for per_class ROC overlay and facet (PR #88)"
```

---

## Task T8: NEWS, final gate, and PR

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 1: Add feature entry to the `ggRandomForests v2.8.0 (development)` section in `NEWS.md`**

```
* **`gg_roc`: per-class one-vs-rest ROC curves (#88, closes #72).**
  - `gg_roc()` gains a `per_class = FALSE` argument.  When `per_class = TRUE`
    and the forest has more than two classes, returns a long-format `gg_roc`
    data frame with a `class` factor column and a named AUC vector attribute
    (one entry per class, ordered by descending AUC).
  - `plot.gg_roc()` gains a `panel = c("overlay", "facet")` argument.  When the
    `gg_roc` object contains a `class` column, the overlay path colours curves
    by class; the facet path wraps each class into its own panel.
  - `summary.gg_roc()` now prints named per-class AUC values when the `class`
    column is present.
  - Binary forests: `per_class = TRUE` is a silent no-op (single-curve result
    returned unchanged).
  - ROC confidence intervals are deferred to v2.9.0 (issue #7 / #72-CIs).
```

- [ ] **Step 2: Run R CMD check**

```bash
Rscript -e "devtools::check(args='--as-cran')"
```

Expected: `0 errors | 0 warnings | 0 notes`

- [ ] **Step 3: Commit NEWS**

```bash
git add NEWS.md
git commit -m "docs: update NEWS for PR #88 — per_class ROC"
```

- [ ] **Step 4: Push the branch**

```bash
git push -u origin feat/rf-88-multiclass-roc
```

- [ ] **Step 5: Open the PR**

```bash
gh pr create \
  --title "feat: gg_roc per_class=TRUE — per-class OvR ROC curves (#88, closes #72)" \
  --body "$(cat <<'EOF'
## Summary

- `gg_roc()` gains `per_class = FALSE`. When `per_class = TRUE` on a multi-class forest, returns a long-format `gg_roc` data frame with a `class` factor column and a named AUC vector attribute (ordered by descending AUC).
- `plot.gg_roc()` gains `panel = c("overlay", "facet")`. Detects the `class` column and dispatches to the new multi-class overlay or faceted path.
- `summary.gg_roc()` updated to print named per-class AUC values when the `class` column is present.
- Binary forests: `per_class = TRUE` is a silent no-op.
- ROC CIs deferred to v2.9.0 (issue #7 / #72-CIs).

## Test plan
- [x] `devtools::test()` — all tests pass, 0 failures
- [x] `devtools::check(args="--as-cran")` — 0 errors, 0 warnings, 0 notes

Closes #72

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

- [ ] **Step 6: Verify CI is green before requesting merge**

```bash
gh pr checks <PR-NUMBER>
```

Expected: all checks pass.
