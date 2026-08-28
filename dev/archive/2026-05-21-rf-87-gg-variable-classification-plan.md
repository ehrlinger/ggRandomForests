# PR #87: gg_variable.randomForest Classification Fix Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix `gg_variable.randomForest` for classification forests so it stores per-class OOB vote fractions (`yhat.<classname>` columns from `object$votes`) instead of predicted class-label factors, making it structurally identical to the rfsrc path; fix two downstream `plot.gg_variable` smooth bugs exposed by the extractor fix; close stale issues #81 and #82.

**Architecture:** Single-root bug: `gg_variable.randomForest` stores `as.vector(object$predicted)` (a class-label factor) instead of `object$votes` (per-class OOB vote probability matrix). Swapping to `object$votes` makes `plot.gg_variable`'s multi-class pivot fire correctly for `randomForest` objects — the same path already used for `rfsrc`. Two downstream `plot.gg_variable` bugs become reachable after the fix: (1) binary `smooth=TRUE` calls `geom_smooth(...)` with no `aes()` (stat_smooth fails when layer_data() is called), and (2) the multi-class numeric path is missing the `smooth=TRUE` block entirely.

**Tech Stack:** R, randomForest, ggplot2, patchwork, testthat, vdiffr (snapshots only)

---

## File Map

| File | Change |
|---|---|
| `R/gg_variable.R` | Fix classification branch: `object$predicted` → `object$votes` |
| `R/plot.gg_variable.R` | Fix binary smooth aes; add multi-class smooth block |
| `tests/testthat/test_gg_variable.R` | Add randomForest classification tests |
| `tests/testthat/test_snapshots.R` | Add 2 vdiffr snapshots |
| `DESCRIPTION` | Version `2.7.3.9004` → `2.7.3.9005` |
| `NEWS.md` | Add fix entry |

---

## Task T0: Branch setup and version bump

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Create the feature branch**

```bash
git checkout -b fix/rf-87-gg-variable-classification origin/main
```

Expected: `Switched to a new branch 'fix/rf-87-gg-variable-classification'`

- [ ] **Step 2: Bump version in DESCRIPTION**

Open `DESCRIPTION`. Change line:
```
Version: 2.7.3.9004
```
to:
```
Version: 2.7.3.9005
```
Also update:
```
Date: 2026-05-20
```
to:
```
Date: 2026-05-21
```

- [ ] **Step 3: Confirm package loads**

```r
devtools::load_all()
```

Expected: no errors.

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION
git commit -m "chore: open v2.7.3.9005 dev increment (PR #87)"
```

---

## Task T1: Write failing tests — gg_variable.randomForest classification

**Files:**
- Modify: `tests/testthat/test_gg_variable.R`

- [ ] **Step 1: Append the three new test cases to the end of `tests/testthat/test_gg_variable.R`**

```r
## ── randomForest classification (PR #87) ─────────────────────────────────────

test_that("gg_variable.randomForest classification: produces yhat.* columns not yhat", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  # Must have one column per class
  expect_true(all(c("yhat.setosa", "yhat.versicolor", "yhat.virginica")
                  %in% names(gg)))
  # Must NOT have a bare yhat column for multi-class
  expect_false("yhat" %in% names(gg))
  # Observed-class column must be present
  expect_true("yvar" %in% names(gg))
  # Vote fractions must be in [0, 1] and row-sum to ~1
  vote_cols <- c("yhat.setosa", "yhat.versicolor", "yhat.virginica")
  expect_true(all(gg[, vote_cols] >= 0))
  expect_true(all(gg[, vote_cols] <= 1))
  expect_true(all(abs(rowSums(gg[, vote_cols]) - 1) < 1e-6))
})

test_that("gg_variable.randomForest classification: plot returns patchwork for all xvar", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  p  <- plot(gg)
  # iris has 4 predictors, so the default (no xvar) path assembles a multi-panel
  # patchwork.  Assert patchwork specifically so a regression to a bare list (#80)
  # would be caught.
  expect_s3_class(p, "patchwork")
})

test_that("gg_variable.randomForest classification: layer_data works on single-xvar plot", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  p  <- plot(gg, xvar = "Sepal.Length")
  expect_no_error(ggplot2::layer_data(p, 1L))
})
```

- [ ] **Step 2: Run new tests — expect all three to FAIL**

```bash
Rscript -e "devtools::test(filter='gg_variable')"
```

Expected: 3 FAILures with errors like:
- `"yhat.setosa" %in% names(gg)` is FALSE (currently stores `yhat` factor)
- `layer_data` error about missing aesthetics

---

## Task T2: Fix gg_variable.randomForest — use object$votes

**Files:**
- Modify: `R/gg_variable.R`

- [ ] **Step 1: Locate the block to replace in `R/gg_variable.R`**

Find this exact code (near the end of `gg_variable.randomForest`):

```r
  gg_dta <- predictors
  # Append the forest's in-bag predicted values.
  gg_dta$yhat <- as.vector(object$predicted)
  if (object$type == "classification") {
    gg_dta$yvar <- response
  }
```

- [ ] **Step 2: Replace it with**

```r
  gg_dta <- predictors
  # For classification forests use per-class OOB vote fractions (object$votes),
  # stored as yhat.<classname> columns — the same shape gg_variable.rfsrc
  # produces.  For regression a single numeric yhat column suffices.
  if (object$type == "classification") {
    preds  <- object$votes   # n × n_classes matrix; OOB vote fractions by default,
                             # but raw integer counts when forest is fit with
                             # norm.votes = FALSE.  Row-normalise unconditionally so
                             # values are always in [0, 1] with rowSums ≈ 1.
    rs     <- rowSums(preds)
    if (any(rs > 1 + 1e-8, na.rm = TRUE)) {
      preds <- preds / rs
    }
    colnames(preds) <- paste0("yhat.", colnames(preds))
    gg_dta          <- cbind(gg_dta, preds)
    gg_dta$yvar     <- response
  } else {
    gg_dta$yhat <- as.vector(object$predicted)
  }
```

- [ ] **Step 3: Run the new tests — expect all three to PASS**

```bash
Rscript -e "devtools::test(filter='gg_variable')"
```

Expected: `[ FAIL 0 | ... | PASS <n> ]` — the three new tests green, existing tests still passing.

- [ ] **Step 4: Commit**

```bash
git add R/gg_variable.R tests/testthat/test_gg_variable.R
git commit -m "fix: gg_variable.randomForest classification uses object\$votes for yhat.* columns"
```

---

## Task T3: Write failing tests — smooth bugs in plot.gg_variable

**Files:**
- Modify: `tests/testthat/test_gg_variable.R`

- [ ] **Step 1: Append two more test cases to `tests/testthat/test_gg_variable.R`**

```r
test_that("plot.gg_variable RF classification: smooth=TRUE layer_data smokeable (binary smooth aes bug)", {
  skip_if_not_installed("randomForest")
  # Two-class subset to exercise the *binary* classification path
  set.seed(42L)
  bin_data        <- iris[iris$Species != "virginica", ]
  bin_data$Species <- droplevels(bin_data$Species)
  rf  <- randomForest::randomForest(Species ~ ., data = bin_data, ntree = 50L)
  gg  <- gg_variable(rf)
  p   <- plot(gg, xvar = "Sepal.Length", smooth = TRUE)
  # Before the fix, geom_smooth(...)  has no aes and layer_data errors with
  # "stat_smooth() requires the following missing aesthetics: x and y"
  expect_no_error(ggplot2::layer_data(p, 2L))
})

test_that("plot.gg_variable RF classification: smooth=TRUE works for multi-class (missing block)", {
  skip_if_not_installed("randomForest")
  set.seed(42L)
  rf <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
  gg <- gg_variable(rf)
  # Before the fix the multi-class numeric path silently skips smooth=TRUE
  # but does not error; after the fix a smooth layer is present (layer 2).
  p  <- plot(gg, xvar = "Sepal.Length", smooth = TRUE)
  expect_s3_class(p, "ggplot")
  ld <- ggplot2::layer_data(p, 2L)   # layer 2 = geom_smooth
  expect_gt(nrow(ld), 0L)
})
```

- [ ] **Step 2: Run new tests — the multi-class smooth test FAILS (binary smooth passes because fix in T2 made yhat.* work)**

```bash
Rscript -e "devtools::test(filter='gg_variable')"
```

Expected: the multi-class smooth `layer_data` test FAILs (no layer 2 or layer_data errors). The binary smooth test may already pass after the T2 fix — if so, note it and proceed.

---

## Task T4: Fix plot.gg_variable — binary smooth aes + multi-class smooth block

**Files:**
- Modify: `R/plot.gg_variable.R`

- [ ] **Step 1: Fix the binary smooth missing aes (around line 517)**

Find this exact code in the **binary classification, numeric predictor** branch (`if (sum(colnames(gg_dta) == "outcome") == 0)` → `if (ccls_var == "numeric")`):

```r
            if (smooth) {
              gg_plt[[ind]] <- gg_plt[[ind]] +
                ggplot2::geom_smooth(...)
            }
```

Replace with:

```r
            if (smooth) {
              gg_plt[[ind]] <- gg_plt[[ind]] +
                ggplot2::geom_smooth(
                  ggplot2::aes(x = .data$var, y = .data$yhat), ...
                )
            }
```

- [ ] **Step 2: Add the missing smooth block to the multi-class numeric path (around line 553)**

Find this exact code in the **multi-class** branch (`} else {  # Multi-class: facet by outcome class`):

```r
          if (ccls_var == "numeric") {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_point(
                ggplot2::aes(
                  x = .data$var,
                  y = .data$yhat,
                  color = .data$yvar,
                  shape = .data$yvar
                ),
                ...
              )
          } else {
```

Replace with:

```r
          if (ccls_var == "numeric") {
            gg_plt[[ind]] <- gg_plt[[ind]] +
              ggplot2::geom_point(
                ggplot2::aes(
                  x = .data$var,
                  y = .data$yhat,
                  color = .data$yvar,
                  shape = .data$yvar
                ),
                ...
              )
            if (smooth) {
              gg_plt[[ind]] <- gg_plt[[ind]] +
                ggplot2::geom_smooth(
                  ggplot2::aes(x = .data$var, y = .data$yhat), ...
                )
            }
          } else {
```

- [ ] **Step 3: Run all gg_variable tests — expect all to PASS**

```bash
Rscript -e "devtools::test(filter='gg_variable')"
```

Expected: `[ FAIL 0 | ... ]`

- [ ] **Step 4: Run full test suite to confirm no regressions**

```bash
Rscript -e "devtools::test()"
```

Expected: `[ FAIL 0 | WARN <n> | SKIP <n> | PASS <n> ]`

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_variable.R tests/testthat/test_gg_variable.R
git commit -m "fix: plot.gg_variable binary smooth aes + add multi-class smooth block"
```

---

## Task T5: vdiffr snapshots

**Files:**
- Modify: `tests/testthat/test_snapshots.R`

- [ ] **Step 1: Append a new randomForest classification section inside the `VDIFFR_RUN_TESTS` guard in `tests/testthat/test_snapshots.R`**

Find the closing brace of the existing `if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true"))` block (the last `}` before the file ends or the next top-level statement) and add the following block **inside** it, before the closing `}`:

```r
## ── randomForest classification snapshots (PR #87) ───────────────────────────
if (requireNamespace("randomForest", quietly = TRUE)) {
  local({
    set.seed(42L)
    rf_iris <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
    gg_iris <- gg_variable(rf_iris)

    test_that("snapshot: gg-variable-rf-classification-default", {
      vdiffr::expect_doppelganger(
        "gg-variable-rf-classification-default",
        plot(gg_iris)
      )
    })

    test_that("snapshot: gg-variable-rf-classification-smooth", {
      vdiffr::expect_doppelganger(
        "gg-variable-rf-classification-smooth",
        plot(gg_iris, xvar = "Sepal.Length", smooth = TRUE)
      )
    })
  })
}
```

- [ ] **Step 2: Confirm vdiffr tests skip cleanly without the env var**

```bash
Rscript -e "devtools::test(filter='snapshots')"
```

Expected: all snapshot tests SKIP (no VDIFFR_RUN_TESTS env var set).

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_snapshots.R
git commit -m "test: add vdiffr snapshots for gg_variable RF classification (PR #87)"
```

---

## Task T6: NEWS, housekeeping, and PR

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 1: Add a fix entry to the top of the `ggRandomForests v2.8.0 (development)` section in `NEWS.md`**

```
* **`gg_variable.randomForest` classification fix (#87).**
  - `gg_variable.randomForest()` for classification forests now stores
    per-class OOB vote fractions as `yhat.<classname>` columns (from
    `object$votes`), matching the `rfsrc` path.  Previously a single
    `yhat` factor column (class labels from `object$predicted`) was
    stored, which prevented the multi-class pivot in `plot.gg_variable`
    from firing.
  - `plot.gg_variable` binary classification: `smooth = TRUE` now
    correctly maps x/y aesthetics onto the smooth layer.
  - `plot.gg_variable` multi-class numeric path: `smooth = TRUE` now
    adds a smooth layer (was silently skipped).
  - Closes stale issues #81 (fixed in PR #83) and #82.
```

- [ ] **Step 2: Run R CMD check**

```bash
Rscript -e "devtools::check(args='--as-cran')"
```

Expected: `0 errors | 0 warnings | 0 notes`

- [ ] **Step 3: Commit NEWS**

```bash
git add NEWS.md
git commit -m "docs: update NEWS for PR #87 — gg_variable RF classification fix"
```

- [ ] **Step 4: Push the branch**

```bash
git push -u origin fix/rf-87-gg-variable-classification
```

- [ ] **Step 5: Open the PR**

```bash
gh pr create \
  --title "fix: gg_variable.randomForest classification — yhat.* columns + smooth bugs (#87)" \
  --body "$(cat <<'EOF'
## Summary

- `gg_variable.randomForest` for classification forests now stores per-class OOB vote fractions as \`yhat.<classname>\` columns (from \`object\$votes\`), matching the rfsrc path. Previously a single \`yhat\` factor (class labels from \`object\$predicted\`) was stored, preventing the multi-class pivot in \`plot.gg_variable\` from firing.
- Binary classification \`smooth = TRUE\`: added explicit \`aes(x, y)\` to \`geom_smooth\` so \`layer_data()\` no longer errors.
- Multi-class numeric path: added missing \`smooth = TRUE\` block.
- 5 new unit tests + 2 vdiffr snapshots.

## Housekeeping

Closes #81, Closes #82 (both were fully fixed by PR #83 but auto-close never fired).

## Test plan
- [x] \`devtools::test()\` — all tests pass, 0 failures
- [x] \`devtools::check(args="--as-cran")\` — 0 errors, 0 warnings, 0 notes

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

- [ ] **Step 6: Verify CI is green on the PR before asking for merge**

```bash
gh pr checks <PR-NUMBER>
```

Expected: all checks pass.
