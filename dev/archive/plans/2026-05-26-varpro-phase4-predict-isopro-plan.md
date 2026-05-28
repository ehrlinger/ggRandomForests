# varPro Phase 4 — predict.isopro Wrapper Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend `gg_isopro()` with a `newdata` argument so a fitted `varPro::isopro` model can score new observations into the same tidy `gg_isopro` frame, with the same plot / print / summary / autoplot S3 companions.

**Architecture:** One new optional argument on the existing generic and method. When `newdata` is supplied, the method calls `predict(object, newdata, ...)` twice — once with `quantiles = FALSE` for the raw `case.depth` column, once with `quantiles = TRUE` for the per-row quantile — and computes `howbad = 1 - quantile` so the wrapper convention ("higher = more anomalous") holds across train and test. Returns the same `c("gg_isopro", "data.frame")` shape; provenance attribute gains a `prediction = TRUE` field.

**Tech Stack:** R, varPro (Imports), testthat, vdiffr (Suggests).

**Spec:** `dev/plans/2026-05-26-varpro-phase4-predict-isopro-design.md`.

**Branch state:** `feat/varpro-phase4-predict-isopro` already exists from `origin/main`; the spec is already committed on it. Version on `main` is `2.7.3.9008`; this PR bumps to `2.7.3.9010` to leave `.9009` for PR #95 (currently open). If PR #95 has merged by the time this implementation runs, the implementer keeps `.9010`; if PR #95 has been closed without merging, the implementer drops to `.9009`. Either way: bump to the next available `.900x` slot above what is on `main` at branch-execution time.

---

## File map

| File | Purpose |
|------|---------|
| `R/gg_isopro.R` *(modify)* | Add `newdata` arg to generic + method; new internal scoring path |
| `tests/testthat/test_gg_isopro.R` *(append)* | Six new tests for the predict path |
| `tests/testthat/test_snapshots.R` *(append)* | One vdiffr snapshot (train+test overlay) |
| `DESCRIPTION` *(edit)* | Version bump (see branch-state note above) |
| `NEWS.md` *(edit)* | v2.8.0 dev entry |

No new R files. No new S3 methods. No new plot logic.

---

## Task 0: Version bump

**Files:**
- Modify: `DESCRIPTION:4`

- [ ] **Step 1: Confirm branch state**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests
git branch --show-current   # expect: feat/varpro-phase4-predict-isopro
git log --oneline -1        # expect: the design-spec commit (and the polarity-sharpening commit)
grep "^Version:" DESCRIPTION
```

- [ ] **Step 2: Determine the next available .900x slot**

Run `git fetch origin --quiet` and check what `Version:` is on `origin/main` and on any open PR branches. The intent is to bump to the smallest `.900x` value strictly larger than what `origin/main` currently has — and to avoid colliding with any sibling open PR's version. As of this plan's writing, `main` is at `2.7.3.9008`; PR #95 claims `.9009`; this PR takes `.9010`.

If `main` has moved past `.9008` by the time you run this, take the next free slot. Edit `DESCRIPTION`:

```
Version: 2.7.3.9010
```
(or the slot you determined).

- [ ] **Step 3: Confirm package still loads**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE)" 2>&1 | tail -2
```
Expected: no errors.

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION
git commit -m "chore: open v2.7.3.9010 dev cycle (varPro Phase 4 predict.isopro)"
```
(Substitute your chosen `.900x` if different from `.9010`.)

---

## Task 1: Failing tests — extractor shape on `newdata` (TDD red)

**Files:**
- Modify: `tests/testthat/test_gg_isopro.R` (append)

- [ ] **Step 1: Append three shape-and-provenance tests**

Append to `tests/testthat/test_gg_isopro.R`:

```r
## ── predict.isopro path (PR for Phase 4b) ──────────────────────────────────

test_that("gg_isopro: newdata returns gg_isopro data.frame with correct columns", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit, newdata = iris[, 1:4])
  expect_s3_class(gg, "gg_isopro")
  expect_s3_class(gg, "data.frame")
  expect_named(gg, c("obs", "case.depth", "howbad"), ignore.order = TRUE)
  expect_equal(nrow(gg), nrow(iris))
  expect_equal(gg$obs, seq_len(nrow(iris)))
  expect_true(all(gg$howbad >= 0 & gg$howbad <= 1))
})

test_that("gg_isopro: newdata provenance carries prediction = TRUE and right n", {
  fit  <- make_iso_fit(ntree = 25)
  test_df <- iris[c(1:10, 51:60, 101:110), 1:4]  # 30 rows
  gg   <- gg_isopro(fit, newdata = test_df)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_equal(prov$source, "varPro::isopro")
  expect_equal(prov$n, nrow(test_df))
  expect_equal(prov$ntree, 25)
  expect_true(isTRUE(prov$prediction))
})

test_that("gg_isopro: training-path provenance has no prediction field set TRUE", {
  fit  <- make_iso_fit()
  gg   <- gg_isopro(fit)
  prov <- attr(gg, "provenance")
  # The training extractor either omits prediction or sets it to FALSE/NULL.
  expect_false(isTRUE(prov$prediction))
})
```

- [ ] **Step 2: Run — expect FAIL on the newdata path**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```
Expected: the three new tests FAIL (`gg_isopro` does not accept `newdata` yet so it routes the data frame into `...` and silently returns the training tidy frame instead). The third test (provenance without `prediction = TRUE`) may pass already; that's fine.

---

## Task 2: Implement `newdata` path (TDD green)

**Files:**
- Modify: `R/gg_isopro.R` (signature + method body)

- [ ] **Step 1: Update the generic to accept `newdata`**

In `R/gg_isopro.R`, find the generic definition:

```r
gg_isopro <- function(object, ...) {
  UseMethod("gg_isopro", object)
}
```

Replace with:

```r
gg_isopro <- function(object, newdata = NULL, ...) {
  UseMethod("gg_isopro", object)
}
```

- [ ] **Step 2: Replace the method body with the dual-path implementation**

In `R/gg_isopro.R`, find the whole `gg_isopro.isopro` function and replace its body so it branches on `newdata`:

```r
#' @export
gg_isopro.isopro <- function(object, newdata = NULL, ...) {
  if (!inherits(object, "isopro")) {
    stop("gg_isopro expects a 'isopro' object from varPro::isopro().",
         call. = FALSE)
  }

  ntree <- tryCatch(
    as.integer(object$isoforest$ntree),
    error = function(e) NA_integer_
  )
  ntree <- if (length(ntree) == 1L && !is.na(ntree)) ntree else NA_integer_

  ## ---- Training path (newdata = NULL) ------------------------------------
  if (is.null(newdata)) {
    howbad <- as.numeric(object$howbad)
    depth  <- as.numeric(object$case.depth)
    n      <- length(howbad)

    gg_dta <- data.frame(
      obs        = seq_len(n),
      case.depth = depth,
      howbad     = howbad
    )
    class(gg_dta) <- c("gg_isopro", class(gg_dta))
    attr(gg_dta, "provenance") <- list(
      source     = "varPro::isopro",
      n          = n,
      ntree      = ntree,
      prediction = FALSE
    )
    return(invisible(gg_dta))
  }

  ## ---- Prediction path (newdata supplied) -------------------------------
  if (!is.data.frame(newdata)) {
    stop("`newdata` must be a data.frame.", call. = FALSE)
  }

  # Two calls to predict.isopro: raw depth and quantile-against-training.
  # The wrapper polarity is "higher = more anomalous", so we flip the quantile:
  #   howbad = 1 - predict(object, newdata, quantiles = TRUE)
  # case.depth keeps varPro's native scale (lower = more anomalous), giving
  # the user a varPro-polarity number for cross-reference.
  depth <- as.numeric(stats::predict(object, newdata = newdata,
                                     quantiles = FALSE))
  q     <- as.numeric(stats::predict(object, newdata = newdata,
                                     quantiles = TRUE))
  howbad <- 1 - q
  n      <- nrow(newdata)

  gg_dta <- data.frame(
    obs        = seq_len(n),
    case.depth = depth,
    howbad     = howbad
  )
  class(gg_dta) <- c("gg_isopro", class(gg_dta))
  attr(gg_dta, "provenance") <- list(
    source     = "varPro::isopro",
    n          = n,
    ntree      = ntree,
    prediction = TRUE
  )
  invisible(gg_dta)
}
```

- [ ] **Step 3: Regenerate Rd**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
```

The `gg_isopro.Rd` `\usage{}` block will gain `newdata = NULL`. The `@param` lines come later (Task 5); for now the Rd is valid but the new arg is undocumented — Task 5 fills that in.

- [ ] **Step 4: Run Task 1 tests — expect PASS**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```
Expected: `[ FAIL 0 | ... ]`. The three Task 1 tests pass; the original 14 PR #94 tests still pass.

- [ ] **Step 5: Commit**

```bash
git add R/gg_isopro.R man/gg_isopro.Rd NAMESPACE tests/testthat/test_gg_isopro.R
git commit -m "feat(gg_isopro): newdata argument for predict.isopro scoring"
```

---

## Task 3: Validation + sanity tests

**Files:**
- Modify: `tests/testthat/test_gg_isopro.R` (append)

- [ ] **Step 1: Append three more tests**

Append to `tests/testthat/test_gg_isopro.R`:

```r
test_that("gg_isopro: newdata must be a data.frame", {
  fit <- make_iso_fit()
  expect_error(gg_isopro(fit, newdata = "not a df"),
               "newdata must be a data.frame")
  expect_error(gg_isopro(fit, newdata = 1:10),
               "newdata must be a data.frame")
  expect_error(gg_isopro(fit, newdata = list(a = 1)),
               "newdata must be a data.frame")
})

test_that("gg_isopro: scoring the training set as newdata matches training howbad in range and top-5 ordering", {
  fit       <- make_iso_fit()
  train_df  <- iris[, 1:4]
  gg_train  <- gg_isopro(fit)
  gg_pred   <- gg_isopro(fit, newdata = train_df)
  # The two code paths inside varPro are slightly different so byte equality
  # is too strong, but the score range and the most-anomalous rows should agree.
  expect_equal(nrow(gg_pred), nrow(gg_train))
  expect_true(all(gg_pred$howbad  >= 0 & gg_pred$howbad  <= 1))
  expect_true(all(gg_train$howbad >= 0 & gg_train$howbad <= 1))
  top_train <- head(order(-gg_train$howbad), 5)
  top_pred  <- head(order(-gg_pred$howbad),  5)
  # At least 3 of the top-5 anomalous rows should overlap between the two paths.
  expect_gte(length(intersect(top_train, top_pred)), 3L)
})

test_that("gg_isopro: howbad == 1 - predict(quantiles = TRUE)", {
  fit     <- make_iso_fit()
  test_df <- iris[1:30, 1:4]
  gg      <- gg_isopro(fit, newdata = test_df)
  q_raw   <- as.numeric(stats::predict(fit, newdata = test_df, quantiles = TRUE))
  expect_equal(gg$howbad, 1 - q_raw, tolerance = 1e-12)
})
```

- [ ] **Step 2: Run — expect PASS (Task 2 implementation already covers them)**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```
Expected: `[ FAIL 0 | ... ]`. Six new tests added in Task 1 + Task 3 (three each).

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_isopro.R
git commit -m "test(gg_isopro): newdata validation and polarity-flip sanity checks"
```

---

## Task 4: Overlay smoke test (train + test bound with a `method` column)

**Files:**
- Modify: `tests/testthat/test_gg_isopro.R` (append)

- [ ] **Step 1: Append one more test**

Append to `tests/testthat/test_gg_isopro.R`:

```r
test_that("gg_isopro: bind_rows(train, test) plots without error via the method-column path", {
  fit      <- make_iso_fit()
  test_df  <- iris[seq(1, nrow(iris), by = 3), 1:4]  # 50 rows
  gg_train <- gg_isopro(fit)
  gg_test  <- gg_isopro(fit, newdata = test_df)
  gg_both  <- rbind(
    cbind(as.data.frame(gg_train), method = "train"),
    cbind(as.data.frame(gg_test),  method = "test")
  )
  class(gg_both) <- c("gg_isopro", "data.frame")
  p <- plot(gg_both, panel = "both")
  expect_s3_class(p, "patchwork")
  # Each sub-plot must actually build (catches aes/data mismatches the way the
  # original plot.gg_variable regression test did).
  built_top <- ggplot2::ggplot_build(unclass(p))
  expect_true(inherits(built_top, "ggplot_built"))
  for (sub in p$patches$plots) {
    expect_no_error(ggplot2::ggplot_build(sub))
  }
})
```

- [ ] **Step 2: Run — expect PASS**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```
Expected: `[ FAIL 0 | ... ]`.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_isopro.R
git commit -m "test(gg_isopro): train + test overlay via the method-column path"
```

---

## Task 5: Roxygen — newdata arg + scoring-new-data section

**Files:**
- Modify: `R/gg_isopro.R` (roxygen above the generic)

- [ ] **Step 1: Add the `@param newdata` line in the terse register**

In `R/gg_isopro.R`, find the existing `@param object` line and the `@param ...` line right below it. Insert a new `@param newdata` line between them:

```r
#' @param newdata Optional `data.frame` of new observations to score against
#'   the fit. When `NULL` (default) the extractor returns the in-sample tidy
#'   frame from the fit's stored `$case.depth` and `$howbad`. When supplied,
#'   each row is scored via [varPro::predict.isopro()] and the same tidy
#'   shape is returned for the test data.
```

- [ ] **Step 2: Add the narrative-register section explaining the new-data path**

Below the existing `@section What you use this for:` block (and before the `@param object` line — sections live above params in the rendered Rd), insert this new section:

```r
#' @section Scoring new data:
#' Pass a `data.frame` as `newdata` and the extractor calls
#' [varPro::predict.isopro()] twice: once with `quantiles = FALSE` to get the
#' raw mean case depth per row, and once with `quantiles = TRUE` to get the
#' per-row quantile of that depth against the training-data depth
#' distribution.
#'
#' varPro's `predict.isopro` returns quantiles where *smaller is more
#' anomalous*, which is the opposite polarity of the wrapper's `howbad`
#' (where *higher* is more anomalous). The wrapper exposes both
#' conventions so nothing is hidden:
#' - `case.depth` carries varPro's native polarity — *lower = more
#'   anomalous*. This is the unmodified output of
#'   `predict(object, newdata, quantiles = FALSE)`. Use it to
#'   cross-reference against raw varPro output.
#' - `howbad` is the flipped, wrapper-convention version. The relationship
#'   is `howbad = 1 - predict(object, newdata, quantiles = TRUE)`.
#'
#' To overlay training and test scores in one plot, bind the two extractor
#' calls with a `method` label column (the same column [plot.gg_isopro()]
#' uses to colour rnd / unsupv / auto comparisons):
#'
#' ```
#' gg_train <- gg_isopro(fit)
#' gg_test  <- gg_isopro(fit, newdata = test_df)
#' gg_both  <- rbind(cbind(gg_train, method = "train"),
#'                   cbind(gg_test,  method = "test"))
#' class(gg_both) <- c("gg_isopro", "data.frame")
#' plot(gg_both)
#' ```
```

- [ ] **Step 3: Update the "What you use this for" bullet list**

Find the existing `\itemize{...}` block under `@section What you use this for:` and add a new bullet at the end:

Find:
```r
#'   \item give the analyst a ranked list of "look at these first" cases
#'     for a manual review.
#' }
```

Insert one bullet before the closing `}`:

```r
#'   \item score a held-out cohort or a fresh batch of incoming data
#'     against a fitted model and compare the test scores to the training
#'     distribution.
```

Resulting block:

```r
#' @section What you use this for:
#' This is screening, not inference. Reach for it when you want to:
#' \itemize{
#'   \item flag observations that may be data-entry errors, out-of-range
#'     measurements, or distinct subpopulations before fitting a primary
#'     model;
#'   \item check whether a held-out cohort sits inside the training
#'     distribution before scoring with a model trained elsewhere;
#'   \item give the analyst a ranked list of "look at these first" cases
#'     for a manual review;
#'   \item score a held-out cohort or a fresh batch of incoming data
#'     against a fitted model and compare the test scores to the training
#'     distribution.
#' }
```

(Note the trailing comma added to the third bullet so the list parses cleanly.)

- [ ] **Step 4: Regenerate Rd**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
```

Expected: `man/gg_isopro.Rd` updated; usage block shows `gg_isopro(object, newdata = NULL, ...)`; the new section and the new `@param newdata` block both render.

- [ ] **Step 5: Confirm tests still pass**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```
Expected: `[ FAIL 0 | ... ]`.

- [ ] **Step 6: Commit**

```bash
git add R/gg_isopro.R man/gg_isopro.Rd
git commit -m "docs(gg_isopro): document newdata arg and the polarity flip"
```

---

## Task 6: vdiffr snapshot for the train+test overlay

**Files:**
- Modify: `tests/testthat/test_snapshots.R` (append inside the VDIFFR guard)

- [ ] **Step 1: Locate the end of the existing VDIFFR guard block**

```bash
grep -n "VDIFFR_RUN_TESTS\|^}" tests/testthat/test_snapshots.R | tail
```

Find the closing `}` of the `if (... VDIFFR_RUN_TESTS == "true") { ... }` block. (PR #94 added a `gg_isopro snapshots` section near the end; the new snapshot lives next to it.)

- [ ] **Step 2: Insert the new snapshot immediately after the existing `gg-isopro` block, still inside the guard**

```r
## ── gg_isopro predict.isopro overlay snapshot (Phase 4b) ──────────────────
if (requireNamespace("varPro", quietly = TRUE)) {
  local({
    set.seed(1L)
    fit      <- varPro::isopro(data = iris[, 1:4], method = "rnd",
                               sampsize = 32, ntree = 50)
    test_df  <- iris[seq(1, nrow(iris), by = 3), 1:4]
    gg_train <- gg_isopro(fit)
    gg_test  <- gg_isopro(fit, newdata = test_df)
    gg_both  <- rbind(
      cbind(as.data.frame(gg_train), method = "train"),
      cbind(as.data.frame(gg_test),  method = "test")
    )
    class(gg_both) <- c("gg_isopro", "data.frame")

    test_that("snapshot: gg-isopro-predict-overlay", {
      vdiffr::expect_doppelganger("gg-isopro-predict-overlay", plot(gg_both))
    })
  })
}
```

- [ ] **Step 3: Confirm snapshots skip cleanly without the env var**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_snapshots.R', reporter = 'progress')"
```
Expected: SKIP (no `VDIFFR_RUN_TESTS=true`). No failures.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test_snapshots.R
git commit -m "test: vdiffr snapshot for gg_isopro train+test overlay"
```

---

## Task 7: NEWS, R CMD check, push, open PR

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 1: Update `NEWS.md`**

Open `NEWS.md`. Change the line at the top:

```
Version: 2.7.3.9008
```

to your chosen `.900x` slot from Task 0 (e.g.):

```
Version: 2.7.3.9010
```

Then add this bullet at the very top of the `ggRandomForests v2.8.0 (development) — continued` section (above the existing first bullet):

```
* `gg_isopro()` gains a `newdata` argument so a fitted `varPro::isopro`
  model can score new observations into the same tidy `gg_isopro` frame.
  Internally the wrapper calls `predict.isopro()` twice: with
  `quantiles = FALSE` to populate the `case.depth` column (varPro's native
  polarity, lower = more anomalous) and with `quantiles = TRUE` to compute
  `howbad = 1 - quantile` (the wrapper convention, higher = more anomalous).
  Both polarities are visible in the returned data frame, and the
  relationship is named in the roxygen. The plot / print / summary / autoplot
  S3 companions work unchanged on the new tidy frame; to overlay training
  and test scores, bind the two extractor calls with a `method` label
  column and pass the result to `plot()`. Second of three Phase 4
  sub-projects.
```

- [ ] **Step 2: Full R CMD check**

```bash
Rscript -e "devtools::check(args = '--as-cran', quiet = TRUE)" 2>&1 | tail -8
```

Expected: `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`.

If there are notes about non-ASCII characters or doc/code mismatches, fix them before committing. (The voice fingerprint says: prefer ASCII; em-dashes sparingly. If PR #95 has merged by now, markdown roxygen is enabled and the existing Rd-style `\code{}` / `\link{}` markup still works alongside it.)

- [ ] **Step 3: Commit NEWS**

```bash
git add NEWS.md
git commit -m "docs: NEWS entry for varPro Phase 4b predict.isopro wrapper"
```

- [ ] **Step 4: Push branch**

```bash
git push -u origin feat/varpro-phase4-predict-isopro
```

- [ ] **Step 5: Open the PR**

```bash
gh pr create \
  --title "feat: gg_isopro newdata arg — varPro Phase 4b predict.isopro wrapper" \
  --body "$(cat <<'EOF'
## Summary

Second of three Phase 4 sub-projects. Adds a `newdata` argument to
`gg_isopro()` so a fitted `varPro::isopro` model can score new observations
into the same tidy `gg_isopro` frame, with the same plot / print / summary /
autoplot S3 companions as the training extractor.

- `gg_isopro(fit, newdata = test_df)` returns the same `c("gg_isopro",
  "data.frame")` shape: `obs / case.depth / howbad`.
- Internally calls `predict(fit, newdata, quantiles = FALSE)` for `case.depth`
  and `predict(fit, newdata, quantiles = TRUE)` for the quantile, then
  computes `howbad = 1 - quantile` so the wrapper convention
  ("higher = more anomalous") holds across train and test.
- `case.depth` keeps varPro's native polarity (lower = more anomalous), so
  the wrapper isn't hiding the transformation — both polarities are visible.
- To overlay training and test, bind the two extractor outputs with a
  `method` label column; `plot.gg_isopro()` colour-groups by it (the same
  column it uses for rnd / unsupv / auto comparisons in PR #94).

## Test plan
- [x] `devtools::test()` — 7 new tests, all pass; the 14 PR #94 tests
  still green.
- [x] `devtools::check(args = '--as-cran')` — 0 errors, 0 warnings, 0 notes.
- [x] vdiffr snapshot added (`gg-isopro-predict-overlay`), skip cleanly
  without `VDIFFR_RUN_TESTS=true`.

Spec: \`dev/plans/2026-05-26-varpro-phase4-predict-isopro-design.md\`
Plan: \`dev/plans/2026-05-26-varpro-phase4-predict-isopro-plan.md\`

Next Phase 4 sub-projects: \`gg_beta_varpro\`, then \`gg_ivarpro\`.
EOF
)"
```

- [ ] **Step 6: Verify CI green**

```bash
gh pr checks $(gh pr view --json number --jq .number)
```
Expected: all checks pass.

---

## Self-Review

**1. Spec coverage:**
- Extractor signature (`newdata = NULL`) → Tasks 1, 2 ✓
- Internal flow (two predict calls, `howbad = 1 - quantile`) → Task 2 ✓
- Polarity exposed via `case.depth` (raw) + `howbad` (flipped) → Task 2 + Task 5 roxygen ✓
- Provenance `prediction = TRUE` → Task 2 + Task 1 test ✓
- Validation: non-data.frame `newdata` errors → Task 2 + Task 3 test ✓
- Train/test overlay via `method`-column reuse → Task 4 test + Task 6 snapshot + Task 5 roxygen example ✓
- Roxygen: new `@param newdata`, new "Scoring new data" section with the
  transformation written in code form, updated use-cases list → Task 5 ✓
- Version bump + NEWS + R CMD check + PR → Tasks 0, 7 ✓
- Acceptance criteria (0/0/0; 7 new + 14 old tests pass) → Task 7 Step 2 ✓
- Out-of-scope items honoured (no new function, no new S3 method, no
  generalised auto-detect, no exposed `quantiles` arg) ✓

**2. Placeholder scan:** Every code step shows the actual code; every test
step shows the actual test; every command shows the expected output. The
version-bump task explicitly handles the "what `.900x` slot to take" question
in plain English rather than leaving it ambiguous.

**3. Type consistency:** Signature `gg_isopro(object, newdata = NULL, ...)`
is consistent between the generic (Task 2 Step 1), the method (Task 2
Step 2), the roxygen `@param` lines (Task 5), the Rd usage block (regenerated
in Task 2 Step 3 and Task 5 Step 4), and the test calls in Tasks 1, 3, 4.
The returned tidy frame has columns `obs / case.depth / howbad` and class
`c("gg_isopro", "data.frame")` everywhere it appears. Provenance fields
`source / n / ntree / prediction` are consistent between the training and
prediction paths in Task 2 and the assertions in Tasks 1 and 3.
