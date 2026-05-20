# randomForest Engine Validation & Repair — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Validate every supported `randomForest` extractor × family in ggRandomForests, permanently guard them with regression tests, and fix #80 (`plot.gg_variable` bare-list return), #81 (`calc_roc.randomForest` matrix mis-index + `oob`/macro-average), and the `plot.gg_error` `color="Outcome"` unknown-label wart.

**Architecture:** Coverage-first / TDD. Build the full `randomForest` validation matrix first (it makes #80/#81 fail red and surfaces siblings), then fix to green, then snapshots + Rd docs. Fixes follow the established v2.7.3 #77/#78 list→single-`patchwork` pattern. The shared `.validate_which_outcome` / `calc_roc.rfsrc` are left byte-unchanged (rfsrc non-goal); macro-average lives only in the randomForest path.

**Tech Stack:** R package tooling — `devtools`, `roxygen2` 8.0.0 (matches committed `Config/roxygen2/version`), `testthat` 3.x, `vdiffr` (opt-in), `patchwork`, `randomForest`, `randomForestSRC`. Base: `fix/randomforest-engine-validation` @ `f3ffd38` (Phase 0 merged). Design spec: `dev/plans/2026-05-19-randomforest-engine-validation-design.md`. Meta-issue #82 (#80, #81).

---

## File Structure

| File | Responsibility | Action |
|---|---|---|
| `DESCRIPTION` | dev version | Modify (bump `2.7.3.900x`) |
| `NEWS.md` | changelog | Modify (bullets under `v2.8.0 (development)`) |
| `tests/testthat/test_plot_layer_data.R` | structural non-empty/contract guards | Modify (add RF matrix) |
| `tests/testthat/test_gg_roc.R` | ROC numeric correctness + rfsrc characterization | Modify |
| `tests/testthat/test_gg_variable.R` | gg_variable contract test | Modify (`:38-41`, audit `:106`) |
| `tests/testthat/test_snapshots.R` | vdiffr visual snapshots | Modify (RF block) |
| `R/plot.gg_variable.R` | #80 fix + `@return` | Modify (`:604-609`, `:29-31`) |
| `R/calc_roc.R` | #81 indexing/macro-average (RF path only) | Modify (`calc_roc.randomForest` `:154-196`) |
| `R/gg_roc.R` | #81 `oob` plumbing + Rd | Modify (`:26-33`, `:134`, `:153-156`) |
| `R/plot.gg_error.R` | colour-label wart | Modify (`:244,248`) |
| `R/gg_partial.R`, `R/gg_survival.R`, `R/gg_brier.R` | rfsrc-only `@note` | Modify (roxygen) |
| `man/*.Rd` | regenerated docs | Modify (via `devtools::document()`) |

**Logistics (from spec §3, locked):** execute on this branch. roxygen2 is 8.0.0 — `devtools::document()` must leave `DESCRIPTION` byte-identical except intended edits. Plans/specs are in `dev/plans/` (Rbuildignored `^dev$`, git-tracked) and mirrored to the vault by the controller, not by execution tasks. Per-task: subagent-driven, spec + code-quality review.

---

## Task 1: Open the dev increment

**Files:** Modify `DESCRIPTION` (`Version:`), `NEWS.md`.

- [ ] **Step 1: Read the current dev version**

Run:
```bash
R -q -e 'cat(read.dcf("DESCRIPTION")[,"Version"], "\n")'
```
Expected: prints a `2.7.3.900x` value (baseline `2.7.3.9000`; could be higher if other v2.8 dev increments merged first). Call the printed value `<CUR>`.

- [ ] **Step 2: Compute and set the next increment**

If `<CUR>` is `2.7.3.9000`, the new version is `2.7.3.9001`. Otherwise increment the 4th component by 1. Edit `DESCRIPTION` line 4 (`Version: <CUR>`) to `Version: <NEW>`. Edit `DESCRIPTION` `Date:` to today (`2026-05-19`).

- [ ] **Step 3: Add the NEWS scaffold bullet AND sync the NEWS Version header**

In `NEWS.md`:
  (a) update line 2 `Version:` to match the new `DESCRIPTION` `Version:` (Phase 0 convention — `ggrandomforests.news()` reads this file, so the header must mirror DESCRIPTION; precedent: Phase 0 commit `92cf19c`). E.g. `Version: 2.7.3.9000` → `Version: <NEW>`.
  (b) under the existing `ggRandomForests v2.8.0 (development)` heading, add (the detailed sub-bullets are filled in by Tasks 4–6/8):
```
* **randomForest engine validation & repair (#82).** Fixes #80, #81
  and a `plot.gg_error` label wart; adds full randomForest regression
  coverage. See sub-items below.
```

- [ ] **Step 4: Verify + commit**

Run:
```bash
R -q -e 'd<-read.dcf("DESCRIPTION"); stopifnot(grepl("^2\\.7\\.3\\.900[1-9]$", d[,"Version"])); n<-readLines("NEWS.md"); stopifnot(n[2] == paste0("Version: ", d[,"Version"])); cat("OK", d[,"Version"], "\n")'
git add DESCRIPTION NEWS.md
git commit -m "chore: open 2.7.3.900x dev increment for randomForest validation (#82)"
```
Expected: `OK 2.7.3.900x`. (The added `n[2] == paste0("Version: ", ...)` assertion guards against forgetting Step 3(a) — the convention violation surfaced in the first execution of this task.) Append `Co-Authored-By: Claude Opus 4.7 (1M context) <noreply@anthropic.com>` to this and every commit in this plan.

---

## Task 2: Capture fresh post-Phase-0 baseline

**Files:** none (measurement; makes later regressions attributable).

- [ ] **Step 1: Full check baseline**

Run (5–10 min; do not interrupt):
```bash
R -q -e 'devtools::check(document = TRUE, args = "--as-cran", error_on = "never")' 2>&1 | tee /tmp/ggrf-rf-baseline-check.log | tail -20
```
Expected: `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`. If not 0/0/0, STOP and report BLOCKED (the branch base must be clean — it is `f3ffd38`, the merged Phase 0).

- [ ] **Step 2: Test baseline**

Run:
```bash
R -q -e 'devtools::test()' 2>&1 | tee /tmp/ggrf-rf-baseline-test.log | tail -3
```
Expected: `[ FAIL 0 | WARN 0 | SKIP 3 | PASS 678 ]`. Record this `PASS 678` — Task 9 requires `FAIL 0` and `PASS` ≥ 678 + the new RF cases. No commit.

---

## Task 3: RF structural matrix — already-OK families (GREEN) + #80/#81 cases (RED)

**Files:** Modify `tests/testthat/test_plot_layer_data.R` (append; reuse existing `expect_layer_nonempty()` / `expect_layer_has_variation()` defined at `:17-48`). `setup.R` already attaches `survival`/`randomForestSRC`/`randomForest` (Phase 0), so bare `randomForest()` is available in tests.

- [ ] **Step 1: Append the randomForest matrix block**

Append to the end of `tests/testthat/test_plot_layer_data.R`:
```r
# ---------------------------------------------------------------------------
# randomForest engine matrix (#82 / #80 / #81)
# ---------------------------------------------------------------------------

test_that("plot.gg_rfsrc randomForest classification renders", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  p <- plot(gg_rfsrc(rf))
  expect_s3_class(p, "ggplot")
  expect_layer_nonempty(p)
})

test_that("plot.gg_rfsrc randomForest regression renders", {
  set.seed(42)
  rf <- randomForest::randomForest(mpg ~ ., data = mtcars)
  p <- plot(gg_rfsrc(rf))
  expect_s3_class(p, "ggplot")
  expect_layer_nonempty(p)
})

test_that("plot.gg_error randomForest classification renders", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  p <- plot(gg_error(rf))
  expect_s3_class(p, "ggplot")
  expect_layer_nonempty(p)
})

test_that("plot.gg_error randomForest regression has no stray colour label (#82 wart)", {
  set.seed(42)
  rf <- randomForest::randomForest(mpg ~ ., data = mtcars)
  p <- plot(gg_error(rf))
  expect_s3_class(p, "ggplot")
  expect_layer_nonempty(p)
  # #82 wart: plot.gg_error.R:244,248 apply labs(color="Outcome") even on
  # the regression / single-outcome path where no colour aesthetic is
  # mapped — which produces "Ignoring unknown labels: colour Outcome" at
  # build time. ggplot2 emits that warning once-per-session (cli .freq =
  # "once"), so warning-based detection is unreliable inside a long
  # test_file. Assert STRUCTURALLY: if a colour label is set, a colour
  # aesthetic must be mapped somewhere in the plot.
  has_colour_aes <- "colour" %in% names(p$mapping) ||
    any(vapply(p$layers, function(l) "colour" %in% names(l$mapping),
               logical(1)))
  if (!is.null(p$labels$colour)) {
    expect_true(has_colour_aes,
                info = "plot.gg_error set labs(colour=) without a mapped colour aesthetic")
  }
})

test_that("plot.gg_vimp randomForest classification renders (formula + non-formula)", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris, importance = TRUE)
  expect_layer_nonempty(plot(gg_vimp(rf)))
  rf2 <- randomForest::randomForest(iris[, 1:4], iris$Species, importance = TRUE)
  expect_layer_nonempty(plot(gg_vimp(rf2)))
})

test_that("plot.gg_vimp randomForest regression renders", {
  set.seed(42)
  rf <- randomForest::randomForest(mpg ~ ., data = mtcars, importance = TRUE)
  expect_layer_nonempty(plot(gg_vimp(rf)))
})

test_that("plot.gg_variable randomForest default multi-xvar is a single object (#80)", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  p <- plot(gg_variable(rf))                 # default xvar = all predictors
  # #80: post-fix this is a single ggplot/patchwork, not a bare list
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
  expect_layer_nonempty(p)
})

test_that("plot.gg_variable randomForest regression default multi-xvar is a single object (#80)", {
  set.seed(42)
  rf <- randomForest::randomForest(mpg ~ ., data = mtcars)
  p <- plot(gg_variable(rf))
  expect_true(inherits(p, "ggplot") || inherits(p, "patchwork"))
  expect_layer_nonempty(p)
})

test_that("plot.gg_roc randomForest classification renders (#81)", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  p <- plot(gg_roc(rf, which_outcome = 1))
  expect_s3_class(p, "ggplot")
  expect_layer_nonempty(p)
})
```

- [ ] **Step 2: Run; confirm RED-for-right-reasons + GREEN siblings**

Run:
```bash
R -q -e 'testthat::test_file("tests/testthat/test_plot_layer_data.R")' 2>&1 | tail -25
```
Expected: the `gg_rfsrc`/`gg_vimp`/`gg_error classification`/`gg_roc` RF tests **PASS**; **`gg_error` regression test FAILS** (unknown-label warning at `ggplot_build` — the wart); **both `gg_variable` `#80` tests FAIL** (currently a bare `list` of length nvars → `inherits(p,"ggplot|patchwork")` false / `layer_data` errors on a list). NOTE: the `gg_roc` structural test is *correctly green pre-fix* — a 3-row degenerate ROC plot IS a structurally non-empty ggplot; the real #81 RED lives in **RF4** (ROC numeric correctness in `test_gg_roc.R`), not the structural matrix. So the expected RED-here set is exactly **three**: `gg_error randomForest regression renders without unknown-label warning`, `plot.gg_variable randomForest default multi-xvar is a single object (#80)` (classification + regression). If any *already-OK* family fails, that is a **sibling defect** — STOP, report it for logging under #82 before continuing.

- [ ] **Step 3: Commit the failing matrix**

```bash
git add tests/testthat/test_plot_layer_data.R
git commit -m "test: randomForest plot matrix — #80/#81/wart fail red (#82)"
```

---

## Task 4: ROC numeric-correctness tests (RED) + rfsrc characterization (GREEN)

**Files:** Modify `tests/testthat/test_gg_roc.R` (append; match existing style — `expect_s3_class`, `calc_auc`).

- [ ] **Step 1: Append ROC correctness + rfsrc pin**

Append to `tests/testthat/test_gg_roc.R`:
```r
# ---------------------------------------------------------------------------
# randomForest ROC correctness (#81) + rfsrc no-change characterization
# ---------------------------------------------------------------------------

test_that("gg_roc randomForest: separable class AUC ~ 1 and many thresholds (#81)", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  setosa <- which(levels(iris$Species) == "setosa")
  g <- gg_roc(rf, which_outcome = setosa)
  expect_s3_class(g, "gg_roc")
  expect_gt(nrow(g), nlevels(iris$Species))      # not the degenerate ~3-row curve
  expect_gt(calc_auc(g), 0.98)                    # setosa is separable
})

test_that("gg_roc randomForest default is macro-average, many points, no warning (#81)", {
  set.seed(42)
  rf <- randomForest::randomForest(Species ~ ., data = iris)
  expect_no_warning(g <- gg_roc(rf))              # was: 'Must specify which_outcome'
  expect_s3_class(g, "gg_roc")
  expect_gt(nrow(g), nlevels(iris$Species))
  expect_gt(calc_auc(g), 0.5)
})

test_that("calc_roc.rfsrc output is unchanged for an explicit which_outcome (guard)", {
  set.seed(42)
  rfsrc_iris <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)
  g <- gg_roc(rfsrc_iris, which_outcome = 1)
  expect_s3_class(g, "gg_roc")
  expect_equal(ncol(g), 3L)                       # sens, spec, pct (existing contract)
  expect_true(all(c("sens", "spec", "pct") %in% colnames(g)))
  expect_gte(calc_auc(g), 0.9)                    # rfsrc iris setosa-vs-rest stays strong
})
```

- [ ] **Step 2: Run; confirm RED #81 / GREEN rfsrc guard**

Run:
```bash
R -q -e 'testthat::test_file("tests/testthat/test_gg_roc.R")' 2>&1 | tail -20
```
Expected: the two `gg_roc randomForest` tests FAIL (degenerate AUC≈0.5 / ~3 rows / warning); the `calc_roc.rfsrc ... unchanged` characterization test PASSES (it pins current correct rfsrc behavior). If the rfsrc characterization fails now, the fixture/assertion is wrong — fix the test, not the package.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_roc.R
git commit -m "test: RF ROC correctness fails red; rfsrc characterization green (#81/#82)"
```

---

## Task 5: Fix #80 — `plot.gg_variable` multi-`xvar` list → single `patchwork`

**Files:** Modify `R/plot.gg_variable.R` (`:604-609` return; `:29-31` `@return` roxygen). Modify `tests/testthat/test_gg_variable.R` (`:38-41` contract test; audit `:106`). Modify `NEWS.md`. Regenerate `man/plot.gg_variable.Rd`.

- [ ] **Step 1: systematic-debugging — confirm the exact return branch**

Run:
```bash
R -q -e 'suppressMessages(devtools::load_all(".")); set.seed(1); rf<-randomForest::randomForest(Species~.,data=iris); g<-gg_variable(rf); p<-plot(g); cat("class:", paste(class(p),collapse=","), " len:", length(p), "\n")'
```
Expected: `class: list ...` (the non-`panel`, `lng>1` branch at `R/plot.gg_variable.R:412-607` returns `gg_plt` (a `vector("list")`) because the collapse at `:604-607` only fires when `lng == 1`). This confirms the fix site.

- [ ] **Step 2: Implement the unification**

In `R/plot.gg_variable.R`, the current tail of the non-panel branch is:
```r
    # Return a single ggplot when only one variable was requested
    if (lng == 1) {
      gg_plt <- gg_plt[[1]]
    }
  }
  return(gg_plt)
}
```
Replace with (mirrors `R/plot.gg_partial.R:87-93` `wrap_plots` pattern; `@importFrom patchwork wrap_plots` already declared package-wide):
```r
    # Return a single object: one ggplot for a single variable, otherwise a
    # patchwork composite (one panel per variable). Never a bare list — see
    # #80 / NEWS; mirrors the v2.7.3 #77/#78 plot.gg_partial* unification.
    if (lng == 1) {
      gg_plt <- gg_plt[[1]]
    } else {
      gg_plt <- patchwork::wrap_plots(gg_plt, ncol = 1)
    }
  }
  return(gg_plt)
}
```
(The `panel = TRUE` branch already returns a single faceted `ggplot` — unchanged. Only the non-panel `lng > 1` path changes.)

- [ ] **Step 3: Rewrite the `@return` roxygen**

In `R/plot.gg_variable.R` replace lines `:29-31`:
```r
#' @return A single \code{ggplot} object when \code{length(xvar) == 1} or
#'   \code{panel = TRUE}. Otherwise a named list of \code{ggplot} objects, one
#'   per variable in \code{xvar}.
```
with:
```r
#' @return A single \code{ggplot} object when \code{length(xvar) == 1} or
#'   \code{panel = TRUE}; otherwise a \code{patchwork} composite stacking
#'   one panel per variable in \code{xvar}. Always a single plottable
#'   object (never a bare list) so it composes with \code{patchwork} and
#'   works with \code{ggplot2::layer_data()} / \code{autoplot()}.
```

- [ ] **Step 4: Update the contract test + audit other call sites**

In `tests/testthat/test_gg_variable.R`, the block at `:38-41` is currently:
```r
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names)
  expect_type(gg_plt, "list")
```
Replace the assertion with the new contract:
```r
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names)
  expect_true(inherits(gg_plt, "patchwork") || inherits(gg_plt, "ggplot"))
```
Then inspect `tests/testthat/test_gg_variable.R:106` (`plot.gg_variable(gg_dta)` default multi-xvar) and any nearby assertion: if it asserts `expect_type(..., "list")` or indexes `gg_plt[[i]]`, update it to treat the result as a single `patchwork`/`ggplot` (e.g. `expect_s3_class(gg_plt, "patchwork")`). Do NOT change `:32` (single xvar → ggplot, unchanged), `:46-47` (`panel = TRUE`, unchanged), or `:116` (`expect_warning(..., panel = TRUE)`, unchanged). Run:
```bash
R -q -e 'testthat::test_file("tests/testthat/test_gg_variable.R")' 2>&1 | tail -8
```
Expected: PASS (the rewritten contract + unchanged single/panel tests). If `:106` area still fails, it relied on the old list — fix that test's expectation only (never weaken a behavioral assertion).

- [ ] **Step 5: Regenerate docs, run the #80 matrix + full suite**

Run:
```bash
R -q -e 'devtools::document()'
git diff --stat DESCRIPTION
R -q -e 'testthat::test_file("tests/testthat/test_plot_layer_data.R")' 2>&1 | tail -6
R -q -e 'devtools::test()' 2>&1 | tail -3
```
Expected: `git diff --stat DESCRIPTION` empty (roxygen 8.0.0 pin holds; if `RoxygenNote`↔`Config/roxygen2/version` flips, STOP/BLOCKED). The two `gg_variable #80` matrix tests now PASS; full suite `FAIL 0`, PASS ≥ baseline (the `gg_error`/`gg_roc` RED tests may still be red — fixed in Tasks 6/7). No rfsrc single-xvar/panel regression.

- [ ] **Step 6: NEWS bullet + commit**

Add under the `v2.8.0 (development)` `#82` item in `NEWS.md`:
```
  - `plot.gg_variable()` now always returns a single `ggplot` (one
    variable) or a `patchwork` composite (multiple variables / default),
    never a bare list — consistent with the v2.7.3 `plot.gg_partial*`
    change. Previously a list was returned for multiple `xvar`, which
    broke `patchwork`/`autoplot()`/`layer_data()` composition (#80).
```
```bash
git add R/plot.gg_variable.R man/plot.gg_variable.Rd tests/testthat/test_gg_variable.R NEWS.md
git commit -m "fix: plot.gg_variable returns single ggplot/patchwork, not a list (#80)"
```

---

## Task 6: Fix #81 — `calc_roc.randomForest` indexing + `oob` + macro-average

**Files:** Modify `R/calc_roc.R` (`calc_roc.randomForest` `:154-196` ONLY — do **not** touch `.validate_which_outcome` `:16-22` or `calc_roc.rfsrc` `:83-141`). Modify `R/gg_roc.R` (`:134` signature, `:153-156` passthrough, `:26-33` Rd). Modify `NEWS.md`. Regenerate `man/`.

- [ ] **Step 1: Rewrite `calc_roc.randomForest`**

Replace the entire `calc_roc.randomForest` function (`R/calc_roc.R:154-196`) with:
```r
## randomForest ROC: OOB vote probabilities by default; macro-average for
## the overall ("all"/0) case. Does NOT use the shared
## .validate_which_outcome (rfsrc path unchanged). See #81.
#' @export
calc_roc.randomForest <-
  function(object,
           dta,
           which_outcome = "all",
           oob = TRUE,
           ...) {
    if (!is.factor(dta)) {
      dta <- factor(dta)
    }
    lvls <- levels(dta)

    # Probability matrix: OOB votes by default (honest), else in-bag predict.
    prob <- if (isTRUE(oob) && !is.null(object$votes)) {
      as.matrix(object$votes)
    } else {
      stats::predict(object, type = "prob")
    }
    # randomForest votes can be counts; normalise rows to probabilities.
    rs <- rowSums(prob)
    if (any(rs > 1 + 1e-8, na.rm = TRUE)) {
      prob <- prob / rs
    }
    colnames(prob) <- lvls

    one_class_roc <- function(k) {
      res <- dta == lvls[k]
      score <- prob[, k]
      pct <- sort(unique(score))
      last <- length(pct)
      if (last > 1) pct <- pct[-last]
      if (length(pct) > 200) {
        pct <- pct[seq(1, length(pct), length.out = 200)]
      }
      rc <- parallel::mclapply(pct, function(crit) {
        tbl <- xtabs(~ res + (score > crit))
        if (ncol(tbl) < 2) {
          tbl <- cbind(tbl, c(0, 0))
          colnames(tbl) <- c("FALSE", "TRUE")
        }
        spec <- tbl[2, 2] / rowSums(tbl)[2]
        sens <- tbl[1, 1] / rowSums(tbl)[1]
        cbind(sens = sens, spec = spec)
      })
      rc <- do.call(rbind, rc)
      rc <- rbind(c(0, 1), rc, c(1, 0))
      data.frame(rc, pct = c(0, pct, 1), row.names = seq_len(nrow(rc)))
    }

    if (identical(which_outcome, "all") || identical(which_outcome, 0)) {
      # Macro-average one-vs-rest: mean sens/spec on a shared FPR grid.
      curves <- lapply(seq_along(lvls), one_class_roc)
      grid <- seq(0, 1, length.out = 200)
      interp <- vapply(curves, function(cv) {
        cv <- cv[order(cv$spec, decreasing = TRUE), ]
        stats::approx(1 - cv$spec, cv$sens, xout = grid,
                      ties = "ordered", rule = 2)$y
      }, numeric(length(grid)))
      gg_dta <- data.frame(
        sens = rowMeans(interp, na.rm = TRUE),
        spec = 1 - grid,
        pct  = grid,
        row.names = seq_along(grid)
      )
    } else {
      gg_dta <- one_class_roc(which_outcome)
    }
    invisible(gg_dta)
  }
```
This keeps the returned columns (`sens`, `spec`, `pct`) and the anchored-curve shape compatible with `plot.gg_roc` and `calc_auc` (verify in Step 4). It never calls `.validate_which_outcome`.

- [ ] **Step 2: Fix `gg_roc.randomForest` `oob` plumbing + signature**

In `R/gg_roc.R`, change the signature `:134`:
```r
gg_roc.randomForest <- function(object, which_outcome, oob, ...) {
```
to:
```r
gg_roc.randomForest <- function(object, which_outcome, oob = TRUE, ...) {
```
and the `calc_roc()` call `:153-156`:
```r
  gg_dta <- # nolint: object_usage_linter
    calc_roc(object,
      object$y,
      which_outcome = which_outcome
    )
```
to pass `oob` through and default `which_outcome`:
```r
  if (missing(which_outcome)) which_outcome <- "all"
  gg_dta <- # nolint: object_usage_linter
    calc_roc(object,
      object$y,
      which_outcome = which_outcome,
      oob = oob
    )
```
(Leave the existing `missing(which_outcome) -> "all"` block at `:143-145` if present; ensure no double-assignment — if `:143-145` already sets it, do not re-add. Inspect and keep exactly one default assignment.)

- [ ] **Step 3: Update `gg_roc` Rd (macro-average + oob semantics)**

In `R/gg_roc.R` `@param which_outcome` / `@param oob` block (`:26-33`), append to the `which_outcome` description: ``Use \code{which_outcome = "all"} or \code{0} for the overall macro-averaged (one-vs-rest) ROC.`` and to `oob`: ``For \code{randomForest}, \code{oob = TRUE} uses out-of-bag vote probabilities (\code{object$votes}); \code{FALSE} uses in-bag \code{predict(type = "prob")}.``

- [ ] **Step 4: document(), run ROC + matrix + rfsrc guard**

Run:
```bash
R -q -e 'devtools::document()'
git diff --stat DESCRIPTION
R -q -e 'testthat::test_file("tests/testthat/test_gg_roc.R")' 2>&1 | tail -10
R -q -e 'testthat::test_file("tests/testthat/test_plot_layer_data.R")' 2>&1 | tail -6
git diff f3ffd38 -- R/calc_roc.R | grep -nE "^[+-]" | grep -E "validate_which_outcome|calc_roc.rfsrc" || echo "SHARED HELPERS UNTOUCHED"
```
Expected: `DESCRIPTION` diff empty; both `gg_roc randomForest` correctness tests PASS; the `calc_roc.rfsrc ... unchanged` characterization test PASS; the `gg_roc` matrix test PASS; **`SHARED HELPERS UNTOUCHED`** printed (the diff must not modify `.validate_which_outcome` or `calc_roc.rfsrc`). If shared helpers show in the diff, STOP — revert and re-scope the change into the randomForest path only.

- [ ] **Step 5: NEWS bullet + commit**

Add under the `#82` NEWS item:
```
  - `gg_roc()` / `calc_roc()` for `randomForest` now compute a correct
    ROC from class probabilities (OOB votes by default, honoring `oob`)
    instead of a degenerate ~3-point curve; `which_outcome` /
    `gg_roc(rf)` default returns a macro-averaged one-vs-rest ROC with
    no warning. rfsrc behavior is unchanged (#81).
```
```bash
git add R/calc_roc.R R/gg_roc.R man/ NEWS.md
git commit -m "fix: correct randomForest ROC (probabilities, oob, macro-average) (#81)"
```

---

## Task 7: Fix the `plot.gg_error` `color="Outcome"` wart

**Files:** Modify `R/plot.gg_error.R` (`:244,248`). Regenerate `man/` only if roxygen changes (none expected).

- [ ] **Step 1: Inspect the two `labs()` calls**

Run:
```bash
sed -n '230,255p' R/plot.gg_error.R
```
Identify which branch is the multi-outcome/classification path (a `colour`/`color` aesthetic is mapped) versus the regression/single-outcome path (no colour aesthetic). Both currently end with `ggplot2::labs(x = "Number of Trees", y = "OOB Error Rate", color = "Outcome")`.

- [ ] **Step 2: Make the colour label conditional**

For the `labs()` call on the **regression / single-outcome** branch (the one whose plot has no `colour`/`color` aesthetic mapped), remove the `color = "Outcome"` argument so it reads:
```r
      ggplot2::labs(x = "Number of Trees", y = "OOB Error Rate")
```
Leave the multi-outcome/classification branch's `labs(..., color = "Outcome")` unchanged (that plot maps a colour aesthetic, so the label is valid there). If a single shared `labs()` serves both branches, split it so the colour label is only added on the colour-mapped branch.

- [ ] **Step 3: Verify no unknown-label warning + commit**

Run:
```bash
R -q -e 'suppressMessages(devtools::load_all(".")); set.seed(1); rf<-randomForest::randomForest(mpg~.,data=mtcars); testthat::expect_no_warning(print(plot(gg_error(rf)))); cat("NO WARN\n")'
R -q -e 'testthat::test_file("tests/testthat/test_plot_layer_data.R")' 2>&1 | tail -4
```
Expected: `NO WARN`; the `gg_error randomForest regression renders without unknown-label warning` test now PASSES; classification `gg_error` test still PASSES.
```bash
git add R/plot.gg_error.R
git commit -m "fix: drop unconditional color='Outcome' label on gg_error regression plot (#82)"
```

---

## Task 8: vdiffr snapshots for the randomForest matrix

**Files:** Modify `tests/testthat/test_snapshots.R` (add an RF block inside the existing guarded section). Snapshots generated only after Tasks 5–7 (no broken-plot baselines).

- [ ] **Step 1: Add the RF snapshot block**

Inside `tests/testthat/test_snapshots.R`, within the existing guard `if (requireNamespace("vdiffr", quietly = TRUE) && identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {` (`:19-20`), add a `local({...})` block mirroring the existing rfsrc idiom, naming snapshots `"<fn> <type> rf"`:
```r
  local({
    set.seed(42)
    rf_iris <- randomForest::randomForest(Species ~ ., data = iris,
                                          importance = TRUE)
    test_that("snapshot: gg_rfsrc classification rf", {
      vdiffr::expect_doppelganger("gg_rfsrc classification rf",
                                  plot(gg_rfsrc(rf_iris)))
    })
    test_that("snapshot: gg_error classification rf", {
      vdiffr::expect_doppelganger("gg_error classification rf",
                                  plot(gg_error(rf_iris)))
    })
    test_that("snapshot: gg_vimp classification rf", {
      vdiffr::expect_doppelganger("gg_vimp classification rf",
                                  plot(gg_vimp(rf_iris)))
    })
    test_that("snapshot: gg_variable classification rf", {
      vdiffr::expect_doppelganger("gg_variable classification rf",
                                  plot(gg_variable(rf_iris)))
    })
    test_that("snapshot: gg_roc classification rf", {
      vdiffr::expect_doppelganger("gg_roc classification rf",
                                  plot(gg_roc(rf_iris, which_outcome = 1)))
    })
  })
  local({
    set.seed(42)
    rf_mt <- randomForest::randomForest(mpg ~ ., data = mtcars,
                                        importance = TRUE)
    test_that("snapshot: gg_rfsrc regression rf", {
      vdiffr::expect_doppelganger("gg_rfsrc regression rf",
                                  plot(gg_rfsrc(rf_mt)))
    })
    test_that("snapshot: gg_error regression rf", {
      vdiffr::expect_doppelganger("gg_error regression rf",
                                  plot(gg_error(rf_mt)))
    })
    test_that("snapshot: gg_vimp regression rf", {
      vdiffr::expect_doppelganger("gg_vimp regression rf",
                                  plot(gg_vimp(rf_mt)))
    })
    test_that("snapshot: gg_variable regression rf", {
      vdiffr::expect_doppelganger("gg_variable regression rf",
                                  plot(gg_variable(rf_mt)))
    })
  })
```
Place these blocks before the closing `}` of the guard. Match the file's existing indentation.

- [ ] **Step 2: Generate baselines (all plots now fixed)**

Run:
```bash
VDIFFR_RUN_TESTS=true R -q -e 'testthat::test_file("tests/testthat/test_snapshots.R")' 2>&1 | tail -8
ls tests/testthat/_snaps/snapshots/ | grep -i " rf" | head
```
Expected: new `*rf*.svg` baseline snapshots created under `tests/testthat/_snaps/`; tests pass (first run records baselines). Visually sanity-check one or two with `git diff --stat tests/testthat/_snaps/`.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_snapshots.R tests/testthat/_snaps
git commit -m "test: vdiffr snapshots for the randomForest plot matrix (#82)"
```

---

## Task 9: Rd `@note` — intentionally rfsrc-only families

**Files:** Modify `R/gg_partial.R`, `R/gg_survival.R`, `R/gg_brier.R` roxygen. Regenerate `man/`.

- [ ] **Step 1: Add `@note` to each**

In each file's main exported-function roxygen block (above `@export`), add:
- `R/gg_partial.R`:
```r
#' @note Partial-dependence extraction is `randomForestSRC`-only;
#'   there is no `randomForest` method (the `randomForest` package
#'   provides no comparable partial-dependence interface).
```
- `R/gg_survival.R`:
```r
#' @note Survival estimation is `randomForestSRC`-only; `randomForest`
#'   has no survival forest, so no `randomForest` method exists.
```
- `R/gg_brier.R`:
```r
#' @note Brier score / CRPS is `randomForestSRC` survival-only; there
#'   is no `randomForest` method.
```

- [ ] **Step 2: document() + verify + commit**

Run:
```bash
R -q -e 'devtools::document()'
git diff --stat DESCRIPTION
git status --porcelain man/ | head
```
Expected: `DESCRIPTION` diff empty; only the three `man/*.Rd` (gg_partial/gg_survival/gg_brier) changed (plus any from Tasks 5/6 already committed). 
```bash
git add R/gg_partial.R R/gg_survival.R R/gg_brier.R man/
git commit -m "docs: note gg_partial/gg_survival/gg_brier are randomForestSRC-only (#82)"
```

---

## Task 10: Final gate

**Files:** `NEWS.md` (finalize wording only if needed). Verification.

- [ ] **Step 1: Full as-CRAN check**

Run (5–10 min):
```bash
R -q -e 'devtools::check(document = TRUE, args = "--as-cran", error_on = "never")' 2>&1 | tee /tmp/ggrf-rf-final-check.log | tail -25
```
Expected: `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`, identical to the Task 2 baseline (`/tmp/ggrf-rf-baseline-check.log`). Any new NOTE/WARNING/ERROR must be resolved (no suppression).

- [ ] **Step 2: Test suite + invariants**

Run:
```bash
R -q -e 'devtools::test()' 2>&1 | tail -3
git diff f3ffd38 -- R/calc_roc.R | grep -E "validate_which_outcome|^.calc_roc.rfsrc" || echo "rfsrc/.validate_which_outcome BYTE-UNCHANGED"
```
Expected: `[ FAIL 0 | WARN 0 | SKIP 3 | PASS N ]` with `N` = 678 + the new RF matrix/ROC test count (strictly greater than 678). `rfsrc/.validate_which_outcome BYTE-UNCHANGED` printed. The Phase 0 namespace-hygiene guard remains (SKIP 3 includes it under `devtools::test()`; it runs+passes under the check in Step 1).

- [ ] **Step 3: Finalize NEWS + commit if needed**

Ensure the `#82` NEWS bullet + sub-items read coherently. If any wording fix was needed:
```bash
git add NEWS.md
git commit -m "docs: finalize v2.8.0 NEWS for randomForest validation (#82)"
```
(Skip the commit if NEWS already coherent from Tasks 5–6.)

- [ ] **Step 4: Hand off to finishing-a-development-branch**

Use superpowers:finishing-a-development-branch (verify tests → push branch → open PR referencing #82/#80/#81; do NOT merge — the user merges, per CLAUDE.md PR-only policy). The PR closes #80, #81 (and #82 when merged).

---

## Self-Review

**1. Spec coverage:**
- §4(a) structural matrix → Task 3 ✔
- §4(b) ROC correctness + rfsrc characterization → Task 4 ✔
- §4(c) vdiffr snapshots (literal `VDIFFR_RUN_TESTS` guard, `"... rf"` naming) → Task 8 ✔
- §5 #80 (list→single object + `@return` + contract test + NEWS, engine-agnostic, extractor untouched) → Task 5 ✔
- §5 #81 (matrix indexing, `oob` via `$votes`, plumbing, macro-average **only in RF path**, shared helpers byte-unchanged, Rd) → Task 6 ✔ (guard in Tasks 4 & 10)
- §5 minor wart at `plot.gg_error.R:244,248` → Task 7 ✔
- §5 Rd rfsrc-only `@note` → Task 9 ✔
- §3 versioning (next unused `2.7.3.900x` at execution) → Task 1 ✔
- §6 gates (fresh baseline, document() byte-stable DESCRIPTION, FAIL0/PASS>678, namespace-hygiene intact) → Tasks 2, 10 ✔
- §3 execution model / handoff → Task 10 Step 4 ✔

No spec gaps.

**2. Placeholder scan:** No "TBD"/"handle edge cases". Each code step shows complete code or an exact, source-anchored edit (verbatim current text → replacement). The one judgement step (Task 7 Step 2 "identify which branch maps colour") is bounded with an exact inspection command and a deterministic rule.

**3. Type consistency:** `gg_roc`/`calc_roc` return columns `sens, spec, pct` are preserved by the Task 6 rewrite (asserted in Task 4's rfsrc characterization and Task 6 Step 4). `inherits(p, "patchwork") || inherits(p, "ggplot")` used consistently for the #80 contract (Tasks 3, 5). `which_outcome`/`oob` names match `R/gg_roc.R` and `R/calc_roc.R` throughout. Snapshot naming `"<fn> <type> rf"` consistent (Task 8).
