# RHF Phase 2 (`gg_auct`) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `gg_auct()` — a tidy extractor + `plot.gg_auct()` + `print`/`summary`/`autoplot` companions for time-varying AUC from `randomForestRHF::auct.rhf()`.

**Architecture:** `gg_auct()` is an S3 generic; `gg_auct.rhf(object, marker, auct_fit = NULL)` takes a fitted `rhf` object and computes `auct.rhf()` internally (or reuses a precomputed `auct_fit`, mirroring `gg_beta_varpro(beta_fit=)`), returning a tidy `c("gg_auct","data.frame")` frame (`time, auc, se, lower, upper, marker`) with an `iauc` attribute. `plot.gg_auct()` draws AUC(t) with an optional CI ribbon. `randomForestRHF` stays a gated `Suggests:`.

**Tech Stack:** R, S3, ggplot2 (`.data[[ ]]`), testthat 3e, vdiffr (env-gated), roxygen2 markdown.

**Branch / workflow:** `feat/rhf-phase2-gg-auct` (off `dev`) → PR into `dev`. `dev` is PR-protected; required checks: `ubuntu-latest (release)`, `macos-latest (release)`, `windows-latest (release)`.

**Design note (refines umbrella spec):** the umbrella spec said `gg_auct` input = "auct.rhf object"; this plan refines it to `gg_auct.rhf(object, marker, auct_fit = NULL)` (compute-internally with optional cache) per the `gg_brier`/`beta_fit` idioms. Task 2.6 updates the spec's Phase 2 section to match.

**Grounded `auct.rhf` contract** (class `"auct.rhf"`, verified on the pbc fixture, runs <1s):
- `$AUC.by.time`: data.frame cols `time, AUC, n.cases, n.ctrl, G, W` (nrow = n_time).
- `$iAUC.uno`, `$iAUC.std`: scalars. `$marker`: normalized (`"chf"`→`"cumhaz"`, `"haz"`→`"hazard"`).
- `$boot` (only when `bootstrap.rep > 0`): `$AUC.se`, `$AUC.lower`, `$AUC.upper` (each per-time, length = nrow `AUC.by.time`), `$iAUC.uno.se`, `$iAUC.std.se`, `$conf.level`, `$rep`, `$mode`.

---

## File Structure

- Create `R/gg_auct.R` — `gg_auct()` generic + `gg_auct.rhf()`.
- Create `R/plot.gg_auct.R` — `plot.gg_auct()`.
- Modify `R/print_methods.R` — `print.gg_auct()`.
- Modify `R/summary_methods.R` — `summary.gg_auct()`.
- Modify `R/autoplot_methods.R` — `autoplot.gg_auct()`.
- Modify `tests/testthat/helper-rhf-fixtures.R` — memoised `auct.rhf` fixtures (boot + no-boot).
- Create `tests/testthat/test_gg_auct.R` — extractor + S3 tests.
- Create `tests/testthat/test_plot_gg_auct.R` — plot build tests.
- Modify `tests/testthat/test_snapshots.R` — gated `gg-auct` vdiffr snapshot.
- Modify `_pkgdown.yml` — add `gg_auct` + `plot.gg_auct` to the "Survival Analysis" reference section. **(Phase 1 lesson: omitting this fails pkgdown CI.)**
- Modify `DESCRIPTION` + `NEWS.md` — bump dev version to `3.0.0.9001`, add a Phase 2 NEWS bullet.

---

## Task 2.1: Dev-version tick + auct fixtures

**Files:** `DESCRIPTION`, `NEWS.md`, `tests/testthat/helper-rhf-fixtures.R`

- [ ] **Step 1: Bump version.** `DESCRIPTION` line 4 → `Version: 3.0.0.9001`. `NEWS.md` line 2 → `Version: 3.0.0.9001`.

- [ ] **Step 2: Add a NEWS bullet** under the existing `ggRandomForests v4.0.0 (development)` heading (as the first bullet after the heading line + underline):

```
* `gg_auct()` / `plot.gg_auct()`: tidy wrapper and plot for time-varying
  AUC from `randomForestRHF::auct.rhf()` (RHF Phase 2). Returns a long
  frame `time / auc / se / lower / upper / marker` with an `iauc`
  attribute (Uno + standardized integrated AUC); `plot.gg_auct()` draws
  AUC(t) with a bootstrap CI ribbon when available and a 0.5 reference
  line. `gg_auct.rhf(object, marker, auct_fit = NULL)` computes
  `auct.rhf()` internally or reuses a cached fit.
```

- [ ] **Step 3: Add memoised auct fixtures** to `tests/testthat/helper-rhf-fixtures.R` (append; the `.rhf_cache` env already exists):

```r
# auct.rhf on the pbc fit — with bootstrap (CI ribbon) and without (NA CI).
.auct_pbc_boot <- function() {
  if (is.null(.rhf_cache$auct_boot)) {
    o <- .rhf_pbc()
    set.seed(20260529L)
    .rhf_cache$auct_boot <- randomForestRHF::auct.rhf(
      o, marker = "chf", bootstrap.rep = 20L
    )
  }
  .rhf_cache$auct_boot
}

.auct_pbc_noboot <- function() {
  if (is.null(.rhf_cache$auct_noboot)) {
    o <- .rhf_pbc()
    .rhf_cache$auct_noboot <- randomForestRHF::auct.rhf(o, marker = "chf")
  }
  .rhf_cache$auct_noboot
}
```

- [ ] **Step 4: Verify** the version test + fixtures load.

Run: `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_ggrandomforests_news.R"); source("tests/testthat/helper-rhf-fixtures.R"); a<-.auct_pbc_boot(); cat("auct class:", inherits(a,"auct.rhf"), "| boot:", !is.null(a$boot), "\n")'`
Expected: news test passes; `auct class: TRUE | boot: TRUE`.

- [ ] **Step 5: Commit.**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && git add DESCRIPTION NEWS.md tests/testthat/helper-rhf-fixtures.R && git commit -m "chore: tick to 3.0.0.9001 + auct.rhf test fixtures (RHF Phase 2)"
```

---

## Task 2.2: `gg_auct()` generic + `gg_auct.rhf()` extractor

**Files:** Create `R/gg_auct.R`; Test `tests/testthat/test_gg_auct.R`

- [ ] **Step 1: Write failing tests.** Create `tests/testthat/test_gg_auct.R`:

```r
test_that("gg_auct.rhf returns a tidy AUC(t) frame (no bootstrap -> NA CI)", {
  gg <- gg_auct(.rhf_pbc(), marker = "chf", auct_fit = .auct_pbc_noboot())
  expect_s3_class(gg, "gg_auct")
  expect_true(all(c("time", "auc", "se", "lower", "upper", "marker") %in% names(gg)))
  a <- .auct_pbc_noboot()
  expect_equal(nrow(gg), nrow(a$AUC.by.time))
  expect_equal(gg$auc, a$AUC.by.time$AUC)
  expect_true(all(is.na(gg$lower)))             # no bootstrap -> NA CI
  expect_equal(attr(gg, "iauc")$uno, a$iAUC.uno)
})

test_that("gg_auct.rhf carries bootstrap CI when present", {
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_boot())
  expect_false(any(is.na(gg$lower)))
  expect_true(all(gg$upper >= gg$lower))
  expect_true(is.finite(attr(gg, "iauc")$uno.se))
})

test_that("gg_auct rejects non-rhf input and bad auct_fit", {
  expect_error(gg_auct(lm(mpg ~ wt, mtcars)), "rhf")
  expect_error(gg_auct(.rhf_pbc(), auct_fit = list(1)), "auct.rhf")
})
```

- [ ] **Step 2: Run red.** `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_gg_auct.R")'` → FAIL (`could not find function "gg_auct"`).

- [ ] **Step 3: Create `R/gg_auct.R`:**

```r
##=============================================================================
#' Tidy time-varying AUC from a Random Hazard Forest
#'
#' Extracts the time-dependent AUC curve from [randomForestRHF::auct.rhf()]
#' into a tidy long data frame, one row per time point, with bootstrap
#' confidence bounds when available and the integrated AUC (iAUC) summary
#' attached as an attribute.
#'
#' @param object A fitted `rhf` object from \pkg{randomForestRHF}.
#' @param marker Risk marker for the AUC: `"chf"` (cumulative hazard, default)
#'   or `"haz"` (hazard). Ignored when `auct_fit` is supplied.
#' @param auct_fit Optional precomputed [randomForestRHF::auct.rhf()] result
#'   (class `"auct.rhf"`) for the same `object`. `NULL` (default) computes it.
#'   Supply it to reuse an expensive bootstrap run.
#' @param ... Not currently used.
#'
#' @return A `data.frame` of class `c("gg_auct", "data.frame")` with columns
#'   `time`, `auc`, `se`, `lower`, `upper`, `marker` (CI columns `NA` when no
#'   bootstrap), plus an `iauc` attribute: a list with `uno`, `std`, `uno.se`,
#'   `std.se`, `conf.level`.
#'
#' @seealso [plot.gg_auct()], [randomForestRHF::auct.rhf()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   plot(gg_auct(o, marker = "chf"))
#' }
#' }
#'
#' @export
gg_auct <- function(object, ...) {
  UseMethod("gg_auct", object)
}

#' @rdname gg_auct
#' @export
gg_auct.rhf <- function(object, marker = c("chf", "haz"), auct_fit = NULL, ...) {
  if (!inherits(object, "rhf")) {
    stop("gg_auct() only works on 'rhf' objects from randomForestRHF.",
         call. = FALSE)
  }
  marker <- match.arg(marker)

  if (is.null(auct_fit)) {
    if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
      stop("Install the 'randomForestRHF' package to use gg_auct(): ",
           "install.packages('randomForestRHF')", call. = FALSE)
    }
    auct_fit <- randomForestRHF::auct.rhf(object, marker = marker)
  }
  if (!inherits(auct_fit, "auct.rhf")) {
    stop("auct_fit must be an 'auct.rhf' object from ",
         "randomForestRHF::auct.rhf().", call. = FALSE)
  }

  abt  <- auct_fit$AUC.by.time
  boot <- auct_fit$boot

  gg_dta <- data.frame(
    time   = abt$time,
    auc    = abt$AUC,
    se     = if (!is.null(boot)) boot$AUC.se    else NA_real_,
    lower  = if (!is.null(boot)) boot$AUC.lower else NA_real_,
    upper  = if (!is.null(boot)) boot$AUC.upper else NA_real_,
    marker = auct_fit$marker,
    stringsAsFactors = FALSE
  )

  attr(gg_dta, "iauc") <- list(
    uno        = auct_fit$iAUC.uno,
    std        = auct_fit$iAUC.std,
    uno.se     = if (!is.null(boot)) boot$iAUC.uno.se else NA_real_,
    std.se     = if (!is.null(boot)) boot$iAUC.std.se else NA_real_,
    conf.level = if (!is.null(boot)) boot$conf.level  else NA_real_
  )
  class(gg_dta) <- c("gg_auct", class(gg_dta))
  invisible(gg_dta)
}
```

- [ ] **Step 4: Register + run green.** `Rscript -e 'devtools::document()'` then re-run the test_file → all PASS.

- [ ] **Step 5: Commit** (source + test + generated docs):

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && git add R/gg_auct.R tests/testthat/test_gg_auct.R NAMESPACE man/gg_auct.Rd && git commit -m "feat: gg_auct() extractor — tidy time-varying AUC from auct.rhf"
```
Stage ONLY those; do NOT stage unrelated deleted snapshot SVGs / CRAN-SUBMISSION.

---

## Task 2.3: `plot.gg_auct()`

**Files:** Create `R/plot.gg_auct.R`; Test `tests/testthat/test_plot_gg_auct.R`

- [ ] **Step 1: Write failing tests.** Create `tests/testthat/test_plot_gg_auct.R`:

```r
test_that("plot.gg_auct builds an AUC(t) ggplot with a 0.5 reference", {
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_noboot())
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
  expect_gt(nrow(ggplot2::layer_data(p)), 0)
  expect_equal(p$labels$y, "AUC(t)")
})

test_that("plot.gg_auct adds a ribbon when bootstrap CI is present", {
  gg_b <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_boot())
  p_b  <- plot(gg_b)
  geoms_b <- vapply(p_b$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% geoms_b)

  gg_n <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_noboot())
  p_n  <- plot(gg_n)
  geoms_n <- vapply(p_n$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomRibbon" %in% geoms_n)   # no CI -> no ribbon
})

test_that("plot.gg_auct rejects non-gg_auct input", {
  expect_error(plot.gg_auct(mtcars), "gg_auct")
})
```

- [ ] **Step 2: Run red.** `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_plot_gg_auct.R")'` → FAIL (no `plot.gg_auct`).

- [ ] **Step 3: Create `R/plot.gg_auct.R`:**

```r
##=============================================================================
#' Plot a time-varying AUC curve
#'
#' Draws AUC(t) from a [gg_auct()] object: a line over time, a bootstrap
#' confidence ribbon when available, and a dashed reference line at 0.5
#' (chance). The integrated AUC (iAUC) appears in the caption.
#'
#' @param x A `gg_auct` object from [gg_auct()].
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_auct()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   plot(gg_auct(o))
#' }
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon geom_hline labs theme_bw
#' @name plot.gg_auct
#' @export
plot.gg_auct <- function(x, ...) {
  if (!inherits(x, "gg_auct")) {
    stop("plot.gg_auct() requires a 'gg_auct' object.", call. = FALSE)
  }
  iauc <- attr(x, "iauc")
  caption <- if (!is.null(iauc) && is.finite(iauc$uno)) {
    sprintf("iAUC (Uno) = %.3f  |  iAUC (standardized) = %.3f",
            iauc$uno, iauc$std)
  } else {
    NULL
  }

  p <- ggplot2::ggplot(x, ggplot2::aes(x = .data[["time"]], y = .data[["auc"]]))
  if (any(is.finite(x$lower))) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
      alpha = 0.2
    )
  }
  p +
    ggplot2::geom_line() +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed",
                        colour = "grey50") +
    ggplot2::labs(x = "Time", y = "AUC(t)",
                  title = sprintf("Time-varying AUC (%s)", x$marker[1]),
                  caption = caption) +
    ggplot2::theme_bw()
}
```

- [ ] **Step 4: Register + run green.** `Rscript -e 'devtools::document()'` then re-run the test_file → all PASS.

- [ ] **Step 5: Commit:**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && git add R/plot.gg_auct.R tests/testthat/test_plot_gg_auct.R NAMESPACE man/plot.gg_auct.Rd && git commit -m "feat: plot.gg_auct() — AUC(t) with bootstrap CI ribbon"
```

---

## Task 2.4: `print` / `summary` / `autoplot` companions

**Files:** `R/print_methods.R`, `R/summary_methods.R`, `R/autoplot_methods.R`; Test `tests/testthat/test_gg_auct.R`

- [ ] **Step 1: Append failing tests** to `tests/testthat/test_gg_auct.R`:

```r
test_that("gg_auct S3 companions work", {
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_boot())
  expect_output(print(gg), "gg_auct")
  expect_invisible(print(gg))
  s <- summary(gg)
  expect_true(is.data.frame(s))
  expect_true(all(c("iAUC.uno", "iAUC.std") %in% names(s)))
  expect_s3_class(autoplot(gg), "ggplot")
})
```

- [ ] **Step 2: Run red.** `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_gg_auct.R")'` → FAIL.

- [ ] **Step 3a: Add `print.gg_auct()` to `R/print_methods.R`** (after `print.gg_rhf`):

```r
#' @rdname print.gg
#' @export
print.gg_auct <- function(x, ...) {
  iauc <- attr(x, "iauc")
  cat(.gg_header(x, "gg_auct"),
      sprintf("  |  times: %d  marker: %s  iAUC(Uno): %.3f",
              nrow(x), x$marker[1],
              if (!is.null(iauc)) iauc$uno else NA_real_),
      "\n", sep = "")
  invisible(x)
}
```

- [ ] **Step 3b: Add `summary.gg_auct()` to `R/summary_methods.R`** (`@rdname summary.gg`):

```r
#' @rdname summary.gg
#' @export
summary.gg_auct <- function(object, ...) {
  iauc <- attr(object, "iauc")
  data.frame(
    iAUC.uno   = iauc$uno,
    iAUC.std   = iauc$std,
    iAUC.uno.se = iauc$uno.se,
    iAUC.std.se = iauc$std.se,
    conf.level = iauc$conf.level
  )
}
```

- [ ] **Step 3c: Add `autoplot.gg_auct()` to `R/autoplot_methods.R`** (`@rdname autoplot.gg`):

```r
#' @rdname autoplot.gg
#' @export
autoplot.gg_auct <- function(object, ...) {
  plot(object, ...)
}
```

- [ ] **Step 4: Register + run green.** `Rscript -e 'devtools::document()'` then re-run the test_file → all PASS.

- [ ] **Step 5: Commit:**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && git add R/print_methods.R R/summary_methods.R R/autoplot_methods.R NAMESPACE man/ tests/testthat/test_gg_auct.R && git commit -m "feat: print/summary/autoplot S3 companions for gg_auct"
```
Verify `git show --stat HEAD` shows NO snapshot SVGs.

---

## Task 2.5: pkgdown reference index + vdiffr snapshot

**Files:** `_pkgdown.yml`, `tests/testthat/test_snapshots.R`

- [ ] **Step 1: Add the topics to `_pkgdown.yml`.** In the `- title: "Survival Analysis"` section's `contents:` list, after the `- plot.gg_rhf` line, add:

```
      - gg_auct
      - plot.gg_auct
```

- [ ] **Step 2: Verify the pkgdown index is complete** (no missing topics):

Run: `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && Rscript -e 'pkgdown::check_pkgdown()'`
Expected: no error (no "topics missing from index").

- [ ] **Step 3: Add a gated vdiffr snapshot** to `tests/testthat/test_snapshots.R` (append; mirror the existing gating idiom):

```r
test_that("gg-auct-chf", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("randomForestRHF")
  skip_on_cran()
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_boot())
  vdiffr::expect_doppelganger("gg-auct-chf", plot(gg))
})
```

- [ ] **Step 4: Record the snapshot.**

Run: `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_snapshots.R", reporter="summary")'`
Expected: `gg-auct-chf` reports "Adding new file snapshot".

- [ ] **Step 5: VERIFY snapshot git state** (snapshot-prune guard):

Run: `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && git status -s tests/testthat/_snaps/`
Expected: only `gg-auct-chf.svg` untracked, NO deletions. If any `_snaps/` files show DELETED, run `git checkout -- tests/testthat/_snaps/` and re-record. If other untracked collateral `gg-*` SVGs appear, do NOT stage them.

- [ ] **Step 6: Commit:**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && git add _pkgdown.yml tests/testthat/test_snapshots.R tests/testthat/_snaps/snapshots/gg-auct-chf.svg && git commit -m "docs/test: pkgdown index + vdiffr snapshot for gg_auct"
```

---

## Task 2.6: Sync umbrella spec + full gate + PR

**Files:** `dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md`; then verification + PR.

- [ ] **Step 1: Update the umbrella spec's Phase 2 component.** In `dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md`, in the `### \`gg_auct\` (Phase 2)` block, change the `**Input:**` line from `\`auct.rhf\` object.` to:

```
- **Input:** fitted `rhf` object; `gg_auct.rhf(object, marker, auct_fit = NULL)` computes `auct.rhf()` internally or reuses a cached `auct_fit` (the `gg_beta_varpro(beta_fit=)` idiom).
```

- [ ] **Step 2: Commit the spec sync.**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && git add dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md && git commit -m "docs: sync umbrella spec Phase 2 input to gg_auct.rhf(auct_fit=)"
```

- [ ] **Step 3: Run the targeted suite.**

Run: `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && NOT_CRAN=true Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_dir("tests/testthat", filter="(gg_auct|plot_gg_auct|ggrandomforests_news)")'`
Expected: 0 failures.

- [ ] **Step 4: `R CMD check --as-cran` WITH the manual build** (standing release-gate rule).

Run: `cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && _R_CHECK_FORCE_SUGGESTS_=false Rscript -e 'rcmdcheck::rcmdcheck(args=c("--as-cran"), error_on="never")'`
Expected: 0 errors, 0 warnings; at most the dev-version NOTE. (Remove any stray `tests/testthat/Rplots.pdf` before building — it is a local artifact, not part of the package.)

- [ ] **Step 5: Push + open PR into `dev`.**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests && git push -u origin feat/rhf-phase2-gg-auct
gh pr create --base dev --title "RHF Phase 2: gg_auct time-varying AUC (v4.0.0)" --body "Phase 2 of the v4.0.0 RHF integration: gg_auct extractor + plot + S3 companions wrapping randomForestRHF::auct.rhf. Per dev/plans/2026-05-29-rhf-phase2-gg-auct-plan.md. R CMD check --as-cran: 0/0/<dev-version NOTE>."
```

- [ ] **Step 6: Address Copilot review, resolve threads, get CI green (required: ubuntu/macos/windows-latest (release)), merge into `dev`.**

---

## Self-Review

**Spec coverage:** `gg_auct.rhf(object, marker, auct_fit=)` compute-or-cache → Task 2.2 ✓. Tidy frame `time/auc/se/lower/upper/marker` + `iauc` attr → Task 2.2 ✓. Bootstrap CI present/absent paths → Tasks 2.2 (tests both) + 2.3 (ribbon conditional) ✓. `plot.gg_auct` line + ribbon + 0.5 ref + iAUC caption → Task 2.3 ✓. print/summary/autoplot → Task 2.4 ✓. vdiffr snapshot (gated) → Task 2.5 ✓. pkgdown index (Phase 1 lesson) → Task 2.5 ✓. Version + NEWS + `--as-cran` w/ manual → Tasks 2.1, 2.6 ✓. Umbrella-spec sync → Task 2.6 ✓.

**Placeholder scan:** none — every code step shows complete code; commands have expected output.

**Type consistency:** `gg_auct` columns `time/auc/se/lower/upper/marker` used identically across extractor (2.2), plot (2.3 reads `time/auc/lower/upper/marker`), and snapshot (2.5). The `iauc` attribute list keys `uno/std/uno.se/std.se/conf.level` are set in 2.2 and read in 2.3 (plot caption), 2.4 (print: `uno`; summary: all five). `marker` arg values `"chf"/"haz"` consistent; the stored `marker` column is `auct_fit$marker` (normalized `"cumhaz"/"hazard"`), used only as a display label — consistent everywhere it's read.
