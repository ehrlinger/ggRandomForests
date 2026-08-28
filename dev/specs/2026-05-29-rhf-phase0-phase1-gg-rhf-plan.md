# RHF Phase 0 + Phase 1 (`gg_rhf`) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Land Phase 0 (dev-cycle prerequisites + `randomForestRHF` as a gated Suggests) and Phase 1 (`gg_rhf` extractor + `plot.gg_rhf` + `print`/`summary`/`autoplot` S3 companions) for the v4.0.0 RHF integration.

**Architecture:** ggRandomForests stays a visualization layer. `gg_rhf()` is an S3 generic with a `.rhf` method that turns a fitted `randomForestRHF::rhf` object into a tidy long data frame (`id, time, hazard, chf, source`) of class `c("gg_rhf","data.frame")`, carrying a provenance attribute. `plot.gg_rhf()` renders per-case hazard / cumulative-hazard curves. `randomForestRHF` (GPL ≥3) is a gated `Suggests:` — every entry point checks `requireNamespace("randomForestRHF")`; ggRF source stays MIT.

**Tech Stack:** R, S3, ggplot2 (`.data[[ ]]` aesthetics), testthat 3e, vdiffr (env-gated), roxygen2 markdown. Reference spec: `dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md`.

**Branch / workflow:** all work on `feat/rhf-phase1-gg-rhf` (branched off `dev`) → PR into `dev`. `dev` is protected (PR-only; required checks `R-CMD-check` + ubuntu/macos/windows release).

**RHF object reference** (`class c("rhf","grow",family)`, from `rhf.workhorse.R`): `time.interest` (numeric grid), `hazard.oob` / `hazard.inbag` / `chf.oob` / `chf.inbag` (each an `n_case × n_time` matrix; the `.oob` variants are `NULL` when `bootstrap = "none"`), `ensemble.id` (per-case ids), `id`, `n`, `ntree`, `family`, `xvar.names`.

---

## File Structure

- Create `R/gg_rhf.R` — `gg_rhf()` generic + `gg_rhf.rhf()` extractor.
- Create `R/plot.gg_rhf.R` — `plot.gg_rhf()`.
- Modify `R/print_helpers.R` — add an `rhf` branch to `.gg_provenance()`.
- Modify `R/print_methods.R` — add `print.gg_rhf()`.
- Modify `R/summary_methods.R` — add `summary.gg_rhf()`.
- Modify `R/autoplot_methods.R` — add `autoplot.gg_rhf()`.
- Modify `DESCRIPTION` — add `randomForestRHF` to `Suggests:`; bump `Version`/`Date`.
- Modify `NEWS.md` — open the v4.0.0 dev line; add Phase 0/1 bullets.
- Create `tests/testthat/helper-rhf-fixtures.R` — session-memoised `rhf` fixtures.
- Create `tests/testthat/test_gg_rhf.R` — extractor + S3 tests.
- Create `tests/testthat/test_plot_gg_rhf.R` — plot build tests.
- Modify `tests/testthat/test_snapshots.R` — vdiffr snapshots for `gg_rhf`.
- Modify `dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md` — flip `Status` to `Approved`.

---

## Phase 0 — Dev cycle prerequisites

### Task 0.1: Open the v4.0.0 dev line + add gated Suggests

**Files:**
- Modify: `DESCRIPTION` (Version line 4, Date line 5, Suggests block)
- Modify: `NEWS.md` (Version line 2 + new heading)
- Modify: `dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md` (Status line)

- [ ] **Step 1: Add `randomForestRHF` to `Suggests:` in `DESCRIPTION`.** Append it to the existing Suggests list (after `callr`):

```
    ggraph,
    callr,
    randomForestRHF
```

- [ ] **Step 2: Bump dev version in `DESCRIPTION`.** Lines 4–5 become:

```
Version: 3.0.0.9000
Date: 2026-05-29
```

- [ ] **Step 3: Update `NEWS.md`.** Set line 2 to `Version: 3.0.0.9000` and insert a new top section above the `ggRandomForests v3.0.0` heading:

```
ggRandomForests v4.0.0 (development)
====================================
* Begin the v4.0.0 development line: a Random Hazard Forests (RHF)
  visualization layer wrapping the 'randomForestRHF' package (added to
  Suggests). RHF support is gated — every gg_rhf* entry point checks
  `requireNamespace("randomForestRHF")`. No change for users who do not
  install it.
```

- [ ] **Step 4: Flip the spec Status.** In `dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md`, change line 3 from `**Status:** Approved (brainstorm) — 2026-05-29` to `**Status:** Approved — 2026-05-29`.

- [ ] **Step 5: Verify the NEWS-version test passes.** The repo has a test that greps NEWS for the DESCRIPTION version.

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_ggrandomforests_news.R")'`
Expected: all PASS (NEWS line 2 `3.0.0.9000` matches DESCRIPTION).

- [ ] **Step 6: Commit.**

```bash
git add DESCRIPTION NEWS.md dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md
git commit -m "chore: open v4.0.0 dev line + randomForestRHF gated Suggests"
```

---

### Task 0.2: Session-memoised `rhf` test fixtures

`rhf()` fits are slow; build once per session and reuse. Mirrors `helper-varpro-fixtures.R`.

**Files:**
- Create: `tests/testthat/helper-rhf-fixtures.R`

- [ ] **Step 1: Write the fixture helper.**

```r
# Session-memoised randomForestRHF fixtures. rhf() fits are slow; compute
# once per R session and reuse. In-memory only — no disk cache.

.rhf_cache <- new.env(parent = emptyenv())

# Static-covariate pbc fit in counting-process form.
.rhf_pbc <- function() {
  if (is.null(.rhf_cache$pbc)) {
    if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
      testthat::skip("randomForestRHF not installed")
    }
    if (!requireNamespace("randomForestSRC", quietly = TRUE)) {
      testthat::skip("randomForestSRC not installed")
    }
    data(pbc, package = "randomForestSRC")
    d <- randomForestRHF::convert.counting(
      survival::Surv(days, status) ~ ., stats::na.omit(pbc)
    )
    .rhf_cache$pbc <- randomForestRHF::rhf(
      "Surv(id, start, stop, event) ~ .", d, ntree = 30, seed = -1L
    )
  }
  .rhf_cache$pbc
}
```

- [ ] **Step 2: Confirm the fixture loads and returns an `rhf` object.**

Run: `Rscript -e 'suppressMessages(devtools::load_all(quiet=TRUE)); source("tests/testthat/helper-rhf-fixtures.R"); o <- .rhf_pbc(); cat(inherits(o,"rhf"), !is.null(o$hazard.oob), !is.null(o$time.interest), "\n")'`
Expected: `TRUE TRUE TRUE` (skips cleanly to a message if randomForestRHF is absent).

- [ ] **Step 3: Commit.**

```bash
git add tests/testthat/helper-rhf-fixtures.R
git commit -m "test: session-memoised randomForestRHF fixtures"
```

---

## Phase 1 — `gg_rhf` extractor + plot + S3 companions

### Task 1.1: Provenance for `rhf` objects

`.gg_provenance()` only recognizes `rfsrc` and `randomForest`. Add an `rhf` branch so `print.gg_rhf()` shows a real header.

**Files:**
- Modify: `R/print_helpers.R` (inside `.gg_provenance`)
- Test: `tests/testthat/test_gg_rhf.R`

- [ ] **Step 1: Write the failing test.** Create `tests/testthat/test_gg_rhf.R`:

```r
test_that(".gg_provenance recognises rhf objects", {
  o <- .rhf_pbc()
  prov <- ggRandomForests:::.gg_provenance(o)
  expect_equal(prov$source, "randomForestRHF")
  expect_equal(prov$ntree, o$ntree)
  expect_equal(prov$n, o$n)
})
```

- [ ] **Step 2: Run it to confirm it fails.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_gg_rhf.R")'`
Expected: FAIL — `prov$source` is `NULL` (rhf falls through to the function's final `return(NULL)`).

- [ ] **Step 3: Add the `rhf` branch.** In `R/print_helpers.R`, immediately after the `randomForest` block inside `.gg_provenance()`, insert:

```r
  if (inherits(object, "rhf")) {
    return(list(
      source     = "randomForestRHF",
      family     = object$family %||% NA_character_,
      ntree      = object$ntree %||% NA_integer_,
      n          = object$n %||% NA_integer_,
      xvar.names = object$xvar.names %||% character(0)
    ))
  }
```

- [ ] **Step 4: Run the test to confirm it passes.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_gg_rhf.R")'`
Expected: PASS.

- [ ] **Step 5: Commit.**

```bash
git add R/print_helpers.R tests/testthat/test_gg_rhf.R
git commit -m "feat: .gg_provenance recognises randomForestRHF rhf objects"
```

---

### Task 1.2: `gg_rhf()` generic + `gg_rhf.rhf()` extractor

**Files:**
- Create: `R/gg_rhf.R`
- Test: `tests/testthat/test_gg_rhf.R`

- [ ] **Step 1: Write the failing tests.** Append to `tests/testthat/test_gg_rhf.R`:

```r
test_that("gg_rhf.rhf returns a tidy long frame over time.interest", {
  o  <- .rhf_pbc()
  gg <- gg_rhf(o)
  expect_s3_class(gg, "gg_rhf")
  expect_true(all(c("id", "time", "hazard", "chf", "source") %in% names(gg)))
  n_case <- nrow(o$hazard.oob)
  n_time <- length(o$time.interest)
  expect_equal(nrow(gg), n_case * n_time)
  expect_setequal(unique(gg$id), o$ensemble.id)
  expect_setequal(unique(gg$time), o$time.interest)
  expect_equal(unique(gg$source), "oob")
})

test_that("gg_rhf source='inbag' selects the inbag matrices", {
  o  <- .rhf_pbc()
  gg <- gg_rhf(o, source = "inbag")
  expect_equal(unique(gg$source), "inbag")
  expect_equal(nrow(gg), nrow(o$hazard.inbag) * length(o$time.interest))
})

test_that("gg_rhf rejects non-rhf input", {
  expect_error(gg_rhf(lm(mpg ~ wt, mtcars)), "rhf")
})
```

- [ ] **Step 2: Run to confirm failure.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_gg_rhf.R")'`
Expected: FAIL — `could not find function "gg_rhf"`.

- [ ] **Step 3: Implement `R/gg_rhf.R`.**

```r
##=============================================================================
#' Tidy hazard and cumulative-hazard curves from a Random Hazard Forest
#'
#' Extracts case-specific ensemble hazard and cumulative-hazard estimates from
#' a fitted [randomForestRHF::rhf()] object into a tidy long data frame, one
#' row per (case, time) pair on the forest's `time.interest` grid.
#'
#' @param object A fitted `rhf` object from \pkg{randomForestRHF}.
#' @param source Which ensemble estimate to extract: `"oob"` (default,
#'   out-of-bag) or `"inbag"`. Falls back to the other when the requested one
#'   is absent (e.g. `bootstrap = "none"` has no OOB estimate).
#' @param ... Not currently used.
#'
#' @return A `data.frame` of class `c("gg_rhf", "data.frame")` with columns
#'   `id`, `time`, `hazard`, `chf`, `source`, and a `provenance` attribute.
#'
#' @seealso [plot.gg_rhf()], [randomForestRHF::rhf()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   gg <- gg_rhf(o)
#'   plot(gg, idx = c(1, 5, 10))
#' }
#' }
#'
#' @export
gg_rhf <- function(object, ...) {
  UseMethod("gg_rhf", object)
}

#' @rdname gg_rhf
#' @export
gg_rhf.rhf <- function(object, source = c("oob", "inbag"), ...) {
  if (!inherits(object, "rhf")) {
    stop("gg_rhf() only works on 'rhf' objects from randomForestRHF.",
         call. = FALSE)
  }
  source <- match.arg(source)

  haz <- object[[paste0("hazard.", source)]]
  chf <- object[[paste0("chf.", source)]]
  if (is.null(haz)) {
    alt <- setdiff(c("oob", "inbag"), source)
    haz <- object[[paste0("hazard.", alt)]]
    chf <- object[[paste0("chf.", alt)]]
    if (is.null(haz)) {
      stop("rhf object carries no hazard estimates for source = '", source,
           "'.", call. = FALSE)
    }
    source <- alt
  }

  time   <- object$time.interest
  ids    <- object$ensemble.id
  n_case <- nrow(haz)
  n_time <- length(time)

  # haz / chf are n_case x n_time; as.vector() reads column-major (time-major),
  # so id repeats within each time block and time repeats across cases.
  gg_dta <- data.frame(
    id     = rep(ids, times = n_time),
    time   = rep(time, each = n_case),
    hazard = as.vector(haz),
    chf    = as.vector(chf),
    source = source,
    stringsAsFactors = FALSE
  )

  attr(gg_dta, "ntime") <- n_time
  class(gg_dta) <- c("gg_rhf", class(gg_dta))
  gg_dta <- .set_provenance(gg_dta, object)
  invisible(gg_dta)
}
```

- [ ] **Step 4: Run to confirm pass.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_gg_rhf.R")'`
Expected: PASS (all extractor tests).

- [ ] **Step 5: Commit.**

```bash
git add R/gg_rhf.R tests/testthat/test_gg_rhf.R
git commit -m "feat: gg_rhf() extractor — tidy hazard/CHF curves from rhf fits"
```

---

### Task 1.3: `plot.gg_rhf()`

**Files:**
- Create: `R/plot.gg_rhf.R`
- Test: `tests/testthat/test_plot_gg_rhf.R`

- [ ] **Step 1: Write the failing tests.** Create `tests/testthat/test_plot_gg_rhf.R`:

```r
test_that("plot.gg_rhf builds a ggplot of per-case hazard curves", {
  gg <- gg_rhf(.rhf_pbc())
  p  <- plot(gg, idx = c(1, 5, 10))
  expect_s3_class(p, "ggplot")
  ld <- ggplot2::layer_data(p)            # forces a real build
  expect_gt(nrow(ld), 0)
  expect_equal(p$labels$y, "Hazard")
})

test_that("plot.gg_rhf hazard.only = FALSE plots cumulative hazard", {
  gg <- gg_rhf(.rhf_pbc())
  p  <- plot(gg, idx = 1, hazard.only = FALSE)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$y, "Cumulative hazard")
})

test_that("plot.gg_rhf errors on idx not present", {
  gg <- gg_rhf(.rhf_pbc())
  expect_error(plot(gg, idx = -999L), "idx")
})
```

- [ ] **Step 2: Run to confirm failure.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_plot_gg_rhf.R")'`
Expected: FAIL — `could not find function "plot.gg_rhf"` (dispatch falls through to default).

- [ ] **Step 3: Implement `R/plot.gg_rhf.R`.**

```r
##=============================================================================
#' Plot Random Hazard Forest hazard / cumulative-hazard curves
#'
#' Draws case-specific ensemble curves from a [gg_rhf()] object: hazard
#' (default) or cumulative hazard, one line per case selected by `idx`.
#'
#' @param x A `gg_rhf` object from [gg_rhf()].
#' @param idx Integer vector of case ids (matched against the `id` column) to
#'   draw. `NULL` (default) draws every case.
#' @param hazard.only Logical; `TRUE` (default) plots the hazard, `FALSE`
#'   plots the cumulative hazard.
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_rhf()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   plot(gg_rhf(o), idx = c(1, 5, 10))
#' }
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw
#' @name plot.gg_rhf
#' @export
plot.gg_rhf <- function(x, idx = NULL, hazard.only = TRUE, ...) {
  if (!inherits(x, "gg_rhf")) {
    stop("plot.gg_rhf() requires a 'gg_rhf' object.", call. = FALSE)
  }
  ids <- unique(x$id)
  if (is.null(idx)) idx <- ids
  if (!any(idx %in% ids)) {
    stop("none of the requested idx values are present in the gg_rhf id ",
         "column.", call. = FALSE)
  }
  dta <- x[x$id %in% idx, , drop = FALSE]
  dta$id <- factor(dta$id)

  yvar <- if (hazard.only) "hazard" else "chf"
  ylab <- if (hazard.only) "Hazard" else "Cumulative hazard"

  ggplot2::ggplot(
    dta,
    ggplot2::aes(
      x      = .data[["time"]],
      y      = .data[[yvar]],
      colour = .data[["id"]],
      group  = .data[["id"]]
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::labs(x = "Time", y = ylab, colour = "Case") +
    ggplot2::theme_bw()
}
```

- [ ] **Step 4: Run to confirm pass.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_plot_gg_rhf.R")'`
Expected: PASS.

- [ ] **Step 5: Commit.**

```bash
git add R/plot.gg_rhf.R tests/testthat/test_plot_gg_rhf.R
git commit -m "feat: plot.gg_rhf() — per-case hazard / cumulative-hazard curves"
```

---

### Task 1.4: `print` / `summary` / `autoplot` S3 companions

**Files:**
- Modify: `R/print_methods.R`, `R/summary_methods.R`, `R/autoplot_methods.R`
- Test: `tests/testthat/test_gg_rhf.R`

- [ ] **Step 1: Write the failing tests.** Append to `tests/testthat/test_gg_rhf.R`:

```r
test_that("gg_rhf S3 companions work", {
  gg <- gg_rhf(.rhf_pbc())
  expect_output(print(gg), "gg_rhf")
  expect_invisible(print(gg))
  s <- summary(gg)
  expect_true(is.list(s) || is.data.frame(s))
  expect_s3_class(autoplot(gg, idx = 1), "ggplot")
})
```

- [ ] **Step 2: Run to confirm failure.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_gg_rhf.R")'`
Expected: FAIL — no `print.gg_rhf` (header line absent) / no `autoplot.gg_rhf`.

- [ ] **Step 3: Add `print.gg_rhf()` to `R/print_methods.R`** (follow the existing one-line-header convention; place after `print.gg_brier`):

```r
#' @rdname print.gg
#' @export
print.gg_rhf <- function(x, ...) {
  cat(.gg_header(x, "gg_rhf"),
      sprintf("  |  cases: %d  times: %d  source: %s",
              length(unique(x$id)),
              attr(x, "ntime") %||% length(unique(x$time)),
              x$source[1]),
      "\n", sep = "")
  invisible(x)
}
```

- [ ] **Step 4: Add `summary.gg_rhf()` to `R/summary_methods.R`** (return a small per-time summary data frame of the hazard distribution):

```r
#' @rdname summary.gg
#' @export
summary.gg_rhf <- function(object, ...) {
  agg <- stats::aggregate(
    cbind(hazard, chf) ~ time, data = as.data.frame(object),
    FUN = function(v) mean(v, na.rm = TRUE)
  )
  names(agg) <- c("time", "hazard.mean", "chf.mean")
  agg
}
```

- [ ] **Step 5: Add `autoplot.gg_rhf()` to `R/autoplot_methods.R`** (the ggplot2 generic is already re-exported in this file; dispatch to `plot()`):

```r
#' @rdname autoplot.gg
#' @export
autoplot.gg_rhf <- function(object, ...) {
  plot.gg_rhf(object, ...)
}
```

- [ ] **Step 6: Run to confirm pass.**

Run: `Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_gg_rhf.R")'`
Expected: PASS.

- [ ] **Step 7: Regenerate docs + commit.**

```bash
Rscript -e 'devtools::document()'
git add R/print_methods.R R/summary_methods.R R/autoplot_methods.R man/ NAMESPACE tests/testthat/test_gg_rhf.R
git commit -m "feat: print/summary/autoplot S3 companions for gg_rhf"
```

---

### Task 1.5: vdiffr snapshots

**Files:**
- Modify: `tests/testthat/test_snapshots.R`

- [ ] **Step 1: Add a gated snapshot block.** Append to `tests/testthat/test_snapshots.R` (mirror the existing `VDIFFR_RUN_TESTS` + `skip_on_cran` gating used by the other snapshot tests):

```r
test_that("gg-rhf-hazard", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("randomForestRHF")
  skip_on_cran()
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  gg <- gg_rhf(.rhf_pbc())
  vdiffr::expect_doppelganger("gg-rhf-hazard", plot(gg, idx = c(1, 5, 10)))
})

test_that("gg-rhf-chf", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("randomForestRHF")
  skip_on_cran()
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  gg <- gg_rhf(.rhf_pbc())
  vdiffr::expect_doppelganger("gg-rhf-chf",
                              plot(gg, idx = c(1, 5, 10), hazard.only = FALSE))
})
```

- [ ] **Step 2: Record the snapshots.**

Run: `NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_file("tests/testthat/test_snapshots.R", reporter="summary")'`
Expected: the two `gg-rhf-*` tests report "Adding new file snapshot"; two new SVGs appear under `tests/testthat/_snaps/snapshots/`.

- [ ] **Step 3: Confirm only the two new snapshots are added** (guard against the testthat snapshot-pruning side effect).

Run: `git status -s tests/testthat/_snaps/`
Expected: only `gg-rhf-hazard.svg` and `gg-rhf-chf.svg` untracked; nothing deleted. If unrelated snapshots show as deleted, run `git checkout -- tests/testthat/_snaps/` and re-record only these two.

- [ ] **Step 4: Commit.**

```bash
git add tests/testthat/test_snapshots.R \
        tests/testthat/_snaps/snapshots/gg-rhf-hazard.svg \
        tests/testthat/_snaps/snapshots/gg-rhf-chf.svg
git commit -m "test: vdiffr snapshots for gg_rhf hazard / chf plots"
```

---

### Task 1.6: Full local gate + PR into `dev`

**Files:** none (verification + PR).

- [ ] **Step 1: Run the full test suite.**

Run: `NOT_CRAN=true Rscript -e 'devtools::load_all(quiet=TRUE); testthat::test_dir("tests/testthat", filter="(gg_rhf|plot_gg_rhf|ggrandomforests_news)")'`
Expected: 0 failures (the gg_rhf tests skip cleanly if randomForestRHF is absent locally).

- [ ] **Step 2: Run `R CMD check --as-cran` WITH the manual build** (standing release-gate rule — never `--no-manual`; it hides raw-Unicode-in-Rd warnings).

Run: `_R_CHECK_FORCE_SUGGESTS_=false Rscript -e 'rcmdcheck::rcmdcheck(args=c("--as-cran"), error_on="never")'`
Expected: 0 errors, 0 warnings; at most the benign dev-version NOTE (`Version contains large components 3.0.0.9000`).

- [ ] **Step 3: Push and open the PR into `dev`.**

```bash
git push -u origin feat/rhf-phase1-gg-rhf
gh pr create --base dev --title "RHF Phase 0+1: gg_rhf hazard/CHF curves" \
  --body "Phase 0 (dev line + gated randomForestRHF Suggests) and Phase 1 (gg_rhf extractor + plot + print/summary/autoplot). Per dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md. R CMD check --as-cran: 0/0/<dev-version NOTE>."
```

- [ ] **Step 4: Address the Copilot review, resolve threads, get CI green, merge into `dev`** (standard workflow; `dev` requires the R-CMD-check + 3 OS release checks).

---

## Self-Review

**Spec coverage (Phase 0 + Phase 1 only — later phases are separate plans):**
- Suggests-gated `randomForestRHF` → Task 0.1 (Suggests) + every entry point's `requireNamespace`/`inherits` guard (Tasks 0.2, 1.2, fixtures).
- `gg_rhf` tidy frame `id/time/hazard/chf/source` → Task 1.2. ✓
- OOB vs inbag `source` → Task 1.2 (`source=` arg + fallback). ✓
- Provenance attr → Task 1.1 + `.set_provenance` call in 1.2. ✓
- `plot.gg_rhf` per-case curves, `hazard.only` toggle → Task 1.3. ✓
- `print`/`summary`/`autoplot` → Task 1.4. ✓
- vdiffr snapshots (gated) → Task 1.5. ✓
- Version mechanics + `--as-cran` w/ manual → Tasks 0.1, 1.6. ✓
- *Deferred within Phase 1 to a later refinement (not MVP): ensemble-mean overlay and CI ribbon from the spec's `gg_rhf` plot description — the per-case `idx` curves + `hazard.only` are the Phase-1 MVP; the overlay is additive and can land in a follow-up step without changing the tidy frame.*

**Placeholder scan:** no TBD/TODO; every code step shows complete code; commands have expected output.

**Type consistency:** `gg_rhf` columns `id/time/hazard/chf/source` used identically in the extractor (1.2), plot (1.3 reads `time`/`hazard`/`chf`/`id`), summary (1.4 aggregates `hazard`/`chf`/`time`), and snapshots (1.5). `source` arg values `"oob"`/`"inbag"` consistent. `attr(.,"ntime")` set in 1.2, read in 1.4 print.

**Note on the ensemble-overlay gap:** flagged above as a deliberate MVP scope cut, not an omission — the spec's plot section lists it as "optional." If you want it in Phase 1 proper, add a step to Task 1.3 with an `ensemble = FALSE` arg overlaying `aggregate(hazard ~ time, mean)` as a heavier line.
