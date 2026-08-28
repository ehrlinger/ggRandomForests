# varPro Phase 4c — `gg_beta_varpro` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a tidy wrapper and a default plot for `varPro::beta.varpro()` (per-rule lasso-coefficient refinement of variable importance), regression family only, with an explicit `beta_fit` argument for cache reuse.

**Architecture:** One new extractor (`gg_beta_varpro()`, S3 method on `varpro`), one new plot method (`plot.gg_beta_varpro()`), three new S3 companions (`print` / `summary` / `autoplot`), one session-memoised test fixture helper. The extractor optionally accepts a precomputed `varPro::beta.varpro()` result via `beta_fit =` — when supplied, the expensive lasso step is skipped. Per-variable aggregation is `beta_mean = mean(|β̂|)`; selection uses a single scalar `cutoff` (default `mean(beta_mean)`).

**Tech Stack:** R, varPro (Imports), glmnet (transitive — used only inside varPro), testthat, vdiffr (Suggests).

**Spec:** `dev/plans/2026-05-26-varpro-phase4-gg-beta-varpro-design.md`

**Branch state:** `feat/varpro-phase4c-gg-beta-varpro` is already created from `origin/main` with the design spec committed (`94b8f1c`, `cd203ab`). Current `main` is at version `2.7.3.9010` (PR #96 merged). This PR bumps to `2.7.3.9011`. If another PR has merged in the meantime, the implementer picks the next free `.901x` slot above `main`.

---

## File map

| File | Purpose |
|------|---------|
| `R/gg_beta_varpro.R` *(new)* | Extractor (S3 generic + `varpro` method), `print` / `summary` / `autoplot` companions |
| `R/plot.gg_beta_varpro.R` *(new)* | Default bar chart |
| `tests/testthat/helper-beta_varpro.R` *(new)* | `.beta_fit_mtcars()` session-memoised fixture |
| `tests/testthat/test_gg_beta_varpro.R` *(new)* | All 12 spec tests |
| `tests/testthat/test_snapshots.R` *(append)* | `gg-beta-varpro-default` vdiffr baseline |
| `DESCRIPTION` *(edit)* | Version bump |
| `NEWS.md` *(edit)* | v2.8.0 (development) entry |
| `_pkgdown.yml` *(edit)* | Add to varPro reference group |
| `man/gg_beta_varpro.Rd`, `man/plot.gg_beta_varpro.Rd` *(generated)* | `devtools::document()` output |
| `NAMESPACE` *(generated)* | `devtools::document()` output (exports + S3) |

---

## Task 0: Open dev cycle (version bump, NEWS heading)

**Files:**
- Modify: `DESCRIPTION:4`
- Modify: `NEWS.md` (top of file, after the `Package: ggRandomForests` line)

- [ ] **Step 1: Confirm branch state and clean working tree**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests
git branch --show-current   # expect: feat/varpro-phase4c-gg-beta-varpro
git status -sb              # expect: clean tree; ahead of origin
```

- [ ] **Step 2: Confirm current main version**

```bash
git fetch origin main --quiet
grep '^Version:' DESCRIPTION
git show origin/main:DESCRIPTION | grep '^Version:'
```

Expected: this branch has `Version: 2.7.3.9010` (inherited from PR #96). `main` should be `2.7.3.9010` as well after PR #96 merged. The bump below moves to `.9011`.

- [ ] **Step 3: Bump version**

Edit `DESCRIPTION` line 4:

```
Version: 2.7.3.9011
```

- [ ] **Step 4: Add NEWS heading + placeholder bullet**

Edit `NEWS.md`. Replace:

```
Package: ggRandomForests
Version: 2.7.3.9010
```

with:

```
Package: ggRandomForests
Version: 2.7.3.9011
```

Then insert at the top of the `ggRandomForests v2.8.0 (development) — continued` section (the existing top-most section in the file), as the first bullet:

```
* `gg_beta_varpro()` and `plot.gg_beta_varpro()`: tidy wrapper and default
  bar chart for `varPro::beta.varpro()` (per-rule lasso-coefficient
  refinement of variable importance). Regression family only. Optional
  `beta_fit` argument lets callers compute the expensive `beta.varpro`
  step once and reuse the result across multiple wrapper calls. Third of
  three Phase 4 sub-projects.
```

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION NEWS.md
git commit -m "chore: open v2.7.3.9011 dev cycle (varPro Phase 4c gg_beta_varpro)"
```

---

## Task 1: Test fixture helper (memoised `beta.varpro` on mtcars)

**Files:**
- Create: `tests/testthat/helper-beta_varpro.R`

The helper runs `beta.varpro` **once per R session**. testthat auto-sources files matching `helper-*.R` before each test file. Memoisation is via a file-scoped environment.

- [ ] **Step 1: Write the helper**

Create `tests/testthat/helper-beta_varpro.R`:

```r
# Session-memoised varpro + beta.varpro fixtures for the gg_beta_varpro tests.
# beta.varpro() is the expensive call (per-rule glmnet); compute once per R
# session and reuse. In-memory only — no disk cache.

.beta_varpro_cache <- new.env(parent = emptyenv())

.varpro_mtcars <- function() {
  if (is.null(.beta_varpro_cache$v)) {
    if (!requireNamespace("varPro", quietly = TRUE)) {
      testthat::skip("varPro not installed")
    }
    set.seed(20260526L)
    .beta_varpro_cache$v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
  }
  .beta_varpro_cache$v
}

.beta_fit_mtcars <- function() {
  if (is.null(.beta_varpro_cache$b)) {
    v <- .varpro_mtcars()
    set.seed(20260526L)
    .beta_varpro_cache$b <- varPro::beta.varpro(v)
  }
  .beta_varpro_cache$b
}
```

- [ ] **Step 2: Verify the helper loads without error**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); source("tests/testthat/helper-beta_varpro.R"); v <- .varpro_mtcars(); b <- .beta_fit_mtcars(); cat("class(v):", class(v), "\nclass(b):", class(b), "\nfamily:", v$family, "\nnrow(b$results):", nrow(b$results), "\n")'
```

Expected: `class(v): varpro`, `class(b): varpro`, `family: regr`, `nrow(b$results)` is a positive integer.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/helper-beta_varpro.R
git commit -m "test: session-memoised beta.varpro fixture for gg_beta_varpro tests"
```

---

## Task 2: Extractor — shape, aggregation, family guard (RED → GREEN)

**Files:**
- Create: `tests/testthat/test_gg_beta_varpro.R`
- Create: `R/gg_beta_varpro.R`

- [ ] **Step 1: Write failing tests (shape, aggregation, family guard)**

Create `tests/testthat/test_gg_beta_varpro.R`:

```r
test_that("gg_beta_varpro returns the expected tidy shape", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  out <- gg_beta_varpro(v, beta_fit = b)

  expect_s3_class(out, "gg_beta_varpro")
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out),
                  c("variable", "beta_mean", "n_rules", "selected"))
  released <- unique(b$results$variable[is.finite(b$results$imp)])
  expect_equal(nrow(out), length(released))
})

test_that("gg_beta_varpro aggregation equals mean(|imp|) per variable", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  res <- b$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  for (i in seq_len(min(2L, nrow(out)))) {
    var_name <- as.character(out$variable[i])
    var_idx  <- match(var_name, b$xvar.names)
    expected <- mean(abs(res$imp[res$variable == var_idx]))
    expect_equal(out$beta_mean[i], expected, tolerance = 1e-10,
                 label = paste0("beta_mean for ", var_name))
  }
})

test_that("gg_beta_varpro errors on non-regression family", {
  if (!requireNamespace("varPro", quietly = TRUE)) skip("varPro not installed")
  set.seed(20260526L)
  iris2 <- iris
  vc <- varPro::varpro(Species ~ ., data = iris2, ntree = 30)
  expect_error(
    gg_beta_varpro(vc),
    "family = 'class'"
  )
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
R -q -e 'devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: errors on all three tests with `"could not find function \"gg_beta_varpro\""`.

- [ ] **Step 3: Implement minimal extractor (shape, aggregation, family guard only — no cutoff yet, no beta_fit guard yet)**

Create `R/gg_beta_varpro.R`:

```r
##=============================================================================
#' Per-variable lasso-beta importance from a varPro fit
#'
#' Tidy wrapper around \code{varPro::beta.varpro()} for the regression
#' family. Aggregates the per-rule lasso coefficient (β̂) by variable into
#' \code{mean(|β̂|)} and flags variables above a scalar cutoff. Optional
#' \code{beta_fit} argument lets callers compute the expensive
#' \code{beta.varpro()} step once and reuse the result.
#'
#' @param object A \code{varpro} fit from \code{\link[varPro]{varpro}}
#'   (regression family).
#' @param ... Forwarded to \code{\link[varPro]{beta.varpro}} when
#'   \code{beta_fit} is \code{NULL}; ignored otherwise (with a warning).
#' @param cutoff Selection threshold on \code{beta_mean}. \code{NULL}
#'   (default) → \code{mean(beta_mean)}.
#' @param beta_fit Optional pre-computed \code{varPro::beta.varpro()}
#'   result for the same \code{object}. When supplied, the wrapper skips
#'   the expensive lasso fit.
#'
#' @return A \code{data.frame} of class \code{c("gg_beta_varpro", "data.frame")}.
#' @export
gg_beta_varpro <- function(object, ..., cutoff = NULL, beta_fit = NULL) {
  UseMethod("gg_beta_varpro", object)
}

#' @export
gg_beta_varpro.varpro <- function(object, ..., cutoff = NULL, beta_fit = NULL) {
  if (!inherits(object, "varpro")) {
    stop("gg_beta_varpro: expected a 'varpro' object from varPro::varpro().",
         call. = FALSE)
  }
  fam <- object$family
  if (!identical(fam, "regr")) {
    stop(sprintf(
      paste0("gg_beta_varpro currently supports varpro regression forests ",
             "only; got family = '%s'. Classification, regr+, and survival ",
             "are tracked under Phase 4d (see vignette / NEWS)."),
      fam
    ), call. = FALSE)
  }

  b <- if (is.null(beta_fit)) {
    varPro::beta.varpro(object, ...)
  } else {
    beta_fit
  }

  if (is.null(b)) {
    out <- data.frame(
      variable  = character(0),
      beta_mean = numeric(0),
      n_rules   = integer(0),
      selected  = logical(0),
      stringsAsFactors = FALSE
    )
    class(out) <- c("gg_beta_varpro", "data.frame")
    attr(out, "provenance") <- list(
      source = "varPro::beta.varpro", family = fam,
      n_rules_total = 0L, cutoff = NA_real_, cutoff_default = is.null(cutoff),
      precomputed = !is.null(beta_fit)
    )
    return(out)
  }

  res <- b$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  n_rules_total <- nrow(b$results)
  n_rules_nonzero <- sum(abs(res$imp) > 0)

  var_idx <- res$variable
  var_name <- b$xvar.names[var_idx]
  agg <- stats::aggregate(
    list(beta_mean = abs(res$imp), n_rules = rep(1L, nrow(res))),
    by = list(variable = var_name),
    FUN = function(x) if (is.logical(x[1L])) sum(x) else sum(x)
  )
  # aggregate above gives sum; convert to mean for beta_mean
  agg$beta_mean <- vapply(
    split(abs(res$imp), var_name),
    mean, numeric(1)
  )[agg$variable]
  agg$n_rules <- as.integer(agg$n_rules)

  resolved_cutoff <- if (is.null(cutoff)) mean(agg$beta_mean) else as.numeric(cutoff)
  agg$selected <- agg$beta_mean >= resolved_cutoff

  agg <- agg[order(-agg$beta_mean), , drop = FALSE]
  rownames(agg) <- NULL

  class(agg) <- c("gg_beta_varpro", "data.frame")
  attr(agg, "provenance") <- list(
    source          = "varPro::beta.varpro",
    family          = fam,
    ntree           = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff          = resolved_cutoff,
    cutoff_default  = is.null(cutoff),
    use.cv          = isTRUE(list(...)$use.cv),
    n_rules_total   = n_rules_total,
    n_rules_nonzero = n_rules_nonzero,
    precomputed     = !is.null(beta_fit),
    xvar.names      = b$xvar.names
  )
  agg
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: 3 passing tests.

- [ ] **Step 5: Commit**

```bash
git add R/gg_beta_varpro.R tests/testthat/test_gg_beta_varpro.R
git commit -m "feat(gg_beta_varpro): extractor for varPro::beta.varpro (regr)"
```

---

## Task 3: Cutoff handling (explicit + default) — RED → GREEN

**Files:**
- Modify: `tests/testthat/test_gg_beta_varpro.R` *(append)*

The extractor already implements cutoff logic; this task adds the test coverage that locks it in.

- [ ] **Step 1: Append failing tests**

Append to `tests/testthat/test_gg_beta_varpro.R`:

```r
test_that("gg_beta_varpro cutoff = 0 selects everything, Inf selects nothing", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  zero_cut <- gg_beta_varpro(v, beta_fit = b, cutoff = 0)
  expect_true(all(zero_cut$selected))

  inf_cut <- gg_beta_varpro(v, beta_fit = b, cutoff = Inf)
  expect_true(!any(inf_cut$selected))
})

test_that("gg_beta_varpro default cutoff is mean(beta_mean), provenance flagged", {
  b <- .beta_fit_mtcars()
  v <- .varpro_mtcars()

  out <- gg_beta_varpro(v, beta_fit = b)
  prov <- attr(out, "provenance")

  expect_equal(prov$cutoff, mean(out$beta_mean), tolerance = 1e-10)
  expect_true(prov$cutoff_default)

  explicit <- gg_beta_varpro(v, beta_fit = b, cutoff = 0.123)
  expect_false(attr(explicit, "provenance")$cutoff_default)
  expect_equal(attr(explicit, "provenance")$cutoff, 0.123)
})
```

- [ ] **Step 2: Run tests to verify they pass (cutoff is already implemented in Task 2)**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: 5 passing tests.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_beta_varpro.R
git commit -m "test(gg_beta_varpro): cutoff (explicit + default) coverage"
```

---

## Task 4: `beta_fit` shape guard + `...` warning — RED → GREEN

**Files:**
- Modify: `tests/testthat/test_gg_beta_varpro.R` *(append)*
- Modify: `R/gg_beta_varpro.R` (add validation)

- [ ] **Step 1: Append failing tests**

Append to `tests/testthat/test_gg_beta_varpro.R`:

```r
test_that("gg_beta_varpro rejects malformed beta_fit", {
  v <- .varpro_mtcars()
  expect_error(
    gg_beta_varpro(v, beta_fit = list()),
    "beta_fit does not look like a varPro::beta.varpro\\(\\) result"
  )

  b <- .beta_fit_mtcars()
  b_bad <- b
  b_bad$results <- b_bad$results[, setdiff(names(b_bad$results), "imp"), drop = FALSE]
  expect_error(
    gg_beta_varpro(v, beta_fit = b_bad),
    "imp"
  )
})

test_that("gg_beta_varpro warns when ... is supplied alongside beta_fit", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  expect_warning(
    out <- gg_beta_varpro(v, use.cv = TRUE, beta_fit = b),
    "ignored because beta_fit is supplied"
  )
  expect_s3_class(out, "gg_beta_varpro")
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: the two new tests fail (shape guard not implemented; no warning emitted).

- [ ] **Step 3: Add validation to the extractor**

In `R/gg_beta_varpro.R`, replace the `b <- if (is.null(beta_fit)) ...` block with:

```r
  if (is.null(beta_fit)) {
    b <- varPro::beta.varpro(object, ...)
  } else {
    required_cols <- c("tree", "branch", "variable", "n.oob", "imp")
    if (!inherits(beta_fit, "varpro") ||
        !is.data.frame(beta_fit$results)) {
      stop("gg_beta_varpro: beta_fit does not look like a varPro::beta.varpro() result. ",
           "Expected a varpro-class object with a data.frame in $results.",
           call. = FALSE)
    }
    missing_cols <- setdiff(required_cols, names(beta_fit$results))
    if (length(missing_cols) > 0L) {
      stop("gg_beta_varpro: beta_fit does not look like a varPro::beta.varpro() result. ",
           "Missing column(s): ", paste(missing_cols, collapse = ", "), ".",
           call. = FALSE)
    }
    dots <- list(...)
    if (length(dots) > 0L) {
      warning("gg_beta_varpro: arguments in '...' ignored because beta_fit is supplied.",
              call. = FALSE)
    }
    b <- beta_fit
  }
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: 7 passing tests.

- [ ] **Step 5: Commit**

```bash
git add R/gg_beta_varpro.R tests/testthat/test_gg_beta_varpro.R
git commit -m "feat(gg_beta_varpro): validate beta_fit shape, warn on '...'+beta_fit"
```

---

## Task 5: Empty result + zero-β rows preserved — RED → GREEN

**Files:**
- Modify: `tests/testthat/test_gg_beta_varpro.R` *(append)*

The extractor already keeps zero-β rules (only `NA` is filtered) and returns an empty-frame fast path for `NULL`. This task locks both in.

- [ ] **Step 1: Append failing tests**

Append:

```r
test_that("gg_beta_varpro returns empty frame when beta.varpro yields no rules", {
  v <- .varpro_mtcars()
  empty <- structure(
    list(results = data.frame(tree = integer(0), branch = integer(0),
                              variable = integer(0), n.oob = integer(0),
                              imp = numeric(0)),
         xvar.names = v$xvar.names),
    class = "varpro"
  )
  out <- gg_beta_varpro(v, beta_fit = empty)
  expect_s3_class(out, "gg_beta_varpro")
  expect_equal(nrow(out), 0L)
  expect_setequal(names(out), c("variable", "beta_mean", "n_rules", "selected"))
  expect_equal(attr(out, "provenance")$n_rules_total, 0L)
})

test_that("gg_beta_varpro counts lasso-shrunk-to-zero rules in n_rules", {
  v <- .varpro_mtcars()
  fake <- structure(
    list(
      results = data.frame(
        tree     = c(1L, 1L, 2L, 2L),
        branch   = c(1L, 2L, 1L, 2L),
        variable = c(1L, 1L, 1L, 1L),
        n.oob    = c(5L, 5L, 5L, 5L),
        imp      = c(0.0, 0.0, 1.5, -2.5)
      ),
      xvar.names = v$xvar.names
    ),
    class = "varpro"
  )
  out <- gg_beta_varpro(v, beta_fit = fake)
  expect_equal(nrow(out), 1L)
  expect_equal(out$n_rules, 4L)
  # mean(|0|, |0|, |1.5|, |-2.5|) = 1.0
  expect_equal(out$beta_mean, 1.0, tolerance = 1e-10)
})
```

- [ ] **Step 2: Run tests**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: 9 passing tests. (Both behaviours were implemented in Task 2; tests confirm.)

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_beta_varpro.R
git commit -m "test(gg_beta_varpro): empty-frame + zero-β-rows preserved"
```

---

## Task 6: Cache equivalence test (slow — guarded)

**Files:**
- Modify: `tests/testthat/test_gg_beta_varpro.R` *(append)*

This test compares the cached-path output against an internal-call output for byte equivalence. It calls `varPro::beta.varpro()` once more (in addition to the helper fixture), so it is guarded behind a `Sys.getenv("GG_BETA_VARPRO_SLOW_TESTS", "false")` switch — on by default in local dev, can be off on CRAN.

- [ ] **Step 1: Append failing test**

Append:

```r
test_that("gg_beta_varpro cached and uncached paths agree", {
  if (!identical(Sys.getenv("GG_BETA_VARPRO_SLOW_TESTS", "true"), "true")) {
    skip("Slow test — set GG_BETA_VARPRO_SLOW_TESTS=true to run")
  }
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()

  set.seed(20260526L)
  uncached <- gg_beta_varpro(v)
  cached   <- gg_beta_varpro(v, beta_fit = b)

  expect_equal(
    as.data.frame(uncached),
    as.data.frame(cached),
    tolerance = 1e-10
  )
  expect_false(attr(uncached, "provenance")$precomputed)
  expect_true(attr(cached, "provenance")$precomputed)
})
```

- [ ] **Step 2: Run test**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -25
```

Expected: 10 passing tests. (If beta.varpro produces non-deterministic output across two calls under the same seed, the test will fail; in that case, document the non-determinism and tighten tolerance or compare only `out$variable` ordering. Treat that as a finding to escalate, not silently relax.)

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_beta_varpro.R
git commit -m "test(gg_beta_varpro): cache equivalence between cached and internal paths"
```

---

## Task 7: Plot method (smoke test + minimal implementation)

**Files:**
- Modify: `tests/testthat/test_gg_beta_varpro.R` *(append)*
- Create: `R/plot.gg_beta_varpro.R`

- [ ] **Step 1: Append failing test**

Append:

```r
test_that("plot.gg_beta_varpro returns a ggplot that builds", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  p <- plot(out)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_s3_class(built, "ggplot_built")
  expect_true(length(built$data) >= 1L)
})
```

- [ ] **Step 2: Run test to verify it fails**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: error `"no applicable method for 'plot' applied to an object of class \"gg_beta_varpro\""`.

- [ ] **Step 3: Implement plot method**

Create `R/plot.gg_beta_varpro.R`:

```r
##=============================================================================
#' Plot a \code{gg_beta_varpro} object
#'
#' Horizontal bar chart of mean |β̂| per variable, sorted descending.
#' Bars filled blue when above the selection cutoff, grey otherwise.
#'
#' @param x A \code{gg_beta_varpro} object from \code{\link{gg_beta_varpro}}.
#' @param ... Not currently used.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{gg_beta_varpro}}
#' @name plot.gg_beta_varpro
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip
#' @importFrom ggplot2 scale_fill_manual labs theme_minimal
#' @export
plot.gg_beta_varpro <- function(x, ...) {
  if (nrow(x) == 0L) {
    stop("plot.gg_beta_varpro: nothing to plot (gg_beta_varpro has 0 rows).",
         call. = FALSE)
  }
  prov   <- attr(x, "provenance")
  cutoff <- prov$cutoff
  cv_txt <- if (isTRUE(prov$use.cv)) "cv" else "fixed"

  x$variable <- factor(x$variable, levels = x$variable[order(x$beta_mean)])

  ggplot2::ggplot(
    x,
    ggplot2::aes(
      x    = .data[["variable"]],
      y    = .data[["beta_mean"]],
      fill = factor(.data[["selected"]])
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
      guide  = "none"
    ) +
    ggplot2::geom_hline(
      yintercept = cutoff,
      linetype   = "dashed",
      color      = "#e74c3c",
      linewidth  = 0.7
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Mean |β| (per-rule lasso)",
      caption = sprintf(
        "Mean |β| over %d rules. Lasso: %s, cutoff: %.4g.",
        prov$n_rules_total %||% NA_integer_,
        cv_txt,
        cutoff
      )
    ) +
    ggplot2::theme_minimal()
}
```

Note: `%||%` is already defined as a package-internal helper in the existing R sources (used in `R/plot.gg_varpro.R`). If `R CMD check` complains, replace with `if (is.null(prov$n_rules_total)) NA_integer_ else prov$n_rules_total`.

- [ ] **Step 4: Run test to verify it passes**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: 11 passing tests.

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_beta_varpro.R tests/testthat/test_gg_beta_varpro.R
git commit -m "feat: plot.gg_beta_varpro horizontal bar chart"
```

---

## Task 8: S3 companions — print / summary / autoplot

**Files:**
- Modify: `tests/testthat/test_gg_beta_varpro.R` *(append)*
- Modify: `R/gg_beta_varpro.R` (append S3 methods)

- [ ] **Step 1: Append failing test**

Append:

```r
test_that("gg_beta_varpro S3 companions return the expected shapes", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)

  # print invisibly returns x
  pr <- withVisible(print(out))
  expect_false(pr$visible)
  expect_identical(pr$value, out)

  # summary returns a named numeric, sorted descending, attribute n_rules present
  s <- summary(out)
  expect_s3_class(s, "summary.gg_beta_varpro")
  expect_type(unclass(s), "double")
  expect_equal(unname(unclass(s)), sort(unname(unclass(s)), decreasing = TRUE))
  expect_equal(length(s), nrow(out))
  expect_true(!is.null(attr(s, "n_rules")))

  # autoplot delegates to plot
  p1 <- plot(out)
  p2 <- ggplot2::autoplot(out)
  expect_equal(
    ggplot2::ggplot_build(p1)$data,
    ggplot2::ggplot_build(p2)$data
  )
})
```

- [ ] **Step 2: Run test to verify it fails**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: failures (default `print.data.frame` is visible; `summary.data.frame` doesn't return class `summary.gg_beta_varpro`; `autoplot.gg_beta_varpro` not defined).

- [ ] **Step 3: Append S3 companions to `R/gg_beta_varpro.R`**

Append to `R/gg_beta_varpro.R`:

```r
#' @export
print.gg_beta_varpro <- function(x, ...) {
  prov <- attr(x, "provenance")
  cat("gg_beta_varpro (varPro::beta.varpro)\n")
  cat("  family        :", prov$family %||% NA, "\n")
  cat("  variables     :", nrow(x), "\n")
  cat("  n_rules_total :", prov$n_rules_total %||% NA, "\n")
  cat("  cutoff        :", format(prov$cutoff, digits = 4),
      if (isTRUE(prov$cutoff_default)) " (default)" else "", "\n", sep = "")
  cat("  precomputed   :", isTRUE(prov$precomputed), "\n")
  cat("Use head(x) to see rows.\n")
  invisible(x)
}

#' @export
summary.gg_beta_varpro <- function(object, ...) {
  v <- object$beta_mean
  names(v) <- as.character(object$variable)
  v <- sort(v, decreasing = TRUE)
  structure(v,
            n_rules  = setNames(object$n_rules, as.character(object$variable))[names(v)],
            class    = "summary.gg_beta_varpro")
}

#' @export
print.summary.gg_beta_varpro <- function(x, ...) {
  cat("Mean |β| per variable (descending):\n")
  print(unclass(x))
  cat("\nRule counts:\n")
  print(attr(x, "n_rules"))
  invisible(x)
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.gg_beta_varpro <- function(object, ...) {
  plot.gg_beta_varpro(object, ...)
}
```

If `%||%` is not visible inside the package, copy it from `R/utils.R` or replace inline with `if (is.null(x)) y else x`. (Existing wrappers use `%||%` freely; the helper is defined in `R/utils.R`.)

- [ ] **Step 4: Run test to verify it passes**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: 12 passing tests.

- [ ] **Step 5: Commit**

```bash
git add R/gg_beta_varpro.R tests/testthat/test_gg_beta_varpro.R
git commit -m "feat(gg_beta_varpro): print / summary / autoplot S3 companions"
```

---

## Task 9: Roxygen — pedantic β block, caching section, family-guard note

**Files:**
- Modify: `R/gg_beta_varpro.R` (roxygen blocks)
- Modify: `R/plot.gg_beta_varpro.R` (roxygen blocks)
- Regenerated: `man/gg_beta_varpro.Rd`, `man/plot.gg_beta_varpro.Rd`, `NAMESPACE`

- [ ] **Step 1: Replace the `gg_beta_varpro` roxygen header with the full pedagogical version**

Replace the existing `#'` block above `gg_beta_varpro <- function(...)` in `R/gg_beta_varpro.R` with:

```r
##=============================================================================
#' Per-variable lasso-β importance from a varPro fit
#'
#' Tidy wrapper around [varPro::beta.varpro()] for the regression family.
#' Aggregates the per-rule lasso coefficient (β̂) by variable into
#' `mean(|β̂|)` and flags variables above a scalar cutoff. Optional
#' `beta_fit` argument lets callers compute the expensive
#' `beta.varpro()` step once and reuse the result.
#'
#' @section What this is doing:
#' For each rule (a tree-branch pair) in the forest, [varPro::beta.varpro()]
#' fits a one-predictor lasso regression of the response on the released
#' variable's values, restricted to the OOB observations inside the rule's
#' region. The wrapper aggregates those per-rule coefficients into one
#' number per variable.
#'
#' @section What `imp` actually is (pedantic, because the column name is misleading):
#' The `imp` column on `beta.varpro()`'s `$results` is **not** a
#' variable-importance score in the conventional sense. It is a regularised
#' regression coefficient. Specifically:
#'
#' - Per rule, `glmnet` fits a one-predictor lasso of the response on
#'   the released variable inside the rule's OOB region. `use.cv = TRUE`
#'   selects λ by 10-fold CV (default `nfolds = 10`); `use.1se = TRUE`
#'   (default) picks `lambda.1se`. `use.cv = FALSE` uses the full λ path.
#' - `imp` is the **fitted coefficient β̂** at the chosen λ. **Sign is
#'   real** (direction of local association). **Magnitude depends on
#'   the predictor's units** (raw `x`, no standardisation); a predictor
#'   in millimetres has a smaller |β̂| than the same predictor in metres.
#' - Lasso shrinkage can drive β̂ to **exactly zero**. Those zeros are
#'   data, not missingness, and are kept in the aggregation. Convergence
#'   failures land as `NA_real_` and are dropped.
#' - The per-variable aggregate is `beta_mean = mean(|β̂|)` across the
#'   rules where this variable was released. It is **not** a permutation
#'   importance, **not** a split-strength importance, and **not** directly
#'   comparable on the same numeric axis to `gg_varpro()`'s z-scores.
#'   Disagreement with `gg_varpro` is often diagnostic, not a bug.
#'
#' In code form: `imp_r = β̂_glmnet(y | x_v restricted to rule r, λ chosen by use.cv / use.1se)`.
#'
#' @section What's in the output:
#' One row per released variable. Columns:
#' - `variable`: predictor name.
#' - `beta_mean`: mean of `|β̂|` across that variable's rules.
#' - `n_rules`: count of rules contributing (zero-β rules included; only
#'   `NA` failures excluded).
#' - `selected`: `beta_mean >= cutoff`.
#'
#' Provenance attribute carries `source`, `family`, `ntree`, `cutoff`,
#' `cutoff_default`, `use.cv`, `n_rules_total`, `n_rules_nonzero`,
#' `precomputed`, and `xvar.names`.
#'
#' @section What you use this for:
#' Picking variables when local effects matter more than aggregate
#' split-strength contribution. Compare side-by-side with [gg_varpro()] —
#' a variable that scores high here but low in `gg_varpro` is one whose
#' local linear effect inside many rules is real even though its
#' release-rule contrast is modest.
#'
#' @section Caching:
#' `beta.varpro()` is the expensive call (per-rule `glmnet` / `cv.glmnet`,
#' often minutes on real data). Compute it once and reuse:
#'
#' ```r
#' v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 200)
#' b <- varPro::beta.varpro(v, use.cv = TRUE)        # expensive, once
#' gg_a <- gg_beta_varpro(v, beta_fit = b)            # cheap
#' gg_b <- gg_beta_varpro(v, beta_fit = b, cutoff = 0.5)
#' ```
#'
#' Provenance carries `precomputed = TRUE` when `beta_fit` was supplied.
#'
#' @note Classification, multivariate regression (`regr+`), and survival
#'   families are out of scope for this release. The non-regression path
#'   errors with a message naming Phase 4d as the tracker (see NEWS).
#'
#' @param object A `varpro` fit from [varPro::varpro()] (regression family).
#' @param ... Forwarded to [varPro::beta.varpro()] when `beta_fit = NULL`;
#'   ignored otherwise (with a warning). Documented forwardables: `use.cv`,
#'   `use.1se`, `nfolds`, `maxit`, `thresh`, `max.rules.tree`, `max.tree`.
#' @param cutoff Selection threshold on `beta_mean`. `NULL` (default) →
#'   `mean(beta_mean)` across released variables. Numeric scalar otherwise.
#' @param beta_fit Optional pre-computed [varPro::beta.varpro()] result for
#'   the same `object`. `NULL` (default) → the wrapper runs `beta.varpro()`
#'   itself. When supplied, must be a `varpro`-class object whose `$results`
#'   has columns `tree / branch / variable / n.oob / imp`.
#'
#' @return A `data.frame` of class `c("gg_beta_varpro", "data.frame")`,
#'   one row per released variable, sorted by `beta_mean` descending.
#'
#' @seealso [gg_varpro()], [plot.gg_beta_varpro()], [varPro::beta.varpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#'   b <- varPro::beta.varpro(v)
#'   gg <- gg_beta_varpro(v, beta_fit = b)
#'   plot(gg)
#' }
#' }
#'
#' @export
```

- [ ] **Step 2: Replace `plot.gg_beta_varpro` roxygen with the full version**

Replace the existing roxygen above `plot.gg_beta_varpro <- function(x, ...)` in `R/plot.gg_beta_varpro.R` with:

```r
##=============================================================================
#' Plot a `gg_beta_varpro` object
#'
#' Horizontal bar chart of `mean(|β̂|)` per variable, sorted descending so
#' the eye lands on the top variable first. Bars filled blue when above the
#' selection cutoff, grey otherwise. Dashed red line marks the cutoff.
#'
#' @section Reading the chart:
#' Each bar is the average magnitude of a per-rule lasso coefficient for
#' that variable. **The numeric scale carries the predictor's units** —
#' if "age" is in years and "creatinine" is in mg/dL, a longer bar for
#' age does not mean age is "more important" in any unit-free sense.
#' Comparisons across data sets or across variables with very different
#' units require keeping the units context in mind. Within one data set,
#' bars are comparable up to that unit caveat.
#'
#' Variables above the cutoff are coloured blue and flagged `selected`;
#' variables below are grey. Lasso shrinkage can drive a rule's β̂ to
#' exactly zero — those rules are kept in the average, so a variable
#' with many shrunk-to-zero rules will sit lower in the ranking than
#' one whose released coefficients are consistently non-zero.
#'
#' @section What this tells you:
#' Use the bar chart as a selection ranking, not as an effect-size axis.
#' Pair it with [gg_varpro()] to see where split-strength importance and
#' local lasso-β importance agree or disagree; disagreement is often the
#' interesting signal.
#'
#' @param x A `gg_beta_varpro` object from [gg_beta_varpro()].
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_beta_varpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#'   plot(gg_beta_varpro(v))
#' }
#' }
#'
#' @name plot.gg_beta_varpro
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip
#' @importFrom ggplot2 scale_fill_manual labs theme_minimal
#' @export
```

- [ ] **Step 3: Regenerate documentation and NAMESPACE**

```bash
R -q -e 'devtools::document(quiet = TRUE)' 2>&1 | tail -10
```

Expected: writes `man/gg_beta_varpro.Rd`, `man/plot.gg_beta_varpro.Rd`, updates `NAMESPACE` with `export(gg_beta_varpro)`, `S3method(plot, gg_beta_varpro)`, etc. No warnings.

- [ ] **Step 4: Verify R CMD check is happy with the new Rd**

```bash
R -q -e 'devtools::check(args = c("--no-manual", "--as-cran"), quiet = TRUE)' 2>&1 | tail -10
```

Expected: `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`.

- [ ] **Step 5: Commit**

```bash
git add R/gg_beta_varpro.R R/plot.gg_beta_varpro.R man/ NAMESPACE
git commit -m "docs(gg_beta_varpro): pedantic β semantics + caching guidance"
```

---

## Task 10: vdiffr snapshot

**Files:**
- Modify: `tests/testthat/test_snapshots.R` *(append)*

- [ ] **Step 1: Append the snapshot block**

Append to `tests/testthat/test_snapshots.R`:

```r
test_that("gg-beta-varpro-default", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  p <- plot(gg_beta_varpro(v, beta_fit = b))
  vdiffr::expect_doppelganger("gg-beta-varpro-default", p)
})
```

- [ ] **Step 2: Record the baseline locally**

```bash
VDIFFR_RUN_TESTS=true R -q -e 'devtools::test(filter = "snapshots", reporter = "summary")' 2>&1 | tail -10
```

Expected: prompts to add a new doppelganger; accept it. A new SVG appears at `tests/testthat/_snaps/snapshots/gg-beta-varpro-default.svg`.

- [ ] **Step 3: Verify the snapshot is skipped without the env var (CRAN-safe)**

```bash
R -q -e 'devtools::test(filter = "snapshots", reporter = "summary")' 2>&1 | tail -10
```

Expected: the new test reports `SKIP`, not `FAIL`.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test_snapshots.R tests/testthat/_snaps/snapshots/gg-beta-varpro-default.svg
git commit -m "test(snapshots): vdiffr baseline for gg_beta_varpro default plot"
```

---

## Task 11: pkgdown reference group

**Files:**
- Modify: `_pkgdown.yml`

- [ ] **Step 1: Inspect current varPro group**

```bash
grep -n "varPro\|gg_varpro\|gg_partial_varpro\|gg_udependent" _pkgdown.yml
```

Find the reference section that contains `gg_partial_varpro` / `gg_varpro` / `gg_udependent` / `gg_isopro`. That is the varPro family group.

- [ ] **Step 2: Add the two new pages**

In that group's `contents:` list, add (alphabetical or after `gg_isopro`, whichever matches the file's existing convention):

```yaml
      - gg_beta_varpro
      - plot.gg_beta_varpro
```

- [ ] **Step 3: Build pkgdown locally to verify**

```bash
R -q -e 'pkgdown::build_reference()' 2>&1 | tail -10
```

Expected: builds without warnings; `docs/reference/gg_beta_varpro.html` and `docs/reference/plot.gg_beta_varpro.html` exist.

- [ ] **Step 4: Commit (do not commit the built `docs/` — it is gitignored, but `_pkgdown.yml` is tracked)**

```bash
git add _pkgdown.yml
git commit -m "docs: add gg_beta_varpro family to pkgdown reference"
```

---

## Task 12: Final NEWS pass, R CMD check, push, PR

**Files:**
- Modify: `NEWS.md` (refine the bullet added in Task 0 if needed)

- [ ] **Step 1: Refine the NEWS bullet**

Re-open `NEWS.md` and replace the placeholder bullet from Task 0 with the final language:

```
* `gg_beta_varpro()` and `plot.gg_beta_varpro()`: tidy wrapper and default
  horizontal bar chart for `varPro::beta.varpro()` — the per-rule lasso-β
  refinement of variable importance. Aggregates per-rule β̂ by variable
  into `beta_mean = mean(|β̂|)` and flags variables above a selection
  cutoff (default `mean(beta_mean)`). Optional `beta_fit` argument lets
  callers compute the expensive `beta.varpro()` step once and reuse the
  result across multiple wrapper calls (different cutoffs, snapshot
  rebuilds, vignette knits). `print` / `summary` / `autoplot` S3
  companions follow the existing `gg_*` conventions. **Regression family
  only** — classification, regr+, and survival are tracked under Phase 4d
  (see the spec for the endpoint map). Third of three Phase 4 sub-projects.
```

- [ ] **Step 2: Full check pass**

```bash
R -q -e 'devtools::check(args = c("--no-manual", "--as-cran"), quiet = TRUE)' 2>&1 | tail -10
```

Expected: `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`.

- [ ] **Step 3: Full test pass (cached path only — CRAN-safe)**

```bash
R -q -e 'devtools::test(reporter = "summary")' 2>&1 | tail -15
```

Expected: 0 failures.

- [ ] **Step 4: Slow-test pass (cache equivalence + vdiffr)**

```bash
GG_BETA_VARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true \
  R -q -e 'devtools::test(reporter = "summary")' 2>&1 | tail -15
```

Expected: 0 failures.

- [ ] **Step 5: Commit + push + open PR**

```bash
git add NEWS.md
git commit -m "docs(NEWS): finalize gg_beta_varpro v2.7.3.9011 entry"

git push -u origin feat/varpro-phase4c-gg-beta-varpro

gh pr create --title "varPro Phase 4c: gg_beta_varpro wrapper (regression)" --body "$(cat <<'EOF'
## Summary
- New `gg_beta_varpro()` + `plot.gg_beta_varpro()` for `varPro::beta.varpro()` (per-rule lasso-β importance). Regression family only.
- Optional `beta_fit` argument lets callers cache the expensive `beta.varpro` step.
- Pedantic roxygen on β semantics (lasso coefficient, not VIMP; sign + units matter; lasso-shrunk-to-zero rules kept).
- 12 tests, 1 vdiffr snapshot, pkgdown reference group updated.

Spec: `dev/plans/2026-05-26-varpro-phase4-gg-beta-varpro-design.md`
Plan: `dev/plans/2026-05-26-varpro-phase4-gg-beta-varpro-plan.md`

## Test plan
- [ ] `devtools::check(--as-cran)` 0/0/0
- [ ] `devtools::test()` 0 failures (CRAN-safe subset)
- [ ] `GG_BETA_VARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true devtools::test()` 0 failures
- [ ] pkgdown reference pages render

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Expected: PR URL printed.

---

## Self-review notes (from writing-plans skill)

**Spec coverage:** All 12 spec tests are mapped to tasks (Task 2: #1–#3; Task 3: #4–#5; Task 4: #10–#11; Task 5: #6, #12; Task 6: #9; Task 7: #7; Task 8: #8). All four signature-level requirements (regr-only guard, `beta_fit` shape, `...`+`beta_fit` warning, `cutoff` semantics) are covered. The pedantic-β roxygen mandate from the spec is encoded in Task 9 with the full block lifted from the spec. Caching layer 1 (`beta_fit`) is implemented in Task 2 + Task 4; caching layer 2 (session memoised fixture helper) is Task 1.

**Type consistency:** The tidy frame columns `variable / beta_mean / n_rules / selected` are stable across every task. Provenance fields (`source`, `family`, `ntree`, `cutoff`, `cutoff_default`, `use.cv`, `n_rules_total`, `n_rules_nonzero`, `precomputed`, `xvar.names`) match the spec verbatim.

**Placeholders:** None — each step has concrete code or commands. The one note about `%||%` in Task 7/8 is a known existing utility (`R/utils.R`) and the fallback is explicit if it fails.
