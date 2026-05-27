# varPro Phase 4d — `gg_ivarpro` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Tidy wrapper + per-variable-distribution / per-observation-profile plots for `varPro::ivarpro()` covering regression and classification (binary + multi-class) families.

**Architecture:** Family auto-dispatch on `object$family`. Tidy frame is long-format with NA cells filtered out: `(obs, variable, local_imp)` for regression, plus `class` for classification. `which_obs` (integer index) collapses to a per-observation profile; `which_class` (string) collapses to a single class. `cutoff` is polymorphic (NULL / scalar / named vector) with the same contract as gg_beta_varpro classification. `ivarpro_fit` is an explicit cache argument because `ivarpro()` is the most expensive call in varPro.

**Tech Stack:** R, varPro (Imports), testthat, vdiffr (Suggests).

**Spec:** `dev/plans/2026-05-26-varpro-phase4-gg-ivarpro-design.md`

**Branch state:** `feat/varpro-phase4d-gg-ivarpro` already exists from `origin/main` (currently at `2.7.3.9012` after PR #98). This PR bumps to `2.7.3.9013`. The spec is already committed on the branch.

---

## File map

| File | Purpose |
|------|---------|
| `R/gg_ivarpro.R` *(new)* | Extractor + `.gg_ivarpro_regr` + `.gg_ivarpro_class` + `.gg_ivarpro_empty` + `.validate_ivarpro_fit` + `autoplot.gg_ivarpro` |
| `R/plot.gg_ivarpro.R` *(new)* | 4-mode plot dispatch (regr distribution, regr which_obs, cls distribution faceted, cls which_obs faceted) |
| `R/print_methods.R` *(modify, append)* | `print.gg_ivarpro` |
| `R/summary_methods.R` *(modify, append)* | `summary.gg_ivarpro` |
| `tests/testthat/helper-beta_varpro.R` → `tests/testthat/helper-varpro-fixtures.R` *(rename + extend)* | Add `.ivarpro_*` fixtures into the same `.beta_varpro_cache` env (rename env to `.varpro_cache`) |
| `tests/testthat/test_gg_ivarpro.R` *(new)* | ~18 tests |
| `tests/testthat/test_snapshots.R` *(modify)* | 4 vdiffr baselines |
| `DESCRIPTION`, `NEWS.md`, `_pkgdown.yml` *(modify)* | Version bump, release notes, reference group |

---

## Task 0: Open dev cycle

**Files:**
- Modify: `DESCRIPTION:4`
- Modify: `NEWS.md`

- [ ] **Step 1: Confirm branch state**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests
git branch --show-current   # expect: feat/varpro-phase4d-gg-ivarpro
grep '^Version:' DESCRIPTION   # expect: 2.7.3.9012
```

- [ ] **Step 2: Bump version**

Edit `DESCRIPTION` line 4: `Version: 2.7.3.9013`.

Edit `NEWS.md` line 2: `Version: 2.7.3.9013`.

- [ ] **Step 3: Add NEWS placeholder bullet**

Insert at the top of the `ggRandomForests v2.8.0 (development) — continued` section:

```
* `gg_ivarpro()` and `plot.gg_ivarpro()`: tidy wrapper and per-variable
  / per-observation visualisations for `varPro::ivarpro()` (individual /
  local variable importance). Regression and classification (binary +
  multi-class). `which_obs` collapses to a single-observation profile;
  `which_class` collapses to a single class. Optional `ivarpro_fit`
  argument lets callers cache the expensive `ivarpro()` call. Last of
  four Phase 4 sub-projects.
```

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION NEWS.md
git commit -m "chore: open v2.7.3.9013 dev cycle (varPro Phase 4d gg_ivarpro)"
```

---

## Task 1: Rename + extend the varPro fixture helper

**Files:**
- Rename: `tests/testthat/helper-beta_varpro.R` → `tests/testthat/helper-varpro-fixtures.R`
- Modify (after rename): same file — rename cache env, add `.ivarpro_*` fixtures

- [ ] **Step 1: Rename the file**

```bash
git mv tests/testthat/helper-beta_varpro.R tests/testthat/helper-varpro-fixtures.R
```

- [ ] **Step 2: Rename the cache env from `.beta_varpro_cache` to `.varpro_cache`**

In `tests/testthat/helper-varpro-fixtures.R`, replace every occurrence of `.beta_varpro_cache` with `.varpro_cache`. The file currently defines an env at the top:

```r
.beta_varpro_cache <- new.env(parent = emptyenv())
```

becomes

```r
.varpro_cache <- new.env(parent = emptyenv())
```

And every `.beta_varpro_cache$<slot>` becomes `.varpro_cache$<slot>`.

- [ ] **Step 3: Append the ivarpro fixtures to the same file**

Add at the bottom of `tests/testthat/helper-varpro-fixtures.R`:

```r
.ivarpro_boston <- function() {
  if (is.null(.varpro_cache$iv_boston)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    if (!requireNamespace("MASS", quietly = TRUE))   testthat::skip("MASS not installed")
    set.seed(20260526L)
    v <- varPro::varpro(medv ~ ., data = MASS::Boston, ntree = 50)
    .varpro_cache$v_boston <- v
    .varpro_cache$iv_boston <- varPro::ivarpro(v)
  }
  .varpro_cache$iv_boston
}

.varpro_boston <- function() {
  if (is.null(.varpro_cache$v_boston)) {
    invisible(.ivarpro_boston())   # populates v_boston as a side-effect
  }
  .varpro_cache$v_boston
}

.ivarpro_iris_binary <- function() {
  if (is.null(.varpro_cache$iv_iris_binary)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    set.seed(20260526L)
    d <- iris[iris$Species != "setosa", ]
    d$Species <- droplevels(d$Species)
    v <- varPro::varpro(Species ~ ., data = d, ntree = 50)
    .varpro_cache$v_iris_binary <- v
    .varpro_cache$iv_iris_binary <- varPro::ivarpro(v)
  }
  .varpro_cache$iv_iris_binary
}

.varpro_iris_binary_for_ivarpro <- function() {
  if (is.null(.varpro_cache$v_iris_binary)) invisible(.ivarpro_iris_binary())
  .varpro_cache$v_iris_binary
}

.ivarpro_iris_multiclass <- function() {
  if (is.null(.varpro_cache$iv_iris_multi)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    set.seed(20260526L)
    v <- varPro::varpro(Species ~ ., data = iris, ntree = 50)
    .varpro_cache$v_iris_multi <- v
    .varpro_cache$iv_iris_multi <- varPro::ivarpro(v)
  }
  .varpro_cache$iv_iris_multi
}

.varpro_iris_multiclass_for_ivarpro <- function() {
  if (is.null(.varpro_cache$v_iris_multi)) invisible(.ivarpro_iris_multiclass())
  .varpro_cache$v_iris_multi
}
```

(Reuses the existing iris fixtures' seed and ntree conventions from the Phase 4c-classification fixtures, but with independent cache slots — Boston for regression, iris binary + multi-class for classification — because `ivarpro` is heavy enough that we don't want to share a varpro fit that was tuned for `beta.varpro`'s needs.)

- [ ] **Step 4: Verify the helpers load**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); source("tests/testthat/helper-varpro-fixtures.R"); iv <- .ivarpro_boston(); cat("class(iv):", class(iv), "dim:", paste(dim(iv), collapse="x"), "\n"); ivm <- .ivarpro_iris_multiclass(); cat("multiclass class:", class(ivm), "length:", length(ivm), "names:", paste(names(ivm), collapse=","), "\n")'
```

Expected: regression `iv` is a `data.frame` (or class `"data.frame"`); multiclass `ivm` is a `list` of length 3 named `setosa, versicolor, virginica`.

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/helper-varpro-fixtures.R
git commit -m "test: rename helper-beta_varpro -> helper-varpro-fixtures, add ivarpro fixtures"
```

---

## Task 2: Extractor — family dispatch + regression + classification (RED → GREEN)

**Files:**
- Create: `R/gg_ivarpro.R`
- Create: `tests/testthat/test_gg_ivarpro.R`

This is the largest task. It implements the dispatcher, both family paths, the cutoff polymorphism, the `which_obs` / `which_class` filters, the factor-level ordering, and the validation/empty-frame paths. ~10 tests cover the extractor; plot/print/summary tests come in later tasks.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test_gg_ivarpro.R`:

```r
# ---- Shape ----------------------------------------------------------------

test_that("gg_ivarpro regression returns long-format tidy frame", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)

  expect_s3_class(out, "gg_ivarpro")
  expect_s3_class(out, "data.frame")
  expect_setequal(names(out), c("obs", "variable", "local_imp", "selected"))
  expect_true(all(!is.na(out$local_imp)))  # NA cells filtered out
  expect_true(is.factor(out$variable))
})

test_that("gg_ivarpro classification adds class column", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv)

  expect_s3_class(out, "gg_ivarpro")
  expect_true("class" %in% names(out))
  expect_setequal(as.character(unique(out$class)), levels(iris$Species))
})

test_that("gg_ivarpro variable factor levels ordered by descending mean(|local_imp|)", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  expect_true(is.factor(out$variable))

  expected <- tapply(abs(out$local_imp), out$variable, mean, na.rm = TRUE)
  expected_order <- names(sort(expected, decreasing = TRUE))
  expect_equal(levels(out$variable), expected_order)
})

# ---- which_obs ------------------------------------------------------------

test_that("gg_ivarpro which_obs filters to a single observation", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L)
  expect_true(all(out$obs == 1L))
  expect_equal(attr(out, "provenance")$which_obs, 1L)
})

test_that("gg_ivarpro which_obs out of range errors with valid range", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  n  <- nrow(MASS::Boston)
  expect_error(
    gg_ivarpro(v, ivarpro_fit = iv, which_obs = n + 1L),
    "out of range"
  )
})

# ---- which_class ----------------------------------------------------------

test_that("gg_ivarpro binary classification which_class = NULL defaults to last factor level", {
  v  <- .varpro_iris_binary_for_ivarpro()
  iv <- .ivarpro_iris_binary()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  prov <- attr(out, "provenance")
  expect_equal(prov$which_class, "virginica")
  expect_setequal(as.character(unique(out$class)), "virginica")
})

test_that("gg_ivarpro which_class explicit returns single class", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv, which_class = "setosa")
  expect_equal(as.character(unique(out$class)), "setosa")
})

test_that("gg_ivarpro which_class not in levels errors", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  expect_error(
    gg_ivarpro(v, ivarpro_fit = iv, which_class = "bogus"),
    "is not a level of the response"
  )
})

test_that("gg_ivarpro which_class on regression warns and is ignored", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  expect_warning(
    out <- gg_ivarpro(v, ivarpro_fit = iv, which_class = "anything"),
    "ignored for regression family"
  )
  expect_false("class" %in% names(out))
})

# ---- cutoff polymorphism --------------------------------------------------

test_that("gg_ivarpro cutoff = NULL is per-class mean(|local_imp|) (named vector)", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  prov <- attr(out, "provenance")
  expect_named(prov$cutoff, levels(iris$Species))
  for (cls in levels(iris$Species)) {
    expect_equal(
      prov$cutoff[[cls]],
      mean(abs(out$local_imp[out$class == cls])),
      tolerance = 1e-10
    )
  }
})

test_that("gg_ivarpro scalar cutoff broadcasts across classes", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv, cutoff = 0.5)
  prov <- attr(out, "provenance")
  expect_equal(unname(prov$cutoff), rep(0.5, 3))
})

test_that("gg_ivarpro regression cutoff is length-1 named numeric", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  prov <- attr(out, "provenance")
  expect_named(prov$cutoff, "regr")
  expect_length(prov$cutoff, 1L)
})

# ---- ivarpro_fit shape guard ----------------------------------------------

test_that("gg_ivarpro rejects malformed ivarpro_fit", {
  v <- .varpro_boston()
  expect_error(gg_ivarpro(v, ivarpro_fit = list()),
               "does not look like a varPro::ivarpro\\(\\) result")
})

test_that("gg_ivarpro classification ivarpro_fit must be list of K named frames", {
  v <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  iv_bad <- iv[c("setosa", "versicolor")]   # drop one class
  expect_error(
    gg_ivarpro(v, ivarpro_fit = iv_bad),
    "class"
  )
})

# ---- ... + ivarpro_fit warning --------------------------------------------

test_that("gg_ivarpro warns when ... is supplied alongside ivarpro_fit", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  expect_warning(
    out <- gg_ivarpro(v, ivarpro_fit = iv, use.loo = TRUE),
    "ignored because ivarpro_fit is supplied"
  )
  expect_s3_class(out, "gg_ivarpro")
})

# ---- family guard ---------------------------------------------------------

test_that("gg_ivarpro errors on regr+ / surv families", {
  v <- .varpro_boston()
  v_fake <- v
  v_fake$family <- "surv"
  expect_error(gg_ivarpro(v_fake), "family = 'surv'")
})
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
R -q -e 'devtools::test(filter = "gg_ivarpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: all tests fail with `"could not find function \"gg_ivarpro\""`.

- [ ] **Step 3: Create the extractor**

Create `R/gg_ivarpro.R`:

```r
##=============================================================================
#' Individual (local) variable importance from a varPro fit
#'
#' Tidy wrapper around [varPro::ivarpro()] for regression and classification
#' families. Returns a long-format frame with one row per (observation,
#' variable) pair where the local-importance cell is non-NA;
#' classification adds an extra `class` column. `which_obs` collapses to
#' a single-observation profile; `which_class` collapses to a single
#' class. Optional `ivarpro_fit` argument lets callers cache the
#' expensive `ivarpro()` call.
#'
#' @param object A `varpro` fit from [varPro::varpro()] (regression or
#'   classification family).
#' @param ... Forwarded to [varPro::ivarpro()] when `ivarpro_fit = NULL`;
#'   ignored otherwise (with a warning). Documented forwardables:
#'   `adaptive`, `cut`, `cut.max`, `ncut`, `nmin`, `nmax`, `noise.na`,
#'   `max.rules.tree`, `max.tree`, `use.loo`, `use.abs`, `scale`.
#' @param which_obs Optional integer scalar — 1-based row index into the
#'   training data. `NULL` (default) returns the aggregate view.
#' @param which_class Optional response level name. `NULL` default on a
#'   binary classification fit resolves to the last factor level
#'   (positive-class convention). Ignored with a warning on regression
#'   fits.
#' @param cutoff Selection threshold on `|local_imp|`. `NULL` (default)
#'   resolves to the per-class `mean(|local_imp|)` (or per-frame mean
#'   for regression). A numeric scalar broadcasts. A named numeric
#'   vector (names a subset of class levels) overrides per class with
#'   fallback to the per-class mean for missing names.
#' @param ivarpro_fit Optional pre-computed [varPro::ivarpro()] result
#'   for the same `object`. Shape-validated.
#'
#' @return A `data.frame` of class `c("gg_ivarpro", "data.frame")`.
#'   Regression: columns `obs / variable / local_imp / selected`.
#'   Classification: long-format with an extra `class` column.
#'   `variable` is a factor whose levels are set by
#'   `mean(|local_imp|)` descending across all rows (the unified
#'   ranking axis shared across facets / panels).
#'
#' @seealso [gg_varpro()], [gg_beta_varpro()], [varPro::ivarpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#'   iv <- varPro::ivarpro(v)
#'   gg <- gg_ivarpro(v, ivarpro_fit = iv)
#'   plot(gg)
#' }
#' }
#'
#' @export
gg_ivarpro <- function(object, ..., which_obs = NULL, which_class = NULL,
                       cutoff = NULL, ivarpro_fit = NULL) {
  UseMethod("gg_ivarpro", object)
}

#' @export
gg_ivarpro.varpro <- function(object, ..., which_obs = NULL,
                              which_class = NULL, cutoff = NULL,
                              ivarpro_fit = NULL) {
  if (!inherits(object, "varpro")) {
    stop("gg_ivarpro: expected a 'varpro' object from varPro::varpro().",
         call. = FALSE)
  }
  fam <- object$family
  if (!fam %in% c("regr", "class")) {
    stop(sprintf(
      paste0("gg_ivarpro currently supports varpro regression and ",
             "classification forests only; got family = '%s'. regr+ and ",
             "survival are tracked under Phase 4d follow-ups (see NEWS)."),
      fam
    ), call. = FALSE)
  }

  # Capture forwardable args before dispatch (the dots don't pass through
  # to the internal frames).
  dots_use_loo <- if (is.null(ivarpro_fit)) isTRUE(list(...)$use.loo) else NA
  dots_scale   <- if (is.null(ivarpro_fit)) {
    s <- list(...)$scale
    if (is.null(s)) NA_character_ else s[1L]
  } else NA_character_

  # Resolve ivarpro_fit
  if (is.null(ivarpro_fit)) {
    iv <- varPro::ivarpro(object, ...)
  } else {
    .validate_ivarpro_fit(ivarpro_fit, object, fam)
    if (length(list(...)) > 0L) {
      warning("gg_ivarpro: arguments in '...' ignored because ivarpro_fit is supplied.",
              call. = FALSE)
    }
    iv <- ivarpro_fit
  }

  # Warn on which_class with regression
  if (fam == "regr" && !is.null(which_class)) {
    warning("gg_ivarpro: which_class ignored for regression family.",
            call. = FALSE)
    which_class <- NULL
  }

  # Validate which_obs
  n_train <- if (fam == "regr") nrow(iv) else nrow(iv[[1L]])
  if (!is.null(which_obs)) {
    if (!is.numeric(which_obs) || length(which_obs) != 1L ||
        which_obs != as.integer(which_obs) ||
        which_obs < 1L || which_obs > n_train) {
      stop(sprintf(
        "gg_ivarpro: which_obs = %s is out of range. Valid range: 1..%d.",
        format(which_obs), n_train
      ), call. = FALSE)
    }
    which_obs <- as.integer(which_obs)
  }

  if (fam == "regr") {
    return(.gg_ivarpro_regr(object, iv, cutoff, which_obs, ivarpro_fit,
                            dots_use_loo, dots_scale))
  }
  .gg_ivarpro_class(object, iv, cutoff, which_obs, which_class, ivarpro_fit,
                    dots_use_loo, dots_scale)
}

#' @noRd
.validate_ivarpro_fit <- function(ivarpro_fit, object, fam) {
  xvars <- object$xvar.org.names
  n_train <- nrow(object$x)

  if (fam == "regr") {
    if (!is.data.frame(ivarpro_fit)) {
      stop("gg_ivarpro: ivarpro_fit does not look like a varPro::ivarpro() ",
           "regression result (expected a data.frame).", call. = FALSE)
    }
    missing_cols <- setdiff(xvars, names(ivarpro_fit))
    if (length(missing_cols) > 0L) {
      stop("gg_ivarpro: ivarpro_fit missing column(s): ",
           paste(missing_cols, collapse = ", "), ".", call. = FALSE)
    }
    if (nrow(ivarpro_fit) != n_train) {
      stop(sprintf("gg_ivarpro: ivarpro_fit has %d rows but object trained ",
                   "on %d observations.", nrow(ivarpro_fit), n_train),
           call. = FALSE)
    }
  } else {
    cls <- .ivarpro_class_levels(object)
    if (!is.list(ivarpro_fit) || is.data.frame(ivarpro_fit)) {
      stop("gg_ivarpro: ivarpro_fit does not look like a varPro::ivarpro() ",
           "classification result (expected a list of data.frames, one per ",
           "class).", call. = FALSE)
    }
    if (!identical(sort(names(ivarpro_fit)), sort(cls))) {
      stop("gg_ivarpro: ivarpro_fit class names mismatch. Expected: ",
           paste(cls, collapse = ", "), ". Got: ",
           paste(names(ivarpro_fit), collapse = ", "), ".",
           call. = FALSE)
    }
  }
  invisible(NULL)
}

#' @noRd
.ivarpro_class_levels <- function(object) {
  # Same priority chain as gg_beta_varpro classification — y.org first because
  # varPro internally recodes binary y to 0/1.
  if (!is.null(object$y.org) && is.factor(object$y.org)) return(levels(object$y.org))
  if (is.factor(object$y)) return(levels(object$y))
  if (!is.null(attr(object$y, "levels"))) return(attr(object$y, "levels"))
  sort(unique(as.character(object$y)))
}

#' @noRd
.ivarpro_long <- function(mat, xvars) {
  # mat: nrow = n_train, ncol = p (xvars). Returns long-format
  # data.frame (obs, variable, local_imp) with NA cells dropped.
  rows <- list()
  for (j in seq_along(xvars)) {
    col_vals <- mat[[xvars[j]]]
    keep <- which(is.finite(col_vals))
    if (length(keep) == 0L) next
    rows[[j]] <- data.frame(
      obs       = keep,
      variable  = xvars[j],
      local_imp = col_vals[keep],
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0L) {
    return(data.frame(obs = integer(0), variable = character(0),
                      local_imp = numeric(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, rows)
}

#' @noRd
.gg_ivarpro_regr <- function(object, iv, cutoff, which_obs, ivarpro_fit,
                             use_loo, scale_arg) {
  xvars <- object$xvar.org.names
  long  <- .ivarpro_long(iv, xvars)

  # Factor-level ordering by descending mean(|local_imp|)
  agg <- tapply(abs(long$local_imp), long$variable, mean, na.rm = TRUE)
  ord_names <- names(sort(agg, decreasing = TRUE))
  long$variable <- factor(long$variable, levels = ord_names)

  resolved_cutoff <- if (is.null(cutoff)) mean(abs(long$local_imp)) else as.numeric(cutoff)
  long$selected <- abs(long$local_imp) >= resolved_cutoff

  if (!is.null(which_obs)) long <- long[long$obs == which_obs, , drop = FALSE]
  long <- long[order(long$variable, long$obs), , drop = FALSE]
  rownames(long) <- NULL

  class(long) <- c("gg_ivarpro", "data.frame")
  attr(long, "provenance") <- list(
    source         = "varPro::ivarpro",
    family         = "regr",
    ntree          = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff         = stats::setNames(resolved_cutoff, "regr"),
    cutoff_default = is.null(cutoff),
    use.loo        = use_loo,
    scale          = scale_arg,
    n_train        = nrow(object$x),
    n_obs          = length(unique(long$obs)),
    n_var          = nlevels(long$variable),
    precomputed    = !is.null(ivarpro_fit),
    xvar.names     = xvars,
    which_obs      = which_obs,
    which_class    = NULL
  )
  long
}

#' @noRd
.gg_ivarpro_class <- function(object, iv, cutoff, which_obs, which_class,
                              ivarpro_fit, use_loo, scale_arg) {
  cls   <- .ivarpro_class_levels(object)
  xvars <- object$xvar.org.names
  n_cls <- length(cls)

  # Per-class long-format frames
  per_class_long <- lapply(cls, function(c_name) {
    mat <- iv[[c_name]]
    if (is.null(mat)) return(NULL)
    df <- .ivarpro_long(mat, xvars)
    df$class <- c_name
    df
  })
  long <- do.call(rbind, per_class_long)

  if (nrow(long) == 0L) {
    return(.gg_ivarpro_empty(object, "class", which_obs, which_class,
                             ivarpro_fit, cutoff))
  }

  # Unified factor-level ordering across all (obs, class)
  agg <- tapply(abs(long$local_imp), long$variable, mean, na.rm = TRUE)
  ord_names <- names(sort(agg, decreasing = TRUE))
  long$variable <- factor(long$variable, levels = ord_names)

  # Validate which_class
  if (!is.null(which_class)) {
    if (!which_class %in% cls) {
      stop(sprintf(
        "gg_ivarpro: which_class = '%s' is not a level of the response. Levels: %s.",
        which_class, paste(cls, collapse = ", ")
      ), call. = FALSE)
    }
  } else if (n_cls == 2L) {
    which_class <- cls[n_cls]   # binary default = last factor level
  }

  # Per-class mean(|local_imp|) for cutoff resolution
  per_class_mean <- vapply(cls, function(c_name) {
    vals <- long$local_imp[long$class == c_name]
    if (length(vals) == 0L) NA_real_ else mean(abs(vals))
  }, numeric(1))
  names(per_class_mean) <- cls

  resolved_cutoff <- .resolve_class_cutoff(cutoff, per_class_mean, cls)

  long$selected <- mapply(function(li, cl) abs(li) >= resolved_cutoff[[cl]],
                          long$local_imp, long$class)

  if (!is.null(which_class)) long <- long[long$class == which_class, , drop = FALSE]
  if (!is.null(which_obs))   long <- long[long$obs   == which_obs,   , drop = FALSE]

  long$class <- factor(long$class, levels = cls)
  long <- long[order(long$class, long$variable, long$obs), , drop = FALSE]
  rownames(long) <- NULL

  class(long) <- c("gg_ivarpro", "data.frame")
  attr(long, "provenance") <- list(
    source         = "varPro::ivarpro",
    family         = "class",
    ntree          = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff         = resolved_cutoff,
    cutoff_default = is.null(cutoff),
    use.loo        = use_loo,
    scale          = scale_arg,
    n_train        = nrow(object$x),
    n_obs          = length(unique(long$obs)),
    n_var          = nlevels(long$variable),
    precomputed    = !is.null(ivarpro_fit),
    xvar.names     = xvars,
    class_levels   = cls,
    which_obs      = which_obs,
    which_class    = which_class
  )
  long
}

#' @noRd
.gg_ivarpro_empty <- function(object, fam, which_obs, which_class,
                              ivarpro_fit, cutoff) {
  cols <- if (fam == "class") {
    list(obs = integer(0), variable = factor(character(0)),
         class = factor(character(0)),
         local_imp = numeric(0), selected = logical(0))
  } else {
    list(obs = integer(0), variable = factor(character(0)),
         local_imp = numeric(0), selected = logical(0))
  }
  base <- as.data.frame(cols, stringsAsFactors = FALSE)
  class(base) <- c("gg_ivarpro", "data.frame")
  prov <- if (fam == "class") {
    cls <- .ivarpro_class_levels(object)
    list(source = "varPro::ivarpro", family = "class",
         cutoff = stats::setNames(rep(NA_real_, length(cls)), cls),
         cutoff_default = is.null(cutoff),
         n_train = nrow(object$x), n_obs = 0L,
         precomputed = !is.null(ivarpro_fit),
         class_levels = cls,
         which_obs = which_obs, which_class = which_class)
  } else {
    list(source = "varPro::ivarpro", family = "regr",
         cutoff = stats::setNames(NA_real_, "regr"),
         cutoff_default = is.null(cutoff),
         n_train = nrow(object$x), n_obs = 0L,
         precomputed = !is.null(ivarpro_fit),
         which_obs = which_obs, which_class = NULL)
  }
  attr(base, "provenance") <- prov
  base
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.gg_ivarpro <- function(object, ...) {
  plot.gg_ivarpro(object, ...)
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_ivarpro", reporter = "summary")' 2>&1 | tail -20
```

Expected: all 16 extractor tests pass.

- [ ] **Step 5: Commit**

```bash
git add R/gg_ivarpro.R tests/testthat/test_gg_ivarpro.R
git commit -m "feat(gg_ivarpro): extractor with family dispatch (regr + class)"
```

---

## Task 3: Plot method — 4-mode dispatch

**Files:**
- Create: `R/plot.gg_ivarpro.R`
- Modify: `tests/testthat/test_gg_ivarpro.R` *(append)*

- [ ] **Step 1: Append plot smoke tests**

Append to `tests/testthat/test_gg_ivarpro.R`:

```r
# ---- plot dispatch matrix --------------------------------------------------

test_that("plot.gg_ivarpro regression distribution builds", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv))
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot.gg_ivarpro regression which_obs builds (single panel)", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L))
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot.gg_ivarpro classification distribution builds (faceted)", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv))
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_true(length(unique(built$layout$layout$PANEL)) >= 2L)
})

test_that("plot.gg_ivarpro classification which_obs builds (faceted)", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L))
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})
```

- [ ] **Step 2: Run to verify failure**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_ivarpro", reporter = "summary")' 2>&1 | tail -10
```

Expected: 4 new tests fail (`no applicable method for 'plot'`).

- [ ] **Step 3: Create `R/plot.gg_ivarpro.R`**

```r
##=============================================================================
#' Plot a `gg_ivarpro` object
#'
#' Branches on the presence of `which_obs` provenance and the `class`
#' column. Distribution view: jittered points showing per-observation
#' local importances per variable. Per-observation view: horizontal bar
#' chart of one observation's local importances across variables.
#' Classification: faceted by class unless `which_class` collapses to
#' a single class.
#'
#' @section Reading the chart:
#' Each point in the distribution view is one observation's local
#' importance for that variable. Variables are sorted by descending
#' `mean(|local_imp|)`. The cutoff line picks the variables whose local
#' importance is, on average, large enough to flag. For a classification
#' fit, every facet shares the same row order so you can read across.
#'
#' @param x A `gg_ivarpro` object from [gg_ivarpro()].
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_ivarpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#'   plot(gg_ivarpro(v))
#' }
#' }
#'
#' @name plot.gg_ivarpro
#' @importFrom ggplot2 ggplot aes geom_col geom_jitter geom_hline coord_flip
#' @importFrom ggplot2 facet_wrap scale_fill_manual scale_color_manual labs theme_minimal
#' @export
plot.gg_ivarpro <- function(x, ...) {
  if (nrow(x) == 0L) {
    stop("plot.gg_ivarpro: nothing to plot (gg_ivarpro has 0 rows).",
         call. = FALSE)
  }
  prov           <- attr(x, "provenance")
  has_class      <- "class" %in% names(x)
  which_obs_set  <- !is.null(prov) && !is.null(prov$which_obs)

  cutoff_vec <- if (!is.null(prov) && !is.null(prov$cutoff)) {
    prov$cutoff
  } else if (has_class) {
    stats::setNames(vapply(split(abs(x$local_imp), x$class), mean, numeric(1)),
                    levels(x$class))
  } else {
    stats::setNames(mean(abs(x$local_imp)), "regr")
  }

  base <- ggplot2::ggplot(
    x,
    ggplot2::aes(x = .data[["variable"]],
                 y = .data[["local_imp"]])
  )

  if (which_obs_set) {
    p <- base +
      ggplot2::geom_col(
        ggplot2::aes(fill = factor(.data[["selected"]]))
      ) +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
        guide  = "none"
      )
  } else {
    p <- base +
      ggplot2::geom_jitter(
        ggplot2::aes(color = factor(.data[["selected"]])),
        width = 0.2, height = 0, alpha = 0.5
      ) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
        guide  = "none"
      )
  }

  p <- p + ggplot2::coord_flip()

  if (has_class && length(unique(x$class)) > 1L) {
    hline_df <- data.frame(
      class  = factor(names(cutoff_vec), levels = levels(x$class)),
      cutoff = unname(cutoff_vec),
      stringsAsFactors = FALSE
    )
    p <- p +
      ggplot2::facet_wrap(~ class, nrow = 1L) +
      ggplot2::geom_hline(
        data        = hline_df,
        ggplot2::aes(yintercept = .data[["cutoff"]]),
        linetype    = "dashed",
        color       = "#e74c3c",
        linewidth   = 0.7,
        inherit.aes = FALSE
      )
  } else {
    cutoff_scalar <- if (has_class) cutoff_vec[[as.character(x$class[1])]] else cutoff_vec[[1]]
    p <- p + ggplot2::geom_hline(
      yintercept = cutoff_scalar,
      linetype   = "dashed",
      color      = "#e74c3c",
      linewidth  = 0.7
    )
  }

  caption_txt <- sprintf(
    "Local importance over %d obs x %d variables.%s",
    prov$n_obs %||% NA_integer_,
    prov$n_var %||% NA_integer_,
    if (which_obs_set) sprintf(" obs = %d.", prov$which_obs) else ""
  )

  p + ggplot2::labs(
    x = NULL,
    y = "Local importance",
    caption = caption_txt
  ) + ggplot2::theme_minimal()
}
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_ivarpro", reporter = "summary")' 2>&1 | tail -10
```

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_ivarpro.R tests/testthat/test_gg_ivarpro.R
git commit -m "feat(plot.gg_ivarpro): distribution / which_obs / faceted dispatch"
```

---

## Task 4: print + summary methods

**Files:**
- Modify: `R/print_methods.R`
- Modify: `R/summary_methods.R`
- Modify: `tests/testthat/test_gg_ivarpro.R` *(append)*

- [ ] **Step 1: Append failing tests**

Append:

```r
# ---- print + summary ------------------------------------------------------

test_that("print.gg_ivarpro prints invisibly with header", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  pr <- withVisible(print(out))
  expect_false(pr$visible)
  expect_identical(pr$value, out)
})

test_that("summary.gg_ivarpro regression returns descending named numeric", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  s <- summary(out)
  expect_s3_class(s, "summary.gg_ivarpro")
  vals <- as.numeric(unclass(s))
  expect_equal(vals, sort(vals, decreasing = TRUE))
})

test_that("summary.gg_ivarpro classification returns per-class list", {
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  s <- summary(out)
  expect_s3_class(s, "summary.gg_ivarpro")
  expect_true(is.list(unclass(s)))
  expect_setequal(names(unclass(s)), levels(iris$Species))
})

test_that("autoplot.gg_ivarpro matches plot", {
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  out <- gg_ivarpro(v, ivarpro_fit = iv)
  p1 <- plot(out)
  p2 <- ggplot2::autoplot(out)
  expect_equal(
    ggplot2::ggplot_build(p1)$data,
    ggplot2::ggplot_build(p2)$data
  )
})
```

- [ ] **Step 2: Append `print.gg_ivarpro` to `R/print_methods.R`**

Find the end of `print.gg_beta_varpro` and append below it (still inside the file):

```r
#' @rdname print.gg
#' @export
print.gg_ivarpro <- function(x, ...) {
  prov        <- attr(x, "provenance")
  precomputed <- isTRUE(if (!is.null(prov)) prov$precomputed else FALSE)
  n_obs       <- if (!is.null(prov)) prov$n_obs %||% NA_integer_ else NA_integer_
  n_var       <- if (!is.null(prov)) prov$n_var %||% NA_integer_ else NA_integer_
  which_obs   <- if (!is.null(prov)) prov$which_obs else NULL
  which_cls   <- if (!is.null(prov)) prov$which_class else NULL
  has_class   <- "class" %in% names(x)

  view <- if (!is.null(which_obs)) sprintf("obs %d", which_obs) else "aggregate"
  cls_part <- if (has_class) {
    n_cls <- length(unique(x$class))
    if (!is.null(which_cls)) sprintf("  |  class: %s", which_cls)
    else sprintf("  |  %d classes (faceted)", n_cls)
  } else ""

  cat(.gg_header(x, "gg_ivarpro"),
      sprintf("  |  view: %s", view),
      cls_part,
      sprintf("  |  precomputed: %s", precomputed),
      "\n",
      sprintf("  %d (obs x variable) cells; %d unique obs across %d variables\n",
              nrow(x), n_obs, n_var),
      sep = "")
  invisible(x)
}
```

- [ ] **Step 3: Append `summary.gg_ivarpro` to `R/summary_methods.R`**

Find the end of `summary.gg_beta_varpro` and append:

```r
#' @rdname summary.gg
#' @export
summary.gg_ivarpro <- function(object, ...) {
  prov   <- attr(object, "provenance")
  family <- if (!is.null(prov)) prov$family %||% "regr" else "regr"

  per_var <- function(df) {
    v <- tapply(abs(df$local_imp), df$variable, mean, na.rm = TRUE)
    v <- v[!is.na(v)]
    v <- sort(v, decreasing = TRUE)
    n_obs <- tapply(df$obs, df$variable,
                    function(o) length(unique(o)))[names(v)]
    structure(v, n_obs = n_obs)
  }

  if (identical(family, "class") && "class" %in% names(object)) {
    per_class <- split(object, object$class, drop = TRUE)
    by_class  <- lapply(per_class, per_var)
    structure(by_class, class = "summary.gg_ivarpro")
  } else {
    structure(per_var(object), class = "summary.gg_ivarpro")
  }
}
```

Also add a `print.summary.gg_ivarpro` to `R/print_methods.R`:

```r
#' @export
print.summary.gg_ivarpro <- function(x, ...) {
  if (is.list(unclass(x)) && !is.numeric(unclass(x))) {
    for (cls in names(x)) {
      cat(sprintf("Class '%s' - mean |local_imp| per variable (descending):\n", cls))
      print(unclass(x[[cls]]))
      cat("\nObservation counts:\n")
      print(attr(x[[cls]], "n_obs"))
      cat("\n")
    }
  } else {
    cat("Mean |local_imp| per variable (descending):\n")
    print(unclass(x))
    cat("\nObservation counts:\n")
    print(attr(x, "n_obs"))
  }
  invisible(x)
}
```

- [ ] **Step 4: Regenerate NAMESPACE**

```bash
R -q -e 'devtools::document(quiet = TRUE)' 2>&1 | tail -5
```

- [ ] **Step 5: Run tests to verify they pass**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_ivarpro", reporter = "summary")' 2>&1 | tail -10
```

- [ ] **Step 6: Commit**

```bash
git add R/print_methods.R R/summary_methods.R tests/testthat/test_gg_ivarpro.R NAMESPACE
git commit -m "feat(gg_ivarpro): print + summary + autoplot S3 companions"
```

---

## Task 5: Roxygen pedagogical pass

**Files:**
- Modify: `R/gg_ivarpro.R` (roxygen)
- Modify: `R/plot.gg_ivarpro.R` (roxygen)

- [ ] **Step 1: Extend `gg_ivarpro` roxygen with the pedagogical sections**

Replace the existing roxygen block above `gg_ivarpro <- function(...)` in `R/gg_ivarpro.R` with the fuller version below. The header keeps the existing description and params, then inserts pedagogical sections before `@return`:

```r
##=============================================================================
#' Individual (local) variable importance from a varPro fit
#'
#' Tidy wrapper around [varPro::ivarpro()] for the regression or
#' classification family. Returns one row per (observation, variable)
#' pair where the local-importance cell is non-NA; classification adds
#' a `class` column. `which_obs` collapses to a per-observation
#' profile; `which_class` collapses to a single class. Optional
#' `ivarpro_fit` argument lets callers cache the expensive
#' `ivarpro()` call.
#'
#' @section What this is doing:
#' `ivarpro()` walks the varPro forest's rules and, for each
#' (observation, variable) pair, computes a scaled per-rule
#' contribution to predicting that observation. Per-rule LOO removes
#' the observation from its own rule before scoring. Per-region
#' scaling (`scale = "local"`, default) standardises the contribution
#' by the rule's local response standard deviation so values are
#' comparable across rules of different size. Aggregating those
#' per-rule scores into one number per (obs, variable) pair gives the
#' `local_imp` cell.
#'
#' @section What `local_imp` actually is (pedantic):
#' `local_imp[i, v]` is the **scaled aggregated rule contribution** of
#' variable `v` to predicting observation `i`, NOT a permutation
#' importance and NOT a SHAP value. **Sign carries direction** of the
#' local response shift inside the rule's region. **Magnitude is on
#' the response scale** when `scale = "global"`, or unit-free when
#' `scale = "local"` (the default). The matrix is **heavily sparse** —
#' an observation contributes only to rules that retain it as OOB; on
#' real data, per-variable NA fractions of 50-95% are common.
#' Comparison with `gg_varpro()` (aggregate split-strength) and
#' `gg_beta_varpro()` (per-rule lasso β) is diagnostic: a variable
#' that's important globally but has low per-observation contribution
#' for a specific case is interesting; the inverse — high local but
#' low global — flags a regime-specific signal.
#'
#' @section What's in the output:
#' Long-format tidy frame. Regression has columns `obs`, `variable`,
#' `local_imp`, `selected`. Classification adds a `class` column
#' (factor in response-level order). `variable` is a factor whose
#' levels are set by `mean(|local_imp|)` descending across all rows;
#' for classification that aggregate is across all (obs, class) so
#' every facet / panel shows variables in the same row order. NA
#' cells are filtered out — the source matrix is sparse, and the
#' tidy frame only carries the cells where local importance is
#' defined.
#'
#' Provenance attribute carries `source`, `family`, `ntree`, `cutoff`
#' (named numeric vector — length 1 named `"regr"` for regression,
#' length K named with class levels for classification),
#' `cutoff_default`, `use.loo`, `scale`, `n_train`, `n_obs`, `n_var`,
#' `precomputed`, `xvar.names`, `class_levels` (classification only),
#' `which_obs`, `which_class`.
#'
#' @section What you use this for:
#' Per-observation interpretation ("which variables drive *this*
#' prediction?"), variable-selection diagnostics via the aggregate
#' distribution view, and side-by-side comparison against
#' [gg_varpro()] / [gg_beta_varpro()] to spot variables that matter
#' locally but not globally (or vice versa).
#'
#' @section Caching:
#' `ivarpro()` is **the most expensive call in varPro** (per-rule
#' leave-one-out + per-region scaling, often minutes on real data).
#' Compute it once and reuse:
#'
#' ```r
#' v   <- varPro::varpro(medv ~ ., data = Boston, ntree = 200)
#' iv  <- varPro::ivarpro(v, scale = "local")              # expensive, once
#' gg_aggregate <- gg_ivarpro(v, ivarpro_fit = iv)          # cheap
#' gg_case1     <- gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L)
#' ```
#'
#' Provenance carries `precomputed = TRUE` when `ivarpro_fit` was supplied.
#'
#' @section Classification:
#' For a classification fit, `ivarpro()` returns a list of K matrices
#' (one per class). The wrapper stacks these into a long-format frame
#' with a `class` column. `which_class = NULL` returns all classes
#' (binary defaults to the last factor level, the positive-class
#' convention used by `glm` and `gg_roc`); `which_class = "<name>"`
#' filters to a single class. `cutoff` polymorphism mirrors
#' [gg_beta_varpro()] — `NULL` is per-class mean(|local_imp|), a
#' scalar broadcasts, a named numeric vector overrides per class
#' with fallback to that class's mean.
#'
#' @section Reproducibility:
#' Byte-for-byte agreement between cached (`ivarpro_fit = iv`) and
#' uncached (`ivarpro_fit = NULL`) outputs requires reusing the same
#' `ivarpro()` result. `set.seed()` alone is not sufficient because
#' per-rule LOO subsampling can drift across separate calls. Reuse
#' `ivarpro_fit` when reproducibility matters.
#'
#' @note Multivariate regression (`regr+`) and survival families are
#'   out of scope for this release. The non-regression / non-class
#'   path errors with a message naming Phase 4 follow-ups as the
#'   tracker.
```

(Then re-attach the existing `@param`, `@return`, `@seealso`, `@examples`, `@export` block — keep them verbatim below the new sections.)

- [ ] **Step 2: Extend `plot.gg_ivarpro` roxygen with scale caveat + classification note**

Append to the existing "Reading the chart" section in `R/plot.gg_ivarpro.R`:

```
For a classification fit, variables are sorted by descending
`mean(|local_imp|)` across all (obs, class) rows and that ordering
is shared across every facet, so rows line up between classes for
visual comparison. Each facet has its own cutoff line.

The per-observation view (`which_obs`) is a horizontal bar chart of
one observation's local importances; bars below the cutoff are grey,
above are blue. Think of it as a SHAP-style waterfall, but the
values are scaled per-rule contributions, not Shapley values.
```

- [ ] **Step 3: Regenerate documentation**

```bash
R -q -e 'devtools::document(quiet = TRUE)' 2>&1 | tail -5
```

- [ ] **Step 4: Confirm R CMD check stays clean**

```bash
R -q -e 'devtools::check(args = c("--no-manual", "--as-cran"), quiet = TRUE)' 2>&1 | tail -5
```

Expected: 0 / 0 / 0.

- [ ] **Step 5: Commit**

```bash
git add R/gg_ivarpro.R R/plot.gg_ivarpro.R man/ NAMESPACE
git commit -m "docs(gg_ivarpro): pedantic local_imp semantics + caching + classification"
```

---

## Task 6: vdiffr snapshots (4 baselines)

**Files:**
- Modify: `tests/testthat/test_snapshots.R` *(append)*

- [ ] **Step 1: Append snapshot blocks**

```r
test_that("gg-ivarpro-regr-distribution", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv))
  vdiffr::expect_doppelganger("gg-ivarpro-regr-distribution", p)
})

test_that("gg-ivarpro-regr-which-obs", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L))
  vdiffr::expect_doppelganger("gg-ivarpro-regr-which-obs", p)
})

test_that("gg-ivarpro-class-distribution", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv))
  vdiffr::expect_doppelganger("gg-ivarpro-class-distribution", p)
})

test_that("gg-ivarpro-class-which-obs", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L))
  vdiffr::expect_doppelganger("gg-ivarpro-class-which-obs", p)
})
```

- [ ] **Step 2: Record baselines**

```bash
VDIFFR_RUN_TESTS=true R -q -e 'devtools::test(filter = "snapshots", reporter = "summary")' 2>&1 | tail -15
```

Expected: four new SVGs created under `tests/testthat/_snaps/snapshots/`. If vdiffr does not auto-write the baseline on first run, try `R -q -e 'testthat::snapshot_accept()'` after a first run and re-test.

- [ ] **Step 3: Verify CRAN-safe SKIP without the env var**

```bash
R -q -e 'devtools::test(filter = "snapshots", reporter = "summary")' 2>&1 | tail -10
```

Expected: four new tests SKIP.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test_snapshots.R tests/testthat/_snaps/snapshots/gg-ivarpro-*.svg
git commit -m "test(snapshots): vdiffr baselines for gg_ivarpro (4 modes)"
```

---

## Task 7: pkgdown reference + cache-equivalence test + final NEWS + check + push + PR

**Files:**
- Modify: `_pkgdown.yml`
- Modify: `tests/testthat/test_gg_ivarpro.R` *(append cache equivalence test)*
- Modify: `NEWS.md` (refine the placeholder bullet)

- [ ] **Step 1: Add `gg_ivarpro` and `plot.gg_ivarpro` to `_pkgdown.yml`**

Search the existing varPro reference group:

```bash
grep -n "gg_isopro\|gg_beta_varpro\|gg_partial_varpro\|gg_varpro\|gg_udependent" _pkgdown.yml
```

Add the two new entries in the same group (alphabetical or after `gg_isopro`, matching the in-file convention).

- [ ] **Step 2: Append the cache-equivalence test**

Append to `tests/testthat/test_gg_ivarpro.R`:

```r
# ---- slow: cache equivalence ----------------------------------------------

test_that("gg_ivarpro cached and uncached paths agree (slow)", {
  if (!identical(Sys.getenv("GG_IVARPRO_SLOW_TESTS", "true"), "true")) {
    skip("Slow test - set GG_IVARPRO_SLOW_TESTS=true to run")
  }
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()

  set.seed(20260526L)
  uncached <- gg_ivarpro(v)
  cached   <- gg_ivarpro(v, ivarpro_fit = iv)

  strip <- function(df) { attr(df, "provenance") <- NULL; df }
  expect_equal(strip(as.data.frame(uncached)),
               strip(as.data.frame(cached)),
               tolerance = 1e-6)
  expect_false(attr(uncached, "provenance")$precomputed)
  expect_true(attr(cached,   "provenance")$precomputed)
})
```

- [ ] **Step 3: Refine the NEWS bullet**

Replace the placeholder bullet inserted in Task 0 with:

```
* `gg_ivarpro()` and `plot.gg_ivarpro()`: tidy wrapper and
  per-variable-distribution / per-observation-profile plots for
  `varPro::ivarpro()` (individual / local variable importance) across
  regression and classification (binary + multi-class) families. The
  long-format tidy frame is `(obs, variable, local_imp, selected)` for
  regression; classification adds a `class` column. NA cells are
  filtered out and sparsity is surfaced in provenance. `which_obs`
  (integer index) collapses to a single-observation profile; the plot
  switches from a jittered distribution view to a horizontal bar
  chart. `which_class` (response level name) collapses to a single
  class panel; binary fits default to the last factor level (positive
  class). `cutoff` accepts `NULL` (per-class mean), a scalar, or a
  named numeric vector — matching the gg_beta_varpro classification
  contract. Optional `ivarpro_fit` argument lets callers cache the
  expensive `ivarpro()` call. Last of four Phase 4 sub-projects.
```

- [ ] **Step 4: Full check + tests (both env conditions)**

```bash
R -q -e 'devtools::check(args = c("--no-manual", "--as-cran"), quiet = TRUE)' 2>&1 | tail -5
R -q -e 'devtools::test(reporter = "summary")' 2>&1 | tail -10
GG_IVARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true R -q -e 'devtools::test(reporter = "summary")' 2>&1 | tail -10
R -q -e 'cat("lints:", length(lintr::lint_package()), "\n")'
```

All must show 0 failures / 0 errors / 0 warnings; lint = 0.

- [ ] **Step 5: Commit + push + open PR**

```bash
git add _pkgdown.yml tests/testthat/test_gg_ivarpro.R NEWS.md
git commit -m "docs: pkgdown + cache-equivalence test + finalize NEWS for gg_ivarpro"

git push -u origin feat/varpro-phase4d-gg-ivarpro

gh pr create --title "varPro Phase 4d: gg_ivarpro (local variable importance)" --body "$(cat <<'EOF'
## Summary
- New `gg_ivarpro()` + `plot.gg_ivarpro()` for `varPro::ivarpro()` (individual / local variable importance). Regression and classification (binary + multi-class) in one PR.
- Long-format tidy frame: `(obs, variable, local_imp, selected)` for regression; adds `class` for classification. NA cells filtered out; sparsity surfaced via provenance.
- `which_obs` (integer) collapses to a per-observation horizontal bar chart; `which_class` (string) collapses to a single class panel; binary fits default to the last factor level (positive-class convention).
- `cutoff` polymorphic: NULL / scalar / named numeric vector (same contract as gg_beta_varpro classification).
- `ivarpro_fit` cache argument because `ivarpro()` is the most expensive call in varPro.
- 4 vdiffr baselines, pkgdown reference updated.

Spec: `dev/plans/2026-05-26-varpro-phase4-gg-ivarpro-design.md`
Plan: `dev/plans/2026-05-26-varpro-phase4-gg-ivarpro-plan.md`

## Test plan
- [ ] `devtools::check(--as-cran)` 0/0/0
- [ ] `lintr::lint_package()` 0 issues
- [ ] `devtools::test()` 0 failures (CRAN-safe subset)
- [ ] `GG_IVARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true devtools::test()` 0 failures, 4 snapshots build

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Return the PR URL.

---

## Self-review notes

**Spec coverage:** all 18 spec tests are mapped (T2: 1–16 covers shape, family dispatch, which_obs, which_class, cutoff polymorphism, shape guard, warning; T3: plot smoke 4 modes; T4: print + summary + autoplot; T7: cache equivalence under slow guard). Provenance shape contract (`cutoff` always a named numeric) is preserved across both family paths and the empty-frame path. Factor-level ordering by descending `mean(|local_imp|)` is asserted in T2#3 and naturally tested by the snapshot baselines.

**Type consistency:** `local_imp` is the column name everywhere; `obs` is integer; `variable` is a factor; `class` is a factor on the classification path. Provenance fields and their types are consistent between `.gg_ivarpro_regr` and `.gg_ivarpro_class`. The shared `.resolve_class_cutoff` helper (already exists in `R/gg_beta_varpro.R` from PR #98) is reused — no duplication.

**Placeholders:** none.

**Known cross-file dependency:** `.resolve_class_cutoff` lives in `R/gg_beta_varpro.R` and is reused here without re-declaring it. The roxygen `@noRd` markers prevent it from being exported. R sources all `R/*.R` files at package load time, so the cross-file reference works.

**Out-of-scope follow-ups flagged in spec, not in this plan:**
- Phase 5 consolidated varPro vignette (separate brainstorm after merge).
- `\dontrun{}` → `\donttest{}` + chatty `message()` cleanup.
- Factor-level alignment in `gg_vimp` / `plot.gg_varpro(conditional = TRUE)`.
