# varPro Phase 4c-classification — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Extend `gg_beta_varpro()` to handle `varPro::varpro` classification fits (binary and multi-class), with a clinician-friendly default for binary (single positive-class panel) and a faceted view for multi-class.

**Architecture:** Family auto-dispatch on `object$family` (`regr` keeps its current path; `class` adds a new long-format path). Single S3 class `gg_beta_varpro`; plot branches on the presence of a `class` column. `which_class` argument controls binary single-panel default (last factor level) and per-class subset on multi-class. `cutoff` becomes polymorphic (scalar / named vector / `NULL`). `variable` is a factor whose levels are set by `mean(|imp_total|)` descending — one ordering shared across all facets. Regression provenance `cutoff` also moves to a 1-element named numeric for shape consistency.

**Tech Stack:** R, varPro (Imports), testthat, vdiffr (Suggests).

**Spec:** `dev/plans/2026-05-26-varpro-phase4-gg-beta-varpro-classification-design.md`

**Branch state:** `feat/varpro-phase4c-classification` already exists from `origin/main` (currently at `2.7.3.9011` after PR #97). This PR bumps to `2.7.3.9012`. The spec is already committed on the branch.

---

## File map

| File | Purpose |
|------|---------|
| `R/gg_beta_varpro.R` *(modify)* | Drop regr-only guard, family dispatch, new class path, polymorphic cutoff, factor-level ordering, regression provenance shape change |
| `R/plot.gg_beta_varpro.R` *(modify)* | Branch on `class` column; faceted + per-class cutoff lines; single-panel mode unchanged |
| `R/print_methods.R` *(modify)* | Extend `print.gg_beta_varpro` suffix for classification |
| `R/summary_methods.R` *(modify)* | Extend `summary.gg_beta_varpro` to return list-of-classes for class family |
| `tests/testthat/helper-beta_varpro.R` *(modify)* | Add binary + multi-class fixtures (memoised) |
| `tests/testthat/test_gg_beta_varpro.R` *(modify)* | ~16 new classification tests; 1 Phase 4c provenance-shape test update |
| `tests/testthat/test_snapshots.R` *(modify)* | 2 new vdiffr baselines |
| `DESCRIPTION`, `NEWS.md`, `_pkgdown.yml` *(modify)* | Version bump + release notes + reference-description tweak |

---

## Task 0: Open dev cycle

**Files:**
- Modify: `DESCRIPTION:4`
- Modify: `NEWS.md`

- [ ] **Step 1: Confirm branch state**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests
git branch --show-current   # expect: feat/varpro-phase4c-classification
grep '^Version:' DESCRIPTION  # expect: 2.7.3.9011
```

- [ ] **Step 2: Bump version**

Edit `DESCRIPTION` line 4: `Version: 2.7.3.9012`.

Edit `NEWS.md` line 2: `Version: 2.7.3.9012`.

- [ ] **Step 3: Add NEWS placeholder bullet**

Insert at the top of `ggRandomForests v2.8.0 (development) — continued` in `NEWS.md`:

```
* `gg_beta_varpro()` adds varPro classification support (binary +
  multi-class). Binary fits default to a single positive-class panel
  (last factor level); multi-class fits return a long-format frame
  with a `class` column and plot as `facet_wrap(~ class)`. Optional
  `which_class` selects a single class; `cutoff` accepts a scalar or
  per-class named vector. Variables are stored as a factor whose
  levels are set by `mean(|sum-of-class-beta|)` descending so every
  facet shows rows in the same order. Motivating use case: 30-day
  mortality.
```

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION NEWS.md
git commit -m "chore: open v2.7.3.9012 dev cycle (gg_beta_varpro classification)"
```

---

## Task 1: Classification test fixtures (memoised)

**Files:**
- Modify: `tests/testthat/helper-beta_varpro.R`

- [ ] **Step 1: Append fixtures to the helper**

Append to `tests/testthat/helper-beta_varpro.R` (which already has the regression `.varpro_mtcars()` / `.beta_fit_mtcars()` from PR #97):

```r
.varpro_iris_binary <- function() {
  if (is.null(.beta_varpro_cache$vb)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    set.seed(20260526L)
    d <- iris[iris$Species != "setosa", ]
    d$Species <- droplevels(d$Species)
    .beta_varpro_cache$vb <- varPro::varpro(Species ~ ., data = d, ntree = 30)
  }
  .beta_varpro_cache$vb
}

.beta_fit_iris_binary <- function() {
  if (is.null(.beta_varpro_cache$bb)) {
    set.seed(20260526L)
    .beta_varpro_cache$bb <- varPro::beta.varpro(.varpro_iris_binary())
  }
  .beta_varpro_cache$bb
}

.varpro_iris_multiclass <- function() {
  if (is.null(.beta_varpro_cache$vm)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    set.seed(20260526L)
    .beta_varpro_cache$vm <- varPro::varpro(Species ~ ., data = iris, ntree = 30)
  }
  .beta_varpro_cache$vm
}

.beta_fit_iris_multiclass <- function() {
  if (is.null(.beta_varpro_cache$bm)) {
    set.seed(20260526L)
    .beta_varpro_cache$bm <- varPro::beta.varpro(.varpro_iris_multiclass())
  }
  .beta_varpro_cache$bm
}
```

- [ ] **Step 2: Verify the helpers load**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); source("tests/testthat/helper-beta_varpro.R"); vb <- .varpro_iris_binary(); bb <- .beta_fit_iris_binary(); vm <- .varpro_iris_multiclass(); bm <- .beta_fit_iris_multiclass(); cat("binary family:", vb$family, "\nmulticlass family:", vm$family, "\nbb cols:", colnames(bb$results), "\nbm cols:", colnames(bm$results), "\n")'
```

Expected: both families `class`, binary cols include `imp.1 imp.2`, multi-class cols include `imp.1 imp.2 imp.3`.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/helper-beta_varpro.R
git commit -m "test: session-memoised classification fixtures for gg_beta_varpro"
```

---

## Task 2: Family dispatch + class path (RED → GREEN, core aggregation)

**Files:**
- Modify: `R/gg_beta_varpro.R`
- Modify: `tests/testthat/test_gg_beta_varpro.R`

This is the largest task. It (a) drops the regression-only family guard, (b) adds the class path with per-class aggregation, (c) implements `which_class` resolution, (d) implements polymorphic `cutoff`, (e) sets `variable` as a factor ordered by total |β|, (f) moves the regression provenance `cutoff` to a 1-element named numeric, (g) updates the Phase 4c test that asserted scalar.

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test_gg_beta_varpro.R`:

```r
# ---- Classification: shape + factor ordering --------------------------------

test_that("gg_beta_varpro binary classification returns long-format with class", {
  vb <- .varpro_iris_binary()
  bb <- .beta_fit_iris_binary()
  out <- gg_beta_varpro(vb, beta_fit = bb)
  expect_s3_class(out, "gg_beta_varpro")
  expect_true("class" %in% names(out))
  # Binary default: which_class = last factor level (positive class)
  expect_equal(as.character(unique(out$class)),
               tail(levels(droplevels(iris$Species[iris$Species != "setosa"])), 1))
})

test_that("gg_beta_varpro multi-class returns all classes faceted by default", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  expect_true("class" %in% names(out))
  expect_setequal(as.character(unique(out$class)), levels(iris$Species))
})

test_that("gg_beta_varpro variable is a factor ordered by total-|imp| descending", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  expect_true(is.factor(out$variable))

  # Compute expected order independently
  res <- bm$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  var_name <- bm$xvar.names[res$variable]
  agg <- vapply(split(abs(res$imp), var_name), mean, numeric(1))
  expected_levels <- names(sort(agg, decreasing = TRUE))
  expect_equal(levels(out$variable), expected_levels)
})

# ---- which_class semantics --------------------------------------------------

test_that("gg_beta_varpro which_class = NULL on binary resolves to last factor level", {
  vb <- .varpro_iris_binary()
  bb <- .beta_fit_iris_binary()
  out <- gg_beta_varpro(vb, beta_fit = bb)
  prov <- attr(out, "provenance")
  expect_equal(prov$which_class, "virginica")
})

test_that("gg_beta_varpro which_class explicit returns single panel", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm, which_class = "setosa")
  expect_equal(as.character(unique(out$class)), "setosa")
  expect_equal(attr(out, "provenance")$which_class, "setosa")
})

test_that("gg_beta_varpro which_class not in levels errors with levels listed", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  expect_error(
    gg_beta_varpro(vm, beta_fit = bm, which_class = "nope"),
    "is not a level of the response"
  )
})

# ---- Aggregation correctness ------------------------------------------------

test_that("gg_beta_varpro classification beta_mean equals mean(|imp.k|) per (var, class)", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)

  # Pick the first (variable, class) pair
  first_var   <- as.character(out$variable[1])
  first_class <- as.character(out$class[1])
  k_idx       <- match(first_class, levels(vm$y))
  imp_col     <- paste0("imp.", k_idx)
  v_idx       <- match(first_var, bm$xvar.names)

  vals     <- bm$results[[imp_col]][bm$results$variable == v_idx]
  expected <- mean(abs(vals[is.finite(vals)]))
  expect_equal(out$beta_mean[1], expected, tolerance = 1e-10)
})

# ---- cutoff polymorphism ----------------------------------------------------

test_that("gg_beta_varpro cutoff = NULL gives per-class mean", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  prov <- attr(out, "provenance")
  expect_type(prov$cutoff, "double")
  expect_named(prov$cutoff, levels(iris$Species))
  for (cls in levels(iris$Species)) {
    expect_equal(
      prov$cutoff[[cls]],
      mean(out$beta_mean[out$class == cls]),
      tolerance = 1e-10
    )
  }
  expect_true(prov$cutoff_default)
})

test_that("gg_beta_varpro cutoff = scalar broadcasts across classes", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm, cutoff = 0.5)
  prov <- attr(out, "provenance")
  expect_equal(unname(prov$cutoff), rep(0.5, 3))
  expect_named(prov$cutoff, levels(iris$Species))
  expect_false(prov$cutoff_default)
})

test_that("gg_beta_varpro cutoff = named vector respects per-class, falls back to mean", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm,
                       cutoff = c("setosa" = 1.5))   # only one named; others default
  prov <- attr(out, "provenance")
  expect_equal(prov$cutoff[["setosa"]], 1.5)
  expect_equal(prov$cutoff[["versicolor"]],
               mean(out$beta_mean[out$class == "versicolor"]), tolerance = 1e-10)
})

test_that("gg_beta_varpro cutoff = named vector with unknown name errors", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  expect_error(
    gg_beta_varpro(vm, beta_fit = bm, cutoff = c("bogus" = 0.5)),
    "is not a level of the response"
  )
})

# ---- Phase 4c provenance shape update --------------------------------------

test_that("gg_beta_varpro regression cutoff is now a length-1 named numeric", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  out <- gg_beta_varpro(v, beta_fit = b)
  prov <- attr(out, "provenance")
  expect_named(prov$cutoff, "regr")
  expect_length(prov$cutoff, 1L)
  expect_equal(prov$cutoff[["regr"]], mean(out$beta_mean), tolerance = 1e-10)
})

# ---- which_class warning on regression --------------------------------------

test_that("gg_beta_varpro which_class on regression warns and is ignored", {
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  expect_warning(
    out <- gg_beta_varpro(v, beta_fit = b, which_class = "anything"),
    "ignored for regression family"
  )
  expect_s3_class(out, "gg_beta_varpro")
  expect_false("class" %in% names(out))
})
```

Also **find and modify** the Phase 4c default-cutoff test that currently asserts a scalar. Locate it (search for `expect_equal(prov\$cutoff, mean`) and update from:

```r
expect_equal(prov$cutoff, mean(out$beta_mean), tolerance = 1e-10)
```

to:

```r
expect_equal(prov$cutoff[["regr"]], mean(out$beta_mean), tolerance = 1e-10)
```

- [ ] **Step 2: Run tests to verify they fail**

```bash
R -q -e 'devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -30
```

Expected: new tests fail (no class path yet); the updated Phase 4c test fails (still expects scalar).

- [ ] **Step 3: Modify `R/gg_beta_varpro.R`** — drop family guard, add family dispatch + class path. Replace the body of `gg_beta_varpro.varpro` with:

```r
gg_beta_varpro.varpro <- function(object, ..., cutoff = NULL,
                                  beta_fit = NULL, which_class = NULL) {
  if (!inherits(object, "varpro")) {
    stop("gg_beta_varpro: expected a 'varpro' object from varPro::varpro().",
         call. = FALSE)
  }
  fam <- object$family
  if (!fam %in% c("regr", "class")) {
    stop(sprintf(
      paste0("gg_beta_varpro currently supports varpro regression and ",
             "classification forests only; got family = '%s'. regr+ and ",
             "survival are tracked under Phase 4d (see vignette / NEWS)."),
      fam
    ), call. = FALSE)
  }

  # Resolve beta_fit (cache path, unchanged from Phase 4c)
  if (is.null(beta_fit)) {
    b <- varPro::beta.varpro(object, ...)
  } else {
    required_cols <- c("tree", "branch", "variable", "n.oob", "imp")
    if (!inherits(beta_fit, "varpro") || !is.data.frame(beta_fit$results)) {
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

  # Warn on which_class with regression
  if (fam == "regr" && !is.null(which_class)) {
    warning("gg_beta_varpro: which_class ignored for regression family.",
            call. = FALSE)
    which_class <- NULL
  }

  # Empty fast-path
  if (is.null(b) || nrow(b$results) == 0L) {
    return(.gg_beta_varpro_empty(fam, which_class, beta_fit, cutoff))
  }

  if (fam == "regr") {
    return(.gg_beta_varpro_regr(object, b, cutoff, beta_fit))
  }
  .gg_beta_varpro_class(object, b, cutoff, which_class, beta_fit)
}
```

Then add the two internal builders (also in `R/gg_beta_varpro.R`):

```r
#' @keywords internal
.gg_beta_varpro_regr <- function(object, b, cutoff, beta_fit) {
  res <- b$results
  res <- res[is.finite(res$imp), , drop = FALSE]
  n_rules_total   <- nrow(b$results)
  n_rules_nonzero <- sum(abs(res$imp) > 0)

  var_name <- b$xvar.names[res$variable]
  beta_mean_v <- vapply(split(abs(res$imp), var_name), mean, numeric(1))
  n_rules_v   <- vapply(split(res$imp,        var_name), length, integer(1))

  # Factor levels = imp-descending
  ord_names <- names(sort(beta_mean_v, decreasing = TRUE))

  resolved_cutoff <- if (is.null(cutoff)) mean(beta_mean_v) else as.numeric(cutoff)

  out <- data.frame(
    variable  = factor(ord_names, levels = ord_names),
    beta_mean = unname(beta_mean_v[ord_names]),
    n_rules   = as.integer(unname(n_rules_v[ord_names])),
    stringsAsFactors = FALSE
  )
  out$selected <- out$beta_mean >= resolved_cutoff
  rownames(out) <- NULL

  class(out) <- c("gg_beta_varpro", "data.frame")
  attr(out, "provenance") <- list(
    source          = "varPro::beta.varpro",
    family          = "regr",
    ntree           = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff          = stats::setNames(resolved_cutoff, "regr"),
    cutoff_default  = is.null(cutoff),
    use.cv          = if (is.null(beta_fit)) isTRUE(list(...)$use.cv) else NA,
    n_rules_total   = n_rules_total,
    n_rules_nonzero = n_rules_nonzero,
    precomputed     = !is.null(beta_fit),
    xvar.names      = b$xvar.names
  )
  out
}

#' @keywords internal
.gg_beta_varpro_class <- function(object, b, cutoff, which_class, beta_fit) {
  # Class levels — derive from the varpro object's response
  class_levels <- if (is.factor(object$y)) {
    levels(object$y)
  } else if (!is.null(attr(object$y, "levels"))) {
    attr(object$y, "levels")
  } else {
    sort(unique(as.character(object$y)))
  }
  K <- length(class_levels)
  imp_cols <- paste0("imp.", seq_len(K))

  # Validate which_class
  if (!is.null(which_class)) {
    if (!which_class %in% class_levels) {
      stop(sprintf(
        "gg_beta_varpro: which_class = '%s' is not a level of the response. Levels: %s.",
        which_class, paste(class_levels, collapse = ", ")
      ), call. = FALSE)
    }
  } else if (K == 2L) {
    which_class <- class_levels[K]   # binary default = last (positive class)
  }

  res <- b$results
  var_name <- b$xvar.names[res$variable]

  # Total |imp| per variable (for factor-level ordering)
  res_total <- res[is.finite(res$imp), , drop = FALSE]
  total_var <- b$xvar.names[res_total$variable]
  beta_mean_total <- vapply(split(abs(res_total$imp), total_var), mean, numeric(1))
  ord_names <- names(sort(beta_mean_total, decreasing = TRUE))

  # Per-class aggregation — long format
  rows <- list()
  for (k in seq_len(K)) {
    col <- imp_cols[k]
    imp_k <- res[[col]]
    keep  <- is.finite(imp_k)
    if (!any(keep)) next
    sub_var <- var_name[keep]
    sub_imp <- imp_k[keep]
    beta_mean_k <- vapply(split(abs(sub_imp), sub_var), mean, numeric(1))
    n_rules_k   <- vapply(split(sub_imp,        sub_var), length, integer(1))
    vars_present <- intersect(ord_names, names(beta_mean_k))
    rows[[k]] <- data.frame(
      variable  = factor(vars_present, levels = ord_names),
      class     = class_levels[k],
      beta_mean = unname(beta_mean_k[vars_present]),
      n_rules   = as.integer(unname(n_rules_k[vars_present])),
      stringsAsFactors = FALSE
    )
  }
  long <- do.call(rbind, rows)

  # Resolve cutoff
  per_class_mean <- vapply(class_levels, function(cls) {
    vals <- long$beta_mean[long$class == cls]
    if (length(vals) == 0L) NA_real_ else mean(vals)
  }, numeric(1))
  names(per_class_mean) <- class_levels

  resolved_cutoff <- if (is.null(cutoff)) {
    per_class_mean
  } else if (is.null(names(cutoff))) {
    # scalar broadcast
    if (length(cutoff) != 1L) {
      stop("gg_beta_varpro: cutoff must be NULL, scalar, or a named vector with names in class levels.",
           call. = FALSE)
    }
    stats::setNames(rep(as.numeric(cutoff), K), class_levels)
  } else {
    bad <- setdiff(names(cutoff), class_levels)
    if (length(bad) > 0L) {
      stop(sprintf(
        "gg_beta_varpro: cutoff name(s) '%s' is not a level of the response. Levels: %s.",
        paste(bad, collapse = ", "),
        paste(class_levels, collapse = ", ")
      ), call. = FALSE)
    }
    cv <- per_class_mean
    cv[names(cutoff)] <- as.numeric(cutoff)
    cv
  }

  long$selected <- mapply(function(bm, cls) bm >= resolved_cutoff[[cls]],
                          long$beta_mean, long$class)

  # Filter to single class if requested
  if (!is.null(which_class)) {
    long <- long[long$class == which_class, , drop = FALSE]
  }

  # Sort: class factor order, then variable factor order
  long$class <- factor(long$class, levels = class_levels)
  long <- long[order(long$class, long$variable), , drop = FALSE]
  rownames(long) <- NULL

  class(long) <- c("gg_beta_varpro", "data.frame")
  attr(long, "provenance") <- list(
    source          = "varPro::beta.varpro",
    family          = "class",
    ntree           = if (!is.null(object$ntree)) as.integer(object$ntree) else NA_integer_,
    cutoff          = resolved_cutoff,
    cutoff_default  = is.null(cutoff),
    use.cv          = if (is.null(beta_fit)) isTRUE(list(...)$use.cv) else NA,
    n_rules_total   = nrow(b$results),
    n_rules_nonzero = sum(abs(res_total$imp) > 0, na.rm = TRUE),
    precomputed     = !is.null(beta_fit),
    xvar.names      = b$xvar.names,
    class_levels    = class_levels,
    which_class     = which_class
  )
  long
}

#' @keywords internal
.gg_beta_varpro_empty <- function(fam, which_class, beta_fit, cutoff) {
  base <- data.frame(
    variable  = factor(character(0)),
    beta_mean = numeric(0),
    n_rules   = integer(0),
    selected  = logical(0),
    stringsAsFactors = FALSE
  )
  if (fam == "class") {
    base <- cbind(base[, "variable", drop = FALSE],
                  class    = character(0),
                  base[, c("beta_mean", "n_rules", "selected"), drop = FALSE])
  }
  class(base) <- c("gg_beta_varpro", "data.frame")
  attr(base, "provenance") <- list(
    source = "varPro::beta.varpro", family = fam,
    n_rules_total = 0L,
    cutoff = if (fam == "regr") stats::setNames(NA_real_, "regr") else NA_real_,
    cutoff_default = is.null(cutoff),
    precomputed = !is.null(beta_fit),
    which_class = which_class
  )
  base
}
```

The `...` reference inside `.gg_beta_varpro_regr` / `_class` won't work directly (those are not the calling frame). To capture `use.cv` honestly, pass it through. Refactor: in the dispatcher, compute `dots_use_cv <- if (is.null(beta_fit)) isTRUE(list(...)$use.cv) else NA` BEFORE the dispatch, and pass it as an argument to `.gg_beta_varpro_regr` and `.gg_beta_varpro_class` (add `use_cv` parameter to both internals, and use it instead of `list(...)$use.cv`).

- [ ] **Step 4: Run tests to verify they pass**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -30
```

Expected: all classification tests + the Phase 4c update pass.

- [ ] **Step 5: Commit**

```bash
git add R/gg_beta_varpro.R tests/testthat/test_gg_beta_varpro.R
git commit -m "feat(gg_beta_varpro): family dispatch + class path (binary + multi-class)"
```

---

## Task 3: Plot — faceted branch + per-class cutoff lines

**Files:**
- Modify: `R/plot.gg_beta_varpro.R`
- Modify: `tests/testthat/test_gg_beta_varpro.R`

- [ ] **Step 1: Append failing tests**

Append:

```r
test_that("plot.gg_beta_varpro binary builds (single panel)", {
  vb <- .varpro_iris_binary()
  bb <- .beta_fit_iris_binary()
  out <- gg_beta_varpro(vb, beta_fit = bb)
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("plot.gg_beta_varpro multi-class builds (faceted) with per-class cutoff lines", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  # facet_wrap engaged → multiple panels in layout
  expect_true(nrow(built$layout$layout) >= 2L)
})
```

- [ ] **Step 2: Run to verify failure**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -10
```

Expected: facet test fails (current plot is single-panel only).

- [ ] **Step 3: Modify `R/plot.gg_beta_varpro.R`** to branch on `class` column.

Replace the body of `plot.gg_beta_varpro` (after the `nrow == 0` guard) with:

```r
  prov          <- attr(x, "provenance")
  n_rules_total <- if (!is.null(prov)) prov$n_rules_total %||% NA_integer_ else NA_integer_
  cv_txt        <- if (!is.null(prov) && isTRUE(prov$use.cv)) {
    "cv"
  } else if (!is.null(prov) && length(prov$use.cv) == 1L && is.na(prov$use.cv)) {
    "unknown (precomputed)"
  } else {
    "fixed"
  }
  has_class <- "class" %in% names(x)

  # Per-class cutoff vector (or scalar wrapped)
  cutoff_vec <- if (!is.null(prov) && !is.null(prov$cutoff)) {
    prov$cutoff
  } else if (has_class) {
    stats::setNames(vapply(split(x$beta_mean, x$class), mean, numeric(1)),
                    levels(x$class))
  } else {
    stats::setNames(mean(x$beta_mean), "regr")
  }

  p <- ggplot2::ggplot(
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
    ggplot2::labs(
      x = NULL,
      y = "Mean |beta| (per-rule lasso)",
      caption = sprintf(
        "Mean |beta| over %s rules. Lasso: %s. %s",
        if (is.na(n_rules_total)) "NA" else format(n_rules_total),
        cv_txt,
        if (has_class) {
          n_panels <- length(unique(x$class))
          if (n_panels == 1L) sprintf("Class: %s.", as.character(x$class[1]))
          else sprintf("%d classes (faceted).", n_panels)
        } else {
          sprintf("Cutoff: %.4g.", cutoff_vec[[1]])
        }
      )
    ) +
    ggplot2::theme_minimal()

  if (has_class && length(unique(x$class)) > 1L) {
    # Per-class cutoff lines via data join
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
    # Single panel (regression or single-class) — one horizontal line
    cutoff_scalar <- if (has_class) cutoff_vec[[as.character(x$class[1])]] else cutoff_vec[[1]]
    p <- p + ggplot2::geom_hline(
      yintercept = cutoff_scalar,
      linetype   = "dashed",
      color      = "#e74c3c",
      linewidth  = 0.7
    )
  }
  p
```

- [ ] **Step 4: Run tests to verify they pass**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -10
```

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_beta_varpro.R tests/testthat/test_gg_beta_varpro.R
git commit -m "feat(plot.gg_beta_varpro): faceted view with per-class cutoff lines"
```

---

## Task 4: print + summary extensions

**Files:**
- Modify: `R/print_methods.R`
- Modify: `R/summary_methods.R`
- Modify: `tests/testthat/test_gg_beta_varpro.R`

- [ ] **Step 1: Append failing tests**

Append:

```r
test_that("print.gg_beta_varpro classification surfaces class info", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  txt <- utils::capture.output(print(out))
  expect_true(any(grepl("n_classes", txt) | grepl("class", txt)))
  expect_true(any(grepl("faceted", txt)))
})

test_that("summary.gg_beta_varpro classification returns per-class list", {
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  out <- gg_beta_varpro(vm, beta_fit = bm)
  s <- summary(out)
  expect_s3_class(s, "summary.gg_beta_varpro")
  expect_true(is.list(unclass(s)))
  expect_setequal(names(unclass(s)), levels(iris$Species))
})
```

- [ ] **Step 2: Run to verify failure**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -10
```

- [ ] **Step 3: Extend `print.gg_beta_varpro`** in `R/print_methods.R`.

Replace the existing `print.gg_beta_varpro` body with:

```r
print.gg_beta_varpro <- function(x, ...) {
  prov           <- attr(x, "provenance")
  family         <- if (!is.null(prov)) prov$family %||% NA_character_ else NA_character_
  precomputed    <- isTRUE(if (!is.null(prov)) prov$precomputed else FALSE)
  n_total        <- if (!is.null(prov)) prov$n_rules_total  %||% NA_integer_ else NA_integer_
  n_nonzero      <- if (!is.null(prov)) prov$n_rules_nonzero %||% NA_integer_ else NA_integer_
  n_sel          <- sum(x$selected, na.rm = TRUE)

  if (identical(family, "class") && "class" %in% names(x)) {
    n_classes   <- length(unique(x$class))
    which_class <- if (!is.null(prov)) prov$which_class else NULL
    view_str <- if (is.null(which_class)) "faceted" else sprintf("which_class: %s", which_class)
    cat(.gg_header(x, "gg_beta_varpro"),
        sprintf("  |  n_classes: %d  |  view: %s", n_classes, view_str),
        sprintf("  |  precomputed: %s", precomputed),
        "\n",
        sprintf("  %d of %d (variable, class) pairs selected; %d / %d rules with non-zero beta\n",
                n_sel, nrow(x), n_nonzero, n_total),
        sep = "")
  } else {
    cutoff         <- if (!is.null(prov)) prov$cutoff %||% NA_real_ else NA_real_
    cutoff_val     <- if (length(cutoff) >= 1L) cutoff[[1]] else NA_real_
    cutoff_default <- isTRUE(if (!is.null(prov)) prov$cutoff_default else FALSE)
    cat(.gg_header(x, "gg_beta_varpro"),
        sprintf("  |  cutoff: %.4g%s", cutoff_val,
                if (cutoff_default) " (default)" else ""),
        sprintf("  |  precomputed: %s", precomputed),
        "\n",
        sprintf("  %d of %d variables selected; %d / %d rules with non-zero beta\n",
                n_sel, nrow(x), n_nonzero, n_total),
        sep = "")
  }
  invisible(x)
}
```

- [ ] **Step 4: Extend `summary.gg_beta_varpro`** in `R/summary_methods.R`.

Replace the existing body with:

```r
summary.gg_beta_varpro <- function(object, ...) {
  prov   <- attr(object, "provenance")
  family <- if (!is.null(prov)) prov$family %||% "regr" else "regr"

  if (identical(family, "class") && "class" %in% names(object)) {
    per_class <- split(object, object$class, drop = FALSE)
    by_class  <- lapply(per_class, function(df) {
      v <- df$beta_mean
      names(v) <- as.character(df$variable)
      v <- sort(v, decreasing = TRUE)
      structure(v,
                n_rules = stats::setNames(df$n_rules, as.character(df$variable))[names(v)])
    })
    structure(by_class, class = "summary.gg_beta_varpro")
  } else {
    v <- object$beta_mean
    names(v) <- as.character(object$variable)
    v <- sort(v, decreasing = TRUE)
    structure(v,
              n_rules = stats::setNames(object$n_rules,
                                        as.character(object$variable))[names(v)],
              class   = "summary.gg_beta_varpro")
  }
}
```

Also update `print.summary.gg_beta_varpro` in `R/print_methods.R` to handle both shapes:

```r
print.summary.gg_beta_varpro <- function(x, ...) {
  if (is.list(unclass(x)) && !is.numeric(unclass(x))) {
    # Classification: list of per-class summaries
    for (cls in names(x)) {
      cat(sprintf("Class '%s' — mean |beta| per variable (descending):\n", cls))
      print(unclass(x[[cls]]))
      cat("\nRule counts:\n")
      print(attr(x[[cls]], "n_rules"))
      cat("\n")
    }
  } else {
    cat("Mean |beta| per variable (descending):\n")
    print(unclass(x))
    cat("\nRule counts:\n")
    print(attr(x, "n_rules"))
  }
  invisible(x)
}
```

- [ ] **Step 5: Run tests**

```bash
R -q -e 'devtools::load_all(quiet = TRUE); devtools::test(filter = "gg_beta_varpro", reporter = "summary")' 2>&1 | tail -10
```

- [ ] **Step 6: Commit**

```bash
git add R/print_methods.R R/summary_methods.R tests/testthat/test_gg_beta_varpro.R
git commit -m "feat(gg_beta_varpro): print + summary handle classification family"
```

---

## Task 5: Roxygen — `@section Classification` + scale-caveat updates

**Files:**
- Modify: `R/gg_beta_varpro.R` (roxygen)
- Modify: `R/plot.gg_beta_varpro.R` (roxygen)

- [ ] **Step 1: Append a `@section Classification:` block to `gg_beta_varpro` roxygen**, after the existing `@section Caching:` block:

```r
#' @section Classification:
#' For a varpro classification fit (`object$family == "class"`,
#' binary or multi-class), the returned frame is long-format with an
#' extra `class` column: one row per (variable, class) pair. The
#' `beta_mean` column aggregates the **per-class lasso β̂** stored in
#' `beta.varpro()`'s `imp.<k>` columns (one per class level). Same
#' pedantic-β semantics as regression, applied independently to each
#' class.
#'
#' **Binary default**: `which_class = NULL` resolves to the *last*
#' factor level of the response — the positive-class convention used
#' by `glm` and `gg_roc`. For a 30-day-mortality outcome with levels
#' `c("no", "yes")`, that means the wrapper shows you `"yes"` (the
#' event) by default.
#'
#' **Multi-class default**: `which_class = NULL` returns all K
#' classes; the plot method renders `facet_wrap(~ class)` with one
#' cutoff line per facet.
#'
#' **`which_class = "<name>"`** filters to a single class regardless
#' of K. Errors if the name isn't in the response levels.
#'
#' **Per-class cutoffs**: `cutoff = NULL` resolves to each class's
#' `mean(beta_mean)`. A scalar broadcasts. A named numeric vector
#' overrides per class; missing names fall back to that class's mean.
#'
#' Example (30-day mortality, binary):
#' ```r
#' fit <- varPro::varpro(event_30d ~ ., data = clinical, ntree = 200)
#' gg  <- gg_beta_varpro(fit)   # default: "yes" panel
#' plot(gg)
#' ```
```

- [ ] **Step 2: Update `plot.gg_beta_varpro` "Reading the chart" section** to mention factor-level alignment across facets and per-class cutoff lines. Add to the existing `@section Reading the chart:` paragraph (or append a new sentence):

```
For a classification fit, variables are sorted by mean(|sum-of-class-beta|)
descending and that ordering is shared across every facet, so rows line up
between classes for visual comparison. Each facet has its own cutoff line.
```

- [ ] **Step 3: Add Reproducibility note already exists from PR #97; no change needed.**

- [ ] **Step 4: Regenerate documentation**

```bash
R -q -e 'devtools::document(quiet = TRUE)' 2>&1 | tail -10
```

- [ ] **Step 5: Confirm R CMD check stays 0/0/0**

```bash
R -q -e 'devtools::check(args = c("--no-manual", "--as-cran"), quiet = TRUE)' 2>&1 | tail -10
```

- [ ] **Step 6: Commit**

```bash
git add R/gg_beta_varpro.R R/plot.gg_beta_varpro.R man/ NAMESPACE
git commit -m "docs(gg_beta_varpro): @section Classification + per-class scale-caveat"
```

---

## Task 6: vdiffr snapshots

**Files:**
- Modify: `tests/testthat/test_snapshots.R`

- [ ] **Step 1: Append two snapshot blocks**

```r
test_that("gg-beta-varpro-class-binary", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  vb <- .varpro_iris_binary()
  bb <- .beta_fit_iris_binary()
  p <- plot(gg_beta_varpro(vb, beta_fit = bb))
  vdiffr::expect_doppelganger("gg-beta-varpro-class-binary", p)
})

test_that("gg-beta-varpro-class-multiclass", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  p <- plot(gg_beta_varpro(vm, beta_fit = bm))
  vdiffr::expect_doppelganger("gg-beta-varpro-class-multiclass", p)
})
```

- [ ] **Step 2: Record baselines**

```bash
VDIFFR_RUN_TESTS=true R -q -e 'devtools::test(filter = "snapshots", reporter = "summary")' 2>&1 | tail -15
```

Expected: two SVGs added under `tests/testthat/_snaps/snapshots/`.

- [ ] **Step 3: Verify CRAN-safe SKIP without env var**

```bash
R -q -e 'devtools::test(filter = "snapshots", reporter = "summary")' 2>&1 | tail -10
```

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test_snapshots.R tests/testthat/_snaps/snapshots/gg-beta-varpro-class-binary.svg tests/testthat/_snaps/snapshots/gg-beta-varpro-class-multiclass.svg
git commit -m "test(snapshots): vdiffr baselines for gg_beta_varpro classification"
```

---

## Task 7: Final NEWS, R CMD check, push, PR

**Files:**
- Modify: `NEWS.md` (refine the placeholder bullet from T0)

- [ ] **Step 1: Refine NEWS bullet**

Replace the Task 0 placeholder with:

```
* `gg_beta_varpro()` extended to varPro classification forests (binary
  and multi-class). Family auto-dispatches: `regr` keeps the Phase 4c
  regression path; `class` produces a long-format frame with a `class`
  column (one row per variable × class). Binary fits default to a
  single panel for the *last factor level* (positive-class
  convention, matching `glm` and `gg_roc`); multi-class fits render
  as `facet_wrap(~ class)` with per-class cutoff lines. New
  `which_class` argument selects a single class on either family.
  New `cutoff` polymorphism: `NULL` → per-class mean(beta_mean);
  scalar → broadcast; named numeric vector → per-class override with
  fallback. Variables are stored as a factor whose levels are set by
  `mean(|sum-across-classes-beta|)` descending so every facet shows
  rows in the same order. Motivating use case: 30-day mortality.
* Provenance shape: `attr(*, "provenance")$cutoff` is now always a
  named numeric vector (length 1 named `"regr"` for regression;
  length K named with class levels for classification). Downstream
  tooling should read it as a vector and select by name; the prior
  scalar shape is gone.
```

- [ ] **Step 2: Full check + test passes (both env-var conditions)**

```bash
R -q -e 'devtools::check(args = c("--no-manual", "--as-cran"), quiet = TRUE)' 2>&1 | tail -10
R -q -e 'devtools::test(reporter = "summary")' 2>&1 | tail -15
GG_BETA_VARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true R -q -e 'devtools::test(reporter = "summary")' 2>&1 | tail -15
```

All three must report 0 failures / 0 errors / 0 warnings.

- [ ] **Step 3: Commit + push + open PR**

```bash
git add NEWS.md
git commit -m "docs(NEWS): finalize gg_beta_varpro classification v2.7.3.9012 entry"

git push -u origin feat/varpro-phase4c-classification

gh pr create --title "varPro Phase 4c-classification: gg_beta_varpro for class family" --body "$(cat <<'EOF'
## Summary
- `gg_beta_varpro()` extended to varPro classification (binary + multi-class) via family auto-dispatch.
- Binary fits default to a single panel for the last factor level (positive-class convention; motivating use case: **30-day mortality**); multi-class fits return a long-format frame and plot as `facet_wrap(~ class)` with per-class cutoff lines.
- New `which_class` argument; `cutoff` becomes polymorphic (scalar / named vector / NULL).
- Variables stored as a factor whose levels are set by `mean(|sum-of-class-beta|)` descending, so every facet shows rows in the same order. Shared package-wide convention going forward (consistency follow-up for `gg_vimp` / `plot.gg_varpro(conditional = TRUE)` tracked separately).
- Provenance: `prov\$cutoff` is now always a named numeric vector (length 1 for regression, length K for classification). Small breaking change in provenance shape; Phase 4c regression test updated.

Spec: `dev/plans/2026-05-26-varpro-phase4-gg-beta-varpro-classification-design.md`
Plan: `dev/plans/2026-05-26-varpro-phase4-gg-beta-varpro-classification-plan.md`

## Test plan
- [ ] `devtools::check(--as-cran)` 0/0/0
- [ ] `devtools::test()` 0 failures
- [ ] `GG_BETA_VARPRO_SLOW_TESTS=true VDIFFR_RUN_TESTS=true devtools::test()` 0 failures, both snapshots build
- [ ] pkgdown reference pages render

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Return the PR URL.

---

## Self-review notes

**Spec coverage:** all 16 spec tests are mapped (T2: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11; T2 also covers regression provenance update via the new assertion + Phase 4c test edit; T3: 15; T4: 16 + print; family-guard sanity covered by the existing Phase 4c family-guard test which now allows class). The provenance shape alignment is implemented in `.gg_beta_varpro_regr` and tested. The `which_class` regression-warning case is tested. The cache path on classification piggybacks on the existing Phase 4c shape-guard tests (since the same `beta_fit` validation block runs); a dedicated cache test is not explicitly added, which is acceptable because the cache mechanics are not family-specific.

**Type consistency:** the tidy frame `variable` is a factor in every path. `class` column appears only on classification. `cutoff` is always a named numeric vector going forward (length 1 for regr, length K for class). Provenance fields are consistent across families except for `class_levels` / `which_class` (classification-only).

**Placeholders:** none.

**Out-of-scope follow-ups flagged in spec, not in this plan:**
- `gg_vimp` / `plot.gg_varpro(conditional = TRUE)` factor-level alignment.
- `regr+` / `surv` family support.
- `which_class` as vector subset.
- Per-class faithful overlay.
