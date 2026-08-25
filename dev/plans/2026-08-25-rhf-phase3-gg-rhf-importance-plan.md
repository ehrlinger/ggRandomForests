# RHF Phase 3 (`gg_rhf_importance`) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a tidy extractor and ggplot2 point-matrix display for time-localized Random Hazard Forest variable priority.

**Architecture:** `gg_rhf_importance.rhf()` accepts a fitted RHF object and either computes `randomForestRHF::importance.rhf()` or reuses a supplied `importance_fit`. It validates the upstream result, maps `importance.long` to a stable snake-case data-frame contract, and orders variables by q90 priority. Plot, print, summary, and autoplot methods consume that object without retaining or recomputing the upstream cache.

**Tech Stack:** R 4.4+, S3, ggplot2, testthat edition 2, vdiffr, roxygen2 markdown, `randomForestRHF` 1.0.1.

**Spec:** `dev/plans/2026-08-25-rhf-phase3-gg-rhf-importance-design.md`

## Global Constraints

- Work from `dev_rhf`; merge Phase 3 back through a PR, never push directly to `main`.
- Keep `randomForestRHF` in `Suggests` and require version `>= 1.0.1`; never attach it from `R/`.
- The public value is `priority`, not `z`; do not add a `selected` field or a `0.79` cutoff.
- Returned data retain original upstream priority values. Transformations and caps are display-only.
- Rows stay chronological by window and descending within window; factor levels alone carry the q90 plot order.
- The most important variable is the last factor level so it appears at the top of the plot.
- Every plot/autoplot method returns an object and never calls `print()`.
- Every RNG-using `test_that()` block calls `set.seed()` inside that block.
- Slow tests call `skip_on_cran()` and RHF tests call `skip_if_not_installed("randomForestRHF")`.
- Do not hand-edit `NAMESPACE` or `man/`; regenerate them with `devtools::document()`.
- Keep the package version at `4.0.0`; update both version files only if the maintainer separately requests a patch bump.
- The Phase 5 RHF vignette is a mandatory release gate. Phase 3 adds its bibliography entries but does not build the vignette.
- Phase 3 may merge into `dev_rhf`, but it does not authorize a v4 release PR, tag, GitHub Release, or CRAN submission.
- The CRAN submission remains on hold until the maintainer explicitly lifts it; v4 is not released or tagged until CRAN accepts it.

---

## File Structure

- Create `R/gg_rhf_importance.R`: generic, RHF method, validation, tidy conversion, q90 summary helper.
- Create `R/plot.gg_rhf_importance.R`: variable filtering, display transforms/caps, point-matrix ggplot.
- Modify `R/print_methods.R`, `R/summary_methods.R`, `R/autoplot_methods.R`: S3 companions.
- Modify `tests/testthat/helper-rhf-fixtures.R`: memoized real cache and four-window importance result.
- Create `tests/testthat/test_gg_rhf_importance.R`: extractor, validation, provenance, summary, and integration tests.
- Create `tests/testthat/test_plot_gg_rhf_importance.R`: plot filtering, transform, capping, and error tests.
- Modify `tests/testthat/test_plot_conventions.R`: cross-family top-order contract.
- Modify `tests/testthat/test_snapshots.R`: one RHF priority point-matrix baseline.
- Modify `DESCRIPTION`, `NEWS.md`, `_pkgdown.yml`, `R/gg_rhf.R`, `R/gg_auct.R`, `vignettes/ggRandomForests.bib`, and the May umbrella design: dependency floor, citations, release note, and corrected Phase 3 contract.
- Generate `NAMESPACE`, `man/*.Rd`, and the new vdiffr SVG.

---

### Task 1: Dependency floor and deterministic RHF importance fixture

**Files:**
- Modify: `DESCRIPTION`
- Modify: `tests/testthat/helper-rhf-fixtures.R`

**Interfaces:**
- Consumes: `.rhf_pbc()` and `.rhf_cache` from the existing RHF fixture file.
- Produces: `.rhf_importance_cache_pbc()` returning `varpro.cache.rhf`; `.rhf_importance_pbc()` returning a four-window `importance.rhf`; `.rhf_importance_indices()` returning the fixed window indices; `.fake_rhf_importance()` returning aligned synthetic `rhf` and `importance.rhf` objects for fast unit tests.

- [ ] **Step 1: Pin the suggested-package floor.** Change the existing line in `DESCRIPTION` to:

```text
    randomForestRHF (>= 1.0.1),
```

- [ ] **Step 2: Add the memoized fixture functions.** Append:

```r
.rhf_importance_cache_pbc <- function() {
  if (is.null(.rhf_cache$importance_cache)) {
    o <- .rhf_pbc()
    set.seed(20260825L)
    .rhf_cache$importance_cache <- randomForestRHF::varpro.cache.rhf(
      o, max.rules.tree = 30L, max.tree = 12L
    )
  }
  .rhf_cache$importance_cache
}

.rhf_importance_indices <- function() {
  cache <- .rhf_importance_cache_pbc()
  unique(pmax(1L, round(seq(1L, cache$K, length.out = 4L))))
}

.rhf_importance_pbc <- function() {
  if (is.null(.rhf_cache$importance)) {
    o <- .rhf_pbc()
    cache <- .rhf_importance_cache_pbc()
    set.seed(20260825L)
    .rhf_cache$importance <- randomForestRHF::importance.rhf(
      o,
      cache = cache,
      time.index = .rhf_importance_indices()
    )
  }
  .rhf_cache$importance
}

.fake_rhf_importance <- function() {
  object <- structure(list(
    xvar.names = c("x1", "x2", "x3"),
    time.interest = c(1, 2),
    family = "surv", ntree = 10L, n = 12L
  ), class = "rhf")
  mat <- matrix(c(0.2, 0.8, 0.1, 1.2, 0.4, 0.3), nrow = 3L,
                dimnames = list(object$xvar.names, c("1", "2")))
  win <- data.frame(
    index = 1:2, time = 1:2, start = c(0, 1), stop = 1:2,
    midpoint = c(0.5, 1.5), n.risk = c(12L, 8L),
    n.rules = c(20L, 15L), label = c("(0, 1]", "(1, 2]")
  )
  long <- data.frame(
    variable = rep(rownames(mat), times = 2L),
    time = rep(win$time, each = 3L), time.index = rep(win$index, each = 3L),
    window = rep(win$label, each = 3L), start = rep(win$start, each = 3L),
    stop = rep(win$stop, each = 3L), midpoint = rep(win$midpoint, each = 3L),
    n.risk = rep(win$n.risk, each = 3L), n.rules = rep(win$n.rules, each = 3L),
    importance = as.vector(mat)
  )
  long <- long[order(long$time.index, -long$importance, long$variable), ]
  fit <- structure(list(
    xvar.names = object$xvar.names, importance.matrix = mat,
    importance.long = long, window.info = win,
    y.source = "int.haz.oob", trim = 0.1
  ), class = "importance.rhf")
  list(object = object, fit = fit)
}
```

- [ ] **Step 3: Verify the fixture against CRAN 1.0.1.**

Run:

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); source("tests/testthat/helper-rhf-fixtures.R"); x <- .rhf_importance_pbc(); stopifnot(inherits(x, "importance.rhf"), nrow(x$window.info) == 4L, identical(names(x$importance.long), c("variable", "time", "time.index", "window", "start", "stop", "midpoint", "n.risk", "n.rules", "importance")))'
```

Expected: exit 0 with no output.

- [ ] **Step 4: Commit the dependency and fixture.**

```bash
git add DESCRIPTION tests/testthat/helper-rhf-fixtures.R
git commit -m "test: add RHF priority fixtures"
```

---

### Task 2: Tidy extractor and strict upstream validation

**Files:**
- Create: `R/gg_rhf_importance.R`
- Create: `tests/testthat/test_gg_rhf_importance.R`

**Interfaces:**
- Consumes: `randomForestRHF::importance.rhf()`, `.set_provenance()`, `%||%`, and Task 1 fixtures.
- Produces: `gg_rhf_importance()`; `gg_rhf_importance.rhf(object, importance_fit = NULL, cache = NULL, time.index = NULL, ...)`; `.validate_rhf_importance_fit()`; `.rhf_priority_summary()`; class `c("gg_rhf_importance", "data.frame")`.

- [ ] **Step 1: Write precomputed-path tests using `.fake_rhf_importance()` from Task 1.** Start `tests/testthat/test_gg_rhf_importance.R` with:

```r
test_that("gg_rhf_importance tidies a supplied priority result", {
  f <- .fake_rhf_importance()
  x <- gg_rhf_importance(f$object, importance_fit = f$fit)
  expect_s3_class(x, "gg_rhf_importance")
  expect_identical(names(x), c(
    "variable", "time_window", "time", "time_index", "start", "stop",
    "midpoint", "n_risk", "n_rules", "priority"
  ))
  expect_equal(x$priority, f$fit$importance.long$importance)
  expect_false(any(c("z", "selected") %in% names(x)))
  expect_equal(tail(levels(x$variable), 1L), "x1")
  expect_true(attr(x, "provenance")$precomputed)
})

test_that("precomputed and calculation-only arguments cannot be mixed", {
  f <- .fake_rhf_importance()
  expect_error(
    gg_rhf_importance(f$object, importance_fit = f$fit, time.index = 1L),
    "calculation arguments"
  )
})
```

- [ ] **Step 2: Run the new tests red.**

Run:

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_gg_rhf_importance.R")'
```

Expected: failure because `gg_rhf_importance()` does not exist.

- [ ] **Step 3: Implement the generic, method, conversion, and q90 ordering.** The executable core in `R/gg_rhf_importance.R` is:

```r
gg_rhf_importance <- function(object, ...) {
  UseMethod("gg_rhf_importance", object)
}

gg_rhf_importance.rhf <- function(object, importance_fit = NULL, cache = NULL,
                                  time.index = NULL, ...) {
  if (!inherits(object, "rhf")) {
    stop("gg_rhf_importance() only works on 'rhf' objects from randomForestRHF.",
         call. = FALSE)
  }
  dots <- list(...)
  precomputed <- !is.null(importance_fit)
  if (precomputed && (!is.null(cache) || !is.null(time.index) || length(dots))) {
    stop("Do not supply calculation arguments with 'importance_fit'.", call. = FALSE)
  }
  if (!precomputed) {
    if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
      stop("Install the 'randomForestRHF' package to use gg_rhf_importance(): install.packages('randomForestRHF')",
           call. = FALSE)
    }
    importance_fit <- do.call(
      randomForestRHF::importance.rhf,
      c(list(o = object, cache = cache, time.index = time.index), dots)
    )
  }
  .validate_rhf_importance_fit(object, importance_fit)
  d <- importance_fit$importance.long
  out <- data.frame(
    variable = as.character(d$variable),
    time_window = as.character(d$window),
    time = as.numeric(d$time), time_index = as.integer(d$time.index),
    start = as.numeric(d$start), stop = as.numeric(d$stop),
    midpoint = as.numeric(d$midpoint), n_risk = as.integer(d$n.risk),
    n_rules = as.integer(d$n.rules), priority = as.numeric(d$importance),
    stringsAsFactors = FALSE
  )
  rank <- .rhf_priority_summary(out)
  out$variable <- factor(out$variable, levels = rev(rank$variable))
  class(out) <- c("gg_rhf_importance", class(out))
  out <- .set_provenance(out, object)
  prov <- attr(out, "provenance") %||% list()
  prov$precomputed <- precomputed
  prov$y_source <- importance_fit$y.source %||% NA_character_
  prov$trim <- importance_fit$trim %||% NA_real_
  prov$n_windows <- nrow(importance_fit$window.info)
  prov$rank_by <- "q90"
  prov$randomForestRHF_version <- if (requireNamespace("randomForestRHF", quietly = TRUE)) {
    as.character(utils::packageVersion("randomForestRHF"))
  } else {
    NA_character_
  }
  attr(out, "provenance") <- prov
  invisible(out)
}

.rhf_priority_summary <- function(x) {
  by_var <- split(x$priority, as.character(x$variable))
  stat <- function(v, fun) {
    v <- v[is.finite(v)]
    if (length(v)) fun(v) else NA_real_
  }
  out <- data.frame(
    variable = names(by_var),
    q90 = vapply(by_var, stat, numeric(1), fun = function(v) {
      unname(stats::quantile(v, 0.9, names = FALSE))
    }),
    median = vapply(by_var, stat, numeric(1), fun = stats::median),
    mean = vapply(by_var, stat, numeric(1), fun = mean),
    max = vapply(by_var, stat, numeric(1), fun = max),
    n_windows = vapply(names(by_var), function(v) {
      length(unique(x$time_index[as.character(x$variable) == v]))
    }, integer(1)),
    n_finite = vapply(by_var, function(v) sum(is.finite(v)), integer(1))
  )
  out[order(-out$q90, -out$median, -out$max, out$variable, na.last = TRUE), ]
}
```

Add complete roxygen for the signature, ten returned columns, precomputed-first example, interpretation, both references, and `@export` tags. Keep narrative wording aligned with the approved spec and `.claude/house-style.md`.

- [ ] **Step 4: Implement `.validate_rhf_importance_fit()`.** Use this validation sequence so malformed saved objects fail before conversion:

```r
.validate_rhf_importance_fit <- function(object, fit) {
  if (!inherits(fit, "importance.rhf")) {
    stop("'importance_fit' must inherit from 'importance.rhf'.", call. = FALSE)
  }
  if (!identical(as.character(fit$xvar.names), as.character(object$xvar.names))) {
    stop("'importance_fit$xvar.names' do not match the RHF object.", call. = FALSE)
  }
  mat <- fit$importance.matrix
  if (!is.matrix(mat) || !is.numeric(mat) || !length(mat) ||
      !identical(rownames(mat), fit$xvar.names)) {
    stop("'importance_fit$importance.matrix' is malformed.", call. = FALSE)
  }
  win <- fit$window.info
  win_names <- c("index", "time", "start", "stop", "midpoint",
                 "n.risk", "n.rules", "label")
  if (!is.data.frame(win) || !all(win_names %in% names(win)) ||
      nrow(win) != ncol(mat)) {
    stop("'importance_fit$window.info' does not align with the matrix.",
         call. = FALSE)
  }
  if (any(!is.finite(win$index)) || any(win$index < 1L) ||
      any(win$index > length(object$time.interest)) ||
      !isTRUE(all.equal(as.numeric(win$time),
                        as.numeric(object$time.interest[win$index]),
                        tolerance = sqrt(.Machine$double.eps)))) {
    stop("'importance_fit$window.info' does not match object$time.interest.",
         call. = FALSE)
  }
  long <- fit$importance.long
  long_names <- c("variable", "time", "time.index", "window", "start",
                  "stop", "midpoint", "n.risk", "n.rules", "importance")
  if (!is.data.frame(long) || !all(long_names %in% names(long)) ||
      nrow(long) != length(mat)) {
    stop("'importance_fit$importance.long' is malformed.", call. = FALSE)
  }
  row_index <- match(long$variable, rownames(mat))
  col_index <- match(long$time.index, win$index)
  key <- paste(long$variable, long$time.index, sep = "\r")
  if (anyNA(row_index) || anyNA(col_index) ||
      length(unique(key)) != length(mat)) {
    stop("'importance_fit$importance.long' has unknown variables or windows.",
         call. = FALSE)
  }
  expected <- mat[cbind(row_index, col_index)]
  if (!isTRUE(all.equal(as.numeric(long$importance), as.numeric(expected),
                        tolerance = sqrt(.Machine$double.eps)))) {
    stop("'importance_fit$importance.long' does not match importance.matrix.",
         call. = FALSE)
  }
  win_row <- match(long$time.index, win$index)
  metadata_pairs <- list(
    c("time", "time"), c("window", "label"), c("start", "start"),
    c("stop", "stop"), c("midpoint", "midpoint"),
    c("n.risk", "n.risk"), c("n.rules", "n.rules")
  )
  metadata_ok <- vapply(metadata_pairs, function(pair) {
    isTRUE(all.equal(long[[pair[1L]]], win[[pair[2L]]][win_row],
                     tolerance = sqrt(.Machine$double.eps),
                     check.attributes = FALSE))
  }, logical(1))
  if (!all(metadata_ok)) {
    stop("'importance_fit$importance.long' does not match window.info.",
         call. = FALSE)
  }
  if (any(long$importance[is.finite(long$importance)] < 0)) {
    stop("'importance_fit$importance.long' contains negative priority values.",
         call. = FALSE)
  }
  invisible(TRUE)
}
```

- [ ] **Step 5: Add malformed-shape, mismatch, and real integration tests.** Include mutations of the synthetic fit for every validator branch. Add this compute-path comparison:

```r
test_that("computed and precomputed paths return the same priority frame", {
  skip_on_cran()
  skip_if_not_installed("randomForestRHF")
  set.seed(20260825L)
  o <- .rhf_pbc()
  fit <- .rhf_importance_pbc()
  cached <- gg_rhf_importance(o, importance_fit = fit)
  computed <- gg_rhf_importance(
    o,
    cache = .rhf_importance_cache_pbc(),
    time.index = .rhf_importance_indices()
  )
  expect_equal(as.data.frame(computed), as.data.frame(cached))
  expect_false(attr(computed, "provenance")$precomputed)
})
```

- [ ] **Step 6: Run extractor tests green.**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_gg_rhf_importance.R")'
```

Expected: 0 failures, 0 errors, no skipped RHF integration blocks.

- [ ] **Step 7: Commit the extractor.**

```bash
git add R/gg_rhf_importance.R tests/testthat/test_gg_rhf_importance.R
git commit -m "feat: add RHF variable-priority extractor"
```

---

### Task 3: Point-matrix plot with display-only controls

**Files:**
- Create: `R/plot.gg_rhf_importance.R`
- Create: `tests/testthat/test_plot_gg_rhf_importance.R`

**Interfaces:**
- Consumes: `gg_rhf_importance` data frame and its factor ordering.
- Produces: `plot.gg_rhf_importance(x, vars = NULL, top_n_union = 15L, transform = c("none", "log10"), size_cap = 0.99, color_cap = 0.99, display_note = TRUE, ...)`; internal `.rhf_priority_plot_data()` and `.rhf_priority_cap()`.

- [ ] **Step 1: Write failing plot tests.** Test a `GeomPoint` layer, q90 top-at-top factor order, explicit-variable filtering, per-window top union, `top_n_union = NULL`, log10 display values, cap captions without mutation, zeros retained, NAs omitted from the layer, unknown variables, invalid controls, and no-finite-data errors. Build each test from `.fake_rhf_importance()` and `gg_rhf_importance()`.

```r
test_that("plot.gg_rhf_importance returns the published point-matrix shape", {
  f <- .fake_rhf_importance()
  x <- gg_rhf_importance(f$object, importance_fit = f$fit)
  original <- x$priority
  p <- plot(x, top_n_union = NULL)
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(layer) class(layer$geom)[1L], character(1))
  expect_true("GeomPoint" %in% geoms)
  expect_equal(x$priority, original)
  expect_equal(p$labels$size, "RHF variable priority")
})
```

- [ ] **Step 2: Run plot tests red.**

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_plot_gg_rhf_importance.R")'
```

Expected: failure because `plot.gg_rhf_importance()` does not exist.

- [ ] **Step 3: Implement filtering and point-matrix rendering.** The renderer must:

```r
plot.gg_rhf_importance <- function(x, vars = NULL, top_n_union = 15L,
                                   transform = c("none", "log10"),
                                   size_cap = 0.99, color_cap = 0.99,
                                   display_note = TRUE, ...) {
  if (!inherits(x, "gg_rhf_importance")) {
    stop("plot.gg_rhf_importance() requires a 'gg_rhf_importance' object.",
         call. = FALSE)
  }
  transform <- match.arg(transform)
  d <- .rhf_priority_plot_data(x, vars, top_n_union)
  d <- d[is.finite(d$priority), , drop = FALSE]
  if (!nrow(d)) stop("No finite RHF priority values to plot.", call. = FALSE)
  d$display_priority <- if (transform == "log10") log10(d$priority + 1) else d$priority
  size <- .rhf_priority_cap(d$display_priority, size_cap, "size_cap")
  color <- .rhf_priority_cap(d$display_priority, color_cap, "color_cap")
  d$size_display <- size$value
  d$color_display <- color$value
  ordered_windows <- unique(d[order(d$time_index), c("time_index", "time_window")])
  d$time_window <- factor(d$time_window, levels = ordered_windows$time_window)
  note <- if (isTRUE(display_note)) {
    bits <- c(if (size$applied) sprintf("size capped at q%.0f", 100 * size_cap),
              if (color$applied) sprintf("color capped at q%.0f", 100 * color_cap))
    if (length(bits)) paste("Display only:", paste(bits, collapse = "; ")) else NULL
  } else NULL
  ggplot2::ggplot(d, ggplot2::aes(
    x = .data[["time_window"]], y = .data[["variable"]],
    size = .data[["size_display"]], color = .data[["color_display"]]
  )) +
    ggplot2::geom_point(alpha = 0.9, ...) +
    ggplot2::scale_size_continuous(range = c(1.5, 7)) +
    ggplot2::scale_color_gradient(low = "grey85", high = "steelblue4") +
    ggplot2::labs(x = "Time window", y = NULL,
                  size = "RHF variable priority",
                  color = "RHF variable priority", caption = note) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
```

`.rhf_priority_plot_data()` validates `vars` and `top_n_union`; explicit names override the union. The union selects the leading finite rows within each window, then filters the original frame without reordering it. Implement both helpers as:

```r
.rhf_priority_plot_data <- function(x, vars, top_n_union) {
  available <- levels(x$variable)
  if (!is.null(vars)) {
    if (!is.character(vars) || !length(vars) || anyNA(vars)) {
      stop("'vars' must be a nonempty character vector.", call. = FALSE)
    }
    unknown <- setdiff(vars, available)
    if (length(unknown)) {
      stop("Unknown RHF priority variables: ", paste(unknown, collapse = ", "),
           call. = FALSE)
    }
    keep <- unique(vars)
  } else if (is.null(top_n_union)) {
    keep <- available
  } else {
    if (!is.numeric(top_n_union) || length(top_n_union) != 1L ||
        !is.finite(top_n_union) || top_n_union < 1L ||
        top_n_union != as.integer(top_n_union)) {
      stop("'top_n_union' must be NULL or one positive integer.", call. = FALSE)
    }
    finite <- x[is.finite(x$priority), , drop = FALSE]
    by_window <- split(finite, finite$time_index)
    keep <- unique(unlist(lapply(by_window, function(d) {
      d <- d[order(-d$priority, as.character(d$variable)), , drop = FALSE]
      utils::head(as.character(d$variable), as.integer(top_n_union))
    }), use.names = FALSE))
  }
  x[as.character(x$variable) %in% keep, , drop = FALSE]
}

.rhf_priority_cap <- function(x, prob, arg) {
  if (!is.numeric(prob) || length(prob) != 1L || !is.finite(prob) ||
      prob <= 0 || prob > 1) {
    stop("'", arg, "' must be one numeric value in (0, 1].", call. = FALSE)
  }
  cap <- unname(stats::quantile(x[is.finite(x)], prob, names = FALSE))
  list(value = pmin(x, cap), applied = any(x > cap, na.rm = TRUE))
}
```

- [ ] **Step 4: Add full roxygen and run plot tests green.** Document that point size/color and caps are display-only, zeros receive the minimum point size, missing values are not drawn, and the plot follows the paper's variable-priority matrix.

```bash
Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_plot_gg_rhf_importance.R")'
```

Expected: 0 failures and 0 errors.

- [ ] **Step 5: Commit the plot.**

```bash
git add R/plot.gg_rhf_importance.R tests/testthat/test_plot_gg_rhf_importance.R
git commit -m "feat: plot RHF priority over time"
```

---

### Task 4: S3 companions and cross-family ordering contract

**Files:**
- Modify: `R/print_methods.R`
- Modify: `R/summary_methods.R`
- Modify: `R/autoplot_methods.R`
- Modify: `tests/testthat/test_gg_rhf_importance.R`
- Modify: `tests/testthat/test_plot_conventions.R`

**Interfaces:**
- Consumes: `.rhf_priority_summary()` and `plot.gg_rhf_importance()`.
- Produces: `print.gg_rhf_importance()`, `summary.gg_rhf_importance()`, `autoplot.gg_rhf_importance()`.

- [ ] **Step 1: Add failing companion tests.** Assert print output contains `variables`, `windows`, `precomputed`, `q90`, and `y_source`; print returns invisibly; summary has the exact seven columns from the spec and descending q90; autoplot returns ggplot and forwards `vars`.

- [ ] **Step 2: Run the companion tests red.** Use the extractor test-file command from Task 2 and expect missing-method failures.

- [ ] **Step 3: Add the three methods.**

```r
print.gg_rhf_importance <- function(x, ...) {
  prov <- attr(x, "provenance") %||% list()
  cat(.gg_header(x, "gg_rhf_importance"),
      sprintf("  |  variables: %d  windows: %d  y_source: %s",
              nlevels(x$variable), length(unique(x$time_index)),
              prov$y_source %||% NA_character_),
      sprintf("  |  precomputed: %s  rank: q90", isTRUE(prov$precomputed)),
      "\n", sep = "")
  invisible(x)
}

summary.gg_rhf_importance <- function(object, ...) {
  .rhf_priority_summary(object)
}

autoplot.gg_rhf_importance <- function(object, ...) {
  plot(object, ...)
}
```

Add `gg_rhf`, `gg_auct`, and `gg_rhf_importance` to the `autoplot.gg` supported-class details if either existing RHF class is still absent.

- [ ] **Step 4: Extend the importance-ordering test.** Use `.fake_rhf_importance()` or a small local constructed object; assert the highest q90 variable is the last factor level and that the first row of each window remains that window's highest priority. Do not add a forest fit to this cross-cutting test.

- [ ] **Step 5: Run focused companion and convention tests.**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_gg_rhf_importance.R"); testthat::test_file("tests/testthat/test_plot_conventions.R")'
```

Expected: 0 failures and 0 errors.

- [ ] **Step 6: Commit companions and convention coverage.**

```bash
git add R/print_methods.R R/summary_methods.R R/autoplot_methods.R tests/testthat/test_gg_rhf_importance.R tests/testthat/test_plot_conventions.R
git commit -m "feat: complete RHF priority S3 methods"
```

---

### Task 5: Documentation, citations, package index, and design reconciliation

**Files:**
- Modify: `R/gg_rhf.R`
- Modify: `R/gg_auct.R`
- Modify: `R/gg_rhf_importance.R`
- Modify: `R/plot.gg_rhf_importance.R`
- Modify: `NEWS.md`
- Modify: `_pkgdown.yml`
- Modify: `vignettes/ggRandomForests.bib`
- Modify: `dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md`
- Generate: `NAMESPACE`, `man/gg_rhf_importance.Rd`, `man/plot.gg_rhf_importance.Rd`, and updated shared method pages.

**Interfaces:**
- Consumes: approved design language and the two verified citations.
- Produces: discoverable Phase 3 API and synchronized generated documentation.

- [ ] **Step 1: Add exact bibliography entries.** Append:

```bibtex
@article{Ishwaran:RHF:2026,
  author  = {Ishwaran, Hemant and Hsich, Eileen M. and Kogalur, Udaya B. and Lee, Donald K. K.},
  title   = {Random Hazard Forests},
  journal = {arXiv preprint},
  year    = {2026},
  doi     = {10.48550/arXiv.2608.21597},
  url     = {https://arxiv.org/abs/2608.21597}
}

@manual{Ishwaran:RHF:software:2026,
  author  = {Ishwaran, Hemant and Kogalur, Udaya B.},
  title   = {randomForestRHF: Random Hazard Forests},
  year    = {2026},
  note    = {R package version 1.0.1},
  url     = {https://CRAN.R-project.org/package=randomForestRHF}
}
```

- [ ] **Step 2: Add the method citation to `gg_rhf()` and `gg_auct()`.** Use an `@references` block with all four paper authors, year, title, arXiv identifier, and DOI. The new Phase 3 page includes that block plus the software citation.

- [ ] **Step 3: Add the Phase 3 NEWS entry.** Under the v4 development heading, state the ten-column return contract, compute-or-reuse interface, q90 ordering, point-matrix display, no cutoff, and the `randomForestRHF >= 1.0.1` floor. Do not describe the priority score as a z-score.

- [ ] **Step 4: Add `gg_rhf_importance` and `plot.gg_rhf_importance` after `gg_auct` in `_pkgdown.yml`'s Survival Analysis section.**

- [ ] **Step 5: Reconcile the May umbrella design.** Replace its Phase 3 input/output/plot paragraph with the approved signature, ten columns, `priority` terminology, and point matrix. Add the new paper and CRAN software to its references. Preserve the historical phase/version notes outside the Phase 3 correction.

- [ ] **Step 6: Confirm the Phase 5 vignette release gate remains open.** Do not create or render the vignette in this phase. The checkbox in the approved Phase 3 design must still be `- [ ]`, must name all four RHF families, and must state that no v4 release or CRAN submission proceeds before it is completed.

- [ ] **Step 7: Generate documentation first.**

```bash
Rscript -e 'devtools::document()'
```

Expected: exit 0; `NAMESPACE` contains the generic/method plus plot, print, summary, and autoplot S3 registrations; generated `.Rd` pages contain both citations and no `z`/`selected` contract.

- [ ] **Step 8: Run documentation-focused checks.**

```bash
rg -n 'gg_rhf_importance|Ishwaran.*Hsich|2608\.21597|randomForestRHF \(>= 1\.0\.1\)' DESCRIPTION NEWS.md NAMESPACE R man _pkgdown.yml vignettes/ggRandomForests.bib dev/plans
Rscript -e 'devtools::load_all(quiet = TRUE); stopifnot(exists("gg_rhf_importance"), is.function(getS3method("gg_rhf_importance", "rhf")), is.function(getS3method("plot", "gg_rhf_importance")))'
```

Expected: citations and registrations present; R command exits 0.

- [ ] **Step 9: Commit source and generated documentation.**

```bash
git add DESCRIPTION NEWS.md NAMESPACE R/gg_rhf.R R/gg_auct.R R/gg_rhf_importance.R R/plot.gg_rhf_importance.R R/print_methods.R R/summary_methods.R R/autoplot_methods.R man _pkgdown.yml vignettes/ggRandomForests.bib dev/plans/2026-05-29-rhf-integration-v4.0.0-design.md
git commit -m "docs: document RHF variable priority"
```

---

### Task 6: Visual regression and complete verification

**Files:**
- Modify: `tests/testthat/test_snapshots.R`
- Create: `tests/testthat/_snaps/snapshots/gg-rhf-importance-priority.svg`

**Interfaces:**
- Consumes: Task 1 real fixture and Task 3 plot method.
- Produces: one deterministic visual baseline and evidence for every repository gate.

- [ ] **Step 1: Record snapshot state before touching the suite.**

```bash
git status --short
git ls-files tests/testthat/_snaps/snapshots | wc -l
```

Expected: only intended Phase 3 changes, and the existing tracked baseline count is 52 before adding the new file.

- [ ] **Step 2: Add the snapshot test.** Inside the existing guarded RHF snapshot area, add:

```r
test_that("snapshot: gg_rhf_importance priority matrix", {
  skip_on_cran()
  skip_if_not_installed("randomForestRHF")
  set.seed(20260825L)
  x <- gg_rhf_importance(.rhf_pbc(), importance_fit = .rhf_importance_pbc())
  vdiffr::expect_doppelganger(
    "gg_rhf_importance priority matrix",
    plot(x, top_n_union = 8L)
  )
})
```

- [ ] **Step 3: Run all focused nonvisual tests before baseline generation.**

```bash
NOT_CRAN=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_gg_rhf_importance.R"); testthat::test_file("tests/testthat/test_plot_gg_rhf_importance.R"); testthat::test_file("tests/testthat/test_plot_conventions.R")'
```

Expected: 0 failures and 0 errors.

- [ ] **Step 4: Generate and accept the one new baseline with the guard explicitly on.**

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(quiet = TRUE); testthat::test_file("tests/testthat/test_snapshots.R"); testthat::snapshot_accept()'
```

Expected: exactly one new RHF priority SVG. Do not use blanket checkout commands on `_snaps/`.

- [ ] **Step 5: Verify snapshot scope immediately.**

```bash
git status --short tests/testthat/_snaps tests/testthat/test_snapshots.R
git diff --name-status -- tests/testthat/_snaps
```

Expected: one added SVG and no deleted or modified baseline.

- [ ] **Step 6: Run the definition of done in its required order.** Check status before and after the suite.

```bash
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'
git status --short tests/testthat/_snaps
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'
git status --short tests/testthat/_snaps
```

Expected: documentation exits 0; lint reports 0 lints; tests report 0 failures and 0 errors; snapshot status is unchanged across the suite.

- [ ] **Step 7: Run `R CMD check --as-cran` from a clean archive export with the manual as a Phase 3 quality gate, not as release authorization.** Use a throwaway directory so worktree-only files cannot enter the tarball:

```bash
phase3_check_dir=$(mktemp -d)
git archive HEAD | tar -x -C "$phase3_check_dir"
(cd "$phase3_check_dir" && R CMD build .)
(cd "$phase3_check_dir" && R CMD check --as-cran ggRandomForests_4.0.0.tar.gz)
tar tzf "$phase3_check_dir/ggRandomForests_4.0.0.tar.gz" | grep -E '/\.[^/]+'
tar xzf "$phase3_check_dir/ggRandomForests_4.0.0.tar.gz" -O ggRandomForests/DESCRIPTION | sed -n '4,5p'
tar tzf "$phase3_check_dir/ggRandomForests_4.0.0.tar.gz" | grep -c cran-comments
```

The hidden-path command must report only `ggRandomForests/.Rinstignore`;
DESCRIPTION must report `Version: 4.0.0` and `Date: 2026-08-05`; the final
count must be `0`.

Expected: `Status: OK` or only the repository's explicitly understood time-sensitive CRAN NOTE. Any new warning, error, or note blocks completion.

This local check does not lift the CRAN hold. Do not call
`devtools::submit_cran()`, create a v4 tag, or create a GitHub Release in this
phase.

- [ ] **Step 8: Commit the snapshot and any regenerated files.**

```bash
git add tests/testthat/test_snapshots.R tests/testthat/_snaps/snapshots/gg-rhf-importance-priority.svg NAMESPACE man
git commit -m "test: add RHF priority visual regression"
```

- [ ] **Step 9: Request code review before integration.** Use `superpowers:requesting-code-review`, resolve findings with `superpowers:receiving-code-review`, and rerun proportionate verification after any change. The Phase 5 vignette release gate and CRAN hold remain open after Phase 3 merges.

---

## Release Hold After Phase 3

Finishing this plan means Phase 3 is eligible to merge into `dev_rhf`. It does
not mean v4 is releasable. Release work resumes only when all of these are
true:

1. Phase 4 is complete.
2. The Phase 5 RHF vignette release-gate checkbox is completed.
3. The full package and CRAN release gates pass.
4. The maintainer explicitly lifts the CRAN submission hold.
5. CRAN accepts v4 before the release tag and GitHub Release are created.
