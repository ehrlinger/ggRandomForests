# varPro Phase 4 — gg_isopro Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `gg_isopro()` (tidy extractor) + `plot.gg_isopro()` (patchwork: elbow + density, with threshold annotation and multi-method dispatch) + the S3 companions (`print` / `summary` / `autoplot`) for `varPro::isopro` isolation-forest anomaly scores.

**Architecture:** All computation lives in the extractor. Plot method is pure rendering, with `panel = c("both", "elbow", "density")` controlling the return shape (`patchwork` vs single `ggplot`). Threshold annotation is opt-in via `threshold` (score-space) or `top_n_pct` (quantile-space); both set → `threshold` wins with a `message()`. Multi-method comparison via `dplyr::bind_rows` of three single-fit calls; the plot method auto-detects a `method` column.

**Tech Stack:** R, varPro (Imports), ggplot2, patchwork (Imports), testthat, vdiffr (Suggests).

**Spec:** `dev/plans/2026-05-26-varpro-phase4-gg-isopro-design.md`.

**Branch state:** `feat/varpro-phase4-gg-isopro` already exists from `origin/main`; the spec is already committed on it. Version on `main` is `2.7.3.9007`; this PR bumps to `2.7.3.9008`.

---

## File map

| File | Purpose |
|------|---------|
| `R/gg_isopro.R` *(new)* | `gg_isopro()` generic + `gg_isopro.isopro()` method; ~60 lines |
| `R/plot.gg_isopro.R` *(new)* | `plot.gg_isopro()` method + private helpers; ~150 lines |
| `R/print_methods.R` *(append)* | `print.gg_isopro` |
| `R/summary_methods.R` *(append)* | `summary.gg_isopro` |
| `R/autoplot_methods.R` *(append)* | `autoplot.gg_isopro` |
| `tests/testthat/test_gg_isopro.R` *(new)* | extractor + plot + S3 tests |
| `tests/testthat/test_snapshots.R` *(append)* | 2 vdiffr snapshots |
| `DESCRIPTION` *(edit)* | Version bump to `2.7.3.9008` |
| `NEWS.md` *(edit)* | v2.8.0 dev entry |

---

## Task 0: Version bump

**Files:**
- Modify: `DESCRIPTION:4`

- [ ] **Step 1: Confirm branch state**

```bash
cd /Users/ehrlinj/Documents/GitHub/ggRandomForests
git branch --show-current   # expect: feat/varpro-phase4-gg-isopro
git log --oneline -1        # expect: the design-spec commit
```

- [ ] **Step 2: Bump version in DESCRIPTION**

In `DESCRIPTION` change the single line:

```
Version: 2.7.3.9007
```

to:

```
Version: 2.7.3.9008
```

- [ ] **Step 3: Commit**

```bash
git add DESCRIPTION
git commit -m "chore: open v2.7.3.9008 dev cycle (varPro Phase 4 gg_isopro)"
```

---

## Task 1: Extractor — failing tests (TDD red)

**Files:**
- Create: `tests/testthat/test_gg_isopro.R`

- [ ] **Step 1: Write the extractor test file**

Create `tests/testthat/test_gg_isopro.R` with this exact content:

```r
# Tests for gg_isopro() — varPro::isopro tidy-data wrapper.
# Phase 4 of the v2.8.0 varPro integration.

make_iso_fit <- function(seed = 1L, method = "rnd", ntree = 25, sampsize = 16) {
  skip_if_not_installed("varPro")
  set.seed(seed)
  varPro::isopro(
    data     = iris[, 1:4],
    method   = method,
    sampsize = sampsize,
    ntree    = ntree
  )
}

test_that("gg_isopro: returns gg_isopro data.frame with correct columns", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_s3_class(gg, "gg_isopro")
  expect_s3_class(gg, "data.frame")
  expect_named(gg, c("obs", "case.depth", "howbad"), ignore.order = TRUE)
})

test_that("gg_isopro: row count equals n; howbad in [0,1]; obs is 1..n", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_equal(nrow(gg), nrow(iris))
  expect_true(all(gg$howbad >= 0 & gg$howbad <= 1))
  expect_equal(gg$obs, seq_len(nrow(iris)))
})

test_that("gg_isopro: provenance attribute attached", {
  fit  <- make_iso_fit(ntree = 25)
  gg   <- gg_isopro(fit)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_equal(prov$source, "varPro::isopro")
  expect_equal(prov$n, nrow(iris))
  expect_equal(prov$ntree, 25)
})
```

- [ ] **Step 2: Run — expect FAIL**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```

Expected: 3 failures, all "could not find function 'gg_isopro'".

---

## Task 2: Extractor — implementation (TDD green)

**Files:**
- Create: `R/gg_isopro.R`
- Modify: `NAMESPACE` (auto via roxygen)

- [ ] **Step 1: Write `R/gg_isopro.R`**

Create `R/gg_isopro.R` with this exact content:

```r
####**********************************************************************
####  gg_isopro: tidy extractor for varPro::isopro anomaly scores.
####
####  varPro::isopro returns a list with $howbad (per-observation anomaly
####  score in [0,1]) and $case.depth (average isolation depth, lower =
####  more anomalous). gg_isopro() reshapes these into a tidy data.frame
####  the plot/print/summary methods can consume.
####**********************************************************************

#' Tidy data from a varPro isolation-forest fit
#'
#' Pulls per-observation anomaly scores out of a `varPro::isopro` fit so
#' you can plot them, sort them, or write them to disk without having to
#' know the internal shape of the fit.
#'
#' @param object An `isopro` fit returned by `varPro::isopro()`.
#' @param ... Currently unused.
#'
#' @return A `data.frame` of class `c("gg_isopro", "data.frame")`, one row
#'   per observation. Columns:
#'   \describe{
#'     \item{obs}{Integer; observation index `1..n`.}
#'     \item{case.depth}{Numeric; mean isolation depth across the forest
#'       (lower = more anomalous).}
#'     \item{howbad}{Numeric in `[0, 1]`; anomaly score (higher = more
#'       anomalous). The plot method uses this as the primary axis.}
#'   }
#'   A `provenance` attribute records the source, `n`, and `ntree`.
#'
#' @details
#'   To compare methods (`"rnd"`, `"unsupv"`, `"auto"`), call `gg_isopro()`
#'   on each fit and `dplyr::bind_rows()` the results with a `method` label
#'   column. The plot method auto-detects `method` and colours the curves.
#'
#' @seealso [plot.gg_isopro()], [varPro::isopro()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   fit <- varPro::isopro(data = iris[, 1:4], method = "rnd",
#'                         sampsize = 32, ntree = 50)
#'   gg <- gg_isopro(fit)
#'   plot(gg)
#' }
#' }
#'
#' @export
gg_isopro <- function(object, ...) {
  UseMethod("gg_isopro", object)
}

#' @export
gg_isopro.isopro <- function(object, ...) {
  if (!inherits(object, "isopro")) {
    stop("gg_isopro expects a 'isopro' object from varPro::isopro().",
         call. = FALSE)
  }

  howbad <- as.numeric(object$howbad)
  depth  <- as.numeric(object$case.depth)
  n      <- length(howbad)

  gg_dta <- data.frame(
    obs        = seq_len(n),
    case.depth = depth,
    howbad     = howbad
  )

  class(gg_dta) <- c("gg_isopro", class(gg_dta))

  # isopro-specific provenance (the shared .gg_provenance helper only knows
  # about rfsrc / randomForest objects, so build the list inline).
  ntree <- tryCatch(
    as.integer(object$isoforest$ntree),
    error = function(e) NA_integer_
  )
  attr(gg_dta, "provenance") <- list(
    source = "varPro::isopro",
    n      = n,
    ntree  = if (length(ntree) == 1 && !is.na(ntree)) ntree else NA_integer_
  )

  invisible(gg_dta)
}
```

- [ ] **Step 2: Regenerate NAMESPACE**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
```

Expected: `NAMESPACE` gains `export(gg_isopro)` and `S3method(gg_isopro, isopro)`.

- [ ] **Step 3: Run the Task 1 tests — expect PASS**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```

Expected: `[ FAIL 0 | WARN 0 | SKIP 0 | PASS 3 ]`.

- [ ] **Step 4: Commit**

```bash
git add R/gg_isopro.R man/gg_isopro.Rd NAMESPACE tests/testthat/test_gg_isopro.R
git commit -m "feat(gg_isopro): tidy extractor for varPro::isopro anomaly scores"
```

---

## Task 3: Plot — panel return shapes (TDD red)

**Files:**
- Modify: `tests/testthat/test_gg_isopro.R` (append)

- [ ] **Step 1: Append panel-return-shape tests**

Append to `tests/testthat/test_gg_isopro.R`:

```r
test_that("plot.gg_isopro: panel='both' returns patchwork with 2 sub-plots that build", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "both")
  expect_s3_class(p, "patchwork")
  expect_length(p$patches$plots, 1L) # left panel is `p` itself; right is in $patches$plots
  for (sub in c(list(p), p$patches$plots)) {
    expect_no_error(ggplot2::ggplot_build(sub))
  }
})

test_that("plot.gg_isopro: panel='elbow' returns a single ggplot", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "elbow")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "patchwork"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot.gg_isopro: panel='density' returns a single ggplot", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p   <- plot(gg, panel = "density")
  expect_s3_class(p, "ggplot")
  expect_false(inherits(p, "patchwork"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot.gg_isopro: rejects bad panel values via match.arg", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_error(plot(gg, panel = "nope"))
})
```

- [ ] **Step 2: Run — expect FAIL**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```

Expected: the 4 new tests FAIL (`plot.gg_isopro` does not yet exist). The 3 from Task 1 still PASS.

---

## Task 4: Plot — implementation (TDD green for panel shapes)

**Files:**
- Create: `R/plot.gg_isopro.R`

- [ ] **Step 1: Write `R/plot.gg_isopro.R`**

Create `R/plot.gg_isopro.R` with this exact content:

```r
####**********************************************************************
####  plot.gg_isopro: ranked-elbow + density plot for isopro anomaly scores.
####**********************************************************************

#' Plot a varPro isolation-forest anomaly score
#'
#' Renders a `gg_isopro` object as a ranked elbow (observations sorted by
#' anomaly score), a density of scores, or both side-by-side. Optionally
#' annotates a threshold either in score-space (`threshold`) or in
#' quantile-space (`top_n_pct`).
#'
#' @param x A `gg_isopro` object from [gg_isopro()].
#' @param panel One of `"both"` (default — a `patchwork` of elbow + density),
#'   `"elbow"`, or `"density"` (each returns a single `ggplot`).
#' @param threshold Numeric in `[0, 1]`, or `NULL` (default). If set, draws
#'   a reference line at that `howbad` value on the elbow and density.
#' @param top_n_pct Numeric in `(0, 100)`, or `NULL` (default). If set,
#'   resolves to the matching `howbad` quantile and draws the same
#'   reference line. If both `threshold` and `top_n_pct` are supplied,
#'   `threshold` wins with a `message()`.
#' @param ... Currently unused.
#'
#' @return A `ggplot` (single panel) or a `patchwork` (panel = "both").
#'
#' @seealso [gg_isopro()], [varPro::isopro()]
#' @export
plot.gg_isopro <- function(x,
                           panel     = c("both", "elbow", "density"),
                           threshold = NULL,
                           top_n_pct = NULL,
                           ...) {
  panel <- match.arg(panel)

  # Resolve threshold (score wins if both supplied).
  thr <- .resolve_isopro_threshold(x$howbad, threshold, top_n_pct)

  # Multi-method detection: a `method` column means the user bound several
  # fits together with bind_rows; colour/group by method.
  has_method <- "method" %in% names(x)

  elbow   <- .gg_isopro_elbow(x, thr, has_method)
  density <- .gg_isopro_density(x, thr, has_method)

  if (panel == "elbow")    return(elbow)
  if (panel == "density")  return(density)

  # panel == "both"
  elbow + density
}

# Returns the resolved threshold (numeric scalar) or NA_real_ if neither
# arg was supplied. Emits a message when both args are set.
.resolve_isopro_threshold <- function(howbad, threshold, top_n_pct) {
  if (!is.null(threshold) && !is.null(top_n_pct)) {
    message("Both `threshold` and `top_n_pct` supplied; using `threshold`.")
    return(as.numeric(threshold))
  }
  if (!is.null(threshold)) return(as.numeric(threshold))
  if (!is.null(top_n_pct)) {
    q <- 1 - (as.numeric(top_n_pct) / 100)
    return(as.numeric(stats::quantile(howbad, probs = q, na.rm = TRUE)))
  }
  NA_real_
}

# Rank-ordered elbow panel.
.gg_isopro_elbow <- function(x, thr, has_method) {
  if (has_method) {
    x$rank <- stats::ave(x$howbad, x$method, FUN = function(v) rank(v, ties.method = "first"))
  } else {
    x$rank <- rank(x$howbad, ties.method = "first")
  }
  x <- x[order(x$rank), , drop = FALSE]

  aes_line <- if (has_method) {
    ggplot2::aes(x = .data$rank, y = .data$howbad, colour = .data$method)
  } else {
    ggplot2::aes(x = .data$rank, y = .data$howbad)
  }

  p <- ggplot2::ggplot(x) +
    ggplot2::geom_line(aes_line) +
    ggplot2::labs(x = "Observation rank (by howbad)",
                  y = "howbad (anomaly score)")

  if (!is.na(thr)) {
    p <- p +
      ggplot2::geom_hline(yintercept = thr, linetype = "dashed",
                          colour = "red", linewidth = 0.4)
  }
  p
}

# Score-density panel.
.gg_isopro_density <- function(x, thr, has_method) {
  aes_dens <- if (has_method) {
    ggplot2::aes(x = .data$howbad, fill = .data$method)
  } else {
    ggplot2::aes(x = .data$howbad)
  }

  p <- ggplot2::ggplot(x) +
    ggplot2::geom_density(aes_dens, alpha = if (has_method) 0.4 else 1) +
    ggplot2::labs(x = "howbad (anomaly score)", y = "Density")

  if (!is.na(thr)) {
    p <- p +
      ggplot2::geom_vline(xintercept = thr, linetype = "dashed",
                          colour = "red", linewidth = 0.4)
  }
  p
}
```

- [ ] **Step 2: Regenerate NAMESPACE**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
```

- [ ] **Step 3: Run the panel tests — expect PASS**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```

Expected: `[ FAIL 0 | ... | PASS 7 ]`.

- [ ] **Step 4: Commit**

```bash
git add R/plot.gg_isopro.R man/plot.gg_isopro.Rd NAMESPACE tests/testthat/test_gg_isopro.R
git commit -m "feat(plot.gg_isopro): elbow + density patchwork with panel arg"
```

---

## Task 5: Threshold annotation — failing tests

**Files:**
- Modify: `tests/testthat/test_gg_isopro.R` (append)

- [ ] **Step 1: Append threshold tests**

Append to `tests/testthat/test_gg_isopro.R`:

```r
# Helper: count geom_hline / geom_vline layers in a ggplot.
.count_ref_lines <- function(p) {
  if (inherits(p, "patchwork")) {
    sub <- c(list(p), p$patches$plots)
    return(sum(vapply(sub, .count_ref_lines, integer(1L))))
  }
  sum(vapply(p$layers, function(l) {
    inherits(l$geom, "GeomHline") || inherits(l$geom, "GeomVline")
  }, logical(1L)))
}

test_that("plot.gg_isopro: threshold adds a reference line on each panel", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  p_none <- plot(gg, panel = "both")
  p_thr  <- plot(gg, panel = "both", threshold = 0.8)
  # panel = both has elbow + density => one hline + one vline = 2 ref lines.
  expect_equal(.count_ref_lines(p_none), 0L)
  expect_equal(.count_ref_lines(p_thr), 2L)
})

test_that("plot.gg_isopro: top_n_pct resolves to the matching quantile", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  q95 <- as.numeric(stats::quantile(gg$howbad, 0.95))
  p   <- plot(gg, panel = "elbow", top_n_pct = 5)
  # Find the hline; check its yintercept equals q95.
  yints <- vapply(p$layers, function(l) {
    if (inherits(l$geom, "GeomHline")) l$data$yintercept else NA_real_
  }, numeric(1L))
  yints <- yints[!is.na(yints)]
  expect_length(yints, 1L)
  expect_equal(yints[[1]], q95, tolerance = 1e-9)
})

test_that("plot.gg_isopro: threshold + top_n_pct both set => message, threshold wins", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_message(
    p <- plot(gg, panel = "elbow", threshold = 0.7, top_n_pct = 5),
    "Both .* using `threshold`"
  )
  yints <- vapply(p$layers, function(l) {
    if (inherits(l$geom, "GeomHline")) l$data$yintercept else NA_real_
  }, numeric(1L))
  yints <- yints[!is.na(yints)]
  expect_equal(yints[[1]], 0.7, tolerance = 1e-9)
})
```

- [ ] **Step 2: Run — expect PASS (the implementation in Task 4 already covers this)**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```

Expected: `[ FAIL 0 | ... | PASS 10 ]`. (Task 4's `.resolve_isopro_threshold` already implements the precedence; these tests verify it green-on-arrival.)

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_isopro.R
git commit -m "test(plot.gg_isopro): threshold and top_n_pct precedence"
```

---

## Task 6: Multi-method dispatch — failing test then verify

**Files:**
- Modify: `tests/testthat/test_gg_isopro.R` (append)

- [ ] **Step 1: Append multi-method test**

Append to `tests/testthat/test_gg_isopro.R`:

```r
test_that("plot.gg_isopro: a method column triggers colour grouping in the elbow", {
  fit_rnd <- make_iso_fit(seed = 1L, method = "rnd")
  fit_uns <- make_iso_fit(seed = 1L, method = "unsupv")
  gg <- rbind(
    cbind(gg_isopro(fit_rnd), method = "rnd"),
    cbind(gg_isopro(fit_uns), method = "unsupv")
  )
  class(gg) <- c("gg_isopro", "data.frame")

  p <- plot(gg, panel = "elbow")
  expect_s3_class(p, "ggplot")
  # Built scales should include a colour scale because `method` is mapped.
  built <- ggplot2::ggplot_build(p)
  scales <- built$plot$scales$scales
  has_colour <- any(vapply(scales, function(s) "colour" %in% s$aesthetics, logical(1L)))
  # Even when ggplot2 hasn't materialised a discrete colour scale into
  # $scales, the layer's aes mapping must include `colour = method`.
  layer_aes <- p$layers[[1]]$mapping
  expect_true("colour" %in% names(layer_aes) || has_colour)
})
```

- [ ] **Step 2: Run — expect PASS (Task 4 implementation already wires this up)**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```

Expected: `[ FAIL 0 | ... | PASS 11 ]`.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test_gg_isopro.R
git commit -m "test(plot.gg_isopro): method column triggers colour grouping"
```

---

## Task 7: S3 companions — `print`, `summary`, `autoplot`

**Files:**
- Modify: `R/print_methods.R` (append)
- Modify: `R/summary_methods.R` (append)
- Modify: `R/autoplot_methods.R` (append)
- Modify: `tests/testthat/test_gg_isopro.R` (append)

- [ ] **Step 1: Append S3 tests**

Append to `tests/testthat/test_gg_isopro.R`:

```r
test_that("print.gg_isopro: prints a one-line header, invisibly", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  txt <- utils::capture.output(out <- print(gg))
  expect_identical(out, gg)               # returns invisibly
  expect_length(txt, 1L)                  # exactly one line
  expect_match(txt, "gg_isopro")
})

test_that("summary.gg_isopro: returns summary.gg with quantile snapshot in body", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  s   <- summary(gg)
  expect_s3_class(s, "summary.gg")
  expect_true(any(grepl("rows", s$body)))
  expect_true(any(grepl("howbad", s$body)))
})

test_that("autoplot.gg_isopro: same return as plot.gg_isopro for the default args", {
  fit <- make_iso_fit()
  gg  <- gg_isopro(fit)
  expect_s3_class(autoplot(gg), "patchwork")
})
```

- [ ] **Step 2: Append `print.gg_isopro` to `R/print_methods.R`**

Add at the end of `R/print_methods.R`:

```r
#' @rdname print.gg
#' @export
print.gg_isopro <- function(x, ...) {
  cat(.gg_header(x, "gg_isopro"), "\n", sep = "")
  invisible(x)
}
```

- [ ] **Step 3: Append `summary.gg_isopro` to `R/summary_methods.R`**

Add at the end of `R/summary_methods.R`:

```r
#' @rdname summary.gg
#' @export
summary.gg_isopro <- function(object, ...) {
  q  <- stats::quantile(object$howbad, probs = c(0.05, 0.50, 0.95),
                        na.rm = TRUE, names = FALSE)
  body <- c(
    sprintf("rows: %d", nrow(object)),
    sprintf("howbad range: [%.4g, %.4g]",
            min(object$howbad, na.rm = TRUE),
            max(object$howbad, na.rm = TRUE)),
    sprintf("howbad quantiles (5%%/50%%/95%%): %.4g / %.4g / %.4g",
            q[1], q[2], q[3])
  )
  .summary_skel(object, "gg_isopro", body)
}
```

- [ ] **Step 4: Append `autoplot.gg_isopro` to `R/autoplot_methods.R`**

Add at the end of `R/autoplot_methods.R`:

```r
#' @rdname autoplot.gg
#' @export
autoplot.gg_isopro <- function(object, ...) {
  plot.gg_isopro(object, ...)
}
```

- [ ] **Step 5: Regenerate NAMESPACE**

```bash
Rscript -e "devtools::document(quiet = TRUE)"
```

- [ ] **Step 6: Run — expect PASS**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_gg_isopro.R', reporter = 'progress')"
```

Expected: `[ FAIL 0 | ... | PASS 14 ]`.

- [ ] **Step 7: Commit**

```bash
git add R/print_methods.R R/summary_methods.R R/autoplot_methods.R \
        NAMESPACE man/print.gg.Rd man/summary.gg.Rd man/autoplot.gg.Rd \
        tests/testthat/test_gg_isopro.R
git commit -m "feat(gg_isopro): print / summary / autoplot S3 companions"
```

---

## Task 8: vdiffr snapshots

**Files:**
- Modify: `tests/testthat/test_snapshots.R` (append inside the existing VDIFFR guard block)

- [ ] **Step 1: Locate the closing `}` of the VDIFFR guard in `test_snapshots.R`**

```bash
grep -n "VDIFFR_RUN_TESTS\|^}" tests/testthat/test_snapshots.R | tail
```

You're looking for the final `}` that closes the
`if (identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) { ... }` block.

- [ ] **Step 2: Insert the gg_isopro snapshot block immediately before that closing `}`**

```r
## ── gg_isopro snapshots (Phase 4) ────────────────────────────────────────
if (requireNamespace("varPro", quietly = TRUE)) {
  local({
    set.seed(1L)
    fit <- varPro::isopro(data = iris[, 1:4], method = "rnd",
                          sampsize = 32, ntree = 50)
    gg  <- gg_isopro(fit)

    test_that("snapshot: gg-isopro-default", {
      vdiffr::expect_doppelganger("gg-isopro-default", plot(gg))
    })

    test_that("snapshot: gg-isopro-threshold", {
      vdiffr::expect_doppelganger("gg-isopro-threshold",
                                  plot(gg, threshold = 0.8))
    })
  })
}
```

- [ ] **Step 3: Confirm snapshots skip cleanly without the env var**

```bash
Rscript -e "devtools::load_all('.', quiet = TRUE); testthat::test_file('tests/testthat/test_snapshots.R', reporter = 'progress')"
```

Expected: snapshot tests SKIP (no `VDIFFR_RUN_TESTS=true`); no failures.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test_snapshots.R
git commit -m "test: vdiffr snapshots for gg_isopro (Phase 4)"
```

---

## Task 9: NEWS, R CMD check, push, open PR

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 1: Update `NEWS.md`**

Open `NEWS.md`. Change the line near the top:

```
Version: 2.7.3.9007
```

to:

```
Version: 2.7.3.9008
```

Then add this bullet at the top of the `ggRandomForests v2.8.0 (development) — continued` section:

```
* New `gg_isopro()` and `plot.gg_isopro()`: tidy wrapper and ranked-elbow +
  density visualisation for `varPro::isopro` isolation-forest anomaly
  scores. `plot.gg_isopro()` takes `panel = c("both", "elbow", "density")`
  and optional `threshold` (score-space) or `top_n_pct` (quantile-space)
  to draw a reference line; if both are set, `threshold` wins with a
  message. A `method` column auto-triggers colour grouping for multi-method
  comparisons (use `dplyr::bind_rows()` on three `gg_isopro()` calls).
  `print` / `summary` / `autoplot` S3 companions follow the existing `gg_*`
  conventions. First of three Phase 4 sub-projects.
```

- [ ] **Step 2: Full R CMD check**

```bash
Rscript -e "devtools::check(args = '--as-cran', quiet = TRUE)" 2>&1 | tail -8
```

Expected: `0 errors ✔ | 0 warnings ✔ | 0 notes ✔`.

If there are notes about non-ASCII characters or doc/code mismatch, fix
them before committing. (The voice fingerprint says: prefer ASCII; em-dashes
sparingly.)

- [ ] **Step 3: Commit NEWS**

```bash
git add NEWS.md
git commit -m "docs: NEWS entry for varPro Phase 4 gg_isopro"
```

- [ ] **Step 4: Push branch**

```bash
git push -u origin feat/varpro-phase4-gg-isopro
```

- [ ] **Step 5: Open the PR**

```bash
gh pr create \
  --title "feat: gg_isopro — varPro Phase 4 anomaly-score wrapper" \
  --body "$(cat <<'EOF'
## Summary

First of three Phase 4 sub-projects: a tidy-data wrapper and plot method for
`varPro::isopro` isolation-forest anomaly scores.

- `gg_isopro(fit)` returns a `data.frame` (one row per observation) with
  columns `obs`, `case.depth`, `howbad`; class `c("gg_isopro", "data.frame")`;
  provenance attribute.
- `plot.gg_isopro()` returns a patchwork of a ranked elbow and a score
  density by default; `panel = "elbow"` or `panel = "density"` returns a
  single ggplot. `threshold` (score-space) or `top_n_pct` (quantile-space)
  annotates a reference line; both supplied -> `threshold` wins with a
  message.
- Multi-method comparison: bind_rows the outputs of three `gg_isopro()`
  calls with a `method` label; the plot auto-detects and colours.
- `print` / `summary` / `autoplot` companions.

## Test plan
- [x] `devtools::test()` — new tests pass, no regressions
- [x] `devtools::check(args = '--as-cran')` — 0 errors, 0 warnings, 0 notes
- [x] vdiffr snapshots added (skip cleanly without `VDIFFR_RUN_TESTS=true`)

Spec: `dev/plans/2026-05-26-varpro-phase4-gg-isopro-design.md`
Plan: `dev/plans/2026-05-26-varpro-phase4-gg-isopro-plan.md`

Next Phase 4 sub-projects: `gg_beta_varpro`, then `gg_ivarpro` (and
`predict.isopro` follow-up).
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
- Extractor (spec § Extractor) → Tasks 1–2 ✓
- Plot panel shapes (spec § Plot method) → Tasks 3–4 ✓
- Threshold annotation + precedence → Task 5 + the implementation in Task 4 ✓
- Multi-method dispatch → Task 6 (test) + Task 4 implementation ✓
- `print` / `summary` / `autoplot` companions → Task 7 ✓
- Snapshots → Task 8 ✓
- Version bump + NEWS + R CMD check + PR → Tasks 0, 9 ✓
- Acceptance criteria (0/0/0; tests pass; voice from start) → Task 9 Step 2 ✓
- Out-of-scope items honoured (no ROC, no `shap.ivarpro`, no `predict.isopro`) ✓

**2. Placeholder scan:** no TBD/TODO; every code step shows the actual code;
every test step shows the actual test; every command shows the expected
output. The roxygen docs in Tasks 2 and 4 are written in John's voice
against `dev/voice-fingerprint.md` (terse register for `@param`/`@return`;
narrative for `@description`/`@details`) — no follow-on audit.

**3. Type consistency:** the extractor produces columns
`obs`/`case.depth`/`howbad` and class `c("gg_isopro", "data.frame")`;
every test and every plot helper reads exactly those names. The plot
signature `plot(x, panel, threshold, top_n_pct, ...)` is consistent across
Task 4 (definition), Tasks 3/5/6 (tests), and Task 9 (PR body).
