# RHF v4 Tuning Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a tested and documented `gg_tune_rhf()` family that tidies and plots saved `randomForestRHF` tree-size tuning results without launching a tuning run.

**Architecture:** A new S3 extractor accepts only an upstream `tune.treesize.rhf` object, validates its tuning path, and returns a five-column `gg_tune_rhf` data frame with compact provenance. A separate plot method draws the evaluated path and selected tree size, while the shared print, summary, and autoplot files provide the standard package companions. Synthetic fixtures carry most coverage; one guarded, session-memoised upstream fit and one vdiffr baseline pin compatibility and appearance.

**Tech Stack:** R 4.4+, S3, ggplot2, testthat edition 2, vdiffr, roxygen2, randomForestRHF 1.0.1 (Suggests), devtools, lintr.

**Spec:** `dev/plans/2026-08-25-rhf-v4-consistency-tuning-vignette-design.md`

## Global Constraints

- Work on `codex/rhf-v4-tuning` and target the pull request to `dev_rhf`; do not merge, tag, submit to CRAN, change the version, or release.
- `gg_tune_rhf()` accepts an already calculated `tune.treesize.rhf` object and never calls `tune.treesize.rhf()`, `tune.rhf()`, or `tune.iAUC.rhf()`.
- The documented default workflow calculates and retains the upstream tuning object, then supplies that object to `gg_tune_rhf()`.
- Accept the common `tune.treesize.rhf` class returned by `randomForestRHF::tune.treesize.rhf()`, `randomForestRHF::tune.rhf()`, and `randomForestRHF::tune.iAUC.rhf()`.
- Return exactly `c("gg_tune_rhf", "data.frame")` with columns `treesize`, `metric`, `value`, `se`, and `selected`, in upstream `path` row order.
- Keep `randomForestRHF (>= 1.0.1)` in `Suggests`; add no dependency and never attach it from `R/`.
- Do not copy the optional upstream `forest` component into the returned object or its provenance.
- Risk paths use `path$risk` and never display uncertainty. iAUC paths use `path$iAUC`; use `path$iAUC.se` only when supplied upstream.
- Documentation is for a general CRAN R user. Follow `.claude/house-style.md`, use package-qualified upstream calls, and cite both randomForestRHF 1.0.1 and Ishwaran et al. (2026), arXiv:2608.21597.
- Add no unguarded slow calculation. Every RNG-using `test_that()` block calls `set.seed()` inside the block, and slow integration tests call `skip_on_cran()`.
- Never hand-edit `NAMESPACE` or `man/`; regenerate them with `devtools::document()`.
- Every full suite run is exactly `NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'`, with snapshot status checked before and after.
- Generate or approve the new vdiffr baseline last; do not run a later suite with `VDIFFR_RUN_TESTS` disabled.

---

### Task 1: Tidy and validate saved RHF tuning paths

**Files:**
- Create: `R/gg_tune_rhf.R`
- Create: `tests/testthat/test_gg_tune_rhf.R`
- Reference: `R/gg_rhf_importance.R`
- Reference: `tests/testthat/helper-rhf-fixtures.R`

**Interfaces:**
- Consumes: an object inheriting from `tune.treesize.rhf` with `best.size`, `best.err`, `bounds`, `method`, `perf`, and `path`.
- Produces: `gg_tune_rhf(tune_fit, ...)`, `gg_tune_rhf.tune.treesize.rhf(tune_fit, ...)`, `.validate_rhf_tune_fit(tune_fit)`, and a `c("gg_tune_rhf", "data.frame")` result used by every later task.
- Produces provenance fields named `best_size`, `best_err`, `perf`, `method`, `bounds`, `n_evaluations`, and `randomForestRHF_version`.

- [ ] **Step 1: Add synthetic risk and iAUC fixtures at the top of the new test file**

```r
.fake_rhf_tune_risk <- function() {
  structure(list(
    best.size = 8L,
    best.err = 0.24,
    bounds = c(lower = 2L, upper = 12L),
    n.subjects = 40L,
    C = 3,
    method = "golden",
    perf = "risk",
    path = data.frame(
      treesize = c(2L, 5L, 8L, 12L),
      risk = c(0.40, 0.30, 0.24, 0.29)
    ),
    forest = structure(list(marker = "must not be copied"), class = "rhf")
  ), class = "tune.treesize.rhf")
}

.fake_rhf_tune_iauc <- function(with_se = TRUE) {
  path <- data.frame(
    treesize = c(3L, 6L, 9L),
    risk = c(0.32, 0.21, 0.26),
    iAUC = c(0.68, 0.79, 0.74)
  )
  if (with_se) {
    path$iAUC.se <- c(0.04, 0.03, 0.05)
  }
  structure(list(
    best.size = 6L,
    best.err = 0.21,
    bounds = c(lower = 3L, upper = 9L),
    n.subjects = 30L,
    C = 3,
    method = "bisect",
    perf = "iAUC",
    path = path
  ), class = "tune.treesize.rhf")
}
```

- [ ] **Step 2: Write failing extractor-contract tests**

```r
test_that("gg_tune_rhf tidies an OOB risk path in upstream order", {
  fit <- .fake_rhf_tune_risk()
  out <- gg_tune_rhf(fit)

  expect_identical(class(out), c("gg_tune_rhf", "data.frame"))
  expect_identical(names(out),
                   c("treesize", "metric", "value", "se", "selected"))
  expect_identical(out$treesize, fit$path$treesize)
  expect_identical(out$metric, rep("OOB risk", nrow(fit$path)))
  expect_equal(out$value, fit$path$risk)
  expect_true(all(is.na(out$se)))
  expect_identical(which(out$selected), 3L)
})

test_that("gg_tune_rhf uses iAUC and optional bootstrap standard errors", {
  fit <- .fake_rhf_tune_iauc()
  out <- gg_tune_rhf(fit)

  expect_identical(out$metric, rep("OOB iAUC", nrow(fit$path)))
  expect_equal(out$value, fit$path$iAUC)
  expect_equal(out$se, fit$path$iAUC.se)
  expect_identical(which(out$selected), 2L)

  no_se <- gg_tune_rhf(.fake_rhf_tune_iauc(with_se = FALSE))
  expect_true(all(is.na(no_se$se)))
})
```

- [ ] **Step 3: Run the focused test file and confirm the RED state**

Run:

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_gg_tune_rhf.R")'
```

Expected: failures report that `gg_tune_rhf()` is not found; no snapshot files change.

- [ ] **Step 4: Implement the generic, method, tidy frame, and provenance**

Create `R/gg_tune_rhf.R` with roxygen for the public generic and this implementation shape:

```r
#' @export
gg_tune_rhf <- function(tune_fit, ...) {
  if (!inherits(tune_fit, "tune.treesize.rhf")) {
    stop("'tune_fit' must inherit from 'tune.treesize.rhf'.",
         call. = FALSE)
  }
  UseMethod("gg_tune_rhf", tune_fit)
}

#' @rdname gg_tune_rhf
#' @export
gg_tune_rhf.tune.treesize.rhf <- function(tune_fit, ...) {
  .validate_rhf_tune_fit(tune_fit)
  path <- tune_fit$path
  is_iauc <- identical(tune_fit$perf, "iAUC")
  se <- if (is_iauc && "iAUC.se" %in% names(path)) {
    as.numeric(path$iAUC.se)
  } else {
    rep(NA_real_, nrow(path))
  }
  out <- data.frame(
    treesize = as.integer(path$treesize),
    metric = rep(if (is_iauc) "OOB iAUC" else "OOB risk", nrow(path)),
    value = as.numeric(if (is_iauc) path$iAUC else path$risk),
    se = se,
    selected = path$treesize == tune_fit$best.size,
    stringsAsFactors = FALSE
  )
  class(out) <- c("gg_tune_rhf", "data.frame")
  attr(out, "provenance") <- list(
    best_size = as.integer(tune_fit$best.size),
    best_err = as.numeric(tune_fit$best.err),
    perf = tune_fit$perf,
    method = tune_fit$method,
    bounds = tune_fit$bounds,
    n_evaluations = nrow(path),
    randomForestRHF_version = if (
      requireNamespace("randomForestRHF", quietly = TRUE)
    ) {
      as.character(utils::packageVersion("randomForestRHF"))
    } else {
      NA_character_
    }
  )
  invisible(out)
}
```

The roxygen page must state that this function does no tuning, describe all five columns and provenance fields, recommend retaining and supplying the upstream result, include both required references, and use a package-qualified guarded `\donttest{}` example.

- [ ] **Step 5: Run the focused tests and confirm the basic extractor is GREEN**

Run the focused command from Step 3.

Expected: both contract blocks pass; generated documentation is not yet expected.

- [ ] **Step 6: Add table-driven validation tests**

Add separate `test_that()` blocks covering:

```r
test_that("gg_tune_rhf rejects the wrong upstream class", {
  expect_error(gg_tune_rhf(unclass(.fake_rhf_tune_risk())),
               "tune.treesize.rhf")
})

test_that("gg_tune_rhf validates required scalar metadata", {
  cases <- list(
    best_size = list(field = "best.size", value = c(5L, 8L)),
    best_err = list(field = "best.err", value = NA_real_),
    bounds = list(field = "bounds", value = c(2, NA_real_)),
    method = list(field = "method", value = character()),
    perf = list(field = "perf", value = "other")
  )
  for (case in cases) {
    fit <- .fake_rhf_tune_risk()
    fit[[case$field]] <- case$value
    expect_error(gg_tune_rhf(fit), case$field, fixed = TRUE)
  }
})

test_that("gg_tune_rhf validates path shape and numeric alignment", {
  fit <- .fake_rhf_tune_risk()
  fit$path$treesize[2L] <- fit$path$treesize[1L]
  expect_error(gg_tune_rhf(fit), "unique positive")

  fit <- .fake_rhf_tune_iauc()
  fit$path$iAUC.se <- "not numeric"
  expect_error(gg_tune_rhf(fit), "iAUC.se")
})

test_that("gg_tune_rhf requires one evaluated upstream optimum", {
  fit <- .fake_rhf_tune_risk()
  fit$best.size <- 7L
  expect_error(gg_tune_rhf(fit), "best.size")

  fit <- .fake_rhf_tune_risk()
  fit$path$risk[4L] <- fit$best.err
  expect_error(gg_tune_rhf(fit), "unique optimum")

  fit <- .fake_rhf_tune_iauc()
  fit$best.err <- 0.15
  expect_error(gg_tune_rhf(fit), "best.err")
})
```

Also cover an empty `path`, missing `risk`, missing `iAUC`, non-finite metric values, negative or non-finite standard errors, and a `best.size` row whose metric does not agree with `best.err` (`risk == best.err` for risk; `1 - iAUC == best.err` for iAUC). Use `sqrt(.Machine$double.eps)` as the numeric comparison tolerance.

- [ ] **Step 7: Run the validation tests and confirm the RED state**

Run the focused command from Step 3.

Expected: contract tests pass and new malformed-input cases fail because `.validate_rhf_tune_fit()` is absent or incomplete.

- [ ] **Step 8: Implement strict validation without recalculating tuning**

Implement `.validate_rhf_tune_fit()` and focused internal helpers in `R/gg_tune_rhf.R`. The validator must enforce:

```r
required <- c("best.size", "best.err", "bounds", "method", "perf", "path")
allowed_perf <- c("risk", "iAUC")
metric_name <- if (identical(fit$perf, "iAUC")) "iAUC" else "risk"
selected <- which(fit$path$treesize == fit$best.size)
criterion <- if (identical(fit$perf, "iAUC")) 1 - fit$path$iAUC else fit$path$risk
optimum <- which(abs(criterion - min(criterion)) <= sqrt(.Machine$double.eps))
```

Require exactly one selected row and exactly one optimum row, and require them to be the same row. Require `best.err` to equal the selected criterion within tolerance. Preserve `path` order; validation must not sort or mutate the upstream object. Accept `NA_real_` in `iAUC.se` as “not supplied for this evaluation,” but reject finite negative values and non-finite non-missing values.

- [ ] **Step 9: Run the focused test file and confirm all extractor tests pass**

Run the focused command from Step 3.

Expected: zero failures, zero errors, and no snapshot changes.

- [ ] **Step 10: Commit the extractor contract**

```bash
git add R/gg_tune_rhf.R tests/testthat/test_gg_tune_rhf.R
git commit -m "feat: add RHF tuning extractor"
```

### Task 2: Plot the tuning path and selected size

**Files:**
- Create: `R/plot.gg_tune_rhf.R`
- Create: `tests/testthat/test_plot_gg_tune_rhf.R`
- Reference: `R/plot.gg_rhf.R`

**Interfaces:**
- Consumes: the exact five-column `gg_tune_rhf` data frame from Task 1.
- Produces: `plot.gg_tune_rhf(x, se_band = TRUE, se_mult = 1, ...)`, returning one ggplot object. `...` is passed to the ordinary evaluated-point `ggplot2::geom_point()` layer.

- [ ] **Step 1: Write failing risk-plot tests**

```r
test_that("plot.gg_tune_rhf draws the risk path and selected size", {
  x <- gg_tune_rhf(.fake_rhf_tune_risk())
  p <- plot(x)
  built <- ggplot2::ggplot_build(p)

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$x, "Tree size")
  expect_identical(p$labels$y, "OOB risk")
  expect_equal(nrow(built$data[[1L]]), nrow(x))
  expect_equal(sum(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomRibbon")
  }, logical(1))), 0L)
})

test_that("plot.gg_tune_rhf validates its object and display arguments", {
  expect_error(plot.gg_tune_rhf(data.frame()), "gg_tune_rhf")
  x <- gg_tune_rhf(.fake_rhf_tune_iauc())
  expect_error(plot(x, se_band = NA), "se_band")
  expect_error(plot(x, se_mult = 0), "se_mult")
})
```

- [ ] **Step 2: Run the plot test file and confirm the RED state**

Run:

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_plot_gg_tune_rhf.R")'
```

Expected: failures report that `plot.gg_tune_rhf()` is not found.

- [ ] **Step 3: Implement the minimal line, point, and selected-point plot**

Implement the method in `R/plot.gg_tune_rhf.R` with this layer contract:

```r
ggplot2::ggplot(x, ggplot2::aes(
  x = .data[["treesize"]], y = .data[["value"]]
)) +
  ggplot2::geom_line(color = "grey45", linewidth = 0.6) +
  do.call(ggplot2::geom_point, point_args) +
  ggplot2::geom_point(
    data = x[x$selected, , drop = FALSE],
    shape = 21, size = 3.5, stroke = 0.8,
    color = "black", fill = "steelblue"
  ) +
  ggplot2::labs(x = "Tree size", y = x$metric[1L]) +
  ggplot2::theme_bw()
```

Default `point_args` to `list(size = 2, color = "grey25")` only when the user did not supply those names. Validate that `se_band` is one non-missing logical and `se_mult` is one finite positive number.

- [ ] **Step 4: Run the risk-plot tests and confirm they pass**

Run the focused plot command from Step 2.

Expected: risk plot tests pass and no ribbon layer appears.

- [ ] **Step 5: Add failing iAUC ribbon tests**

```r
test_that("plot.gg_tune_rhf adds an iAUC ribbon only for finite standard errors", {
  with_se <- plot(gg_tune_rhf(.fake_rhf_tune_iauc()))
  without_se <- plot(gg_tune_rhf(.fake_rhf_tune_iauc(with_se = FALSE)))
  disabled <- plot(gg_tune_rhf(.fake_rhf_tune_iauc()), se_band = FALSE)

  ribbon_count <- function(p) sum(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomRibbon")
  }, logical(1)))
  expect_equal(ribbon_count(with_se), 1L)
  expect_equal(ribbon_count(without_se), 0L)
  expect_equal(ribbon_count(disabled), 0L)
})
```

- [ ] **Step 6: Run the iAUC plot test and confirm the RED state**

Run the focused plot command from Step 2.

Expected: the default iAUC plot lacks the expected ribbon.

- [ ] **Step 7: Add the conditional standard-error ribbon**

Before the line and point layers, add a ribbon only when `se_band` is true, the metric is `"OOB iAUC"`, and at least one `se` value is finite. Use only finite-SE rows as ribbon data and calculate:

```r
ribbon$ymin <- pmax(0, ribbon$value - se_mult * ribbon$se)
ribbon$ymax <- pmin(1, ribbon$value + se_mult * ribbon$se)
ggplot2::geom_ribbon(
  data = ribbon,
  ggplot2::aes(ymin = .data[["ymin"]], ymax = .data[["ymax"]]),
  inherit.aes = TRUE,
  fill = "steelblue", alpha = 0.18
)
```

Do not add a risk ribbon and do not infer standard errors from `risk` or `best.err`.

- [ ] **Step 8: Run both focused test files**

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_gg_tune_rhf.R"); testthat::test_file("tests/testthat/test_plot_gg_tune_rhf.R")'
```

Expected: zero failures and zero errors.

- [ ] **Step 9: Commit the plot method**

```bash
git add R/plot.gg_tune_rhf.R tests/testthat/test_plot_gg_tune_rhf.R
git commit -m "feat: plot RHF tuning paths"
```

### Task 3: Add print, summary, autoplot, and cross-family dispatch coverage

**Files:**
- Modify: `R/print_methods.R`
- Modify: `R/summary_methods.R`
- Modify: `R/autoplot_methods.R`
- Modify: `tests/testthat/test_gg_tune_rhf.R`
- Modify: `tests/testthat/test_autoplot_equivalence.R`

**Interfaces:**
- Consumes: `gg_tune_rhf` objects and `plot.gg_tune_rhf()` from Tasks 1 and 2.
- Produces: `print.gg_tune_rhf(x, ...)`, `summary.gg_tune_rhf(object, ...)`, and `autoplot.gg_tune_rhf(object, ...)`.
- `summary.gg_tune_rhf()` returns a one-row ordinary data frame with columns `metric`, `treesize`, `value`, `se`, and `n_evaluations`.

- [ ] **Step 1: Write failing S3 companion tests**

```r
test_that("gg_tune_rhf print reports the tuning context invisibly", {
  x <- gg_tune_rhf(.fake_rhf_tune_iauc())
  expect_output(print(x), "gg_tune_rhf")
  expect_output(print(x), "OOB iAUC")
  expect_output(print(x), "evaluations: 3")
  expect_output(print(x), "selected treesize: 6")
  expect_invisible(print(x))
})

test_that("gg_tune_rhf summary returns the selected result", {
  x <- gg_tune_rhf(.fake_rhf_tune_risk())
  s <- summary(x)
  expect_identical(names(s),
                   c("metric", "treesize", "value", "se", "n_evaluations"))
  expect_identical(s$metric, "OOB risk")
  expect_identical(s$treesize, 8L)
  expect_equal(s$value, 0.24)
  expect_true(is.na(s$se))
  expect_identical(s$n_evaluations, 4L)
})

test_that("gg_tune_rhf autoplot delegates display arguments", {
  x <- gg_tune_rhf(.fake_rhf_tune_iauc())
  p <- ggplot2::autoplot(x, se_band = FALSE, color = "purple")
  expect_s3_class(p, "ggplot")
  expect_false(any(vapply(p$layers, function(layer) {
    inherits(layer$geom, "GeomRibbon")
  }, logical(1))))
})
```

- [ ] **Step 2: Run focused companion tests and confirm the RED state**

Run the Task 1 focused command.

Expected: failures report missing or incorrect print, summary, and autoplot dispatch.

- [ ] **Step 3: Implement the three companion methods in the shared files**

Add methods following the adjacent RHF methods:

```r
print.gg_tune_rhf <- function(x, ...) {
  selected <- x[x$selected, , drop = FALSE]
  cat(.gg_header(x, "gg_tune_rhf"),
      sprintf("  |  metric: %s  evaluations: %d",
              x$metric[1L], nrow(x)),
      sprintf("  |  selected treesize: %d  value: %.4g",
              selected$treesize, selected$value),
      "\n", sep = "")
  invisible(x)
}

summary.gg_tune_rhf <- function(object, ...) {
  selected <- object[object$selected, , drop = FALSE]
  data.frame(
    metric = selected$metric,
    treesize = selected$treesize,
    value = selected$value,
    se = selected$se,
    n_evaluations = nrow(object),
    stringsAsFactors = FALSE
  )
}

autoplot.gg_tune_rhf <- function(object, ...) {
  plot(object, ...)
}
```

Add `gg_tune_rhf` to the documented class lists on the shared print, summary, and autoplot pages.

- [ ] **Step 4: Extend the generic autoplot-equivalence object list**

Inside `test_autoplot_equivalence.R`, append a synthetic object that has no package or RNG cost:

```r
objects[["gg_tune_rhf (iAUC)"]] <- gg_tune_rhf(.fake_rhf_tune_iauc())
```

Update only the top comment's family count if it remains a literal count after documentation regeneration. Do not add a duplicate vdiffr baseline for autoplot.

- [ ] **Step 5: Run focused extractor, plot, and equivalence tests**

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_gg_tune_rhf.R"); testthat::test_file("tests/testthat/test_plot_gg_tune_rhf.R"); testthat::test_file("tests/testthat/test_autoplot_equivalence.R")'
```

Expected: zero failures and zero errors.

- [ ] **Step 6: Commit the S3 companions**

```bash
git add R/print_methods.R R/summary_methods.R R/autoplot_methods.R tests/testthat/test_gg_tune_rhf.R tests/testthat/test_autoplot_equivalence.R
git commit -m "feat: complete RHF tuning S3 family"
```

### Task 4: Pin the installed randomForestRHF API and default plot

**Files:**
- Modify: `tests/testthat/helper-rhf-fixtures.R`
- Modify: `tests/testthat/test_gg_tune_rhf.R`
- Modify: `tests/testthat/test_snapshots.R`
- Create during vdiffr approval: `tests/testthat/_snaps/snapshots/gg-tune-rhf-iauc.svg`

**Interfaces:**
- Consumes: `randomForestRHF::hazard.simulation(1)` and `randomForestRHF::tune.iAUC.rhf()` 1.0.1 API.
- Produces: `.rhf_tune_iauc()` session-memoised fixture and one default iAUC tuning snapshot.

- [ ] **Step 1: Add a small memoised upstream tuning fixture**

Append to `helper-rhf-fixtures.R`:

```r
.rhf_tune_iauc <- function() {
  if (is.null(.rhf_cache$tune_iauc)) {
    if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
      testthat::skip("randomForestRHF not installed")
    }
    set.seed(20260825L)
    simulated <- randomForestRHF::hazard.simulation(1)
    .rhf_cache$tune_iauc <- randomForestRHF::tune.iAUC.rhf(
      "Surv(id, start, stop, event) ~ .",
      simulated$dta,
      ntree = 12L,
      lower = 2L,
      upper = 5L,
      max.evals = 4L,
      seed = 20260825L,
      verbose = FALSE,
      forest = FALSE
    )
  }
  .rhf_cache$tune_iauc
}
```

Before retaining these exact arguments, time the call once. If it exceeds 15 seconds locally, reduce only `ntree` while retaining at least three evaluated sizes and a unique optimum. Record the measured duration in a code comment next to the fixture.

- [ ] **Step 2: Write the guarded real-API integration test**

```r
test_that("gg_tune_rhf accepts a real CRAN randomForestRHF tuning result", {
  skip_on_cran()
  skip_if_not_installed("randomForestRHF", minimum_version = "1.0.1")
  set.seed(20260825L)
  fit <- .rhf_tune_iauc()
  out <- gg_tune_rhf(fit)

  expect_s3_class(fit, "tune.treesize.rhf")
  expect_s3_class(out, "gg_tune_rhf")
  expect_identical(out$treesize, fit$path$treesize)
  expect_equal(out$value, fit$path$iAUC)
  expect_identical(sum(out$selected), 1L)
  expect_identical(attr(out, "provenance")$randomForestRHF_version,
                   as.character(utils::packageVersion("randomForestRHF")))
})
```

- [ ] **Step 3: Run the integration test and resolve only API-contract failures**

Run the Task 1 focused command.

Expected: the real CRAN 1.0.1 object passes the same contract as the synthetic fixture. If upstream produces a tied optimum with the chosen seed, change the fixture seed or narrow bounds; do not weaken the approved unique-optimum validation silently.

- [ ] **Step 4: Add the guarded vdiffr test**

Add to the RHF section of `test_snapshots.R`:

```r
test_that("snapshot: gg_tune_rhf iAUC path", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("randomForestRHF", minimum_version = "1.0.1")
  skip_on_cran()
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  set.seed(20260825L)
  x <- gg_tune_rhf(.rhf_tune_iauc())
  vdiffr::expect_doppelganger("gg-tune-rhf-iauc", plot(x))
})
```

- [ ] **Step 5: Generate and inspect the new baseline last**

First record snapshot status:

```bash
git status --short tests/testthat/_snaps
```

Then run only the named snapshot block through the repository's normal test environment with both environment variables set. Review the pending figure with `testthat::snapshot_review()` and accept only `gg-tune-rhf-iauc.svg` with `testthat::snapshot_accept(files = "gg-tune-rhf-iauc.svg")`. Inspect it for: tree sizes in upstream order, a connected iAUC path, one selected point, a visible SE band when finite SE is present, and no printed plot side effect.

Expected: exactly one new SVG and no deleted or modified pre-existing baseline.

- [ ] **Step 6: Run focused tests with the snapshot guard and recheck integrity**

```bash
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test_gg_tune_rhf.R"); testthat::test_file("tests/testthat/test_plot_gg_tune_rhf.R"); testthat::test_file("tests/testthat/test_snapshots.R", desc = "gg_tune_rhf")'
git status --short tests/testthat/_snaps
git diff --name-status -- tests/testthat/_snaps
```

Expected: tests pass; the only snapshot delta against the task's starting commit is the new `gg-tune-rhf-iauc.svg`.

- [ ] **Step 7: Commit real-API and visual coverage**

```bash
git add tests/testthat/helper-rhf-fixtures.R tests/testthat/test_gg_tune_rhf.R tests/testthat/test_snapshots.R tests/testthat/_snaps/snapshots/gg-tune-rhf-iauc.svg
git commit -m "test: cover RHF tuning integration"
```

### Task 5: Publish the tuning family in CRAN-facing documentation

**Files:**
- Modify: `R/gg_tune_rhf.R`
- Modify: `R/plot.gg_tune_rhf.R`
- Modify: `R/help.R`
- Modify: `README.md`
- Modify: `_pkgdown.yml`
- Modify: `NEWS.md`
- Modify: `release-checklist-v4.0.0.md`
- Generate: `man/gg_tune_rhf.Rd`
- Generate: `man/plot.gg_tune_rhf.Rd`
- Generate: `man/print.gg.Rd`
- Generate: `man/summary.gg.Rd`
- Generate: `man/autoplot.gg.Rd`
- Generate: `man/ggRandomForests-package.Rd`
- Generate: `NAMESPACE`

**Interfaces:**
- Consumes: the complete public API and behavior from Tasks 1 through 4.
- Produces: discoverable help, README, NEWS, pkgdown indexing, generated S3 registrations, and a release-checklist disposition for PR 2.

- [ ] **Step 1: Finish the extractor and plot roxygen in the package voice**

The extractor page must contain this workflow, with a deliberately small guarded example:

```r
tune_fit <- randomForestRHF::tune.iAUC.rhf(
  "Surv(id, start, stop, event) ~ .",
  simulated$dta,
  ntree = 12L,
  lower = 2L,
  upper = 5L,
  verbose = FALSE,
  forest = FALSE
)
tuning <- gg_tune_rhf(tune_fit)
plot(tuning)
```

State plainly that the expensive step is upstream tuning, retaining `tune_fit` avoids repeating it, and `gg_tune_rhf()` only prepares the saved search path for inspection and plotting. Describe OOB risk as minimized and OOB iAUC as maximized. Do not describe either metric as a p-value, threshold, or inferential uncertainty measure.

- [ ] **Step 2: Update package-level discovery surfaces**

- Add `gg_tune_rhf()` to the RHF extractor list in `R/help.R` as tree-size tuning by OOB risk or OOB iAUC.
- Add a README function-table row mapping `gg_tune_rhf()` to a `tune.treesize.rhf` object and an inspected tuning path.
- Update the README v4 development summary from three RHF families to four and name tuning last.
- Add `gg_tune_rhf` and `plot.gg_tune_rhf` after the other RHF topics in `_pkgdown.yml`.
- Add one v4 NEWS bullet describing the supplied-object-only API, five returned columns, selected-size marker, and conditional iAUC SE ribbon.
- In `release-checklist-v4.0.0.md`, change only the “RHF tuning family” audit row from `deferred` to `corrected`, cite the new source/help/tests, and add a new chronological “PR 2 verification” section after final verification. Do not check the RHF vignette, full release verification, authorization, submission, or CRAN acceptance gates.

- [ ] **Step 3: Regenerate documentation first**

```bash
Rscript -e 'devtools::document()'
```

Expected: `NAMESPACE` exports `gg_tune_rhf`; registers `gg_tune_rhf.tune.treesize.rhf`, `plot.gg_tune_rhf`, `print.gg_tune_rhf`, `summary.gg_tune_rhf`, and `autoplot.gg_tune_rhf`; the listed Rd files are created or updated.

- [ ] **Step 4: Audit generated documentation and public mappings**

```bash
rg -n "gg_tune_rhf|tune\.treesize\.rhf|tune\.iAUC\.rhf|arXiv\.2608\.21597|randomForestRHF" R man README.md NEWS.md _pkgdown.yml release-checklist-v4.0.0.md
rg -n "library\(randomForestRHF\)|gg_tune_rhf\([^)]*rhf|calculat(e|es|ed).*inside gg_tune_rhf" R man README.md NEWS.md
```

Expected: the first search shows consistent qualified upstream calls, class names, and citations; the second returns no misleading attachment, fitted-forest input, or claim that the extractor performs tuning.

- [ ] **Step 5: Run lint after documentation**

```bash
Rscript -e 'lintr::lint_package()'
```

Expected: zero lints.

- [ ] **Step 6: Run the guarded full suite and preserve every snapshot**

```bash
git status --short tests/testthat/_snaps
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'
git status --short tests/testthat/_snaps
git diff --name-status -- tests/testthat/_snaps
```

Expected: zero failures and zero errors; the expected new tuning SVG remains and no pre-existing baseline is deleted or modified.

- [ ] **Step 7: Commit generated documentation and public discovery updates**

```bash
git add R/gg_tune_rhf.R R/plot.gg_tune_rhf.R R/help.R README.md _pkgdown.yml NEWS.md release-checklist-v4.0.0.md NAMESPACE man
git commit -m "docs: publish RHF tuning family"
```

### Task 6: Perform PR-level verification and open the pull request

**Files:**
- Modify with evidence only: `release-checklist-v4.0.0.md`
- Verify: all tracked package files

**Interfaces:**
- Consumes: the complete feature branch from Tasks 1 through 5.
- Produces: fresh definition-of-done evidence, a clean-archive CRAN check, and a pull request targeting `dev_rhf`; it does not merge the pull request.

- [ ] **Step 1: Re-run the definition of done in the required order**

```bash
Rscript -e 'devtools::document()'
Rscript -e 'lintr::lint_package()'
git status --short tests/testthat/_snaps
NOT_CRAN=true VDIFFR_RUN_TESTS=true Rscript -e 'devtools::test()'
git status --short tests/testthat/_snaps
git diff --name-status -- tests/testthat/_snaps
```

Expected: documentation exits 0; lint reports zero lints; tests report zero failures and zero errors; no pre-existing snapshot changes or deletions appear.

- [ ] **Step 2: Build from a clean archive and inspect the tarball**

Export `HEAD` into a new `mktemp -d` directory, run `R CMD build` there with an isolated temporary home if Quarto needs it, and inspect:

```bash
tar tzf ggRandomForests_4.0.0.tar.gz | grep -E '/\.[^/]+'
tar xzf ggRandomForests_4.0.0.tar.gz -O ggRandomForests/DESCRIPTION | sed -n '4,5p'
tar tzf ggRandomForests_4.0.0.tar.gz | grep -c cran-comments
```

Expected: hidden-file output is only `ggRandomForests/.Rinstignore`; DESCRIPTION reports `Version: 4.0.0` and `Date: 2026-08-05`; the `cran-comments` count is 0.

- [ ] **Step 3: Run the manual-inclusive clean-archive CRAN check**

```bash
R CMD check --as-cran ggRandomForests_4.0.0.tar.gz
```

Expected: 0 errors and 0 warnings. Review every NOTE; the known incoming-feasibility update-frequency NOTE may remain, but a documentation, namespace, example, dependency, timing, or citation NOTE blocks the pull request until resolved.

- [ ] **Step 4: Record fresh PR 2 evidence without advancing release gates**

Append the exact command date, test pass/fail/warning/skip totals, snapshot result, tarball inspection, and check summary to the PR 2 chronological section in `release-checklist-v4.0.0.md`. Keep the RHF vignette, full release verification, maintainer authorization, submission, and CRAN acceptance rows pending.

- [ ] **Step 5: Commit the verification record**

```bash
git add release-checklist-v4.0.0.md
git commit -m "docs: record RHF tuning verification"
```

- [ ] **Step 6: Self-review the complete branch diff**

```bash
git diff --check origin/dev_rhf...HEAD
git diff --stat origin/dev_rhf...HEAD
git log --oneline origin/dev_rhf..HEAD
git status --short --branch
```

Confirm that the diff contains the extractor, plot, S3 methods, tests, one new baseline, generated documentation, discovery updates, and verification evidence only. Confirm there is no optional forest copied into returned data/provenance and no tuning call in production code.

- [ ] **Step 7: Push the feature branch and open the PR**

```bash
git push -u origin codex/rhf-v4-tuning
gh pr create --base dev_rhf --head codex/rhf-v4-tuning --title "Add RHF tree-size tuning visualization" --body-file /tmp/ggRandomForests-rhf-tuning-pr.md
```

The PR body must summarize the supplied-object boundary, risk/iAUC behavior, conditional uncertainty ribbon, real CRAN API coverage, documentation surfaces, and fresh verification. It must state that the release hold remains in place pending the RHF vignette, full release verification, explicit authorization, submission, and CRAN acceptance.

- [ ] **Step 8: Stop for review**

Report the PR URL and verification results. Do not merge it, start the vignette PR, alter release gates, tag, or submit to CRAN until the maintainer directs the next step.
