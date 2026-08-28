# SHAP analysis for rfsrc — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a SHAP explanation surface to ggRandomForests — a `gg_shap` tidy object plus importance/beeswarm/dependence plots — for regression and classification `rfsrc`/`randomForest` forests.

**Architecture:** Follow the package's `gg_*` extract → arrange → plot idiom. `gg_shap()` wraps `kernelshap::kernelshap()` (model-agnostic, driven by the model's `predict()`), pulling predictors from `object$xvar`, and returns a long tidy `data.frame`. `plot.gg_shap()` routes by `type` to three exported ggplot builders.

**Tech Stack:** R, S3, `kernelshap` (new — `Suggests`), `ggplot2`, `tidyr`, `dplyr`, `testthat`, `vdiffr`, `roxygen2`.

---

## Revision note (read first)

The original plan wrapped `fastshap::explain()`. During Task 1, `fastshap`
turned out to have been **removed from CRAN on 2026-05-27** ("issues were not
corrected despite reminders") and would not install — a hard blocker, since
ggRandomForests is itself a CRAN package and cannot carry a `Suggests` on a
package outside a mainstream repository. This revision swaps the engine to
**`kernelshap`** (CRAN 0.9.1, actively maintained), confirmed live:

```r
kernelshap::kernelshap(object, X, bg_X = NULL, pred_fun = stats::predict,
                        ..., verbose = TRUE, seed = NULL)
# returns a list with:
#   $S        - plain numeric matrix, nrow(X) x length(feature_names)
#   $baseline - scalar: mean prediction over the background sample
```

Confirmed against a live `rfsrc` regression fit (`airquality`, 5 predictors,
111 obs): explaining the full training set with a 20-row background sample
took ~0.3s (`exact` mode kicks in automatically for ≤ 8 features, which
covers every test dataset used below — no per-test row subsetting needed).

Net effect on the design: `gg_shap()`'s Monte-Carlo `nsim` argument becomes
`bg_n` (background sample size); `pred_wrapper` becomes `pred_fun`; the
`baseline` attribute now comes directly from kernelshap's own `$baseline`
rather than a separate mean-prediction call. The tidy object schema
(`id`/`vars`/`shap`/`value`/`value_label`) and every plot function are
unchanged — the engine lives entirely inside `gg_shap()`.

---

## Notes for the implementer (read first)

- **NAMESPACE is generated.** Never edit `NAMESPACE` by hand. Use roxygen `@export` / `@rdname` tags and run `devtools::document()`.
- **`kernelshap` is in `Suggests`.** Every example, and every test, must guard with `requireNamespace("kernelshap", quietly = TRUE)` / `testthat::skip_if_not_installed("kernelshap")`. It is already installed locally (confirmed during replanning, CRAN 0.9.1) — if a fresh environment lacks it: `install.packages("kernelshap")`.
- **kernelshap contract:** `kernelshap::kernelshap(object, X, bg_X, pred_fun, verbose = FALSE)` where `X` is a `data.frame` of predictors to explain, `bg_X` is a `data.frame` background/reference sample, and `pred_fun(object, newdata)` returns a numeric vector of length `nrow(newdata)`. Returns a list; use `$S` (matrix, `nrow(X) × ncol(X)`, column names = predictor names) and `$baseline` (scalar).
- **Provenance helper:** `.set_provenance(gg_dta, object)` (in `R/print_helpers.R`) — call it last, like every other `gg_*`.
- **Run a single test file:** `devtools::test(filter = "gg_shap")` (the `filter` matches the test file name without the `test-` prefix / `.R`).
- **Determinism:** kernelshap samples a background set and (for > 8 features) a coalition sample. All test datasets here have ≤ 5 predictors, so kernelshap runs in exact mode — deterministic given a fixed background sample. Still call `set.seed()` before `gg_shap()` in every test for reproducibility of the background sampling.

---

## File Structure

- Create `R/gg_shap.R` — `gg_shap()` generic + `.default`/`.rfsrc`/`.randomForest` methods and the internal reshape helper.
- Create `R/plot.gg_shap.R` — `plot.gg_shap()` router + `shap_importance()`, `shap_beeswarm()`, `shap_dependence()`.
- Modify `R/autoplot_methods.R` — add `autoplot.gg_shap` one-liner + doc entry.
- Modify `DESCRIPTION` — add `kernelshap` to `Suggests`; bump version.
- Modify `NEWS.md` — bump version line + changelog entry.
- Create `tests/testthat/test-gg_shap.R` — object + plot tests.
- Snapshots land under `tests/testthat/_snaps/gg_shap/` (vdiffr, generated).

---

## Task 1: Add `kernelshap` to Suggests (supersedes prior `fastshap` commit)

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Check for a stale `fastshap` entry and remove it**

Run: `grep -n "fastshap" DESCRIPTION`
If a `fastshap,` line is present under `Suggests:` (from the superseded attempt), remove that line.

- [ ] **Step 2: Add `kernelshap`**

In `DESCRIPTION`, under `Suggests:`, add `kernelshap` in alphabetical order (the list is one comma-terminated entry per line). For example, insert it between `ggraph,` and `knitr,` — check the current file for the correct alphabetical neighbors, since other tasks may have touched this list.

- [ ] **Step 3: Verify it installs and parses**

Run: `Rscript -e 'stopifnot(requireNamespace("kernelshap", quietly = TRUE)); cat("ok\n")'`
Expected: `ok` (install first with `Rscript -e 'install.packages("kernelshap")'` if missing).

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION
git commit -m "build: switch gg_shap engine from fastshap to kernelshap

fastshap was removed from CRAN on 2026-05-27 and will not install.
kernelshap is the CRAN-current, actively maintained equivalent."
```

---

## Task 2: `gg_shap()` generic, `.default`, and `.rfsrc` regression

**Files:**
- Create: `R/gg_shap.R`
- Test: `tests/testthat/test-gg_shap.R`

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-gg_shap.R`:

```r
test_that("gg_shap.rfsrc returns a long tidy object for regression", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  dta <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = dta, ntree = 50)

  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(gg_dta, "gg_shap")
  expect_true(all(c("id", "vars", "shap", "value", "value_label") %in%
                    colnames(gg_dta)))

  n_obs  <- nrow(rf$xvar)
  n_vars <- ncol(rf$xvar)
  expect_equal(nrow(gg_dta), n_obs * n_vars)
  expect_type(gg_dta$shap, "double")
  expect_true(is.factor(gg_dta$vars))
})

test_that("gg_shap.default errors on a non-forest object", {
  expect_error(gg_shap(lm(mpg ~ wt, mtcars)), "rfsrc.*randomForest")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: FAIL — `could not find function "gg_shap"`.

- [ ] **Step 3: Write minimal implementation**

Create `R/gg_shap.R`:

```r
#' SHAP (Shapley additive explanations) data object
#'
#' \code{gg_shap} computes SHAP values for a
#' \code{\link[randomForestSRC]{rfsrc}} or
#' \code{\link[randomForest]{randomForest}} regression or classification forest
#' by wrapping \code{\link[kernelshap]{kernelshap}}, and reshapes them into a
#' tidy data set with one row per (observation, variable).
#'
#' @param object A \code{\link[randomForestSRC]{rfsrc}} or
#'   \code{\link[randomForest]{randomForest}} object (regression or
#'   classification).
#' @param newdata Optional \code{data.frame} of predictor values to explain
#'   (same columns as the model's training predictors). When missing, the
#'   model's own training predictors are used.
#' @param bg_n Size of the background/reference sample drawn from the
#'   training predictors and passed to \code{\link[kernelshap]{kernelshap}}
#'   as \code{bg_X}. Larger values are more accurate but slower.
#' @param which.class For classification forests, the class (integer column
#'   index into the predicted-probability matrix) whose predicted probability
#'   is explained. Defaults to 1.
#' @param ... Passed through to \code{\link[kernelshap]{kernelshap}} (e.g.
#'   \code{seed}, \code{exact}, \code{max_iter}).
#'
#' @return A \code{gg_shap} object: a \code{data.frame} with columns
#'   \code{id} (observation index), \code{vars} (variable name, an ordered
#'   factor ranked by mean absolute SHAP), \code{shap} (the signed SHAP
#'   contribution), \code{value} (numeric feature value, \code{NA} for
#'   categorical features), and \code{value_label} (feature value as
#'   character). The background-sample mean prediction is stored in the
#'   \code{"baseline"} attribute.
#'
#' @seealso \code{\link{plot.gg_shap}} \code{\link[kernelshap]{kernelshap}}
#'
#' @examples
#' \donttest{
#' if (requireNamespace("kernelshap", quietly = TRUE)) {
#'   rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
#'                                ntree = 50)
#'   gg_dta <- gg_shap(rf, bg_n = 20)
#'   plot(gg_dta)
#' }
#' }
#'
#' @aliases gg_shap gg_shap.rfsrc gg_shap.randomForest
#' @export
gg_shap <- function(object, newdata, bg_n = 50, which.class = 1, ...) {
  UseMethod("gg_shap", object)
}

#' @export
gg_shap.default <- function(object, newdata, bg_n = 50, which.class = 1, ...) {
  stop("gg_shap: expected an 'rfsrc' or 'randomForest' object; got an object ",
       "of class ", paste(class(object), collapse = "/"), ".", call. = FALSE)
}

#' @export
gg_shap.rfsrc <- function(object, newdata, bg_n = 50, which.class = 1, ...) {
  if (!requireNamespace("kernelshap", quietly = TRUE)) {
    stop("gg_shap requires the 'kernelshap' package. Install it with ",
         "install.packages('kernelshap').", call. = FALSE)
  }

  x_train <- object$xvar
  x_explain <- if (missing(newdata) || is.null(newdata)) x_train else newdata
  bg_x <- x_train[sample.int(nrow(x_train), min(bg_n, nrow(x_train))), ,
                  drop = FALSE]

  is_class <- object$family == "class"
  pred_fun <- function(object, newdata) {
    pr <- predict(object, newdata)$predicted
    if (is_class) as.numeric(pr[, which.class]) else as.numeric(pr)
  }

  res <- kernelshap::kernelshap(object, X = x_explain, bg_X = bg_x,
                                pred_fun = pred_fun, verbose = FALSE, ...)

  .gg_shap_reshape(res$S, x_explain, res$baseline, object,
                   bg_n = bg_n, which.class = which.class)
}

# Internal: turn a SHAP matrix (obs x vars) + the explained predictors into a
# long tidy gg_shap data.frame. Not exported.
.gg_shap_reshape <- function(sv, x_explain, baseline, object,
                             bg_n, which.class) {
  sv <- as.data.frame(sv)
  n <- nrow(sv)
  vars <- colnames(sv)

  sv$id <- seq_len(n)
  shap_long <- tidyr::pivot_longer(sv, cols = tidyr::all_of(vars),
                                   names_to = "vars", values_to = "shap")

  # numeric feature value (NA for non-numeric columns), for beeswarm coloring
  num_mat <- vapply(x_explain, function(col) {
    if (is.numeric(col)) as.numeric(col) else rep(NA_real_, length(col))
  }, numeric(n))
  val_num <- data.frame(
    id    = rep(seq_len(n), times = length(vars)),
    vars  = rep(vars, each = n),
    value = as.vector(num_mat),
    stringsAsFactors = FALSE
  )
  val_lab <- data.frame(
    id          = rep(seq_len(n), times = length(vars)),
    vars        = rep(vars, each = n),
    value_label = as.vector(vapply(x_explain[vars], as.character,
                                   character(n))),
    stringsAsFactors = FALSE
  )

  gg_dta <- merge(merge(shap_long, val_num, by = c("id", "vars")),
                  val_lab, by = c("id", "vars"))

  # rank variables by mean absolute SHAP; reverse levels so the most important
  # plots at the top after coord_flip (matching plot.gg_vimp).
  rank <- stats::aggregate(abs(gg_dta$shap),
                           by = list(vars = gg_dta$vars), FUN = mean)
  ord <- rank$vars[order(rank$x, decreasing = TRUE)]
  gg_dta$vars <- factor(gg_dta$vars, levels = rev(as.character(ord)))

  attr(gg_dta, "baseline") <- baseline
  attr(gg_dta, "bg_n") <- bg_n
  attr(gg_dta, "which.class") <- which.class
  class(gg_dta) <- c("gg_shap", class(gg_dta))
  .set_provenance(gg_dta, object)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: PASS (both tests).

- [ ] **Step 5: Commit**

```bash
git add R/gg_shap.R tests/testthat/test-gg_shap.R
git commit -m "feat(gg_shap): rfsrc regression SHAP via kernelshap"
```

---

## Task 3: Classification `rfsrc` + `randomForest` method

**Files:**
- Modify: `R/gg_shap.R`
- Test: `tests/testthat/test-gg_shap.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-gg_shap.R`:

```r
test_that("gg_shap.rfsrc handles classification via which.class", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Species ~ ., data = iris, ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20, which.class = 2)

  expect_s3_class(gg_dta, "gg_shap")
  expect_equal(nrow(gg_dta), nrow(iris) * 4L)
  expect_equal(attr(gg_dta, "which.class"), 2)
})

test_that("gg_shap.randomForest works for regression", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  dta <- na.omit(airquality)
  rf <- randomForest::randomForest(Ozone ~ ., data = dta, ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(gg_dta, "gg_shap")
  expect_true(all(c("id", "vars", "shap") %in% colnames(gg_dta)))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: the classification test passes already (rfsrc branch handles it), but `gg_shap.randomForest` test FAILS — `no applicable method for 'gg_shap' applied to an object of class "randomForest"` (dispatch falls to `.default` which errors).

- [ ] **Step 3: Write minimal implementation**

Append to `R/gg_shap.R`:

```r
#' @export
gg_shap.randomForest <- function(object, newdata, bg_n = 50,
                                 which.class = 1, ...) {
  if (!requireNamespace("kernelshap", quietly = TRUE)) {
    stop("gg_shap requires the 'kernelshap' package. Install it with ",
         "install.packages('kernelshap').", call. = FALSE)
  }

  info <- .rf_recover_model_frame(object)
  if (is.null(info)) {
    stop("gg_shap: could not recover training predictors from this ",
         "randomForest object.", call. = FALSE)
  }
  x_train <- info$model_frame[, setdiff(colnames(info$model_frame),
                                        info$response_name), drop = FALSE]
  x_explain <- if (missing(newdata) || is.null(newdata)) x_train else newdata
  bg_x <- x_train[sample.int(nrow(x_train), min(bg_n, nrow(x_train))), ,
                  drop = FALSE]

  is_class <- object$type == "classification"
  pred_fun <- function(object, newdata) {
    if (is_class) {
      as.numeric(predict(object, newdata, type = "prob")[, which.class])
    } else {
      as.numeric(predict(object, newdata))
    }
  }

  res <- kernelshap::kernelshap(object, X = x_explain, bg_X = bg_x,
                                pred_fun = pred_fun, verbose = FALSE, ...)

  .gg_shap_reshape(res$S, x_explain, res$baseline, object,
                   bg_n = bg_n, which.class = which.class)
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: PASS (all four tests).

- [ ] **Step 5: Commit**

```bash
git add R/gg_shap.R tests/testthat/test-gg_shap.R
git commit -m "feat(gg_shap): classification which.class + randomForest method"
```

---

## Task 4: `shap_importance()` + `plot.gg_shap()` router

**Files:**
- Create: `R/plot.gg_shap.R`
- Test: `tests/testthat/test-gg_shap.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-gg_shap.R`:

```r
test_that("shap_importance and plot(type='importance') return ggplots", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(shap_importance(gg_dta), "ggplot")
  expect_s3_class(plot(gg_dta, type = "importance"), "ggplot")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: FAIL — `could not find function "shap_importance"`.

- [ ] **Step 3: Write minimal implementation**

Create `R/plot.gg_shap.R`:

```r
#' Plot a \code{\link{gg_shap}} object
#'
#' Routes to one of three SHAP views. \code{type = "beeswarm"} (default) draws
#' the signature SHAP summary; \code{"importance"} draws a mean-absolute-SHAP
#' bar chart; \code{"dependence"} draws SHAP value against a single feature's
#' value.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param type One of \code{"beeswarm"}, \code{"importance"}, or
#'   \code{"dependence"}.
#' @param xvar For \code{type = "dependence"}, the variable to plot. When
#'   \code{NULL}, the top-ranked variable is used.
#' @param ... Passed to the underlying builder.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{gg_shap}} \code{\link{shap_importance}}
#'   \code{\link{shap_beeswarm}} \code{\link{shap_dependence}}
#'
#' @examples
#' \donttest{
#' if (requireNamespace("kernelshap", quietly = TRUE)) {
#'   rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
#'                                ntree = 50)
#'   gg_dta <- gg_shap(rf, bg_n = 20)
#'   plot(gg_dta, type = "importance")
#' }
#' }
#'
#' @export
plot.gg_shap <- function(x, type = c("beeswarm", "importance", "dependence"),
                         xvar = NULL, ...) {
  type <- match.arg(type)
  switch(type,
         beeswarm   = shap_beeswarm(x, ...),
         importance = shap_importance(x, ...),
         dependence = shap_dependence(x, xvar = xvar, ...))
}

#' SHAP global importance bar chart
#'
#' Bar chart of mean absolute SHAP value per variable — the SHAP analog of
#' \code{\link{plot.gg_vimp}}.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gg_shap}} \code{\link{plot.gg_shap}}
#' @export
shap_importance <- function(x, ...) {
  imp <- dplyr::summarise(dplyr::group_by(x, .data$vars),
                          mean_abs = mean(abs(.data$shap)), .groups = "drop")
  ggplot2::ggplot(imp) +
    ggplot2::geom_bar(
      ggplot2::aes(x = .data$vars, y = .data$mean_abs),
      stat = "identity", width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "mean(|SHAP|)")
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: FAIL — the router calls `shap_beeswarm`/`shap_dependence` which don't exist yet, but `shap_importance` and `plot(type="importance")` should PASS. If `plot()` default `type = "beeswarm"` is exercised anywhere it will error; the test only calls `type = "importance"`, so it passes. Confirm the two assertions pass.

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_shap.R tests/testthat/test-gg_shap.R
git commit -m "feat(gg_shap): shap_importance + plot.gg_shap router"
```

---

## Task 5: `shap_beeswarm()`

**Files:**
- Modify: `R/plot.gg_shap.R`
- Test: `tests/testthat/test-gg_shap.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-gg_shap.R`:

```r
test_that("shap_beeswarm and default plot() return ggplots", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(shap_beeswarm(gg_dta), "ggplot")
  expect_s3_class(plot(gg_dta), "ggplot")   # default type = "beeswarm"
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: FAIL — `could not find function "shap_beeswarm"`.

- [ ] **Step 3: Write minimal implementation**

Append to `R/plot.gg_shap.R`:

```r
#' SHAP beeswarm summary plot
#'
#' The signature SHAP summary: one jittered point per (observation, variable),
#' positioned by SHAP value and colored by the (scaled) feature value.
#' Categorical features have no numeric value and render uncolored.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gg_shap}} \code{\link{plot.gg_shap}}
#' @export
shap_beeswarm <- function(x, ...) {
  ggplot2::ggplot(x, ggplot2::aes(x = .data$shap, y = .data$vars)) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
    ggplot2::geom_jitter(ggplot2::aes(colour = .data$value),
                         height = 0.2, width = 0, alpha = 0.6) +
    ggplot2::scale_colour_viridis_c(name = "Feature value") +
    ggplot2::labs(x = "SHAP value (impact on prediction)", y = "")
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_shap.R tests/testthat/test-gg_shap.R
git commit -m "feat(gg_shap): shap_beeswarm summary plot"
```

---

## Task 6: `shap_dependence()`

**Files:**
- Modify: `R/plot.gg_shap.R`
- Test: `tests/testthat/test-gg_shap.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-gg_shap.R`:

```r
test_that("shap_dependence honors xvar and defaults to top variable", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(shap_dependence(gg_dta, xvar = "Temp"), "ggplot")
  expect_s3_class(shap_dependence(gg_dta), "ggplot")            # NULL -> top var
  expect_error(shap_dependence(gg_dta, xvar = "not_a_var"), "not_a_var")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: FAIL — `could not find function "shap_dependence"`.

- [ ] **Step 3: Write minimal implementation**

Append to `R/plot.gg_shap.R`:

```r
#' SHAP dependence plot
#'
#' SHAP value against the value of a single feature — the SHAP analog of a
#' partial-dependence plot. Numeric features use a continuous x-axis; factor
#' or character features fall back to their labels on a discrete axis.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param xvar The variable to plot. When \code{NULL}, the top-ranked variable
#'   (largest mean absolute SHAP) is used.
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gg_shap}} \code{\link{plot.gg_shap}}
#' @export
shap_dependence <- function(x, xvar = NULL, ...) {
  # vars levels are reversed (most important last); top variable is the last
  # level.
  if (is.null(xvar)) {
    xvar <- utils::tail(levels(x$vars), 1)
  }
  if (!xvar %in% levels(x$vars)) {
    stop("shap_dependence: '", xvar, "' is not a variable in this gg_shap ",
         "object.", call. = FALSE)
  }

  sub <- x[as.character(x$vars) == xvar, , drop = FALSE]
  is_numeric_feature <- any(!is.na(sub$value))

  gg_plt <- ggplot2::ggplot(sub) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, colour = "grey60") +
    ggplot2::labs(x = xvar, y = paste("SHAP value for", xvar))

  if (is_numeric_feature) {
    gg_plt + ggplot2::geom_point(
      ggplot2::aes(x = .data$value, y = .data$shap), alpha = 0.6)
  } else {
    gg_plt + ggplot2::geom_boxplot(
      ggplot2::aes(x = .data$value_label, y = .data$shap))
  }
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/plot.gg_shap.R tests/testthat/test-gg_shap.R
git commit -m "feat(gg_shap): shap_dependence plot"
```

---

## Task 7: `autoplot.gg_shap` + document (regenerate NAMESPACE/Rd)

**Files:**
- Modify: `R/autoplot_methods.R`
- Test: `tests/testthat/test-gg_shap.R`

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-gg_shap.R`:

```r
test_that("autoplot.gg_shap delegates to plot", {
  skip_if_not_installed("kernelshap")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  expect_s3_class(ggplot2::autoplot(gg_dta), "ggplot")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: FAIL — no `autoplot` method for `gg_shap` (dispatches to default and errors).

- [ ] **Step 3: Write minimal implementation**

In `R/autoplot_methods.R`, add `\item{\code{gg_shap}}{SHAP explanations}` to the `\describe{}` list in the `@details`, and append at the end of the file:

```r
#' @rdname autoplot.gg
#' @export
autoplot.gg_shap <- function(object, ...) {
  plot(object, ...)
}
```

- [ ] **Step 4: Regenerate docs and NAMESPACE, then run tests**

Run: `Rscript -e 'devtools::document()'`
Expected: writes `man/gg_shap.Rd`, `man/plot.gg_shap.Rd`, `man/shap_importance.Rd`, `man/shap_beeswarm.Rd`, `man/shap_dependence.Rd`, and adds `S3method(autoplot,gg_shap)`, `S3method(gg_shap,*)`, `export(gg_shap)`, `export(shap_importance)`, `export(shap_beeswarm)`, `export(shap_dependence)`, `S3method(plot,gg_shap)` to `NAMESPACE`.

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "gg_shap")'`
Expected: PASS (all tests).

- [ ] **Step 5: Commit**

```bash
git add R/autoplot_methods.R NAMESPACE man/
git commit -m "feat(gg_shap): autoplot method + generated docs"
```

---

## Task 8: vdiffr snapshots for the three plots

**Files:**
- Test: `tests/testthat/test-gg_shap.R`
- Generated: `tests/testthat/_snaps/gg_shap/`

- [ ] **Step 1: Write the snapshot tests**

Append to `tests/testthat/test-gg_shap.R`:

```r
test_that("gg_shap plots are visually stable", {
  skip_if_not_installed("kernelshap")
  skip_if_not_installed("vdiffr")
  skip_on_cran()

  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
                               ntree = 50)
  set.seed(42)
  gg_dta <- gg_shap(rf, bg_n = 20)

  vdiffr::expect_doppelganger("shap-importance",
                              plot(gg_dta, type = "importance"))
  vdiffr::expect_doppelganger("shap-beeswarm",
                              plot(gg_dta, type = "beeswarm"))
  vdiffr::expect_doppelganger("shap-dependence",
                              plot(gg_dta, type = "dependence", xvar = "Temp"))
})
```

- [ ] **Step 2: Generate the baseline snapshots**

Run: `Rscript -e 'devtools::load_all(); testthat::snapshot_accept("gg_shap"); devtools::test(filter = "gg_shap")'`
Expected: snapshots written under `tests/testthat/_snaps/gg_shap/`; tests PASS.

Note (project practice): local runs may prune guarded vdiffr snapshots — confirm all three `.svg` files are present under `tests/testthat/_snaps/gg_shap/` before committing.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-gg_shap.R tests/testthat/_snaps/gg_shap/
git commit -m "test(gg_shap): vdiffr snapshots for the three plots"
```

---

## Task 9: Version bump + full check

**Files:**
- Modify: `DESCRIPTION`, `NEWS.md`

- [ ] **Step 1: Bump the patch version in both files**

In `DESCRIPTION` line 4-ish, change `Version: 3.4.1` → `Version: 3.4.2` and update `Date:` to the current date. In `NEWS.md`, update the top `Version:` line to `3.4.2` (a test greps NEWS for the exact DESCRIPTION version) and add a bullet:

```
* Added `gg_shap()` and `plot.gg_shap()` (with `shap_importance()`,
  `shap_beeswarm()`, `shap_dependence()`) for SHAP explanations of
  regression and classification forests, wrapping `kernelshap` (Suggests).
```

- [ ] **Step 2: Run the version-consistency test**

Run: `Rscript -e 'devtools::load_all(); devtools::test(filter = "news")'`
Expected: PASS (NEWS version matches DESCRIPTION). If the filter matches nothing, run the full suite in Step 3.

- [ ] **Step 3: Full test suite + document check**

Run: `Rscript -e 'devtools::document(); devtools::test()'`
Expected: 0 failures. SHAP tests skip on CRAN but run locally with `kernelshap` installed.

- [ ] **Step 4: R CMD check (as-cran, with manual)**

Run: `Rscript -e 'devtools::check()'`
Expected: 0 errors / 0 warnings. Notes acceptable only if pre-existing. Confirm overall check time stays within the < 10 min CRAN budget (SHAP examples are `\donttest` + guarded, so they should not run during check).

- [ ] **Step 5: Commit**

```bash
git add DESCRIPTION NEWS.md
git commit -m "release: gg_shap SHAP analysis (3.4.2)"
```

---

## Self-Review

**Spec coverage:**
- Engine = kernelshap in Suggests, guarded → Task 1, guards throughout. (Superseded `fastshap` choice recorded in the Revision note and reverted in Task 1 Step 1.)
- `gg_shap()` generic + `.default`/`.rfsrc`/`.randomForest`, `X` from `object$xvar` → Tasks 2, 3.
- Long tidy columns `id/vars/shap/value/value_label` + baseline/bg_n attrs + provenance → Task 2 (`.gg_shap_reshape`).
- `plot.gg_shap(type=)` router + `shap_importance/beeswarm/dependence` → Tasks 4, 5, 6.
- `autoplot.gg_shap` → Task 7.
- Factor-feature handling (numeric `value` NA, `value_label` discrete fallback) → Task 2 reshape + Task 6 dependence branch.
- classification `which.class` validation → implicit via column index; **gap:** no explicit out-of-range error. Acceptable for v1 (indexing errors surface from `predict`), but note for follow-up.
- Tests + vdiffr, guarded + skip_on_cran → Tasks 2–8.
- Patch version bump, NEWS+DESCRIPTION → Task 9.

**Placeholder scan:** none — all steps carry real code/commands.

**Type consistency:** `.gg_shap_reshape(sv, x_explain, baseline, object, bg_n, which.class)` signature matches both call sites (Tasks 2, 3). Column names `id/vars/shap/value/value_label` consistent across reshape, importance, beeswarm, dependence. `which.class`/`bg_n` attributes read only in tests.

One deliberate deferral recorded above: explicit `which.class` range validation is left to a follow-up.
