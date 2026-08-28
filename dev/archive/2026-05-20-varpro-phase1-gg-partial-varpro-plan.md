# ggRandomForests v2.8.0 — Phase 1: `gg_partial_varpro` — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rename `gg_partialpro` → `gg_partial_varpro`, add survival A+C paths with honest mortality documentation, promote `varPro` from `Suggests:` to `Imports:`, and leave a soft-deprecation shim for `gg_partialpro` for ≥1 CRAN cycle.

**Architecture:** New `R/gg_partial_varpro.R` (extractor) + `R/plot.gg_partial_varpro.R` (plot method) implement the canonical name. The existing `gg_partialpro` family becomes thin `.Deprecated()` shims that forward all arguments. Survival path C re-uses `gg_partial_rfsrc()` infrastructure by prepending `"gg_partial_varpro"` to the result's class vector and delegating via `NextMethod()`. The `provenance` attribute carries `source`, `family`, `ntree`, `n`, `scale`, `rmst_tau`, `xvar.names`, `path` — read by `print.gg_partial_varpro` via the shared `.gg_header()` helper.

**Tech Stack:** R (≥ 4.4.0), roxygen2 ≥ 8.0.0 (`Config/roxygen2/version: 8.0.0`), testthat (3.x), vdiffr, devtools, varPro (CRAN ≥ 3.1.0), randomForestSRC (≥ 3.4.0), randomForest, survival, ggplot2, patchwork, tidyr, dplyr.

**Pre-execution state:** This plan file was committed to `main` before execution begins. The executing engineer starts from a clean `main` at `bd5ebbb` (Version: `2.7.3.9001`). Do not re-create or re-commit the plan file during execution.

**Design spec:** `dev/plans/2026-05-20-varpro-phase1-gg-partial-varpro-design.md` (committed to `main`). The vault mirror is `~/Documents/ObsidianVault/R packages/ggRandomForests varPro Integration Design (2026-05-18).md` §5.

**Versioning:** `2.7.3.9001` (current main) → **`2.7.3.9002`** (this PR).

**roxygen2:** All `devtools::document()` calls in this plan use roxygen2 ≥ 8.0.0 (`Config/roxygen2/version: 8.0.0`). Do NOT pin to 7.3.3.

---

## File Structure

| File | Responsibility | Action |
|------|---------------|--------|
| `R/gg_partial_varpro.R` | Extractor: A-path + C-path dispatch, provenance, `@importFrom varPro partialpro` | **Create** |
| `R/plot.gg_partial_varpro.R` | `plot.gg_partial_varpro`: honest y-label, A-path rendering, C-path delegation | **Create** |
| `tests/testthat/test_gg_partial_varpro.R` | Full test suite for the new family | **Create** |
| `R/gg_partialpro.R` | Replace body with `.Deprecated()` + forward call | **Modify** |
| `R/plot.gg_partial.R` | Replace `plot.gg_partialpro` body with shim | **Modify** |
| `R/autoplot_methods.R` | Add `autoplot.gg_partial_varpro`; update `@details` list | **Modify** |
| `R/print_methods.R` | Add `print.gg_partial_varpro`; shim `print.gg_partialpro` | **Modify** |
| `R/summary_methods.R` | Add `summary.gg_partial_varpro`; shim `summary.gg_partialpro` | **Modify** |
| `DESCRIPTION` | `varPro` `Suggests:` → `Imports:`; version `2.7.3.9002`; date | **Modify** |
| `NAMESPACE` | Re-roxygenize: new S3 registrations + `importFrom(varPro,partialpro)` | **Regenerate** |
| `_pkgdown.yml` | Add `gg_partial_varpro`, `plot.gg_partial_varpro` under Partial Dependence | **Modify** |
| `NEWS.md` | Add `## ggRandomForests 2.7.3.9002` entry | **Modify** |
| `tests/testthat/test_gg_partialpro.R` | Add deprecation-shim regression guards | **Modify** |

---

## Task 0: Branch setup and dev cycle open

**Files:**
- Modify: `DESCRIPTION`
- Modify: `NEWS.md`

- [ ] **Step 1: Fetch + verify clean base**

```bash
cd ~/Documents/GitHub/ggRandomForests
git fetch origin
git log --oneline origin/main -3
```

Expected: top commit is `bd5ebbb v2.8 cycle: validate randomForest engine`.

- [ ] **Step 2: Create worktree + branch**

```bash
git worktree add .claude/worktrees/varpro-phase1 -b feat/varpro-phase1-gg-partial-varpro origin/main
cd .claude/worktrees/varpro-phase1
```

All subsequent steps in this plan run from `.claude/worktrees/varpro-phase1` unless stated otherwise.

- [ ] **Step 3: Verify full dependency closure is installed**

```bash
R -q -e 'for (p in c("varPro","randomForestSRC","randomForest","survival","devtools","roxygen2","testthat","vdiffr","tidyr","dplyr","ggplot2","patchwork","stringr","lintr")) cat(sprintf("%-18s %s\n", p, tryCatch(as.character(packageVersion(p)), error=function(e) "MISSING")))'
```

Expected: no `MISSING`. `varPro >= 3.1.0`, `randomForestSRC >= 3.4.0`, `roxygen2 >= 8.0.0`.

- [ ] **Step 4: Bump version + date in DESCRIPTION**

Edit `DESCRIPTION`, change:
```
Version: 2.7.3.9001
Date: 2026-05-19
```
to:
```
Version: 2.7.3.9002
Date: 2026-05-20
```

- [ ] **Step 5: Add NEWS stub**

At the top of `NEWS.md`, above the existing `Package: ggRandomForests` line, insert:

```
Package: ggRandomForests
Version: 2.7.3.9002

```

Then under the `ggRandomForests v2.8.0 (development)` section header (which starts after this new stub), add a placeholder bullet that will be replaced in Task 8:

```
* **varPro partial dependence: `gg_partial_varpro` (#84).** *(details added in Task 8)*
```

- [ ] **Step 6: Commit the dev-cycle open**

```bash
git add DESCRIPTION NEWS.md
git commit -m "chore: open v2.7.3.9002 dev cycle for varPro Phase 1"
```

---

## Task 1: New extractor `R/gg_partial_varpro.R` — A-path (TDD)

**Files:**
- Create: `R/gg_partial_varpro.R`
- Create: `tests/testthat/test_gg_partial_varpro.R`

### Step 1a: Write failing tests

- [ ] **Step 1: Create test file with failing tests**

Create `tests/testthat/test_gg_partial_varpro.R`:

```r
# Tests for gg_partial_varpro (Phase 1: A-path extractor)
# C-path tests are in Task 6.

## ── Helper: mock partialpro data ────────────────────────────────────────────
make_mock_vpro_data <- function(n_obs = 30, n_pts = 15) {
  set.seed(42)
  list(
    age = list(
      xvirtual    = seq(30, 80, length.out = n_pts),   # continuous: 15 > 10
      xorg        = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
      yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
      yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
      yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
    ),
    sex = list(
      xvirtual    = c(0, 1),                           # categorical: 2 <= 10
      xorg        = sample(c(0, 1), n_obs, replace = TRUE),
      yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs),
      yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
      yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
    )
  )
}

## ── Input validation ─────────────────────────────────────────────────────────
test_that("gg_partial_varpro: neither part_dta nor object → stop", {
  expect_error(gg_partial_varpro(), regexp = "at least one")
})

test_that("gg_partial_varpro: scale='surv' without object → stop", {
  expect_error(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "surv"),
    regexp = "requires 'object'"
  )
})

test_that("gg_partial_varpro: scale='chf' without object → stop", {
  expect_error(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "chf"),
    regexp = "requires 'object'"
  )
})

test_that("gg_partial_varpro: scale='rmst' without time → stop", {
  expect_error(
    gg_partial_varpro(part_dta = make_mock_vpro_data(), scale = "rmst"),
    regexp = "requires 'time'"
  )
})

## ── Class & structure ────────────────────────────────────────────────────────
test_that("gg_partial_varpro returns gg_partial_varpro class", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_s3_class(result, "gg_partial_varpro")
})

test_that("gg_partial_varpro has continuous and categorical elements", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_named(result, c("continuous", "categorical"))
  expect_s3_class(result$continuous, "data.frame")
  expect_s3_class(result$categorical, "data.frame")
})

test_that("gg_partial_varpro continuous has required columns", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_true(all(c("variable", "parametric", "nonparametric", "causal", "name")
                  %in% colnames(result$continuous)))
})

test_that("gg_partial_varpro categorical has required columns", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_true(all(c("parametric", "nonparametric", "causal", "variable", "name")
                  %in% colnames(result$categorical)))
})

test_that("gg_partial_varpro continuous: one row per xvirtual point (age)", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  age_rows <- result$continuous[result$continuous$name == "age", ]
  expect_equal(nrow(age_rows), 15L)
})

test_that("gg_partial_varpro: age is continuous, sex is categorical", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_true("age" %in% result$continuous$name)
  expect_false("age" %in% result$categorical$name)
  expect_true("sex" %in% result$categorical$name)
  expect_false("sex" %in% result$continuous$name)
})

## ── Provenance attribute ─────────────────────────────────────────────────────
test_that("gg_partial_varpro attaches provenance list attr", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  prov <- attr(result, "provenance")
  expect_type(prov, "list")
  expect_true(all(c("family", "scale", "rmst_tau", "xvar.names", "n", "path",
                    "source", "ntree")
                  %in% names(prov)))
})

test_that("gg_partial_varpro: provenance path = 'A' for A-path", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_equal(attr(result, "provenance")$path, "A")
})

## ── Scale resolution ─────────────────────────────────────────────────────────
test_that("gg_partial_varpro: scale='auto' no object → prov scale='generic'", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_equal(attr(result, "provenance")$scale, "generic")
})

test_that("gg_partial_varpro: scale='mortality' recorded in provenance", {
  result <- gg_partial_varpro(make_mock_vpro_data(), scale = "mortality")
  expect_equal(attr(result, "provenance")$scale, "mortality")
})

test_that("gg_partial_varpro: scale='rmst' with time stored in provenance", {
  result <- gg_partial_varpro(make_mock_vpro_data(), scale = "rmst", time = 365)
  prov <- attr(result, "provenance")
  expect_equal(prov$scale, "rmst")
  expect_equal(prov$rmst_tau, 365)
})

## ── nvars + model ────────────────────────────────────────────────────────────
test_that("gg_partial_varpro: nvars=1 processes only first variable", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1)
  expect_true("age" %in% result$continuous$name)
  expect_equal(nrow(result$categorical), 0L)
})

test_that("gg_partial_varpro: model arg adds column", {
  result <- gg_partial_varpro(make_mock_vpro_data(), model = "forest1")
  expect_true("model" %in% colnames(result$continuous))
  expect_true("model" %in% colnames(result$categorical))
  expect_equal(unique(result$continuous$model), "forest1")
})

test_that("gg_partial_varpro: no model arg → no model column", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_false("model" %in% colnames(result$continuous))
})
```

- [ ] **Step 2: Run tests — expect ALL to fail (function not found)**

```bash
Rscript -e "devtools::test(filter = 'test_gg_partial_varpro')"
```

Expected: all tests fail with `Error: could not find function "gg_partial_varpro"`.

### Step 1b: Implement the A-path extractor

- [ ] **Step 3: Create `R/gg_partial_varpro.R`**

```r
##=============================================================================
#' Partial dependence data from a varPro model
#'
#' Splits the list returned by \code{varpro::partialpro} into separate
#' data frames for continuous and categorical predictors, with provenance-
#' aware y-axis labeling for downstream plot methods.
#'
#' @param part_dta Partial plot data from \code{varpro::partialpro}.  Each
#'   element must contain \code{xvirtual}, \code{xorg}, \code{yhat.par},
#'   \code{yhat.nonpar}, and \code{yhat.causal}.  At least one of
#'   \code{part_dta} or \code{object} must be supplied.
#' @param object A fitted \code{varpro} object (the originating forest).
#'   When supplied, used for provenance metadata and — when \code{part_dta}
#'   is \code{NULL} — \code{varpro::partialpro(object)} is called
#'   internally.  Required when \code{scale \%in\% c("surv","chf")}.
#' @param scale Character; controls y-axis labeling and (for survival)
#'   the output type.  One of \code{"auto"} (default), \code{"mortality"},
#'   \code{"rmst"}, \code{"surv"}, or \code{"chf"}.
#' @param time Numeric; required when \code{scale = "rmst"} (the RMST
#'   horizon \eqn{\tau}), and when \code{scale \%in\% c("surv","chf")} to
#'   label the evaluation time point.
#' @param nvars Integer; number of variables (list elements) to process.
#'   Defaults to all variables in \code{part_dta}.
#' @param cat_limit Integer; variables with
#'   \code{length(xvirtual) <= cat_limit} are treated as categorical.
#'   Default \code{10}.
#' @param model Character; label appended to all rows (useful when
#'   combining results from multiple models in a single figure).
#'
#' @details
#' **Scale detection:** \code{scale = "auto"} with a supplied \code{object}
#' resolves to \code{"mortality"} for survival forests and \code{"generic"}
#' for regression/classification forests.  The RMST horizon \eqn{\tau} is
#' \emph{not} stored in the \code{varpro} object (varPro 3.1.0); pass
#' \code{scale = "rmst", time = \tau} explicitly for RMST-labeled output.
#'
#' **Ensemble mortality (scale = "mortality"):** The y-axis represents
#' \emph{ensemble mortality}: the expected number of events if the subject
#' experienced the study-average cumulative hazard, equivalent to the
#' \code{rfsrc} \code{predicted} value for survival forests (Ishwaran,
#' Kogalur, Blackstone & Lauer, 2008 <doi:10.1214/08-AOAS169>).  This is
#' an \strong{unbounded relative-risk score}---\emph{not} a survival
#' probability or \eqn{1 - S(t)}---and must not be interpreted as one.
#' For probability-scale output refit with
#' \code{varpro(\ldots, rmst = \tau)} and use \code{scale = "rmst"}.
#'
#' @return A named list of class \code{"gg_partial_varpro"} with elements:
#' \describe{
#'   \item{continuous}{data.frame with columns \code{variable},
#'     \code{parametric}, \code{nonparametric}, \code{causal}, \code{name}
#'     (and optionally \code{model}).}
#'   \item{categorical}{data.frame with the same columns but one row per
#'     observation per category level.}
#' }
#' A \code{"provenance"} attribute carries \code{source}, \code{family},
#' \code{ntree}, \code{n}, \code{scale}, \code{rmst_tau},
#' \code{xvar.names}, and \code{path}.
#'
#' @references
#' Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS (2008).
#' Random survival forests. \emph{The Annals of Applied Statistics},
#' \bold{2}(3), 841--860. \doi{10.1214/08-AOAS169}.
#'
#' @seealso \code{\link{plot.gg_partial_varpro}},
#'   \code{\link{gg_partialpro}} (deprecated),
#'   \code{\link{gg_partial_rfsrc}}, \code{\link{varpro_feature_names}}
#'
#' @examples
#' set.seed(42)
#' n_obs <- 30; n_pts <- 15
#' mock_data <- list(
#'   age = list(
#'     xvirtual    = seq(30, 80, length.out = n_pts),
#'     xorg        = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
#'     yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
#'     yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
#'     yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
#'   ),
#'   sex = list(
#'     xvirtual    = c(0, 1),
#'     xorg        = sample(c(0, 1), n_obs, replace = TRUE),
#'     yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs),
#'     yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
#'     yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
#'   )
#' )
#' result <- gg_partial_varpro(mock_data)
#' head(result$continuous)
#' head(result$categorical)
#'
#' @importFrom varPro partialpro
#' @export
gg_partial_varpro <- function(part_dta  = NULL,
                               object    = NULL,
                               scale     = c("auto", "rmst", "mortality",
                                             "surv", "chf"),
                               time      = NULL,
                               nvars     = NULL,
                               cat_limit = 10,
                               model     = NULL) {
  scale <- match.arg(scale)

  ## ---- Input validation --------------------------------------------------
  if (is.null(part_dta) && is.null(object)) {
    stop("at least one of 'part_dta' or 'object' must be supplied",
         call. = FALSE)
  }
  if (scale %in% c("surv", "chf") && is.null(object)) {
    stop("scale = '", scale, "' requires 'object' (the varpro fit)",
         call. = FALSE)
  }
  if (scale == "rmst" && is.null(time)) {
    stop("scale = 'rmst' requires 'time' (the RMST horizon tau)",
         call. = FALSE)
  }

  ## ---- C-path: route through gg_partial_rfsrc ----------------------------
  if (!is.null(object) && scale %in% c("surv", "chf")) {
    return(.gg_partial_varpro_cpath(object, scale, time, model))
  }

  ## ---- A-path: partialpro-based partial dependence -----------------------
  if (is.null(part_dta)) {
    part_dta <- varPro::partialpro(object)
  }

  ## Provenance fields from object (NA when no object supplied).
  prov_family <- if (!is.null(object)) object$family   else NA_character_
  prov_xvars  <- if (!is.null(object)) object$xvar.names else NA_character_
  prov_n      <- if (!is.null(object)) nrow(object$x)  else NA_integer_
  prov_ntree  <- if (!is.null(object)) object$max.tree  else NA_integer_

  scale_used  <- .resolve_varpro_scale(scale, prov_family)

  if (is.null(nvars)) {
    nvars <- length(part_dta)
  }

  cont_list <- list()
  cat_list  <- list()

  for (feature in seq(nvars)) {
    if (length(part_dta[[feature]]$xvirtual) > cat_limit) {
      ## -- Continuous: one row per xvirtual grid point ---------------------
      plt.df <- dplyr::bind_cols(
        variable      = part_dta[[feature]]$xvirtual,
        parametric    = colMeans(part_dta[[feature]]$yhat.par,    na.rm = TRUE),
        nonparametric = colMeans(part_dta[[feature]]$yhat.nonpar, na.rm = TRUE),
        causal        = colMeans(part_dta[[feature]]$yhat.causal,  na.rm = TRUE)
      )
      plt.df$name <- names(part_dta)[[feature]]
      cont_list[[feature]] <- plt.df

    } else {
      ## -- Categorical: stack per-observation rows per category level ------
      n_cats   <- length(unique(part_dta[[feature]]$xorg))
      cat_feat <- list()
      for (ind in seq(n_cats)) {
        cat_feat[[ind]] <- dplyr::bind_cols(
          parametric    = part_dta[[feature]]$yhat.par[, ind],
          nonparametric = part_dta[[feature]]$yhat.nonpar[, ind],
          causal        = part_dta[[feature]]$yhat.causal[, ind]
        )
        cat_feat[[ind]]$variable <- unique(part_dta[[feature]]$xorg)[ind]
        plt.df <- if (ind == 1L) cat_feat[[ind]] else
          dplyr::bind_rows(plt.df, cat_feat[[ind]])
      }
      plt.df$name <- names(part_dta)[[feature]]
      cat_list[[feature]] <- plt.df
    }
  }

  continuous  <- dplyr::bind_rows(cont_list)
  categorical <- dplyr::bind_rows(cat_list)

  if (!is.null(model)) {
    continuous$model <- model
    categorical$model <- model
  }

  result <- list(continuous = continuous, categorical = categorical)
  class(result) <- c("gg_partial_varpro", "list")

  attr(result, "provenance") <- list(
    source     = "varPro",
    family     = prov_family,
    ntree      = prov_ntree,
    n          = prov_n,
    scale      = scale_used,
    rmst_tau   = time,
    xvar.names = prov_xvars,
    path       = "A"
  )
  result
}

## ---------------------------------------------------------------------------
## Internal helpers

#' @keywords internal
.resolve_varpro_scale <- function(scale, family) {
  if (scale != "auto") return(scale)
  if (is.na(family) || is.null(family)) return("generic")
  if (family == "surv")  return("mortality")
  "generic"   # regr, class, or unknown
}

#' @keywords internal
.gg_partial_varpro_cpath <- function(object, scale, time, model) {
  rf           <- object$rf
  partial_type <- if (scale == "surv") "surv" else "chf"

  partial_time <- NULL
  if (!is.null(time)) {
    ti           <- rf$time.interest
    partial_time <- ti[which.min(abs(ti - time))]
  }

  pd <- gg_partial_rfsrc(rf,
                          xvar.names   = object$xvar.names,
                          partial.time = partial_time,
                          partial.type = partial_type)

  ## Prepend our class so S3 dispatch routes here first; NextMethod() falls
  ## through to plot.gg_partial_rfsrc for rendering.
  class(pd) <- c("gg_partial_varpro", class(pd))

  if (!is.null(model)) {
    if (is.data.frame(pd$continuous))  pd$continuous$model  <- model
    if (is.data.frame(pd$categorical)) pd$categorical$model <- model
  }

  attr(pd, "provenance") <- list(
    source     = "varPro",
    family     = object$family,
    ntree      = object$max.tree,
    n          = nrow(object$x),
    scale      = scale,
    rmst_tau   = time,
    xvar.names = object$xvar.names,
    path       = "C"
  )
  pd
}
```

- [ ] **Step 4: Run tests — expect all to pass**

```bash
Rscript -e "devtools::test(filter = 'test_gg_partial_varpro')"
```

Expected: all tests pass. Zero failures.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partial_varpro.R tests/testthat/test_gg_partial_varpro.R
git commit -m "feat: add gg_partial_varpro extractor (A-path, TDD)"
```

---

## Task 2: `varPro` Suggests→Imports and roxygenize

**Files:**
- Modify: `DESCRIPTION`
- Regenerate: `NAMESPACE`

**Background:** Phase 0 intentionally declared `varPro` in `Suggests:` because no `importFrom(varPro,...)` existed yet, which would have caused `R CMD check --as-cran` to emit `NOTE: Namespace in Imports field not imported from: 'varPro'`. Task 1 added `@importFrom varPro partialpro` to `gg_partial_varpro.R`; this task promotes the field and re-roxygenizes so the NOTE is eliminated.

- [ ] **Step 1: Move varPro in DESCRIPTION**

In `DESCRIPTION`, find the `Suggests:` block and remove `varPro` from it:

```
Suggests:
    testthat,
    bookdown,
    RColorBrewer,
    MASS,
    lintr,
    covr,
    vdiffr,
    datasets,
    rmarkdown,
    quarto,
    pkgdown,
    pkgload,
    knitr,
    plotly,
    igraph,
    callr
```

And add `varPro` to the `Imports:` block (after `randomForest`):

```
Imports:
    randomForestSRC (>= 3.4.0),
    randomForest,
    varPro,
    survival,
    parallel,
    tidyr,
    dplyr,
    ggplot2,
    patchwork,
    stringr
```

- [ ] **Step 2: Regenerate NAMESPACE**

```bash
Rscript -e "devtools::document()"
```

Expected: NAMESPACE now contains `importFrom(varPro,partialpro)`. Verify:

```bash
grep "varPro" NAMESPACE
```

Expected output: `importFrom(varPro,partialpro)`

- [ ] **Step 3: Verify 0-note R CMD check**

```bash
Rscript -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-vignettes'), error_on = 'warning')"
```

Expected: 0 errors, 0 warnings, 0 notes. If `rcmdcheck` is not installed:

```bash
R CMD build . --no-build-vignettes && R CMD check --as-cran --no-vignettes ggRandomForests_*.tar.gz
```

- [ ] **Step 4: Commit**

```bash
git add DESCRIPTION NAMESPACE
git commit -m "feat: promote varPro Suggests->Imports, add importFrom(varPro,partialpro)"
```

---

## Task 3: Deprecation shim for `gg_partialpro` (TDD)

**Files:**
- Modify: `R/gg_partialpro.R`
- Modify: `tests/testthat/test_gg_partialpro.R`

- [ ] **Step 1: Add regression guards to the existing test file**

Open `tests/testthat/test_gg_partialpro.R` and append at the end:

```r
# ---- deprecation shim regression guards ------------------------------------

test_that("gg_partialpro shim emits a deprecation warning", {
  mock_dta <- make_mock_partialpro_data()
  expect_warning(
    gg_partialpro(mock_dta),
    regexp = "deprecated.*gg_partial_varpro",
    ignore.case = TRUE
  )
})

test_that("gg_partialpro shim output is a gg_partial_varpro object", {
  mock_dta <- make_mock_partialpro_data()
  result   <- suppressWarnings(gg_partialpro(mock_dta))
  expect_s3_class(result, "gg_partial_varpro")
})

test_that("gg_partialpro shim: continuous/categorical structure preserved", {
  mock_dta <- make_mock_partialpro_data()
  result   <- suppressWarnings(gg_partialpro(mock_dta))
  expect_named(result, c("continuous", "categorical"))
  expect_true("age" %in% result$continuous$name)
  expect_true("sex" %in% result$categorical$name)
})
```

- [ ] **Step 2: Run — expect the new tests to fail**

```bash
Rscript -e "devtools::test(filter = 'test_gg_partialpro')"
```

Expected: the three new `shim` tests fail (either no warning, or class mismatch).

- [ ] **Step 3: Replace the body of `R/gg_partialpro.R`**

Replace the entire file with:

```r
##=============================================================================
#' Partial dependence data from a varPro model (deprecated)
#'
#' \lifecycle{deprecated}
#'
#' \code{gg_partialpro()} has been renamed to
#' \code{\link{gg_partial_varpro}()}.  This function is a thin alias that
#' will be removed in the release after \pkg{ggRandomForests} v2.8.0.
#'
#' @param part_dta Passed to \code{\link{gg_partial_varpro}}.
#' @param nvars Passed to \code{\link{gg_partial_varpro}}.
#' @param cat_limit Passed to \code{\link{gg_partial_varpro}}.
#' @param model Passed to \code{\link{gg_partial_varpro}}.
#'
#' @return A \code{gg_partial_varpro} object (see
#'   \code{\link{gg_partial_varpro}}).
#'
#' @seealso \code{\link{gg_partial_varpro}}
#'
#' @rdname gg_partial_varpro
#' @export
gg_partialpro <- function(part_dta,
                           nvars     = NULL,
                           cat_limit = 10,
                           model     = NULL) {
  .Deprecated(
    new = "gg_partial_varpro",
    msg = paste0("'gg_partialpro()' is deprecated. ",
                 "Use 'gg_partial_varpro()' instead.")
  )
  gg_partial_varpro(part_dta = part_dta,
                    nvars     = nvars,
                    cat_limit = cat_limit,
                    model     = model)
}
```

- [ ] **Step 4: Run — expect all tests in test_gg_partialpro.R to pass**

```bash
Rscript -e "devtools::test(filter = 'test_gg_partialpro')"
```

Expected: all tests pass including the three new shim guards.

- [ ] **Step 5: Commit**

```bash
git add R/gg_partialpro.R tests/testthat/test_gg_partialpro.R
git commit -m "feat: soft-deprecate gg_partialpro() -> gg_partial_varpro() shim"
```

---

## Task 4: `R/plot.gg_partial_varpro.R` — A-path plot method (TDD)

**Files:**
- Create: `R/plot.gg_partial_varpro.R`
- Modify: `R/plot.gg_partial.R` (replace `plot.gg_partialpro` body with shim)
- Add to: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Append plot tests to `tests/testthat/test_gg_partial_varpro.R`**

```r
## ── plot.gg_partial_varpro (A-path) ──────────────────────────────────────────
test_that("plot.gg_partial_varpro: continuous-only returns ggplot", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1)
  gg <- plot(result)
  expect_s3_class(gg, "ggplot")
})

test_that("plot.gg_partial_varpro: both cont + cat returns ggplot", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  gg <- plot(result)
  expect_s3_class(gg, "ggplot")
})

test_that("plot.gg_partial_varpro: type arg selects effect columns", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1)
  gg <- plot(result, type = "parametric")
  expect_s3_class(gg, "ggplot")
})

test_that("plot.gg_partial_varpro: scale='mortality' → honest y-label", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1,
                               scale = "mortality")
  gg <- plot(result)
  expect_true(grepl("mortality|Ensemble|expected", gg$labels$y,
                    ignore.case = TRUE))
})

test_that("plot.gg_partial_varpro: scale='rmst' with time → RMST y-label", {
  result <- gg_partial_varpro(make_mock_vpro_data(), nvars = 1,
                               scale = "rmst", time = 365)
  gg <- plot(result)
  expect_true(grepl("RMST|365", gg$labels$y))
})
```

- [ ] **Step 2: Run — expect new plot tests to fail**

```bash
Rscript -e "devtools::test(filter = 'test_gg_partial_varpro')"
```

Expected: the new plot tests fail with `no applicable method for 'plot'`.

- [ ] **Step 3: Create `R/plot.gg_partial_varpro.R`**

```r
##=============================================================================
#' Plot a \code{\link{gg_partial_varpro}} object
#'
#' Produces ggplot2 partial dependence curves from the named list returned
#' by \code{\link{gg_partial_varpro}}.  Continuous predictors are shown as
#' overlaid line curves (one per effect type); categorical predictors as
#' side-by-side boxplots.  For survival path-C objects (produced when
#' \code{scale \%in\% c("surv","chf")} is passed to the extractor) the plot
#' is delegated to \code{\link{plot.gg_partial_rfsrc}}.
#'
#' @param x A \code{\link{gg_partial_varpro}} object.
#' @param type Character vector; one or more of \code{"parametric"},
#'   \code{"nonparametric"}, \code{"causal"}.  Defaults to all three.
#'   Ignored for path-C objects.
#' @param ... Not currently used for path-A objects; forwarded to
#'   \code{plot.gg_partial_rfsrc} for path-C objects.
#'
#' @details
#' **Ensemble mortality (scale = "mortality"):** When the provenance scale
#' is \code{"mortality"}, the y-axis label reads
#' \emph{"Ensemble mortality (expected events)"} to make clear that this
#' is an \strong{unbounded relative-risk score}, not a survival probability
#' or \eqn{1 - S(t)} (Ishwaran, Kogalur, Blackstone & Lauer, 2008
#' <doi:10.1214/08-AOAS169>).
#'
#' @return A \code{ggplot} (or \code{patchwork}) object.
#'
#' @references
#' Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS (2008).
#' Random survival forests. \emph{The Annals of Applied Statistics},
#' \bold{2}(3), 841--860. \doi{10.1214/08-AOAS169}.
#'
#' @seealso \code{\link{gg_partial_varpro}}
#'
#' @examples
#' set.seed(42)
#' n_obs <- 30; n_pts <- 15
#' mock_data <- list(
#'   age = list(
#'     xvirtual    = seq(30, 80, length.out = n_pts),
#'     xorg        = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
#'     yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
#'     yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
#'     yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
#'   ),
#'   sex = list(
#'     xvirtual    = c(0, 1),
#'     xorg        = sample(c(0, 1), n_obs, replace = TRUE),
#'     yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs),
#'     yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
#'     yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
#'   )
#' )
#' pp <- gg_partial_varpro(mock_data)
#' plot(pp)
#' plot(pp, type = "parametric")
#'
#' @importFrom ggplot2 .data
#' @importFrom patchwork wrap_plots
#' @export
plot.gg_partial_varpro <- function(x,
                                    type = c("parametric", "nonparametric",
                                             "causal"),
                                    ...) {
  ## C-path: delegate to plot.gg_partial_rfsrc via NextMethod().
  prov <- attr(x, "provenance")
  if (!is.null(prov) && identical(prov$path, "C")) {
    return(NextMethod())
  }

  ## A-path rendering.
  type   <- match.arg(type, several.ok = TRUE)
  ylabel <- .partial_varpro_ylabel(prov)

  gg_cont <- NULL
  if (!is.null(x$continuous) && nrow(x$continuous) > 0) {
    cont_long <- tidyr::pivot_longer(
      x$continuous,
      cols      = tidyr::all_of(type),
      names_to  = "effect_type",
      values_to = "yhat"
    )
    gg_cont <- ggplot2::ggplot(
      cont_long,
      ggplot2::aes(
        x        = .data$variable,
        y        = .data$yhat,
        color    = .data$effect_type,
        linetype = .data$effect_type
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~name, scales = "free_x") +
      ggplot2::labs(x = NULL, y = ylabel,
                    color = "Effect type", linetype = "Effect type")
  }

  gg_cat <- NULL
  if (!is.null(x$categorical) && nrow(x$categorical) > 0) {
    cat_long <- tidyr::pivot_longer(
      x$categorical,
      cols      = tidyr::all_of(type),
      names_to  = "effect_type",
      values_to = "yhat"
    )
    gg_cat <- ggplot2::ggplot(
      cat_long,
      ggplot2::aes(
        x    = factor(.data$variable),
        y    = .data$yhat,
        fill = .data$effect_type
      )
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~name, scales = "free_x") +
      ggplot2::labs(x = NULL, y = ylabel, fill = "Effect type")
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    patchwork::wrap_plots(gg_cont, gg_cat, ncol = 1)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}

## ---------------------------------------------------------------------------
## Internal: build honest y-axis label from provenance.
#' @keywords internal
.partial_varpro_ylabel <- function(prov) {
  if (is.null(prov)) return("Partial Effect")
  scale <- prov$scale %||% "generic"
  switch(scale,
    mortality = "Ensemble mortality (expected events)",
    rmst      = {
      tau <- prov$rmst_tau
      if (!is.null(tau) && !is.na(tau)) {
        sprintf("RMST (τ = %g)", tau)
      } else {
        "RMST"
      }
    },
    surv      = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) {
        sprintf("Survival probability at t = %g", t)
      } else {
        "Survival probability"
      }
    },
    chf       = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) {
        sprintf("Cumulative hazard at t = %g", t)
      } else {
        "Cumulative hazard"
      }
    },
    "Partial Effect"   # generic / auto-regr / auto-class / unknown
  )
}
```

- [ ] **Step 4: Replace `plot.gg_partialpro` in `R/plot.gg_partial.R` with a shim**

Open `R/plot.gg_partial.R`. Find the block starting at (approximately) line 241:

```r
#' Plot a \code{\link{gg_partialpro}} object
```

and ending after:

```r
  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    wrap_plots(gg_cont, gg_cat, ncol = 1)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}
```

Replace that entire block (from `#' Plot a \code{\link{gg_partialpro}}` through the closing `}`) with:

```r
#' @rdname plot.gg_partial_varpro
#' @export
plot.gg_partialpro <- function(x, type = c("parametric", "nonparametric",
                                            "causal"), ...) {
  ## Deprecated class shim: re-dispatch to plot.gg_partial_varpro.
  class(x) <- c("gg_partial_varpro", setdiff(class(x), "gg_partialpro"))
  plot.gg_partial_varpro(x, type = type, ...)
}
```

- [ ] **Step 5: Run tests — expect all plot tests to pass**

```bash
Rscript -e "devtools::test(filter = 'test_gg_partial_varpro')"
```

Expected: all tests pass.

Also verify the existing partialpro plot test still passes:

```bash
Rscript -e "devtools::test(filter = 'test_gg_partialpro')"
```

Expected: all tests pass.

- [ ] **Step 6: Commit**

```bash
git add R/plot.gg_partial_varpro.R R/plot.gg_partial.R \
        tests/testthat/test_gg_partial_varpro.R
git commit -m "feat: add plot.gg_partial_varpro with honest y-label; shim plot.gg_partialpro"
```

---

## Task 5: autoplot, print, summary S3 methods (TDD)

**Files:**
- Modify: `R/autoplot_methods.R`
- Modify: `R/print_methods.R`
- Modify: `R/summary_methods.R`
- Add to: `tests/testthat/test_gg_partial_varpro.R`

- [ ] **Step 1: Append smoke tests**

Append to `tests/testthat/test_gg_partial_varpro.R`:

```r
## ── autoplot / print / summary ───────────────────────────────────────────────
test_that("autoplot.gg_partial_varpro: returns ggplot", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  expect_s3_class(ggplot2::autoplot(result), "ggplot")
})

test_that("print.gg_partial_varpro: returns x invisibly", {
  result <- gg_partial_varpro(make_mock_vpro_data())
  out <- capture.output(ret <- print(result))
  expect_identical(ret, result)
  expect_true(any(grepl("gg_partial_varpro", out)))
})

test_that("summary.gg_partial_varpro: returns summary.gg", {
  result  <- gg_partial_varpro(make_mock_vpro_data())
  s       <- summary(result)
  expect_s3_class(s, "summary.gg")
})
```

- [ ] **Step 2: Run — expect new tests to fail**

```bash
Rscript -e "devtools::test(filter = 'test_gg_partial_varpro')"
```

Expected: the three new tests fail with `no applicable method`.

- [ ] **Step 3: Add `autoplot.gg_partial_varpro` to `R/autoplot_methods.R`**

After the existing `autoplot.gg_partialpro` block, insert:

```r
#' @rdname autoplot.gg
#' @export
autoplot.gg_partial_varpro <- function(object, ...) {
  plot(object, ...)
}
```

Also update the `@details` list inside the shared roxygen block (find the `\item{\code{gg_partialpro}}` line) to add a new item for the new class:

```r
#'   \item{\code{gg_partial_varpro}}{Partial dependence (via \code{varPro})}
#'   \item{\code{gg_partialpro}}{Partial dependence via \code{varPro} (deprecated alias)}
```

- [ ] **Step 4: Add `print.gg_partial_varpro` to `R/print_methods.R`**

After the existing `print.gg_partialpro` block, insert:

```r
#' @rdname print.gg
#' @export
print.gg_partial_varpro <- function(x, ...) {
  nvar_cont <- if (is.data.frame(x$continuous) && nrow(x$continuous) > 0) {
    length(unique(x$continuous$name))
  } else {
    0L
  }
  nvar_cat  <- if (is.data.frame(x$categorical) && nrow(x$categorical) > 0) {
    length(unique(x$categorical$name))
  } else {
    0L
  }
  prov  <- attr(x, "provenance")
  scale <- if (!is.null(prov)) prov$scale %||% NA_character_ else NA_character_
  cat(.gg_header(x, "gg_partial_varpro"),
      sprintf("  |  continuous: %d, categorical: %d", nvar_cont, nvar_cat),
      if (!is.na(scale)) sprintf("  |  scale: %s", scale) else "",
      "\n", sep = "")
  invisible(x)
}
```

Also replace the existing `print.gg_partialpro` block with a shim that
re-dispatches (its class may now be `gg_partial_varpro`):

```r
#' @rdname print.gg
#' @export
print.gg_partialpro <- function(x, ...) {
  ## Deprecated-class shim: re-dispatch to print.gg_partial_varpro.
  class(x) <- c("gg_partial_varpro", setdiff(class(x), "gg_partialpro"))
  print.gg_partial_varpro(x, ...)
}
```

- [ ] **Step 5: Add `summary.gg_partial_varpro` to `R/summary_methods.R`**

After the existing `summary.gg_partialpro` block, insert:

```r
#' @rdname summary.gg
#' @export
summary.gg_partial_varpro <- function(object, ...) {
  .summary_skel(object, "gg_partial_varpro", .partialpro_body(object))
}
```

Also replace the existing `summary.gg_partialpro` block:

```r
#' @rdname summary.gg
#' @export
summary.gg_partialpro <- function(object, ...) {
  ## Deprecated-class shim.
  class(object) <- c("gg_partial_varpro",
                      setdiff(class(object), "gg_partialpro"))
  summary.gg_partial_varpro(object, ...)
}
```

- [ ] **Step 6: Regenerate NAMESPACE**

```bash
Rscript -e "devtools::document()"
```

Verify new S3 registrations appear:

```bash
grep "gg_partial_varpro" NAMESPACE
```

Expected output includes:
```
S3method(autoplot,gg_partial_varpro)
S3method(plot,gg_partial_varpro)
S3method(print,gg_partial_varpro)
S3method(summary,gg_partial_varpro)
export(gg_partial_varpro)
```

- [ ] **Step 7: Run all tests**

```bash
Rscript -e "devtools::test()"
```

Expected: 0 failures, 0 errors.

- [ ] **Step 8: Commit**

```bash
git add R/autoplot_methods.R R/print_methods.R R/summary_methods.R \
        NAMESPACE tests/testthat/test_gg_partial_varpro.R
git commit -m "feat: add autoplot/print/summary for gg_partial_varpro; shim old-class methods"
```

---

## Task 6: C-path — survival path via `object$rf` (TDD)

**Files:**
- Add to: `tests/testthat/test_gg_partial_varpro.R`

The C-path implementation (`gg_partial_varpro_cpath` + `plot.gg_partial_varpro` `NextMethod()`) is already present from Task 1 and Task 4. This task writes the tests that exercise it.

- [ ] **Step 1: Append C-path tests**

Append to `tests/testthat/test_gg_partial_varpro.R`:

```r
## ── C-path (scale = surv / chf) ──────────────────────────────────────────────
test_that("gg_partial_varpro: C-path returns gg_partial_varpro class", {
  skip_if_not_installed("randomForestSRC")
  set.seed(7)
  rf <- randomForestSRC::rfsrc(
    survival::Surv(time, status) ~ .,
    data   = survival::veteran,
    ntree  = 30,
    importance = FALSE
  )
  vp_mock <- list(
    rf         = rf,
    family     = "surv",
    xvar.names = rf$xvar.names,
    x          = rf$xvar,
    max.tree   = 30L
  )
  class(vp_mock) <- "varpro"

  result <- gg_partial_varpro(object = vp_mock, scale = "surv",
                               time = median(rf$time.interest))
  expect_s3_class(result, "gg_partial_varpro")
})

test_that("gg_partial_varpro: C-path provenance path='C'", {
  skip_if_not_installed("randomForestSRC")
  set.seed(7)
  rf <- randomForestSRC::rfsrc(
    survival::Surv(time, status) ~ .,
    data   = survival::veteran,
    ntree  = 30,
    importance = FALSE
  )
  vp_mock <- list(
    rf         = rf,
    family     = "surv",
    xvar.names = rf$xvar.names,
    x          = rf$xvar,
    max.tree   = 30L
  )
  class(vp_mock) <- "varpro"

  result <- gg_partial_varpro(object = vp_mock, scale = "surv",
                               time = median(rf$time.interest))
  expect_equal(attr(result, "provenance")$path, "C")
  expect_equal(attr(result, "provenance")$scale, "surv")
})

test_that("gg_partial_varpro: plot C-path returns ggplot", {
  skip_if_not_installed("randomForestSRC")
  set.seed(7)
  rf <- randomForestSRC::rfsrc(
    survival::Surv(time, status) ~ .,
    data      = survival::veteran,
    ntree     = 30,
    importance = FALSE
  )
  vp_mock <- list(
    rf         = rf,
    family     = "surv",
    xvar.names = rf$xvar.names[1L],
    x          = rf$xvar[, rf$xvar.names[1L], drop = FALSE],
    max.tree   = 30L
  )
  class(vp_mock) <- "varpro"

  result <- gg_partial_varpro(object = vp_mock, scale = "surv",
                               time = median(rf$time.interest))
  gg <- plot(result)
  expect_s3_class(gg, "ggplot")
})
```

- [ ] **Step 2: Run C-path tests**

```bash
Rscript -e "devtools::test(filter = 'test_gg_partial_varpro')"
```

Expected: all tests pass including the three new C-path tests (they are skipped if `randomForestSRC` is absent, but should pass with it installed).

- [ ] **Step 3: Run the full suite**

```bash
Rscript -e "devtools::test()"
```

Expected: 0 failures.

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test_gg_partial_varpro.R
git commit -m "test: add C-path (scale=surv/chf) tests for gg_partial_varpro"
```

---

## Task 7: vdiffr snapshots

**Files:**
- Create: `tests/testthat/_snaps/snapshots/gg-partial-varpro-continuous.svg`
- Create: `tests/testthat/_snaps/snapshots/gg-partial-varpro-categorical.svg`
- Create: `tests/testthat/_snaps/snapshots/gg-partial-varpro-both.svg`
- Create: `tests/testthat/_snaps/snapshots/gg-partial-varpro-mortality.svg`
- Add to: `tests/testthat/test_snapshots.R`

- [ ] **Step 1: Add snapshot tests to `tests/testthat/test_snapshots.R`**

Open `tests/testthat/test_snapshots.R` and append:

```r
## ---- gg_partial_varpro snapshots -------------------------------------------
make_mock_vpro_snap <- function(n_obs = 30, n_pts = 15) {
  set.seed(42)
  list(
    age = list(
      xvirtual    = seq(30, 80, length.out = n_pts),
      xorg        = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
      yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
      yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
      yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
    ),
    sex = list(
      xvirtual    = c(0, 1),
      xorg        = sample(c(0, 1), n_obs, replace = TRUE),
      yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs),
      yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
      yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
    )
  )
}

test_that("snapshot: gg-partial-varpro-continuous", {
  skip_if_not_installed("vdiffr")
  result <- gg_partial_varpro(make_mock_vpro_snap(), nvars = 1)
  vdiffr::expect_doppelganger("gg-partial-varpro-continuous", plot(result))
})

test_that("snapshot: gg-partial-varpro-categorical", {
  skip_if_not_installed("vdiffr")
  mock <- make_mock_vpro_snap()
  result <- gg_partial_varpro(mock["sex"])
  vdiffr::expect_doppelganger("gg-partial-varpro-categorical", plot(result))
})

test_that("snapshot: gg-partial-varpro-both", {
  skip_if_not_installed("vdiffr")
  result <- gg_partial_varpro(make_mock_vpro_snap())
  vdiffr::expect_doppelganger("gg-partial-varpro-both", plot(result))
})

test_that("snapshot: gg-partial-varpro-mortality", {
  skip_if_not_installed("vdiffr")
  result <- gg_partial_varpro(make_mock_vpro_snap(), nvars = 1,
                               scale = "mortality")
  vdiffr::expect_doppelganger("gg-partial-varpro-mortality", plot(result))
})
```

- [ ] **Step 2: Generate snapshots**

```bash
Rscript -e "vdiffr::manage_cases()"
```

In the vdiffr UI: validate the four new `gg-partial-varpro-*` cases. The mortality snapshot should show y-axis label "Ensemble mortality (expected events)".

- [ ] **Step 3: Verify snapshots are accepted**

```bash
Rscript -e "devtools::test(filter = 'test_snapshots')"
```

Expected: all snapshot tests pass (no new failures).

- [ ] **Step 4: Guard against snapshot-pruning**

Before committing, verify the SVG files were actually written:

```bash
ls tests/testthat/_snaps/snapshots/gg-partial-varpro-*.svg
```

Expected: four files listed. If any are missing, re-run `vdiffr::manage_cases()`.

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test_snapshots.R \
        tests/testthat/_snaps/snapshots/gg-partial-varpro-continuous.svg \
        tests/testthat/_snaps/snapshots/gg-partial-varpro-categorical.svg \
        tests/testthat/_snaps/snapshots/gg-partial-varpro-both.svg \
        tests/testthat/_snaps/snapshots/gg-partial-varpro-mortality.svg
git commit -m "test: add vdiffr snapshots for plot.gg_partial_varpro"
```

---

## Task 8: `_pkgdown.yml` + NEWS finalize + roxygenize + final gate + PR

**Files:**
- Modify: `_pkgdown.yml`
- Modify: `NEWS.md`
- Regenerate: `NAMESPACE` (final clean pass)

- [ ] **Step 1: Update `_pkgdown.yml` Partial Dependence section**

Open `_pkgdown.yml`. Find the `"Partial Dependence"` reference section:

```yaml
  - title: "Partial Dependence"
    desc: "Partial dependence plots for individual variables."
    contents:
      - gg_partial
      - plot.gg_partial
      - gg_partial_rfsrc
      - plot.gg_partial_rfsrc
      - gg_partialpro
      - plot.gg_partialpro
```

Replace with:

```yaml
  - title: "Partial Dependence"
    desc: "Partial dependence plots for individual variables."
    contents:
      - gg_partial
      - plot.gg_partial
      - gg_partial_rfsrc
      - plot.gg_partial_rfsrc
      - gg_partial_varpro
      - plot.gg_partial_varpro
      - gg_partialpro
      - plot.gg_partialpro
```

- [ ] **Step 2: Replace NEWS stub with final entry**

At the top of `NEWS.md`, replace the placeholder bullet added in Task 0 with the full entry. The file should begin:

```
Package: ggRandomForests
Version: 2.7.3.9002

```

Then the existing `ggRandomForests v2.8.0 (development)` section should gain a new bullet at the top of its list:

```
* **varPro partial dependence: `gg_partial_varpro()` (#84).**
  - `gg_partial_varpro()` replaces `gg_partialpro()` as the primary entry
    point for varPro partial dependence plots.  The new extractor accepts
    an optional `object` argument (the originating `varpro` fit) for
    provenance-aware axis labeling and a `scale` argument
    (`"auto"`, `"mortality"`, `"rmst"`, `"surv"`, `"chf"`).
  - **Ensemble mortality labeling** (Ishwaran et al. 2008): when
    `scale = "mortality"` (or `scale = "auto"` with a survival forest),
    the y-axis is labeled "Ensemble mortality (expected events)" — an
    unbounded relative-risk score, not a survival probability.  The
    documentation explicitly warns against misinterpretation.
  - **Survival path C:** `scale = "surv"` or `scale = "chf"` extracts
    `object$rf` (the embedded rfsrc forest) and returns true S(t)/CHF
    partial curves via `gg_partial_rfsrc` infrastructure.
  - `varPro` is now a hard dependency (`Imports:`).
  - `gg_partialpro()` is soft-deprecated: it emits a deprecation warning
    and delegates to `gg_partial_varpro()`.  Removal is planned for the
    release after v2.8.0.
```

- [ ] **Step 3: Final roxygenize**

```bash
Rscript -e "devtools::document()"
```

Confirm no unexpected changes:

```bash
git diff NAMESPACE
```

Expected: only the previously-committed additions (`importFrom(varPro,partialpro)`, S3 methods for `gg_partial_varpro`). No regressions.

- [ ] **Step 4: Full test suite**

```bash
Rscript -e "devtools::test()"
```

Expected: 0 failures, 0 errors.

- [ ] **Step 5: lintr**

```bash
Rscript -e "lintr::lint_package()"
```

Expected: 0 lints. Key thresholds: `cyclocomp_linter` ≤ 20, `line_length_linter` ≤ 120.

If cyclocomp violations appear in `gg_partial_varpro.R`, extract the inner loop logic into a named helper (e.g., `.process_cont_var`, `.process_cat_var`) to reduce per-function complexity.

- [ ] **Step 6: R CMD check --as-cran**

```bash
Rscript -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-vignettes'), error_on = 'warning')"
```

Expected: 0 errors, 0 warnings, 0 notes.

- [ ] **Step 7: Commit final files**

```bash
git add _pkgdown.yml NEWS.md NAMESPACE man/
git commit -m "docs: finalize NEWS, pkgdown, NAMESPACE for gg_partial_varpro"
```

- [ ] **Step 8: Open PR**

```bash
cd ~/Documents/GitHub/ggRandomForests   # repo root (not the worktree)
gh pr create \
  --title "varPro Phase 1: gg_partial_varpro — rename, deprecation shim, A+C survival paths (#84)" \
  --body "$(cat <<'EOF'
## Summary

- Introduces `gg_partial_varpro()` as the canonical varPro partial-dependence extractor (rename of `gg_partialpro`, per v2.8.0 design spec §5).
- Adds `scale` argument: `auto`, `mortality`, `rmst`, `surv`, `chf`; each produces a self-documenting y-axis label.
- **Ensemble mortality documentation (Ishwaran et al. 2008):** `@details` on both extractor and plot method explicitly defines mortality as an unbounded expected-event count, not 1−S(t).
- Survival path C: `scale = "surv"/"chf"` routes `object$rf` through `gg_partial_rfsrc`, prepends `gg_partial_varpro` class, and delegates plotting via `NextMethod()`.
- `varPro` promoted from `Suggests:` → `Imports:` (first `varPro::` call; `importFrom(varPro,partialpro)` in NAMESPACE).
- `gg_partialpro()` soft-deprecated: emits `.Deprecated()` warning and delegates to `gg_partial_varpro()`.
- 4 vdiffr snapshots for all A-path plot modes.
- Bumps version `2.7.3.9001` → `2.7.3.9002`.

## Test plan

- [ ] All testthat tests pass (`devtools::test()` — 0 failures)
- [ ] `lintr::lint_package()` — 0 lints
- [ ] `R CMD check --as-cran --no-vignettes` — 0 errors / 0 warnings / 0 notes
- [ ] vdiffr snapshots accepted for 4 new plot modes
- [ ] `gg_partialpro()` emits deprecation warning; output is `gg_partial_varpro` class
- [ ] C-path tests pass with `randomForestSRC` installed

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

---

## Acceptance Criteria (checklist for PR reviewer)

- [ ] `R CMD check --as-cran`: 0 errors / 0 warnings / 0 notes
- [ ] `devtools::test()`: 0 failures, 0 errors
- [ ] `lintr::lint_package()`: 0 lints (cyclocomp ≤ 20, line_length ≤ 120)
- [ ] `vdiffr` snapshots generated for 4 `plot.gg_partial_varpro` modes
- [ ] `gg_partialpro()` emits `warning()` and returns a `gg_partial_varpro` object
- [ ] Mortality `@details` block present in both `gg_partial_varpro.R` and `plot.gg_partial_varpro.R`
- [ ] `varPro` in `Imports:` in DESCRIPTION; `importFrom(varPro,partialpro)` in NAMESPACE
- [ ] `NEWS.md` top version header matches `DESCRIPTION` `Version:` field exactly (`2.7.3.9002`)
- [ ] `_pkgdown.yml` lists `gg_partial_varpro` and `plot.gg_partial_varpro` in the Partial Dependence section
