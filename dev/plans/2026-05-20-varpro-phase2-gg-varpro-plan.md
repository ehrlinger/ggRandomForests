# varPro Phase 2: gg_varpro Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add `gg_varpro()` + `plot.gg_varpro()` — honest 15th/85th boxplot of varPro variable importance with optional per-tree overlay (`faithful=TRUE`) and class-faceted conditional importance (`conditional=TRUE`).

**Architecture:** `gg_varpro()` always calls `varPro::importance(object, local.std=FALSE)` internally to obtain `$imp.tree` (ntree×p matrix). Per-tree z-scores are derived from `imp.tree` by normalising each column to z-scale (`z_ij = imp_ij * sqrt(ntree) / sd_j`); the box stats (q05/q15/median/q85/q95) are computed from this z-normalised matrix and stored in `$stats`. The plot method is pure rendering — no computation. This mirrors the Phase 1 `gg_partial_varpro` extractor/plot separation.

**Tech Stack:** R, varPro (Imports), ggplot2, tidyr, dplyr, patchwork; testthat + vdiffr for testing; roxygen2 8.x; lintr cyclocomp ≤ 20.

**Design spec:** `dev/plans/2026-05-20-varpro-phase2-gg-varpro-design.md`

**Prerequisite:** origin/main must include Phase 1 PR #84 (`eba71c4`). Run `git fetch origin` and verify before starting T0.

---

## File Map

| Action | Path | Responsibility |
|--------|------|----------------|
| Create | `R/gg_varpro.R` | Extractor + 3 internal helpers |
| Create | `R/plot.gg_varpro.R` | `plot.gg_varpro` + `.varpro_imp_ylabel` |
| Create | `tests/testthat/test_gg_varpro.R` | Full TDD suite |
| Modify | `R/autoplot_methods.R` | Add `autoplot.gg_varpro` + `@details` entry |
| Modify | `R/print_methods.R` | Add `print.gg_varpro` |
| Modify | `R/summary_methods.R` | Add `.varpro_body` helper + `summary.gg_varpro` |
| Modify | `DESCRIPTION` | Version `2.7.3.9003`; Date `2026-05-20` |
| Modify | `NAMESPACE` | Re-roxygenize (new S3 + importFrom) |
| Modify | `_pkgdown.yml` | Add `gg_varpro`, `plot.gg_varpro` under Variable Importance |
| Modify | `NEWS.md` | Add `## ggRandomForests 2.7.3.9003` entry |
| Modify | `tests/testthat/test_snapshots.R` | Add 3 vdiffr snapshot tests |

---

## Task 0: Branch setup and dev-cycle open

**Files:**
- Modify: `DESCRIPTION`

- [ ] **Step 1: Sync local main with origin/main**

```bash
git fetch origin
git rebase origin/main
```

Expected: local main now includes commit `eba71c4` (PR #84) plus the 3 doc
commits already present. Resolve any rebase conflicts (unlikely — doc commits
don't touch the same files as PR #84).

- [ ] **Step 2: Create isolated worktree and branch**

```bash
git worktree add .claude/worktrees/varpro-phase2 -b feat/varpro-phase2-gg-varpro
cd .claude/worktrees/varpro-phase2
```

All subsequent steps run from `.claude/worktrees/varpro-phase2`.

- [ ] **Step 3: Confirm clean baseline**

```bash
Rscript -e "devtools::test()" 2>&1 | tail -5
```

Expected: all tests pass, 0 failures. If any failures, fix before proceeding.

- [ ] **Step 4: Bump version in DESCRIPTION**

Open `DESCRIPTION`. Change:
```
Version: 2.7.3.9002
Date: 2026-05-20
```
to:
```
Version: 2.7.3.9003
Date: 2026-05-20
```

`varPro` is already in `Imports:` from Phase 1 — no DESCRIPTION change needed
for imports.

- [ ] **Step 5: Commit version bump**

```bash
git add DESCRIPTION
git commit -m "chore: open v2.7.3.9003 dev cycle (Phase 2 gg_varpro)"
```

---

## Task 1: `gg_varpro` extractor (TDD)

**Files:**
- Create: `R/gg_varpro.R`
- Create: `tests/testthat/test_gg_varpro.R`

### Step 1: Write failing input-validation tests

Create `tests/testthat/test_gg_varpro.R`:

```r
# Tests for gg_varpro (Phase 2: importance extractor)

## ── Helpers ──────────────────────────────────────────────────────────────────

# Regression fit — fast, always available (mtcars is base R)
make_vp_regr <- function(ntree = 25L) {
  set.seed(42L)
  varPro::varpro(mpg ~ ., data = mtcars, ntree = ntree)
}

# Classification fit — iris, always available
make_vp_class <- function(ntree = 25L) {
  set.seed(42L)
  varPro::varpro(Species ~ ., data = iris, ntree = ntree)
}

## ── Input validation ─────────────────────────────────────────────────────────

test_that("gg_varpro: missing object -> stop", {
  expect_error(gg_varpro(), regexp = "object")
})

test_that("gg_varpro: non-varpro object -> stop", {
  expect_error(gg_varpro(list(x = 1)), regexp = "varpro")
})

test_that("gg_varpro: conditional=TRUE on regression -> stop", {
  vp <- make_vp_regr()
  expect_error(gg_varpro(vp, conditional = TRUE),
               regexp = "classification")
})

test_that("gg_varpro: type='raw' with local.std=TRUE -> stop at plot time", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = TRUE)
  expect_error(plot(gg, type = "raw"), regexp = "local\\.std")
})
```

- [ ] **Step 2: Run to confirm FAIL**

```bash
Rscript -e "devtools::test(filter='gg_varpro')" 2>&1 | tail -10
```

Expected: 4 errors — `gg_varpro` not found.

### Step 3: Create `R/gg_varpro.R` stub + internal helpers

Create `R/gg_varpro.R` with the complete implementation:

```r
##=============================================================================
#' Variable importance data from a varPro model
#'
#' Extracts per-tree importance scores from a fitted \code{varpro} object,
#' summarises them into an honest boxplot-ready data structure (hinges at
#' 15th/85th percentile, whiskers at 5th/95th), and optionally retains
#' class-conditional importance for classification forests.
#'
#' @param object A fitted \code{varpro} object (required).
#' @param local.std Logical; default \code{TRUE}.  When \code{TRUE} the
#'   per-tree importances are normalised to z-scale before computing box
#'   statistics.  Set to \code{FALSE} to retain the raw importance scale
#'   (required for \code{type = "raw"} in \code{\link{plot.gg_varpro}}).
#' @param cutoff Numeric z-score threshold for variable selection; default
#'   \code{0.79}.  Variables with aggregate z > cutoff are flagged
#'   \code{selected = TRUE} in \code{$imp}.
#' @param faithful Logical; default \code{FALSE}.  When \code{TRUE},
#'   \code{$imp.tree} is retained so \code{\link{plot.gg_varpro}} can
#'   overlay per-tree jitter points.
#' @param conditional Logical; default \code{FALSE}.  When \code{TRUE}
#'   (classification forests only) extracts the \code{$conditional.z}
#'   matrix and stores it as \code{$conditional}.
#' @param nvar Integer; retain only the top \code{nvar} variables (by
#'   median z) after applying the cutoff filter.  \code{NULL} keeps all.
#' @param ... Additional arguments passed to \code{varPro::importance()}.
#'
#' @return A named list of class \code{"gg_varpro"} with elements:
#' \describe{
#'   \item{\code{$imp}}{Long tidy data frame: \code{variable} (factor,
#'     ordered by median z descending), \code{z} (aggregate z-score),
#'     \code{selected} (logical, \code{z > cutoff}).}
#'   \item{\code{$imp.tree}}{\code{NULL} when \code{faithful = FALSE};
#'     otherwise the raw ntree×p importance matrix from
#'     \code{varPro::importance()}.}
#'   \item{\code{$stats}}{Per-variable summary: \code{variable},
#'     \code{median}, \code{q05}, \code{q15}, \code{q85}, \code{q95}
#'     (on z-scale when \code{local.std = TRUE}, raw when \code{FALSE}),
#'     plus \code{mean} (raw importance mean, always stored).}
#'   \item{\code{$conditional}}{\code{NULL} when \code{conditional = FALSE};
#'     otherwise a data frame with columns \code{variable}, \code{class},
#'     \code{z} (one row per variable × class combination).}
#' }
#' A \code{"provenance"} attribute carries \code{family}, \code{local.std},
#' \code{cutoff}, \code{faithful}, \code{conditional}, \code{xvar.names},
#' and \code{n}.
#'
#' @seealso \code{\link{plot.gg_varpro}}, \code{\link{gg_vimp}}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#' gg <- gg_varpro(vp)
#' print(gg)
#' plot(gg)
#' }
#'
#' @importFrom varPro importance
#' @export
gg_varpro <- function(object,
                      local.std   = TRUE,
                      cutoff      = 0.79,
                      faithful    = FALSE,
                      conditional = FALSE,
                      nvar        = NULL,
                      ...) {

  ## ---- Validation -----------------------------------------------------------
  .validate_varpro_imp_inputs(object, local.std, conditional)

  ## ---- Always call importance with local.std=FALSE to get imp.tree ----------
  imp_out <- varPro::importance(object, local.std = FALSE, ...)

  ## ---- Build tidy data structures ------------------------------------------
  dfs <- .build_varpro_imp_dfs(imp_out, object$family, cutoff, nvar,
                                faithful, local.std, conditional)

  ## ---- Assemble result ------------------------------------------------------
  result <- structure(
    list(
      imp         = dfs$imp,
      imp.tree    = dfs$imp_tree,
      stats       = dfs$stats,
      conditional = dfs$conditional
    ),
    class = c("gg_varpro", "list")
  )

  attr(result, "provenance") <- list(
    family      = object$family,
    local.std   = local.std,
    cutoff      = cutoff,
    faithful    = faithful,
    conditional = conditional,
    xvar.names  = object$xvar.names,
    n           = nrow(object$x)
  )

  result
}

## ---- Internal helpers -------------------------------------------------------

.validate_varpro_imp_inputs <- function(object, local.std, conditional) {
  if (missing(object) || is.null(object)) {
    stop("'object' must be a fitted varpro object.", call. = FALSE)
  }
  if (!inherits(object, "varpro")) {
    stop("'object' must be a varpro fit (class \"varpro\").", call. = FALSE)
  }
  if (conditional && !identical(object$family, "class")) {
    stop("conditional=TRUE requires a classification forest ",
         "(object$family == \"class\").", call. = FALSE)
  }
}

.varpro_imp_stats <- function(mat, local.std = TRUE) {
  ## mat: ntree x p matrix of raw per-tree importances.
  ## When local.std=TRUE, normalise each column to per-tree z-scores:
  ##   z_ij = imp_ij * sqrt(ntree) / sd_j
  ## so that mean(z_ij) == aggregate z-score for variable j.
  ntree <- nrow(mat)
  if (local.std) {
    sd_j <- apply(mat, 2L, stats::sd, na.rm = TRUE)
    sd_j[sd_j < .Machine$double.eps] <- 1  # guard zero-variance columns
    display_mat <- sweep(mat, 2L, sd_j / sqrt(ntree), FUN = "/")
  } else {
    display_mat <- mat
  }
  data.frame(
    variable = colnames(mat),
    median   = apply(display_mat, 2L, stats::quantile, probs = 0.50,
                     na.rm = TRUE),
    q05      = apply(display_mat, 2L, stats::quantile, probs = 0.05,
                     na.rm = TRUE),
    q15      = apply(display_mat, 2L, stats::quantile, probs = 0.15,
                     na.rm = TRUE),
    q85      = apply(display_mat, 2L, stats::quantile, probs = 0.85,
                     na.rm = TRUE),
    q95      = apply(display_mat, 2L, stats::quantile, probs = 0.95,
                     na.rm = TRUE),
    mean     = colMeans(mat, na.rm = TRUE),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

.build_varpro_imp_dfs <- function(imp_out, family, cutoff, nvar,
                                   faithful, local.std, conditional) {
  ## Unpack importance() output — different shape for classification vs others.
  is_class <- identical(family, "class")

  if (is_class && !is.null(imp_out$unconditional)) {
    z_vec    <- imp_out$unconditional$z
    imp_tree <- imp_out$unconditional$imp.tree
    mean_vec <- imp_out$unconditional$mean
    cond_mat <- if (conditional) imp_out$conditional.z else NULL
  } else {
    z_vec    <- imp_out$z
    imp_tree <- imp_out$imp.tree
    mean_vec <- imp_out$mean
    cond_mat <- NULL
  }

  ## $imp: one row per variable
  imp_df <- data.frame(
    variable = names(z_vec),
    z        = as.numeric(z_vec),
    selected = as.numeric(z_vec) > cutoff,
    stringsAsFactors = FALSE
  )

  ## Apply nvar truncation (top-N by z)
  if (!is.null(nvar)) {
    imp_df <- imp_df[order(-imp_df$z), , drop = FALSE]
    imp_df <- imp_df[seq_len(min(nvar, nrow(imp_df))), , drop = FALSE]
  }

  ## $stats: box quantiles per variable (z- or raw-scale)
  keep_vars  <- imp_df$variable
  imp_tree_k <- imp_tree[, keep_vars, drop = FALSE]
  stats_df   <- .varpro_imp_stats(imp_tree_k, local.std = local.std)

  ## Order $imp factor by descending median z (matches plot sort)
  var_order  <- stats_df$variable[order(-stats_df$median)]
  imp_df$variable <- factor(imp_df$variable, levels = var_order)
  stats_df$variable <- factor(stats_df$variable, levels = var_order)

  ## $conditional: tidy class-conditional z-scores
  cond_df <- NULL
  if (!is.null(cond_mat)) {
    cond_df <- data.frame(
      variable = rep(rownames(cond_mat), ncol(cond_mat)),
      class    = rep(colnames(cond_mat), each = nrow(cond_mat)),
      z        = as.vector(cond_mat),
      stringsAsFactors = FALSE
    )
    cond_df <- cond_df[cond_df$variable %in% keep_vars, , drop = FALSE]
    cond_df$variable <- factor(cond_df$variable, levels = levels(imp_df$variable))
  }

  list(
    imp         = imp_df,
    imp_tree    = if (faithful) imp_tree_k else NULL,
    stats       = stats_df,
    conditional = cond_df
  )
}
```

- [ ] **Step 4: Run validation tests — expect PASS**

```bash
Rscript -e "devtools::test(filter='gg_varpro')" 2>&1 | tail -10
```

Expected: 4 tests pass (the 3 stop() tests + the type="raw" deferred test
will be caught later when plot.gg_varpro exists).

### Step 5: Add class, structure, and provenance tests

Append to `tests/testthat/test_gg_varpro.R`:

```r
## ── Class & structure ────────────────────────────────────────────────────────

test_that("gg_varpro returns gg_varpro class", {
  vp <- make_vp_regr()
  expect_s3_class(gg_varpro(vp), "gg_varpro")
})

test_that("gg_varpro$imp has variable, z, selected columns", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_named(gg$imp, c("variable", "z", "selected"))
  expect_s3_class(gg$imp$variable, "factor")
  expect_type(gg$imp$z, "double")
  expect_type(gg$imp$selected, "logical")
})

test_that("gg_varpro$stats has expected columns", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_true(all(c("variable", "median", "q05", "q15", "q85", "q95", "mean")
                  %in% names(gg$stats)))
})

test_that("gg_varpro$imp.tree is NULL when faithful=FALSE", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = FALSE)
  expect_null(gg$imp.tree)
})

test_that("gg_varpro$imp.tree is a matrix when faithful=TRUE", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = TRUE)
  expect_true(is.matrix(gg$imp.tree))
})

test_that("gg_varpro$conditional is NULL when conditional=FALSE", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, conditional = FALSE)
  expect_null(gg$conditional)
})

test_that("gg_varpro$conditional has variable, class, z when conditional=TRUE", {
  vp <- make_vp_class()
  gg <- gg_varpro(vp, conditional = TRUE)
  expect_false(is.null(gg$conditional))
  expect_true(all(c("variable", "class", "z") %in% names(gg$conditional)))
})

## ── Provenance attribute ─────────────────────────────────────────────────────

test_that("gg_varpro provenance has all expected fields", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  prov <- attr(gg, "provenance")
  expect_type(prov, "list")
  expect_true(all(c("family", "local.std", "cutoff", "faithful",
                    "conditional", "xvar.names", "n") %in% names(prov)))
})

test_that("gg_varpro provenance cutoff matches argument", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, cutoff = 1.2)
  expect_equal(attr(gg, "provenance")$cutoff, 1.2)
})

## ── cutoff and nvar ──────────────────────────────────────────────────────────

test_that("gg_varpro$imp$selected reflects z > cutoff", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, cutoff = 0.79)
  expect_true(all(gg$imp$z[gg$imp$selected]  >  0.79))
  expect_true(all(gg$imp$z[!gg$imp$selected] <= 0.79))
})

test_that("gg_varpro nvar=3 returns exactly 3 variables", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, nvar = 3L)
  expect_equal(nrow(gg$imp), 3L)
  expect_equal(nrow(gg$stats), 3L)
})

test_that("gg_varpro variable factor ordered by descending median z", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  med_vals <- gg$stats$median[order(match(gg$stats$variable,
                                           levels(gg$imp$variable)))]
  expect_true(all(diff(med_vals) <= 0))
})
```

- [ ] **Step 6: Run — expect all PASS**

```bash
Rscript -e "devtools::test(filter='gg_varpro')" 2>&1 | tail -10
```

Expected: ~17 tests pass.

- [ ] **Step 7: Roxygenize and verify NAMESPACE**

```bash
Rscript -e "roxygen2::roxygenize()"
grep "gg_varpro" NAMESPACE
```

Expected lines in NAMESPACE:
```
S3method(plot,gg_varpro)    # will appear after T2
export(gg_varpro)
importFrom(varPro,importance)
```

At this stage only `export(gg_varpro)` and `importFrom(varPro,importance)` appear.

- [ ] **Step 8: R CMD check — 0 errors, 0 warnings**

```bash
Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual')" 2>&1 | grep -E "^(ERROR|WARNING|NOTE|OK)"
```

- [ ] **Step 9: Commit**

```bash
git add R/gg_varpro.R tests/testthat/test_gg_varpro.R NAMESPACE man/
git commit -m "feat: add gg_varpro extractor with honest box stats (TDD)"
```

---

## Task 2: `plot.gg_varpro` (TDD)

**Files:**
- Create: `R/plot.gg_varpro.R`
- Modify: `tests/testthat/test_gg_varpro.R`

### Step 1: Add plot smoke tests

Append to `tests/testthat/test_gg_varpro.R`:

```r
## ── Plot smoke tests ─────────────────────────────────────────────────────────

test_that("plot.gg_varpro default returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='z' returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)  # local.std=TRUE default
  p  <- plot(gg, type = "z")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='raw' with local.std=FALSE returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = FALSE)
  p  <- plot(gg, type = "raw")
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro type='raw' with local.std=TRUE -> stop", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, local.std = TRUE)
  expect_error(plot(gg, type = "raw"), regexp = "local\\.std")
})

test_that("plot.gg_varpro faithful=TRUE returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp, faithful = TRUE)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
})

test_that("plot.gg_varpro conditional=TRUE returns ggplot with FacetWrap", {
  vp <- make_vp_class()
  gg <- gg_varpro(vp, conditional = TRUE)
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
})
```

- [ ] **Step 2: Run — expect all plot tests FAIL**

```bash
Rscript -e "devtools::test(filter='gg_varpro')" 2>&1 | grep -E "FAIL|Error"
```

Expected: 6 failures (plot.gg_varpro not found).

### Step 3: Create `R/plot.gg_varpro.R`

```r
##=============================================================================
#' Plot a \code{gg_varpro} variable importance object
#'
#' Renders a horizontal boxplot of per-tree importance z-scores (or raw
#' importances) with optional per-tree jitter overlay (\code{faithful=TRUE})
#' or class-conditional facets (\code{conditional=TRUE}).
#'
#' @param x A \code{gg_varpro} object from \code{\link{gg_varpro}}.
#' @param type Character; \code{"z"} (default) displays the z-normalised
#'   importance scale.  \code{"raw"} displays raw per-tree importance and
#'   requires \code{local.std = FALSE} at extract time.
#' @param ... Not currently used.
#'
#' @details
#' **Honest boxplot geometry:** Hinges are the 15th and 85th percentiles of
#' the per-tree z-distribution; whiskers extend to the 5th and 95th
#' percentiles.  This is \strong{not} a Tukey boxplot.  A mandatory plot
#' caption states this explicitly.
#'
#' **\code{faithful = TRUE}:** Per-tree z-scores are overlaid as jittered
#' semi-transparent points; the box is drawn at reduced opacity; a
#' white-outlined filled dot marks the mean.
#'
#' **\code{conditional = TRUE}:** The conditional class-importance scores
#' (\code{$conditional}) are shown as a faceted bar chart
#' (\code{facet_wrap(~class, nrow = 1)}); variable sort order follows the
#' unconditional median z from \code{$stats}.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{gg_varpro}}, \code{\link{autoplot.gg_varpro}}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#' plot(gg_varpro(vp))
#' plot(gg_varpro(vp, faithful = TRUE))
#' }
#'
#' @name plot.gg_varpro
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_point
#'   geom_col geom_vline geom_hline coord_flip facet_wrap scale_fill_manual
#'   labs theme_minimal
#' @importFrom tidyr pivot_longer
#' @export
plot.gg_varpro <- function(x, type = c("z", "raw"), ...) {
  type <- match.arg(type)
  prov <- attr(x, "provenance")

  ## ---- Validation ---------------------------------------------------------
  if (identical(type, "raw") && isTRUE(prov$local.std)) {
    stop("type = 'raw' requires local.std = FALSE at gg_varpro() extract time.",
         call. = FALSE)
  }

  ## ---- Conditional view ---------------------------------------------------
  if (!is.null(x$conditional)) {
    return(.plot_varpro_conditional(x, prov))
  }

  ## ---- Default / faithful view --------------------------------------------
  .plot_varpro_main(x, type, prov)
}

## ---- Internal renderers ----------------------------------------------------

.plot_varpro_main <- function(x, type, prov) {
  stats_df <- x$stats
  # Merge selected flag
  sel_df <- unique(x$imp[, c("variable", "selected")])
  stats_df <- merge(stats_df, sel_df, by = "variable", all.x = TRUE)

  faithful  <- isTRUE(prov$faithful)
  cutoff    <- prov$cutoff %||% 0.79
  fill_vals <- c("TRUE" = "#4e8fcd", "FALSE" = "#888888")
  cap_text  <- paste0(
    "Hinges: 15th/85th percentiles; whiskers: 5th/95th. ",
    if (faithful) "Points show per-tree importance. " else "",
    "Not a Tukey boxplot."
  )

  p <- ggplot2::ggplot(stats_df,
         ggplot2::aes(x = variable,
                      ymin   = q05,
                      lower  = q15,
                      middle = median,
                      upper  = q85,
                      ymax   = q95,
                      fill   = factor(selected))) +
    ggplot2::geom_boxplot(stat = "identity",
                          alpha = if (faithful) 0.4 else 0.85) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::geom_hline(yintercept = cutoff,
                        linetype = "dashed", color = "#e74c3c", linewidth = 0.7) +
    ggplot2::labs(x = NULL,
                  y = .varpro_imp_ylabel(type, prov),
                  caption = cap_text) +
    ggplot2::theme_minimal()

  ## ---- faithful overlay ---------------------------------------------------
  if (faithful && !is.null(x$imp.tree)) {
    ntree <- nrow(x$imp.tree)
    long_df <- as.data.frame(x$imp.tree)
    long_df$tree <- seq_len(ntree)
    long_df <- tidyr::pivot_longer(long_df, -tree,
                                   names_to = "variable", values_to = "imp_raw")
    # Normalise to z-scale (same transform as .varpro_imp_stats)
    sd_j <- tapply(long_df$imp_raw, long_df$variable,
                   stats::sd, na.rm = TRUE)
    sd_j[sd_j < .Machine$double.eps] <- 1
    long_df$z <- long_df$imp_raw / (sd_j[long_df$variable] / sqrt(ntree))
    long_df$variable <- factor(long_df$variable, levels = levels(x$imp$variable))

    mean_df <- stats_df
    mean_df$mean_z <- mean_df$mean /
      (sd_j[as.character(mean_df$variable)] / sqrt(ntree))

    p <- p +
      ggplot2::geom_jitter(data = long_df,
                           ggplot2::aes(x = variable, y = z),
                           inherit.aes = FALSE,
                           height = 0.15, alpha = 0.4, size = 1.2,
                           color = "#4e8fcd") +
      ggplot2::geom_point(data = mean_df,
                          ggplot2::aes(x = variable, y = mean_z),
                          inherit.aes = FALSE,
                          shape = 21, fill = "white", color = "#4e8fcd",
                          size = 2.5)
  }

  p
}

.plot_varpro_conditional <- function(x, prov) {
  ## Class-conditional z-scores as faceted bar chart.
  cond_df   <- x$conditional
  cond_df$variable <- factor(cond_df$variable, levels = levels(x$imp$variable))
  cutoff    <- prov$cutoff %||% 0.79

  ggplot2::ggplot(cond_df,
                  ggplot2::aes(x = variable, y = z, fill = z > cutoff)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ class, nrow = 1L) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
      guide  = "none"
    ) +
    ggplot2::geom_hline(yintercept = cutoff,
                        linetype = "dashed", color = "#e74c3c",
                        linewidth = 0.7) +
    ggplot2::labs(x = NULL,
                  y = "Variable importance (z)",
                  caption = paste0("Dashed line at z = ", cutoff,
                                   ". Conditional class importance.")) +
    ggplot2::theme_minimal()
}

.varpro_imp_ylabel <- function(type, prov) {
  if (identical(type, "raw")) {
    "Variable importance"
  } else {
    "Variable importance (z)"
  }
}
```

- [ ] **Step 4: Run plot tests — expect all PASS**

```bash
Rscript -e "devtools::test(filter='gg_varpro')" 2>&1 | tail -10
```

Expected: ~23 tests pass, 0 failures.

- [ ] **Step 5: Roxygenize + R CMD check**

```bash
Rscript -e "roxygen2::roxygenize()"
Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual')" 2>&1 | grep -E "^(ERROR|WARNING|NOTE|OK)"
```

Expected: 0 errors, 0 warnings.

- [ ] **Step 6: Lintr check (cyclocomp ≤ 20)**

```bash
Rscript -e "lintr::lint('R/gg_varpro.R'); lintr::lint('R/plot.gg_varpro.R')"
```

Expected: 0 lints. If `cyclocomp_linter` fires, extract helper functions
until each function body has complexity ≤ 20.

- [ ] **Step 7: Commit**

```bash
git add R/plot.gg_varpro.R tests/testthat/test_gg_varpro.R NAMESPACE man/
git commit -m "feat: add plot.gg_varpro — honest boxplot + faithful + conditional (TDD)"
```

---

## Task 3: `autoplot`, `print`, `summary` S3 methods (TDD)

**Files:**
- Modify: `R/autoplot_methods.R`
- Modify: `R/print_methods.R`
- Modify: `R/summary_methods.R`
- Modify: `tests/testthat/test_gg_varpro.R`

### Step 1: Add companion smoke tests

Append to `tests/testthat/test_gg_varpro.R`:

```r
## ── S3 companions ────────────────────────────────────────────────────────────

test_that("autoplot.gg_varpro returns a ggplot", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  expect_s3_class(ggplot2::autoplot(gg), "ggplot")
})

test_that("print.gg_varpro returns object invisibly", {
  vp  <- make_vp_regr()
  gg  <- gg_varpro(vp)
  out <- capture.output(ret <- print(gg))
  expect_identical(ret, gg)
  expect_true(any(grepl("gg_varpro", out)))
})

test_that("print.gg_varpro output contains selected/total counts", {
  vp  <- make_vp_regr()
  gg  <- gg_varpro(vp, cutoff = 0.79)
  out <- capture.output(print(gg))
  expect_true(any(grepl("selected", out, ignore.case = TRUE)))
})

test_that("summary.gg_varpro returns summary.gg_varpro class", {
  vp <- make_vp_regr()
  gg <- gg_varpro(vp)
  s  <- summary(gg)
  expect_s3_class(s, "summary.gg_varpro")
})
```

- [ ] **Step 2: Run — expect 4 failures**

```bash
Rscript -e "devtools::test(filter='gg_varpro')" 2>&1 | grep -E "FAIL|Error"
```

### Step 3: Add `autoplot.gg_varpro` to `R/autoplot_methods.R`

In `R/autoplot_methods.R`:

1. In the `@details` `\describe{}` block, add after the `gg_partial_varpro` item:
   ```r
   #'   \item{\code{gg_varpro}}{Variable importance from \code{varPro}}
   ```

2. Append at the end of the file:
   ```r
   #' @rdname autoplot.gg
   #' @export
   autoplot.gg_varpro <- function(object, ...) {
     plot(object, ...)
   }
   ```

### Step 4: Add `print.gg_varpro` to `R/print_methods.R`

Append at the end of `R/print_methods.R`:

```r
#' @rdname print.gg
#' @export
print.gg_varpro <- function(x, ...) {
  prov     <- attr(x, "provenance")
  cutoff   <- if (!is.null(prov)) prov$cutoff   %||% 0.79 else 0.79
  faithful <- if (!is.null(prov)) prov$faithful  %||% FALSE else FALSE
  family   <- if (!is.null(prov)) prov$family    %||% NA_character_ else NA_character_
  n_total  <- nrow(x$imp)
  n_sel    <- sum(x$imp$selected, na.rm = TRUE)
  cat(.gg_header(x, "gg_varpro"),
      sprintf("  |  family: %s", family),
      sprintf("  |  cutoff: %.2g", cutoff),
      sprintf("  |  faithful: %s", faithful),
      "\n",
      sprintf("  %d of %d variables selected (z > %.2g)\n",
              n_sel, n_total, cutoff),
      sep = "")
  invisible(x)
}
```

### Step 5: Add `summary.gg_varpro` to `R/summary_methods.R`

Append the helper and method at the end of `R/summary_methods.R`:

```r
.varpro_body <- function(x) {
  prov   <- attr(x, "provenance")
  cutoff <- if (!is.null(prov)) prov$cutoff %||% 0.79 else 0.79
  n_sel  <- sum(x$imp$selected, na.rm = TRUE)
  top_n  <- min(5L, nrow(x$stats))
  top_df <- x$stats[order(-x$stats$median), ][seq_len(top_n), ]
  c(
    sprintf("variables: %d total, %d selected (z > %.2g)",
            nrow(x$imp), n_sel, cutoff),
    sprintf("top %d by median z: %s", top_n,
            paste(sprintf("%s (%.3g)",
                          as.character(top_df$variable),
                          top_df$median),
                  collapse = ", ")),
    sprintf("z range: [%.3g, %.3g]",
            min(x$imp$z, na.rm = TRUE),
            max(x$imp$z, na.rm = TRUE))
  )
}

#' @rdname summary.gg
#' @export
summary.gg_varpro <- function(object, ...) {
  .summary_skel(object, "gg_varpro", .varpro_body(object))
}
```

- [ ] **Step 6: Run — expect all PASS**

```bash
Rscript -e "devtools::test(filter='gg_varpro')" 2>&1 | tail -10
```

Expected: ~27 tests pass.

- [ ] **Step 7: Roxygenize + R CMD check + lintr**

```bash
Rscript -e "roxygen2::roxygenize()"
Rscript -e "rcmdcheck::rcmdcheck(args='--no-manual')" 2>&1 | grep -E "^(ERROR|WARNING|NOTE|OK)"
Rscript -e "lintr::lint_package()" 2>&1 | grep -v "^$"
```

Expected: 0 errors, 0 warnings, 0 lints.

- [ ] **Step 8: Commit**

```bash
git add R/autoplot_methods.R R/print_methods.R R/summary_methods.R \
        tests/testthat/test_gg_varpro.R NAMESPACE man/
git commit -m "feat: add autoplot/print/summary S3 methods for gg_varpro (TDD)"
```

---

## Task 4: vdiffr visual regression snapshots

**Files:**
- Modify: `tests/testthat/test_snapshots.R`

- [ ] **Step 1: Add snapshot tests inside the existing guard**

Open `tests/testthat/test_snapshots.R`. The file ends with `} # end CI guard`.
Insert the following block immediately before that closing brace:

```r
  ## ---- gg_varpro snapshots ------------------------------------------------
  local({
    set.seed(42L)
    vp_regr <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50L)
    vp_class <- varPro::varpro(Species ~ ., data = iris, ntree = 50L)

    test_that("snapshot: gg-varpro-default", {
      gg <- gg_varpro(vp_regr)
      vdiffr::expect_doppelganger("gg-varpro-default", plot(gg))
    })

    test_that("snapshot: gg-varpro-faithful", {
      gg <- gg_varpro(vp_regr, faithful = TRUE)
      vdiffr::expect_doppelganger("gg-varpro-faithful", plot(gg))
    })

    test_that("snapshot: gg-varpro-conditional", {
      gg <- gg_varpro(vp_class, conditional = TRUE)
      vdiffr::expect_doppelganger("gg-varpro-conditional", plot(gg))
    })
  })
```

- [ ] **Step 2: Generate baseline SVGs**

```bash
Rscript -e "
  Sys.setenv(VDIFFR_RUN_TESTS = 'true')
  devtools::test(filter = 'snapshots')
"
```

Expected: 3 new files created in `tests/testthat/_snaps/snapshots/`:
- `gg-varpro-default.svg`
- `gg-varpro-faithful.svg`
- `gg-varpro-conditional.svg`

If SVGs look wrong, fix the plot code and re-run. Accept with:

```bash
Rscript -e "testthat::snapshot_accept()"
```

- [ ] **Step 3: Verify snapshots pass in CI mode**

```bash
Rscript -e "
  Sys.setenv(VDIFFR_RUN_TESTS = 'true')
  devtools::test(filter = 'snapshots')
" 2>&1 | tail -5
```

Expected: all snapshot tests pass (no new/changed files).

- [ ] **Step 4: Commit**

```bash
git add tests/testthat/test_snapshots.R \
        tests/testthat/_snaps/snapshots/gg-varpro-default.svg \
        tests/testthat/_snaps/snapshots/gg-varpro-faithful.svg \
        tests/testthat/_snaps/snapshots/gg-varpro-conditional.svg
git commit -m "test: add vdiffr snapshots for gg_varpro (default, faithful, conditional)"
```

---

## Task 5: `_pkgdown.yml`, `NEWS.md`, final gate, and PR

**Files:**
- Modify: `_pkgdown.yml`
- Modify: `NEWS.md`

### Step 1: Update `_pkgdown.yml`

Open `_pkgdown.yml`. Find the Variable Importance section:

```yaml
  - title: "Variable Importance"
    desc: "Assess and plot variable importance (VIMP)."
    contents:
      - gg_vimp
      - plot.gg_vimp
```

Replace with:

```yaml
  - title: "Variable Importance"
    desc: "Assess and plot variable importance (VIMP)."
    contents:
      - gg_vimp
      - plot.gg_vimp
      - gg_varpro
      - plot.gg_varpro
```

### Step 2: Update `NEWS.md`

Open `NEWS.md`. The file currently starts with:

```
Package: ggRandomForests
Version: 2.7.3.9002
```

Replace the first two lines with:

```
Package: ggRandomForests
Version: 2.7.3.9003
```

Then insert a new section after those two lines (before the existing
`ggRandomForests v2.8.0` header):

```md
ggRandomForests v2.8.0 (development) — continued
=================================================
* **varPro variable importance: `gg_varpro()` (#85).**
  - `gg_varpro()` extracts per-tree importance scores from a fitted
    `varpro` object and renders an honest boxplot — hinges at the
    15th/85th percentile, whiskers at the 5th/95th — of the per-tree
    z-score distribution per variable.  Variables whose aggregate
    z > `cutoff` (default 0.79) are colour-highlighted.
  - `faithful = TRUE` overlays individual per-tree z-scores as jittered
    semi-transparent points with a white-outlined mean dot, reproducing
    the distributional view from varPro's internal `bxp` output.
  - `conditional = TRUE` (classification forests only) extracts
    `$conditional.z` and renders class-conditional importance as a
    `facet_wrap(~class, nrow=1)` bar chart.
  - `local.std = FALSE` enables `plot(..., type = "raw")` to display
    raw per-tree importance instead of z-normalised values.
```

### Step 3: Final test suite

```bash
Rscript -e "devtools::test()" 2>&1 | tail -10
```

Expected: all tests pass, 0 failures.

### Step 4: Full lintr

```bash
Rscript -e "lintr::lint_package()" 2>&1 | grep -v "^$"
```

Expected: 0 lints. Fix any cyclocomp > 20 by extracting helpers.

### Step 5: R CMD check --as-cran

```bash
Rscript -e "rcmdcheck::rcmdcheck(args=c('--as-cran','--no-manual'))" \
  2>&1 | grep -E "^(ERROR|WARNING|NOTE|OK)"
```

Expected: 0 errors, 0 warnings, 0 notes.

### Step 6: Commit final changes

```bash
git add _pkgdown.yml NEWS.md
git commit -m "docs: update pkgdown and NEWS for gg_varpro (v2.7.3.9003)"
```

### Step 7: Open PR

```bash
gh pr create \
  --title "varPro Phase 2: gg_varpro — honest boxplot + faithful + conditional (#85)" \
  --body "$(cat <<'EOF'
## Summary

- Adds `gg_varpro()` extractor for varPro variable importance (honest 15th/85th boxplot)
- Adds `plot.gg_varpro()` with `faithful=TRUE` per-tree jitter overlay and `conditional=TRUE` class-faceted view
- Adds `autoplot`, `print`, `summary` S3 companions
- 3 vdiffr snapshots added
- Version bumped to `2.7.3.9003`

## Test plan
- [ ] `devtools::test()` — all tests pass
- [ ] `lintr::lint_package()` — 0 lints
- [ ] `R CMD check --as-cran` — 0 errors, 0 warnings, 0 notes
- [ ] vdiffr snapshots committed

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

---

## Self-review checklist

- [ ] Spec §3 (extractor signature): covered by T1 ✅
- [ ] Spec §4 (plot rendering — default, faithful, conditional, type="raw"): covered by T2 ✅
- [ ] Spec §5 (`@importFrom varPro importance`): in T1 roxygen block ✅
- [ ] Spec §6 (autoplot/print/summary): covered by T3 ✅
- [ ] Spec §7 (file changes — DESCRIPTION, NAMESPACE, pkgdown, NEWS): T0 + T5 ✅
- [ ] Spec §8 (test scope — validation, class, structure, provenance, cutoff/nvar, plot, companions, snapshots): covered by T1–T4 ✅
- [ ] Spec §9 (NEWS entry): T5 Step 2 ✅
- [ ] Spec §10 (acceptance criteria): all gates in T5 Steps 3–5 ✅
