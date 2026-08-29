# Session-memoised varpro + beta.varpro fixtures for the gg_beta_varpro tests.
# beta.varpro() is the expensive call (per-rule glmnet); compute once per R
# session and reuse. In-memory only — no disk cache.
#
# randomForestSRC's gcc-UBSAN report at entry.c:184 fires for any rfsrc grow
# with NO outcome: rfsrc passes yvar.wt = numeric(0) and the native code
# decrements that zero-length pointer. The test is therefore not "is this
# function unsupervised?" but "does this call reach rfsrc without a formula?",
# which can happen several layers down:
#   * varPro::isopro(method = "unsupv")   — direct; skipped (test_gg_isopro.R)
#   * varPro::partialpro()                — calls isopro() internally and lets
#     it default to "unsupv" whenever more than one variable survives, so
#     gg_partial_varpro() reaches it without ever naming isopro. This is what
#     escaped the 3.5.0 audit; see test_gg_partial_varpro.R.
# uvarpro() defaults to method = "auto" and isopro(method = "rnd"/"auto") both
# pass a formula, so those are supervised grows and are clean.
#
# The fixtures below all grow *supervised* varpro/ivarpro/beta.varpro forests
# (a real Y), so yvar.wt is non-empty. They intentionally run on CRAN; do not
# skip_on_cran().

.varpro_cache <- new.env(parent = emptyenv())

.varpro_mtcars <- function() {
  if (is.null(.varpro_cache$v)) {
    if (!requireNamespace("varPro", quietly = TRUE)) {
      testthat::skip("varPro not installed")
    }
    set.seed(20260526L)
    .varpro_cache$v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
  }
  .varpro_cache$v
}

.beta_fit_mtcars <- function() {
  if (is.null(.varpro_cache$b)) {
    v <- .varpro_mtcars()
    set.seed(20260526L)
    .varpro_cache$b <- varPro::beta.varpro(v)
  }
  .varpro_cache$b
}

.varpro_iris_binary <- function() {
  if (is.null(.varpro_cache$vb)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    set.seed(20260526L)
    d <- iris[iris$Species != "setosa", ]
    d$Species <- droplevels(d$Species)
    .varpro_cache$vb <- varPro::varpro(Species ~ ., data = d, ntree = 30)
  }
  .varpro_cache$vb
}

.beta_fit_iris_binary <- function() {
  if (is.null(.varpro_cache$bb)) {
    set.seed(20260526L)
    .varpro_cache$bb <- varPro::beta.varpro(.varpro_iris_binary())
  }
  .varpro_cache$bb
}

.varpro_iris_multiclass <- function() {
  if (is.null(.varpro_cache$vm)) {
    if (!requireNamespace("varPro", quietly = TRUE)) testthat::skip("varPro not installed")
    set.seed(20260526L)
    .varpro_cache$vm <- varPro::varpro(Species ~ ., data = iris, ntree = 30)
  }
  .varpro_cache$vm
}

.beta_fit_iris_multiclass <- function() {
  if (is.null(.varpro_cache$bm)) {
    set.seed(20260526L)
    .varpro_cache$bm <- varPro::beta.varpro(.varpro_iris_multiclass())
  }
  .varpro_cache$bm
}

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

# Lightweight synthetic gg_varpro fixture for plot.gg_varpro() labels tests.
# .plot_varpro_main() draws from x$stats (variable, q05, q15, median, q85,
# q95 -- NOT "selected"; that column lives on x$imp only, and .plot_varpro_main()
# merges it in) and reads cutoff from the provenance attribute. Mirrors the
# real shape produced by .varpro_imp_stats() in R/gg_varpro.R.
# When conditional = TRUE, includes class-conditional data for .plot_varpro_conditional().
make_mock_gg_varpro <- function(vars = c("bpd", "vis", "age"), conditional = FALSE) {
  stats <- data.frame(variable = factor(vars, levels = vars),
                      q05      = c(1.0, 0.6, 0.1),
                      q15      = c(1.5, 0.9, 0.2),
                      median   = c(2.1, 1.4, 0.5),
                      q85      = c(2.6, 1.8, 0.8),
                      q95      = c(3.0, 2.1, 1.0),
                      mean     = c(2.1, 1.4, 0.5),
                      stringsAsFactors = FALSE)
  imp <- data.frame(variable = factor(vars, levels = vars),
                    z        = c(2.1, 1.4, 0.5),
                    selected = c(TRUE, TRUE, FALSE),
                    stringsAsFactors = FALSE)

  ## Build conditional data frame if requested: one row per variable per class.
  cond_data <- NULL
  if (conditional) {
    classes <- c("class_a", "class_b")
    cond_rows <- expand.grid(
      variable = factor(vars, levels = vars),
      class = factor(classes, levels = classes),
      stringsAsFactors = FALSE
    )
    cond_rows$z <- c(1.5, 0.8, 0.4, 2.0, 1.2, 0.3)  # Heterogeneous z per class
    cond_data <- cond_rows
  }

  out <- structure(
    list(imp = imp, imp.tree = NULL, stats = stats, conditional = cond_data),
    class = c("gg_varpro", "list")
  )
  attr(out, "provenance") <- list(family      = "class",
                                  local.std   = TRUE,
                                  cutoff      = 0.79,
                                  faithful    = FALSE,
                                  conditional = conditional,
                                  xvar.names  = vars,
                                  n           = 200L)
  out
}

# Variable-axis tick labels from a built plot.  These methods coord_flip(), so
# the variable categories land on the y scale.  Faceted plots have one panel
# per class, so collect across every panel.
.varpro_axis_labels <- function(p) {
  built <- ggplot2::ggplot_build(p)
  unlist(lapply(built$layout$panel_params,
                function(pp) as.character(pp$y$get_labels())))
}
