# Session-memoised varpro + beta.varpro fixtures for the gg_beta_varpro tests.
# beta.varpro() is the expensive call (per-rule glmnet); compute once per R
# session and reuse. In-memory only — no disk cache.
#
# These all grow *supervised* varpro/ivarpro/beta.varpro forests (a real Y), so
# yvar.wt is non-empty and they do NOT trip randomForestSRC's gcc-UBSAN report
# at entry.c:184 (that fires only for the unsupervised family — verified under
# -fsanitize=undefined). They intentionally run on CRAN; do not skip_on_cran().
# Only the unsupervised isopro grow is skipped (see test_gg_isopro.R).

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
