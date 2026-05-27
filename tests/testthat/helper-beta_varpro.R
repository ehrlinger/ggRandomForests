# Session-memoised varpro + beta.varpro fixtures for the gg_beta_varpro tests.
# beta.varpro() is the expensive call (per-rule glmnet); compute once per R
# session and reuse. In-memory only — no disk cache.

.beta_varpro_cache <- new.env(parent = emptyenv())

.varpro_mtcars <- function() {
  if (is.null(.beta_varpro_cache$v)) {
    if (!requireNamespace("varPro", quietly = TRUE)) {
      testthat::skip("varPro not installed")
    }
    set.seed(20260526L)
    .beta_varpro_cache$v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
  }
  .beta_varpro_cache$v
}

.beta_fit_mtcars <- function() {
  if (is.null(.beta_varpro_cache$b)) {
    v <- .varpro_mtcars()
    set.seed(20260526L)
    .beta_varpro_cache$b <- varPro::beta.varpro(v)
  }
  .beta_varpro_cache$b
}

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
