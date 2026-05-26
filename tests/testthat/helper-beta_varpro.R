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
