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
