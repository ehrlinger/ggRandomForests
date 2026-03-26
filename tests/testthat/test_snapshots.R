# Visual regression tests using vdiffr.
#
# These tests generate reference SVGs on the first run and compare on
# subsequent runs. To regenerate snapshots (e.g. after an intentional
# visual change) run: vdiffr::manage_cases()
#
# All models are built with set.seed() to ensure reproducible plots.

# Skip the entire file if vdiffr is not available (e.g. on CRAN).
if (!requireNamespace("vdiffr", quietly = TRUE)) {
  skip("vdiffr not installed")
}

# Guard: only register snapshot tests when NOT running on CI.
# testthat::skip_on_ci() at file scope only creates a phantom skip; it does
# not prevent test_that() blocks inside local() from being registered and run.
# Wrapping in a plain `if` is the reliable way to suppress the entire file.
#
# To generate baselines locally:
#   1. Ensure CI is unset (or run outside GitHub Actions)
#   2. Run devtools::test(filter = "snapshots")
#   3. Call testthat::snapshot_accept()
#   4. Commit tests/testthat/_snaps/ to the repo
if (!nzchar(Sys.getenv("CI"))) {

## ---- Shared fixtures -------------------------------------------------------

# Classification — iris
local({
  set.seed(42L)
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    importance = TRUE,
    tree.err = TRUE,
    ntree = 100L
  )

  test_that("snapshot: gg_vimp classification", {
    gg_dta <- gg_vimp(rfsrc_iris)
    vdiffr::expect_doppelganger("gg_vimp classification rfsrc", plot(gg_dta))
  })

  test_that("snapshot: gg_error classification", {
    gg_dta <- gg_error(rfsrc_iris)
    vdiffr::expect_doppelganger("gg_error classification rfsrc", plot(gg_dta))
  })

  test_that("snapshot: gg_roc classification rfsrc", {
    gg_dta <- gg_roc(rfsrc_iris, which_outcome = 1L)
    vdiffr::expect_doppelganger("gg_roc classification rfsrc", plot(gg_dta))
  })

  test_that("snapshot: gg_rfsrc classification", {
    gg_dta <- gg_rfsrc(rfsrc_iris)
    vdiffr::expect_doppelganger("gg_rfsrc classification rfsrc", plot(gg_dta))
  })
})

# Regression — Boston housing
local({
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  set.seed(42L)
  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    importance = TRUE,
    tree.err = TRUE,
    ntree = 100L
  )

  test_that("snapshot: gg_vimp regression", {
    gg_dta <- gg_vimp(rfsrc_boston)
    vdiffr::expect_doppelganger("gg_vimp regression rfsrc", plot(gg_dta))
  })

  test_that("snapshot: gg_error regression", {
    gg_dta <- gg_error(rfsrc_boston)
    vdiffr::expect_doppelganger("gg_error regression rfsrc", plot(gg_dta))
  })

  test_that("snapshot: gg_rfsrc regression", {
    gg_dta <- gg_rfsrc(rfsrc_boston)
    vdiffr::expect_doppelganger("gg_rfsrc regression rfsrc", plot(gg_dta))
  })

  test_that("snapshot: gg_variable regression single xvar", {
    gg_rfsrc_dta <- gg_rfsrc(rfsrc_boston)
    xvar <- "lstat"
    gg_dta <- gg_variable(rfsrc_boston, time = NULL)
    vdiffr::expect_doppelganger(
      "gg_variable regression lstat",
      plot(gg_dta, xvar = xvar)
    )
  })
})

# Survival — pbc
local({
  data(pbc, package = "randomForestSRC")
  pbc$time <- pbc$days / 364.25
  pbc_sub <- pbc[, c("time", "status", "treatment", "age", "bili", "albumin")]

  set.seed(42L)
  rfsrc_pbc <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = pbc_sub,
    importance = TRUE,
    tree.err = TRUE,
    ntree = 100L
  )

  test_that("snapshot: gg_vimp survival", {
    gg_dta <- gg_vimp(rfsrc_pbc)
    vdiffr::expect_doppelganger("gg_vimp survival rfsrc", plot(gg_dta))
  })

  test_that("snapshot: gg_error survival", {
    gg_dta <- gg_error(rfsrc_pbc)
    vdiffr::expect_doppelganger("gg_error survival rfsrc", plot(gg_dta))
  })

  test_that("snapshot: gg_rfsrc survival no CI", {
    gg_dta <- gg_rfsrc(rfsrc_pbc)
    vdiffr::expect_doppelganger("gg_rfsrc survival no ci", plot(gg_dta))
  })

  test_that("snapshot: gg_rfsrc survival with bootstrap CI", {
    set.seed(1L)
    gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int = 0.95, bs_samples = 50L)
    vdiffr::expect_doppelganger("gg_rfsrc survival bootstrap ci", plot(gg_dta))
  })
})

# randomForest classification — iris
local({
  set.seed(42L)
  rf_iris <- randomForest::randomForest(Species ~ ., data = iris)

  test_that("snapshot: gg_vimp randomForest classification", {
    gg_dta <- gg_vimp(rf_iris)
    vdiffr::expect_doppelganger("gg_vimp classification rf", plot(gg_dta))
  })

  test_that("snapshot: gg_error randomForest classification", {
    gg_dta <- gg_error(rf_iris)
    vdiffr::expect_doppelganger("gg_error classification rf", plot(gg_dta))
  })

  test_that("snapshot: gg_roc randomForest classification", {
    gg_dta <- gg_roc(rf_iris, which_outcome = 1L)
    vdiffr::expect_doppelganger("gg_roc classification rf", plot(gg_dta))
  })
})

} # end CI guard
