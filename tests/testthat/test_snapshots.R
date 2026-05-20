# Visual regression tests using vdiffr.
#
# These tests generate reference SVGs on the first run and compare on
# subsequent runs. To regenerate snapshots (e.g. after an intentional
# visual change) run: vdiffr::manage_cases()
#
# All models are built with set.seed() to ensure reproducible plots.

# Guard: only register snapshot tests when explicitly opted in AND vdiffr is
# available. Set VDIFFR_RUN_TESTS=true to generate or compare visual baselines.
# This avoids failures on fresh checkouts (no _snaps/ directory) and in CI,
# and avoids a top-level skip() that would appear as an empty test in reports.
#
# To generate baselines locally:
#   1. Run Sys.setenv(VDIFFR_RUN_TESTS = "true")
#   2. Run devtools::test(filter = "snapshots")
#   3. Call testthat::snapshot_accept()
#   4. Commit tests/testthat/_snaps/ to the repo
if (requireNamespace("vdiffr", quietly = TRUE) &&
    identical(Sys.getenv("VDIFFR_RUN_TESTS"), "true")) {

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

  test_that("snapshot: gg_brier overall line", {
    gg_dta <- gg_brier(rfsrc_pbc)
    vdiffr::expect_doppelganger("gg_brier survival overall", plot(gg_dta))
  })

  test_that("snapshot: gg_brier with envelope ribbon", {
    gg_dta <- gg_brier(rfsrc_pbc)
    vdiffr::expect_doppelganger("gg_brier survival envelope",
                                plot(gg_dta, envelope = TRUE))
  })

  test_that("snapshot: gg_brier CRPS", {
    gg_dta <- gg_brier(rfsrc_pbc)
    vdiffr::expect_doppelganger("gg_brier survival crps",
                                plot(gg_dta, type = "crps"))
  })
})

  local({
    set.seed(42)
    rf_iris <- randomForest::randomForest(Species ~ ., data = iris,
                                          importance = TRUE)
    test_that("snapshot: gg_rfsrc classification rf", {
      vdiffr::expect_doppelganger("gg_rfsrc classification rf",
                                  plot(gg_rfsrc(rf_iris)))
    })
    test_that("snapshot: gg_error classification rf", {
      vdiffr::expect_doppelganger("gg_error classification rf",
                                  plot(gg_error(rf_iris)))
    })
    test_that("snapshot: gg_vimp classification rf", {
      vdiffr::expect_doppelganger("gg_vimp classification rf",
                                  plot(gg_vimp(rf_iris)))
    })
    test_that("snapshot: gg_roc classification rf", {
      vdiffr::expect_doppelganger("gg_roc classification rf",
                                  plot(gg_roc(rf_iris, which_outcome = 1)))
    })
  })
  local({
    set.seed(42)
    rf_mt <- randomForest::randomForest(mpg ~ ., data = mtcars,
                                        importance = TRUE)
    test_that("snapshot: gg_rfsrc regression rf", {
      vdiffr::expect_doppelganger("gg_rfsrc regression rf",
                                  plot(gg_rfsrc(rf_mt)))
    })
    test_that("snapshot: gg_error regression rf", {
      vdiffr::expect_doppelganger("gg_error regression rf",
                                  plot(gg_error(rf_mt)))
    })
    test_that("snapshot: gg_vimp regression rf", {
      vdiffr::expect_doppelganger("gg_vimp regression rf",
                                  plot(gg_vimp(rf_mt)))
    })
    test_that("snapshot: gg_variable regression rf", {
      vdiffr::expect_doppelganger("gg_variable regression rf",
                                  plot(gg_variable(rf_mt)))
    })
  })

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
    result <- gg_partial_varpro(make_mock_vpro_snap(), nvars = 1)
    vdiffr::expect_doppelganger("gg-partial-varpro-continuous", plot(result))
  })

  test_that("snapshot: gg-partial-varpro-categorical", {
    mock <- make_mock_vpro_snap()
    result <- gg_partial_varpro(mock["sex"])
    vdiffr::expect_doppelganger("gg-partial-varpro-categorical", plot(result))
  })

  test_that("snapshot: gg-partial-varpro-both", {
    result <- gg_partial_varpro(make_mock_vpro_snap())
    vdiffr::expect_doppelganger("gg-partial-varpro-both", plot(result))
  })

  test_that("snapshot: gg-partial-varpro-mortality", {
    result <- gg_partial_varpro(make_mock_vpro_snap(), nvars = 1,
                                 scale = "mortality")
    vdiffr::expect_doppelganger("gg-partial-varpro-mortality", plot(result))
  })

} # end CI guard
