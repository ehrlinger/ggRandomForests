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
if (requireNamespace("vdiffr", quietly = TRUE) && # nolint: cyclocomp_linter
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

  ## ---- gg_varpro snapshots ------------------------------------------------
  local({
    set.seed(42L)
    vp_regr  <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50L)
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

  ## ---- gg_udependent snapshots ---------------------------------------------
  if (requireNamespace("ggraph", quietly = TRUE)) {
    local({
      set.seed(42L)
      uv <- varPro::uvarpro(iris[, -5L], ntree = 50L)

      test_that("snapshot: gg-udependent-default", {
        gg <- gg_udependent(uv)
        vdiffr::expect_doppelganger("gg-udependent-default", plot(gg))
      })

      test_that("snapshot: gg-udependent-undirected", {
        gg <- gg_udependent(uv, directed = FALSE)
        vdiffr::expect_doppelganger("gg-udependent-undirected", plot(gg))
      })
    })
  }

## ── randomForest classification snapshots (PR #87) ───────────────────────────
if (requireNamespace("randomForest", quietly = TRUE)) {
  local({
    set.seed(42L)
    rf_iris <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50L)
    gg_iris <- gg_variable(rf_iris)

    test_that("snapshot: gg-variable-rf-classification-default", {
      vdiffr::expect_doppelganger(
        "gg-variable-rf-classification-default",
        plot(gg_iris)
      )
    })

    test_that("snapshot: gg-variable-rf-classification-smooth", {
      vdiffr::expect_doppelganger(
        "gg-variable-rf-classification-smooth",
        plot(gg_iris, xvar = "Sepal.Length", smooth = TRUE)
      )
    })
  })
}

## ── per_class ROC snapshots (PR #88) ─────────────────────────────────────
if (requireNamespace("randomForest", quietly = TRUE)) {
  local({
    set.seed(1L)
    rf_iris    <- randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
    gg_pc_iris <- gg_roc(rf_iris, per_class = TRUE)

    test_that("snapshot: gg-roc-multiclass-overlay", {
      vdiffr::expect_doppelganger(
        "gg-roc-multiclass-overlay",
        plot(gg_pc_iris, panel = "overlay")
      )
    })

    test_that("snapshot: gg-roc-multiclass-facet", {
      vdiffr::expect_doppelganger(
        "gg-roc-multiclass-facet",
        plot(gg_pc_iris, panel = "facet")
      )
    })
  })
}

## ── gg_isopro snapshots (Phase 4) ────────────────────────────────────────
if (requireNamespace("varPro", quietly = TRUE)) {
  local({
    set.seed(1L)
    fit <- varPro::isopro(data = iris[, 1:4], method = "rnd",
                          sampsize = 32, ntree = 50)
    gg  <- gg_isopro(fit)

    test_that("snapshot: gg-isopro-default", {
      vdiffr::expect_doppelganger("gg-isopro-default", plot(gg))
    })

    test_that("snapshot: gg-isopro-threshold", {
      vdiffr::expect_doppelganger("gg-isopro-threshold",
                                  plot(gg, threshold = 0.8))
    })
  })
}

## ── gg_isopro predict.isopro overlay snapshot (Phase 4b) ──────────────────
if (requireNamespace("varPro", quietly = TRUE)) {
  local({
    set.seed(1L)
    fit      <- varPro::isopro(data = iris[, 1:4], method = "rnd",
                               sampsize = 32, ntree = 50)
    test_df  <- iris[seq(1, nrow(iris), by = 3), 1:4]
    gg_train <- gg_isopro(fit)
    gg_test  <- gg_isopro(fit, newdata = test_df)
    gg_both  <- rbind(
      cbind(as.data.frame(gg_train), method = "train"),
      cbind(as.data.frame(gg_test),  method = "test")
    )
    class(gg_both) <- c("gg_isopro", "data.frame")

    test_that("snapshot: gg-isopro-predict-overlay", {
      vdiffr::expect_doppelganger("gg-isopro-predict-overlay", plot(gg_both))
    })
  })
}

## ── gg_beta_varpro default snapshot (Phase 4c) ─────────────────────────────
test_that("gg-beta-varpro-default", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v <- .varpro_mtcars()
  b <- .beta_fit_mtcars()
  p <- plot(gg_beta_varpro(v, beta_fit = b))
  vdiffr::expect_doppelganger("gg-beta-varpro-default", p)
})

## ── gg_beta_varpro classification snapshots (Phase 4c-classification) ─────
test_that("gg-beta-varpro-class-binary", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  vb <- .varpro_iris_binary()
  bb <- .beta_fit_iris_binary()
  p <- plot(gg_beta_varpro(vb, beta_fit = bb))
  vdiffr::expect_doppelganger("gg-beta-varpro-class-binary", p)
})

test_that("gg-beta-varpro-class-multiclass", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  vm <- .varpro_iris_multiclass()
  bm <- .beta_fit_iris_multiclass()
  p <- plot(gg_beta_varpro(vm, beta_fit = bm))
  vdiffr::expect_doppelganger("gg-beta-varpro-class-multiclass", p)
})

## ── gg_ivarpro snapshots (Phase 4d) ────────────────────────────────────────
test_that("gg-ivarpro-regr-distribution", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  set.seed(1L)
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv))
  vdiffr::expect_doppelganger("gg-ivarpro-regr-distribution", p)
})

test_that("gg-ivarpro-regr-which-obs", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v  <- .varpro_boston()
  iv <- .ivarpro_boston()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L))
  vdiffr::expect_doppelganger("gg-ivarpro-regr-which-obs", p)
})

test_that("gg-ivarpro-class-distribution", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  set.seed(1L)
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv))
  vdiffr::expect_doppelganger("gg-ivarpro-class-distribution", p)
})

test_that("gg-ivarpro-class-which-obs", {
  skip_if_not_installed("vdiffr")
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  v  <- .varpro_iris_multiclass_for_ivarpro()
  iv <- .ivarpro_iris_multiclass()
  p <- plot(gg_ivarpro(v, ivarpro_fit = iv, which_obs = 1L))
  vdiffr::expect_doppelganger("gg-ivarpro-class-which-obs", p)
})

test_that("gg-rhf-hazard", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("randomForestRHF")
  skip_on_cran()
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  gg <- gg_rhf(.rhf_pbc())
  vdiffr::expect_doppelganger("gg-rhf-hazard", plot(gg, idx = c(1, 5, 10)))
})

test_that("gg-rhf-chf", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("randomForestRHF")
  skip_on_cran()
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  gg <- gg_rhf(.rhf_pbc())
  vdiffr::expect_doppelganger("gg-rhf-chf",
                              plot(gg, idx = c(1, 5, 10), hazard.only = FALSE))
})

test_that("gg-auct-chf", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("randomForestRHF")
  skip_on_cran()
  if (!identical(Sys.getenv("VDIFFR_RUN_TESTS", "false"), "true")) {
    skip("vdiffr snapshots skipped (set VDIFFR_RUN_TESTS=true to run)")
  }
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_boot())
  vdiffr::expect_doppelganger("gg-auct-chf", plot(gg))
})

} else {

## ---- Preserve baselines when the vdiffr comparison is opted out ------------
# When VDIFFR_RUN_TESTS != "true" (or vdiffr is unavailable) none of the
# expect_doppelganger() calls above register, so testthat's snapshot cleanup
# would treat every committed baseline under _snaps/snapshots/ as orphaned and
# delete it during a normal `devtools::test()` run. Announcing each file marks
# it as still in use so cleanup leaves the repo untouched.
# See ?testthat::announce_snapshot_file.
test_that("vdiffr baseline snapshots are preserved when comparison is skipped", {
  # announce_snapshot_file() (and vdiffr's file snapshots) are 3rd-edition
  # features; the package otherwise defaults to edition 2, so opt in locally.
  local_edition(3)
  snap_dir <- test_path("_snaps", "snapshots")
  skip_if(!dir.exists(snap_dir), "no vdiffr baselines present to preserve")
  svgs <- list.files(snap_dir, pattern = "\\.svg$")
  for (f in svgs) announce_snapshot_file(name = f)
  succeed()
})

} # end CI guard
