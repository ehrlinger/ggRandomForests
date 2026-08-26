test_that("the precomputed RHF vignette bundle satisfies its artifact contract", {
  path <- testthat::test_path("..", "..", "vignettes", "rhf_precomputed.rds")
  expect_true(file.exists(path), info = "Missing vignettes/rhf_precomputed.rds")
  if (!file.exists(path)) {
    return(invisible())
  }

  bundle <- readRDS(path)
  expect_identical(names(bundle), c(
    "data", "fit", "auct_cumulative", "auct_incident", "importance",
    "tune_risk", "tune_iauc", "seed", "settings", "versions"
  ))
  expect_lte(file.info(path)$size, 1.75 * 1024^2)

  expect_s3_class(bundle$fit, "rhf")
  expect_s3_class(bundle$tune_risk, "tune.treesize.rhf")
  expect_s3_class(bundle$tune_iauc, "tune.treesize.rhf")
  expect_false("forest" %in% names(bundle$tune_risk))
  expect_false("forest" %in% names(bundle$tune_iauc))

  expect_equal(
    bundle$data$xtd,
    (bundle$data$x.4 + bundle$data$x.5) * bundle$data$stop
  )
  expect_gt(length(unique(bundle$data$id)), 1L)
  expect_true(any(duplicated(bundle$data$id)))
  expect_true(all(bundle$data$start < bundle$data$stop))

  expect_identical(bundle$auct_cumulative$method, "cumulative")
  expect_identical(bundle$auct_cumulative$marker, "cumhaz")
  expect_identical(bundle$auct_incident$method, "incident")
  expect_identical(bundle$auct_incident$marker, "hazard")

  time_index <- bundle$settings$importance_time_index
  expect_length(time_index, 5L)
  expect_true(is.integer(time_index))
  expect_true(all(time_index > 0L))
  expect_identical(length(unique(time_index)), 5L)

  expect_identical(bundle$tune_risk$perf, "risk")
  expect_identical(bundle$tune_iauc$perf, "iAUC")
  expect_identical(bundle$seed, 20260825L)

  expect_identical(names(bundle$settings), c(
    "formula", "fit", "auct_cumulative", "auct_incident",
    "importance_cache", "importance_time_index", "tune_risk", "tune_iauc"
  ))
  expect_identical(bundle$settings$formula, "Surv(id, start, stop, event) ~ .")
  expect_identical(bundle$settings$fit, list(ntree = 50L, seed = -1L))
  expect_identical(bundle$settings$auct_cumulative, list(
    marker = "cumhaz", method = "cumulative", verbose = FALSE
  ))
  expect_identical(bundle$settings$auct_incident, list(
    marker = "hazard", method = "incident", riskset = "subject", verbose = FALSE
  ))
  expect_identical(bundle$settings$importance_cache, list(
    max.rules.tree = 30L, max.tree = 20L, verbose = FALSE
  ))
  expect_identical(bundle$settings$tune_risk, list(
    ntree = 20L, perf = "risk", lower = 2L, upper = 6L, max.evals = 5L,
    seed = 20260825L, verbose = FALSE, forest = FALSE
  ))
  expect_identical(bundle$settings$tune_iauc, list(
    ntree = 20L, lower = 2L, upper = 6L, max.evals = 5L,
    seed = 20260825L, verbose = FALSE, forest = FALSE
  ))

  expect_identical(names(bundle$versions), c(
    "R", "ggRandomForests", "randomForestRHF", "ggplot2"
  ))
  expect_true(all(nzchar(unname(bundle$versions))))
  expect_false(any(startsWith(
    unlist(lapply(bundle, class), use.names = FALSE), "gg_"
  )))
})
