# Tests for surv_partial.rfsrc

# Survival formula helper (rfsrc requires Surv to be in local scope)
Surv <- survival::Surv

test_that("surv_partial.rfsrc returns list with one element per variable", {
  skip_if_not_installed("randomForestSRC")

  data(veteran, package = "randomForestSRC")
  set.seed(42)
  v.obj <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  result <- surv_partial.rfsrc(v.obj, var_list = "age", partial.type = "mort")

  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(result[[1]]$name, "age")
})

test_that("surv_partial.rfsrc result element has name and dta fields", {
  skip_if_not_installed("randomForestSRC")

  data(veteran, package = "randomForestSRC")
  set.seed(42)
  v.obj <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  result <- surv_partial.rfsrc(v.obj, var_list = "age", partial.type = "mort")

  expect_named(result[[1]], c("name", "dta"))
  expect_true(!is.null(result[[1]]$dta))
})

test_that("surv_partial.rfsrc processes multiple variables", {
  skip_if_not_installed("randomForestSRC")

  data(veteran, package = "randomForestSRC")
  set.seed(42)
  v.obj <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  result <- surv_partial.rfsrc(v.obj,
    var_list = c("age", "karno"),
    partial.type = "mort"
  )

  expect_length(result, 2)
  names_out <- sapply(result, function(x) x$name)
  expect_true("age" %in% names_out)
  expect_true("karno" %in% names_out)
})

test_that("surv_partial.rfsrc works with surv partial.type", {
  skip_if_not_installed("randomForestSRC")

  data(veteran, package = "randomForestSRC")
  set.seed(42)
  v.obj <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  result <- surv_partial.rfsrc(v.obj, var_list = "age", partial.type = "surv")

  expect_type(result, "list")
  expect_length(result, 1)
  expect_equal(result[[1]]$name, "age")
})

test_that("surv_partial.rfsrc npts argument limits unique x values", {
  skip_if_not_installed("randomForestSRC")

  data(veteran, package = "randomForestSRC")
  set.seed(42)
  v.obj <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  result <- surv_partial.rfsrc(v.obj,
    var_list = "age",
    partial.type = "mort",
    npts = 5
  )

  expect_type(result, "list")
  expect_length(result, 1)
})

## ---- Shared fixture (built once, reused below) ----------------------------

local({
  skip_if_not_installed("randomForestSRC")
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  v.obj <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  test_that("surv_partial.rfsrc dta element has x and yhat columns", {
    result <- surv_partial.rfsrc(v.obj, var_list = "age", partial.type = "mort")
    dta <- result[[1]]$dta
    expect_true(!is.null(dta))
    # get.partial.plot.data returns a list with $x (predictor values) and
    # $yhat (matrix of partial predictions, one column per time point)
    expect_true("x" %in% names(dta))
    expect_true("yhat" %in% names(dta))
  })

  test_that("surv_partial.rfsrc npts limits evaluation points", {
    npts_requested <- 5L
    result <- surv_partial.rfsrc(v.obj,
      var_list = "age",
      partial.type = "mort",
      npts = npts_requested
    )
    dta <- result[[1]]$dta
    # The number of evaluation points should be <= npts_requested
    expect_true(length(dta$x) <= npts_requested)
  })

  test_that("surv_partial.rfsrc mort and surv partial.types return different yhat scales", {
    res_mort <- surv_partial.rfsrc(v.obj, var_list = "age", partial.type = "mort")
    res_surv <- surv_partial.rfsrc(v.obj, var_list = "age", partial.type = "surv")

    yhat_mort <- res_mort[[1]]$dta$yhat
    yhat_surv <- res_surv[[1]]$dta$yhat

    # Mortality (years lost) and survival (probability) are on different scales;
    # survival probabilities are bounded [0, 1]; mortality values are unbounded
    if (is.matrix(yhat_surv)) {
      expect_true(all(yhat_surv >= 0 & yhat_surv <= 1 + 1e-8))
    }
    # The two types should not produce identical predictions
    expect_false(identical(yhat_mort, yhat_surv))
  })

  test_that("surv_partial.rfsrc verbose: prints variable name during computation", {
    expect_output(
      surv_partial.rfsrc(v.obj, var_list = "age", partial.type = "mort"),
      regexp = "age"
    )
  })

  test_that("surv_partial.rfsrc errors on invalid variable name", {
    expect_error(
      surv_partial.rfsrc(v.obj, var_list = "nonexistent_var", partial.type = "mort")
    )
  })

  test_that("surv_partial.rfsrc result names match requested var_list order", {
    vars <- c("karno", "age", "diagtime")
    result <- surv_partial.rfsrc(v.obj, var_list = vars, partial.type = "mort")
    names_out <- vapply(result, function(x) x$name, character(1L))
    expect_equal(names_out, vars)
  })
})
