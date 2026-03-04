# Tests for surv_partial.rfsrc
context("surv_partial.rfsrc tests")

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
