# testthat for gg_survival function

test_that("gg_survival classifications", {
  expect_error(gg_survival(data = iris))
})


test_that("gg_survival survival", {
  #   ## Load the cached forest
  data(pbc, package = "randomForestSRC")

  # Test the cached forest type
  expect_s3_class(pbc, "data.frame")

  # Test object type
  gg_dta <- gg_survival(
    interval = "days",
    censor = "status",
    by = "treatment",
    data = pbc,
    conf.int = .95
  )

  expect_s3_class(gg_dta, "gg_survival")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_survival(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  expect_s3_class(plot(gg_dta, error = "bars"), "ggplot")
  expect_s3_class(plot(gg_dta, error = "none"), "ggplot")
  expect_s3_class(plot(gg_dta, error = "lines"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "surv"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "cum_haz"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "density"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "mid_int"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "life"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "hazard"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "proplife"), "ggplot")
  # Test object type
  gg_dta <- gg_survival(
    interval = "days",
    censor = "status",
    by = "treatment",
    data = pbc,
    conf.int = .95,
    type = "nelson"
  )

  expect_s3_class(gg_dta, "gg_survival")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_survival(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")


  # Test object type
  gg_dta <- gg_survival(
    interval = "days",
    censor = "status",
    data = pbc,
    conf.int = .95
  )

  expect_s3_class(gg_dta, "gg_survival")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_survival(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  expect_s3_class(plot(gg_dta, error = "bars"), "ggplot")
  expect_s3_class(plot(gg_dta, error = "none"), "ggplot")
  expect_s3_class(plot(gg_dta, error = "lines"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "surv"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "cum_haz"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "density"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "mid_int"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "life"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "hazard"), "ggplot")
  expect_s3_class(plot(gg_dta, type = "proplife"), "ggplot")

})

test_that("gg_survival regression", {
  ## Load the data
  data(Boston, package = "MASS")

  ## Create the correct gg_error object
  expect_error(gg_survival(data = Boston))
})

test_that("gg_survival.rfsrc extracts KM from a survival forest", {
  skip_if_not_installed("randomForestSRC")

  veteran  <- survival::veteran
  Surv     <- survival::Surv  # nolint: object_name_linter
  set.seed(42)
  rf <- randomForestSRC::rfsrc(
    Surv(time, status) ~ trt + karno + diagtime + age + prior,
    data   = veteran,
    ntree  = 50,
    nsplit = 5
  )

  gg_dta <- gg_survival(rf)

  expect_s3_class(gg_dta, "gg_survival")
  expect_true(all(c("time", "surv", "lower", "upper") %in% colnames(gg_dta)))
  expect_s3_class(plot(gg_dta, error = "none"), "ggplot")
})

test_that("gg_survival.rfsrc supports stratification via by", {
  skip_if_not_installed("randomForestSRC")

  veteran  <- survival::veteran
  Surv     <- survival::Surv  # nolint: object_name_linter
  set.seed(42)
  rf <- randomForestSRC::rfsrc(
    Surv(time, status) ~ trt + karno + diagtime + age + prior,
    data   = veteran,
    ntree  = 50,
    nsplit = 5
  )

  gg_dta <- gg_survival(rf, by = "trt")

  expect_s3_class(gg_dta, "gg_survival")
  expect_true("groups" %in% colnames(gg_dta))
  expect_s3_class(plot(gg_dta), "ggplot")
})

test_that("gg_survival.rfsrc errors on non-survival forest", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf_reg <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 30)

  # Regression forests have no $yvar with two survival columns
  expect_error(gg_survival(rf_reg), regexp = "survival forest")
})
