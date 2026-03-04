# Tests for gg_partial and gg_partial_rfsrc
context("gg_partial tests")

# Helper: create mock partial plot data (matching rfsrc::plot.variable structure)
make_mock_partial_data <- function() {
  set.seed(42)
  # Wind: continuous (15 unique values > cat_limit of 10)
  wind_x <- seq(1.7, 20.7, length.out = 15)
  # Month: categorical (5 unique values < cat_limit of 10)
  month_x <- rep(c(5, 6, 7, 8, 9), 3)

  list(
    plotthis = list(
      Wind = list(
        x = wind_x,
        yhat = rnorm(15, mean = 50, sd = 10)
      ),
      Month = list(
        x = month_x,
        yhat = rnorm(15, mean = 50, sd = 10)
      )
    )
  )
}

# ---- gg_partial unit tests -----------------------------------------------

test_that("gg_partial returns list with continuous and categorical", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta)

  expect_type(result, "list")
  expect_named(result, c("continuous", "categorical"))
  expect_s3_class(result$continuous, "data.frame")
  expect_s3_class(result$categorical, "data.frame")
})

test_that("gg_partial correctly separates continuous and categorical", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta)

  # Wind has 15 unique values > cat_limit=10, so it is continuous
  expect_true("Wind" %in% result$continuous$name)
  expect_false("Wind" %in% result$categorical$name)

  # Month has 5 unique values <= cat_limit=10, so it is categorical
  expect_true("Month" %in% result$categorical$name)
  expect_false("Month" %in% result$continuous$name)
})

test_that("gg_partial continuous output has correct columns", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta)

  expect_true(all(c("x", "yhat", "name") %in% colnames(result$continuous)))
})

test_that("gg_partial categorical output has correct columns", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta)

  expect_true(all(c("x", "yhat", "name") %in% colnames(result$categorical)))
  # Categorical x should be a factor
  expect_s3_class(result$categorical$x, "factor")
})

test_that("gg_partial respects nvars argument", {
  mock_dta <- make_mock_partial_data()

  # nvars=1 processes only Wind (first variable, continuous)
  result <- gg_partial(mock_dta, nvars = 1)

  expect_true("Wind" %in% result$continuous$name)
  expect_equal(nrow(result$categorical), 0)
})

test_that("gg_partial adds model column when model argument is provided", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta, model = "my_model")

  expect_true("model" %in% colnames(result$continuous))
  expect_true("model" %in% colnames(result$categorical))
  expect_equal(unique(result$continuous$model), "my_model")
  expect_equal(unique(result$categorical$model), "my_model")
})

test_that("gg_partial without model argument has no model column", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta)

  expect_false("model" %in% colnames(result$continuous))
  expect_false("model" %in% colnames(result$categorical))
})

test_that("gg_partial with high cat_limit classifies Wind as categorical", {
  mock_dta <- make_mock_partial_data()
  # cat_limit=100 means all variables become categorical
  result <- gg_partial(mock_dta, cat_limit = 100)

  expect_equal(nrow(result$continuous), 0)
  expect_true("Wind" %in% result$categorical$name)
  expect_true("Month" %in% result$categorical$name)
})

test_that("gg_partial with low cat_limit classifies Month as continuous", {
  mock_dta <- make_mock_partial_data()
  # cat_limit=1 means Month (5 unique) is also continuous
  result <- gg_partial(mock_dta, cat_limit = 1)

  expect_true("Wind" %in% result$continuous$name)
  expect_true("Month" %in% result$continuous$name)
  expect_equal(nrow(result$categorical), 0)
})

# ---- gg_partial_rfsrc integration tests -----------------------------------

test_that("gg_partial_rfsrc regression returns correct structure", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  result <- gg_partial_rfsrc(rf, xvar.names = c("Wind"))

  expect_type(result, "list")
  expect_named(result, c("continuous", "categorical"))
  expect_s3_class(result$continuous, "data.frame")
  expect_s3_class(result$categorical, "data.frame")
})

test_that("gg_partial_rfsrc Wind is continuous in airquality model", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  result <- gg_partial_rfsrc(rf, xvar.names = c("Wind"))

  # Wind has many unique values, should land in continuous
  expect_gt(nrow(result$continuous), 0)
  expect_true("Wind" %in% result$continuous$name)
})

test_that("gg_partial_rfsrc error on invalid xvar.names", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  result <- gg_partial_rfsrc(rf, xvar.names = c("NotAColumn"))
  expect_match(result, "xvar.names contains column names not found")
})

test_that("gg_partial_rfsrc error on invalid newx columns", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  bad_newx <- data.frame(foo = 1:10, bar = 1:10)
  result <- gg_partial_rfsrc(rf, xvar.names = c("Wind"), newx = bad_newx)
  expect_match(result, "newx must be a dataframe")
})

test_that("gg_partial_rfsrc uses supplied newx data", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  newx <- rf$xvar[1:20, ]
  result <- gg_partial_rfsrc(rf, xvar.names = c("Wind"), newx = newx)

  expect_type(result, "list")
  expect_named(result, c("continuous", "categorical"))
})

test_that("gg_partial_rfsrc works with multiple xvar.names", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  result <- gg_partial_rfsrc(rf, xvar.names = c("Wind", "Temp"))

  expect_gt(nrow(result$continuous), 0)
})

test_that("gg_partial_rfsrc with xvar2.name returns grouped result", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  result <- gg_partial_rfsrc(rf,
    xvar.names = c("Wind"),
    xvar2.name = "Month"
  )

  expect_type(result, "list")
  expect_named(result, c("continuous", "categorical"))
  # grouped output should have a grp column
  if (nrow(result$continuous) > 0) {
    expect_true("grp" %in% colnames(result$continuous))
  }
})
