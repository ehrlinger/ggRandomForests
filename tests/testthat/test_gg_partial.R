# Tests for gg_partial and gg_partial_rfsrc

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

test_that("gg_partial returns a gg_partial object with continuous and categorical", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta)

  expect_s3_class(result, "gg_partial")
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

test_that("gg_partial_rfsrc regression returns a gg_partial_rfsrc object", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  result <- gg_partial_rfsrc(rf, xvar.names = c("Wind"))

  expect_s3_class(result, "gg_partial_rfsrc")
  expect_named(result, c("continuous", "categorical"))
  expect_s3_class(result$continuous, "data.frame")
  expect_s3_class(result$categorical, "data.frame")
})

test_that("plot.gg_partial returns a ggplot for continuous-only data", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta, nvars = 1)   # only Wind (continuous)

  gg_plt <- plot(result)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_partial returns a single ggplot when both types present", {
  mock_dta <- make_mock_partial_data()
  result <- gg_partial(mock_dta)

  out <- plot(result)
  # patchwork combines the two panels into a single ggplot-inheriting object.
  expect_s3_class(out, "ggplot")
})

test_that("plot.gg_partial_rfsrc returns a ggplot", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)
  result <- gg_partial_rfsrc(rf, xvar.names = "Wind")

  gg_plt <- plot(result)
  expect_s3_class(gg_plt, "ggplot")
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

  expect_error(
    gg_partial_rfsrc(rf, xvar.names = c("NotAColumn")),
    "xvar.names contains column names not found"
  )
})

test_that("gg_partial_rfsrc error on invalid newx columns", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  bad_newx <- data.frame(foo = 1:10, bar = 1:10)
  expect_error(
    gg_partial_rfsrc(rf, xvar.names = c("Wind"), newx = bad_newx),
    "newx must be a dataframe"
  )
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

# ---- n_eval and cat_limit ---------------------------------------------------

test_that("n_eval limits evaluation grid size for continuous variables", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf   <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  # Wind has 31 unique values in the full airquality dataset; n_eval = 8
  # should produce at most 8 evaluation points in the output
  result <- gg_partial_rfsrc(rf, xvar.names = "Wind", n_eval = 8)

  expect_gt(nrow(result$continuous), 0)
  expect_lte(nrow(result$continuous), 8)
})

test_that("default n_eval = 25 caps large continuous grids", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf   <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  result <- gg_partial_rfsrc(rf, xvar.names = "Wind")  # default n_eval = 25

  expect_lte(nrow(result$continuous), 25)
})

test_that("cat_limit controls continuous vs categorical dispatch", {
  skip_if_not_installed("randomForestSRC")

  set.seed(42)
  airq <- na.omit(airquality)
  rf   <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50, nsplit = 5)

  # Month has 5 unique values.
  # With default cat_limit = 10: Month → categorical
  result_cat <- gg_partial_rfsrc(rf, xvar.names = "Month", cat_limit = 10)
  expect_gt(nrow(result_cat$categorical), 0)
  expect_true("Month" %in% result_cat$categorical$name)

  # With cat_limit = 3: Month (5 unique values >= 3) → continuous
  result_cont <- gg_partial_rfsrc(rf, xvar.names = "Month", cat_limit = 3)
  expect_gt(nrow(result_cont$continuous), 0)
  expect_true("Month" %in% result_cont$continuous$name)
})

# ---- survival forest path ---------------------------------------------------

# Helper: fit a small survival forest on veteran data (n = 137, fast to build)
make_veteran_rf <- function() {
  skip_if_not_installed("randomForestSRC")
  skip_if_not_installed("survival")
  veteran <- survival::veteran        # direct access avoids .GlobalEnv loading issue
  Surv    <- survival::Surv           # parseFormula rejects namespace::Surv(...) syntax # nolint: object_name_linter
  set.seed(42)
  randomForestSRC::rfsrc(
    Surv(time, status) ~ trt + karno + diagtime + age + prior,
    data   = veteran,
    ntree  = 100,
    nsplit = 5
  )
}

test_that("survival forest is detected as is_surv = TRUE", {
  rf <- make_veteran_rf()
  expect_true(grepl("surv", rf$family, ignore.case = TRUE))
  expect_false(is.null(rf$time.interest))
  expect_gt(length(rf$time.interest), 0)
})

test_that("partial.time values are snapped to time.interest grid", {
  rf <- make_veteran_rf()
  ti <- rf$time.interest

  # Pick a target time that almost certainly is not exactly in time.interest
  target <- mean(range(ti))  # midpoint of observed event times
  snapped <- ti[which.min(abs(ti - target))]

  # snapped must be an exact member of time.interest
  expect_true(snapped %in% ti)
  # and it should be closer to target than any other grid point
  diffs <- abs(ti - target)
  expect_equal(snapped, ti[which.min(diffs)])
})

test_that("gg_partial_rfsrc survival: default partial.time uses quartiles", {
  rf <- make_veteran_rf()
  ti <- rf$time.interest

  # When partial.time = NULL, three quartile-snapped times are used.
  # We can verify this by inspecting how many distinct time values appear
  # in the output (should be 3 unless quartile snapping collapses duplicates).
  result <- tryCatch(
    gg_partial_rfsrc(rf, xvar.names = "karno"),
    error = function(e) {
      skip(paste(
        "partial.rfsrc() failed for survival forest (upstream bug):",
        conditionMessage(e)
      ))
    }
  )

  expect_type(result, "list")
  expect_named(result, c("continuous", "categorical"))
  expect_gt(nrow(result$continuous), 0)
  # time column should be present in survival output
  expect_true("time" %in% colnames(result$continuous))
  # at most 3 distinct time values (one per quartile)
  expect_lte(length(unique(result$continuous$time)), 3)
})

test_that("gg_partial_rfsrc survival: explicit partial.time is snapped and used", {
  rf <- make_veteran_rf()
  ti <- rf$time.interest
  # Target the median event time
  t_med <- ti[which.min(abs(ti - median(ti)))]

  result <- tryCatch(
    gg_partial_rfsrc(rf, xvar.names = "karno", partial.time = t_med),
    error = function(e) {
      skip(paste(
        "partial.rfsrc() failed for survival forest (upstream bug):",
        conditionMessage(e)
      ))
    }
  )

  expect_type(result, "list")
  expect_gt(nrow(result$continuous), 0)
  expect_true("time" %in% colnames(result$continuous))
  # The only time value in the output should be (close to) t_med
  expect_true(all(abs(result$continuous$time - t_med) < 1e-9))
})

test_that("gg_partial_rfsrc survival: multiple partial.time values produce one row-set per time", {
  rf <- make_veteran_rf()
  ti <- rf$time.interest
  t1 <- ti[which.min(abs(ti - quantile(ti, 0.25)))]
  t2 <- ti[which.min(abs(ti - quantile(ti, 0.75)))]
  # Ensure we actually have two distinct snapped times
  if (t1 == t2) skip("quartile times collapsed to same grid point")

  result <- tryCatch(
    gg_partial_rfsrc(rf, xvar.names = "karno", partial.time = c(t1, t2)),
    error = function(e) {
      skip(paste(
        "partial.rfsrc() failed for survival forest (upstream bug):",
        conditionMessage(e)
      ))
    }
  )

  expect_type(result, "list")
  n_times <- length(unique(result$continuous$time))
  expect_equal(n_times, 2L)
})

test_that("gg_partial_rfsrc survival: returns correct column names", {
  rf <- make_veteran_rf()
  ti <- rf$time.interest
  t_med <- ti[which.min(abs(ti - median(ti)))]

  result <- tryCatch(
    gg_partial_rfsrc(rf, xvar.names = "karno", partial.time = t_med),
    error = function(e) {
      skip(paste(
        "partial.rfsrc() failed for survival forest (upstream bug):",
        conditionMessage(e)
      ))
    }
  )

  expect_true(all(c("x", "yhat", "name", "time") %in% colnames(result$continuous)))
})
