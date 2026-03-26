# testthat for gg_rfsrc function

# Bring survival::Surv into the file-scope environment so that rfsrc() survival
# formulas (e.g. Surv(time, status) ~ .) can resolve the symbol without
# requiring library(survival) to be called explicitly in every test block.
Surv <- survival::Surv  # nolint: object_name_linter

test_that("gg_rfsrc classifications", {
  ## Load the cached forest
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    importance = TRUE,
    save.memory = TRUE)

  # Test the cached forest type
  expect_s3_class(rfsrc_iris, "rfsrc")

  # Test the forest family
  expect_s3_class(rfsrc_iris, "class")

  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_rfsrc")

  # Test classification dimensions
  expect_equal(nrow(gg_dta), nrow(rfsrc_iris$predicted.oob))
  expect_equal(ncol(gg_dta), ncol(rfsrc_iris$predicted.oob) + 1)

  # Test data is correctly pulled from randomForest obect.
  expect_equal(as.matrix(gg_dta[, -which(colnames(gg_dta) == "y")], ignore_attr = TRUE),
                    rfsrc_iris$predicted.oob)

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")


  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_iris, oob = FALSE)

  # Test object type
  expect_s3_class(gg_dta, "gg_rfsrc")

  # Test classification dimensions
  expect_equal(nrow(gg_dta), nrow(rfsrc_iris$predicted))
  expect_equal(ncol(gg_dta), ncol(rfsrc_iris$predicted) + 1)

  # Test data is correctly pulled from randomForest obect.
  expect_equal(as.matrix(gg_dta[, -which(colnames(gg_dta) == "y")], ignore_attr = TRUE),
                    rfsrc_iris$predicted)

  rf_iris <- randomForest::randomForest(Species ~ ., data = iris)
  gg_dta <- gg_rfsrc(rf_iris)

})

test_that("gg_rfsrc regression", {
  data(Boston, package = "MASS")
  boston <- Boston

  boston$chas <- as.logical(boston$chas)

  ## Load the cached forest
  rfsrc_boston <-
    randomForestSRC::rfsrc(
      medv ~ .,
      data = boston,
      forest = TRUE,
      importance = TRUE,
      tree.err = TRUE,
      save.memory = TRUE)

  # Test the cached forest type
  expect_s3_class(rfsrc_boston, "rfsrc")

  # Test the forest family
  expect_match(rfsrc_boston$family, "regr")

  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_s3_class(gg_dta, "regr")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_boston, oob = FALSE)

  # Test object type
  expect_s3_class(gg_dta, "gg_rfsrc")

  # Test classification dimensions
  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_boston, by = "chas")

  # Test object type
  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_s3_class(gg_dta, "regr")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)

  # Test data is correctly pulled from randomForest obect.
  # Predicted values
  rfsrc_boston$family <- "test"
  expect_error(gg_rfsrc(rfsrc_boston))

  # Test exceptions
  # Is it an rfsrc object?
  expect_error(gg_rfsrc(gg_plt))

  # Does it contain the forest?
  rfsrc_boston$forest <- NULL
  expect_error(gg_rfsrc(rfsrc_boston))

  data(Boston, package = "MASS")
  rf_boston <- randomForest(medv ~ ., data = Boston)
  plot(gg_rfsrc(rf_boston))

})

test_that("gg_rfsrc survival: per-observation curves (no conf.int, no by)", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  expect_s3_class(rfsrc_veteran, "rfsrc")
  expect_match(rfsrc_veteran$family, "surv")

  gg_dta <- gg_rfsrc(rfsrc_veteran)

  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_s3_class(gg_dta, "surv")
  # Per-observation long format: should have variable, value, obs_id, event
  expect_true("variable" %in% colnames(gg_dta))
  expect_true("value" %in% colnames(gg_dta))
  expect_true("obs_id" %in% colnames(gg_dta))
  expect_true("event" %in% colnames(gg_dta))
})

test_that("gg_rfsrc survival: plot of per-observation curves", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran)
  gg_plt <- plot.gg_rfsrc(gg_dta)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_rfsrc survival: confidence interval calculation", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int = 0.95)

  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_s3_class(gg_dta, "surv")
  # conf.int output has lower, upper, median columns
  expect_true("lower" %in% colnames(gg_dta))
  expect_true("upper" %in% colnames(gg_dta))
  expect_true("median" %in% colnames(gg_dta))

  # Plot the conf.int result
  gg_plt <- plot.gg_rfsrc(gg_dta)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_rfsrc survival: confidence interval with percentage > 1", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  # conf.int > 1 should be divided by 100
  gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int = 95)

  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_true("lower" %in% colnames(gg_dta))
})

test_that("gg_rfsrc survival: by argument groups observations", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran, by = "trt")

  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_s3_class(gg_dta, "surv")
  expect_true("group" %in% colnames(gg_dta))

  # Plot the grouped result
  gg_plt <- plot.gg_rfsrc(gg_dta)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_rfsrc survival: by + conf.int produces grouped confidence bands", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran, by = "trt", conf.int = 0.95)

  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_true("group" %in% colnames(gg_dta))
  expect_true("lower" %in% colnames(gg_dta))

  # Plot
  gg_plt <- plot.gg_rfsrc(gg_dta)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_rfsrc survival: surv_type = 'chf' (cumulative hazard)", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran, surv_type = "chf")
  expect_s3_class(gg_dta, "gg_rfsrc")
})

test_that("gg_rfsrc survival: surv_type = 'mortality'", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran, surv_type = "mortality")
  expect_s3_class(gg_dta, "gg_rfsrc")
})

test_that("gg_rfsrc survival: unknown surv_type throws error", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  expect_error(gg_rfsrc(rfsrc_veteran, surv_type = "unknown_type"))
})

test_that("gg_rfsrc survival: by vector (not column name) works", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  by_vec <- veteran$trt
  gg_dta <- gg_rfsrc(rfsrc_veteran, by = by_vec)

  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_true("group" %in% colnames(gg_dta))
})

test_that("gg_rfsrc survival: by vector wrong length throws error", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  expect_error(gg_rfsrc(rfsrc_veteran, by = c(1, 2, 3)))
})

test_that("gg_rfsrc survival: oob = FALSE uses in-bag predictions", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran, oob = FALSE)
  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_s3_class(gg_dta, "surv")
})

test_that("gg_rfsrc survival: conf.int with custom bs.sample", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int = 0.95, bs.sample = 20)
  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_true("lower" %in% colnames(gg_dta))
})

test_that("gg_rfsrc survival: by + conf.int with custom bs.sample", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_rfsrc(rfsrc_veteran, by = "trt", conf.int = 0.95, bs.sample = 20)
  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_true("group" %in% colnames(gg_dta))
  expect_true("lower" %in% colnames(gg_dta))
})

test_that("gg_rfsrc survival: conf.int with two-element level_set", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5,
    forest = TRUE,
    save.memory = TRUE
  )

  # Two-element conf.int (lower and upper directly)
  gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int = c(0.025, 0.975))
  expect_s3_class(gg_dta, "gg_rfsrc")
})

test_that("gg_rfsrc classification: by argument adds group column", {
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    save.memory = TRUE
  )

  # Use Petal.Width as a logical grouping vector (not character to avoid column-name lookup)
  by_vec <- iris$Petal.Width > median(iris$Petal.Width)
  gg_dta <- gg_rfsrc(rfsrc_iris, by = by_vec)

  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_true("group" %in% colnames(gg_dta))
})

test_that("gg_rfsrc regression: by = vector works", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  by_vec <- boston$chas
  gg_dta <- gg_rfsrc(rfsrc_boston, by = by_vec)

  expect_s3_class(gg_dta, "gg_rfsrc")
  expect_true("group" %in% colnames(gg_dta))
})

test_that("plot.gg_rfsrc notch=FALSE suppresses notch in boxplot", {
  # The notch argument used to be hardcoded to TRUE; callers had no way to
  # suppress it.  Passing notch = FALSE must not error and must return a ggplot.
  set.seed(42)
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ ., data = iris, ntree = 50, save.memory = TRUE
  )
  gg_dta <- gg_rfsrc(rfsrc_iris)
  gg_plt <- plot.gg_rfsrc(gg_dta, notch = FALSE)
  expect_s3_class(gg_plt, "ggplot")

  data(Boston, package = "MASS")
  Boston$chas <- as.logical(Boston$chas) # nolint: object_name_linter
  set.seed(42)
  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ ., data = Boston, ntree = 50, save.memory = TRUE
  )
  gg_dta_regr <- gg_rfsrc(rfsrc_boston)
  gg_plt_regr <- plot.gg_rfsrc(gg_dta_regr, notch = FALSE)
  expect_s3_class(gg_plt_regr, "ggplot")
})
