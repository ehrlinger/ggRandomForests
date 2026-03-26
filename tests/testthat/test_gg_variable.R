# testthat for gg_variable function

# Survival formula helper (rfsrc requires Surv to be in local scope)
Surv <- survival::Surv # nolint: object_name_linter

test_that("gg_variable classifications", {
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
  expect_equal(rfsrc_iris$family, "class")

  ## Create the correct gg_error object
  gg_dta <- gg_variable(rfsrc_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_variable")

  ## Ensure non-OOB predictions keep predictor columns
  gg_full <- gg_variable(rfsrc_iris, oob = FALSE)
  expect_true(all(rfsrc_iris$xvar.names %in% names(gg_full)))

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, xvar = "Petal.Width")

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names)

  # Test return is s ggplot object
  expect_type(gg_plt, "list")
  expect_equal(length(gg_plt), length(rfsrc_iris$xvar.names))
  for (ind in seq_along(rfsrc_iris$xvar.names))
    expect_s3_class(gg_plt[[ind]], "ggplot")
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names,
                             panel = TRUE)

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")

  rf_iris <- randomForest::randomForest(Species ~ .,
                                        data = iris)

  ## Create the correct gg_error object
  gg_dta <- gg_variable(rf_iris)

  # Test object type
  expect_s3_class(gg_dta, "gg_variable")

  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, xvar = "Petal.Width")

  # Test return is s ggplot object
  expect_s3_class(gg_plt, "ggplot")
  gg_plt <- plot(gg_dta)

  ## Ensure we can rebuild training data for subset() calls
  iris_two <- droplevels(subset(iris, Species != "setosa"))
  rf_subset <- randomForest::randomForest(Species ~ .,
                                          data = iris_two,
                                          ntree = 60)
  gg_subset <- gg_variable(rf_subset)
  expect_s3_class(gg_subset, "gg_variable")
  expect_true(all(c("Sepal.Length", "Sepal.Width") %in% names(gg_subset)))

})


test_that("gg_variable regression", {
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

  ## Create the correct gg_error object
  gg_dta <- gg_variable(rfsrc_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_variable")

  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta)

  # Test return is s ggplot object
  expect_type(gg_plt, "list")
  expect_equal(length(gg_plt), length(rfsrc_boston$xvar.names))
  for (ind in seq_along(rfsrc_boston$xvar.names))
    expect_s3_class(gg_plt[[ind]], "ggplot")


  ## Test plotting the gg_error object
  expect_warning(gg_plt <- plot.gg_variable(gg_dta, panel = TRUE))
  expect_s3_class(gg_plt, "ggplot")


  data(Boston, package = "MASS")
  rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
  gg_dta <- gg_variable(rf_boston)

  # Test object type
  expect_s3_class(gg_dta, "gg_variable")

  expect_warning(gg_plt <- plot(gg_dta, panel = TRUE))
  expect_s3_class(gg_plt, "ggplot")

})

test_that("gg_variable survival handles late time requests", {
  data(veteran, package = "randomForestSRC")
  Surv <- survival::Surv # nolint: object_name_linter
  rfsrc_veteran <- randomForestSRC::rfsrc(Surv(time, status) ~ .,
                                          data = veteran,
                                          ntree = 50,
                                          nsplit = 5)
  late_time <- max(rfsrc_veteran$time.interest) + 50
  expect_silent(gg_dta <- gg_variable(rfsrc_veteran, time = late_time))
  expect_s3_class(gg_dta, "gg_variable")
})

test_that("gg_variable survival: single time, single continuous variable plot", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = 90)
  expect_s3_class(gg_dta, "gg_variable")

  gg_plt <- plot(gg_dta, xvar = "age")
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_variable survival: single time, panel plot", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = 90)
  gg_plt <- plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_variable survival: multiple times, single variable facets", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = c(30, 90, 365))
  expect_s3_class(gg_dta, "gg_variable")

  # Single xvar → plot.gg_variable returns a single ggplot (not a list)
  gg_plt <- plot(gg_dta, xvar = "age")
  expect_s3_class(gg_plt, "ggplot")

  # Multiple xvars → returns a list
  gg_plt2 <- plot(gg_dta, xvar = c("age", "diagtime"))
  expect_type(gg_plt2, "list")
  expect_s3_class(gg_plt2[[1]], "ggplot")
})

test_that("gg_variable survival: multiple times, panel plot", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = c(30, 90))
  gg_plt <- plot(gg_dta, xvar = c("age", "diagtime"), panel = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("gg_variable survival: points=FALSE smooth=TRUE options", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = 90)

  gg_plt <- plot(gg_dta, xvar = "age", points = FALSE, smooth = TRUE)
  expect_s3_class(gg_plt, "ggplot")

  gg_plt2 <- plot(gg_dta, xvar = "age", points = TRUE, smooth = FALSE)
  expect_s3_class(gg_plt2, "ggplot")
})

test_that("gg_variable survival: oob=FALSE uses in-bag predictions", {
  data(veteran, package = "randomForestSRC")
  set.seed(42)
  rfsrc_veteran <- randomForestSRC::rfsrc(
    Surv(time, status) ~ .,
    data = veteran,
    ntree = 50,
    nsplit = 5
  )

  gg_dta <- gg_variable(rfsrc_veteran, time = 90, oob = FALSE)
  expect_s3_class(gg_dta, "gg_variable")
})

test_that("plot.gg_variable regression: points=FALSE smooth=TRUE", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)

  gg_plt <- plot(gg_dta, xvar = "lstat", points = FALSE, smooth = TRUE)
  expect_s3_class(gg_plt, "ggplot")

  gg_plt2 <- plot(gg_dta, xvar = "lstat", points = FALSE, smooth = FALSE)
  expect_s3_class(gg_plt2, "ggplot")
})

test_that("plot.gg_variable regression: factor x variable triggers boxplot", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)
  gg_dta$chas <- factor(gg_dta$chas)

  gg_plt <- plot(gg_dta, xvar = "chas")
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable regression: panel with two continuous variables", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)
  gg_plt <- plot(gg_dta, xvar = c("lstat", "rm"), panel = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable regression: panel points=FALSE smooth=TRUE", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)
  gg_plt <- plot(gg_dta, xvar = c("lstat", "rm"),
                 panel = TRUE, points = FALSE, smooth = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable classification: panel with multiple continuous vars", {
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_iris)
  gg_plt <- plot(gg_dta, xvar = c("Petal.Width", "Sepal.Width"), panel = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable classification: smooth and no-points path", {
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_iris)
  gg_plt <- plot(gg_dta, xvar = "Petal.Width", points = FALSE, smooth = TRUE)
  expect_s3_class(gg_plt, "ggplot")
})

test_that("plot.gg_variable: missing xvar returns list for all predictors", {
  data(Boston, package = "MASS")
  boston <- Boston
  boston$chas <- as.logical(boston$chas)

  rfsrc_boston <- randomForestSRC::rfsrc(
    medv ~ .,
    data = boston,
    forest = TRUE,
    save.memory = TRUE
  )

  gg_dta <- gg_variable(rfsrc_boston)
  gg_plt <- plot(gg_dta)
  expect_type(gg_plt, "list")
  expect_gt(length(gg_plt), 0)
  expect_s3_class(gg_plt[[1]], "ggplot")
})
