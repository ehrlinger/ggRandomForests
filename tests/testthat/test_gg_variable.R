# testthat for gg_variable function
context("gg_variable tests")

test_that("gg_variable classifications", {
  ## Load the cached forest
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    importance = TRUE,
    save.memory = TRUE)
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_equal(rfsrc_iris$family, "class")
  
  ## Create the correct gg_error object
  gg_dta <- gg_variable(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_variable")

  ## Ensure non-OOB predictions keep predictor columns
  gg_full <- gg_variable(rfsrc_iris, oob = FALSE)
  expect_true(all(rfsrc_iris$xvar.names %in% names(gg_full)))
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, xvar = "Petal.Width")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "list")
  expect_equal(length(gg_plt), length(rfsrc_iris$xvar.names))
  for (ind in seq_len(length(rfsrc_iris$xvar.names)))
    expect_is(gg_plt[[ind]], "ggplot")
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_iris$xvar.names,
                             panel = TRUE)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  rf_iris <- randomForest::randomForest(Species ~ .,
                                        data = iris)
  
  ## Create the correct gg_error object
  gg_dta <- gg_variable(rf_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_variable")
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, xvar = "Petal.Width")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  gg_plt <- plot(gg_dta)

  ## Ensure we can rebuild training data for subset() calls
  iris_two <- droplevels(subset(iris, Species != "setosa"))
  rf_subset <- randomForest::randomForest(Species ~ .,
                                          data = iris_two,
                                          ntree = 60)
  gg_subset <- gg_variable(rf_subset)
  expect_is(gg_subset, "gg_variable")
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
  expect_is(rfsrc_boston, "rfsrc")
  
  ## Create the correct gg_error object
  gg_dta <- gg_variable(rfsrc_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_variable")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_variable(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "list")
  expect_equal(length(gg_plt), length(rfsrc_boston$xvar.names))
  for (ind in seq_len(length(rfsrc_boston$xvar.names)))
    expect_is(gg_plt[[ind]], "ggplot")
  
  
  ## Test plotting the gg_error object
  expect_warning(gg_plt <- plot.gg_variable(gg_dta, panel = TRUE))
  expect_is(gg_plt, "ggplot")
  
  
  data(Boston, package = "MASS")
  rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
  gg_dta <- gg_variable(rf_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_variable")
  
  expect_warning(gg_plt <- plot(gg_dta, panel = TRUE))
  expect_is(gg_plt, "ggplot")
  
})

test_that("gg_variable survival handles late time requests", {
  data(veteran, package = "randomForestSRC")
  Surv <- survival::Surv
  rfsrc_veteran <- randomForestSRC::rfsrc(Surv(time, status) ~ .,
                                          data = veteran,
                                          ntree = 50,
                                          nsplit = 5)
  late_time <- max(rfsrc_veteran$time.interest) + 50
  expect_silent(gg_dta <- gg_variable(rfsrc_veteran, time = late_time))
  expect_is(gg_dta, "gg_variable")
})
