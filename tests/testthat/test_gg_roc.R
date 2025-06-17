# testthat for gg_roc function
context("gg_roc tests")

test_that("gg_roc classifications", {
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
  expect_match(rfsrc_iris$family, "class")
  
  ## Create the correct gg_roc object
  which_outcome <- 1
  gg_dta <- gg_roc(rfsrc_iris, which_outcome)
  
  # Test object type
  expect_is(gg_dta, "gg_roc")
  
  # Test classification dimensions
  expect_equal(ncol(gg_dta), 3)
  
  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(rfsrc_iris$predicted.oob[, which_outcome]))
  
  ## Test plotting the gg_roc object
  gg_obj <- plot.gg_roc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_obj, "ggplot")
  
  # Try test set prediction.
  gg_dta <- gg_roc(rfsrc_iris, which_outcome, oob = FALSE)
  # Try test set prediction.
  gg_plt <- plot.gg_roc(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_roc")
  
  # Test classification dimensions
  expect_equal(ncol(gg_dta), 3)
  
  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(rfsrc_iris$predicted[, which_outcome]))
  
  ## Test plotting the gg_roc object
  gg_obj <- plot.gg_roc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_obj, "ggplot")
  
  expect_is(plot.gg_roc(rfsrc_iris), "ggplot")
  expect_error(gg_roc.randomForest(rfsrc_iris))
  expect_error(gg_roc.rfrsrc(rf_iris))
})

test_that("gg_roc randomForest classifications", {
  ## Load the cached forest
  rf_iris <- randomForest(Species ~ ., data = iris)
  
  # Test the cached forest type
  expect_is(rf_iris, "randomForest")
  
  # Test the forest family
  expect_match(rf_iris$type, "classification")
  
  ## Create the correct gg_roc object
  which_outcome <- 1
  gg_dta <- gg_roc(rf_iris, which_outcome)
  
  # Test object type
  expect_is(gg_dta, "gg_roc")
  
  ## Test plotting the gg_roc object
  gg_obj <- plot.gg_roc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_obj, "ggplot")
  
  # Try test set prediction.
  gg_dta <- gg_roc(rf_iris, which_outcome, oob = FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_roc")
  # Test classification dimensions
  expect_equal(ncol(gg_dta), 3)
  
  ## Test plotting the gg_roc object
  gg_obj <- plot.gg_roc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_obj, "ggplot")
  
  expect_is(plot.gg_roc(rf_iris), "ggplot")
  
  expect_error(gg_roc.rfrsrc(rf_iris))
})

test_that("gg_roc regression", {
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
  
  # Test the forest family
  expect_match(rfsrc_boston$family, "regr")
  
  ## Create the correct gg_roc object
  expect_error(gg_roc(rfsrc_boston))
  expect_error(plot.gg_roc(rfsrc_boston))
  
})

test_that("calc_roc", {
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    forest = TRUE,
    importance = TRUE,
    save.memory = TRUE)
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_iris$family, "class")
  
  gg_dta <- calc_roc.rfsrc(rfsrc_iris,
                           rfsrc_iris$yvar,
                           which_outcome = 1,
                           oob = TRUE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  
  # Test oob=FALSE
  gg_dta <- calc_roc.rfsrc(rfsrc_iris,
                           rfsrc_iris$yvar,
                           which_outcome = 1,
                           oob = FALSE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
  # The second outcome.
  gg_dta <- ggRandomForests::calc_roc(rfsrc_iris,
                           rfsrc_iris$yvar,
                           which_outcome = 2,
                           oob = TRUE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
  # and the third...
  gg_dta <- calc_roc(rfsrc_iris,
                           rfsrc_iris$yvar,
                           which_outcome = 3,
                           oob = TRUE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
})
