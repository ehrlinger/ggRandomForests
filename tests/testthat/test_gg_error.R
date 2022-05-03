# testthat for gg_error function
context("gg_error tests")

test_that("gg_error.rfsrc classifications", {
  ## Load the cached forest
  data(iris, package = "datasets")
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    importance = TRUE,
    tree.err = TRUE
  )
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_iris$family, "class")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta)[1], dim(na.omit(rfsrc_iris$err.rate))[1])
  expect_equal(dim(gg_dta)[2], dim(rfsrc_iris$err.rate)[2] + 1)
  
  # Test data is correctly pulled from randomForest obect.
  # expect_equivalent(as.matrix(gg_dta[, -which(colnames(gg_dta) == "ntree")]),
  #                   rfsrc_iris$err.rate)
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # "Incorrect object type: Expects a gg_error object"
  expect_error(gg_error(gg_plt))
  expect_error(gg_error.rfsrc(gg_plt))
  rfsrc_iris$err.rate <- NULL
  expect_error(gg_error(rfsrc_iris))
  
  gg_dta <- gg_error(rfsrc_iris, training = TRUE)
  expect_is(gg_dta, "gg_error")
})


test_that("gg_error.randomForest classifications", {
  ## Load the cached forest
  data(iris, package = "datasets")
  ## Load the cached forest
  rf_iris <- randomForest::randomForest(Species ~ .,
                                        data = iris)
  
  # Test the cached forest type
  expect_is(rf_iris, "randomForest")
  
  # Test the forest family
  expect_match(rf_iris$type, "classification")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rf_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta)[1], dim(rf_iris$err.rate)[1])
  expect_equal(dim(gg_dta)[2], dim(rf_iris$err.rate)[2] + 1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta[, -which(colnames(gg_dta) == "ntree")]),
                    rf_iris$err.rate)
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # "Incorrect object type: Expects a gg_error object"
  expect_error(gg_error(gg_plt))
  expect_error(gg_error.randomForest(gg_plt))
  rf_iris$err.rate <- NULL
  expect_error(gg_error(rf_iris))
  
  
  gg_dta <- gg_error(rf_iris, training = TRUE)
  expect_is(gg_dta, "gg_error")
  
})



test_that("gg_error regression rfsrc", {
  ## Load the cached forest
  data(Boston, package = "MASS")
  
  Boston$chas <- as.logical(Boston$chas)
  
  rfsrc_boston <- randomForestSRC::rfsrc(medv ~ ., data = Boston)
  # Test the cached forest type
  expect_is(rfsrc_boston, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_boston$family, "regr")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), length(na.omit(rfsrc_boston$err.rate)))
  expect_equal(ncol(gg_dta), 2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(c(gg_dta[, 1]), na.omit(rfsrc_boston$err.rate))
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Test the exception for input
  expect_error(gg_error(gg_plt))

})


test_that("gg_error regression randomForest", {
  ## Load the cached forest
  data(Boston, package = "MASS")
  
  Boston$chas <- as.logical(Boston$chas)
  
  rf_boston <- randomForest::randomForest(medv ~ ., data = Boston)
  # Test the cached forest type
  expect_is(rf_boston, "randomForest")
  
  # Test the forest family
  expect_match(rf_boston$type, "regression")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rf_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), length(na.omit(rf_boston$mse)))
  expect_equal(ncol(gg_dta), 2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(c(gg_dta[, 1]), rf_boston$mse)
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Test the exception for input
  expect_error(gg_error(gg_plt))
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rf_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
})
