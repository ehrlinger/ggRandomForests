# testthat for gg_rfsrc function
context("gg_rfsrc tests")

test_that("gg_rfsrc classifications", {
  ## Load the cached forest
  data(rfsrc_iris, package = "ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_is(rfsrc_iris, "class")
  
  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), nrow(rfsrc_iris$predicted.oob))
  expect_equal(ncol(gg_dta), ncol(rfsrc_iris$predicted.oob) + 1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta[, -which(colnames(gg_dta) == "y")]),
                    rfsrc_iris$predicted.oob)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  
  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_iris, oob = FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), nrow(rfsrc_iris$predicted))
  expect_equal(ncol(gg_dta), ncol(rfsrc_iris$predicted) + 1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta[, -which(colnames(gg_dta) == "y")]),
                    rfsrc_iris$predicted)
})

test_that("gg_rfsrc regression", {
  ## Load the cached forest
  data(rfsrc_Boston, package = "ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_Boston$family, "regr")
  
  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_Boston)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "regr")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_Boston, oob = FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_Boston, by = "chas")
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "regr")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test data is correctly pulled from randomForest obect.
  # Predicted values
  rfsrc_Boston$family <- "test"
  expect_error(gg_rfsrc(rfsrc_Boston))
  
  # Test exceptions
  # Is it an rfsrc object?
  expect_error(gg_rfsrc(gg_plt))
  
  # Does it contain the forest?
  rfsrc_Boston$forest <- NULL
  expect_error(gg_rfsrc(rfsrc_Boston))
  
})
