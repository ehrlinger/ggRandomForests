# testthat for gg_variable function
context("gg_variable tests")

test_that("gg_variable classifications", {
  ## Load the cached forest
  data(rfsrc_iris, package = "ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_equal(rfsrc_iris$family, "class")
  
  ## Create the correct gg_error object
  gg_dta <- gg_variable(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_variable")
  
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
  
})


test_that("gg_variable regression", {
  ## Load the cached forest
  data(rfsrc_boston, package = "ggRandomForests")
  
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
