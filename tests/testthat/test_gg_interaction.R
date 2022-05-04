# testthat for gg_interaction function
context("gg_interaction tests")

test_that("gg_interaction classifications", {
  data(iris, package = "datasets")
  rfsrc_iris <- randomForestSRC::rfsrc(
    Species ~ .,
    data = iris,
    importance = TRUE,
    tree.err = TRUE
  )
  ## Load the cached forest
  interaction_iris <- randomForestSRC::find.interaction(rfsrc_iris)
  
  # Test the cached interaction structure
  expect_is(interaction_iris, "matrix")
  
  ## Create the correct gg_interaction object
  gg_dta <- gg_interaction(interaction_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta), dim(interaction_iris))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta), interaction_iris)
  
  ## Test plotting the gg_interaction object
  gg_plt <- plot.gg_interaction(gg_dta, xvar = "Petal.Width")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # This one should fail with a variable not found message
  expect_error(plot.gg_interaction(gg_dta, xvar = "Petal"))
  
  # "Incorrect object type: Expects a gg_interaction object"
  ## Test plotting the gg_interaction object
  gg_plt <- plot.gg_interaction(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
})

test_that("gg_interaction regression rfsrc", {
  data(Boston, package = "MASS")
  
  Boston$chas <- as.logical(Boston$chas)
  
  rf_boston <- randomForestSRC::rfsrc(medv ~ ., data = Boston)
  interaction_boston <- randomForestSRC::find.interaction(rf_boston)
  # Test the cached interaction structure
  expect_is(interaction_boston, "matrix")
  
  ## Create the correct gg_interaction object
  gg_dta <- gg_interaction(interaction_boston)
  
  # Test object type
  expect_is(gg_dta, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta), dim(interaction_boston))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta), interaction_boston)
  
  ## Test plotting the gg_interaction object
  gg_plt <- plot.gg_interaction(gg_dta, xvar = "rm")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## This one costs a lot of time in calculating the interaction matrix.
  ## Create the correct gg_interaction object
  expect_warning(gg_dta <- gg_interaction(rfsrc_boston))
  
  # Test object type
  expect_is(gg_dta, "gg_interaction")
  
  
  # Test the cached interaction structure
  expect_is(interaction_boston, "matrix")
  
  interaction_boston <- interaction_boston[-2, ]
  expect_error(gg_interaction(interaction_boston))
})
