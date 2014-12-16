# testthat for gg_interaction function
context("gg_interaction tests")

test_that("gg_interaction classifications",{
  
  ## Load the cached forest
  data(iris_interaction, package="ggRandomForests")
  
  # Test the cached interaction structure
  expect_is(iris_interaction, "matrix")
  
  ## Create the correct gg_interaction object
  ggrf.obj <- gg_interaction(iris_interaction)
  
  # Test object type
  expect_is(ggrf.obj, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj), dim(iris_interaction))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(ggrf.obj), iris_interaction)
  
  ## Test plotting the gg_interaction object
  gg.obj <- plot.gg_interaction(ggrf.obj, xvar="Petal.Width")
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  # This one should fail with a variable not found message
  expect_that(plot.gg_interaction(ggrf.obj, xvar="Petal"),
              #throws_error('Error in plot.gg_interaction(ggrf.obj, xvar = "Petal") : \n Invalid xvar (Petal) specified, covariate not found.\n'))
              throws_error())
  # "Incorrect object type: Expects a gg_interaction object"
})


test_that("gg_interaction survival",{
  
  #   data(pbc, package = "randomForestSRC")
  #   pbc_rf <- rfsrc(Surv(days, status) ~ ., pbc,
  #                 nsplit = 10, na.action = "na.impute")
  #   pbc_interaction <- find.interaction(pbc.rf)
  #
  ## Load the cached forest
  data(pbc_interaction, package="ggRandomForests")
  
  # Test the cached interaction structure
  expect_is(pbc_interaction, "matrix")
  
  ## Create the correct gg_interaction object
  ggrf.obj <- gg_interaction(pbc_interaction)
  
  # Test object type
  expect_is(ggrf.obj, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj), dim(pbc_interaction))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(ggrf.obj), pbc_interaction)
  
  ## Test plotting the gg_interaction object
  gg.obj <- plot.gg_interaction(ggrf.obj, xvar="bili")
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  
  # "Incorrect object type: Expects a gg_interaction object"
})

test_that("gg_interaction regression",{
  
  ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   ggrf.obj<- gg_interaction(airq.obj)
  ## Load the cached forest
  data(airq_interaction, package="ggRandomForests")
  
  # Test the cached interaction structure
  expect_is(airq_interaction, "matrix")
  
  ## Create the correct gg_interaction object
  ggrf.obj <- gg_interaction(airq_interaction)
  
  # Test object type
  expect_is(ggrf.obj, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj), dim(airq_interaction))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(ggrf.obj), airq_interaction)
  
  ## Test plotting the gg_interaction object
  gg.obj <- plot.gg_interaction(ggrf.obj, xvar = "Temp")
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
})