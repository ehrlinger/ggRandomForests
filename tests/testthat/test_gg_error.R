# testthat for gg_error function
context("gg_error tests")

test_that("gg_error classifications",{
  
  ## Load the cached forest
  data(iris_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(iris_rf, "rfsrc")
  
  # Test the forest family
  expect_match(iris_rf$family, "class")
  
  ## Create the correct gg_error object
  ggrf.obj <- gg_error(iris_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_error")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj)[1], dim(iris_rf$err.rate)[1])
  expect_equal(dim(ggrf.obj)[2], dim(iris_rf$err.rate)[2]+1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(select(ggrf.obj, -ntree)), iris_rf$err.rate)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_error(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  # "Incorrect object type: Expects a gg_error object"
})


test_that("gg_error survival",{
  
  #   data(veteran, package = "randomForestSRC")
  #   v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  
  ## Load the cached forest
  data(veteran_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(veteran_rf, "rfsrc")
  
  # Test the forest family
  expect_match(veteran_rf$family, "surv")
  
  ## Create the correct gg_error object
  ggrf.obj <- gg_error(veteran_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_error")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj)[1], length(veteran_rf$err.rate))
  expect_equal(dim(ggrf.obj)[2], 2)
  
  # Test data is correctly pulled from randomForest obect.
  tmp <- c(ggrf.obj[,1])[[1]]
  expect_equivalent(tmp, veteran_rf$err.rate)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_error(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  # "Incorrect object type: Expects a gg_error object"
})

test_that("gg_error regression",{
  
  ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   ggrf.obj<- gg_error(airq.obj)
  
  ## Load the cached forest
  data(airq_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(airq_rf, "rfsrc")
  
  # Test the forest family
  expect_match(airq_rf$family, "regr")
  
  ## Create the correct gg_error object
  ggrf.obj <- gg_error(airq_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_error")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj)[1], length(airq_rf$err.rate))
  expect_equal(dim(ggrf.obj)[2], 2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(c(ggrf.obj[,1])[[1]], airq_rf$err.rate)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_error(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
})