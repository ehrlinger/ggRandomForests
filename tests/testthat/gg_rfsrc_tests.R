# testthat for gg_rfsrc function
context("gg_rfsrc tests")

test_that("gg_rfsrc classifications",{
  ## IF we want to build the forest every time...
  #   iris_rf <- rfsrc(Species ~ ., data = iris)
  
  ## Load the cached forest
  data(iris_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(iris_rf, "rfsrc")
  
  # Test the forest family
  expect_match(iris_rf$family, "class")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_rfsrc(iris_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj)[1], dim(iris_rf$predicted.oob)[1])
  expect_equal(dim(ggrf.obj)[2], dim(iris_rf$predicted.oob)[2]+1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(select(ggrf.obj, -y)), iris_rf$predicted.oob)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_rfsrc(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_rfsrc(iris_rf, oob=FALSE)
  
  # Test object type
  expect_is(ggrf.obj, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj)[1], dim(iris_rf$predicted)[1])
  expect_equal(dim(ggrf.obj)[2], dim(iris_rf$predicted)[2]+1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(select(ggrf.obj, -y)), iris_rf$predicted) 
})


test_that("gg_rfsrc survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  
  ## Load the cached forest
  data(veteran_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(veteran_rf, "rfsrc")
  
  # Test the forest family
  expect_match(veteran_rf$family, "surv")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_rfsrc(veteran_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_rfsrc")
  
  # Test classification dimensions
  # Same number of rows:
  expect_equal(dim(ggrf.obj)[1], dim(veteran_rf$survival.oob)[1])
  # Survival has time and cens columns
  expect_equal(dim(ggrf.obj)[2], dim(veteran_rf$survival.oob)[2] +2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(select(ggrf.obj, -ptid, -cens)),
                    veteran_rf$survival.oob)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_rfsrc(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_rfsrc(veteran_rf, oob=FALSE)
  
  # Test object type
  expect_is(ggrf.obj, "gg_rfsrc")
  
  # Test classification dimensions
  # Same number of rows:
  expect_equal(dim(ggrf.obj)[1], dim(veteran_rf$survival)[1])
  # Survival has time and cens columns
  expect_equal(dim(ggrf.obj)[2], dim(veteran_rf$survival)[2]+2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(select(ggrf.obj, -ptid, -cens)),
                    veteran_rf$survival)
})

test_that("gg_rfsrc regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  
  ## Load the cached forest
  data(airq_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(airq_rf, "rfsrc")
  
  # Test the forest family
  expect_match(airq_rf$family, "regr")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_rfsrc(airq_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_rfsrc")
  
  # Test Regression dimensions
  expect_equal(dim(ggrf.obj)[1], length(airq_rf$predicted.oob))
  expect_equal(dim(ggrf.obj)[2], 2)
  
  # Test data is correctly pulled from randomForest obect.
  # Predicted values
  expect_equivalent(ggrf.obj$yhat,airq_rf$predicted.oob)
  # y value for comparision
  expect_equivalent(ggrf.obj$Ozone,airq_rf$yvar)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_rfsrc(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_rfsrc(airq_rf, oob=FALSE)
  
  # Test object type
  expect_is(ggrf.obj, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(dim(ggrf.obj)[1], length(airq_rf$predicted))
  expect_equal(dim(ggrf.obj)[2], 2)
  
  # Test data is correctly pulled from randomForest obect.
  # Predicted values
  expect_equivalent(ggrf.obj$yhat,airq_rf$predicted)
  # y value for comparision
  expect_equivalent(ggrf.obj$Ozone,airq_rf$yvar)
  
})