# testthat for gg_roc function
context("gg_roc tests")

test_that("gg_roc classifications",{
  
  ## Load the cached forest
  data(iris_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(iris_rf, "rfsrc")
  
  # Test the forest family
  expect_match(iris_rf$family, "class")
  
  ## Create the correct gg_roc object
  which.outcome=1
  ggrf.obj <- gg_roc(iris_rf, which.outcome)
  
  # Test object type
  expect_is(ggrf.obj, "gg_roc")
  
  # Test classification dimensions
  expect_equal(nrow(ggrf.obj), length(unique(iris_rf$predicted.oob[,which.outcome]))+1)
  expect_equal(ncol(ggrf.obj), 3)
  
  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(iris_rf$predicted.oob[,which.outcome]))
  expect_equivalent(ggrf.obj$pct, c(0,unts[-length(unts)],1))
  
  ## Test plotting the gg_roc object
  gg.obj <- plot.gg_roc(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  # "Incorrect object type: Expects a gg_roc object"
})


test_that("gg_roc survival",{
  
  #   data(veteran, package = "randomForestSRC")
  #   v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  
  ## Load the cached forest
  data(veteran_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(veteran_rf, "rfsrc")
  
  # Test the forest family
  expect_match(veteran_rf$family, "surv")
  
  ## Create the correct gg_roc object
  expect_that(gg_roc(veteran_rf), throws_error())
  
})

test_that("gg_roc regression",{
  
  ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   ggrf.obj<- gg_roc(airq.obj)
  
  ## Load the cached forest
  data(airq_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(airq_rf, "rfsrc")
  
  # Test the forest family
  expect_match(airq_rf$family, "regr")
  
  ## Create the correct gg_roc object
  expect_that(gg_roc(airq_rf), throws_error())
})