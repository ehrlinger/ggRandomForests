# testthat for gg_survival function
context("gg_survival tests")

test_that("gg_survival classifications",{
  ## IF we want to build the forest every time...
  #   iris_rf <- rfsrc(Species ~ ., data = iris)
  
  ## Load the cached forest
  data(iris_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(iris_rf, "rfsrc")
  
  # Test the forest family
  expect_match(iris_rf$family, "class")
  
  ## Create the correct gg_error object
  expect_that(gg_survival(iris_rf), throws_error())
})


test_that("gg_survival survival",{
  
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
  ggrf.obj<- gg_survival(veteran_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_survival")
  
  # Test classification dimensions
  # Same number of rows:
  expect_equal(ggrf.obj$time, veteran_rf$time.interest)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_survival(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_survival(veteran_rf, oob=FALSE)
  
  # Test object type
  expect_is(ggrf.obj, "gg_survival")
  
  # Test classification dimensions
  # Same number of rows:
  expect_equal(ggrf.obj$time, veteran_rf$time.interest)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_survival(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
})

test_that("gg_survival regression",{
  
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
  expect_that(gg_survival(airq_rf), throws_error())
})