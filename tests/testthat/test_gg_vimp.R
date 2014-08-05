# testthat for gg_vimp function
context("gg_vimp tests")

test_that("gg_vimp classifications",{
  ## IF we want to build the forest every time...
  #   iris_rf <- rfsrc(Species ~ ., data = iris)
  # iris_rf <- var.select(iris_rf)
  
  ## Load the cached forest
  data(iris_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(iris_rf, "rfsrc")
  
  # Test the forest family
  expect_that(iris_rf$family, equals("class"))
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_vimp(iris_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_vimp")
  
  # Test varselect is the same
 #expect_equivalent(select(ggrf.obj$varselect, -names), iris_rf$importance)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})


test_that("gg_vimp survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   veteran_rf <- var.select(veteran_rf)
  ## Load the cached forest
  data(veteran_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(veteran_rf, "rfsrc")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_vimp(veteran_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_vimp")
  
  # Test varselect is the same
  expect_equal(ggrf.obj$VIMP, as.vector(sort(veteran_rf$importance, decreasing=TRUE)))
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_vimp regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   airq_rf <- var.select(airq_rf)
  ## Load the cached forest
  data(airq_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(airq_rf, "rfsrc")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_vimp(airq_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_vimp")
  
  # Test varselect is the same
  expect_equal(ggrf.obj$VIMP, as.vector(sort(airq_rf$importance, decreasing=TRUE)))
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})