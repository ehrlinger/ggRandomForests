# testthat for gg_partial function
context("gg_partial tests")

test_that("gg_partial classifications",{
  ## IF we want to build the forest every time...
  #   iris_rf <- rfsrc(Species ~ ., data = iris)
  # iris_rf <- var.select(iris_rf)
  
  ## Load the cached forest
  data(iris_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(iris_rf, "rfsrc")
  
  # Test the forest family
  expect_that(iris_rf$family, equals("class"))
  
  # Load saved partial plot data.
  data(iris_prtl, package="ggRandomForests")
  
  expect_equivalent(length(iris_prtl$pData), length(iris_rf$xvar.names))
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_partial(iris_prtl)
  
  # Test object type
  expect_is(ggrf.obj, "gg_partial_list")
  
  # Test varselect is the same
 #expect_equivalent(select(ggrf.obj$varselect, -names), iris_rf$importance)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_partial(ggrf.obj[[2]])
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
 
 ## Test plotting the gg_error object
 gg.obj <- plot.gg_partial_list(ggrf.obj)
 
 # Test return is s ggplot object
 expect_is(gg.obj[[1]], "ggplot")
 
 expect_equivalent(length(gg.obj),length(iris_prtl$pData) )
})


test_that("gg_partial survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   veteran_rf <- var.select(veteran_rf)
  ## Load the cached forest
  data(veteran_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(veteran_rf, "rfsrc")
  
  ## Get the partial data.
  data(veteran_prtl, package="ggRandomForests")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_partial(veteran_prtl)
  
  # Test object type
  expect_is(ggrf.obj, "gg_partial")
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_partial(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_partial regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   airq_rf <- var.select(airq_rf)
  ## Load the cached forest
  data(airq_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(airq_rf, "rfsrc")
  
  ## Create the correct gg_error object
  data(airq_prtl, package="ggRandomForests")
  ggrf.obj<- gg_partial(airq_prtl)
  
  # Test object type
  expect_is(ggrf.obj, "gg_partial_list")
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_partial(ggrf.obj[[1]])
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_partial_list(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "list")
  
  expect_equivalent(length(gg.obj) , length(ggrf.obj))
})