# testthat for gg_variable function
context("gg_variable tests")

test_that("gg_variable classifications",{
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
  ggrf.obj<- gg_variable(iris_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_variable")
#   
#   ## Test plotting the gg_error object
#   gg.obj <- plot.gg_variable(ggrf.obj, x_var = "Petal.Width", )
#   
#   # Test return is s ggplot object
#   expect_is(gg.obj, "ggplot")
})


test_that("gg_variable survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   veteran_rf <- var.select(veteran_rf)
  ## Load the cached forest
  data(veteran_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(veteran_rf, "rfsrc")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_variable(veteran_rf, time=30)
  
  # Test object type
  expect_is(ggrf.obj, "gg_variable")
  
  ## Test plotting the gg_variable object
  gg.obj <- plot.gg_variable(ggrf.obj, x_var="age")
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_variable regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   airq_rf <- var.select(airq_rf)
  ## Load the cached forest
  data(airq_rf, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(airq_rf, "rfsrc")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_variable(airq_rf)
  
  # Test object type
  expect_is(ggrf.obj, "gg_variable")
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_variable(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj[[1]], "ggplot")
})