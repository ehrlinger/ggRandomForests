# testthat for gg_variable function
context("gg_variable tests")

test_that("gg_variable classifications",{
  ## IF we want to build the forest every time...
  #   rfsrc_iris <- rfsrc(Species ~ ., data = iris)
  # rfsrc_iris <- var.select(rfsrc_iris)
  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_that(rfsrc_iris$family, equals("class"))
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_variable(rfsrc_iris)
  
  # Test object type
  expect_is(ggrf.obj, "gg_variable")
#   
#   ## Test plotting the gg_error object
#   gg.obj <- plot.gg_variable(ggrf.obj, xvar = "Petal.Width", )
#   
#   # Test return is s ggplot object
#   expect_is(gg.obj, "ggplot")
})


test_that("gg_variable survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   rfsrc_veteran <- var.select(rfsrc_veteran)
  ## Load the cached forest
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_variable(rfsrc_veteran, time=30)
  
  # Test object type
  expect_is(ggrf.obj, "gg_variable")
  
  ## Test plotting the gg_variable object
  gg.obj <- plot.gg_variable(ggrf.obj, xvar="age")
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_variable regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   rfsrc_airq <- var.select(rfsrc_airq)
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_variable(rfsrc_airq)
  
  # Test object type
  expect_is(ggrf.obj, "gg_variable")
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_variable(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj[[1]], "ggplot")
})