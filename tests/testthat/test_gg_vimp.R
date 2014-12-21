# testthat for gg_vimp function
context("gg_vimp tests")

test_that("gg_vimp classifications",{
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
  ggrf.obj<- gg_vimp(rfsrc_iris)
  
  # Test object type
  expect_is(ggrf.obj, "gg_vimp")
  
  # Test varselect is the same
 #expect_equivalent(select(ggrf.obj$varselect, -names), rfsrc_iris$importance)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})


test_that("gg_vimp survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   rfsrc_veteran <- var.select(rfsrc_veteran)
  ## Load the cached forest
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_vimp(rfsrc_veteran)
  
  # Test object type
  expect_is(ggrf.obj, "gg_vimp")
  
  # Test varselect is the same
  expect_equal(ggrf.obj$vimp, as.vector(sort(rfsrc_veteran$importance, decreasing=TRUE)))
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_vimp regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   rfsrc_airq <- var.select(rfsrc_airq)
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_vimp(rfsrc_airq)
  
  # Test object type
  expect_is(ggrf.obj, "gg_vimp")
  
  # Test varselect is the same
  expect_equal(ggrf.obj$vimp, as.vector(sort(rfsrc_airq$importance, decreasing=TRUE)))
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})