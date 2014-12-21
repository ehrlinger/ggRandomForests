# testthat for gg_minimal_vimp function
context("gg_minimal_vimp tests")

test_that("gg_minimal_vimp classifications",{
  ## IF we want to build the forest every time...
  #   iris_rf <- rfsrc(Species ~ ., data = iris)
  # varsel_iris <- var.select(iris_rf)
  
  ## Load the cached forest
  data(varsel_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_iris, "list")
  
  # Test the forest family
  expect_that(is.null(varsel_iris$md.obj), is_false())
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_vimp(varsel_iris)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_iris$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})


test_that("gg_minimal_vimp survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   varsel_veteran <- var.select(rfsrc_veteran)
  ## Load the cached forest
  data(varsel_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_veteran, "list")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_vimp(varsel_veteran)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_veteran$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_minimal_vimp regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   varsel_airq <- var.select(rfsrc_airq)
  ## Load the cached forest
  data(varsel_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_airq, "list")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_vimp(varsel_airq)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_airq$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})