# testthat for gg_minimal_depth function
context("gg_minimal_depth tests")

test_that("gg_minimal_depth classifications",{
  ## IF we want to build the forest every time...
  #   iris_rf <- rfsrc(Species ~ ., data = iris)
  # iris_vs <- var.select(iris_rf)
  
  ## Load the cached forest
  data(iris_vs, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(iris_vs, "list")
  
  # Test the forest family
  expect_that(is.null(iris_vs$md.obj), is_false())
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_depth(iris_vs)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_depth")
  
  # Test varselect is the same
  expect_equivalent(select(ggrf.obj$varselect, -names), iris_vs$varselect)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_depth(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})


test_that("gg_minimal_depth survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   veteran_vs <- var.select(veteran_rf)
  ## Load the cached forest
  data(veteran_vs, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(veteran_vs, "list")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_depth(veteran_vs)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_depth")
  
  # Test varselect is the same
  expect_equivalent(select(ggrf.obj$varselect, -names), veteran_vs$varselect)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_depth(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_minimal_depth regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   airq_vs <- var.select(airq_rf)
  ## Load the cached forest
  data(airq_vs, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(airq_vs, "list")
  
  ## Create the correct gg_error object
  ggrf.obj<- gg_minimal_depth(airq_vs)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_depth")
  
  # Test varselect is the same
  expect_equivalent(select(ggrf.obj$varselect, -names), airq_vs$varselect)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_depth(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})