# testthat for gg_roc function
context("gg_roc tests")

test_that("gg_roc classifications",{
  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_iris$family, "class")
  
  ## Create the correct gg_roc object
  which.outcome=1
  ggrf.obj <- gg_roc(rfsrc_iris, which.outcome)
  
  # Test object type
  expect_is(ggrf.obj, "gg_roc")
  
  # Test classification dimensions
  expect_equal(nrow(ggrf.obj), length(unique(rfsrc_iris$predicted.oob[,which.outcome]))+1)
  expect_equal(ncol(ggrf.obj), 3)
  
  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(rfsrc_iris$predicted.oob[,which.outcome]))
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
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_veteran$family, "surv")
  
  ## Create the correct gg_roc object
  expect_that(gg_roc(rfsrc_veteran), throws_error())
  
})

test_that("gg_roc regression",{
  
  ## New York air quality measurements
  #   rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   ggrf.obj<- gg_roc(rfsrc_airq)
  
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_airq$family, "regr")
  
  ## Create the correct gg_roc object
  expect_that(gg_roc(rfsrc_airq), throws_error())
})