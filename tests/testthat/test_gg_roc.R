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
  
  # Try test set prediction.
  ggrf.obj <- gg_roc(rfsrc_iris, which.outcome, oob=FALSE)
  
  # Test object type
  expect_is(ggrf.obj, "gg_roc")
  
  # Test classification dimensions
  expect_equal(nrow(ggrf.obj), length(unique(rfsrc_iris$predicted[,which.outcome]))+1)
  expect_equal(ncol(ggrf.obj), 3)
  
  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(rfsrc_iris$predicted[,which.outcome]))
  expect_equivalent(ggrf.obj$pct, c(0,unts[-length(unts)],1))
  
  ## Test plotting the gg_roc object
  gg.obj <- plot.gg_roc(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  expect_is(plot.gg_roc(rfsrc_iris), "ggplot")
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
  expect_error(gg_roc(rfsrc_veteran))
  
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
  expect_error(gg_roc(rfsrc_airq))
})

test_that("calc_roc",{
  data(rfsrc_iris)
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_iris$family, "class")
  
  gg_dta <- calc_roc.rfsrc(rfsrc_iris, 
                           rfsrc_iris$yvar, 
                           which.outcome=1, oob=TRUE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  expect_equal(nrow(gg_dta), 27)
  
  
  # Test oob=FALSE
  gg_dta <- calc_roc.rfsrc(rfsrc_iris, 
                           rfsrc_iris$yvar, 
                           which.outcome=1, oob=FALSE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_equal(auc, 1)
  
  # The second outcome.
  gg_dta <- calc_roc.rfsrc(rfsrc_iris, 
                           rfsrc_iris$yvar, 
                           which.outcome=2, oob=TRUE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  expect_equal(nrow(gg_dta), 82)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_equal(auc, 0.9844)
  
  # and the third...
  gg_dta <- calc_roc.rfsrc(rfsrc_iris, 
                           rfsrc_iris$yvar, 
                           which.outcome=3, oob=TRUE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  expect_equal(nrow(gg_dta), 68)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_equal(auc, 0.9886)
})