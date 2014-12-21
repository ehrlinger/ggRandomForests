# testthat for gg_rfsrc function
context("gg_rfsrc tests")

test_that("gg_rfsrc classifications",{
  ## IF we want to build the forest every time...
  #   rfsrc_iris <- rfsrc(Species ~ ., data = iris)
  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_iris$family, "class")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta$yhat), nrow(rfsrc_iris$predicted.oob))
  expect_equal(ncol(gg_dta$yhat), ncol(rfsrc_iris$predicted.oob)+1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta$yhat[, -which(colnames(gg_dta$yhat)=="y")]),
                    rfsrc_iris$predicted.oob)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_iris, oob=FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta$yhat), nrow(rfsrc_iris$predicted))
  expect_equal(ncol(gg_dta$yhat), ncol(rfsrc_iris$predicted)+1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(select(gg_dta$yhat, -y)), 
                    rfsrc_iris$predicted) 
})


test_that("gg_rfsrc survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  
  ## Load the cached forest
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_veteran$family, "surv")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_veteran)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  # Same number of rows:
  expect_equal(nrow(gg_dta$yhat), nrow(rfsrc_veteran$survival.oob))
  # Survival has time and cens columns
  expect_equal(ncol(gg_dta$yhat), ncol(rfsrc_veteran$survival.oob) +2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(select(gg_dta$yhat, -ptid, -cens)),
                    rfsrc_veteran$survival.oob)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_veteran, oob=FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  # Same number of rows:
  expect_equal(nrow(gg_dta$yhat), nrow(rfsrc_veteran$survival))
  # Survival has time and cens columns
  expect_equal(ncol(gg_dta$yhat), ncol(rfsrc_veteran$survival)+2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(select(gg_dta$yhat, -ptid, -cens)),
                    rfsrc_veteran$survival)
})

test_that("gg_rfsrc regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_airq$family, "regr")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_airq)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test Regression dimensions
  expect_equal(nrow(gg_dta$yhat), length(rfsrc_airq$predicted.oob))
  expect_equal(ncol(gg_dta$yhat), 2)
  
  # Test data is correctly pulled from randomForest obect.
  # Predicted values
  expect_equivalent(gg_dta$yhat$yhat,rfsrc_airq$predicted.oob)
  # y value for comparision
  expect_equivalent(gg_dta$yhat$Ozone,rfsrc_airq$yvar)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_airq, oob=FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta$yhat), length(rfsrc_airq$predicted))
  expect_equal(ncol(gg_dta$yhat), 2)
  
  # Test data is correctly pulled from randomForest obect.
  # Predicted values
  expect_equivalent(gg_dta$yhat$yhat,rfsrc_airq$predicted)
  # y value for comparision
  expect_equivalent(gg_dta$yhat$Ozone,rfsrc_airq$yvar)
  
})