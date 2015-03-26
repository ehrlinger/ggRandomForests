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
  which.outcome <- 1
  gg_dta <- gg_roc(rfsrc_iris, which.outcome)
  
  # Test object type
  expect_is(gg_dta, "gg_roc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), 
               length(unique(rfsrc_iris$predicted.oob[,which.outcome])) + 1)
  expect_equal(ncol(gg_dta), 3)
  
  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(rfsrc_iris$predicted.oob[,which.outcome]))
  expect_equivalent(gg_dta$pct, c(0,unts[-length(unts)],1))
  
  ## Test plotting the gg_roc object
  gg.obj <- plot.gg_roc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  # Try test set prediction.
  gg_dta <- gg_roc(rfsrc_iris, which.outcome, oob=FALSE)
  # Try test set prediction.
  gg_plt <- plot.gg_roc(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_roc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), 
               length(unique(rfsrc_iris$predicted[,which.outcome])) + 1)
  expect_equal(ncol(gg_dta), 3)
  
  # Test data is correctly pulled from randomForest obect.
  unts <- sort(unique(rfsrc_iris$predicted[,which.outcome]))
  expect_equivalent(gg_dta$pct, c(0,unts[-length(unts)],1))
  
  ## Test plotting the gg_roc object
  gg.obj <- plot.gg_roc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  expect_is(plot.gg_roc(rfsrc_iris), "ggplot")
})


test_that("gg_roc survival",{
  ## Load the cached forest
  data(rfsrc_pbc, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_pbc, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_pbc$family, "surv")
  
  ## Create the correct gg_roc object
  expect_error(gg_roc(rfsrc_pbc))
  
})

test_that("gg_roc regression",{
  ## Load the cached forest
  data(rfsrc_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_Boston$family, "regr")
  
  ## Create the correct gg_roc object
  expect_error(gg_roc(rfsrc_Boston))
  expect_error(plot.gg_roc(rfsrc_Boston))
  
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
  expect_equal(nrow(gg_dta), length(unique(rfsrc_iris$predicted.oob[,1])) + 1)
  
  expect_error(calc_roc.rfsrc(rfsrc_iris, 
                              rfsrc_iris$yvar, 
                              which.outcome="all"))
  # Test oob=FALSE
  gg_dta <- calc_roc.rfsrc(rfsrc_iris, 
                           rfsrc_iris$yvar, 
                           which.outcome=1, oob=FALSE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
  # The second outcome.
  gg_dta <- calc_roc.rfsrc(rfsrc_iris, 
                           rfsrc_iris$yvar, 
                           which.outcome=2, oob=TRUE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  expect_equal(nrow(gg_dta), length(unique(rfsrc_iris$predicted.oob[,2])) + 1)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
  # and the third...
  gg_dta <- calc_roc.rfsrc(rfsrc_iris, 
                           rfsrc_iris$yvar, 
                           which.outcome=3, oob=TRUE)
  
  # Test the cached forest type
  expect_is(gg_dta, "data.frame")
  
  expect_equal(ncol(gg_dta), 3)
  expect_equal(nrow(gg_dta),length(unique(rfsrc_iris$predicted.oob[,3])) + 1)
  
  # test the auc calculator
  auc <- calc_auc(gg_dta)
  expect_true(auc > .9)
  expect_true(auc <= 1)
})