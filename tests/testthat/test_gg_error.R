# testthat for gg_error function
context("gg_error tests")

test_that("gg_error classifications",{
  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_iris$family, "class")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta)[1], dim(rfsrc_iris$err.rate)[1])
  expect_equal(dim(gg_dta)[2], dim(rfsrc_iris$err.rate)[2]+1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta[, -which(colnames(gg_dta)=="ntree")]), rfsrc_iris$err.rate)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_error(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # "Incorrect object type: Expects a gg_error object"
  expect_error(gg_error(gg_plt))
  rfsrc_iris$err.rate <- NULL
  expect_error(gg_error(rfsrc_iris))
  
})


test_that("gg_error survival",{
  
  #   data(veteran, package = "randomForestSRC")
  #   v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  
  ## Load the cached forest
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_veteran$family, "surv")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_veteran)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta)[1], length(rfsrc_veteran$err.rate))
  expect_equal(dim(gg_dta)[2], 2)
  
  # Test data is correctly pulled from randomForest obect.
  tmp <- c(gg_dta[,1])
  expect_equivalent(tmp, rfsrc_veteran$err.rate)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_error(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  expect_error(gg_error(gg_plt))
  # "Incorrect object type: Expects a gg_error object"
})

test_that("gg_error regression",{
  
  ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   gg_dta<- gg_error(airq.obj)
  
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_airq$family, "regr")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_airq)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), length(rfsrc_airq$err.rate))
  expect_equal(ncol(gg_dta), 2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(c(gg_dta[,1]), rfsrc_airq$err.rate)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_error(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
})