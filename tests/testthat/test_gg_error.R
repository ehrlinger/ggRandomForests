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
  expect_equal(dim(gg_dta)[2], dim(rfsrc_iris$err.rate)[2] + 1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta[, -which(colnames(gg_dta) == "ntree")]), rfsrc_iris$err.rate)
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # "Incorrect object type: Expects a gg_error object"
  expect_error(gg_error(gg_plt))
  expect_error(gg_error.rfsrc(gg_plt))
  rfsrc_iris$err.rate <- NULL
  expect_error(gg_error(rfsrc_iris))
  
})


test_that("gg_error survival", {  
  ## Load the cached forest
  data(rfsrc_pbc, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_pbc, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_pbc$family, "surv")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_pbc)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta)[1], length(rfsrc_pbc$err.rate))
  expect_equal(dim(gg_dta)[2], 2)
  
  # Test data is correctly pulled from randomForest obect.
  tmp <- c(gg_dta[,1])
  expect_equivalent(tmp, rfsrc_pbc$err.rate)
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  expect_error(gg_error(gg_plt))
  # "Incorrect object type: Expects a gg_error object"
})

test_that("gg_error regression",{
  ## Load the cached forest
  data(rfsrc_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_Boston$family, "regr")
  
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_Boston)
  
  # Test object type
  expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), length(rfsrc_Boston$err.rate))
  expect_equal(ncol(gg_dta), 2)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(c(gg_dta[,1]), rfsrc_Boston$err.rate)
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_error(rfsrc_Boston)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Test the exception for input
  expect_error(gg_error(gg_plt))
  
  ## Create the correct gg_error object
 # gg_dta <- gg_error(rfsrc_Boston, training=TRUE)
  
  # Test object type
#  expect_is(gg_dta, "gg_error")
})