# testthat for gg_vimp function
context("partial.rfsrc tests")

test_that("partial.rfsrc regression",{  
   ## Load the cached forest
  data(rfsrc_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  ## Create the correct gg_error object
  # gg_dta<- partial.rfsrc(rfsrc_Boston, xvar.names = "lstat", npts=10)
  
  
})

test_that("partial.rfsrc survival",{  
  ## Load the cached forest
  data(rfsrc_pbc, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_pbc, "rfsrc")
  
  ## Create the correct gg_error object
  # gg_dta<- partial.rfsrc(rfsrc_Boston, xvar.names = "lstat", npts=10)
  
  
})

test_that("partial.rfsrc classification",{  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  ## Create the correct gg_error object
  # gg_dta<- partial.rfsrc(rfsrc_Boston, xvar.names = "lstat", npts=10)
  
  
})