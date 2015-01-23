# testthat for gg_vimp function
context("partial.rfsrc tests")

test_that("partial.rfsrc regression",{  
  ## Load the cached forest
  ## Load the cached forest
  data(rfsrc_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  ## Create the correct gg_error object
  # gg_dta<- partial.rfsrc(rfsrc_Boston, xvar.names = "lstat", npts=10)
  
  
})