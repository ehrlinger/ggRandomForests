# testthat for surface_matrix function
context("surface_matrix tests")

test_that("cutting a vector at evenly space points",{
  
  # Load the stored rfsrc and partial coplot data.
  data(rfsrc_Boston)
  data(partial_coplot_Boston_surf)
  
  # Find the quantile points 50
  rm_pts <- quantile_pts(rfsrc_Boston$xvar$rm, groups=50)
  
  # Instead of groups, we want the raw rm point values,
  # To make the dimensions match, we need to repeat the values
  # for each of the 50 points in the lstat direction
  rm.tmp <- do.call(c,lapply(rm_pts, 
                             function(grp){rep(grp, 50)}))
  
  # attach the data to the gg_partial_coplot
  partial_coplot_Boston_surf$rm <- rm.tmp
  
  srf <- surface_matrix(partial_coplot_Boston_surf, c("lstat", "rm", "yhat"))
  
  # a list,
  expect_is(srf, "list")
  
  # with 3 dimensions
  expect_equal(length(srf), 3)
  
  expect_equal(nrow(srf[[1]]), 50)
  expect_equal(ncol(srf[[1]]), 50)
  
  expect_equal(nrow(srf[[1]]),nrow(srf[[2]]) )
  expect_equal(nrow(srf[[1]]),nrow(srf[[3]]) )
  expect_equal(ncol(srf[[1]]),ncol(srf[[2]]) )
  expect_equal(ncol(srf[[1]]),ncol(srf[[3]]) )
  
  # Test that we get something back here.
  srf <- surface_matrix(partial_coplot_Boston_surf)
  
})