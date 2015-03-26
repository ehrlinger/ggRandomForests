# testthat for surface_matrix function
context("surface_matrix tests")

test_that("cutting a vector at evenly space points",{
  
  # Load the stored rfsrc and partial coplot data.
  data(rfsrc_Boston)
  rm_pts <- quantile_pts(rfsrc_Boston$xvar$rm, groups=50)
  
  ## From vignette(randomForestRegression, package="ggRandomForests")
  ##
  # Load the stored partial coplot data.
  data(partial_Boston_surf)
  
  # Instead of groups, we want the raw rm point values,
  # To make the dimensions match, we need to repeat the values
  # for each of the 50 points in the lstat direction
  rm.tmp <- do.call(c,lapply(rm_pts, 
                             function(grp){ 
                               rep(grp, 50)
                             }))
  
  # Convert the list of plot.variable output to 
  partial_surf <- do.call(rbind,lapply(partial_Boston_surf, gg_partial))
  
  # attach the data to the gg_partial_coplot
  partial_surf$rm <- rm.tmp
  
  # Transform the gg_partial_coplot object into a list of three named matrices
  # for surface plotting with plot3D::surf3D
  expect_warning(srf <- surface_matrix(partial_surf, c("lstat", "rm", "yhat")))
  
  # a list,
  expect_is(srf, "list")
  
  # with 3 dimensions
  expect_equal(length(srf), 3)
  
  expect_equal(nrow(srf[[1]]), 50)
  expect_equal(ncol(srf[[1]]), 50)
  
  expect_equal(nrow(srf[[1]]), nrow(srf[[2]]))
  expect_equal(nrow(srf[[1]]), nrow(srf[[3]]))
  expect_equal(ncol(srf[[1]]), ncol(srf[[2]]))
  expect_equal(ncol(srf[[1]]), ncol(srf[[3]]))
  
})