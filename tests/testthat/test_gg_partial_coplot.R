
# testthat for gg_partial_coplot function
context("gg_partial_coplot tests")
test_that("gg_error classifications",{
  
  data(rfsrc_Boston, package="ggRandomForests")
  
  # Find the rm variable points to create 6 intervals of roughly 
  # equal size population
  rm_pts <- quantile_pts(rfsrc_Boston$xvar$rm, groups=3, intervals=TRUE)
  
  # Pass these variable points to create the 6 (factor) intervals
  rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)
  
  partial_coplot_Boston <- gg_partial_coplot(rfsrc_Boston, xvar="lstat", 
                                             groups=rm_grp,
                                             show.plots=FALSE,
                                             npts=5)
  expect_is(partial_coplot_Boston, "gg_partial_coplot")
  
  expect_equal(ncol(partial_coplot_Boston), 3)
  
  expect_equal(length(unique(partial_coplot_Boston$group)), 3)
  
})