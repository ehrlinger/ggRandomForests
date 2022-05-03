



# testthat for gg_partial_coplot function
context("gg_partial_coplot tests")
test_that("gg_error classifications", {
  data(Boston, package = "MASS")
  
  # Unless we are on the same version as Travis-CI,
  # we need to build rather than cache the rfsrc
  rfsrc_boston <- rfsrc(
    medv ~ .,
    data = Boston,
    importance = "none",
    nsplit = 5,
    forest = TRUE
  )
  # fast.restore can be added after randomForestSRC V1.6 release
  
  # Find the rm variable points to create 6 intervals of roughly
  # equal size population
  rm_pts <-
    quantile_pts(rfsrc_boston$xvar$rm,
                 groups = 3,
                 intervals = TRUE)
  
  # Pass these variable points to create the 6 (factor) intervals
  rm_grp <- cut(rfsrc_boston$xvar$rm, breaks = rm_pts)
  
  # This is the expensive part.
  partial_coplot_boston <-
    gg_partial_coplot(
      rfsrc_boston,
      xvar = "lstat",
      groups = rm_grp,
      show.plots = FALSE,
      npts = 5
    )
  expect_is(partial_coplot_boston, "gg_partial_coplot")
  
  expect_equal(ncol(partial_coplot_boston), 4)
  
  expect_equal(length(unique(partial_coplot_boston$group)), 3)
  
  expect_error(gg_partial_coplot(
    partial_coplot_boston,
    xvar = "lstat",
    groups = rm_grp,
    npts = 5
  ))
  expect_error(gg_partial_coplot(rfsrc_boston, xvar = "lstat",
                                 npts = 5))
  rfsrc_boston$forest <- NULL
  expect_error(
    gg_partial_coplot(
      rfsrc_boston,
      xvar = "lstat",
      groups = rm_grp,
      show.plots = FALSE,
      npts = 5
    )
  )
  
  
})
