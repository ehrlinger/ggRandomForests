# testthat for surface_matrix function
context("surface_matrix tests")

test_that("cutting a vector at evenly space points", {
  # Load the stored rfsrc and partial coplot data.
  data(Boston, package = "MASS")
  boston <- Boston
  
  boston$chas <- as.logical(boston$chas)
  
  ## Load the cached forest
  rfsrc_boston <-
    randomForestSRC::rfsrc(
      medv ~ .,
      data = boston,
      forest = TRUE,
      importance = TRUE,
      tree.err = TRUE,
      save.memory = TRUE)
  
  rm_pts <-
    quantile_pts(rfsrc_boston$xvar$rm,
                 groups = 49,
                 intervals = TRUE)
  ## From vignette(randomForestRegression, package="ggRandomForests")
  ##
  # Load the stored partial coplot data.
  partial_boston_surf <- lapply(rm_pts, function(ct) {
    rfsrc_boston$xvar$rm <- ct
    randomForestSRC::plot.variable(
      rfsrc_boston,
      xvar.names = "lstat",
      time = 1,
      npts = 50,
      show.plots = FALSE,
      partial = TRUE
    )
  })
  # Instead of groups, we want the raw rm point values,
  # To make the dimensions match, we need to repeat the values
  # for each of the 50 points in the lstat direction
  rm_tmp <- do.call(c, lapply(rm_pts,
                              function(grp) {
                                rep(grp, length(partial_boston_surf))
                              }))
  
  # Convert the list of plot.variable output to
  partial_surf <-
    do.call(rbind, lapply(partial_boston_surf, gg_partial))
  
  # attach the data to the gg_partial_coplot
  partial_surf$rm <- rm_tmp
  
  # Transform the gg_partial_coplot object into a list of three named matrices
  # for surface plotting with plot3D::surf3D
  expect_warning(srf <-
                   surface_matrix(partial_surf, c("lstat", "rm", "yhat")))
  
  # a list,
  expect_is(srf, "list")
  
  # with 3 dimensions
  expect_equal(length(srf), 3)
  
  expect_equal(nrow(srf[[1]]), 50)
  expect_equal(ncol(srf[[1]]), length(partial_boston_surf))
  
  expect_equal(nrow(srf[[1]]), nrow(srf[[2]]))
  expect_equal(nrow(srf[[1]]), nrow(srf[[3]]))
  expect_equal(ncol(srf[[1]]), ncol(srf[[2]]))
  expect_equal(ncol(srf[[1]]), ncol(srf[[3]]))
  
})
