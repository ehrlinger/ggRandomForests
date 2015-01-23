# testthat for gg_vimp function
context("partial.rfsrc tests")

test_that("partial.rfsrc regression",{  
  ## Load the cached forest
  data(rfsrc_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  ## Create the correct object
  gg_dta<- partial.rfsrc(rfsrc_Boston, 
                         xvar.names = c("lstat","chas"),
                         npts=10)
  
  ## Correct npts spec.
  gg_dta<- partial.rfsrc(rfsrc_Boston, 
                         xvar.names = c("lstat","chas"),
                         npts=-1)
  
  ## Number of vars spec.
  gg_dta<- partial.rfsrc(rfsrc_Boston, 
                         nvar = 2,
                         npts=3)
  
  ## subset by row numbers
  gg_dta<- partial.rfsrc(rfsrc_Boston, 
                         xvar.names = c("rm"),
                         subset=which(rfsrc_Boston$xvar$chas==1),
                         npts=3)
  ## subset by logicals
  gg_dta<- partial.rfsrc(rfsrc_Boston, 
                         xvar.names = c("rm"),
                         subset=rfsrc_Boston$xvar$chas==1,
                         npts=3)
  ##incorrect subset
  expect_error(partial.rfsrc(rfsrc_Boston, 
                             xvar.names = c("rm"),
                             subset=FALSE,
                             npts=10))
})

test_that("partial.rfsrc survival",{  
  ## Load the cached forest
  data(rfsrc_pbc, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_pbc, "rfsrc")
  
  ## Create the correct object
  gg_dta<- partial.rfsrc(rfsrc_pbc, 
                         xvar.names = c("age", "copper"), 
                         npts=10, time=1, surv.type="surv")
  
  # Survival without a time.
  expect_warning(partial.rfsrc(rfsrc_pbc, 
                             xvar.names = c("age", "copper"), 
                             npts=10, surv.type="surv"))
  
  # pretend we have an unsupervised forest
  rfsrc_pbc$family <- "unsupv" 
  expect_error(partial.rfsrc(rfsrc_pbc, 
                             npts=10))
})

test_that("partial.rfsrc classification",{  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  ## Load the cached forest
  data(partial_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  ## Create the correct object
  gg_dta<- partial.rfsrc(rfsrc_iris, 
                         npts=10)
  
  ## Create the correct object
  gg_dta<- partial.rfsrc(rfsrc_iris, 
                         npts=10, which.outcome="versicolor")
  
  ## Create the correct object
  gg_dta<- partial.rfsrc(rfsrc_iris, 
                         npts=10, which.outcome=2)
  # Wrong "rfsrc" type 
  expect_error(partial.rfsrc(partial_iris, 
                             npts=10))
  
  # No "forest" stored in rfsrc object
  rfsrc_iris$forest <- NULL
  expect_error(partial.rfsrc(rfsrc_iris, 
                             npts=10))
  
  # Incorrect xvar name.
  expect_error(partial.rfsrc(rfsrc_iris,xvar.names = "lmstat", 
                             npts=10))
})