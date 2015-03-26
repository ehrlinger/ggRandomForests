# testthat for gg_vimp function
context("partial.rfsrc tests")

test_that("partial.rfsrc regression",{  
  
  ## Load the cached forest
  #data(rfsrc_Boston, package="ggRandomForests")
  data(Boston, package="MASS")
  
  Boston$chas <- as.logical(Boston$chas)
  
  rfsrc_Boston <- rfsrc(medv~., data=Boston, ntree=100)
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  xvar <- c("lstat","chas")
  ## Create the correct object
  gg_dta <- partial.rfsrc(rfsrc_Boston, 
                         xvar.names = xvar,
                         npts=10)
  expect_equal(gg_dta$xvar.names, xvar)
  expect_equal(names(gg_dta$pData), xvar)
  expect_equal(gg_dta$pData[[1]]$xvar.name, xvar[1])
  expect_equal(gg_dta$pData[[2]]$xvar.name, xvar[2])
  expect_equal(length(gg_dta$pData[[1]]$yhat),10)
  expect_equal(length(gg_dta$pData[[2]]$yhat),length(unique(rfsrc_Boston$xvar$chas)))
  
  ## Correct npts spec.
  gg_dta <- partial.rfsrc(rfsrc_Boston, 
                         xvar.names = c("lstat","chas"),
                         npts=-1)
  
  ## Number of vars spec.
  gg_dta <- partial.rfsrc(rfsrc_Boston, 
                         nvar = 2,
                         npts=3)
  
  ## subset by row numbers
  gg_dta <- partial.rfsrc(rfsrc_Boston, 
                         xvar.names = c("rm"),
                         subset=which(rfsrc_Boston$xvar$chas == 1),
                         npts=3)
  ## subset by logicals
  gg_dta <- partial.rfsrc(rfsrc_Boston, 
                         xvar.names = c("rm"),
                         subset=rfsrc_Boston$xvar$chas == 1,
                         npts=3)
  ##incorrect subset
  expect_error(partial.rfsrc(rfsrc_Boston, 
                             xvar.names = c("rm"),
                             subset=FALSE,
                             npts=10))
})

test_that("partial.rfsrc survival",{  
  ## Load the cached forest
  #data(rfsrc_pbc, package="ggRandomForests")
  data(pbc, package="randomForestSRC")
  
  # For whatever reason, the age variable is in days... makes no sense to me
  for(ind in 1:dim(pbc)[2]){
    if(!is.factor(pbc[,ind])){
      if(length(unique(pbc[which(!is.na(pbc[,ind])),ind])) <= 2) {
        if(sum(range(pbc[,ind],na.rm=TRUE) == c(0,1)) == 2){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
      }
    }else{
      if(length(unique(pbc[which(!is.na(pbc[,ind])),ind])) <= 2) {
        if(sum(sort(unique(pbc[,ind])) == c(0,1)) == 2){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
        if(sum(sort(unique(pbc[,ind])) == c(FALSE, TRUE)) == 2){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
      }
    }
    if(!is.logical(pbc[, ind]) & 
         length(unique(pbc[which(!is.na(pbc[,ind])),ind])) <= 5) {
      pbc[,ind] <- factor(pbc[,ind])
    }
  }
  # Convert age to years
  pbc$age <- pbc$age / 364.24
  
  pbc$years <- pbc$days / 364.24
  pbc <- pbc[, -which(colnames(pbc) == "days")]
  pbc$treatment <- as.numeric(pbc$treatment)
  pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
  pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
  pbc$treatment <- factor(pbc$treatment)
  
  dta.train <- pbc[-which(is.na(pbc$treatment)),]
  # Create a test set from the remaining patients
  pbc.test <- pbc[which(is.na(pbc$treatment)),]
  rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., dta.train, nsplit = 10,
                     na.action="na.impute", ntree=100)
  
  # Test the cached forest type
  expect_is(rfsrc_pbc, "rfsrc")
  xvar <- c("age", "copper")
  ## Create the correct object
  gg_dta <- partial.rfsrc(rfsrc_pbc, 
                         xvar.names = xvar, 
                         npts=10, surv.type="surv")
  
  expect_equal(gg_dta$xvar.names, xvar)
  expect_equal(names(gg_dta$pData), xvar)
  expect_equal(gg_dta$pData[[1]]$xvar.name, xvar[1])
  expect_equal(gg_dta$pData[[2]]$xvar.name, xvar[2])
  expect_equal(length(gg_dta$pData[[1]]$yhat),10)
  
  
  # pretend we have an unsupervised forest
  rfsrc_pbc$family <- "unsupv" 
  expect_error(partial.rfsrc(rfsrc_pbc, 
                             npts=10))
})

test_that("partial.rfsrc classification",{  
  ## Load the cached forest
  #data(rfsrc_iris, package="ggRandomForests")
  rfsrc_iris <- rfsrc(Species ~., data = iris, ntree=100)
  ## Load the cached forest
  data(partial_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  ## Create the correct object
  gg_dta <- partial.rfsrc(rfsrc_iris, 
                         npts=10)
  
  ## Create the correct object
  gg_dta <- partial.rfsrc(rfsrc_iris, 
                         npts=10, which.outcome="versicolor")
  
  ## Create the correct object
  gg_dta <- partial.rfsrc(rfsrc_iris, 
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