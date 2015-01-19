# testthat for gg_rfsrc function
context("gg_rfsrc tests")

test_that("gg_rfsrc classifications",{
  ## IF we want to build the forest every time...
  #   rfsrc_iris <- rfsrc(Species ~ ., data = iris)
  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_is(rfsrc_iris, "class")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), nrow(rfsrc_iris$predicted.oob))
  expect_equal(ncol(gg_dta), ncol(rfsrc_iris$predicted.oob)+1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta[, -which(colnames(gg_dta)=="y")]),
                    rfsrc_iris$predicted.oob)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_iris, oob=FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  expect_equal(nrow(gg_dta), nrow(rfsrc_iris$predicted))
  expect_equal(ncol(gg_dta), ncol(rfsrc_iris$predicted)+1)
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta[, -which(colnames(gg_dta)=="y")]), 
                    rfsrc_iris$predicted) 
})


test_that("gg_rfsrc survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  
  ## Load the cached forest
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_veteran$family, "surv")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_veteran)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "surv")
  
  # Test classification dimensions
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_veteran, oob=FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "surv")
  # Test classification dimensions
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta, alpha=.4)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Test classification dimensions
  
  gg_dta<- gg_rfsrc(rfsrc_veteran, by="trt")
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "surv")
  
  ## Create the correct gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  
  gg_dta<- gg_rfsrc(rfsrc_veteran,conf.int=.68)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "surv")
  
  # Test multiple conf intervals
  gg_dta<- gg_rfsrc(rfsrc_veteran,conf.int=c(.025, .975), bs.sample=100)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "surv")
  
  ## Create the correct gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Test prediction
  ## Load the cached forest
  data("rfsrc_pbc", package="ggRandomForests")
  data(pbc, package="randomForestSRC")
  # For whatever reason, the age variable is in days... makes no sense to me
  for(ind in 1:dim(pbc)[2]){
    if(!is.factor(pbc[,ind])){
      if(length(unique(pbc[which(!is.na(pbc[,ind])),ind]))<=2) {
        if(sum(range(pbc[,ind],na.rm=TRUE) == c(0,1))==2){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
      }
    }else{
      if(length(unique(pbc[which(!is.na(pbc[,ind])),ind]))<=2) {
        if(sum(sort(unique(pbc[,ind])) == c(0,1))==2){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
        if(sum(sort(unique(pbc[,ind])) == c(FALSE, TRUE))==2){
          pbc[,ind] <- as.logical(pbc[,ind])
        }
      }
    }
    if(!is.logical(pbc[, ind]) & 
         length(unique(pbc[which(!is.na(pbc[,ind])),ind]))<=5) {
      pbc[,ind] <- factor(pbc[,ind])
    }
  }
  # Convert age to years
  pbc$age <- pbc$age/364.24
  
  pbc$years <- pbc$days/364.24
  pbc <- pbc[, -which(colnames(pbc)=="days")]
  pbc$treatment <- as.numeric(pbc$treatment)
  pbc$treatment[which(pbc$treatment==1)] <- "DPCA"
  pbc$treatment[which(pbc$treatment==2)] <- "placebo"
  pbc$treatment <- factor(pbc$treatment)
  
  prd_dta <- predict(rfsrc_pbc,
                     newdata=pbc[which(is.na(pbc$treatment)),],
                     na.action="na.impute")
  
  expect_is(gg_dta <- gg_rfsrc(prd_dta), "gg_rfsrc")
  
  # Test for group "by" name exists
  expect_error(gg_rfsrc(rfsrc_pbc, by="trt"))
  # And it's a vector or factor (not a number)
  expect_error(gg_rfsrc(rfsrc_pbc, by=3))
  
  # Test confidence intervals
  
})

test_that("gg_rfsrc regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  # Test the forest family
  expect_match(rfsrc_airq$family, "regr")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_airq)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "regr")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_airq, oob=FALSE)
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  
  # Test classification dimensions
  ## Create the correct gg_error object
  gg_dta<- gg_rfsrc(rfsrc_airq, by="Month")
  
  # Test object type
  expect_is(gg_dta, "gg_rfsrc")
  expect_is(gg_dta, "regr")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test data is correctly pulled from randomForest obect.
  # Predicted values
  rfsrc_airq$family <- "test"
  expect_error(gg_rfsrc(rfsrc_airq))
  
  # Test exceptions
  # Is it an rfsrc object?
  expect_error(gg_rfsrc(gg_plt))
  
  # Does it contain the forest?
  rfsrc_airq$forest <- NULL
  expect_error(gg_rfsrc(rfsrc_airq))
  
})