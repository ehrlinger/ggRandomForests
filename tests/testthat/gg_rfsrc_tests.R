# testthat for gg_rfsrc function
context("gg_rfsrc tests")

test_that("gg_rfsrc classifications",{
  
  iris.obj <- rfsrc(Species ~ ., data = iris)
  ggrf.obj<- gg_rfsrc(iris.obj)
  expect_is(ggrf.obj, "gg_rfsrc")
  expect_equal(dim(ggrf.obj)[1], dim(iris.obj$predicted.oob)[1])
  expect_equal(dim(ggrf.obj)[2], dim(iris.obj$predicted.oob)[2]+1)
  
  expect_equivalent(as.matrix(select(ggrf.obj, -y)), iris.obj$predicted.oob)
  
  gg.obj <- plot(ggrf.obj)
  expect_is(gg.obj, "ggplot")
  
})


test_that("gg_rfsrc survival",{
  
  data(veteran, package = "randomForestSRC")
  v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  
  ggrf.obj<- gg_rfsrc(v.obj)
  expect_is(ggrf.obj, "gg_rfsrc")
  expect_equal(dim(ggrf.obj)[1], dim(v.obj$survival.oob)[1])
  expect_equal(dim(ggrf.obj)[2], dim(v.obj$survival.oob)[2] +2)
  
  expect_equivalent(as.matrix(select(ggrf.obj, -ptid, -cens)),
                    v.obj$survival.oob)
  
  gg.obj <- plot(ggrf.obj, se=.95)
  expect_is(gg.obj, "ggplot")
})

test_that("gg_rfsrc regression",{
  ## New York air quality measurements
  airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  ggrf.obj<- gg_rfsrc(airq.obj)
  expect_is(ggrf.obj, "gg_rfsrc")
  expect_equal(dim(ggrf.obj)[1], length(airq.obj$predicted.oob))
  expect_equal(dim(ggrf.obj)[2], 2)
  
  
  expect_equivalent(ggrf.obj[,1], airq.obj$err.rate)
  
  gg.obj <- plot(ggrf.obj)
  expect_is(gg.obj, "ggplot")
})