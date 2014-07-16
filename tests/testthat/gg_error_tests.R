# testthat for gg_error function
context("gg_error tests")

test_that("gg_error classifications",{
  
  iris.obj <- rfsrc(Species ~ ., data = iris)
  ggrf.obj<- gg_error(iris.obj)
  expect_true(inherits(ggrf.obj, "gg_error"))
  expect_equal(dim(ggrf.obj)[1], dim(iris.obj$err.rate)[1])
  expect_equal(dim(ggrf.obj)[2], dim(iris.obj$err.rate)[2]+1)
  
  gg.obj <- plot(ggrf.obj)
  expect_true(inherits(gg.obj, "ggplot"))
  
})


test_that("gg_error survival",{
  
  data(veteran, package = "randomForestSRC")
  v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  
  ggrf.obj<- gg_error(v.obj)
  expect_true(inherits(ggrf.obj, "gg_error"))
  expect_equal(dim(ggrf.obj)[1], length(v.obj$err.rate))
  expect_equal(dim(ggrf.obj)[2], 2)
  
  gg.obj <- plot(ggrf.obj)
  expect_true(inherits(gg.obj, "ggplot"))
})

test_that("gg_error regression",{
  ## New York air quality measurements
  airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  ggrf.obj<- gg_error(airq.obj)
  expect_true(inherits(ggrf.obj, "gg_error"))
  expect_equal(dim(ggrf.obj)[1], length(airq.obj$err.rate))
  expect_equal(dim(ggrf.obj)[2], 2)
  
  gg.obj <- plot(ggrf.obj)
  expect_true(inherits(gg.obj, "ggplot"))
})