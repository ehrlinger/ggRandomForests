# testthat for gg_partial function
context("gg_partial tests")

test_that("gg_partial classifications",{
  ## IF we want to build the forest every time...
  #   rfsrc_iris <- rfsrc(Species ~ ., data = iris)
  # varsel_iris <- var.select(rfsrc_iris)
  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_that(rfsrc_iris$family, equals("class"))
  
  # Load saved partial plot data.
  data(partial_iris, package="ggRandomForests")
  
  expect_equivalent(length(partial_iris$pData), length(rfsrc_iris$xvar.names))
  
  ## Create the correct gg_error object
  gg_dta<- gg_partial(partial_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_partial_list")
  
  # Test varselect is the same
 #expect_equivalent(select(gg_dta$varselect, -names), rfsrc_iris$importance)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_partial(gg_dta[[2]])
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
 
 ## Test plotting the gg_error object
 gg_plt <- plot.gg_partial_list(gg_dta)
 
 # Test return is s ggplot object
 expect_is(gg_plt[[1]], "ggplot")
 
 expect_equivalent(length(gg_plt),length(partial_iris$pData) )
})


test_that("gg_partial survival",{
  
  ## IF we want to build the forest every time...
  #   data(veteran, package = "randomForestSRC")
  #   rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
  #   rfsrc_veteran <- var.select(rfsrc_veteran)
  ## Load the cached forest
  data(rfsrc_veteran, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_veteran, "rfsrc")
  
  ## Get the partial data.
  data(partial_veteran, package="ggRandomForests")
  
  ## Create the correct gg_error object
  gg_dta <- gg_partial(partial_veteran[[1]])
  
  # Test object type
  expect_is(gg_dta, "gg_partial_list")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_partial(gg_dta[[1]])
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt[[1]], "ggplot")
  
  expect_equivalent(length(gg_plt) , length(gg_dta))
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, panel=TRUE)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
})

test_that("gg_partial regression",{
  
  ## IF we want to build the forest every time...
  #   ## New York air quality measurements
  #   rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
  #   rfsrc_airq <- var.select(rfsrc_airq)
  ## Load the cached forest
  data(rfsrc_airq, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_airq, "rfsrc")
  
  ## Create the correct gg_error object
  data(partial_airq, package="ggRandomForests")
  gg_dta<- gg_partial(partial_airq)
  
  # Test object type
  expect_is(gg_dta, "gg_partial_list")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_partial(gg_dta[[1]])
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_partial_list(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "list")
  
  expect_equivalent(length(gg_plt) , length(gg_dta))
})