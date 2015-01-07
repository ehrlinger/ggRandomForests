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
  expect_equal(rfsrc_iris$family, "class")
  
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

test_that("gg_partial combine",{
  
  # Load a set of plot.variable partial plot data
  data(partial_veteran)
  
  # A list of 2 plot.variable objects
  expect_is(partial_veteran, "list")
  expect_more_than(length(partial_veteran), 1) 
  
  for(ind in 1:length(partial_veteran)){
    expect_is(partial_veteran[[ind]], "rfsrc")
    expect_is(partial_veteran[[ind]], "plot.variable")
    expect_is(partial_veteran[[ind]], "surv")
  }
  
  # Create gg_partial objects
  ggPrtl <- lapply(partial_veteran, gg_partial)
  for(ind in 1:length(partial_veteran)){
    expect_is(ggPrtl[[ind]], "gg_partial_list")
  }
  
  # Combine the objects to get multiple time curves 
  # along variables on a single figure.
  ggpart <- combine.gg_partial(ggPrtl[[1]], ggPrtl[[2]], 
                               lbls = c("30 day", "6 month"))
  expect_is(ggpart, "gg_partial_list")
  
  # We should have at least 5 
  expect_more_than(length(ggpart), 5)
  
  # Plot each figure separately
  gg_plt <- plot(ggpart)                                  
  expect_is(gg_plt, "list")
  expect_more_than(length(gg_plt), 5)
  expect_equal(length(gg_plt), length(ggpart))
  
  for(ind in 1:length(gg_plt)){
    expect_is(gg_plt[[ind]], "ggplot")
  }
  
  # Get the continuous data for a panel of continuous plots.
  ggcont <- ggpart
  
  ggcont$celltype <- ggcont$trt <- ggcont$prior <- NULL
  expect_more_than(length(ggcont), 5-3)
  
  gg_plt <- plot(ggcont, panel=TRUE) 
  expect_is(gg_plt, "ggplot")
  # And the categorical for a panel of categorical plots.
  ggpart$karno <- ggpart$diagtime <- ggpart$age <- NULL
  expect_more_than(length(ggpart), 5-3)
  
  gg_plt <- plot(ggpart, panel=TRUE) 
  expect_is(gg_plt, "ggplot")
  
  # Test coverage, auto labels
  ggpart <- combine.gg_partial(ggPrtl[[1]], ggPrtl[[2]])
  expect_is(ggpart, "gg_partial_list")
  
  expect_error(combine.gg_partial(ggPrtl))
  expect_error(combine.gg_partial(ggPrtl, ggPrtl))
})
