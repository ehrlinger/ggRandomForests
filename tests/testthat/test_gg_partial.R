# testthat for gg_partial function
context("gg_partial tests")

test_that("gg_partial classifications",{
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
  gg_dta <- gg_partial(partial_iris)
  
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


test_that("gg_partial regression",{
  ## Load the cached forest
  data(rfsrc_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  ## Create the correct gg_error object
  data(partial_Boston, package="ggRandomForests")
  gg_dta <- gg_partial(partial_Boston)
  
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
  
  # gg_partial exceptions
  expect_error(gg_partial(gg_plt))
  
  # Remove all but one partial data.
  partial_Boston$xvar.names <- "lstat"
  partial_Boston$nvar <- 1
  for(ind in length(partial_Boston$pData):2){
    partial_Boston$pData[[ind]] <- NULL
  }
  gg_dta <- gg_partial(partial_Boston)
  
  # Test object type
  expect_is(gg_dta, "gg_partial")
  
  # generate a list of gg_partial objects, one per xvar.
  expect_error(gg_p <- gg_partial(gg_dta), "gg_partial")
  
  expect_is(plot(gg_dta, error="bars"), "ggplot")
  expect_is(plot(gg_dta, error="none"), "ggplot")
  expect_is(plot(gg_dta, error="lines"), "ggplot")
  expect_is(plot(gg_dta, error="shade"), "ggplot")
  # Test object type
  
  expect_is(plot(gg_dta), "ggplot")
  gg_plt <- plot(gg_dta, error="shade")+ geom_smooth(se=.95)
  
})
