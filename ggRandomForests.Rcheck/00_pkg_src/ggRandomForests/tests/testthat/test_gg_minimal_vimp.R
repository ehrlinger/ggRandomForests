# testthat for gg_minimal_vimp function
context("gg_minimal_vimp tests")

test_that("gg_minimal_vimp classifications",{
  ## Load the cached forest
  data(varsel_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_iris, "list")
  
  # Test the forest family
  expect_false(is.null(varsel_iris$md.obj))
  
  ## Create the correct gg_error object
  ggrf.obj <- gg_minimal_vimp(varsel_iris)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_iris$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})


test_that("gg_minimal_vimp survival",{
  ## Load the cached forest
  data(varsel_pbc, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_pbc, "list")
  
  ## Create the correct gg_error object
  ggrf.obj <- gg_minimal_vimp(varsel_pbc)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_pbc$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
})

test_that("gg_minimal_vimp regression",{
  ## Load the cached forest
  data(varsel_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_Boston, "list")
  
  ## Create the correct gg_error object
  ggrf.obj <- gg_minimal_vimp(varsel_Boston)
  
  # Test object type
  expect_is(ggrf.obj, "gg_minimal_vimp")
  
  
  # Test varselect is the same
  expect_equivalent(dim(ggrf.obj)[1], dim(varsel_Boston$varselect)[1])
  expect_equivalent(dim(ggrf.obj)[2], 4)
  
  ## Test plotting the gg_error object
  gg.obj <- plot.gg_minimal_vimp(ggrf.obj)
  
  # Test return is s ggplot object
  expect_is(gg.obj, "ggplot")
  
  data(Boston, package="MASS")
  
  cls <- sapply(Boston, class) 
  # 
  lbls <- 
    #crim
    c("Crime rate by town.",
      # zn
      "Proportion of residential land zoned for lots over 25,000 sq.ft.",
      # indus
      "Proportion of non-retail business acres per town.",
      # chas
      "Charles River (tract bounds river).",
      # nox
      "Nitrogen oxides concentration (10 ppm).",
      # rm
      "Number of rooms per dwelling.",
      # age
      "Proportion of units built prior to 1940.",
      # dis
      "Distances to Boston employment center.",
      # rad
      "Accessibility to highways.",
      # tax
      "Property-tax rate per $10,000.",
      # ptratio
      "Pupil-teacher ratio by town.",
      # black
      "Proportion of blacks by town.",
      # lstat
      "Lower status of the population (percent).",
      # medv
      "Median value of homes ($1000s).")
  
  # Build a table for data description
  dta.labs <- data.frame(cbind(Variable=names(cls), Description=lbls, type=cls))
  
  # Build a named vector for labeling figures later/
  st.labs <- as.character(dta.labs$Description)
  names(st.labs) <- names(cls)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_minimal_vimp(varsel_Boston, lbls=st.labs)
  expect_is(gg_plt, "ggplot")
  
})


test_that("gg_minimal_vimp exceptions",{
  data(varsel_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_Boston, "list")
  
  vsel <- varsel_Boston
  vsel$varselect <- NULL
  
  expect_error(gg_minimal_vimp(vsel))
  
  vsel$threshold <- NULL
  expect_error(gg_minimal_vimp(vsel))
  
  vsel <- varsel_Boston
  vsel$varselect$vimp <- NULL
  expect_error(gg_minimal_vimp(vsel))
  expect_error(plot.gg_minimal_vimp(vsel))
  
  # data(rfsrc_Boston, package="ggRandomForests")
  # expect_output(gg_dta <- gg_minimal_vimp(rfsrc_Boston, fast=TRUE),
  #               "minimal depth variable selection")
  # expect_is(gg_dta, "gg_minimal_vimp")
  # gg_plt <- plot.gg_minimal_vimp(gg_dta)
  # expect_error(gg_minimal_depth(gg_plt))
  
})