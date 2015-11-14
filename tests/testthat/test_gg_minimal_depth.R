# testthat for gg_minimal_depth function
context("gg_minimal_depth tests")

test_that("gg_minimal_depth classifications",{
  ## Load the cached forest
  data(varsel_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_iris, "list")
  
  # Test the forest family
  expect_false(is.null(varsel_iris$md.obj))
  
  ## Create the correct gg_error object
  gg_dta <- gg_minimal_depth(varsel_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_minimal_depth")
  
  # Test varselect is the same
  expect_equivalent(gg_dta$varselect[,-which(colnames(gg_dta$varselect) == "names")],
                    varsel_iris$varselect)
#   
#   expect_is(gg_dta$threshold - mean(gg_dta$varselect$depth) < 1.e-6 )
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  expect_error(gg_minimal_depth(gg_plt))
  expect_error(gg_minimal_depth.rfsrc(gg_plt))
})


test_that("gg_minimal_depth survival",{
  ## Load the cached forest
  data(varsel_pbc, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_pbc, "list")
  
  ## Create the correct gg_error object
  gg_dta <- gg_minimal_depth(varsel_pbc)
  
  # Test object type
  expect_is(gg_dta, "gg_minimal_depth")
  
  
  # Test varselect is the same
  expect_equivalent(gg_dta$varselect[, -which(colnames(gg_dta$varselect) == "names")],
                    varsel_pbc$varselect)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_minimal_depth(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  gg_plt <- plot(gg_dta, nvar=12)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
})

test_that("gg_minimal_depth regression",{
  ## Load the cached forest
  data(varsel_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_Boston, "list")
  
  ## Create the correct gg_error object
  gg_dta <- gg_minimal_depth(varsel_Boston)
  
  # Test object type
  expect_is(gg_dta, "gg_minimal_depth")
  
  # Test varselect is the same
  expect_equivalent(gg_dta$varselect[, -which(colnames(gg_dta$varselect) == "names")], 
                    varsel_Boston$varselect)
  
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_minimal_depth(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Create the correct gg_minimal_depth object
  gg_dta <- gg_minimal_depth(varsel_Boston)
  # Test object type
  expect_is(gg_dta, "gg_minimal_depth")
  
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
  gg_plt <- plot.gg_minimal_depth(gg_dta, lbls=st.labs, selection=TRUE)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot") 
})

test_that("gg_minimal_depth exceptions",{
  data(varsel_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(varsel_Boston, "list")
  
  vsel <- varsel_Boston
  vsel$varselect <- NULL
  
  expect_error(gg_minimal_depth(vsel))
  
  vsel$threshold <- NULL
  expect_error(gg_minimal_depth(vsel))
  
  expect_output(print(gg_minimal_depth(varsel_Boston)), 
                "gg_minimal_depth", ignore.case = TRUE)
  
  
  vsel <- varsel_Boston
  vsel$varselect$vimp <- NULL
  gg_dta <- gg_minimal_depth(vsel)
  
  expect_is(gg_dta, "gg_minimal_depth")
  
  expect_is(plot(gg_dta, type="rank"), "ggplot")
  
  # data(rfsrc_Boston, package="ggRandomForests")
  # expect_output(gg_dta <- gg_minimal_depth(rfsrc_Boston, fast=TRUE),
  #               "minimal depth variable selection")
  # expect_is(gg_dta, "gg_minimal_depth")
  # 
  # expect_output(gg_plt <- plot.gg_minimal_depth(rfsrc_Boston, fast=TRUE),
  #               "minimal depth variable selection")
  # expect_error(gg_minimal_depth(gg_plt))
  
})