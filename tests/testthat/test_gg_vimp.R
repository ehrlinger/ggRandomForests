# testthat for gg_vimp function
context("gg_vimp tests")

test_that("gg_vimp classifications", {  
  ## Load the cached forest
  data(rfsrc_iris, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_equal(rfsrc_iris$family, "class")
  
  ## Create the correct gg_error object
  gg_dta <- gg_vimp(rfsrc_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  
  # Test varselect is the same
  #expect_equivalent(select(gg_dta$varselect, -names), rfsrc_iris$importance)
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Grab only one class... by number.
  gg_dta <- gg_vimp(rfsrc_iris, which.outcome=2)
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  # Grab only one class... by number - for the overall model.
  gg_dta <- gg_vimp(rfsrc_iris, which.outcome=0)
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  # Grab only one class... by name - for the overall model.
  gg_dta <- gg_vimp(rfsrc_iris, which.outcome="all")
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  # Grab only one class... by name - for the overall model.
  gg_dta <- gg_vimp(rfsrc_iris, which.outcome="setosa")
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # Grab only one class... by name - that doesn't exist.
  expect_error(gg_vimp(rfsrc_iris, which.outcome="nothing special"))
  
  # Grab only one class... by number - that doesn't exist.
  expect_error(gg_vimp(rfsrc_iris, which.outcome=200))
  
  ## Single class/
  iris2 <- iris
  iris2$spec <- factor(as.character(iris2$Species) == "setosa")
  iris2 <- iris2[,-which(colnames(iris2) == "Species")]
  
  rf <- rfsrc(spec~., iris2)
  
  gg_dta <- gg_vimp(rf)
  
  expect_is(gg_dta, "gg_vimp")
  
  # Test passing in the wrong object
  expect_error(gg_vimp(gg_dta))
  expect_error(gg_vimp.rfsrc(gg_dta))
  
})


test_that("gg_vimp survival",{
  ## Load the cached forest
  data(rfsrc_pbc, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_pbc, "rfsrc")
  
  ## Create the correct gg_error object
  gg_dta <- gg_vimp(rfsrc_pbc)
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  
  # Test varselect is the same
  expect_equal(gg_dta$vimp, as.vector(sort(rfsrc_pbc$importance, decreasing=TRUE)))
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, nvar=5)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  expect_is(plot(gg_dta, relative=TRUE), "ggplot")
  
  # Test cutting the size down
  expect_is(gg_dta <- gg_vimp(rfsrc_pbc, n_var=10), "gg_vimp")
  expect_equal(nrow(gg_dta), 10)
  expect_is(plot(gg_dta), "ggplot")
  
  # Test the relative vimp output and plotting
  expect_is(gg_dta <- gg_vimp(rfsrc_pbc, relative=TRUE), "gg_vimp")
  expect_is(plot(gg_dta), "ggplot")
  
  expect_is(gg_dta <- gg_vimp(rfsrc_pbc, n_var=10, relative=TRUE), "gg_vimp")
  expect_is(plot(gg_dta), "ggplot")
  
  # Test importance calculations. 
  # If the forest does not have importance
  rfsrc_pbc$importance <- NULL
  expect_warning(gg_dta <- gg_vimp(rfsrc_pbc))
  expect_is(gg_dta, "gg_vimp")
  expect_is(plot(gg_dta), "ggplot")
  
})

test_that("gg_vimp regression",{
  ## Load the cached forest
  data(rfsrc_Boston, package="ggRandomForests")
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  ## Create the correct gg_error object
  gg_dta <- gg_vimp(rfsrc_Boston)
  
  # Test object type
  expect_is(gg_dta, "gg_vimp")
  
  # Test varselect is the same
  expect_equal(gg_dta$vimp, as.vector(sort(rfsrc_Boston$importance, decreasing=TRUE)))
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, relative=TRUE)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
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
  
  ## Test plotting the rfsrc object
  gg_plt <- plot.gg_vimp(rfsrc_Boston, lbls=st.labs, relative=TRUE, 
                         bars=rfsrc_Boston$xvar.names)
  expect_is(gg_plt, "ggplot")
  
  
})