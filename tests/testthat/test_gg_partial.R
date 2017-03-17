# testthat for gg_partial function
context("gg_partial tests")

test_that("gg_partial classifications",{
  ## Load the cached forest ## Load the cached forest
  data(iris, package="datasets")
  rfsrc_iris <- randomForestSRC::rfsrc(Species ~., 
                                       data = iris, 
                                       importance=TRUE, tree.err=TRUE)
  
  # Test the cached forest type
  expect_is(rfsrc_iris, "rfsrc")
  
  # Test the forest family
  expect_equal(rfsrc_iris$family, "class")
  
  # Load saved partial plot data.
  partial_iris <- randomForestSRC::plot.variable(rfsrc_iris,
                                                 partial=TRUE, show.plots=FALSE)
  
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


test_that("gg_partial survival",{
  
  pbc <- pbc_data()
  dta.train <- pbc[-which(is.na(pbc$treatment)),]
  # Create a test set from the remaining patients
  pbc.test <- pbc[which(is.na(pbc$treatment)),]
  
  rfsrc_pbc <- randomForestSRC::rfsrc(Surv(years, status) ~ ., 
                                      dta.train, nsplit = 10,
                                      na.action="na.impute",
                                      importance=TRUE, tree.err=TRUE)
  # Test the cached forest type
  expect_is(rfsrc_pbc, "rfsrc")
  xvar <- c("bili", "albumin", "copper", "prothrombin", "age", "edema")
  
  partial_pbc <- lapply(c(1, 3, 5), function(tm){
    randomForestSRC::plot.variable(rfsrc_pbc, surv.type = "surv", 
                                   time = tm, sorted = FALSE, 
                                   xvar.names = xvar, partial = TRUE, 
                                   show.plots = FALSE)
  })

  ## Create the correct gg_error object
  gg_dta <- gg_partial(partial_pbc[[1]])
  
  # Test object type
  expect_is(gg_dta, "gg_partial_list")
  
  ## Test plotting the gg_data object
  gg_plt <- plot(gg_dta[[1]])
  
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

  varsel_pbc <- randomForestSRC::var.select(rfsrc_pbc)
  
  # Data generation
  ggrf <- gg_variable(rfsrc_pbc, time = c(1, 3), 
                      time.labels = c("1 Year", "3 Years"))
  
  # Plot the bilirubin variable dependence plot
  gg_plt <- plot(ggrf, xvar = "bili", alpha = .3)
  
  gg_plt <- gg_plt+ geom_smooth(se=.95)
  
  
  xvar <- varsel_pbc$topvars
  xvar.cat <- c("edema", "stage")
  xvar <- xvar[-which(xvar %in% xvar.cat)]
  
  # plot the next 5 continuous variable dependence plots.
  gg_plt <- plot(ggrf, xvar = xvar[2:6], panel = TRUE)
  
  gg_plt <- gg_plt + geom_smooth(se = FALSE, alpha = .3, 
                                 method = "glm", formula = y~poly(x,2))
  
  expect_warning(gg_plt <- plot(ggrf, xvar = xvar.cat, panel=TRUE))
  
  
  
  # A list of 2 plot.variable objects
  expect_is(partial_pbc, "list")
  expect_gt(length(partial_pbc), 1) 
  
  for(ind in 1:length(partial_pbc)){
    expect_is(partial_pbc[[ind]], "rfsrc")
    expect_is(partial_pbc[[ind]], "plot.variable")
    expect_is(partial_pbc[[ind]], "surv")
  }
  
  # Create gg_partial objects
  gg_prtl <- lapply(partial_pbc, gg_partial)
  for(ind in 1:length(partial_pbc)){
    expect_is(gg_prtl[[ind]], "gg_partial_list")
  }
  
  # Combine the objects to get multiple time curves 
  # along variables on a single figure.
  ggpart <- combine.gg_partial(gg_prtl[[1]], gg_prtl[[2]], 
                               lbls = c("30 day", "6 month"))
  expect_is(ggpart, "gg_partial_list")
  
  # We should have at least 5 
  expect_gt(length(ggpart), 5)
  
  # Plot each figure separately
  gg_plt <- plot(ggpart)                                  
  expect_is(gg_plt, "list")
  expect_gt(length(gg_plt), 5)
  expect_equal(length(gg_plt), length(ggpart))
  
  for(ind in 1:length(gg_plt)){
    expect_is(gg_plt[[ind]], "ggplot")
  }
  
  # Get the continuous data for a panel of continuous plots.
  ggcont <- ggpart
  
  ggcont$celltype <- ggcont$trt <- ggcont$prior <- NULL
  expect_gt(length(ggcont), 5 - 3)
  
  gg_plt <- plot(ggcont, panel=TRUE) 
  expect_is(gg_plt, "ggplot")
  # And the categorical for a panel of categorical plots.
  ggpart$karno <- ggpart$diagtime <- ggpart$age <- NULL
  expect_gt(length(ggpart), 5 - 3)
  
  gg_plt <- plot(ggpart, panel=TRUE) 
  expect_is(gg_plt, "ggplot")
  
  # Test coverage, auto labels
  ggpart <- combine.gg_partial(gg_prtl[[1]], gg_prtl[[2]])
  expect_is(ggpart, "gg_partial_list")
  
  expect_error(combine.gg_partial(gg_prtl))
  expect_error(combine.gg_partial(gg_prtl, gg_prtl))
})

test_that("gg_partial regression",{
  ## Load the cached forest
  data(Boston, package="MASS")
  
  Boston$chas <- as.logical(Boston$chas)
  
  rfsrc_Boston <- randomForestSRC::rfsrc(medv~., data=Boston)
  
  # Test the cached forest type
  expect_is(rfsrc_Boston, "rfsrc")
  
  varsel_Boston <- randomForestSRC::var.select(rfsrc_Boston)
  
  partial_Boston <- randomForestSRC::plot.variable(rfsrc_Boston,
                                                   xvar.names=varsel_Boston$topvars,
                                                   sorted=FALSE,
                                                   partial=TRUE, 
                                                   show.plots=FALSE)
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
