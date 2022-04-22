# testthat for gg_interaction function
context("gg_interaction tests")

test_that("gg_interaction classifications",{
  data(iris, package="datasets")
  rfsrc_iris <- randomForestSRC::rfsrc(Species ~., 
                                       data = iris, 
                                       importance=TRUE, tree.err=TRUE)
  ## Load the cached forest
  interaction_iris <- randomForestSRC::find.interaction(rfsrc_iris)
  
  # Test the cached interaction structure
  expect_is(interaction_iris, "matrix")
  
  ## Create the correct gg_interaction object
  gg_dta <- gg_interaction(interaction_iris)
  
  # Test object type
  expect_is(gg_dta, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta), dim(interaction_iris))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta), interaction_iris)
  
  ## Test plotting the gg_interaction object
  gg_plt <- plot.gg_interaction(gg_dta, xvar="Petal.Width")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # This one should fail with a variable not found message
  expect_error(plot.gg_interaction(gg_dta, xvar="Petal"))
  
  # "Incorrect object type: Expects a gg_interaction object"
  ## Test plotting the gg_interaction object
  gg_plt <- plot.gg_interaction(gg_dta)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
})


test_that("gg_interaction survival",{
  
  pbc <- pbc_data()
  dta.train <- pbc[-which(is.na(pbc$treatment)),]
  # Create a test set from the remaining patients
  pbc.test <- pbc[which(is.na(pbc$treatment)),]
  
  rfsrc_pbc <- randomForestSRC::rfsrc(Surv(years, status) ~ ., 
                                      dta.train, nsplit = 10,
                                      na.action="na.impute",
                                      importance=TRUE, tree.err=TRUE)
  ## Load the cached forest
  interaction_pbc <- randomForestSRC::find.interaction(rfsrc_pbc)
  
  # Test the cached interaction structure
  expect_is(interaction_pbc, "matrix")
  
  ## Create the correct gg_interaction object
  gg_dta <- gg_interaction(interaction_pbc)
  
  # Test object type
  expect_is(gg_dta, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta), dim(interaction_pbc))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta), interaction_pbc)
  
  ## Test plotting the gg_interaction object
  gg_plt <- plot.gg_interaction(gg_dta, xvar="bili")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # "Incorrect object type: Expects a gg_interaction object"
  
  labels <- c("event indicator (F = censor, T = death)", 
              "Treament (DPCA, Placebo)", 
              "age in years", 
              "Female", 
              "Asictes", 
              "Hepatomegaly", 
              "Spiders", 
              "Edema", 
              "serum bilirubin (mg/dl)", 
              "serum cholesterol (mg/dl)", 
              "albumin (gm/dl)", 
              "urine copper (ug/day)", 
              "alkaline phosphatase (U/liter)", 
              "SGOT (U/ml)", 
              "triglicerides (mg/dl)", 
              "platelets per cubic ml/1000", 
              "prothrombin time (sec)", 
              "histologic stage", 
              "survival time (years)")
  
  dta.labs <- data.frame(cbind(names = colnames(pbc), label = labels))
  
  st.labs <- as.character(dta.labs$label)
  names(st.labs) <- rownames(dta.labs)
  gg_plt <- plot.gg_interaction(gg_dta, lbls=st.labs)
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
})

test_that("gg_interaction regression",{
  data(Boston, package="MASS")
  
  Boston$chas <- as.logical(Boston$chas)
  
  rf_Boston <- randomForestSRC::rfsrc(medv~., data=Boston)
  interaction_Boston <- randomForestSRC::find.interaction(rf_Boston)
  # Test the cached interaction structure
  expect_is(interaction_Boston, "matrix")
  
  ## Create the correct gg_interaction object
  gg_dta <- gg_interaction(interaction_Boston)
  
  # Test object type
  expect_is(gg_dta, "gg_interaction")
  
  # Test classification dimensions
  expect_equal(dim(gg_dta), dim(interaction_Boston))
  
  # Test data is correctly pulled from randomForest obect.
  expect_equivalent(as.matrix(gg_dta), interaction_Boston)
  
  ## Test plotting the gg_interaction object
  gg_plt <- plot.gg_interaction(gg_dta, xvar = "rm")
  
  # Test return is s ggplot object
  expect_is(gg_plt, "ggplot")
  
  # We really only want to run this one when we're developing 
  # data(rfsrc_Boston, package="ggRandomForests")
  #  
  #  expect_warning(gg_dta <- gg_interaction(rfsrc_Boston,
  #                                          xvar.names=rfsrc_Boston$xvar.names[1:2]))
  #  
  #  expect_error(gg_interaction(gg_dta))
  # 

  
  # This one costs a lot of time in calculating the interaction matrix.
  #   ## Create the correct gg_interaction object
  #   expect_warning(gg_dta <- gg_interaction(rfsrc_Boston))
  #   
  #   # Test object type
  #   expect_is(gg_dta, "gg_interaction")
  #   
 
  # Test the cached interaction structure
  expect_is(interaction_Boston, "matrix")
  
  interaction_Boston <- interaction_Boston[-2,]
  expect_error(gg_interaction(interaction_Boston))
})
