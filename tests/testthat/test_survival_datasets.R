# testthat for gg_error function
context("survival functions tests")

test_that("survival_functions_tests: PBC", {
  testthat::skip_on_cran()
  #========
  # Format the PBC data
  #
  dta <- new.env()
  data(pbc, package = "randomForestSRC",
       envir = dta)
  pbc <- dta$pbc
  # For whatever reason, the age variable is in days... makes no sense to me
  for (ind in seq_len(dim(pbc)[2])) {
    if (!is.factor(pbc[, ind])) {
      if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
        if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
          pbc[, ind] <- as.logical(pbc[, ind])
        }
      }
    } else {
      if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
        if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
          pbc[, ind] <- as.logical(pbc[, ind])
        }
        if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
          pbc[, ind] <- as.logical(pbc[, ind])
        }
      }
    }
    if (!is.logical(pbc[, ind]) &
        length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
      pbc[, ind] <- factor(pbc[, ind])
    }
  }
  # Convert age to years
  pbc$age <- pbc$age / 364.24
  
  pbc$years <- pbc$days / 364.24
  pbc <- pbc[, -which(colnames(pbc) == "days")]
  pbc$treatment <- as.numeric(pbc$treatment)
  pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
  pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
  pbc$treatment <- factor(pbc$treatment)
  
  cat("pbc: rfsrc\n")
  dta_train <- pbc[-which(is.na(pbc$treatment)), ]
  # Create a test set from the remaining patients
  pbc_test <- pbc[which(is.na(pbc$treatment)), ]
  
  #========
  # build the forest:
  rfsrc_pbc <- randomForestSRC::rfsrc(
    Surv(years, status) ~ .,
    dta_train,
    nsplit = 10,
    na.action = "na.impute",
    forest = TRUE,
    importance = TRUE,
    save.memory = TRUE
  )
  
  #========
  # Test cached forest type
  testthat::expect_is(rfsrc_pbc, "rfsrc")
  
  # Test the forest family
  testthat::expect_match(rfsrc_pbc$family, "surv")
  
  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_pbc)
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_rfsrc")
  testthat::expect_is(gg_dta, "surv")
  
  # Test classification dimensions
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  ## Create the correct gg_error object
  gg_dta <- gg_rfsrc(rfsrc_pbc, oob = FALSE)
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_rfsrc")
  testthat::expect_is(gg_dta, "surv")
  
  # Test classification dimensions
  ## Test plotting the gg_error object
  gg_plt <- plot.gg_rfsrc(gg_dta, alpha = .4)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  # Test classification dimensions
  
  gg_dta <- gg_rfsrc(rfsrc_pbc, by = "treatment")
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_rfsrc")
  testthat::expect_is(gg_dta, "surv")
  
  ## Create the correct gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int = .68)
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_rfsrc")
  testthat::expect_is(gg_dta, "surv")
  
  # Test multiple conf intervals
  gg_dta <- gg_rfsrc(rfsrc_pbc,
                     conf.int = c(.025, .975),
                     bs.sample = 100)
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_rfsrc")
  testthat::expect_is(gg_dta, "surv")
  
  ## Create the correct gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  #========
  # Test prediction
  # Predict survival for 106 patients not in randomized trial
  rfsrc_pbc_test <- predict(rfsrc_pbc,
                            newdata = pbc_test,
                            na.action = "na.impute")
  
  # Print prediction summary
  testthat::expect_is(gg_dta <- gg_rfsrc(rfsrc_pbc_test), "gg_rfsrc")
  
  # Test for group "by" name exists
  testthat::expect_error(gg_rfsrc(rfsrc_pbc, by = "trt"))
  # And it's a vector or factor (not a number)
  testthat::expect_error(gg_rfsrc(rfsrc_pbc, by = 3))
  
  #=========
  ## Create the correct gg_error object
  gg_dta <- gg_error(rfsrc_pbc)
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_error")
  
  # Test classification dimensions
  testthat::expect_equal(dim(gg_dta)[1], length(na.omit(rfsrc_pbc$err.rate)))
  testthat::expect_equal(dim(gg_dta)[2], 2)
  
  # Test data is correctly pulled from randomForest obect.
  tmp <- c(gg_dta[, 1])
  testthat::expect_equivalent(tmp, na.omit(rfsrc_pbc$err.rate))
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  testthat::expect_error(gg_error(gg_plt))
  
  ##================
  ## ROC plots should error for survival family
  testthat::expect_error(gg_roc(rfsrc_pbc))
  
  ##================
  ## variable plots
  
  ## Create the correct gg_error object
  gg_dta <- gg_variable(rfsrc_pbc, time = .25)
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_variable")
  
  ## Test plotting the gg_variable object
  gg_plt <- plot.gg_variable(gg_dta, xvar = "age")
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  
  ## Test plotting the gg_variable object
  gg_plt <- plot.gg_variable(gg_dta, xvar = rfsrc_pbc$xvar.names)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "list")
  testthat::expect_equal(length(gg_plt), length(rfsrc_pbc$xvar.names))
  
  
  for (ind in seq_len(length(rfsrc_pbc$xvar.names)))
    testthat::expect_is(gg_plt[[ind]], "ggplot")
  
  
  ## Test plotting the gg_error object
  testthat::expect_warning(gg_plt <- plot.gg_variable(gg_dta,
                                            xvar = rfsrc_pbc$xvar.names,
                                            panel = TRUE))
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  ##================
  ## Interaction plots
  interaction_pbc <- randomForestSRC::find.interaction(rfsrc_pbc)
  
  # Test the cached interaction structure
  testthat::expect_is(interaction_pbc, "matrix")
  
  ## Create the correct gg_interaction object
  gg_dta <- gg_interaction(interaction_pbc)
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_interaction")
  
  # Test classification dimensions
  testthat::expect_equal(dim(gg_dta), dim(interaction_pbc))
  
  # Test data is correctly pulled from randomForest obect.
  testthat::expect_equivalent(as.matrix(gg_dta), interaction_pbc)
  
  ## Test plotting the gg_interaction object
  gg_plt <- plot.gg_interaction(gg_dta, xvar = "bili")
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  # "Incorrect object type: Expects a gg_interaction object"
  
  labels <- c(
    "event indicator (F = censor, T = death)",
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
    "survival time (years)"
  )
  
  dta_labs <-
    data.frame(cbind(names = colnames(pbc), label = labels))
  
  st_labs <- as.character(dta_labs$label)
  names(st_labs) <- rownames(dta_labs)
  gg_plt <- plot.gg_interaction(gg_dta, lbls = st_labs)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  ##================
  # Calculate the partial dependence
  cat("pbc: RF partial plots\n(this will take a little while...)\n")
  #Really want the vars by name...
  xvar <-
    c("bili", "albumin", "copper", "prothrombin", "age", "edema")
  
  cat("pbc: xvar: ", xvar)
  partial_pbc <- lapply(c(1, 3, 5), function(tm) {
    randomForestSRC::plot.variable(
      rfsrc_pbc,
      surv.type = "surv",
      time = tm,
      sorted = FALSE,
      xvar.names = xvar,
      partial = TRUE,
      show.plots = FALSE
    )
    
  })
  
  ## "pbc: RF partial coplots\n(this will take a little while...)\n")
  ## "pbc: bili/albumin\n")
  ggvar <- gg_variable(rfsrc_pbc, time = 1)
  albumin_cts <- quantile_pts(ggvar$albumin, groups = 6,
                              intervals = TRUE)
  albumin_grp <- cut(ggvar$albumin, breaks = albumin_cts)
  
  partial_coplot_pbc <- gg_partial_coplot(
    rfsrc_pbc,
    xvar = "bili",
    groups = albumin_grp,
    surv_type = "surv",
    time = 1,
    show.plots = FALSE
  )
  
  # Find intervals with similar number of observations.
  bili_cts <- quantile_pts(ggvar$bili, groups = 6, intervals = TRUE)
  
  # We need to move the minimal value so we include that observation
  bili_cts[1] <- bili_cts[1] - 1.e-7
  
  # Create the conditional groups and add to the gg_variable object
  bili_grp <- cut(ggvar$bili, breaks = bili_cts)
  
  
  partial_coplot_pbc2 <-
    gg_partial_coplot(
      rfsrc_pbc,
      xvar = "albumin",
      groups = bili_grp,
      surv_type = "surv",
      time = 1,
      show.plots = FALSE
    )
  
  
  ## Create the correct gg_error object
  gg_dta <- gg_partial(partial_pbc[[1]])
  
  # Test object type
  testthat::expect_is(gg_dta, "gg_partial_list")
  
  ## Test plotting the gg_data object
  gg_plt <- plot(gg_dta[[1]])
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt[[1]], "ggplot")
  
  testthat::expect_equivalent(length(gg_plt), length(gg_dta))
  
  ## Test plotting the gg_error object
  gg_plt <- plot(gg_dta, panel = TRUE)
  
  # Test return is s ggplot object
  testthat::expect_is(gg_plt, "ggplot")
  
  # Data generation
  ggrf <- gg_variable(rfsrc_pbc,
                      time = c(1, 3),
                      time.labels = c("1 Year", "3 Years"))
  
  # Plot the bilirubin variable dependence plot
  gg_plt <- plot(ggrf, xvar = "bili", alpha = 0.3)
  
  gg_plt <- gg_plt + ggplot2::geom_smooth(se = 0.95)
  
  
  xvar <- rfsrc_pbc$xvar.names
  xvar_vcat <- c("edema", "stage", "ascites")
  xvar <- xvar[-which(xvar %in% xvar_vcat)]
  
  # plot the next 5 continuous variable dependence plots.
  gg_plt <- plot(ggrf, xvar = xvar[2:6], panel = TRUE)
  
  gg_plt <- gg_plt + ggplot2::geom_smooth(
    se = FALSE,
    alpha = .3,
    method = "glm",
    formula = y ~ poly(x, 2)
  )
  
  testthat::expect_warning(gg_plt <- plot(ggrf, xvar = xvar_vcat, panel = TRUE))
  
  
  # A list of 2 plot.variable objects
  testthat::expect_is(partial_pbc, "list")
  testthat::expect_gt(length(partial_pbc), 1)
  
  for (ind in seq_len(length(partial_pbc))) {
    testthat::expect_is(partial_pbc[[ind]], "rfsrc")
    testthat::expect_is(partial_pbc[[ind]], "plot.variable")
    testthat::expect_is(partial_pbc[[ind]], "surv")
  }
  
  # Create gg_partial objects
  gg_prtl <- lapply(partial_pbc, gg_partial)
  for (ind in seq_len(length(partial_pbc))) {
    testthat::expect_is(gg_prtl[[ind]], "gg_partial_list")
  }
  
  # Combine the objects to get multiple time curves
  # along variables on a single figure.
  ggpart <- combine.gg_partial(gg_prtl[[1]], gg_prtl[[2]],
                               lbls = c("30 day", "6 month"))
  testthat::expect_is(ggpart, "gg_partial_list")
  
  # We should have at least 5
  testthat::expect_gt(length(ggpart), 5)
  
  # Plot each figure separately
  gg_plt <- plot(ggpart)
  testthat::expect_is(gg_plt, "list")
  testthat::expect_gt(length(gg_plt), 5)
  testthat::expect_equal(length(gg_plt), length(ggpart))
  
  for (ind in seq_len(length(gg_plt))) {
    testthat::expect_is(gg_plt[[ind]], "ggplot")
  }
  
  # Get the continuous data for a panel of continuous plots.
  ggcont <- ggpart
  
  ggcont$celltype <- ggcont$trt <- ggcont$prior <- NULL
  testthat::expect_gt(length(ggcont), 5 - 3)
  
  gg_plt <- plot(ggcont, panel = TRUE)
  testthat::expect_is(gg_plt, "ggplot")
  # And the categorical for a panel of categorical plots.
  ggpart$karno <- ggpart$diagtime <- ggpart$age <- NULL
  testthat::expect_gt(length(ggpart), 5 - 3)
  
  gg_plt <- plot(ggpart, panel = TRUE)
  testthat::expect_is(gg_plt, "ggplot")
  
  # Test coverage, auto labels
  ggpart <- combine.gg_partial(gg_prtl[[1]], gg_prtl[[2]])
  testthat::expect_is(ggpart, "gg_partial_list")
  
  testthat::expect_error(combine.gg_partial(gg_prtl))
  testthat::expect_error(combine.gg_partial(gg_prtl, gg_prtl))
  
})
