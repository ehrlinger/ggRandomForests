####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Recreate the cached data sets for the ggRandomForests package
#'
#' @param set Defaults to all sets (NA), however for individual sets

#' specify one or more of c("boston", "iris")
#'
#' @param save Defaults to write files to the current data directory.
#' @param pth the directory to store files.
#' @param ... extra arguments passed to randomForestSRC functions.
#'
#' @details
#' Constructing random forests are computationally expensive, and the
#' \code{ggRandomForests} operates directly on \code{randomForestSRC} objects.
#' We cache computationally intensive \code{randomForestSRC} objects to improve
#' the \code{ggRandomForests} examples, diagnostics and vignettes run times. The
#' set of precompiled \code{randomForestSRC} objects are stored in the package
#' data subfolder, however version changes in the dependent packages may break
#' some functionality. This function was created to help the package developer
#' deal with those changes. We make the function available to end users to
#' create objects for further experimentation.
#'
#' There are five cached data set types:
#' '\itemize{
#' \item \code{\link{rfsrc_data}} - \code{\link[randomForestSRC]{rfsrc}}
#' objects.
#' \item \code{\link{varsel_data}} - \code{\link[randomForestSRC]{var.select}}
#' minimal depth variable selection objects.
#' \item \code{\link{interaction_data}} -
#' \code{\link[randomForestSRC]{find.interaction}} minimal depth, pairwise
#' variable interaction matrices.
#' \item \code{\link{partial_data}} -
#' \code{\link[randomForestSRC]{plot.variable}} objects
#' (\code{partial=TRUE}) for partial variable dependence.
#' \item \code{\link{partial_coplot_data}} -
#' \code{\link[randomForestSRC]{plot.variable}} objects
#' (\code{partial=TRUE}) for partial variable dependence.
#' }
#'
#' For the following data sets:
#' #'\itemize{
#' \item \code{_iris} - The \code{iris} data set.
#' \item \code{_boston} - The \code{boston} housing data set
#' (\code{MASS} package).
#' }
#'
#' @seealso \code{iris} \code{airq} \code{mtcars} \code{\link[MASS]{Boston}}
#' \code{\link[randomForestSRC]{pbc}}
#' \code{\link[randomForestSRC]{veteran}}
#' \code{\link{rfsrc_data}}
#' \code{\link{varsel_data}}
#' \code{\link{interaction_data}}
#' \code{\link{partial_data}}
#' \code{\link{partial_coplot_data}}
#'
#' @importFrom randomForestSRC rfsrc var.select plot.variable find.interaction
#' @importFrom randomForest randomForest
#' @importFrom utils data
#' @export
cache_rfsrc_datasets <- function(set = NA, save = TRUE, pth, ...) {
  dta <- new.env()
  
  # If we're testing this, we don't want to run the rfsrc codes.
  test <- FALSE
  arg_list <- list(...)
  
  if (!is.null(arg_list$test)) {
    test <- arg_list$test
    if (arg_list$test)
      save <- FALSE
  }
  
  if (missing(pth)) {
    pth <- if (file.exists("data")) {
      if (file.info("data")$isdir) {
        "data/"
      } else{
        "./"
      }
    } else{
      "./"
    }
  } else if (!file.info(pth)$isdir) {
    stop("Provided path does not exist, or is not a directory.")
  }
  
  if (length(set) == 1)
    if (is.na(set))
      set <- c("boston", "iris")
  
  ##---------------------------------------------------------------------------
  if ("iris" %in% set) {
    data(iris, package = "datasets",
         envir = dta)
    iris <- dta$iris
    cat("iris: rfsrc\n")
    if (!test)
      rfsrc_iris <- randomForestSRC::rfsrc(
        Species ~ .,
        data = iris,
        forest = TRUE,
        importance = TRUE,
        save.memory = TRUE,
        ...
      )
    if (save)
      save(
        rfsrc_iris,
        file = paste(pth, "rfsrc_iris.rda", sep = ""),
        compress = "xz"
      )
    
    cat("\niris: RF minimal depth\n")
    if (!test)
      varsel_iris <- randomForestSRC::var.select(rfsrc_iris)
    if (save)
      save(
        varsel_iris,
        file = paste(pth, "varsel_iris.rda", sep = ""),
        compress = "xz"
      )
    
    cat("iris: RF interactions\n")
    if (!test)
      interaction_iris <-
      randomForestSRC::find.interaction(rfsrc_iris)
    if (save)
      save(
        interaction_iris,
        file = paste(pth, "interaction_iris.rda", sep = ""),
        compress = "xz"
      )
    
    cat("iris: RF partial dependence\n")
    if (!test)
      partial_iris <- randomForestSRC::plot.variable(rfsrc_iris,
                                                     partial = TRUE,
                                                     show.plots =
                                                       FALSE)
    if (save)
      save(
        partial_iris,
        file = paste(pth, "partial_iris.rda", sep = ""),
        compress = "xz"
      )
  }
  
  ##---------------------------------------------------------------------------
  if ("boston" %in% set) {
    data(Boston, package = "MASS", envir = dta)
    boston <- dta$Boston
    
    boston$chas <- as.logical(boston$chas)
    
    cat("boston: rfsrc\n")
    if (!test)
      rfsrc_boston <-
      randomForestSRC::rfsrc(
        medv ~ .,
        data = boston,
        forest = TRUE,
        importance = TRUE,
        tree.err = TRUE,
        save.memory = TRUE,
        ...
      )
    else{
      data(rfsrc_boston, package = "ggRandomForests",
           envir = dta)
      rfsrc_boston <- dta$rfsrc_boston
    }
    if (save)
      save(
        rfsrc_boston,
        file = paste(pth, "rfsrc_boston.rda", sep = ""),
        compress = "xz"
      )
    
    cat("\nboston: RF minimal depth\n")
    if (!test)
      varsel_boston <- randomForestSRC::var.select(rfsrc_boston)
    if (save)
      save(
        varsel_boston,
        file = paste(pth, "varsel_boston.rda", sep = ""),
        compress = "xz"
      )
    
    cat("boston: RF interactions\n")
    if (!test)
      interaction_boston <-
      randomForestSRC::find.interaction(rfsrc_boston)
    if (save)
      save(
        interaction_boston,
        file = paste(pth, "interaction_boston.rda", sep = ""),
        compress = "xz"
      )
    
    cat("boston: RF partial dependence\n(this will take a little while...)\n")
    if (!test)
      partial_boston <- randomForestSRC::plot.variable(
        rfsrc_boston,
        xvar.names =
          varsel_boston$topvars,
        sorted =
          FALSE,
        partial =
          TRUE,
        show.plots =
          FALSE
      )
    if (save)
      save(
        partial_boston,
        file = paste(pth, "partial_boston.rda", sep = ""),
        compress = "xz"
      )
    
    cat(
      "\nboston: RF partial coplots\n\tlstat by rm groups
      \n(this will take a little longer...)\n"
    )
    rm_pts <-
      quantile_pts(rfsrc_boston$xvar$rm,
                   groups = 6,
                   intervals = TRUE)
    rm_grp <- cut(rfsrc_boston$xvar$rm, breaks = rm_pts)
    if (!test)
      partial_coplot_boston <- gg_partial_coplot(
        rfsrc_boston,
        xvar = "lstat",
        groups = rm_grp,
        show.plots = FALSE
      )
    
    if (save)
      save(
        partial_coplot_boston,
        file = paste(pth, "partial_coplot_boston.rda", sep = ""),
        compress = "xz"
      )
    
    cat("\nboston: RF partial coplots\n\trm by lstat groups
        \n(so will this...)\n")
    lstat_pts <- quantile_pts(rfsrc_boston$xvar$lstat,
                              groups = 6,
                              intervals = TRUE)
    lstat_grp <- cut(rfsrc_boston$xvar$lstat, breaks = lstat_pts)
    if (!test)
      partial_coplot_boston2 <- gg_partial_coplot(
        rfsrc_boston,
        xvar = "rm",
        groups = lstat_grp,
        show.plots = FALSE
      )
    
    if (save)
      save(
        partial_coplot_boston2,
        file = paste(pth, "partial_coplot_boston2.rda", sep =
                       ""),
        compress = "xz"
      )
    cat("\nboston: RF partial surface plot\n(Go get a coffee...)\n")
    if (!test) {
      rm_pts <-
        quantile_pts(rfsrc_boston$xvar$rm,
                     groups = 49,
                     intervals = TRUE)
      partial_boston_surf <- lapply(rm_pts, function(ct) {
        rfsrc_boston$xvar$rm <- ct
        randomForestSRC::plot.variable(
          rfsrc_boston,
          xvar.names = "lstat",
          time = 1,
          npts = 50,
          show.plots = FALSE,
          partial = TRUE
        )
      })
    }
    
    if (save)
      save(
        partial_boston_surf,
        file = paste(pth, "partial_boston_surf.rda", sep = ""),
        compress = "xz"
      )
    
  }
  
  ##---------------------------------------------------------------------------
  ## Survival Models
  ##---------------------------------------------------------------------------
  if ("pbc" %in% set) {
    data(pbc, package = "randomForestSRC",
         envir = dta)
    pbc <- dta$pbc
    # For whatever reason, the age variable is in days... makes no sense to me
    for (ind in 1:dim(pbc)[2]) {
      if (!is.factor(pbc[, ind])) {
        if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
          if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
            pbc[, ind] <- as.logical(pbc[, ind])
          }
        }
      } else{
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
    pbc <- pbc[,-which(colnames(pbc) == "days")]
    pbc$treatment <- as.numeric(pbc$treatment)
    pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
    pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
    pbc$treatment <- factor(pbc$treatment)
    
    cat("pbc: rfsrc\n")
    dta_train <- pbc[-which(is.na(pbc$treatment)),]
    # Create a test set from the remaining patients
    pbc_test <- pbc[which(is.na(pbc$treatment)),]
    
    rfsrc_pbc <- randomForestSRC::rfsrc(
      Surv(years, status) ~ .,
      dta_train,
      nsplit = 10,
      na.action = "na.impute",
      forest = TRUE,
      importance = TRUE,
      save.memory = TRUE,
    )
    
    cat("pbc: rfsrc predict\n")
    
    rfsrc_pbc_test <- predict(rfsrc_pbc,
                              newdata = pbc_test,
                              na.action = "na.impute")
    # Print prediction summary
    rfsrc_pbc_test
    
    cat("pbc: RF minimal depth\n")
    varsel_pbc <- randomForestSRC::var.select(rfsrc_pbc)
    
    cat("pbc: RF interactions\n")
    interaction_pbc <-
      randomForestSRC::find.interaction(rfsrc_pbc)
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
    
    
    cat("pbc: RF partial coplots\n(this will take a little while...)\n")
    cat("pbc: bili/albumin\n")
    ggvar <- gg_variable(rfsrc_pbc, time = 1)
    albumin_cts <- quantile_pts(ggvar$albumin, groups = 6,
                                intervals = TRUE)
    albumin_grp <- cut(ggvar$albumin, breaks = albumin_cts)
    
    partial_coplot_pbc <-
      gg_partial_coplot(
        rfsrc_pbc,
        xvar = "bili",
        groups = albumin_grp,
        surv_type = "surv",
        time = 1,
        show.plots = FALSE
      )
    
    
    cat("pbc: albumin/bili\n")
    # Find intervals with similar number of observations.
    bili_cts <-
      quantile_pts(ggvar$bili, groups = 6, intervals = TRUE)
    
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
    
    
    # Restrict the time of interest to less than 5 years.
    time_pts <-
      rfsrc_pbc$time.interest[which(rfsrc_pbc$time.interest <= 5)]
    
    # Find the 50 points in time, evenly space along the distribution of
    # event times for a series of partial dependence curves
    time_cts <- quantile_pts(time_pts, groups = 50)
    
    # Generate the gg_partial_coplot data object
    partial_pbc_time <- lapply(time_cts, function(ct) {
      randomForestSRC::plot.variable(
        rfsrc_pbc,
        xvar.names = "bili",
        time = ct,
        npts = 50,
        show.plots = FALSE,
        partial = TRUE,
        surv.type = "surv"
      )
    })
    
    # Find the quantile points to create 50 cut points
    alb_partial_pts <-
      quantile_pts(rfsrc_pbc$xvar$albumin, groups = 50)
    
    partial_pbc_surf <- lapply(alb_partial_pts, function(ct) {
      rfsrc_pbc$xvar$albumin <- ct
      randomForestSRC::plot.variable(
        rfsrc_pbc,
        xvar.names = "bili",
        time = 1,
        npts = 50,
        show.plots = FALSE,
        partial = TRUE,
        surv.type = "surv"
      )
    })
  }
}