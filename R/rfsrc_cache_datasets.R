#' Recreate the cached data sets for the ggRandomForests package
#' 
#' @param set Defaults to all sets (NA), however for individual sets specify one 
#' or more of c("airq", "Boston", "iris", "mtcars", "pbc", "veteran")
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
#' data subfolder, however version changes in the dependant packages may break
#' some functionality. This function was created to help the package developer 
#' deal with thoses changes. We make the function available to end users to
#' create objects for further experimentation.
#'
#' There are five cached data set types:
#' '\itemize{
#' \item \code{\link{rfsrc_data}} - \code{randomForestSRC::rfsrc} objects. 
#' \item \code{\link{varsel_data}} - \code{randomForestSRC::var.select} minimal depth variable 
#' selection objects.
#' \item \code{\link{interaction_data}} - \code{randomForestSRC::find.interaction} minimal depth, 
#' pairwise variable interaction matrices.
#' \item \code{\link{partial_data}} - \code{randomForestSRC::plot.variable} objects 
#' (\code{partial=TRUE}) for partial variable dependence. 
#' \item \code{\link{partial_coplot_data}} - \code{randomForestSRC::plot.variable} objects 
#' (\code{partial=TRUE}) for partial variable dependence. 
#' }
#' 
#' For the following data sets:
#' #'\itemize{
#' \item \code{_iris} - The \code{iris} data set. 
#' \item \code{_airq} - The \code{airquality} data set.
#' \item \code{_mtcars} - The \code{mtcars} data set.
#' \item \code{_Boston} - The \code{Boston} housing data set (\code{MASS} package).
#' \item \code{_pbc} - The \code{pbc} data set (\code{randomForestSRC} package).  
#' \item \code{_veteran} - The \code{veteran} data set (\code{randomForestSRC} package).  
#' }
#' 
#' @seealso \code{iris} \code{airq} \code{mtcars} \code{MASS::Boston} 
#' \code{randomForestSRC::pbc}
#' \code{randomForestSRC::veteran} 
#' \code{\link{rfsrc_data}} 
#' \code{\link{varsel_data}}
#' \code{\link{interaction_data}}
#' \code{\link{partial_data}} 
#' \code{\link{partial_coplot_data}}
#' 
#' @export rfsrc_cache_datasets
#' @importFrom randomForestSRC rfsrc var.select plot.variable find.interaction
#' @importFrom parallel mclapply

# # @export rf_cache_datasets
# # @aliases rfsrc_cache_datasets rf_cache_datasets

rfsrc_cache_datasets <- function(set=NA, save=TRUE, pth, ...){
  dta <- new.env()
  
  # If we're testing this, we don't want to run the rfsrc codes.
  test <- FALSE
  arg_list <- list(...)
  
  if(!is.null(arg_list$test)){
    test <- arg_list$test
    if(arg_list$test)
      save <- FALSE
  }
  
  if(missing(pth)){
    pth <- if(file.exists("data")){
      if(file.info("data")$isdir){
        "data/"
      }else{
        "./"
      }
    }else{
      "./"
    } 
  }else if(!file.info(pth)$isdir){
    stop("Provided path does not exist, or is not a directory.")
  }
  
  if(is.na(set))
    set <- c("Boston", "iris", "pbc")
  
  if("airq" %in% set){
    cat("airq: randomForest\n")
    if(!test)
      rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute", ...)
    
    if(save) save(rfsrc_airq, file=paste(pth, "rfsrc_airq.rda", sep=""), compress="xz")
    
    cat("\nairq: RF minimal depth\n")
    if(!test) varsel_airq <- var.select(rfsrc_airq)
    
    if(save) save(varsel_airq, file=paste(pth, "varsel_airq.rda", sep=""), compress="xz")
    
    cat("airq: RF interactions\n")
    if(!test) interaction_airq <- find.interaction(rfsrc_airq)
    if(save) save(interaction_airq, file=paste(pth, "interaction_airq.rda", sep=""), compress="xz")
    
    cat("airq: RF partial dependence\n")
    if(!test) partial_airq <- plot.variable(rfsrc_airq,
                                            partial=TRUE, show.plots=FALSE)
    if(save) save(partial_airq, file=paste(pth, "partial_airq.rda", sep=""), compress="xz")
  }
  
  if("iris" %in% set){
    cat("iris: randomForest\n")
    if(!test) rfsrc_iris <- rfsrc(Species ~., data = iris, ...)
    if(save) save(rfsrc_iris, file=paste(pth, "rfsrc_iris.rda", sep=""), compress="xz")
    
    cat("\niris: RF minimal depth\n")
    if(!test)  varsel_iris <- var.select(rfsrc_iris)
    if(save) save(varsel_iris, file=paste(pth, "varsel_iris.rda", sep=""), compress="xz")
    
    cat("iris: RF interactions\n")
    if(!test) interaction_iris <- find.interaction(rfsrc_iris)
    if(save) save(interaction_iris, file=paste(pth, "interaction_iris.rda", sep=""), compress="xz")
    
    cat("iris: RF partial dependence\n")
    if(!test) partial_iris <- plot.variable(rfsrc_iris,
                                            partial=TRUE, show.plots=FALSE)
    if(save) save(partial_iris, file=paste(pth, "partial_iris.rda", sep=""), compress="xz")
  }
  
  
  if("mtcars" %in% set){
    cat("mtcars: randomForest\n")
    if(!test) rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, ...)
    if(save) save(rfsrc_mtcars, file=paste(pth, "rfsrc_mtcars.rda", sep=""), compress="xz")
    
    cat("\nmtcars: RF minimal depth\n")
    if(!test) varsel_mtcars <- var.select(rfsrc_mtcars)
    if(save) save(varsel_mtcars, file=paste(pth, "varsel_mtcars.rda", sep=""), compress="xz")
    
    cat("mtcars: RF interactions\n")
    if(!test) interaction_mtcars <- find.interaction(rfsrc_mtcars)
    if(save) save(interaction_mtcars, file=paste(pth, "interaction_mtcars.rda", sep=""), compress="xz")
    
    cat("mtcars: RF partial dependence\n")
    if(!test) partial_mtcars <- plot.variable(rfsrc_mtcars,
                                              partial=TRUE, show.plots=FALSE)
    if(save) save(partial_mtcars, file=paste(pth, "partial_mtcars.rda", sep=""), compress="xz")
  }
  if("Boston" %in% set){
    data(Boston, package="MASS",
         envir = dta)
    Boston <- dta$Boston
    
    Boston$chas <- as.logical(Boston$chas)
    
    cat("Boston: randomForest\n")
    if(!test) rfsrc_Boston <- rfsrc(medv~., data=Boston, ...)
    else{
      data(rfsrc_Boston, package="ggRandomForests",
           envir = dta)
      rfsrc_Boston <- dta$rfsrc_Boston
    }
    if(save) save(rfsrc_Boston, file=paste(pth, "rfsrc_Boston.rda", sep=""), compress="xz")
    
    cat("\nBoston: RF minimal depth\n")
    if(!test) varsel_Boston <- var.select(rfsrc_Boston)
    if(save) save(varsel_Boston, file=paste(pth, "varsel_Boston.rda", sep=""), compress="xz")
    
    cat("Boston: RF interactions\n")
    if(!test) interaction_Boston <- find.interaction(rfsrc_Boston)
    if(save) save(interaction_Boston, file=paste(pth, "interaction_Boston.rda", sep=""), compress="xz")
    
    cat("Boston: RF partial dependence\n(this will take a little while...)\n")
    if(!test) partial_Boston <- plot.variable(rfsrc_Boston,
                                              xvar.names=varsel_Boston$topvars,
                                              sorted=FALSE,
                                              partial=TRUE, 
                                              show.plots=FALSE)
    if(save) save(partial_Boston, file=paste(pth, "partial_Boston.rda", sep=""), compress="xz")
    
    cat("\nBoston: RF partial coplots\n\tlstat by rm groups\n(this will take a little longer...)\n")
    rm_pts <- quantile_pts(rfsrc_Boston$xvar$rm, groups=6, intervals=TRUE)
    rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)
    if(!test) partial_coplot_Boston <- gg_partial_coplot(rfsrc_Boston, xvar="lstat", 
                                                         groups=rm_grp,
                                                         show.plots=FALSE)
    
    if(save) save(partial_coplot_Boston, 
                  file=paste(pth, "partial_coplot_Boston.rda", sep=""), 
                  compress="xz")
    
    cat("\nBoston: RF partial coplots\n\trm by lstat groups\n(so will this...)\n")
    lstat_pts <- quantile_pts(rfsrc_Boston$xvar$lstat, groups=6, intervals=TRUE)
    lstat_grp <- cut(rfsrc_Boston$xvar$lstat, breaks=lstat_pts)
    if(!test) partial_coplot_Boston2 <- gg_partial_coplot(rfsrc_Boston, xvar="rm", 
                                                          groups=lstat_grp,
                                                          show.plots=FALSE)
    
    if(save) save(partial_coplot_Boston2, 
                  file=paste(pth, "partial_coplot_Boston2.rda", sep=""), 
                  compress="xz")
    
  }
  
  if("pbc" %in% set){
    data(pbc, package="randomForestSRC",
         envir = dta)
    pbc <- dta$pbc
    # For whatever reason, the age variable is in days... makes no sense to me
    for(ind in 1:dim(pbc)[2]){
      if(!is.factor(pbc[,ind])){
        if(length(unique(pbc[which(!is.na(pbc[,ind])),ind]))<=2) {
          if(sum(range(pbc[,ind],na.rm=TRUE) == c(0,1))==2){
            pbc[,ind] <- as.logical(pbc[,ind])
          }
        }
      }else{
        if(length(unique(pbc[which(!is.na(pbc[,ind])),ind]))<=2) {
          if(sum(sort(unique(pbc[,ind])) == c(0,1))==2){
            pbc[,ind] <- as.logical(pbc[,ind])
          }
          if(sum(sort(unique(pbc[,ind])) == c(FALSE, TRUE))==2){
            pbc[,ind] <- as.logical(pbc[,ind])
          }
        }
      }
      if(!is.logical(pbc[, ind]) & 
           length(unique(pbc[which(!is.na(pbc[,ind])),ind]))<=5) {
        pbc[,ind] <- factor(pbc[,ind])
      }
    }
    # Convert age to years
    pbc$age <- pbc$age/364.24
    
    pbc$years <- pbc$days/364.24
    pbc <- pbc[, -which(colnames(pbc)=="days")]
    pbc$treatment <- as.numeric(pbc$treatment)
    pbc$treatment[which(pbc$treatment==1)] <- "DPCA"
    pbc$treatment[which(pbc$treatment==2)] <- "placebo"
    pbc$treatment <- factor(pbc$treatment)
    
    cat("pbc: randomForest\n")
    dta.train <- pbc[-which(is.na(pbc$treatment)),]
    # Create a test set from the remaining patients
    pbc.test <- pbc[which(is.na(pbc$treatment)),]
    
    if(!test) rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., dta.train, nsplit = 10,
                                 na.action="na.impute", ...)
    if(save) save(rfsrc_pbc, file=paste(pth, "rfsrc_pbc.rda", sep=""), compress="xz")
    
    cat("pbc: randomForest predict\n")
    # Predict survival for 106 patients not in randomized trial
    if(!test)  rfsrc_pbc_test <- predict(rfsrc_pbc, 
                                         newdata = pbc.test,
                                         na.action = "na.impute")
    if(save) save(rfsrc_pbc_test, file=paste(pth, "rfsrc_pbc_test.rda", sep=""), compress="xz")
    
    # Print prediction summary  
    rfsrc_pbc_test
    cat("pbc: RF minimal depth\n")
    if(!test) varsel_pbc <- var.select(rfsrc_pbc)
    else{
      data(varsel_pbc, package="ggRandomForests",
           envir = dta)
      varsel_pbc <- dta$varsel_pbc
    }
    if(save) save(varsel_pbc, file=paste(pth, "varsel_pbc.rda", sep=""), compress="xz")
    
    cat("pbc: RF interactions\n")
    if(!test) interaction_pbc <- find.interaction(rfsrc_pbc)
    if(save) save(interaction_pbc, file=paste(pth, "interaction_pbc.rda", sep=""), compress="xz")
    
    # Calculate the partial dependence
    cat("pbc: RF partial plots\n(this will take a little while...)\n")
    xvar <- varsel_pbc$topvars[c(1:4, 6)]
    
    # Remove the categorical variables
    xvar.cat <- c("edema")
    
    
    if(!test) partial_pbc <- mclapply(c(1,3,5), function(tm){
      plot.variable(rfsrc_pbc, surv.type = "surv", 
                    time = tm, 
                    xvar.names = xvar, partial = TRUE, 
                    show.plots = FALSE)
    })
    
    if(save) save(partial_pbc, file=paste(pth, "partial_pbc.rda", sep=""), compress="xz")
    
  }
  
  if("veteran" %in% set){
    data(veteran, package="randomForestSRC",
         envir = dta)
    vet <- dta$veteran
    # For whatever reason, the age variable is in days... makes no sense to me
    for(ind in 1:dim(vet)[2]){
      if(!is.factor(vet[,ind])){
        if(length(unique(vet[which(!is.na(vet[,ind])),ind]))<=2) {
          if(sum(range(vet[,ind],na.rm=TRUE) == c(0,1))==2){
            vet[,ind] <- as.logical(vet[,ind])
          }
        }
      }else{
        if(length(unique(vet[which(!is.na(vet[,ind])),ind]))<=2) {
          if(sum(sort(unique(vet[,ind])) == c(0,1))==2){
            vet[,ind] <- as.logical(vet[,ind])
          }
          if(sum(sort(unique(vet[,ind])) == c(FALSE, TRUE))==2){
            vet[,ind] <- as.logical(vet[,ind])
          }
        }
      }
      if(!is.logical(vet[, ind]) & 
           length(unique(vet[which(!is.na(vet[,ind])),ind]))<=5) {
        vet[,ind] <- factor(vet[,ind])
      }
    }
  
    dta$veteran <- vet
    
    cat("veteran: randomForest\n")
    if(!test) rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = dta$veteran, ...)
    
    if(save) save(rfsrc_veteran, 
                  file=paste(pth, "rfsrc_veteran.rda", sep=""), compress="xz")
    
    cat("\nveteran: RF minimal depth\n")
    if(!test) varsel_veteran <- var.select(rfsrc_veteran)
    if(save) save(varsel_veteran, 
                  file=paste(pth, "varsel_veteran.rda", sep=""), compress="xz")
    
    cat("veteran: RF interactions\n")
    if(!test) interaction_veteran <- find.interaction(rfsrc_veteran)
    if(save) save(interaction_veteran, 
                  file=paste(pth, "interaction_veteran.rda", sep=""), compress="xz")
    
    cat("veteran:  RF partial plots\n(this will take a little while...)\n")
    if(!test) partial_veteran <- mclapply(c(30, 180), function(tm){
      plot.variable(rfsrc_veteran, 
                    surv.type = "surv", 
                    partial = TRUE, 
                    time=tm,
                    show.plots=FALSE)
    })
    
    if(save) save(partial_veteran, 
                  file=paste(pth, "partial_veteran.rda", sep=""), compress="xz")
  }
}
# 
# # For randomForest implementation
# rf_cache_datasets <- function(set=NA, save=TRUE, pth, ...){
#   dta <- new.env()
#   
#   if(missing(pth)){
#     pth <- if(file.exists("data")){
#       if(file.info("data")$isdir){
#         "data/"
#       }else{
#         "./"
#       }
#     }else{
#       "./"
#     } 
#   }else if(!file.info("data")$isdir){
#     stop("Provided path does not exist, or is not a directory.")
#   }
#   
#   if(is.na(set))
#     set <- c("airq", "Boston", "iris", "mtcars")
#   
#   if("airq" %in% set){
#     cat("airq: randomForest\n")
#     if(!test) rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute", ...)
#     if(save) save(rfsrc_airq, file=paste(pth, "rfsrc_airq.rda", sep=""), compress="xz")
#     
#     cat("airq: RF partial dependence\n")
#     
#   }
#   
#   if("iris" %in% set){
#     cat("iris: randomForest\n")
#     if(!test) rfsrc_iris <- rfsrc(Species ~., data = iris, ...)
#     if(save) save(rfsrc_iris, file=paste(pth, "rfsrc_iris.rda", sep=""), compress="xz")
#     
#     cat("iris: RF partial dependence\n")
#   }
#   
#   
#   if("mtcars" %in% set){
#     cat("mtcars: randomForest\n")
#     if(!test) rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, ...)
#     if(save) save(rfsrc_mtcars, file=paste(pth, "rfsrc_mtcars.rda", sep=""), compress="xz")
#     
#     cat("mtcars: RF partial dependence\n")
#   }
#   if("Boston" %in% set){
#     data(Boston, package="MASS",
#          envir = dta)
#     Boston <- dta$Boston
#     
#     Boston$chas <- as.logical(Boston$chas)
#     
#     cat("Boston: randomForest\n")
#     if(!test) rfsrc_Boston <- rfsrc(medv~., data=Boston, ...)
#     if(save) save(rfsrc_Boston, file=paste(pth, "rfsrc_Boston.rda", sep=""), compress="xz")
#     
#     cat("Boston: RF partial dependence\n(this will take a little while...)\n")
#     
#     cat("\nBoston: RF partial coplots\n\tlstat by rm groups\n(this will take a little longer...)\n")
#     
#     cat("\nBoston: RF partial coplots\n\trm by lstat groups\n(so will this...)\n")
#   }
#   
# }