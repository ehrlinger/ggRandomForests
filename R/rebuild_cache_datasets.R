#' Recreate the cached data sets for the ggRandomForests package
#' 
#' @param set Defaults to all sets (NA), however for individual sets specify one 
#' or more of c("airq", "iris", "mtcars", "pbc", "veteran", "Boston")
#' @param save Defaults to write files to the current data directory.
#' @param pth the directory to store files.
#' @param ... extra arguments passed to randomForestSRC functions.
#' 
#' ggRandomForests operates directly on randomForestSRC objects. In order to 
#' improve performance on function examples and package tests and checks,
#' a set of precompiled randomForestSRC objects are stored in the package
#' data subfolder, however version changes in the dependant package and break
#' some functionality. 
#'
#' @details This function was created to help the package developer deal with changes 
#' in the randomForestSRC package during version iterations. 
#' 
#' @export rebuild_cache_datasets
#' 
#' @importFrom randomForestSRC rfsrc var.select plot.variable find.interaction
#' @importFrom dplyr filter
#' 
#' @examples
#' \dontrun{
#' rebuild_cache_datasets()
#' }
#'
#'
rebuild_cache_datasets <- function(set=NA, save=TRUE, pth, ...){
  dta <- new.env()
  
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
  }else if(!file.info("data")$isdir){
    stop("Provided path does not exist, or is not a directory.")
  }
  
  if(is.na(set))
    set <- c("airq", "iris", "mtcars", "pbc", "veteran", "Boston")
  
  if("airq" %in% set){
    cat("airq: randomForest\n")
    rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute", ...)
    if(save) save(rfsrc_airq, file=paste(pth, "rfsrc_airq.rda", sep=""), compress="xz")
    
    cat("\nairq: RF minimal depth\n")
    varsel_airq <- var.select(rfsrc_airq)
    if(save) save(varsel_airq, file=paste(pth, "varsel_airq.rda", sep=""), compress="xz")
    
    cat("airq: RF interactions\n")
    interaction_airq <- find.interaction(rfsrc_airq)
    if(save) save(interaction_airq, file=paste(pth, "interaction_airq.rda", sep=""), compress="xz")
    
    cat("airq: RF partial dependence\n")
    partial_airq <- plot.variable(rfsrc_airq,
                                  partial=TRUE, show.plots=FALSE)
    if(save) save(partial_airq, file=paste(pth, "partial_airq.rda", sep=""), compress="xz")
  }
  
  if("iris" %in% set){
    cat("iris: randomForest\n")
    rfsrc_iris <- rfsrc(Species ~., data = iris, ...)
    if(save) save(rfsrc_iris, file=paste(pth, "rfsrc_iris.rda", sep=""), compress="xz")
    
    cat("\niris: RF minimal depth\n")
    varsel_iris <- var.select(rfsrc_iris)
    if(save) save(varsel_iris, file=paste(pth, "varsel_iris.rda", sep=""), compress="xz")
    
    cat("iris: RF interactions\n")
    interaction_iris <- find.interaction(rfsrc_iris)
    if(save) save(interaction_iris, file=paste(pth, "interaction_iris.rda", sep=""), compress="xz")
    
    cat("iris: RF partial dependence\n")
    partial_iris <- plot.variable(rfsrc_iris,
                                  partial=TRUE, show.plots=FALSE)
    if(save) save(partial_iris, file=paste(pth, "partial_iris.rda", sep=""), compress="xz")
  }
  
  
  if("mtcars" %in% set){
    cat("mtcars: randomForest\n")
    rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, ...)
    if(save) save(rfsrc_mtcars, file=paste(pth, "rfsrc_mtcars.rda", sep=""), compress="xz")
    
    cat("\nmtcars: RF minimal depth\n")
    varsel_mtcars <- var.select(rfsrc_mtcars)
    if(save) save(varsel_mtcars, file=paste(pth, "varsel_mtcars.rda", sep=""), compress="xz")
    
    cat("mtcars: RF interactions\n")
    interaction_mtcars <- find.interaction(rfsrc_mtcars)
    if(save) save(interaction_mtcars, file=paste(pth, "interaction_mtcars.rda", sep=""), compress="xz")
    
    cat("mtcars: RF partial dependence\n")
    partial_mtcars <- plot.variable(rfsrc_mtcars,
                                    partial=TRUE, show.plots=FALSE)
    if(save) save(partial_mtcars, file=paste(pth, "partial_mtcars.rda", sep=""), compress="xz")
  }
  if("Boston" %in% set){
    data(Boston, package="MASS",
         envir = dta)
    Boston <- dta$Boston
    
    Boston$chas <- as.logical(Boston$chas)
    
    cat("Boston: randomForest\n")
    rfsrc_Boston <- rfsrc(medv~., data=Boston, ...)
    if(save) save(rfsrc_Boston, file=paste(pth, "rfsrc_Boston.rda", sep=""), compress="xz")
    
    cat("\nBoston: RF minimal depth\n")
    varsel_Boston <- var.select(rfsrc_Boston)
    if(save) save(varsel_Boston, file=paste(pth, "varsel_Boston.rda", sep=""), compress="xz")
    
    cat("Boston: RF interactions\n")
    interaction_Boston <- find.interaction(rfsrc_Boston)
    if(save) save(interaction_Boston, file=paste(pth, "interaction_Boston.rda", sep=""), compress="xz")
    
    cat("Boston: RF partial dependence\n(this will take a little while...)\n")
    partial_Boston <- plot.variable(rfsrc_Boston,partial=TRUE, show.plots = FALSE )
    if(save) save(partial_Boston, file=paste(pth, "partial_Boston.rda", sep=""), compress="xz")
    
    cat("\nBoston: RF partial coplots\n\tlstat by rm groups\n(this will take a little longer...)\n")
    rm_pts <- cut_distribution(rfsrc_Boston$xvar$rm, groups=6)
    rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)
    partial_coplots_Boston <- gg_partial_coplot(rfsrc_Boston, xvar="lstat", 
                                                groups=rm_grp,
                                                show.plots=FALSE)
    
    if(save) save(partial_coplots_Boston, 
                  file=paste(pth, "partial_coplots_Boston.rda", sep=""), 
                  compress="xz")
    
    cat("\nBoston: RF partial coplots\n\trm by lstat groups\n(so will this...)\n")
    lstat_pts <- cut_distribution(rfsrc_Boston$xvar$lstat, groups=6)
    lstat_grp <- cut(rfsrc_Boston$xvar$lstat, breaks=lstat_pts)
    partial_coplots_Boston2 <- gg_partial_coplot(rfsrc_Boston, xvar="rm", 
                                                 groups=lstat_grp,
                                                 show.plots=FALSE)
    
    if(save) save(partial_coplots_Boston2, 
                  file=paste(pth, "partial_coplots_Boston2.rda", sep=""), 
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
    rfsrc_pbc <- rfsrc(Surv(years, status) ~ ., dta.train, nsplit = 10,
                       na.action="na.impute", ...)
    if(save) save(rfsrc_pbc, file=paste(pth, "rfsrc_pbc.rda", sep=""), compress="xz")
    
    cat("pbc: RF minimal depth\n")
    varsel_pbc <- var.select(rfsrc_pbc)
    if(save) save(varsel_pbc, file=paste(pth, "varsel_pbc.rda", sep=""), compress="xz")
    
    cat("pbc: RF interactions\n")
    interaction_pbc <- find.interaction(rfsrc_pbc)
    if(save) save(interaction_pbc, file=paste(pth, "interaction_pbc.rda", sep=""), compress="xz")
    
    # Calculate the partial dependence
    cat("pbc: RF partial plots\n(this will take a little while...)\n")
    xvar <- varsel_pbc$topvars[1:6]
    
    partial_pbc <- lapply(c(1,3,5), function(tm){
      plot.variable(rfsrc_pbc, surv.type = "surv", 
                    time = tm, 
                    xvar.names = xvar, partial = TRUE, 
                    show.plots = FALSE)
    })
    
    save(partial_pbc, file=paste(pth, "partial_pbc.rda", sep=""), compress="xz")
    
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
    rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = dta$veteran, ...)
    
    if(save) save(rfsrc_veteran, 
                  file=paste(pth, "rfsrc_veteran.rda", sep=""), compress="xz")
    
    cat("\nveteran: RF minimal depth\n")
    varsel_veteran <- var.select(rfsrc_veteran)
    if(save) save(varsel_veteran, 
                  file=paste(pth, "varsel_veteran.rda", sep=""), compress="xz")
    
    cat("veteran: RF interactions\n")
    interaction_veteran <- find.interaction(rfsrc_veteran)
    if(save) save(interaction_veteran, 
                  file=paste(pth, "interaction_veteran.rda", sep=""), compress="xz")
    
    cat("veteran:  RF partial plots\n(this will take a little while...)\n")
    partial_veteran <- lapply(c(30, 180), function(tm){
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