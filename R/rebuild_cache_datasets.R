#' Recreate the cached data sets for the ggRandomForests package
#' 
#' @param set Defaults to all sets (NA), however for individual sets specify one 
#' or more of c("airq", "iris", "mtcars", "pbc", "veteran")
#' @param save Defaults to write files to the current data directory.
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
#' @examples
#' \dontrun{
#' rebuild_cache_datasets()
#' }
#'
#'
rebuild_cache_datasets <- function(set=NA, save=TRUE){
  dta <- new.env()
  if(is.na(set))
    set <- c("airq", "iris", "mtcars", "pbc", "veteran")
  
  if("airq" %in% set){
    cat("airq: randomForest\n")
    airq_rf <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
    if(save) save(airq_rf, file="data/airq_rf.rda")
    
    cat("airq: RF minimal depth\n")
    airq_vs <- var.select(airq_rf)
    if(save) save(airq_vs, file="data/airq_vs.rda")
    
    cat("airq: RF interactions\n")
    airq_interaction <- find.interaction(airq_rf)
    if(save) save(airq_interaction, file="data/airq_interaction.rda")
    
    cat("airq: RF partial dependence\n")
    airq_prtl <- plot.variable(airq_rf,
                               partial=TRUE, show.plots=FALSE)
    if(save) save(airq_prtl, file="data/airq_prtl.rda", compress="xz")
  }
  
  if("iris" %in% set){
    cat("iris: randomForest\n")
    iris_rf <- rfsrc(Species ~., data = iris)
    if(save) save(iris_rf, file="data/iris_rf.rda", compress="xz")
    
    cat("iris: RF minimal depth\n")
    iris_vs <- var.select(iris_rf)
    if(save) save(iris_vs, file="data/iris_vs.rda", compress="xz")
    
    cat("iris: RF interactions\n")
    iris_interaction <- find.interaction(iris_rf)
    if(save) save(iris_interaction, file="data/iris_interaction.rda", compress="xz")
    
    cat("iris: RF partial dependence\n")
    iris_prtl <- plot.variable(iris_rf,
                               partial=TRUE, show.plots=FALSE)
    if(save) save(iris_prtl, file="data/iris_prtl.rda", compress="xz")
  }
  
  
  if("mtcars" %in% set){
    cat("mtcars: randomForest\n")
    mtcars_rf <- rfsrc(mpg ~ ., data = mtcars)
    if(save) save(mtcars_rf, file="data/mtcars_rf.rda", compress="xz")
    
    cat("mtcars: RF minimal depth\n")
    mtcars_vs <- var.select(mtcars_rf)
    if(save) save(mtcars_vs, file="data/mtcars_vs.rda", compress="xz")
    
    cat("mtcars: RF interactions\n")
    mtcars_interaction <- find.interaction(mtcars_rf)
    if(save) save(mtcars_interaction, file="data/mtcars_interaction.rda", compress="xz")
    
    cat("mtcars: RF partial dependence\n")
    mtcars_prtl <- plot.variable(mtcars_rf,
                                 partial=TRUE, show.plots=FALSE)
    if(save) save(mtcars_prtl, file="data/mtcars_prtl.rda", compress="xz")
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
    pbc$treatment[which(pbc$treatment==1)] <- "D-pen"
    pbc$treatment[which(pbc$treatment==2)] <- "placebo"
    pbc$treatment <- factor(pbc$treatment)
    
    dta$pbc <- pbc
    
    cat("pbc: randomForest\n")
    pbc_rf <- rfsrc(Surv(years, status) ~ ., dta$pbc, nsplit = 10,
                    na.action="na.impute", 
                    ntree=500)
    if(save) save(pbc_rf, file="data/pbc_rf.rda", compress="xz")
    
    cat("pbc: RF minimal depth\n")
    pbc_vs <- var.select(pbc_rf)
    if(save) save(pbc_vs, file="data/pbc_vs.rda", compress="xz")
    
    cat("pbc: RF interactions\n")
    pbc_interaction <- find.interaction(pbc_rf)
    if(save) save(pbc_interaction, file="data/pbc_interaction.rda", compress="xz")
    
    # Calculate the 1 year partial dependence
    xvar <- pbc_vs$topvars[1:6]
    
    pbc_prtl_time <- mclapply(c(1,3,5), function(tm){
      plot.variable(pbc_rf, surv.type = "surv", 
                    time = tm, 
                    xvar.names = xvar, partial = TRUE, 
                    show.plots = FALSE)
    })
    
    save(pbc_prtl_time, file="data/pbc_prtl_time.rda", compress="xz")
    
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
    veteran_rf <- rfsrc(Surv(time, status) ~ ., data = dta$veteran, ntree = 100)
    
    if(save) save(veteran_rf, file="data/veteran_rf.rda", compress="xz")
    
    cat("veteran: RF minimal depth\n")
    veteran_vs <- var.select(veteran_rf)
    if(save) save(veteran_vs, file="data/veteran_vs.rda", compress="xz")
    
    cat("veteran: RF interactions\n")
    veteran_interaction <- find.interaction(veteran_rf)
    if(save) save(veteran_interaction, file="data/veteran_interaction.rda", compress="xz")
    
    cat("veteran: RF partial dependence\n")
    veteran_prtl <- plot.variable(veteran_rf, surv.type = "surv",
                                  partial = TRUE, time=30,
                                  show.plots=FALSE)
    
    if(save) save(veteran_prtl, file="data/veteran_prtl.rda", compress="xz")
  }
}