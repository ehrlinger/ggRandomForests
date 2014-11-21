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
    if(save) save(airq_prtl, file="data/airq_prtl.rda", compress="bzip2")
  }
  
  if("iris" %in% set){
    cat("iris: randomForest\n")
    iris_rf <- rfsrc(Species ~., data = iris)
    if(save) save(iris_rf, file="data/iris_rf.rda", compress="bzip2")
    
    cat("iris: RF minimal depth\n")
    iris_vs <- var.select(iris_rf)
    if(save) save(iris_vs, file="data/iris_vs.rda", compress="bzip2")
    
    cat("iris: RF interactions\n")
    iris_interaction <- find.interaction(iris_rf)
    if(save) save(iris_interaction, file="data/iris_interaction.rda", compress="bzip2")
    
    cat("iris: RF partial dependence\n")
    iris_prtl <- plot.variable(iris_rf,
                               partial=TRUE, show.plots=FALSE)
    if(save) save(iris_prtl, file="data/iris_prtl.rda", compress="bzip2")
  }
  
  
  if("mtcars" %in% set){
    cat("mtcars: randomForest\n")
    mtcars_rf <- rfsrc(mpg ~ ., data = mtcars)
    if(save) save(mtcars_rf, file="data/mtcars_rf.rda", compress="bzip2")
    
    cat("mtcars: RF minimal depth\n")
    mtcars_vs <- var.select(mtcars_rf)
    if(save) save(mtcars_vs, file="data/mtcars_vs.rda", compress="bzip2")
    
    cat("mtcars: RF interactions\n")
    mtcars_interaction <- find.interaction(mtcars_rf)
    if(save) save(mtcars_interaction, file="data/mtcars_interaction.rda", compress="bzip2")
    
    cat("mtcars: RF partial dependence\n")
    mtcars_prtl <- plot.variable(mtcars_rf,
                                 partial=TRUE, show.plots=FALSE)
    if(save) save(mtcars_prtl, file="data/mtcars_prtl.rda", compress="bzip2")
  }
  
  if("pbc" %in% set){
    data(pbc, package="randomForestSRC",
         envir = dta)
    
    cat("pbc: randomForest\n")
    pbc_rf <- rfsrc(Surv(days, status) ~ ., dta$pbc, nsplit = 10,
                    ntree=500)
    if(save) save(pbc_rf, file="data/pbc_rf.rda", compress="bzip2")
    
    cat("pbc: RF minimal depth\n")
    pbc_vs <- var.select(pbc_rf)
    if(save) save(pbc_vs, file="data/pbc_vs.rda", compress="bzip2")
    
    cat("pbc: RF interactions\n")
    pbc_interaction <- find.interaction(pbc_rf)
    if(save) save(pbc_interaction, file="data/pbc_interaction.rda", compress="bzip2")
    
    cat("pbc: RF partial dependence\n")
    pbc_prtl <- plot.variable(pbc_rf, time=90,surv.type="surv",
                              xvar.names = c("bili", "copper", "albumin", "age"),
                              partial=TRUE, show.plots=FALSE)
    
    if(save) save(pbc_prtl, file="data/pbc_prtl.rda", compress="bzip2")
  }
  
  if("veteran" %in% set){
    data(veteran, package="randomForestSRC",
         envir = dta)
    cat("veteran: randomForest\n")
    veteran_rf <- rfsrc(Surv(time, status) ~ ., data = dta$veteran, ntree = 100)
    
    if(save) save(veteran_rf, file="data/veteran_rf.rda", compress="bzip2")
    
    cat("veteran: RF minimal depth\n")
    veteran_vs <- var.select(veteran_rf)
    if(save) save(veteran_vs, file="data/veteran_vs.rda", compress="bzip2")
    
    cat("veteran: RF interactions\n")
    veteran_interaction <- find.interaction(veteran_rf)
    if(save) save(veteran_interaction, file="data/veteran_interaction.rda", compress="bzip2")
    
    cat("veteran: RF partial dependence\n")
    veteran_prtl <- plot.variable(veteran_rf, surv.type = "surv",
                                  partial = TRUE, time=30, xvar.names = "age",
                                  show.plots=FALSE)
    
    if(save) save(veteran_prtl, file="data/veteran_prtl.rda", compress="bzip2")
  }
  
  
  
}