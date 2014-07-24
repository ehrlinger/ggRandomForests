####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####    Assistant Staff
####    Dept of Quantitative Health Sciences
####    Learner Research Institute
####    Cleveland Clinic Foundation
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' plot.gg_error
#' Plot a \code{\link{gg_error}} object, the cumulative OOB error rates of the
#' forest as a function of number of trees.
#' 
#' @param x gg_error object created from a randomForestSRC object
#' @param ... extra arguments
#' 
#' @return ggplot graph
#' 
#' @export plot.gg_error
#' 
#' @seealso \code{\link{gg_error.ggRandomForests}} rfsrc
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews, 
#' 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression 
#' and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## You can build a randomForest
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' # ... or load a cached randomForestSRC object
#' data(iris_rf, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' ggrf.obj<- gg_error(iris_rf)
#' 
#' # Plot the gg_error object
#' plot(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' # airq_rf <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' # ... or load a cached randomForestSRC object
#' data(airq_rf, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' ggrf.obj<- gg_error(airq_rf)
#' 
#' # Plot the gg_error object
#' plot(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' 
#' # Load a cached randomForestSRC object
#' data(veteran_rf, package="ggRandomForests")
#' 
#' ggrf.obj <- gg_error(veteran_rf)
#' plot(ggrf.obj)
#'}
#' @importFrom ggplot2 ggplot geom_line theme aes_string labs 
#' @importFrom reshape2 melt
### error rate plot
plot.gg_error <- function(x, ...){
  obj <- x
  if(inherits(obj, "rfsrc")) obj <- gg_error(obj)
  
  if(!inherits(obj, "gg_error")) stop("Incorrect object type: Expects a gg_error object")
  
  obj<-melt(obj, id.vars = "ntree")
  
  # We expect the object to have the following columns
  gDta <- ggplot(obj, aes_string(x="ntree",y="value", col="variable"))+
    geom_line() +
    labs(x = "Number of Trees",
         y = "OOB Error Rate")
  
  if(length(unique(obj$variable)) == 1){
    gDta <- gDta + theme(legend.position="none")
  }
  return(gDta)
}
