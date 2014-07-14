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
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggrf.obj<- gg_error(iris.obj)
#' 
#' plot.gg_error(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- gg_error(v.obj)
#' plot(ggrf.obj)
#'}
#' @importFrom ggplot2 ggplot geom_line theme aes_string labs 
### error rate plot
plot.gg_error <- function(x, ...){
  obj <- x
  if(inherits(obj, "rfsrc")) obj <- gg_error(obj)
  
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
