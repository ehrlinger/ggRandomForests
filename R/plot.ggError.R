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
#' plot.ggError
#' Plot a \link{\code{ggError}} object, the cumulative OOB error rates of the forest as a function of number of trees.
#' 
#' @param x ggError object created from a randomForestSRC object
#' 
#' @return ggplot object
#' 
#' @export plot.ggError
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggrf.obj<- ggError(iris.obj)
#' 
#' plot.ggError(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- ggError(v.obj)
#' plot(ggrf.obj)
#'
### error rate plot
plot.ggError <- function(obj){
 
  if(class(obj)[1] == "rfsrc") obj<- ggError(obj)
  
  gDta <- ggplot(obj, aes(x=indx,y=value, col=variable))+
    geom_line()+
    labs(x = "Number of Trees",
         y = "OOB Error Rate")
  
  if(length(unique(obj$variable)) ==1){
    gDta <- gDta + theme(legend.position="none")
  }
  return(gDta)
}
