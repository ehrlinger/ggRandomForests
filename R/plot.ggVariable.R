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
#' plot.ggVariable
#' Plot a \code{\link{ggVariable}} object, 
#' 
#' @param x ggVariable object created from a randomForestSRC object
#' @param var variable (or list of variables) of interest.
#' @param ... arguments passed to the \code{\link{ggVariable}} function.
#' 
#' @return ggplot object
#' 
#' @export plot.ggVariable
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for 
#' R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.4.
#' 
### error rate plot
plot.ggVariable<- function(x, var, ...){
 object <- x 
  if(inherits(object, "rfsrc")) object<- ggVariable(object, ...)
  
  gDta <- ggplot(object)+
    geom_point(aes_string(x=var, y="yhat", col="cens", shape="cens"), alpha=.5)
  
  if(length(levels(object$time)) > 1)
    gDta<- gDta + facet_wrap(~time, ncol=1)
  
  return(gDta)
}
