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
#' plot.ggPartial
#' Plot a \code{\link{ggPartial}} object, the partial plot data object.
#' 
#' @param x ggPartial object created from a randomForestSRC object
#' @param ... extra arguments
#' 
#' @return ggplot object
#' 
#' @export plot.ggPartial
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, 
#' Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.4.
#' 
#'
### error rate plot
plot.ggPartialList <- function(x, points=TRUE, smooth="loess", ...){
  object <- x 
  
  if(!inherits(object, "list")) stop("Functions expects a list object")
  lng <- length(object)
  gDat <- vector("list", length=lng)
  for(ind in 1:lng){
    gDat[[ind]] <- plot.ggPartial(object[[ind]], points, smooth, ...)
  }
  
  return(gDat)
}
