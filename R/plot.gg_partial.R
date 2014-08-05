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
#' Partial plot generic function, operates on a \code{\link{gg_partial}} object.
#' 
#' @description Generate a risk adjusted (partial) variable dependence plot. 
#' The function plots the randomForest response variable (y-axis) against
#' the covariate of interest (specified when creating the
#'  \code{\link{gg_partial}} object).
#' 
#' @param x gg_partial object created from a randomForestSRC object
#' @param points plot points (boolean)
#' @param smooth use smooth curve (by type)
#' @param ... extra arguments
#' 
#' @return ggplot object
#' 
#' @export plot.gg_partial
#' 
#' @seealso \code{plot.variable} \code{\link{gg_partial}} \code{\link{plot.gg_partial_list}}
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
#' @importFrom ggplot2 ggplot aes labs geom_point geom_smooth 
### error rate plot
plot.gg_partial <- function(x, points=TRUE, smooth="loess", ...){
  object <- x 
  
  # Get the colname of the independent variable
  hName <- colnames(object)[2]
  
  colnames(object)[2] <- "x"
  prt.plt<- ggplot(object,aes_string(x="x", y="yhat"))+
    labs(x=hName, y="predicted")
  
  if(points)  
    prt.plt<- prt.plt+geom_point()
  if(!is.null(smooth)){
    prt.plt<- prt.plt+geom_smooth(method=smooth, ...)
  }
  
  return(prt.plt)
  
}
