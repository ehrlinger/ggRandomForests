####**********************************************************************
####**********************************************************************
####  ----------------------------------------------------------------
####  Written by:
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
#' @title gg_interactions extract the variable interactions.
#' 
#' @param object a randomForestSRC object or the output from the
#' \code{find.interactions} function call
#' @param ... optional extra arguments passed to find.interactions
#' 
#' @export gg_interactions.ggRandomForests gg_interactions
#' @aliases gg_interactions
#' 
gg_interactions.ggRandomForests <- function(object, ...){
  
  # Want to also handle a plot.variable where partial!= TRUE
  if (!inherits(object, "rfsrc")) {
    stop("gg_variable expects a randomForest or plot.variable object.")
  }
  
  # IF we called this with a partial plot obect, instead of marginal.
  if(inherits(object, "plot.variable"))
    if(object$partial) invisible(gg_partial(object, ...))
  
  
}