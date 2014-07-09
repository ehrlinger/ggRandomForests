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
#' show.ggVariableList
#' Plot a \code{\link{ggVariable}} object, 
#' 
#' @export show.ggVariableList
#' 
show.ggVariableList <- function(object, ...){
  
  if(!inherits(object, "list")) stop("Functions expects a list object")
  for(ind in 1:length(object)){
    show(object[[ind]])
  }
}
  