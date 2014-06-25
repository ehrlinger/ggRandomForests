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
#' ggMinimalDepth
#' Plot minimal depth values from an RF-S object.  
#'
#' @param rfObject An object of class (rfsrc, grow) or (rfsrc, predict).
#' 
#' @details If subset is not specified, generates the following three plots
#'  (going from top to bottom, left to right):
#' 
#' 
#' Invisibly, the conditional and unconditional Brier scores, and 
#' the integrated Brier score (if they are available).
#' 
#' @export ggMinimalDepth.ggRandomForests 
#' @export ggMinimalDepth
#' 
ggMinimalDepth.ggRandomForests <- function (object, ...){
  
  if (inherits(object, "rfsrc") == TRUE){
    vSel <- var.select(object, ...)
  }else if (!is.null(object$varselect)) {
    # Test for variable selection minimal depth object
    vSel <- object
  }else if(is.null(object$threshold)) {
    # Test for max.subtree minimal depth object, convert to vSel object
    
    stop("No support for max.subtree yet, use var.select instead")
  }else{
    stop("Function works only on rfsrc or var.select objects.")
  }
  
  class(vSel) <- c("ggMinimalDepth", class(vSel))
  invisible(vSel) 
}

ggMinimalDepth<-ggMinimalDepth.ggRandomForests                         