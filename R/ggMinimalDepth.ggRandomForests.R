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
#' ggMinimalDepth Extract the minimal depth values from an rfsrc object.  
#'
#' @param object A randomForestSRC forest object, predict object or
#' the list from the var.select.rfsrc function
#'  @param ... optional arguments passed to the var.select function 
#'  of randomForestSRC
#'  
#' @return Invisibly, the modified list from the var.select.rfsrc
#' function 
#' 
#' @export ggMinimalDepth.ggRandomForests ggMinimalDepth
#' @aliases ggMinimalDepth
#' 
#' @importFrom randomForestSRC var.select
#' @importFrom dplyr tbl_df
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
  vSel$varselect$names <- rownames(vSel$varselect)
  
  vSel$varselect$names <- factor(vSel$varselect$names, 
                                 levels=unique(vSel$varselect$names))
  vSel <- tbl_df(vSel)
  
  class(vSel) <- c("ggMinimalDepth", class(vSel))
  invisible(vSel) 
}

ggMinimalDepth<-ggMinimalDepth.ggRandomForests                         