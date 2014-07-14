####**********************************************************************
####**********************************************************************
####
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
#' gg_minimal_depth Extract the minimal depth values from an rfsrc object.  
#'
#' @param object A randomForestSRC forest object, predict object or
#' the list from the var.select.rfsrc function
#' @param ... optional arguments passed to the var.select function 
#'  of randomForestSRC
#' 
#' @description the var.select function implements random forest variable 
#' selection using tree minimal depth methodology. The gg_minimal_depth 
#' function takes the output from var.select and creates a data.frame 
#' formatted for the plot.gg_minimal_depth function.
#'  
#' @return Invisibly, the modified list of variables from the 
#' var.select.rfsrc function, ordered by minimal depth rank. 
#' 
#' @export gg_minimal_depth.ggRandomForests gg_minimal_depth
#' @aliases gg_minimal_depth
#' 
#' @seealso \code{var.select} \code{\link{plot.gg_minimal_depth}}
#' 
#' @importFrom randomForestSRC var.select
#' @importFrom dplyr tbl_df
gg_minimal_depth.ggRandomForests <- function (object, ...){
  
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
  
  class(vSel) <- c("gg_minimal_depth", class(vSel))
  invisible(vSel) 
}

gg_minimal_depth<-gg_minimal_depth.ggRandomForests                         