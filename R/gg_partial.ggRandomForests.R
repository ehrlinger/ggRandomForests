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
#' Partial variable dependence object 
#' 
#' @description \code{plot.variable} generates a 
#' list of either marginal variable dependance or partial variable dependence
#' data from a randomForestSRC object. 
#' The \code{gg_partial} function formulats the \code{plot.variable} output
#' for partial plots  (where partial=TRUE) into a data object for creation of 
#' partial dependence plots using the \code{\link{plot.gg_partial}} function. 
#' Partial variable dependence plots are the risk adjusted estimates of the specified response as a 
#' function of a single covariate, possibly subsetted on other covariates.
#' 
#' @param object the partial rfsrc data object from \code{plot.variable} function
#' @param named optional column for merging multiple plots together
#' @param ... optional arguments
#'  
#' @return A data.frame or list of data.frames corresponding the variables 
#' contained within the \code{plot.variable} output. 
#' 
#' @seealso \code{plot.variable.rfsrc} \code{\link{plot.gg_partial}}
#' 
#' @aliases gg_partial
#' 
#' @export gg_partial.ggRandomForests gg_partial
#' @importFrom dplyr tbl_df
#' 
gg_partial.ggRandomForests <- function(object, 
                                      named,
                                      ...){
  if(!inherits(object,"plot.variable")){
    stop("gg_partial expects a plot.variable object, Run plot.variable with partial=TRUE")
  }
  if(!object$partial) invisible(gg_variable(object, ...))
  
  # How many variables
  n.var=length(object$pData)
  
  # Create a list of data
  pDat <- lapply(1:n.var, function(ind){
    data.frame(cbind(yhat=object$pData[[ind]]$yhat, 
                     x=object$pData[[ind]]$x.uniq))
  })
  
  # name the data, so labels come out correctly.
  for(ind in 1:n.var){
    pDat[[ind]] <- tbl_df(pDat[[ind]])
    colnames(pDat[[ind]])[-1] <- object$xvar.names[ind]
    if(!missing(named)) pDat[[ind]]$id=named
    class(pDat[[ind]]) <- c("gg_partial", class(pDat[[ind]]))
  }
  
  if(n.var ==1 ){
    # If there is only one, no need for a list
    invisible(pDat[[1]])
  }else{
    # otherwise, add a class label so we can handle it correctly. 
    class(pDat) <- c("gg_partial_list", class(pDat))
    invisible(pDat)
  }
  
}

gg_partial <- gg_partial.ggRandomForests
