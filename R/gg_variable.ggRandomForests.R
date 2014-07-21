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
#' @title gg_variable extract the marginal variable depedencies from
#' a randomForestSRC object, or the output from the \code{plot.variable.rfsrc}
#' function.
#' 
#' @description \code{plot.variable} generates a list containing either the marginal
#' variable dependance or the partial variable dependence. The gg_variable function
#' creates a data.frame of the marginal dependence data for creating figures using 
#' the \code{\link{plot.gg_variable}} function.
#' 
#' @param object a randomForestSRC object 
#' @param time point (or points) of interest (for survival forests only)
#' @param time.labels If more than one time is specified, a vector of time.labels 
#' for differentiating the time points (for survival forests only)
#' @param oob indicate if predicted results should include oob or full data set.
#' @param ... extra arguments 
#'  
#' @return A matrix for creating the marginal variable dependence plots.
#' 
#' @seealso  \code{\link{plot.gg_variable}} \code{plot.variable.rfsrc}
#' 
#' @export gg_variable.ggRandomForests gg_variable.rfsrc
#' @export gg_variable
#' 
#' @aliases gg_variable gg_variable.rfsrc
#' 
#' @importFrom dplyr tbl_df
#'
gg_variable.ggRandomForests <- function(object,
                                       time,
                                       time.labels,
                                       oob=TRUE,
                                       ...)
{
  
  # Want to also handle a plot.variable where partial!= TRUE
  if (!inherits(object, "rfsrc")) {
    stop("gg_variable expects a randomForest or plot.variable object.")
  }
  
  # IF we called this with a partial plot obect, instead of marginal.
  if(inherits(object, "plot.variable"))
    if(object$partial) invisible(gg_partial(object, ...))
  
  #!! Have to verify this works with a plot.variable object...
  
  # gg_variable is really just cutting the data into time slices.
  pDat <- data.frame(object$xvar)
  
  if(object$family == "regr"){
    if(oob)
      pDat$yhat <- object$predicted.oob
    else
      pDat$yhat <- object$predicted
    
  }else  if(object$family == "class"){
    if(oob){
      colnames(object$predicted.oob) <- paste("yhat.", colnames(object$predicted.oob),
                                              sep="")
      pDat <- cbind(pDat, object$predicted.oob)
      
    }else{
      colnames(object$predicted) <- paste("yhat.", colnames(object$predicted),
                                          sep="")
      pDat <- object$predicted
    }
    pDat$yvar <- object$yvar
    
  }else if(object$family == "surv"){
    pDat$cens <- as.logical(object$yvar[,2])
    colnames(pDat) <- c(object$xvar.names, "cens")
    
    lng <- length(time)
    for(ind in 1:lng){
      if(ind > 1){
        pDat.t.old <- pDat.t
      }
      ## For marginal plot.
      # Plot.variable returns the resubstituted survival, not OOB. So we calculate it.
      # Time is really straight forward since survival is a step function
      #
      # Get the event time occuring before or at 1 year. 
      pDat.t <- pDat
      inTime <-which(object$time.interest> time[ind])[1] -1
      if(inTime == 0)
        stop("The time of interest is less than the first event time. Make sure you are using the correct time units.")
      
      if(oob)
        pDat.t$yhat=100*object$survival.oob[,inTime]
      else
        pDat.t$yhat=100*object$survival[,inTime]
      
      if(missing(time.labels)){
        pDat.t$time <- time[ind]
      }else{
        pDat.t$time <- time.labels[ind]
      }
      
      if(ind > 1){
        pDat.t<- rbind(pDat.t.old, pDat.t)
      }    
    }
    
    pDat <- pDat.t
    pDat$time <- factor(pDat$time, levels=unique(pDat$time))
  }
  pDat <- tbl_df(pDat)
  class(pDat) <- c("gg_variable", class(pDat))
  invisible(pDat)
}


gg_variable.rfsrc <- gg_variable.ggRandomForests

gg_variable <- gg_variable.ggRandomForests
