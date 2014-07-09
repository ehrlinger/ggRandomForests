####**********************************************************************
####**********************************************************************
####
####  ggRandomForests - GGPLOT2 GRAPHICS FOR RANDOM FORESTS FOR SURVIVAL, 
####  REGRESSION, AND CLASSIFICATION (RF-SRC)
####
####  This program is free software; you can redistribute it and/or
####  modify it under the terms of the GNU General Public License
####  as published by the Free Software Foundation; either version 2
####  of the License, or (at your option) any later version.
####
####  This program is distributed in the hope that it will be useful,
####  but WITHOUT ANY WARRANTY; without even the implied warranty of
####  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
####  GNU General Public License for more details.
####
####  You should have received a copy of the GNU General Public
####  License along with this program; if not, write to the Free
####  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
####  Boston, MA  02110-1301, USA.
####
####  ----------------------------------------------------------------
####  Project Partially Funded By: 
####  ----------------------------------------------------------------
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
#' @title Plot the marginal dependence of variables.
#' 
#' @description plot.variable.ggRandomForests generates a list of either marginal variable 
#' dependance or partial variable dependence figures using \code{\link{ggplot}}.
#' 
#' @param object a randomForestSRC object 
#' @param time point (or points) of interest
#' @param time.labels If more than one time is specified, a vector of time.labels 
#' for differentiating the time points
#' @param oob indicate if predicted results should include oob or full data set.
#' @param ... extra arguments 
#'  
#' @return A list of \code{\link{ggplot2}} plot objects corresponding the variables 
#' contained within the \code{x} argument 
#' 
#' @seealso \code{\link{plot.variable.rfsrc}}
#' 
#' @export ggVariable.ggRandomForests ggVariable.rfsrc
#' @export ggVariable
#' 
#' @aliases ggVariable
#' 
#'
ggVariable.ggRandomForests <- function(object,
                                       time,
                                       time.labels,
                                       oob=TRUE,
                                       ...)
{
  
  # Want to also handle a plot.variable where partial!= TRUE
  if (!inherits(object, "rfsrc")) {
    stop("ggVariable expects a randomForest or plot.variable object.")
  }
  
  # ggVariable is really just cutting the data into time slices.
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
    pDat$cens <- as.logical(object$yvar$dead)
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
  class(pDat) <- c("ggVariable", class(pDat))
  invisible(pDat)
}


ggVariable.rfsrc <- ggVariable.ggRandomForests

ggVariable <- ggVariable.ggRandomForests
