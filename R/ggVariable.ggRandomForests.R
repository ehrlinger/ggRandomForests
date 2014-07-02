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
#' @param x a marginal or partial rfsrc data object from \code{\link{pred.variable}}
#' @param smooth.lines boolean indicating the inclusion of confidence intervals
#'
#' @return A list of \code{\link{ggplot2}} plot objects corresponding the variables 
#' contained within the \code{x} argument 
#' 
#' @seealso \code{\link{plot.variable.rfsrc}}
#' 
#' @export ggVariable.ggRandomForests
#' @export ggVariable
#' 
#'
ggVariable.ggRandomForests <- function(object,
                                       time,
                                       time.labels,
                                       ...)
{
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
  
  # ggVariable is really just cutting the data into time slices.
  pDat <- data.frame(x=object$xvar,
                     cens=object$yvar$dead)
  pDat$cens <- as.logical(pDat$cens)
  
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
    pDat.t$yhat=100*object$survival.oob[,inTime]
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
  
  class(pDat) <- c("ggVariable", class(pDat))
  invisible(pDat)
}


ggVariable <- ggVariable.ggRandomForests
