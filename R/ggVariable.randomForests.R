####**********************************************************************
####**********************************************************************
####
####  ggrandomForests - GGPLOT2 GRAPHICS FOR RANDOM FORESTS FOR SURVIVAL, 
####  REGRESSION, AND CLASSIFICATION (RF-SRC)
####  Version 0.6.0
####
####  Copyright 2012, Cleveland Clinic Foundation
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
####    URL:    https://github.com/ehrlinger/ggrandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' @title Plot the randomForests marginal dependence of variables.
#' 
#' @description plot.variable.randomForests.ggrandomForests generates a list of either marginal variable 
#' dependance or partial variable dependence figures using \code{\link{ggplot}}.
#' 
#' @param x an object of class randomForests, which contains a forest component.
#' @param pred.data	a data frame used for contructing the plot, usually the training data used to contruct the random forest.
#' @param x.var	name of the variable for which partial dependence is to be examined.
#' @param which.class	For classification data, the class to focus on (default the first class).
#' @param w	weights to be used in averaging; if not supplied, mean is not weighted
#' @param plot whether the plot should be shown on the graphic device.
#' @param add	whether to add to existing plot (TRUE).
#' @param n.pt	if x.var is continuous, the number of points on the grid for evaluating partial dependence.
#' @param rug	whether to draw hash marks at the bottom of the plot indicating the deciles of x.var.
#' @param xlab	label for the x-axis.
#' @param ylab	label for the y-axis.
#' @param main	main title for the plot.
#' @param ...	 other graphical parameters to be passed on to plot or lines. x a randomForests object
#' @param smooth.lines boolean indicating the inclusion of confidence intervals
#'
#' @return A list of \code{\link{ggplot2}} plot objects corresponding the variables 
#' contained within the \code{x} argument 
#' 
#' @seealso \code{\link{plot.variable.rfsrc}}
#' 
#' @export ggVariable.randomForests ggVariable.rfsrc ggVariable
#' 
#'
ggVariable.randomForests <- function(object,
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
                     cens=rf.surv$yvar$dead)
  pDat$cens <- as.logical(pDat$cens)
  
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
    inTime <-which(rf.surv$time.interest> time[ind])[1] -1
    pDat.t$yhat=rf.surv$survival.oob[,inTime]
    if(missing(time.labels)){
      pDat.t$time <- time[ind]
    }else{
      pDat.t$time <- time.labels[ind]
    }
    
    if(ind > 1){
      pDat.t<- rbind(pDat.t, pDat.old)
    }    
  }
  
  pDat$time <- factor(pDat$time, levels=unique(pDat$time))
  
  class(pDat) <- c("ggVariable", class(pDat))
  invisible(pDat)
}
