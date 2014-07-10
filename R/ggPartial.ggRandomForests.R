####**********************************************************************
####**********************************************************************
####
####  ggRandomForests - GGPLOT2 GRAPHICS FOR RANDOM FORESTS FOR SURVIVAL, 
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
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' @title Plot the marginal dependence of variables.
#' 
#' @description plot.variable.randomForestSRC generates a 
#' list of either marginal variable dependance or partial variable dependence. 
#' The ggPartial object is formulated to create partial dependence plots.
#' 
#' @param object the partial rfsrc data object from plot.variable function
#' @param named optional column for merging multiple plots together
#' @param ... optional arguments
#'  
#' @return A list of \code{\link{ggplot2}} plot objects corresponding the variables 
#' contained within the \code{x} argument 
#' 
#' @seealso plot.variable.rfsrc
#' 
#' @aliases ggPartial
#' 
#' @export ggPartial.ggRandomForests ggPartial
#'
ggPartial.ggRandomForests <- function(object, 
                                      named,
                                      ...){
  if(!inherits(object,"plot.variable")){
    stop("ggPartial expects a plot.variable object, Run plot.variable with partial=TRUE")
  }
  if(!object$partial) invisible(ggVariable(object, ...))
  
  n.var=length(object$pData)
  
  pDat <- lapply(1:n.var, function(ind){
    data.frame(cbind(yhat=object$pData[[ind]]$yhat, x=object$pData[[ind]]$x.uniq))
  })
  
  for(ind in 1:n.var){
    colnames(pDat[[ind]])[-1] <- object$xvar.names[ind]
    if(!missing(named)) pDat[[ind]]$id=named
    class(pDat[[ind]]) <- c("ggPartial", class(pDat[[ind]]))
  }
  
  if(n.var ==1 ){
    invisible(pDat[[1]])
  }else{
    class(pDat) <- c("ggPartialList", class(pDat))
    invisible(pDat)
  }
    
}

ggPartial <- ggPartial.ggRandomForests
