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
#' @export ggPartial.ggRandomForests
#' @export ggPartial
#' 
#'
ggPartial.ggRandomForests <- function()
{
 
}

ggPartial <- ggPartial.ggRandomForests
