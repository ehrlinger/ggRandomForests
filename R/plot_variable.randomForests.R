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
#' @export plot.variable.randomForests.ggrandomForests
#' @export plot.variable.randomForests
#' 
#'
plot.variable.randomForests.ggrandomForests <- function(x, 
                                               pred.data, 
                                               xvar.names, 
                                               which.outcome,
                                               w, 
                                               show.plots = TRUE, 
                                               partial = FALSE,
                                               rug = TRUE, 
                                               xlab=deparse(substitute(x.var)), 
                                               ylab="",
                                               main=paste("Partial Dependence on", deparse(substitute(x.var))),
                                               smooth.lines = FALSE,
                                               sorted = TRUE, 
                                               nvar, 
                                               npts = 25, 
                                               subset, 
                                               ...)
{
  object <- x
  remove(x)
  if (!inherits(object, "randomForests")){
    stop("This function only works for objects of class `randomForests'.")
  }
  if (partial && is.null(object$forest)) {
    stop("forest is empty:  re-run randomForests call with keep.forest=TRUE")
  }
  
  # Handle subsetting the training data observations.
  if (missing(subset)) {
    subset <- 1:nrow(pred.data)
  }
  else {
    if (is.logical(subset)) 
      subset <- which(subset)
    subset <- unique(subset[subset >= 1 & subset <= nrow(object$xvar)])
    if (length(subset) == 0) {
      stop("'subset' not set properly.")
    }
  }
  object$xvar <- object$xvar[subset, , drop = FALSE]
  
  # Check the type of forest (classification or regression)
  fmly <- object$type
  if (fmly == "classification"){
    
    # Want to change the behavior for multiclass cases without specific which.outcome
    if (missing(which.outcome)) {
      which.outcome <- 1
    }
    else if (is.character(which.outcome)) {
      which.outcome <- match(match.arg(which.outcome, levels(object$y)), 
                             levels(object$y))
    }
    else {
      if (which.outcome > length(levels(object$y)) | 
            which.outcome < 1) {
        stop("which.outcome is specified incorrectly:", 
             which.outcome)
      }
    }
    pred.type <- "prob"
    yvar.dim <- 1
    VIMP <- importance(object)[, which.outcome]
    ylabel <- paste("probability", levels(object$y)[which.outcome])
  }else {
    pred.type <- "y"
    yvar.dim <- 1
    which.outcome <- NULL
    VIMP <- importance(object)
    ylabel <- expression(hat(y))
  }
  
  # Get the x variables.
  xvar <- pred.data
  
  n <- nrow(xvar)
  
  #If we did not specify the variables of interest
  if (missing(xvar.names)) {
    xvar.names <- attributes(object$terms)$term.labels
  }else {
    # If we have, then validate them.
    if (length(setdiff(xvar.names, attributes(object$terms)$term.labels)) > 0) {
      stop("x-variable names supplied does not match available list:\n", 
           attributes(object$terms)$term.labels)
    }
    xvar.names <- unique(xvar.names)
  }
  if (sorted & !is.null(VIMP)) {
    xvar.names <- xvar.names[rev(order(VIMP[xvar.names]))]
  }
  if (!missing(nvar)) {
    nvar <- max(round(nvar), 1)
    xvar.names <- xvar.names[1:min(length(xvar.names), nvar)]
  }
  nvar <- length(xvar.names)
  
  # Now get the data.
  if (!partial) {
    yhat <- extract.RFpred(object, pred.type, subset,
                           which.outcome)
  }
  else {
    #class(object$forest) <- c("rfsrc", "partial", class(object)[3])
    if (npts < 1) 
      npts <- 1
    else npts <- round(npts)
    prtl <- lapply(1:nvar, function(k) {
      x <- xvar[, which(colnames(xvar) == xvar.names[k])]
      if (is.factor(x)) 
        x <- factor(x, exclude = NULL)
      n.x <- length(unique(x))
      if (!is.factor(x) & n.x > npts) {
        x.uniq <- sort(unique(x))[unique(as.integer(
          seq(1, n.x, length = min(npts, n.x))))]
      }
      else {
        x.uniq <- sort(unique(x))
      }
      n.x <- length(x.uniq)
      yhat <- yhat.se <- NULL
      newdata.x <- xvar
      factor.x <- is.factor(x)
      for (l in 1:n.x) {
        cat(x.uniq, " ")
        newdata.x[, object$xvar.names == xvar.names[k]] <- rep(x.uniq[l], n)
        pred.temp <- extract.RFpred(predict(object, newdata.x, type=pred.type), pred.type, 1:n, 
                                    which.outcome)
        mean.temp <- mean(pred.temp, na.rm = TRUE)
        if (!factor.x) {
          yhat <- c(yhat, mean.temp)
          if (fmly == "class") {
            yhat.se <- c(yhat.se, mean.temp * (1 - 
                                                 mean.temp)/sqrt(n))
          }
          else {
            yhat.se <- c(yhat.se, sd(pred.temp/sqrt(n), 
                                     na.rm = TRUE))
          }
        }
        else {
          pred.temp <- mean.temp + (pred.temp - mean.temp)/sqrt(n)
          yhat <- c(yhat, pred.temp)
        }
      }
      list(xvar.name = xvar.names[k], yhat = yhat, 
           yhat.se = yhat.se, n.x = n.x, x.uniq = x.uniq, 
           x = x)
    })
  }
  
  if(fmly=="classification")fmly<- "class"
  
  plot.variable.obj <- list(family = fmly, partial = partial, 
                            which.outcome = which.outcome, 
                            ylabel = ylabel, yvar.dim = yvar.dim, n = n, xvar.names = xvar.names, 
                            nvar = nvar, 
                            smooth.lines = smooth.lines)
  if (partial) {
    plot.variable.obj$pData <- prtl
  }else {
    plot.variable.obj$yhat <- yhat
    plot.variable.obj$xvar <- xvar[, which(colnames(pred.data) %in% xvar.names)]
  }
  class(plot.variable.obj) <- c("ggrandomForests", "plot.variable", 
                                fmly)
  invisible(plot.variable.obj)
}

plot.variable.randomForests <- plot.variable.randomForests.ggrandomForests