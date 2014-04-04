####**********************************************************************
####**********************************************************************
####
####  RANDOM FORESTS FOR SURVIVAL, REGRESSION, AND CLASSIFICATION (RF-SRC)
####  Version 1.1.0
####
####  Copyright 2012, University of Miami
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
####  Dr. Ishwaran's work was funded in part by DMS grant 1148991 from the
####  National Science Foundation and grant R01 CA163739 from the National
####  Cancer Institute.
####
####  Dr. Kogalur's work was funded in part by grant R01 CA163739 from the 
####  National Cancer Institute.
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    Hemant Ishwaran, Ph.D.
####    Director of Statistical Methodology
####    Professor, Division of Biostatistics
####    Clinical Research Building, Room 1058
####    1120 NW 14th Street
####    University of Miami, Miami FL 33136
####
####    email:  hemant.ishwaran@gmail.com
####    URL:    http://web.ccs.miami.edu/~hishwaran
####    --------------------------------------------------------------
####    Udaya B. Kogalur, Ph.D.
####    Adjunct Staff
####    Dept of Quantitative Health Sciences
####    Cleveland Clinic Foundation
####    
####    Kogalur & Company, Inc.
####    5425 Nestleway Drive, Suite L1
####    Clemmons, NC 27012
####
####    email:  ubk@kogalur.com
####    URL:    http://www.kogalur.com
####    --------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************


plot.variable.rfsrc <- function(
  x,
  xvar.names,
  surv.type = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf"),
  time,
  which.outcome,
  partial = FALSE,
  plots.per.page = 4,
  granule = 5,
  sorted = TRUE,
  nvar,
  npts = 25,
  smooth.lines = FALSE,
  subset,
  ...)
{
  
  ### check that object is interpretable
  ### first rename x to object to avoid confusion with x matrix
  object <- x
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
      sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("Function only works for objects of class `(rfsrc, grow)', '(rfsrc, predict)'.")
  }
    
  ## process the subsetted index 
  ## assumes the entire data set is to be used if not specified
  if (missing(subset)) {
    subset <- 1:nrow(object$xvar)
  }
  else {
    ## convert the user specified subset into a usable form
    if (is.logical(subset)) subset <- which(subset)
    subset <- unique(subset[subset >= 1 & subset <= nrow(object$xvar)])

    if (length(subset) == 0) {
      stop("'subset' not set properly.")
    }
  }
  ## subset the x-variable data
  object$xvar <- object$xvar[subset,, drop = FALSE]
 
  ## process the object depending on the underlying family

  ##survival families
  if (grepl("surv", object$family)) {

    ##extract event information
    event.info <- randomForestSRC:::get.event.info(object, subset)
    yvar.dim <- event.info$r.dim
    cens <- event.info$cens
    event.type <- event.info$event.type

    ## assign time if missing
    if (missing(time)) {
      time <- median(event.info$time.interest, na.rm = TRUE)
    }
    
    ## special processing for  CR analysis
    if (object$family == "surv-CR") {
      if (missing(which.outcome)) {
        which.outcome <- 1
      }
      else {
        if (which.outcome < 1 || which.outcome > max(event.type, na.rm = TRUE)) {
          stop("'which.outcome' is specified incorrectly")
        }
      }
      VIMP <- object$importance[, which.outcome]
      surv.type <- setdiff(surv.type, c("mort", "rel.freq", "surv"))
      pred.type <- match.arg(surv.type, c("years.lost", "cif", "chf"))
      ylabel <- switch(pred.type,
           "years.lost" = paste("Years lost for event ", which.outcome),
           "cif" = paste("CIF for event ", which.outcome, " (time=", time, ")", sep = ""),
           "chf" = paste("CHF for event ", which.outcome, " (time=", time, ")", sep = ""))
    }
    ## usual right-censoring setup
    else {
      which.outcome <- 1
      VIMP <- object$importance
      surv.type <- setdiff(surv.type, c("years.lost", "cif", "chf"))
      pred.type <- match.arg(surv.type, c("mort", "rel.freq", "surv"))
      ylabel <- switch(pred.type,
           "mort"      = "mortality",
           "rel.freq"  = "standardized mortality",
           "surv"      = paste("predicted survival (time=", time, ")", sep = ""))
    }
    
  }
  ## all other families
  else {

    ## assign a null time value
    time <- NULL

    ## classification families
    if (object$family == "class") {
      if (missing(which.outcome)) {
        which.outcome <- 1
      }
      else if (is.character(which.outcome)) {
        which.outcome <- match(match.arg(which.outcome, levels(object$yvar)), levels(object$yvar))
      }
      else {
        if (which.outcome > length(levels(object$yvar)) | which.outcome < 1) {
          stop("which.outcome is specified incorrectly:", which.outcome)
        }
      }
      pred.type <- "prob"
      yvar.dim <- 1
      VIMP <- object$importance[, 1 + which.outcome]
      ylabel <- paste("probability", levels(object$yvar)[which.outcome])
    }
    ## regression families
    else {
      pred.type <- "y"
      yvar.dim <- 1
      VIMP <- object$importance
      ylabel <- expression(hat(y))
    }
    
  }

  ### get x-variable matrix (use imputed values if available)
  xvar <- object$xvar
  if (!is.null(object$imputed.indv)) {
    xvar[object$imputed.indv, ] <- object$imputed.data[, -(1:yvar.dim)]
  }
  n <- nrow(xvar)

  ### extract xvar names to be plotted
  ### should xvar be sorted by importance?
  if (missing(xvar.names)) {
    xvar.names <- object$xvar.names
  }
  else {
    if (length(setdiff(xvar.names, object$xvar.names)) >  0){
      stop("x-variable names supplied does not match available list:\n", object$xvar.names)
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

  ## Save par settings
  old.par <- par(no.readonly = TRUE)

  ##--------------------------------------------------------------------------------
  ##
  ## Marginal Plots
  ##
  ##--------------------------------------------------------------------------------
  if (!partial) {
    plots.per.page <- max(round(min(plots.per.page,nvar)), 1)
    granule <- max(round(granule),1)
    par(mfrow = c(min(plots.per.page, ceiling(nvar / plots.per.page)), plots.per.page))
    yhat <- extract.pred(object, pred.type, subset, time, which.outcome)
    if (n > 500) cex.pt <- 0.5 else cex.pt <- 0.75
    for (k in 1:nvar) {
      x <- xvar[, object$xvar.names == xvar.names[k]]
      x.uniq <- unique(x)
      n.x <- length(x.uniq)
      if (!is.factor(x) & n.x > granule) {
        plot(x,
             yhat,
             xlab = xvar.names[k],
             ylab = ylabel,
             type = "n") 
        if (grepl("surv", object$family)) {
          points(x[cens == which.outcome], yhat[cens == which.outcome], pch = 16, col = 4, cex = cex.pt)
          points(x[cens == 0], yhat[cens == 0], pch = 16, cex = cex.pt)
        }
        lines(lowess(x[!is.na(x)], yhat[!is.na(x)]), col = 2, lwd=3)
      }
      else {
        if (is.factor(x)) x <- factor(x, exclude = NULL)          
        boxplot(yhat ~ x, na.action = "na.omit",
                xlab = xvar.names[k],
                ylab = ylabel,
                notch = TRUE,
                outline = FALSE,
                col = "bisque",
                names = rep("", n.x),
                xaxt = "n")
        at.pretty <- unique(round(pretty(1:n.x, min(30, n.x))))
        at.pretty <- at.pretty[at.pretty >= 1 & at.pretty <= n.x]
        axis(1,
             at = at.pretty,
             labels = format(sort(x.uniq)[at.pretty], trim = TRUE, digits = 4),
             tick = TRUE)
      }
    }
  }
  ##--------------------------------------------------------------------------------
  ##
  ## Partial Plots
  ##
  ##--------------------------------------------------------------------------------
  else {
    if (is.null(object$forest)) {
      stop("forest is empty:  re-run rfsrc (grow) call with forest=TRUE")
    }
    plots.per.page <- max(round(min(plots.per.page,nvar)), 1)
    granule <- max(round(granule),1)
    par(mfrow = c(min(plots.per.page, ceiling(nvar/plots.per.page)), plots.per.page))
    baseForest <- object$forest
    class(baseForest) <- c("rfsrc", "partial", class(object)[3])
    if (npts < 1) npts <- 1 else npts <- round(npts)
    for (k in 1:nvar) {
      x <- xvar[, object$xvar.names == xvar.names[k]]
      if (is.factor(x)) x <- factor(x, exclude = NULL)          
      n.x <- length(unique(x))
      if (!is.factor(x) & n.x > npts) {
        x.uniq <- sort(unique(x))[unique(as.integer(seq(1, n.x, length = min(npts, n.x))))]
      }
      else {
        x.uniq <- sort(unique(x))
      }
      n.x <- length(x.uniq)
      if (n.x > 25) cex.pt <- 0.5 else cex.pt <- 0.75
      yhat <- yhat.se <- NULL
      newdata.x <- xvar
      factor.x <- !(!is.factor(x) & (n.x > granule))
      for (l in 1:n.x) {        
        newdata.x[, object$xvar.names == xvar.names[k]] <- rep(x.uniq[l], n)
        pred.temp <- extract.pred(predict.rfsrc(baseForest, newdata.x), pred.type, 1:n, time, which.outcome)
        mean.temp <- mean(pred.temp , na.rm = TRUE)
        if (!factor.x) {
          yhat <- c(yhat, mean.temp)
          if (object$family == "class") {
            yhat.se <- c(yhat.se, mean.temp * (1 - mean.temp) / sqrt(n))
          }
          else {
            yhat.se <- c(yhat.se, sd(pred.temp / sqrt(n) , na.rm = TRUE))
          }
        }
        else {
          pred.temp <- mean.temp + (pred.temp - mean.temp)/sqrt(n)
          yhat <- c(yhat, pred.temp)
        }
      }
      if (!factor.x) {
        plot(c(min(x), x.uniq, max(x), x.uniq, x.uniq),
             c(NA, yhat, NA, yhat + 2 * yhat.se, yhat - 2 * yhat.se),
             xlab = xvar.names[k],
             ylab = ylabel,
             type = "n")
        points(x.uniq, yhat, pch = 16, cex = cex.pt, col = 2)
        if (!is.na(yhat.se) && any(yhat.se > 0)) {
          if (smooth.lines) {
            lines(lowess(x.uniq, yhat + 2 * yhat.se), lty = 3, col = 2)
            lines(lowess(x.uniq, yhat - 2 * yhat.se), lty = 3, col = 2)
          }
          else {
            lines(x.uniq, yhat + 2 * yhat.se, lty = 3, col = 2)
            lines(x.uniq, yhat - 2 * yhat.se, lty = 3, col = 2)
          }
        }
        if (smooth.lines) {
          lines(lowess(x.uniq, yhat), lty = 2, lwd=2)
        }
        else {
          lines(x.uniq, yhat, lty = 2, lwd=2)
        }
        rug(x, ticksize=0.03)
      }
      else {
        y.se <- 0.005
        bxp.call <- boxplot(yhat ~ rep(x.uniq, rep(n, n.x)), range = 2, plot = FALSE)
        boxplot(yhat ~ rep(x.uniq, rep(n, n.x)),
                xlab = xvar.names[k],
                ylab = ylabel,
                notch = TRUE,
                outline = FALSE,
                range = 2,
                ylim = c(min(bxp.call$stats[1,], na.rm=TRUE) * ( 1 - y.se ),
                   max(bxp.call$stats[5,], na.rm=TRUE) * ( 1 + y.se )),
                col = "bisque",
                names = rep("",n.x),
                xaxt = "n")
        at.pretty <- unique(round(pretty(1:n.x, min(30,n.x))))
        at.pretty <- at.pretty[at.pretty >= 1 & at.pretty <= n.x]
        axis(1,
             at = at.pretty,
             labels = format(sort(x.uniq)[at.pretty], trim = TRUE, digits = 4),
             tick = TRUE)
      }
    }
  }
  
  ## Restore par settings
  par(old.par)
  

}

plot.variable <- plot.variable.rfsrc
