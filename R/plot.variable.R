####**********************************************************************
####**********************************************************************
####
####  RANDOM FORESTS FOR SURVIVAL, REGRESSION, AND CLASSIFICATION (RF-SRC)
####  Version 1.0.2
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
####    email:  kogalurshear@gmail.com
####    URL:    http://www.kogalur.com
####    --------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************

#' @title Plot Marginal Effect of Variables
#' 
#' @description Plot the marginal effect of an x-variable on the class 
#' probability (classification), response (regression), mortality (survival), 
#' or the expected years lost (competing risk) from a RF-SRC analysis. Users 
#' can select between marginal (unadjusted, but fast) and partial plots 
#' (adjusted, but slow).
#' 
#' @usage plot.variable(x, xvar.names, 
#' surv.type = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf"), 
#' percentile, time=NULL, which.outcome, partial = FALSE, plots.per.page = 4, granule = 5, 
#' sorted = TRUE, nvar, npts = 25, smooth.lines = FALSE, subset, ...)
#' 
#' plot.variable overrides the \code{\link{plot.variable.rfsrc}} method. The 
#' difference between the methods is the return value from this function is
#' a marginal or partial data object returned from  \code{\link{pred.variable}}
#'
#' 
#' @param x  An object of class (rfsrc, grow) or (rfsrc, predict).
#' @param xvar.names Names of the x-variables to be used.
#' @param surv.type  For survival families, specifies the predicted value.
#' @param percentile For survival families, the percentile of the follow up time used for 
#' plotting predicted survival. Default is the median.
#' @param time For survival families, the follow up time used for plotting predicted 
#' survival. Default is to use the median percentile.
#' @param which.outcome	For classification families, an integer or character value 
#' specifying the class to focus on (defaults to the first class). For competing risk 
#' families, an integer value between 1 and J indicating the event of interest, where J 
#' is the number of event types. The default is to use the first event type.
#' @param partial	Should partial plots be used?
#' @param granule	Integer value controlling whether a plot for a specific variable should 
#' be given as a boxplot or scatter plot. Larger values coerce boxplots.
#' @param sorted Should variables be sorted by importance values.
#' @param nvar Number of variables to be predicted. Default is all.
#' @param npts Maximum number of points used when generating partial predictions for 
#' continuous variables.
#' @param subset Vector indicating which rows of the x-variable matrix x$xvar to use. 
#' All rows are used if not specified.
#' @param ...	Further arguments passed to or from other methods.
#' 
#' @details
#' This function creates data structures suitable for plotting using the \code{\link{plot.variable}} function. 
#' \itemize{
#' \item For regression, the predicted response is used.
#' \item For classification, it is the predicted class probability specified by which.outcome.
#' \item For survival, the choices are:
#' \itemize{
#' \item Mortality (mort).
#' \item Relative frequency of mortality (rel.freq).
#' \item Predicted survival (surv), where the predicted survival is for the time point specified using the percentile 
#' of the follow up time (the default is 50, the median follow up time). Alternatively, a specific follow up 
#' time can also be specified.
#' }
#' \item For competing risks, the choices are:
#'   \itemize{
#'   \item The expected number of life years lost (years.lost).
#'   \item The cumulative incidence function (cif).
#' \item The cumulative hazard function (chf).
#' }
#' In all three cases, the predicted value is for the event type specified by which.outcome. For cif and chf 
#' the quantity is evaluated at the time point specified by percentile or by time.
#' }
#' For partial plots use partial=TRUE. Their interpretation are different than marginal plots. The y-value for 
#' a variable X, evaluated at X=x, is
#' 
#' \tilde{f}(x) = \frac{1}{n} ∑_{i=1}^n \hat{f}(x, x_{i,o}),
#' 
#' where x_{i,o} represents the value for all other variables other than X for individual i and \hat{f} is the 
#' predicted value. Generating partial plots can be very slow. Choosing a small value for npts can speed up 
#' computational times as this restricts the number of distinct x values used in computing \tilde{f}.
#' 
#' For continuous variables, red points are used to indicate partial values and dashed red lines indicate a 
#' smoothed error bar of +/- two standard errors. Black dashed line are the partial values. Set smooth.lines=TRUE 
#' for lowess smoothed lines. For discrete variables, partial values are indicated using boxplots with whiskers 
#' extending out approximately two standard errors from the mean. Standard errors are meant only to be a guide 
#' and should be interpreted with caution.
#' 
#' Partial plots can be slow. Setting npts to a smaller number can help.
#' 
#' @author Hemant Ishwaran \code{hemant.ishwaran@gmail.com} and Udaya B. Kogalur \code{kogalurshear@gmail.com}
#' 
#' @references
#' Friedman J.H. (2001). Greedy function approximation: a gradient boosting machine, Ann. of Statist., 5:1189-1232.
#' Ishwaran H., Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random survival forests, Ann. App. Statist., 2:841-860.
#' Ishwaran H., Gerds, T.A. Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M. (2012). Random survival forests for competing risks.
#'
#'  @seealso \code{\link{rfsrc}}, \code{\link{predict.rfsrc}}, \code{\link{plot.variable}}
#' @examples
#' ## Not run: 
#' ### survival/CR examples
#' 
#' # survival
#' data(veteran, package = "randomForestSRC") 
#' v.obj <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' plot.variable(v.obj, plots.per.page = 3)
#' plot.variable(v.obj, plots.per.page = 2, xvar.names = c("trt", "karno", "age"))
#' plot.variable(v.obj, surv.type = "surv", nvar = 1, percentile = 75)
#' plot.variable(v.obj, surv.type = "surv", nvar = 1, time=5) 
#' plot.variable(v.obj, surv.type = "surv", partial = TRUE, smooth.lines = TRUE)
#' plot.variable(v.obj, surv.type = "rel.freq", partial = TRUE, nvar = 2)
#' 
#' # competing risks
#' data(follic, package = "randomForestSRC")
#' follic.obj <- rfsrc(Surv(time, status) ~ ., follic, nsplit = 3, ntree = 100)
#' plot.variable(follic.obj, which.outcome = 2)
#' 
#' ### regression examples
#' 
#' # airquality
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' plot.variable(airq.obj, partial = TRUE, smooth.lines = TRUE)
#' 
#' # motor trend cars
#' mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
#' plot.variable(mtcars.obj, partial = TRUE, smooth.lines = TRUE)
#' 
#' ### classification example
#' 
#' # iris
#' iris.obj <- rfsrc(Species ~., data = iris)
#' plot.variable(iris.obj, partial = TRUE)
#' 
#' # motor trend cars: predict number of carburetors
#' mtcars2 <- mtcars
#' mtcars2$carb <- factor(mtcars2$carb,
#'                        labels = paste("carb", sort(unique(mtcars$carb))))
#' mtcars2.obj <- rfsrc(carb ~ ., data = mtcars2)
#' plot.variable(mtcars2.obj, partial = TRUE)
#' 
#' ## End(Not run)
#' 
#' 
#' @export plot.variable.ggrfsrc plot.variable
#' 
plot.variable.ggrfsrc <- function(
  x,
  xvar.names,
  surv.type = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf"),
  percentile,
  time=NULL,
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
  call <- match.call()
  ### check that object is interpretable
  ### first rename x to object to avoid confusion with x matrix
  object <- x
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("marginal", "rfsrc"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("partial", "rfsrc"), TRUE) == c(1, 2)) != 2) {
    stop("Function only works for objects of class '(rfsrc, grow)', '(rfsrc, predict)', '(rfsrc, marginal)' or '(rfsrc, partial)'.")
  }
  
  # If we are given a "grow" or "predict" object, then we need to generate a set of plot data.
  if(sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) == 2 |
       sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) == 2){
    
    # Expand out the current call (element [1] is removed, the current call name)
    # into the pred.variable function call. Cool that this works!
    object <- do.call(pred.variable, as.list(call[-1]))
  }
  
  # assign missing values to key options
  if(is.null(object$call$percentile)){
    percentile <- 50
  }else{
    percentile <- object$call$percentile
  }

  ## process the object depending on the underlying family
  ##survival families
  if (grepl("surv", object$family)) {
    event.info <- object$event.info
    yvar.dim <- event.info$r.dim
    cens <- event.info$cens
    event.type <- event.info$event.type
    
    if (percentile > 1) percentile <- percentile / 100
    if (percentile < 0 | percentile > 1) percentile <- 0.5
    
    ## special processing for  CR analysis
    if (object$family == "surv-CR") {
      if(is.null(object$which.outcome)){
        which.outcome <- 1
      }else{
        which.outcome <- object$which.outcome 
      } 
      if (which.outcome < 1 || which.outcome > max(event.type, na.rm = TRUE)) {
        stop("'which.outcome' is specified incorrectly")
      }
      
      surv.type <- setdiff(object$call$surv.type, c("mort", "rel.freq", "surv"))
      pred.type <- match.arg(surv.type, c("years.lost", "cif", "chf"))
      if(is.null(object$call$time)){
        ylabel <- switch(pred.type,
                         "years.lost" = paste("Years lost for event ", which.outcome),
                         "cif" = paste("CIF for event ", which.outcome, " (", round(100 * percentile), "%)", sep = ""),
                         "chf" = paste("CHF for event ", which.outcome, " (", round(100 * percentile), "%)", sep = ""))
      }else{
        ylabel <- switch(pred.type,
                         "years.lost" = paste("Years lost for event ", which.outcome),
                         "cif" = paste("CIF for event ", which.outcome, sep = ""),
                         "chf" = paste("CHF for event ", which.outcome, sep = ""))
      }     
    }
    else {
      which.outcome <- 1
      surv.type <- setdiff(object$call$surv.type, c("years.lost", "cif", "chf"))
      pred.type <- match.arg(object$call$surv.type, c("mort", "rel.freq", "surv"))
      if(is.null(object$call$time)){
        ylabel <- switch(pred.type,
                         "mort"      = "mortality",
                         "rel.freq"  = "standardized mortality",
                         "surv"      = paste("predicted survival (", round(100 * percentile), "%)", sep = ""))
      }else{
        ylabel <- switch(pred.type,
                         "mort"      = "mortality",
                         "rel.freq"  = "standardized mortality",
                         "surv"      = paste("predicted survival", sep = ""))
      }
    }
  }
  ## classification families
  else {
    if (object$family == "class") {
      if (is.null(object$which.outcome)) {
        which.outcome <- 1
      }else{
        which.outcome <- object$which.outcome
      }
      if (is.character(which.outcome)) {
        which.outcome <- match(match.arg(which.outcome, levels(object$yvar)), levels(object$yvar))
      }
      else {
        if (which.outcome > length(levels(object$yvar)) | which.outcome < 1) {
          stop("which.outcome is specified incorrectly: ", which.outcome, " of ", length(levels(object$yvar)) )
        }
      }
      pred.type <- "prob"
      yvar.dim <- 1
      ylabel <- paste("probability", levels(object$yhat)[which.outcome])
    }
    ## regression families
    else {
      pred.type <- "y"
      yvar.dim <- 1
      ylabel <- expression(hat(y))
    }
  }
  
  ## Save par settings
  old.par <- par(no.readonly = TRUE)
  
  ##--------------------------------------------------------------------------------
  ##
  ## Marginal Plots
  ##
  ##--------------------------------------------------------------------------------
  if (is.null(object$call$partial)) {
    xvar <- object$x
    n <- nrow(xvar)
    nvar <- ncol(xvar)
    plots.per.page <- max(round(min(plots.per.page,nvar)), 1)
    granule <- max(round(granule),1)
    par(mfrow = c(min(plots.per.page, ceiling(nvar / plots.per.page)), plots.per.page))
    yhat <- object$yhat
    if (n > 500) cex.pt <- 0.5 else cex.pt <- 0.75
    for (k in 1:nvar) {
      x <- xvar[, k]
      x.uniq <- unique(x)
      n.x <- length(x.uniq)
      if (!is.factor(x) & n.x > granule) {
        plot(x,
             yhat,
             xlab = colnames(xvar)[k],
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
                xlab = colnames(xvar)[k],
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
    nvar <- length(object$partial)
    plots.per.page <- max(round(min(plots.per.page,nvar)), 1)
    if(is.null(object$call$granule))
      granule <- 5
    else
      granule <- max(round(object$call$granule),1)
    
    par(mfrow = c(min(plots.per.page, ceiling(nvar/plots.per.page)), plots.per.page))
    if(is.null(object$call$npts)) 
      npts<- 25
    else
      if (object$call$npts < 1) npts <- 1 else npts <- round(object$call$npts)
    for (k in 1:nvar) {
      x <- object$partial[[k]]$x
      yhat <- object$partial[[k]]$yhat
      yhat.se <- object$partial[[k]]$yhat.se
      x.uniq <- object$partial[[k]]$xhat
      name <- object$partial[[k]]$name
      n.x <- object$partial[[k]]$n.x
      if (n.x > 25) cex.pt <- 0.5 else cex.pt <- 0.75
      factor.x <- !(length(x.uniq) == length(yhat))
      
      if (!factor.x) {
        plot(c(min(x), x.uniq, max(x), x.uniq, x.uniq),
             c(NA, yhat, NA, yhat + 2 * yhat.se, yhat - 2 * yhat.se),
             xlab = name,
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
        n <- length(x)
        y.se <- 0.005
        bxp.call <- boxplot(yhat ~ rep(x.uniq, rep(n, n.x)), range = 2, plot = FALSE)
        boxplot(yhat ~ rep(x.uniq, rep(n, n.x)),
                xlab = name,
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
  
  # return the integrated data. Useful for future calls to plot.variable
  invisible(object)
}

plot.variable <- plot.variable.ggrfsrc
