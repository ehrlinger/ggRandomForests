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

#' @title Predict Marginal Effect of Variables
#' 
#' @description Predict the marginal effect of an x-variable on the class 
#' probability (classification), response (regression), mortality (survival), 
#' or the expected years lost (competing risk) from a RF-SRC analysis. Users 
#' can select between marginal (unadjusted, but fast) and partial plots 
#' (adjusted, but slow).
#' 
#' @usage pred.variable(x, xvar.names, 
#' surv.type = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf"), 
#' percentile, time=NULL, which.outcome, partial = FALSE, plots.per.page = 4, granule = 5, 
#' sorted = TRUE, nvar, npts = 25, smooth.lines = FALSE, subset, ...)
#' 
#' @param x  An object of class (rfsrc, grow) or (rfsrc, predict).
#' @param xvar.names Names of the x-variables to be used.
#' @param surv.type	For survival families, specifies the predicted value.
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
#' The logic for this function was cut out of the \code{\link{plot.variable.rfsrc}} (V 1.0.2) directly. Then 
#' was modified to create the data objects used by \code{\link{plot.variable.ggrfsrc}} and \code{\link{ggplot.variable}}.
#' 
#' The function creates data structures suitable for plotting using the \code{\link{plot.variable}} function. 
#' \itemize{
#' \item For regression, the predicted response is used.
#' \item For classification, it is the predicted class probabilities optionally specified by which.outcome.
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
#' \deqn{\tilde{f}(x) = \frac{1}{n} \sum_{i=1}^n \hat{f}(x, x_{i,o})},
#' 
#' where x_{i,o} represents the value for all other variables other than X for individual i and \deqn{\hat{f}} is the 
#' predicted value. Generating partial plots can be very slow. Choosing a small value for npts can speed up 
#' computational times as this restricts the number of distinct x values used in computing \deqn{\tilde{f}}.
#' 
#' For continuous variables, red points are used to indicate partial values and dashed red lines indicate a 
#' smoothed error bar of +/- two standard errors. Black dashed line are the partial values. Set smooth.lines=TRUE 
#' for lowess smoothed lines. For discrete variables, partial values are indicated using boxplots with whiskers 
#' extending out approximately two standard errors from the mean. Standard errors are meant only to be a guide 
#' and should be interpreted with caution.
#' 
#' Partial plots can be slow. Setting npts to a smaller number can help.
#' 
#' @author Hemant Ishwaran \code{hemant.ishwaran@gmail.com} and Udaya B. Kogalur \code{kogalurshear@gmail.com},
#'  ggrfsrc modifications by John Ehrlinger \code{john.ehrlinger@gmail.com}
#' 
#' @references
#' Friedman J.H. (2001). Greedy function approximation: a gradient boosting machine, Ann. of Statist., 5:1189-1232.
#' Ishwaran H., Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random survival forests, Ann. App. Statist., 2:841-860.
#' Ishwaran H., Gerds, T.A. Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M. (2012). Random survival forests for competing risks.
#'
#'  @seealso \code{\link{rfsrc}}, \code{\link{predict.rfsrc}}, \code{\link{plot.variable.rfsrc}}, \code{\link{plot.variable}}
#' 
#' @export pred.variable.ggrfsrc pred.variable
#' 
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

pred.variable.ggrfsrc <- function(
  x,
  xvar.names,
  surv.type = c("mort", "rel.freq", "surv", "years.lost", "cif", "chf"),
  percentile,
  time=NULL,
  which.outcome,
  partial = FALSE,
  granule = 5,
  sorted = TRUE,
  nvar,
  npts = 25,
  subset,
  ...)
{
  call <- match.call()
  ### check that object is interpretable
  ### first rename x to object to avoid confusion with x matrix
  object <- x
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("Function only works for objects of class `(rfsrc, grow)', '(rfsrc, predict)'.")
  }
  
  # assign missing values to key options
  if (missing(percentile)) percentile <- 50
  
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
  
  event.info <- NULL
  
  # Check for invalid time spec (<=0)
  if(!is.null(time)){
    if(time <= 0){ 
      time=NULL
      warning("Time argument must be greater than 0, using percentile predictions")
    }
  }
  
  ## process the object depending on the underlying family
  
  ##survival families
  if (grepl("surv", object$family)) {
    event.info <- randomForestSRC:::get.event.info(object, subset)
    yvar.dim <- event.info$r.dim
    cens <- event.info$cens
    event.type <- event.info$event.type
    if (percentile > 1) percentile <- percentile / 100
    if (percentile < 0 | percentile > 1) percentile <- 0.5
    
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
    }
    else {
      which.outcome <- 1
      VIMP <- object$importance
      surv.type <- setdiff(surv.type, c("years.lost", "cif", "chf"))
      pred.type <- match.arg(surv.type, c("mort", "rel.freq", "surv"))
    }
  }
  ## classification families
  else {
    if (object$family == "class") {
      if (missing(which.outcome)) {
        which.outcome <- NULL
      }
      else if (is.character(which.outcome)) {
        which.outcome <- match(match.arg(which.outcome, levels(object$yvar)), levels(object$yvar))
      }
      else {
        if (which.outcome > length(levels(object$yvar))) {
          stop("which.outcome is specified incorrectly: ", which.outcome)
        }
      }
      pred.type <- "prob"
      yvar.dim <- 1
      VIMP <- object$importance[, which.outcome]
    }
    ## regression families
    else {
      pred.type <- "y"
      yvar.dim <- 1
      VIMP <- object$importance
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
  
  ##--------------------------------------------------------------------------------
  ##
  ## Marginal Plots
  ##
  ##--------------------------------------------------------------------------------
  if (!partial) {
    yhat <- randomForestSRC:::extract.pred(object, pred.type, subset, percentile, time, which.outcome)
    
    # Create storage data.frame
    x <- data.frame(matrix(ncol=nvar, nrow=n))
    colnames(x) <- xvar.names
    
    # Sort results into the ordered data.frame by column name
    for (k in 1:nvar) {
      x[,k] <- xvar[, object$xvar.names == xvar.names[k]]
      x.uniq <- unique(x[,k])
      n.x <- length(x.uniq)
    }
    
    # Create the data structure for the return object
    mrgn <- list(call=call, family=object$family,yhat=yhat, x=x)
    if(!is.null(event.info))
      mrgn$event.info <- event.info
    if(!missing(which.outcome))
      mrgn$which.outcome <- which.outcome
    if(object$family == "class")
      mrgn$yvar <- object$yvar
    
    
    
    class(mrgn) <- c("marginal", "rfsrc")
    invisible(mrgn)
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
    granule <- max(round(granule),1)
    baseForest <- object$forest
    class(baseForest) <- c("rfsrc", "partial", class(object)[3])
    if (npts < 1) npts <- 1 else npts <- round(npts)
    
    # Create the return object as a list (of lists)
    prtl <- vector("list", length=nvar)
    
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
      yhat <- yhat.se <- NULL
      newdata.x <- xvar
      factor.x <- !(!is.factor(x) & (n.x > granule))
      for (l in 1:n.x) {        
        newdata.x[, object$xvar.names == xvar.names[k]] <- rep(x.uniq[l], n)
        pred.temp <- randomForestSRC:::extract.pred(predict.rfsrc(baseForest, newdata.x), pred.type, 1:n, percentile, time, which.outcome)
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
  
      prtl[[k]] <- list(name=xvar.names[k], yhat=yhat, yhat.se=yhat.se, n.x = n.x, xhat=x.uniq, x=x)
    }
    
    mrgn <- list(call=call, family=object$family, partial=prtl)
    
    if(!is.null(event.info))
      mrgn$event.info <- event.info
    if(!missing(which.outcome))
      mrgn$which.outcome <- which.outcome
    
    mrgn$yvar <- object$yvar
    
    class(mrgn) <- c("partial", "rfsrc")
    invisible(mrgn)
  }
}

pred.variable <- pred.variable.ggrfsrc


