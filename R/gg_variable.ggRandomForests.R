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
#' Marginal variable depedance data object.
#' 
#' @details The marginal variable dependence is determined by comparing relation
#' between the predicted response from the randomforest and a covariate of interest.
#' 
#' The \code{gg_variable} function operates on a \code{randomForestSRC::rfsrc} object, 
#' or the output from the \code{randomForestSRC::plot.variable} function.
#' 
#' @description \code{randomForestSRC::plot.variable} generates a \code{data.frame}
#'  containing the marginal variable dependance or the partial variable dependence. 
#'  The \code{gg_variable} function creates a \code{data.frame} of containing the 
#'  full set of covariate data (predictor variables) and the predicted response for 
#'  each observation. Marginal dependence figures are created using the 
#'  \code{\link{plot.gg_variable}} function.
#' 
#' @param object a \code{randomForestSRC::rfsrc} object 
#' @param time point (or vector of points) of interest (for survival forests only)
#' @param time.labels If more than one time is specified, a vector of time.labels 
#' for differentiating the time points (for survival forests only)
#' @param oob indicate if predicted results should include oob or full data set.
#' @param ... extra arguments 
#'  
#' @return A matrix for creating the marginal variable dependence plots.
#' 
#' @seealso  \code{\link{plot.gg_variable}} \code{randomForestSRC::plot.variable}
#' 
#' @export gg_variable.ggRandomForests gg_variable.rfsrc
#' @export gg_variable
#' 
#' @aliases gg_variable gg_variable.rfsrc
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' 
#' ## iris
#' #iris.obj <- rfsrc(Species ~., data = iris)
#' data(iris_rf, package="ggRandomForests")
#' 
#' ggrf <- gg_variable(iris_rf)
#' plot(ggrf, x_var="Sepal.Width")
#' plot(ggrf, x_var="Sepal.Length")
#' 
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' 
#' ## airquality
#' #airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' data(airq_rf, package="ggRandomForests")
#' ggrf <- gg_variable(airq_rf)
#' plot(ggrf, x_var="Wind")
#' plot(ggrf, x_var="Temp")
#' plot(ggrf, x_var="Solar.R")
#' 
#' ## motor trend cars
#' #mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
#' data(mtcars_rf, package="ggRandomForests")
#' ggrf <- gg_variable(mtcars_rf)
#' 
#' # mtcars$cyl is an ordinal variable
#' plot(ggrf, x_var="cyl")
#' 
#' # Others are continuous
#' plot(ggrf, x_var="disp")
#' plot(ggrf, x_var="hp")
#' plot(ggrf, x_var="wt")
#' 
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' 
#' ## survival
#' # data(veteran, package = "randomForestSRC")
#' # veteran_rf <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' data(veteran_rf, package="ggRandomForests")
#' 
#' # get the 1 year survival time.
#' ggrf <- gg_variable(veteran_rf, time=30)
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(ggrf, x_var = "age")
#' plot(ggrf, x_var = "diagtime")
#' 
#' # If we want to compare survival at different time points, say 30, 90 day 
#' # and 1 year
#' ggrf <- gg_variable(veteran_rf, time=c(30, 90, 365))
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(ggrf, x_var = "age")
#' plot(ggrf, x_var = "diagtime") 
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
  class(pDat) <- c("gg_variable", object$family, class(pDat))
  invisible(pDat)
}


gg_variable.rfsrc <- gg_variable.ggRandomForests

gg_variable <- gg_variable.ggRandomForests
