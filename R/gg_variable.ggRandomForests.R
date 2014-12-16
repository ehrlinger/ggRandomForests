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
#' gg_dta <- gg_variable(iris_rf)
#' plot(gg_dta, xvar="Sepal.Width")
#' plot(gg_dta, xvar="Sepal.Length")
#' 
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' 
#' ## airquality
#' #airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' data(airq_rf, package="ggRandomForests")
#' gg_dta <- gg_variable(airq_rf)
#' plot(gg_dta, xvar="Wind")
#' plot(gg_dta, xvar="Temp")
#' plot(gg_dta, xvar="Solar.R")
#' 
#' ## motor trend cars
#' #mtcars.obj <- rfsrc(mpg ~ ., data = mtcars)
#' data(mtcars_rf, package="ggRandomForests")
#' gg_dta <- gg_variable(mtcars_rf)
#' 
#' # mtcars$cyl is an ordinal variable 
#' gg_dta$cyl <- factor(gg_dta$cyl)
#' plot(gg_dta, xvar="cyl")
#' 
#' # Others are continuous
#' plot(gg_dta, xvar="disp")
#' plot(gg_dta, xvar="hp")
#' plot(gg_dta, xvar="wt")
#' 
#' # panel
#' plot(gg_dta, xvar=c("disp","hp"), panel=TRUE)
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
#' gg_dta <- gg_variable(veteran_rf, time=90)
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime")
#' 
#' # Generate coplots
#' plot(gg_dta, xvar = c("age", "diagtime"), panel=TRUE)
#' 
#' # If we want to compare survival at different time points, say 30, 90 day 
#' # and 1 year
#' gg_dta <- gg_variable(veteran_rf, time=c(30, 90, 365))
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime") 
#' 
#' # Generate coplots
#' plot(gg_dta, xvar =  c("age", "diagtime"), panel=TRUE)
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
  gg_dta <- data.frame(object$xvar)
  
  if(object$family == "regr"){
    if(oob)
      gg_dta$yhat <- object$predicted.oob
    else
      gg_dta$yhat <- object$predicted
    
  }else  if(object$family == "class"){
    if(oob){
      colnames(object$predicted.oob) <- paste("yhat.", colnames(object$predicted.oob),
                                              sep="")
      gg_dta <- cbind(gg_dta, object$predicted.oob)
      
    }else{
      colnames(object$predicted) <- paste("yhat.", colnames(object$predicted),
                                          sep="")
      gg_dta <- object$predicted
    }
    gg_dta$yvar <- object$yvar
    
  }else if(object$family == "surv"){
    gg_dta$cens <- as.logical(object$yvar[,2])
    colnames(gg_dta) <- c(object$xvar.names, "cens")
    
    lng <- length(time)
    for(ind in 1:lng){
      if(ind > 1){
        gg_dta.t.old <- gg_dta.t
      }
      ## For marginal plot.
      # Plot.variable returns the resubstituted survival, not OOB. So we calculate it.
      # Time is really straight forward since survival is a step function
      #
      # Get the event time occuring before or at 1 year. 
      gg_dta.t <- gg_dta
      inTime <-which(object$time.interest> time[ind])[1] -1
      if(inTime == 0)
        stop("The time of interest is less than the first event time. Make sure you are using the correct time units.")
      
      if(oob)
        gg_dta.t$yhat=100*object$survival.oob[,inTime]
      else
        gg_dta.t$yhat=100*object$survival[,inTime]
      
      if(missing(time.labels)){
        gg_dta.t$time <- time[ind]
      }else{
        gg_dta.t$time <- time.labels[ind]
      }
      
      if(ind > 1){
        gg_dta.t<- rbind(gg_dta.t.old, gg_dta.t)
      }    
    }
    
    gg_dta <- gg_dta.t
    gg_dta$time <- factor(gg_dta$time, levels=unique(gg_dta$time))
  }
  class(gg_dta) <- c("gg_variable", object$family, class(gg_dta))
  invisible(gg_dta)
}


gg_variable.rfsrc <- gg_variable.ggRandomForests

gg_variable <- gg_variable.ggRandomForests
