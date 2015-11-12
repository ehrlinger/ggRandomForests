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
#' The \code{gg_variable} function operates on a \code{\link[randomForestSRC]{rfsrc}} object, 
#' or the output from the \code{\link[randomForestSRC]{plot.variable}} function.
#' 
#' @description \code{\link[randomForestSRC]{plot.variable}} generates a \code{data.frame}
#'  containing the marginal variable dependance or the partial variable dependence. 
#'  The \code{gg_variable} function creates a \code{data.frame} of containing the 
#'  full set of covariate data (predictor variables) and the predicted response for 
#'  each observation. Marginal dependence figures are created using the 
#'  \code{\link{plot.gg_variable}} function.
#' 
#' @param object a \code{\link[randomForestSRC]{rfsrc}} object 
#' @param time point (or vector of points) of interest (for survival forests only)
#' @param time.labels If more than one time is specified, a vector of time.labels 
#' for differentiating the time points (for survival forests only)
#' @param oob indicate if predicted results should include oob or full data set.
#' @param ... extra arguments 
#'  
#' @return \code{gg_variable} object
#' 
#' @seealso  \code{\link{plot.gg_variable}} \code{\link[randomForestSRC]{plot.variable}}
#' 
#' @aliases gg_variable gg_variable.rfsrc
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## iris
#' #rfsrc_iris <- rfsrc(Species ~., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' 
#' gg_dta <- gg_variable(rfsrc_iris)
#' plot(gg_dta, xvar="Sepal.Width")
#' plot(gg_dta, xvar="Sepal.Length")
#' 
#' plot(gg_dta, xvar=rfsrc_iris$xvar.names, 
#'      panel=TRUE) # , se=FALSE)
#' 
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- air quality data
#' #rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta <- gg_variable(rfsrc_airq)
#' 
#' # an ordinal variable 
#' gg_dta[,"Month"] <- factor(gg_dta[,"Month"])
#' 
#' plot(gg_dta, xvar="Wind")
#' plot(gg_dta, xvar="Temp")
#' plot(gg_dta, xvar="Solar.R")
#' 
#' 
#' plot(gg_dta, xvar=c("Solar.R", "Wind", "Temp", "Day"), panel=TRUE)
#' 
#' plot(gg_dta, xvar="Month", notch=TRUE)
#' }
#' \dontrun{
#' ## -------- motor trend cars data
#' #rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#' data(rfsrc_mtcars, package="ggRandomForests")
#' gg_dta <- gg_variable(rfsrc_mtcars)
#' 
#' # mtcars$cyl is an ordinal variable 
#' gg_dta$cyl <- factor(gg_dta$cyl)
#' gg_dta$am <- factor(gg_dta$am)
#' gg_dta$vs <- factor(gg_dta$vs)
#' gg_dta$gear <- factor(gg_dta$gear)
#' gg_dta$carb <- factor(gg_dta$carb)
#' 
#' plot(gg_dta, xvar="cyl")
#' 
#' # Others are continuous
#' plot(gg_dta, xvar="disp")
#' plot(gg_dta, xvar="hp")
#' plot(gg_dta, xvar="wt")
#' 
#' 
#' # panels
#' plot(gg_dta,xvar=c("disp","hp", "drat", "wt", "qsec"),  panel=TRUE)
#' plot(gg_dta, xvar=c("cyl", "vs", "am", "gear", "carb"), panel=TRUE, notch=TRUE)
#' }
#' ## -------- Boston data
#' 
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- veteran data
#' ## survival
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' data(rfsrc_veteran, package="ggRandomForests")
#' 
#' # get the 1 year survival time.
#' gg_dta <- gg_variable(rfsrc_veteran, time=90)
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' plot(gg_dta, xvar = "diagtime", )
#' 
#' # Generate coplots
#' plot(gg_dta, xvar = c("age", "diagtime"), panel=TRUE, se=FALSE)
#' 
#' # If we want to compare survival at different time points, say 30, 90 day 
#' # and 1 year
#' gg_dta <- gg_variable(rfsrc_veteran, time=c(30, 90, 365))
#' 
#' # Generate variable dependance plots for age and diagtime
#' plot(gg_dta, xvar = "age")
#' }
#' ## -------- pbc data

#' @export
gg_variable.rfsrc <- function(object,
                              time,
                              time.labels,
                              oob=TRUE,
                              ...){
  
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
    if (oob)
      gg_dta$yhat <- object$predicted.oob
    else
      gg_dta$yhat <- object$predicted
    
  }else if(object$family == "class"){
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
    for (ind in 1:lng){
      if (ind > 1){
        gg_dta_t_old <- gg_dta_t
      }
      ## For marginal plot.
      # Plot.variable returns the resubstituted survival, not OOB. So we calculate it.
      # Time is really straight forward since survival is a step function
      #
      # Get the event time occuring before or at 1 year. 
      gg_dta_t <- gg_dta
      in_time <-which(object$time.interest> time[ind])[1] -1
      if(in_time == 0)
        stop("The time of interest is less than the first event time. Make sure you are using the correct time units.")
      
      if (oob)
        gg_dta_t$yhat <- object$survival.oob[,in_time]
      else
        gg_dta_t$yhat <- object$survival[,in_time]
      
      if(missing(time.labels)){
        gg_dta_t$time <- time[ind]
      }else{
        gg_dta_t$time <- time.labels[ind]
      }
      
      if(ind > 1){
        gg_dta_t <- rbind(gg_dta_t_old, gg_dta_t)
      }    
    }
    
    gg_dta <- gg_dta_t
    gg_dta$time <- factor(gg_dta$time, levels=unique(gg_dta$time))
  }
  class(gg_dta) <- c("gg_variable", object$family, class(gg_dta))
  invisible(gg_dta)
}
#'@export
# gg_variable <- function (object, ...) {
#   UseMethod("gg_variable", object)
# }
gg_variable <- gg_variable.rfsrc
