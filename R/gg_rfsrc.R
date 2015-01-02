####**********************************************************************
####**********************************************************************
####
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
#'
#' Predicted response data object
#' 
#' Extracts the predicted response values from the \code{randomForestSRC::rfsrc} object, 
#' and formats data for plotting the response using \code{\link{plot.gg_rfsrc}}.
#' 
#' @param object \code{randomForestSRC::rfsrc} object
#' @param ... extra arguments
#' 
#' @return \code{gg_rfsrc} object
#' 
#' @details 
#'    surv_type ("surv", "chf", "mortality", "hazard") for survival forests
#'    
#'    oob boolean, should we return the oob prediction , or the full
#' forest prediction.
#' 
#' 
#' @seealso \code{\link{plot.gg_rfsrc}} \code{rfsrc} \code{plot.rfsrc} \code{\link{gg_survival}}
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_iris)
#' 
#' plot.gg_rfsrc(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_airq)
#' 
#' plot.gg_rfsrc(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' data(rfsrc_veteran, package = "ggRandomForests")
#' gg_dta <- gg_rfsrc(rfsrc_veteran)
#' plot(gg_dta)
#' plot(gg_dta, level=.68)
#' 
#' @aliases gg_rfsrc gg_rfsrc.rfsrc
#' @export gg_rfsrc.rfsrc gg_rfsrc
#'
#'
gg_rfsrc <- function (object, ...) {
  UseMethod("gg_rfsrc", object)
}

gg_rfsrc.rfsrc <- function(object, 
                           ...) {
  
  
  ##!!TODO!! Stratified predictions...
  
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(object$forest)) {
    stop("The function requires the \"forest = TRUE\" attribute when growing the randomForest")
  }
  oob <- TRUE
  # get optional arguments
  arg_list <- list(...)
  
  if(!is.null(arg_list$oob)) oob <- arg_list$oob
  if (inherits(object, "predict")){
    oob <- FALSE
  }
  
  if(object$family == "class"){
    ### Classification models...
    
    # Need to add multiclass methods
    if(oob){
      gg_dta <- 
      if(ncol(object$predicted.oob) <= 2){
        data.frame(cbind(object$predicted.oob[,-1]))
      }else{ 
        data.frame(cbind(object$predicted.oob))
      }
    }else{
      gg_dta <- if(ncol(object$predicted) <= 2){
        data.frame(cbind(object$predicted[,-1]))
      }else{ 
        data.frame(cbind(object$predicted))
      }
    }
    if(ncol(gg_dta) == 1){
      colnames(gg_dta)<- object$yvar.names
      # Force this to logical return value... 
      #
      # This may be a bug in rfsrc, as it converts all classification models
      # into factors.
      gg_dta$y = as.logical(as.numeric(object$yvar)-1)
    }else{
      colnames(gg_dta) <- levels(object$yvar)
      gg_dta$y <- object$yvar
      
    }
    
  }else if(object$family == "surv"){
    
    ### Survival models
    surv_type <- "surv"
    
    if(!is.null(arg_list$surv_type)) surv_type = arg_list$surv_type
    
    if(oob){
      rng<-switch(surv_type,
                  surv=data.frame(object$survival.oob),
                  chf=data.frame(object$chf.oob),
                  mortality =data.frame(1-object$survival.oob),
                  stop(paste(surv_type, " not implemented at this time"))
      )
    }else{
      rng<-switch(surv_type,
                  surv=data.frame(object$survival),
                  chf=data.frame(object$chf),
                  mortality =data.frame(1-object$survival),
                  stop(paste(surv_type, " not implemented at this time"))
      )
    }
    
    # Do we want all lines, or bootstrap confidence bands.
    colnames(rng) <- object$time.interest
    
    rng$ptid <- 1:nrow(rng)
    rng$cens <- as.logical(object$yvar[,2])
    gg_dta <- rng
  }else if(object$family == "regr"){
    
    # Need to add multiclass methods
    if(oob){
      gg_dta <- data.frame(cbind(object$predicted.oob, object$yvar))
    }else{
      gg_dta <- data.frame(cbind(object$predicted, object$yvar))
    }
    
    colnames(gg_dta) <- c("yhat", object$yvar.names)
  }else{
    stop(paste("Plotting for ", object$family, " randomForestSRC is not yet implemented.", sep=""))
  }
  
  gg_dta <- list(yhat=gg_dta, xvar=object$xvar, family=object$family)
  
  class(gg_dta) <- c("gg_rfsrc", class(gg_dta))
  invisible(gg_dta)
}
