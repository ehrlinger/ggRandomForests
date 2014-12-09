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
#' @param surv_type ("surv", "chf", "mortality", "hazard") for survival forests
#' @param oob boolean, should we return the oob prediction , or the full
#' forest prediction.
#' @param se for survival forests, calculated the se bootstrap confidence 
#' interval
#' @param ... not used
#' 
#' @return \code{gg_rfsrc} object formatted for \code{\link{plot.gg_rfsrc}}
#' 
#' @export gg_rfsrc.ggRandomForests gg_rfsrc
#' 
#' @seealso \code{\link{plot.gg_rfsrc}} \code{rfsrc} \code{plot.rfsrc} \code{\link{gg_survival}}
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' data(iris_rf, package="ggRandomForests")
#' ggrf<- gg_rfsrc(iris_rf)
#' 
#' plot.gg_rfsrc(ggrf)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' # airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' data(airq_rf, package="ggRandomForests")
#' ggrf<- gg_rfsrc(airq_rf)
#' 
#' plot.gg_rfsrc(ggrf)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRCM")
#' # veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' data(veteran_rf, package = "ggRandomForests")
#' ggrf <- gg_rfsrc(veteran_rf)
#' plot(ggrf)
#' 
#' @aliases gg_rfsrc
#'
#' @importFrom dplyr select
gg_rfsrc.ggRandomForests <- function(object, 
                                     surv_type=c("surv", "chf", "mortality", "hazard"), 
                                     oob=TRUE, 
                                     se, 
                                     ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(object$forest)) {
    stop("The function requires the \"forest = TRUE\" attribute when growing the randomForest")
  }
  
  #---
  # Unpack the elipse argument.
  # arg_list <- list(...)
  #---
  
  if(object$family == "class"){
    ### Classification models...
    
    # Need to add multiclass methods
    if(oob){
      gg_dta <- 
        if(dim(object$predicted.oob)[2] <= 2){
          data.frame(cbind(object$predicted.oob[,-1]))
        }else{ 
          data.frame(cbind(object$predicted.oob))
        }
    }else{
      gg_dta <- if(dim(object$predicted)[2] <= 2){
        data.frame(cbind(object$predicted[,-1]))
      }else{ 
        data.frame(cbind(object$predicted))
      }
    }
    if(dim(gg_dta)[2] == 1){
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
    surv_type = match.arg(surv_type)
    
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
    if(missing(se)){
      colnames(rng) <- object$time.interest
      
      rng$ptid <- 1:dim(rng)[1]
      rng$cens <- as.logical(object$yvar[,2])
      gg_dta <- rng
    }else{
      # If we have one value, then it's two sided.
      if(length(se) ==1 ){
        if(se > 1)
          se <- se/100
        
        se.set <- c((1- se)/2, 1-(1-se)/2)
        se.set <- sort(se.set) 
      }else{
        se.set <- sort(se) 
      }
      
      rng <-sapply(1:dim(rng)[2], 
                   function(tPt){quantile(rng[,tPt],probs=c(se.set, .5) )})
      mn <- sapply(1:dim(rng)[2], function(tPt){mean(rng[,tPt])})
      
      gg_dta <- data.frame(cbind(object$time.interest,t(rng),mn))
      
      if(dim(gg_dta)[2] == 5){
        colnames(gg_dta)<- c("time", "lower",  "upper", "median", "mean")
      }else{
        colnames(gg_dta)<- c("time", se.set, "mean")
      }
      class(gg_dta) <- c("survSE", surv_type, class(gg_dta))
    }
    class(gg_dta) <- c(surv_type, class(gg_dta))
    
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
  
  class(gg_dta) <- c("gg_rfsrc", object$family, class(gg_dta))
  invisible(gg_dta)
}

gg_rfsrc <- gg_rfsrc.ggRandomForests
