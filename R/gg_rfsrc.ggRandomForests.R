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
#' gg_rfsrc.ggRandomForests
#' 
#' Extract the predicted response values from the forest, formatted for
#' convergence plot using \code{\link{plot.gg_rfsrc}}.
#' 
#' @param object randomForestSRC object
#' @param surv_type ("surv", "chf", "mortality", "hazard")
#' @param oob boolean, should we return the oob prediction , or the full
#' forest prediction.
#' @param se for survival forests, calculated the se bootstrap confidence 
#' interval
#' @param ... not used
#' 
#' @return gg_rfsrc object formatted for \code{\link{plot.gg_rfsrc}}
#' 
#' @export gg_rfsrc.ggRandomForests gg_rfsrc
#' 
#' @seealso \code{\link{plot.gg_rfsrc}} \code{rfsrc} \code{plot.rfsrc}
#' 
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' # iris.obj <- rfsrc(Species ~ ., data = iris)
#' data(iris_rf, package="ggRandomForests")
#' ggrf.obj<- gg_rfsrc(iris_rf)
#' plot(ggrf.obj)
#' 
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- gg_rfsrc(v.obj, se=.95)
#' plot(ggrf.obj)
#'
#' @aliases gg_rfsrc
#'
#' @importFrom dplyr tbl_df select
#' 
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
      dta <- 
        if(dim(object$predicted.oob)[2] <= 2){
          data.frame(cbind(object$predicted.oob[,-1]))
        }else{ 
          data.frame(cbind(object$predicted.oob))
        }
    }else{
      dta <- if(dim(object$predicted)[2] <= 2){
        data.frame(cbind(object$predicted[,-1]))
      }else{ 
        data.frame(cbind(object$predicted))
      }
    }
    if(dim(dta)[2] == 1){
      colnames(dta)<- object$yvar.names
      # Force this to logical return value... 
      #
      # This may be a bug in rfsrc, as it converts all classification models
      # into factors.
      dta$y = as.logical(as.numeric(object$yvar)-1)
    }else{
      colnames(dta) <- levels(object$yvar)
      dta$y <- object$yvar
      
    }
    
    # Easier reading data.frames (dplyr)
    dta <- tbl_df(dta)
    
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
      
      # Easier reading data.frames (dplyr)
      dta <- tbl_df(rng)
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
      dta<-data.frame(cbind(object$time.interest,t(rng),mn))
      if(dim(dta)[2] == 5){
        colnames(dta)<- c("time", "lower",  "upper", "median", "mean")
      }else{
        colnames(dta)<- c("time", se.set, "mean")
      }
      # Easier reading data.frames (dplyr)
      dta <- tbl_df(dta)
      class(dta) <- c("survSE", surv_type, class(dta))
    }
    class(dta) <- c(surv_type, class(dta))
    
  }else if(object$family == "regr"){
   
    # Need to add multiclass methods
    if(oob){
      dta <- data.frame(cbind(object$predicted.oob, object$yvar))
    }else{
      dta <- data.frame(cbind(object$predicted, object$yvar))
    }
   
    colnames(dta) <- c("yhat", object$yvar.names)
    
    # Easier reading data.frames (dplyr)
    dta <- tbl_df(dta)
  }else{
    stop(paste("Plotting for ", object$family, " randomForestSRC is not yet implemented.", sep=""))
  }
  
  class(dta) <- c("gg_rfsrc", object$family, class(dta))
  invisible(dta)
}

gg_rfsrc <- gg_rfsrc.ggRandomForests
