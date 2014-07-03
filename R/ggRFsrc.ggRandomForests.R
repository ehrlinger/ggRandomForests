####**********************************************************************
####**********************************************************************
####
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
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' ggRFsrc.ggRandomForests
#' Extract the predicted response values from the forest
#' 
#' @param object randomForestSRC object
#' @param oob boolean, should we return the oob prediction , or the full
#' forest prediction.
#' @param se for survival forests, calculated the se bootstrap confidence 
#' interval
#' @param ... not used
#' 
#' @return ggRFsrc object
#' 
#' @export ggRFsrc.ggRandomForests ggRFsrc
#' 
#' @seealso \code{\link{plot.ggRFsrc}} rfsrc
#' 
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggrf.obj<- ggRFsrc(iris.obj)
#' plot(ggrf.obj)
#' 
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- ggRFsrc(v.obj, se=.95)
#' plot(ggrf.obj)
#'
#' @aliases ggRFsrc
#'  
ggRFsrc.ggRandomForests <- function(object, oob=TRUE, se=NULL, ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(object, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(object$forest)) {
    stop("The function requires the \"forest = TRUE\" attribute when growing the randomForest")
  }
  
  if(object$family == "class"){
    
    # Need to add multiclass methods
    if(oob){
      
      dta <- 
        if(dim(object$predicted.oob)[2] == 2){
          data.frame(cbind(100*object$predicted.oob[,-1]))
        }else{ 
          data.frame(cbind(100*object$predicted.oob))
        }
    }else{
      dta <- if(dim(object$predicted.oob)[2] == 2){
        data.frame(cbind(100*object$predicted[,-1]))
      }else{ 
        data.frame(cbind(100*object$predicted))
      }
    }
    if(dim(dta)[2] == 1){
      colnames(dta)<- object$yvar.names
      dta$y = as.logical(as.numeric(object$yvar)-1)
    }else{
      colnames(dta) <- levels(object$yvar)
      dta$y <- object$yvar
      
    }
    
  }else if(object$family == "surv"){
    if(is.null(se)){
      if(oob){
        rng<-data.frame(100*object$survival.oob)
      }else{
        rng<-data.frame(100*object$survival)
      }
      
      colnames(rng) <- object$time.interest
      
      rng$ptid <- 1:dim(rng)[1]
      rng$cens <- as.logical(object$yvar[,2])
      
      dta <- melt(rng, id.vars = c("ptid", "cens"))
      dta$variable <- as.numeric(as.character(dta$variable))
      dta$ptid <- factor(dta$ptid)
    }else{
      # If we have one value, then it's two sided.
      if(length(se) ==1 ){
        if(se > 1)
          se <- se/100
        
        se.set <- c((1- se)/2, 1-(1-se)/2)
        se.set <- sort(se.set) 
      }
      rng<-sapply(1:dim(object$survival.oob)[2], 
                  function(tPt){quantile(object$survival.oob[,tPt],probs=c(se.set, .5) )})
      mn <- sapply(1:dim(object$survival.oob)[2], function(tPt){mean(object$survival.oob[,tPt])})
      dta<-data.frame(cbind(object$time.interest,100*t(rng),100* mn))
      colnames(dta)<- c("time", "lower",  "upper", "median", "mean")
      class(dta) <- c("survSE", class(dta))
    }
  }
  
  class(dta) <- c("ggRFsrc",object$family, class(dta))
  invisible(dta)
}

ggRFsrc <- ggRFsrc.ggRandomForests
