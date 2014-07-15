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
#' Extract the predicted response values from the forest, formatted for
#' convergence plot using \code{\link{plot.gg_rfsrc}}
#' 
#' @param object randomForestSRC object
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
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggrf.obj<- gg_rfsrc(iris.obj)
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
gg_rfsrc.ggRandomForests <- function(object, oob=TRUE, se=NULL, ...) {
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
    dta <- tbl_df(dta)
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
      
       dta <- tbl_df(dta)
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
      dta <- tbl_df(dta)
      class(dta) <- c("survSE", class(dta))
    }
  }
  
  class(dta) <- c("gg_rfsrc",object$family, class(dta))
  invisible(dta)
}

gg_rfsrc <- gg_rfsrc.ggRandomForests
