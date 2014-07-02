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
#' @param rfObj randomForestSRC object
#' @param oob 
#' @param ...
#' 
#' @return ggRFsrc object
#' 
#' @export ggRFsrc.ggRandomForests ggRFsrc
#' 
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
ggRFsrc.ggRandomForests <- function(rfObj, oob=TRUE, se=NULL, ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(rfObj, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(rfObj$forest)) {
    stop("The function requires the \"forest = TRUE\" attribute when growing the randomForest")
  }
  
  if(rfObj$family == "class"){
    
    # Need to add multiclass methods
    if(oob){
      dta <- data.frame(cbind(100*rfObj$predicted.oob[,-1]))
    }else{
      dta <- data.frame(cbind(100*rfObj$predicted[,-1]))
    }
    if(dim(dta)[2] == 1){
      colnames(dta)<- "yhat"
      dta$y = as.logical(as.numeric(rfObj$yvar)-1)
      
    }else{
      dta$y <- rfObj$yvar
    }
    colnames(dta)[-1] <- rfObj$yvar.names
  }else if(rfObj$family == "surv"){
    if(is.null(se)){
      if(oob){
        rng<-data.frame(100*rfObj$survival.oob)
      }else{
        rng<-data.frame(100*rfObj$survival)
      }
      
      colnames(rng) <- rfObj$time.interest
      
      rng$ptid <- 1:dim(rng)[1]
      rng$cens <- as.logical(rfObj$yvar[,2])
      
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
      rng<-sapply(1:dim(rfObj$survival.oob)[2], 
                  function(tPt){quantile(rfObj$survival.oob[,tPt],probs=c(se.set, .5) )})
      mn <- sapply(1:dim(rfObj$survival.oob)[2], function(tPt){mean(rfObj$survival.oob[,tPt])})
      dta<-data.frame(cbind(rfObj$time.interest,100*t(rng),100* mn))
      colnames(dta)<- c("time", "lower",  "upper", "median", "mean")
      class(dta) <- c("survSE", class(dta))
    }
  }
  
  class(dta) <- c("ggRFsrc",rfObj$family, class(dta))
  invisible(dta)
}

ggRFsrc <- ggRFsrc.ggRandomForests
