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
#' plot.ggRFsrc
#' Plot a \link{\code{ggRFsrc}} object, the cumulative OOB error rates of the forest as a function of number of trees.
#' 
#' @param x ggRFsrc object created from a randomForestSRC object
#' 
#' @return ggplot object
#' 
#' @export plot.ggRFsrc
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggrf.obj<- ggRFsrc(iris.obj)
#' 
#' plot.ggRFsrc(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- ggRFsrc(v.obj)
#' plot(ggrf.obj)
#'
### error rate plot
plot.ggRFsrc<- function(obj, ...){
  
  if(class(obj)[1] == "rfsrc") obj<- ggRFsrc(obj, ...)
  
  if("class" %in% class(obj)){
    gDta <- ggplot(obj)+
      geom_jitter(aes(x=1,y=100*yhat, color=factor(y),shape=factor(y)), alpha=.5)+
      geom_boxplot(aes(x=1,y=100*yhat), outlier.colour = "transparent", fill="transparent", notch = TRUE)
    
  }
  if("surv" %in% class(obj)){
    if("survSE" %in% class(obj)){
      gDta <- ggplot(obj)+
        geom_ribbon(aes(x=time, ymin=lower, ymax=upper), alpha=.25)+
        geom_step(aes(x=time, y=median), col="green") + 
        geom_step(aes(x=time, y=mean), col="red")
      
    }else{
      gDta <- ggplot(obj)+
        geom_step(aes(x=variable, y=value, col=!event, by=ptid), alpha=.3, size=.1)
    }
    gDta  +
      labs(x="time (years)", y="OOB Survival (%)")
  }
  return(gDta)
}
