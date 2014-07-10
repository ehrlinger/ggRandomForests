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
#' Plot a \code{\link{ggRFsrc}} object, the forest prediction, possibly 
#' using an OOB estimator from the forest.
#'  
#' @param x ggRFsrc object created from a randomForestSRC object
#' @param ... arguments passed to \code{\link{ggRFsrc}}.
#' @return ggplot object
#' 
#' @export plot.ggRFsrc
#' 
#' @seealso \code{\link{ggRFsrc}} rfsrc
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for 
#' R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression 
#' and Classification (RF-SRC), R package version 1.4.
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
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes aes_string geom_step geom_ribbon labs geom_point geom_smooth geom_jitter geom_boxplot

### error rate plot
plot.ggRFsrc<- function(x, ...){
  obj <- x
  if(class(obj)[1] == "rfsrc") obj<- ggRFsrc(obj, ...)
  
  if(inherits(obj, "class")){
    if(dim(obj)[2] < 3){
      
      gDta <- ggplot(obj)+
        geom_jitter(aes_string(x=1, y=colnames(obj)[1],
                               color=colnames(obj)[2],
                               shape=colnames(obj)[2]), alpha=.5)+
        geom_boxplot(aes_string(x=1, y=colnames(obj)[1]),
                     outlier.colour = "transparent", fill="transparent", notch = TRUE)
    }else{
      mlt <- melt(obj, id.vars = "y")
      gDta <- ggplot(mlt, aes(x=y,y=value, by=variable))+
        geom_jitter(aes(color=y,shape=y), alpha=.5)
    }
    gDta + labs(y="Predicted (%)", x="")
  }
  if(inherits(obj,"surv")){
    if(inherits(obj,"survSE")){
      # Summarized survival plot for the group...
      obj.t <-  melt(select(obj, time, median, mean), id.vars="time")
      
      gDta <- ggplot(obj.t)+
        geom_ribbon(aes(x=time, ymin=lower, ymax=upper), alpha=.25, data=obj)+
        geom_step(aes(x=time, y=value, color=variable))
      
    }else{
      # Lines by observation
      gDta <- ggplot(obj)+
        geom_step(aes(x=variable, y=value, col=cens, by=ptid), alpha=.3, size=.1)
    }
    
    gDta<-gDta  +
      labs(x="time (years)", y="OOB Survival (%)")
  }
  return(gDta)
}
