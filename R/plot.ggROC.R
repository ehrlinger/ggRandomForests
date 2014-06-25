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
#' plot.ggROC
#' Plot a \link{\code{ggError}} object, the cumulative OOB error rates of the forest as a function of number of trees.
#' 
#' @param obj ggROC object created from a randomForestSRC object
#' 
#' @return ggplot object
#' 
#' @export plot.ggROC
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
#' ggrf.obj<- ggError(iris.obj)
#' 
#' plot.ggError(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- ggError(v.obj)
#' plot(ggrf.obj)
#'
### error rate plot
plot.ggROC<- function(obj){
 
  if(class(obj)[1] == "rfsrc") obj<- ggROC(obj)
  obj$fpr <- 1-obj$spec
  obj <- rbind(c(0,0), obj, c(1,1))
  obj <- obj[order(obj$sens),]
  auc <- calcAUC(obj)
  
  gDta<-ggplot(data=obj)+
    #geom_point(aes(x=sens, y=spec))+
    geom_line(aes(x=fpr, y=sens))+
    #geom_smooth(aes(x=sens, y=spec))+
    labs(x="1 - Specificity (FPR)", y="Sensitivity (TPR)")+
    geom_abline(a=1, b=0, col="red", linetype=2, size=.5) +
    coord_fixed()
  
  gDta <-gDta+
    annotate(x=.5,y=.2,geom="text", 
             label=paste("AUC = ",round(auc, digits=3), sep=""), hjust=0)

  return(gDta)
}
