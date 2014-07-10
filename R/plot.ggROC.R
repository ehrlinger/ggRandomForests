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
#' plot.ggROC plot a \code{\link{ggROC}} object.
#' 
#' @param x \code{\link{ggROC}} object created from a classification forest
#' @param ... arguments passed to the \code{\link{ggROC}} function
#' 
#' @return ggplot object of the ROC curve
#' 
#' @export plot.ggROC
#' 
#' @seealso \code{\link{ggROC}} rfsrc
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, 
#' Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' 
#' # ROC for setosa
#' ggrf.obj<- ggROC(iris.obj, which.outcome=1)
#' plot.ggROC(ggrf.obj)
#' 
#' # ROC for versicolor
#' ggrf.obj<- ggROC(iris.obj, which.outcome=2)
#' plot.ggROC(ggrf.obj)
#' 
#' # ROC for virginica
#' ggrf.obj<- ggROC(iris.obj, which.outcome=3)
#' plot.ggROC(ggrf.obj)
#' }
#' @importFrom ggplot2 ggplot aes_string geom_line geom_abline labs coord_fixed annotate
#' 
### error rate plot
plot.ggROC<- function(x, ...){
  obj <- x
  if(inherits(obj, "rfsrc")) obj<- ggROC(obj, ...)
  obj$fpr <- 1-obj$spec
  obj <- rbind(c(0,0), obj, c(1,1))
  obj <- obj[order(obj$sens),]
  auc <- calcAUC(obj)
  
  gDta <- ggplot(data=obj)+
    geom_line(aes_string(x="fpr", y="sens"))+
    labs(x="1 - Specificity (FPR)", y="Sensitivity (TPR)")+
    geom_abline(a=1, b=0, col="red", linetype=2, size=.5) +
    coord_fixed()
  
  gDta <- gDta+
    annotate(x=.5,y=.2,geom="text", 
             label=paste("AUC = ",round(auc, digits=3), sep=""), hjust=0)
  
  return(gDta)
}
