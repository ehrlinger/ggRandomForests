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
#' ROC plot generic function for a \code{\link{gg_roc}} object.
#' 
#' @param x \code{\link{gg_roc}} object created from a classification forest
#' @param which.outcome for multiclass problems, choose the class for plotting
#' @param ... arguments passed to the \code{\link{gg_roc}} function
#' 
#' @return \code{ggplot} object of the ROC curve
#' 
#' @export plot.gg_roc
#' 
#' @seealso \code{\link{gg_roc}} rfsrc
#' 
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
#' #iris_rf <- rfsrc(Species ~ ., data = iris)
#' data(iris_rf, package="ggRandomForests")
#'
#' # ROC for setosa
#' ggrf <- gg_roc(iris_rf, which.outcome=1)
#' plot.gg_roc(ggrf)
#' 
#' # ROC for versicolor
#' ggrf <- gg_roc(iris_rf, which.outcome=2)
#' plot.gg_roc(ggrf)
#' 
#' # ROC for virginica
#' ggrf <- gg_roc(iris_rf, which.outcome=3)
#' plot.gg_roc(ggrf)
#' 
#' # Alternatively, you can plot all three outcomes in one go
#' # by calling the plot function on the full forest. 
#' plot.gg_roc(iris_rf)
#' }
#' @importFrom ggplot2 ggplot aes_string geom_line geom_abline labs coord_fixed annotate
#' 
### error rate plot
plot.gg_roc<- function(x, which.outcome=NULL, ...){
  obj <- x
  
  if(inherits(obj, "rfsrc"))
    if(inherits(obj, "class")){
      # How many classes are there?
      crv <- dim(obj$predicted)[2]
      
      if(crv > 2 & is.null(which.outcome) ){
        obj <- lapply(1:crv, function(ind){
          gg_roc(obj, which.outcome=ind, ...)
        })
        
      }else{
        obj<- gg_roc(obj, which.outcome, ...)
      }
    }else{
      stop("gg_roc expects a classification randomForest.")
    }
  if(inherits(obj, "gg_roc")){
    obj <- obj[order(obj$spec),]
    obj$fpr <- 1-obj$spec
    auc <- calc_auc(obj)
    
    gDta <- ggplot(data=obj)+
      geom_line(aes_string(x="fpr", y="sens"))+
      labs(x="1 - Specificity (FPR)", y="Sensitivity (TPR)")+
      geom_abline(a=1, b=0, col="red", linetype=2, size=.5) +
      coord_fixed()
    
    
    gDta <- gDta+
      annotate(x=.5,y=.2,geom="text", 
               label=paste("AUC = ",round(auc, digits=3), sep=""), hjust=0)
    
  }else{
    obj <- lapply(obj, function(st){st[order(st$spec),]
                                    st})
    obj <- lapply(obj, function(st){st$fpr <- 1-st$spec
                                    st})
    obj <- lapply(1:length(obj), function(ind){ obj[[ind]]$outcome <- ind
                                                obj[[ind]]})
    
    auc <- lapply(obj, function(st){calc_auc(st)})
    
    oDta <- do.call(rbind, obj)
    oDta$outcome <- factor(oDta$outcome)
    
    gDta <- ggplot(data=oDta)+
      geom_line(aes_string(x="fpr", y="sens", linetype="outcome", col="outcome"))+
      labs(x="1 - Specificity (FPR)", y="Sensitivity (TPR)")+
      geom_abline(a=1, b=0, col="red", linetype=2, size=.5) +
      coord_fixed()
    
    if(crv < 2){
      gDta <- gDta+
        annotate(x=.5,y=.2,geom="text", 
                 label=paste("AUC = ",round(auc, digits=3), sep=""), hjust=0)
    }
  }
  return(gDta)
}
