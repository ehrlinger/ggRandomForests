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
  gg_dta <- x
  
  if(inherits(gg_dta, "rfsrc"))
    if(inherits(gg_dta, "class")){
      # How many classes are there?
      crv <- dim(gg_dta$predicted)[2]
      
      if(crv > 2 & is.null(which.outcome) ){
        gg_dta <- lapply(1:crv, function(ind){
          gg_roc(gg_dta, which.outcome=ind, ...)
        })
        
      }else{
        gg_dta<- gg_roc(gg_dta, which.outcome, ...)
      }
    }else{
      stop("gg_roc expects a classification randomForest.")
    }
  if(inherits(gg_dta, "gg_roc")){
    gg_dta <- gg_dta[order(gg_dta$spec),]
    gg_dta$fpr <- 1-gg_dta$spec
    auc <- calc_auc(gg_dta)
    
    gg_plt <- ggplot(data=gg_dta)+
      geom_line(aes_string(x="fpr", y="sens"))+
      labs(x="1 - Specificity (FPR)", y="Sensitivity (TPR)")+
      geom_abline(a=1, b=0, col="red", linetype=2, size=.5) +
      coord_fixed()
    
    
    gg_plt <- gg_plt+
      annotate(x=.5,y=.2,geom="text", 
               label=paste("AUC = ",round(auc, digits=3), sep=""), hjust=0)
    
  }else{
    gg_dta <- lapply(gg_dta, function(st){st[order(st$spec),]
                                    st})
    gg_dta <- lapply(gg_dta, function(st){st$fpr <- 1-st$spec
                                    st})
    gg_dta <- lapply(1:length(gg_dta), function(ind){ gg_dta[[ind]]$outcome <- ind
                                                gg_dta[[ind]]})
    
    auc <- lapply(gg_dta, function(st){calc_auc(st)})
    
    oDta <- do.call(rbind, gg_dta)
    oDta$outcome <- factor(oDta$outcome)
    
    gg_plt <- ggplot(data=oDta)+
      geom_line(aes_string(x="fpr", y="sens", linetype="outcome", col="outcome"))+
      labs(x="1 - Specificity (FPR)", y="Sensitivity (TPR)")+
      geom_abline(a=1, b=0, col="red", linetype=2, size=.5) +
      coord_fixed()
    
    if(crv < 2){
      gg_plt <- gg_plt+
        annotate(x=.5,y=.2,geom="text", 
                 label=paste("AUC = ",round(auc, digits=3), sep=""), hjust=0)
    }
  }
  return(gg_plt)
}
