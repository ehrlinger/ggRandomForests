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
#' ggROC Generate a data from containing the sensitivity and specificity 
#' of a randomForests classification object. 
#' 
#' @param object an rfsrc classification object
#' @param which.outcome select the classification outcome of interest.
#' @param oob use oob estimates (default TRUE)
#' 
#' @return ggROC data.frame for plotting ROC curves.
#' 
#' @seealso \code{\link{plot.ggROC}} \code{rfsrc}
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' options(mc.cores=2, rf.cores=1)
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
#' 
#' @export ggROC.ggRandomForests ggROC
#' @aliases ggROC
#' @importFrom dplyr tbl_df
ggROC.ggRandomForests <- function(object, which.outcome, oob=TRUE){
  
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2 & 
        !inherits(object, "randomForest")) {
    stop("This function only works for objects of class `(rfsrc, grow)', '(rfsrc, predict)' or 'randomForest.")
  }
  if(!inherits(object, "class")){
    stop("ggROC only works with classification forests")
  }
  
  # Want to remove the which.outcomes argument to plot ROC for all outcomes simultaneously.
  if(missing(which.outcome)) which.outcome=1
  
  if(inherits(object, "randomForest")){
    if(object$type != "classification")
      stop("ggROC is intended for classification forests only.")
    
    roc<- calcROC.randomForest(object, object$y, which.outcome=which.outcome)
  }else{
    if(object$family != "class")
      stop("ggROC is intended for classification forests only.")
    
    roc<- calcROC.rfsrc(object, object$yvar, which.outcome=which.outcome, oob=oob)
  }
  roc <- tbl_df(roc)
  class(roc) <- c("ggROC", class(roc))
  
  invisible(roc)
}
ggROC <- ggROC.ggRandomForests
