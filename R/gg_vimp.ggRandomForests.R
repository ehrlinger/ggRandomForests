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
#' Variable Importance (VIMP) data object
#'
#' \code{gg_vimp} Extracts the variable importance (VIMP) information from a
#' a \code{randomForestSRC::rfsrc} object.
#' 
#' @param object A \code{randomForestSRC::rfsrc} object or output from \code{randomForestSRC::vimp}
#' @param ... arguments passed to the \code{randomForestSRC::vimp.rfsrc} function if the 
#' \code{randomForestSRC::rfsrc} object does not contain importance information.
#' 
#' @return a matrix of VIMP measures, in rank order.
#' 
#' @seealso \code{\link{plot.gg_vimp}} \code{randomForestSRC::rfsrc} \code{randomForestSRC::vimp}
#' 
#' @references 
#' Ishwaran H. (2007). Variable importance in binary regression trees and forests, 
#' \emph{Electronic J. Statist.}, 1:519-537.
#' 
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' data(iris_rf, package="ggRandomForests")
#' ggrf <- gg_vimp(iris_rf)
#' plot(ggrf)
#'  
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' 
#' # airq.obj <- rfsrc(Ozone ~ ., airquality)
#' data(airq_rf, package="ggRandomForests")
#' ggrf <- gg_vimp(airq_rf)
#' plot(ggrf)
#' 
#' ## ------------------------------------------------------------
#' ## survival example
#' ## ------------------------------------------------------------
#' data(veteran_rf, package="ggRandomForests")
#' ggrf <- gg_vimp(veteran_rf)
#' plot(ggrf)
#' 
#' @export gg_vimp.ggRandomForests
#' @export gg_vimp
#' @aliases gg_vimp
#' @importFrom randomForestSRC vimp

gg_vimp.ggRandomForests <- function(object, ...){
  
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
 
  ### set importance to NA if it is NULL
  if (is.null(object$importance)){
    warning("rfsrc object does not contain VIMP information. Calculating...")
    imp <- data.frame(sort(vimp(object, ...)$importance, decreasing=TRUE))
  }else{
    imp<-  data.frame(sort(object$importance, decreasing=TRUE))
  }
  
  imp<- cbind(imp, imp/imp[1,1])
  colnames(imp) <- c("VIMP", "relVIMP")
  imp$names <- rownames(imp)
  imp$names[which(is.na(imp$names))] <- rownames(imp)[which(is.na(imp$names))]
  
  imp$names <- factor(imp$names, levels=rev(imp$names))
  imp$positive <- TRUE
  imp$positive[which(imp$VIMP <=0)] <- FALSE
#   
#     if(missing(xvar.names)){
#       rfvimp <- as.data.frame(cbind(rfvimp[order(rfvimp, decreasing=TRUE)][1:n.var]))
#     }else{
#       rfvimp <- rfvimp[which(names(rfvimp) %in% var.names)]
#     }
  class(imp) <- c("gg_vimp", class(imp))
  invisible(imp)
}

gg_vimp <-gg_vimp.ggRandomForests
