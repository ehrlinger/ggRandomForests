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
#' @importFrom tidyr gather_
#' @importFrom dplyr arrange desc %>%
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
  
  # To quite R CMD CHECK... for gather statements 
  cls  <- vars <- NA
  
  ### set importance to NA if it is NULL
  if (is.null(object$importance)){
    warning("rfsrc object does not contain VIMP information. Calculating...")
    imp <- data.frame(sort(vimp(object, ...)$importance, decreasing=TRUE))
  }else{
    imp <- object$importance
  }
  
  # Handle multiclass importance
  if(!is.null(dim(imp))){
    imp <- data.frame(imp)
    imp$vars <- rownames(imp)
    
    clnms <- colnames(imp)[-which(colnames(imp)=="vars")]
    imp <- imp %>% gather(cls, vimp, -vars) %>% arrange(desc(vimp))
    colnames(imp)[2] <- "set"
    imp$vars <- factor(imp$vars)
  }else{
    imp <- data.frame(sort(imp, decreasing=TRUE))
    
    imp<- cbind(imp, imp/imp[1,1])
    colnames(imp) <- c("vimp", "rel_vimp")
    imp$vars <- rownames(imp)
    imp$vars[which(is.na(imp$vars))] <- rownames(imp)[which(is.na(imp$vars))]
  }
  imp$vars <- factor(imp$vars, levels=rev(unique(imp$vars)))
  imp$positive <- TRUE
  imp$positive[which(imp$vimp <=0)] <- FALSE
  #   
  #     if(missing(xvar.vars)){
  #       rfvimp <- as.data.frame(cbind(rfvimp[order(rfvimp, decreasing=TRUE)][1:n.var]))
  #     }else{
  #       rfvimp <- rfvimp[which(vars(rfvimp) %in% var.vars)]
  #     }
  class(imp) <- c("gg_vimp", class(imp))
  invisible(imp)
}

gg_vimp <-gg_vimp.ggRandomForests
