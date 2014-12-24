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
#' @return \code{gg_vimp} object. A \code{data.frame} of VIMP measures, in rank order.
#' 
#' @seealso \code{\link{plot.gg_vimp}} \code{randomForestSRC::rfsrc} \code{randomForestSRC::vimp}
#' 
#' @references 
#' Ishwaran H. (2007). Variable importance in binary regression trees and forests, 
#' \emph{Electronic J. Statist.}, 1:519-537.
#' 
#' @importFrom reshape2 melt
#' @importFrom randomForestSRC vimp
#' 
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_iris)
#' plot(gg_dta)
#'  
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' 
#' # rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_airq)
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## survival example
#' ## ------------------------------------------------------------
#' data(rfsrc_veteran, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_veteran)
#' plot(gg_dta)
#' 
#' @export gg_vimp.ggRandomForests
#' @export gg_vimp
#' @aliases gg_vimp

gg_vimp.ggRandomForests <- function(object, ...){
  
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
  
  ### set importance to NA if it is NULL
  if (is.null(object$importance)){
    warning("rfsrc object does not contain VIMP information. Calculating...")
    gg_dta <- data.frame(sort(vimp(object, ...)$importance, decreasing=TRUE))
  }else{
    gg_dta <- object$importance
  }
  
  # Handle multiclass importance
  if(!is.null(dim(gg_dta))){
    gg_dta <- data.frame(gg_dta)
    gg_dta$vars <- rownames(gg_dta)
    
    clnms <- colnames(gg_dta)[-which(colnames(gg_dta)=="vars")]
    gg_dta <- melt(gg_dta, id.vars="vars", 
                   variable.name="set", value.name="vimp")
    gg_dta <- gg_dta[order(gg_dta$vimp, decreasing=TRUE),]
    gg_dta$vars <- factor(gg_dta$vars)
  }else{
    gg_dta <- data.frame(sort(gg_dta, decreasing=TRUE))
    
    gg_dta<- cbind(gg_dta, gg_dta/gg_dta[1,1])
    colnames(gg_dta) <- c("vimp", "rel_vimp")
    gg_dta$vars <- rownames(gg_dta)
    gg_dta$vars[which(is.na(gg_dta$vars))] <- rownames(gg_dta)[which(is.na(gg_dta$vars))]
  }
  gg_dta$vars <- factor(gg_dta$vars, levels=rev(unique(gg_dta$vars)))
  gg_dta$positive <- TRUE
  gg_dta$positive[which(gg_dta$vimp <=0)] <- FALSE
  
  class(gg_dta) <- c("gg_vimp", class(gg_dta))
  invisible(gg_dta)
}

gg_vimp <-gg_vimp.ggRandomForests
