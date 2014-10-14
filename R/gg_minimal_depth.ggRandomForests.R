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
#' Minimal depth data object (\code{randomForestSRC::var.select})
#'
#' @param object A \code{randomForestSRC::rfsrc} object, \code{randomForestSRC::predict}
#'  object or the list from the \code{randomForestSRC::var.select.rfsrc} function.
#' @param ... optional arguments passed to the \code{randomForestSRC::var.select} function 
#'  if operating on an \code{randomForestSRC::rfsrc} object. 
#' 
#' @description the \code{randomForestSRC::var.select} function implements 
#' random forest variable selection using tree minimal depth methodology. The 
#' gg_minimal_depth 
#' function takes the output from \code{randomForestSRC::var.select} and creates a 
#' \code{data.frame} formatted for the \code{\link{plot.gg_minimal_depth}} function.
#'  
#' @return Invisibly, the modified list of variables from the 
#' \code{randomForestSRC::var.select.rfsrc} function, ordered by minimal depth rank. 
#' 
#' @export gg_minimal_depth.ggRandomForests gg_minimal_depth
#' @aliases gg_minimal_depth
#' 
#' @seealso \code{randomForestSRC::var.select} \code{\link{plot.gg_minimal_depth}}
#' 
#' @importFrom randomForestSRC var.select
#' @importFrom dplyr tbl_df
#' 
#' @examples
#' ## Examples from RFSRC package... 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## You can build a randomForest
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' # iris_vs <- var.select(iris_rf)
#' # ... or load a cached randomForestSRC object
#' data(iris_vs, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' ggrf.obj<- gg_minimal_depth(iris_vs)
#' 
#' # Plot the gg_mkinimal_depth object
#' plot(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' # airq_rf <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' # airq_vs <- var.select(airq_rf)
#' # ... or load a cached randomForestSRC object
#' data(airq_vs, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' ggrf.obj<- gg_minimal_depth(airq_vs)
#' 
#' # Plot the gg_error object
#' plot(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' # veteran_vs <- var.select(veteran_rf)
#' # Load a cached randomForestSRC object
#' data(veteran_vs, package="ggRandomForests")
#' 
#' ggrf.obj <- gg_minimal_depth(veteran_vs)
#' plot(ggrf.obj)
#' 

gg_minimal_depth.ggRandomForests <- function (object, ...){
  
  if (inherits(object, "rfsrc") == TRUE){
    vSel <- var.select(object, ...)
  }else if (!is.null(object$varselect)) {
    # Test for variable selection minimal depth object
    vSel <- object
  }else if(is.null(object$threshold)) {
    # Test for max.subtree minimal depth object, convert to vSel object
    
    stop("No support for max.subtree yet, use var.select instead")
  }else{
    stop("Function works only on rfsrc or var.select objects.")
  }
  
  vSel$varselect$names <- rownames(vSel$varselect)
  
  vSel$varselect$names <- factor(vSel$varselect$names, 
                                 levels=unique(vSel$varselect$names))

  class(vSel) <- c("gg_minimal_depth", class(vSel))
  invisible(vSel) 
}

gg_minimal_depth<-gg_minimal_depth.ggRandomForests                         