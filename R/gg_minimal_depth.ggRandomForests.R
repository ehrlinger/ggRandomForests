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
#' \code{gg_minimal_depth} 
#' function takes the output from \code{randomForestSRC::var.select} and creates a 
#' \code{data.frame} formatted for the \code{\link{plot.gg_minimal_depth}} function.
#'  
#' @return \code{gg_minimal_depth} object, A modified list of variables from the 
#' \code{randomForestSRC::var.select} function, ordered by minimal depth rank. 
#' 
#' @export gg_minimal_depth.ggRandomForests gg_minimal_depth
#' @aliases gg_minimal_depth
#' 
#' @seealso \code{randomForestSRC::var.select} \code{\link{plot.gg_minimal_depth}}
#' 
#' @importFrom randomForestSRC var.select
#' 
#' @examples
#' ## Examples from RFSRC package... 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## You can build a randomForest
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' # varsel_iris <- var.select(rfsrc_iris)
#' # ... or load a cached randomForestSRC object
#' data(varsel_iris, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_minimal_depth(varsel_iris)
#' 
#' # Plot the gg_mkinimal_depth object
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' # varsel_airq <- var.select(rfsrc_airq)
#' # ... or load a cached randomForestSRC object
#' data(varsel_airq, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_depth(varsel_airq)
#' 
#' # Plot the gg_error object
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' # varsel_veteran <- var.select(rfsrc_veteran)
#' # Load a cached randomForestSRC object
#' data(varsel_veteran, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_depth(varsel_veteran)
#' plot(gg_dta)
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