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
#' Minimal depth vs VIMP camparison by variable rankings. 
#' 
#' @param object A \code{randomForestSRC::rfsrc} object, \code{randomForestSRC::predict}
#'  object or the list from the \code{randomForestSRC::var.select.rfsrc} function.
#' @param event an optional vector of logical values (event indicator) for 
#' shaping the points in when plotting.
#' @param ... optional arguments passed to the \code{randomForestSRC::var.select} function 
#'  if operating on an \code{randomForestSRC::rfsrc} object. 
#'  
#'  @return \code{gg_minimal_vimp} comparison object.
#'  
#'  @seealso \code{\link{plot.gg_minimal_vimp}} \code{randomForestSRC::var.select}
#'  
#'  @export gg_minimal_vimp gg_minimal_vimp.rfsrc
#'  @aliases gg_minimal_vimp
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
#' gg_dta<- gg_minimal_vimp(varsel_iris)
#' 
#' # Plot the gg_minimal_depth object
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
#' gg_dta<- gg_minimal_vimp(varsel_airq)
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
#' gg_dta <- gg_minimal_vimp(varsel_veteran)
#' plot(gg_dta)
#'   
gg_minimal_vimp.rfsrc <- function(object, event, ...){
  
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
  
  rnk.md <- rnk.vm <- data.frame(cbind(names=rownames(vSel$varselect)))
  rnk.md$depth <- rnk.vm$vimp <- 1:dim(rnk.md)[1]
  
  # Rename the full vimp.all column to just "vimp"
  if(is.null(vSel$varselect$vimp))
    colnames(vSel$varselect)[which(colnames(vSel$varselect)=="vimp.all")] <- "vimp"
  
  rnk.vm <- rnk.vm[order(vSel$varselect$vimp, decreasing=TRUE),]
  rnk.vm$vimp <- 1:dim(rnk.vm)[1]
  
  # Default color is by negative/positive vimp
  rnk.vm$col <- c("-", "+")[as.numeric(vSel$varselect$vimp[order(vSel$varselect$vimp, 
                                                                 decreasing=TRUE)]>0)+1]
  
  gg_dta <- merge(rnk.vm, rnk.md,by="names")
  
  class(gg_dta) <- c("gg_minimal_vimp", class(gg_dta))
  invisible(gg_dta)
}
gg_minimal_vimp <- gg_minimal_vimp.rfsrc