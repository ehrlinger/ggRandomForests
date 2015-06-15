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
#' @param object A \code{\link[randomForestSRC]{rfsrc}} object, 
#' \code{\link[randomForestSRC]{predict.rfsrc}}
#'  object or the list from the \code{\link[randomForestSRC]{var.select.rfsrc}} function.
#' @param ... optional arguments passed to the \code{\link[randomForestSRC]{var.select}} function 
#'  if operating on an \code{\link[randomForestSRC]{rfsrc}} object. 
#' 
#'  @return \code{gg_minimal_vimp} comparison object.
#'  
#'  @seealso \code{\link{plot.gg_minimal_vimp}} \code{\link[randomForestSRC]{var.select}}
#'  
#'  @aliases gg_minimal_vimp
#'  
#' @importFrom randomForestSRC var.select
#' @examples
#' ## Examples from RFSRC package... 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## You can build a randomForest
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' # varsel_iris <- randomForestSRC::var.select(rfsrc_iris)
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
#' \dontrun{
#' ## -------- air quality data
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' # varsel_airq <- randomForestSRC::var.select(rfsrc_airq)
#' # ... or load a cached randomForestSRC object
#' data(varsel_airq, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_vimp(varsel_airq)
#' 
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' }
#' 
#' ## -------- Boston data
#' data(varsel_Boston, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_vimp(varsel_Boston)
#' 
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' 
#' \dontrun{
#' ## -------- mtcars data
#' data(varsel_mtcars, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_vimp(varsel_mtcars)
#' 
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' }
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' # varsel_veteran <- randomForestSRC::var.select(rfsrc_veteran)
#' # Load a cached randomForestSRC object
#' data(varsel_veteran, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_vimp(varsel_veteran)
#' plot(gg_dta)
#' }
#' ## -------- pbc data
#' data(varsel_pbc, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_vimp(varsel_pbc)
#' plot(gg_dta)
#' @export
gg_minimal_vimp <- function (object, ...) {
  UseMethod("gg_minimal_vimp", object)
}
#' @export
gg_minimal_vimp.rfsrc <- function(object, ...){
  
  if (inherits(object, "rfsrc") == TRUE){
    vsel <- randomForestSRC::var.select(object, ...)
  }else if (!is.null(object$varselect)) {
    # Test for variable selection minimal depth object
    vsel <- object
  }else{
    stop("Function works only on rfsrc or var.select objects.")
  }
  
  rnk.md <- rnk.vm <- data.frame(cbind(names=rownames(vsel$varselect)))
  rnk.md$depth <- rnk.vm$vimp <- 1:dim(rnk.md)[1]
  
  # Rename the full vimp.all column to just "vimp"
  if(is.null(vsel$varselect$vimp))
    colnames(vsel$varselect)[which(colnames(vsel$varselect) == "vimp.all")] <-
      "vimp"
  
  rnk.vm <- rnk.vm[order(vsel$varselect$vimp, decreasing=TRUE),]
  rnk.vm$vimp <- 1:dim(rnk.vm)[1]
  
  # Default color is by negative/positive vimp
  rnk.vm$col <- c("-", "+")[as.numeric(vsel$varselect$vimp[
      order(vsel$varselect$vimp, decreasing = TRUE)] > 0)
    + 1]
  
  gg_dta <- merge(rnk.vm, rnk.md,by="names")
  
  class(gg_dta) <- c("gg_minimal_vimp", class(gg_dta))
  
  # So we can put a horizontal line at the MD selection point.
  attr(gg_dta, "modelsize") <- vsel$modelsize
  
  invisible(gg_dta)
}

#' @export
gg_minimal_vimp.default <- gg_minimal_vimp.rfsrc