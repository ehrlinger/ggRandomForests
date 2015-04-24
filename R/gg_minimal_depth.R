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
#' Minimal depth data object (\code{[randomForestSRC]{var.select}})
#'
#' @param object A \code{[randomForestSRC]{rfsrc}} object, \code{[randomForestSRC]{predict}}
#'  object or the list from the \code{[randomForestSRC]{var.select.rfsrc}} function.
#' @param ... optional arguments passed to the \code{[randomForestSRC]{var.select}} function 
#'  if operating on an \code{[randomForestSRC]{rfsrc}} object. 
#' 
#' @description the \code{[randomForestSRC]{var.select}} function implements 
#' random forest variable selection using tree minimal depth methodology. The 
#' \code{gg_minimal_depth} 
#' function takes the output from \code{[randomForestSRC]{var.select}} and creates a 
#' \code{data.frame} formatted for the \code{\link{plot.gg_minimal_depth}} function.
#'  
#' @return \code{gg_minimal_depth} object, A modified list of variables from the 
#' \code{[randomForestSRC]{var.select}} function, ordered by minimal depth rank. 
#' 
#' @aliases gg_minimal_depth gg_minimal_depth.rfsrc
#' 
#' @seealso \code{[randomForestSRC]{var.select}} \code{\link{plot.gg_minimal_depth}}
#' 
#' @importFrom randomForestSRC var.select
#' 
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
#' gg_dta<- gg_minimal_depth(varsel_iris)
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
#' gg_dta<- gg_minimal_depth(varsel_airq)
#' 
#' # Plot the gg_minimal_depth object
#' plot(gg_dta)
#' }
#' 
#' ## -------- Boston data
#' data(varsel_Boston, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' plot(gg_minimal_depth(varsel_Boston))
#' 
#' \dontrun{
#' ## -------- mtcars data
#' data(varsel_mtcars, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' plot.gg_minimal_depth(varsel_mtcars)
#' }
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- veteran data
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' # varsel_veteran <- randomForestSRC::var.select(rfsrc_veteran)
#' # Load a cached randomForestSRC object
#' data(varsel_veteran, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_depth(varsel_veteran)
#' plot(gg_dta)
#' }
#' 
#' ## -------- pbc data
#' data(varsel_pbc, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_depth(varsel_pbc)
#' plot(gg_dta)
#' 
#' @export
gg_minimal_depth <- function (object, ...) {
  UseMethod("gg_minimal_depth", object)
}

#' @export
gg_minimal_depth.rfsrc <- function (object, ...){
  
  if (inherits(object, "rfsrc") == TRUE){
    vsel <- randomForestSRC::var.select(object, ...)
  }else if (!is.null(object$varselect)) {
    # Test for variable selection minimal depth object
    vsel <- object
  }else if(is.null(object$threshold)) {
    # Test for max.subtree minimal depth object, convert to vsel object
    stop("No support for max.subtree yet, use var.select instead")
  }else{
    stop("Function works only on rfsrc or var.select objects.")
  }
  
  
  # There seems to be a bug in the randomForestSRC::var.select 
  # function that does not calculage the threshold correctly.
  
  
  vsel$varselect$names <- rownames(vsel$varselect)
  
  vsel$varselect$names <- factor(vsel$varselect$names, 
                                 levels=unique(vsel$varselect$names))
  
  class(vsel) <- c("gg_minimal_depth", class(vsel))
  invisible(vsel) 
}

#' @export
gg_minimal_depth.default <- 
  gg_minimal_depth.rfsrc