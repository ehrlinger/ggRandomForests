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
#' Print a \code{\link{gg_minimal_depth}} object.
#' 
#' @param x a \code{\link{gg_minimal_depth}} object.
#' @param ... optional arguments
#' 
#' @examples
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
#' gg_dta <- gg_minimal_depth(varsel_iris)
#' print(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' \dontrun{
#' # ... or load a cached randomForestSRC object
#' data(varsel_airq, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_minimal_depth(varsel_airq)
#' print(gg_dta)
#' 
#' # To nicely print a rfsrc::var.select output... 
#' print(varsel_airq)
#' }
#' 
#' # ... or load a cached randomForestSRC object
#' data(varsel_Boston, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_minimal_depth(varsel_Boston)
#' print(gg_dta)
#' 
#' # To nicely print a rfsrc::var.select output... 
#' print(varsel_Boston)
#' 
#' @export
print.gg_minimal_depth <- function(x, ...){
  gg_dta <- x
  
  # If gg_dta is not a gg_minimal_depth object, check if it is the output
  # from rfsrc::var.select
  if(!inherits(x, "gg_minimal_depth"))
    gg_dta <- gg_minimal_depth(x)
  
  cat("-----------------------------------------------------------\n")
  cat("gg_minimal_depth\n")
  cat("model size         :", gg_dta$modelsize, "\n")
  cat("depth threshold    :", round(gg_dta$md.obj$threshold, 4),  "\n")
  cat("\n")
  cat("PE :")
  print(round(gg_dta$err.rate, 3))
  
  cat("-----------------------------------------------------------\n")
  cat("\n")
  cat("Top variables:\n")
  v_sel <- data.frame(gg_dta$varselect[1:gg_dta$modelsize, 
                                      -which(colnames(gg_dta$varselect) == "names"), 
                                      drop = FALSE])
  
  print(v_sel, digits=3)
  
  cat("-----------------------------------------------------------\n")
  
}