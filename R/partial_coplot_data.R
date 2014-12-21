#' Cached \code{randomForestSRC::plot.variable} objects for examples, diagnostics and vignettes.
#'  
#' Data sets storing \code{randomForestSRC::rfsrc} objects corresponding to 
#' training data according to the following naming convention: 
#'\itemize{
#' \item \code{partial_coplot_Boston} - randomForestS[R]C for the \code{Boston} housing 
#' data set (\code{MASS} package).
#' }
#'   
#' @details 
#' Constructing random forests are computationally expensive.
#' We cache \code{randomForestSRC::rfsrc} objects to improve the \code{ggRandomForests} 
#' examples, diagnostics and vignettes run times. 
#' (see \code{\link{rebuild_cache_datasets}} to rebuild a complete set of these data sets.)
#' 
#' For each data set listed, we build a \code{randomForestSRC::rfsrc}. Tuning parameters used
#' in each case are documented in the examples. Each data set is built with the 
#' \code{\link{rebuild_cache_datasets}} with the \code{randomForestSRC} version listed
#' in the \code{ggRandomForests} DESCRIPTION file.
#' 
#' \itemize{
#' \item \code{partial_coplot_Boston} - The \code{Boston} housing values in suburbs of Boston from the
#' \code{MASS} package. Build a regression random forest for predicting medv (median home 
#' values) on 13 covariates and 506 observations. 
#' 
#' }
#' 
#' @seealso \code{MASS::Boston} \code{randomForestSRC::plot.variable}
#'  \code{\link{rebuild_cache_datasets}}
#'  
#' @examples
#' \dontrun{
#' #---------------------------------------------------------------------
#' # MASS::Boston data - regression random forest 
#' #---------------------------------------------------------------------
#' # Load the data...
#' data(Boston, package="MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' 
#' # rfsrc grow call
#' partial_coplot_Boston <- rfsrc(medv~., data=Boston)
#'
#' }
#' 
#' @references 
#' #---------------------
#'  randomForestSRC
#' ---------------------
#' 
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for
#' Survival, Regression and Classification (RF-SRC), R package
#' version 1.5.5.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests
#' for R. R News 7(2), 25-31.
#' 
#' Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S.
#' (2008). Random survival forests. Ann. Appl. Statist. 2(3),
#' 841-860.
#' 
#' #---------------------
#'  Boston data set
#' ---------------------
#' 
#' 
#' @aliases partial_coplot_data partial_coplots_Boston partial_coplots_Boston2 Boston_lstat_rm
#' @docType data
#' @keywords datasets
#' @format List of \code{randomForestSRC::plot.variable} objects
#' @name partial_coplot_Boston
#' @name partial_coplot_Boston2
#' @name Boston_lstat_rm
NULL