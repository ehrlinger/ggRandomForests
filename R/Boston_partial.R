#' Cached  \code{randomForestSRC::plot.variable} object for the Boston Housing data. 
#' 
#' The \code{Boston_partial} data is constructed from the \code{randomForestSRC::plot.variable} 
#' call, with \code{partial=TRUE}, on the random forest model (\code{randomForestSRC::rfsrc} 
#' in \code{\link{Boston_rfsrc}}) for the Boston Housing data available in the \code{MASS} package. 
#' 
#' @details
#' Many \code{randomForestSRC} functions are computationally expensive. We use cached objects to
#' improve the \code{ggRandomForests} diagnostics and example run times (see
#'  \code{\link{rebuild_cache_datasets}} to rebuild your own copy.)
#' 
#' We build a regression random forest (\code{\link{Boston_rfsrc}}) 
#' with the \code{Boston} housing data set available in the \code{MASS} package, then run 
#' the \code{randomForestSRC::plot.variable}
#' function to determine partial dependence of the response variable on independant variables. 
#' 
#' Housing Values in Suburbs of Boston containing 506 rows and 14 columns.
#' 
#' @seealso \code{MASS::Boston} 
#' \code{randomForestSRC::plot.variable} 
#' \code{randomForestSRC::rfsrc}
#' \code{\link{rebuild_cache_datasets}} 
#' \code{\link{Boston_rfsrc}} 
#' \code{\link{gg_partial}} 
#' \code{\link{plot.gg_partial}}
#' 
#' @examples
#' \dontrun{
#' ## The data was built with the following randomForestSRC commands
#' data(Boston, package="MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' Boston_rfsrc <- rfsrc(medv~., data=Boston, ...)
#' # Save the partial plot data
#' Boston_partial <- plot.variable(Boston_rfsrc,
#'                                 partial=TRUE, 
#'                                 show.plots=FALSE)
#'                     
#' # ggRandomForests functions.                                   
#' gg_dta <- gg_partial(Boston_partial)
#' 
#' # Individual plots (one per page).
#' plot(gg_dta)
#' 
#' # Or as a single panel plot.
#' plot(gg_dta, panel=TRUE) 
#' }
#' 
#' @references 
#' Harrison, D. and Rubinfeld, D.L. (1978) Hedonic prices and the 
#' demand for clean air. J. Environ. Economics and Management 5, 81-102.
#' 
#' Belsley D.A., Kuh, E. and Welsch, R.E. (1980) Regression Diagnostics. 
#' Identifying Influential Data and Sources of Collinearity. New York: Wiley.
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
#' @docType data
#' @keywords datasets
#' @format \code{randomForestSRC::plot.variable} object
#' @name Boston_partial
NULL
