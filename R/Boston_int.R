#' Cached  \code{randomForestSRC::find.interaction} matrix for the Boston Housing data. 
#' 
#' The \code{Boston_int} data set is constructed from the 
#' \code{randomForestSRC::find.interaction} 
#' call on the random forest model (\code{randomForestSRC::rfsrc} in \code{\link{Boston_rfsrc}})
#' for Boston Housing data in the \code{MASS} package. 
#'  
#' @details 
#' Many \code{randomForestSRC} functions are computationally expensive. We use cached objects to
#' improve the \code{ggRandomForests} diagnostic and example run times (see
#'  \code{\link{rebuild_cache_datasets}} to rebuild your own copy.)
#' 
#' We build a regression random forest (\code{\link{Boston_rfsrc}}) 
#' with the \code{Boston} housing data set, then run the \code{randomForestSRC::find.interaction}
#' function to determine pairwise variable interaction measures. 
#' 
#' Housing Values in Suburbs of Boston containing 506 rows and 14 columns.
#' 
#' @seealso \code{MASS::Boston} 
#' \code{randomForestSRC::find.interaction} 
#' \code{randomForestSRC::rfsrc}
#' \code{\link{rebuild_cache_datasets}} 
#' \code{\link{Boston_rfsrc}} 
#' \code{\link{gg_interaction}} 
#' \code{\link{plot.gg_interaction}}
#' 
#' @examples
#' \dontrun{
#' ## The data was built with the following randomForestSRC commands
#' data(Boston, package="MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' Boston_rfsrc <- rfsrc(medv~., data=Boston, ...)
#' Boston_int <- find.interaction(Boston_rfsrc)
#' 
#' # ggRandomForests commands:
#' gg_dta <- gg_interaction(Boston_int)
#' 
#' # Plot the interactions for lstat and rm individually.
#' plot(gg_dta, xvar="lstat")
#' plot(gg_dta, xvar="rm")
#' 
#' # Plot all of the interactions together.
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
#' @format matrix (\code{randomForestSRC::find.interaction})
#' @name Boston_int
NULL
