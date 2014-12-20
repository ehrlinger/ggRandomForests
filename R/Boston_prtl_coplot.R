#' Cached  list of \code{randomForestSRC::plot.variable} objects for the Boston Housing data. 
#' 
#' The \code{Boston_prtl_coplot} data is constructed from a series of
#'  \code{randomForestSRC::plot.variable} calls, with \code{partial=TRUE}, on the random 
#'  forest model (\code{randomForestSRC::rfsrc} in \code{\link{Boston_rfsrc}}) for the Boston 
#'  Housing data available in the \code{MASS} package. 
#' 
#' @details
#' Many \code{randomForestSRC} functions are computationally expensive. We use cached objects to
#' improve the \code{ggRandomForests} diagnostics and example run times (see
#'  \code{\link{rebuild_cache_datasets}} to rebuild your own copy.)
#' 
#' We build a regression random forest (\code{\link{Boston_rfsrc}}) 
#' with the \code{Boston} housing data set available in the \code{MASS} package, then run 
#' the \code{\link{gg_partial_coplot}} to run a series of \code{randomForestSRC::plot.variable} 
#' function calls for partial dependence of \code{lstat} for subsets of the training 
#' data (based on \code{rm}) to determine the conditional partial dependence of the response 
#' variable on the \code{lstat} variable dependent on the \code{rm} variable. 
#' 
#' Housing Values in Suburbs of Boston containing 506 rows and 14 columns.
#' 
#' @seealso \code{MASS::Boston} 
#' \code{randomForestSRC::plot.variable} 
#' \code{randomForestSRC::rfsrc}
#' \code{\link{rebuild_cache_datasets}} 
#' \code{\link{Boston_rfsrc}} 
#' \code{\link{gg_partial_coplot}}
#' 
#' @examples
#' \dontrun{
#' ## The data was built with the following randomForestSRC commands
#' data(Boston, package="MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' Boston_rfsrc <- rfsrc(medv~., data=Boston, ...)
#' 
#' # Create the subgroups
#' rm_pts <- cut_distribution(Boston_rfsrc$xvar$rm, groups=6)
#' rm_grp <- cut(Boston_rfsrc$xvar$rm, breaks=rm_pts)
#' # calculate the partial plot data
#' Boston_prtl_coplot <- gg_partial_coplot(Boston_rfsrc, xvar="lstat", 
#'                                         groups=rm_grp,
#'                                         show.plots=FALSE)
#' 
#' # Plot with ggplot2 commands
#' ggpl <- ggplot(Boston_prtl_coplot, aes(x=lstat, y=yhat, 
#'                                     shape=groups, 
#'                                     color=groups))+
#'                geom_point()+geom_smooth(se=FALSE)+
#'                labs(x=st.labs["lstat"], y="Median Value", 
#'                     color="rm", shape="rm")+
#'                scale_color_brewer(palette="Set1")
#' ggpl
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
#' @format List of \code{randomForestSRC::plot.variable} objects
#' @name Boston_prtl_coplot
NULL
