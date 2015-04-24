#' Cached \code{\link[randomForestSRC]{plot.variable}} objects for examples, diagnostics and vignettes.
#'  
#' Data sets storing \code{\link[randomForestSRC]{rfsrc}} objects corresponding to 
#' training data according to the following naming convention: 
#'\itemize{
#' \item \code{partial_coplot_Boston} - randomForestS[R]C for the \code{Boston} housing 
#' data set (\code{MASS} package).
#' }
#'   
#' @details 
#' Constructing random forests are computationally expensive.
#' We cache \code{\link[randomForestSRC]{rfsrc}} objects to improve the \code{ggRandomForests} 
#' examples, diagnostics and vignettes run times. 
#' (see \code{\link{rfsrc_cache_datasets}} to rebuild a complete set of these data sets.)
#' 
#' For each data set listed, we build a \code{\link[randomForestSRC]{rfsrc}}. Tuning parameters used
#' in each case are documented in the examples. Each data set is built with the 
#' \code{\link{rfsrc_cache_datasets}} with the \code{randomForestSRC} version listed
#' in the \code{ggRandomForests} DESCRIPTION file.
#' 
#' \itemize{
#' \item \code{partial_coplot_Boston} - The \code{Boston} housing values in suburbs of Boston from the
#' \code{MASS} package. Build a regression random forest for predicting medv (median home 
#' values) on 13 covariates and 506 observations. 
#' 
#' }
#' 
#' @seealso \code{\link[MASS]{Boston}} \code{\link[randomForestSRC]{plot.variable}}
#'  \code{\link{rfsrc_cache_datasets}}
#'  
#' @examples
#' \dontrun{
#' #---------------------------------------------------------------------
#' # MASS::Boston data - regression random forest 
#' #---------------------------------------------------------------------
#' data(Boston_rfsrc, package="ggRandomForests")
#' 
#' # Cut the codependent variable
#' rm_pts <- cut_distribution(rfsrc_Boston$xvar$rm, groups=6)
#' rm_grp <- cut(rfsrc_Boston$xvar$rm, breaks=rm_pts)
#' 
#' # plot.variable for lstat on subsets of rm (this will take some time.)
#'  partial_coplot_Boston <- gg_partial_coplot(rfsrc_Boston, xvar="lstat", 
#'                                             groups=rm_grp, 
#'                                             show.plots=FALSE)
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
#'  Belsley, D.A., E. Kuh, and R.E. Welsch. 1980. Regression Diagnostics. Identifying 
#'  Influential Data and Sources of Collinearity. New York: Wiley.
#'  
#' Harrison, D., and D.L. Rubinfeld. 1978. "Hedonic Prices and the Demand for Clean Air."
#'  J. Environ. Economics and Management 5: 81-102.
#' 
#' #---------------------
#'  pbc data set
#' ---------------------
#' 
#' Flemming T.R and Harrington D.P., (1991) Counting Processes and Survival Analysis. 
#' New York: Wiley.
#' 
#' T Therneau and P Grambsch (2000), Modeling Survival Data: Extending the Cox Model, 
#' Springer-Verlag, New York. ISBN: 0-387-98784-3.
#' 
#' 
#' @aliases partial_coplot_data partial_coplot_Boston partial_coplot_Boston2 partial_coplot_pbc partial_coplot_pbc2
#' @docType data
#' @keywords datasets
#' @format List of \code{\link[randomForestSRC]{plot.variable}} objects
#' @name partial_coplot_data
#' @name partial_coplot_Boston
#' @name partial_coplot_Boston2
#' @name partial_coplot_pbc
#' @name partial_coplot_pbc2
#' 
NULL