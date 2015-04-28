#' Cached \code{\link[randomForestSRC]{plot.variable}} objects for examples, 
#' diagnostics and vignettes.
#'  
#' Data sets storing \code{\link[randomForestSRC]{plot.variable}} objects corresponding to 
#' training data according to the following naming convention: 
#'\itemize{
#' \item \code{partial_Boston_surf} - from a randomForestS[R]C for the \code{Boston} housing 
#' data set (\code{MASS} package).
#' \item \code{partial_pbc_surf} - from a randomForest[S]RC for the \code{pbc} data set
#'  (\code{randomForestSRC} package)  
#' \item \code{partial_pbc_time} - from a randomForest[S]RC for the \code{pbc} data set
#'  (\code{randomForestSRC} package)
#' }
#'   
#' @details 
#' Constructing partial plot data with the randomForestsSRC::plot.variable function are 
#' computationally expensive. We cache \code{\link[randomForestSRC]{plot.variable}} objects
#' to improve the \code{ggRandomForests} examples, diagnostics and vignettes run times. 
#' (see \code{\link{rfsrc_cache_datasets}} to rebuild a complete set of these data sets.)
#' 
#' For each data set listed, we build a \code{\link[randomForestSRC]{rfsrc}} 
#' (see \code{\link{rfsrc_data}}), then calculate the partial plot data with 
#' \code{\link[randomForestSRC]{plot.variable}} function, setting \code{partial=TRUE}. Each data set is 
#' built with the \code{\link{rfsrc_cache_datasets}} with the \code{randomForestSRC} version 
#' listed in the \code{ggRandomForests} DESCRIPTION file.
#' 
#' \itemize{
#' \item \code{partial_Boston} - The \code{Boston} housing values in suburbs of Boston from the
#' \code{MASS} package. Build a regression random forest for predicting medv (median home 
#' values) on 13 covariates and 506 observations. 
#' 
#' \item \code{partial_pbc} - The \code{pbc} data from the Mayo Clinic trial in primary biliary 
#' cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, 
#' referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the 
#' randomized placebo controlled trial of the drug D-penicillamine. 312 cases participated in 
#' the randomized trial and contain largely complete data. Data from the \code{randomForestSRC}
#' package. Build a survival random forest for time-to-event death data with 17 covariates and 
#' 312 observations (remaining 106 observations are held out).
#' }
#' 
#' @seealso \code{\link[MASS]{Boston}}
#' \code{\link[randomForestSRC]{pbc}}
#' \code{\link[randomForestSRC]{plot.variable}}
#' \code{\link{rfsrc_data}}
#'  \code{\link{rfsrc_cache_datasets}} 
#'  \code{\link{gg_partial}} 
#'  \code{\link{plot.gg_partial}}
#'  
#' @examples
#' \dontrun{
#' #---------------------------------------------------------------------
#' # MASS::Boston data - regression random forest 
#' #---------------------------------------------------------------------
#' # load the rfsrc object from the cached data
#' data(rfsrc_Boston, package="ggRandomForests")
#' 
#' # The plot.variable call
#' partial_Boston <- plot.variable(rfsrc_Boston,
#'                                 partial=TRUE, show.plots = FALSE )
#' 
#' # plot the forest partial plots
#' gg_dta <- gg_partial(partial_Boston)
#' plot(gg_dta, panel=TRUE)
#' 
#' #---------------------------------------------------------------------
#' # randomForestSRC::pbc data - survival random forest
#' #---------------------------------------------------------------------
#' # load the rfsrc object from the cached data
#' data(rfsrc_pbc, package="ggRandomForests")
#'
#' # Restrict the time of interest to less than 5 years.
#' time_pts <- rfsrc_pbc$time.interest[which(rfsrc_pbc$time.interest<=5)]
#' 
#' # Find the 50 points in time, evenly space along the distribution of 
#' # event times for a series of partial dependence curves
#' time_cts <-quantile_pts(time_pts, groups = 50)
#' 
#' # Generate the gg_partial_coplot data object
#' system.time(partial_pbc_time <- lapply(time_cts, function(ct){
#'    plot.variable(rfsrc_pbc, xvar = "bili", time = ct,
#'                  npts = 50, show.plots = FALSE, 
#'                  partial = TRUE, surv.type="surv")
#'    }))
#' #     user   system  elapsed 
#' # 2561.313   81.446 2641.707 
#' 
#' # Find the quantile points to create 50 cut points
#' alb_partial_pts <-quantile_pts(rfsrc_pbc$xvar$albumin, groups = 50)
#' 
#' system.time(partial_pbc_surf <- lapply(alb_partial_pts, function(ct){
#'   rfsrc_pbc$xvar$albumin <- ct
#'   plot.variable(rfsrc_pbc, xvar = "bili", time = 1,
#'                 npts = 50, show.plots = FALSE, 
#'                 partial = TRUE, surv.type="surv")
#'   }))
#' # user   system  elapsed 
#' # 2547.482   91.978 2671.870 
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
#' @aliases partial_surface_data partial_Boston_surf partial_pbc_surf partial_pbc_time
#' @docType data
#' @keywords datasets
#' @format list of \code{\link[randomForestSRC]{plot.variable}} objects
#' @name partial_surface_data
#' @name partial_Boston_surf
#' @name partial_pbc_surf
#' @name partial_pbc_time
NULL
