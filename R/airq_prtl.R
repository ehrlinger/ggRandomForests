#' Cached \code{randomForestSRC::plot.variable} object for the air quality data. 
#' 
#' The \code{airq_prtl} data is constructed from the \code{randomForestSRC::plot.variable} 
#' call, with \code{partial=TRUE}, on the random forest model (\code{randomForestSRC::rfsrc} 
#' in \code{\link{airq_rf}}) for the New York Air Quality Measurements. 
#' 
#' @details
#' Many \code{randomForestSRC} functions are computationally expensive. We use cached objects to
#' improve the \code{ggRandomForests} diagnostics and example run times (see
#'  \code{\link{rebuild_cache_datasets}} to rebuild your own copy.)
#' 
#' We build a regression random forest (\code{\link{airq_rf}}) 
#' with the \code{airquality} data set, then run the \code{randomForestSRC::plot.variable}
#' function to determine partial dependence of the response variable on independant variables. 
#' 
#' The data were from New York, from May to September 1973. The data was obtained from the 
#' New York State Department of Conservation (ozone data) and the National Weather Service 
#' (meteorological data).
#' 
#' @seealso \code{airquality} \code{randomForestSRC::plot.variable} \code{randomForestSRC::rfsrc}
#'  \code{\link{airq_rf}} \code{\link{rebuild_cache_datasets}}
#'  \code{\link{gg_partial}} \code{\link{plot.gg_partial}}
#' 
#' @examples
#' \dontrun{
#' ## airq data
#' airq_rf <- rfsrc(Ozone ~ ., data = airquality, 
#'                  na.action = "na.impute", ...)
#' 
#' # Save the partial plot data
#' airq_prtl <- plot.variable(airq_rf,
#'                            partial=TRUE, show.plots=FALSE)
#'                     
#' # ggRandomForests functions.                                   
#' gg_dta <- gg_partial(airq_prtl)
#' 
#' # Individual plots (one per page).
#' plot(gg_dta)
#' 
#' # Or as a single panel plot.
#' plot(gg_dta, panel=TRUE)
#' }
#'                               
#' @references 
#' Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. 
#' (1983) Graphical Methods for Data Analysis. Belmont, CA: Wadsworth.
#' 
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for
#' Survival, Regression and Classification (RF-SRC), R package
#' version 1.5.4.
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
#' @format \code{randomForestSRC::plot.variable} object for regression
#' @name airq_prtl
NULL
