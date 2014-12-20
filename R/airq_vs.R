#' Cached  \code{randomForestSRC::var.select} object for the air quality data. 
#' 
#' The \code{airq_vs} data set is constructed from the 
#' \code{randomForestSRC::var.select} 
#' call on the random forest model (\code{randomForestSRC::rfsrc} in \code{\link{airq_rf}})
#' for the New York Air Quality Measurements. 
#'  
#' @details 
#' Many \code{randomForestSRC} functions are computationally expensive. We use cached objects to
#' improve the \code{ggRandomForests} diagnostic and example run times (see
#'  \code{\link{rebuild_cache_datasets}} to rebuild your own copy.)
#' 
#' We build a regression random forest (\code{\link{airq_rf}}) 
#' with the \code{airquality} data set, then run the \code{randomForestSRC::var.select}
#' function for minimal depth variabl selection data. 
#' 
#' The data were from New York, from May to September 1973. The data was obtained from the 
#' New York State Department of Conservation (ozone data) and the National Weather Service 
#' (meteorological data).
#' 
#' @seealso \code{airquality} \code{randomForestSRC::var.select} \code{randomForestSRC::rfsrc}
#'  \code{\link{airq_rf}} \code{\link{rebuild_cache_datasets}} \code{\link{gg_minimal_depth}} 
#'  \code{\link{plot.gg_minimal_depth}}  \code{\link{gg_minimal_vimp}} 
#'  \code{\link{plot.gg_minimal_vimp}}
#'  
#' @examples
#' \dontrun{
#' ## The data was built with the following command
#' airq_rf <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute", ...)
#' airq_vs <- var.select(airq_rf)
#' 
#' # ggRandomForests commands:
#' gg_dta <- gg_minimal_depth(airq_vs)
#' plot(gg_dta)
#' 
#' gg_dta <- gg_minimal_vimp(airq_vs)
#' plot(gg_dta)
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
#' @format A var.select object for a regression random forest
#' @name airq_vs
NULL
