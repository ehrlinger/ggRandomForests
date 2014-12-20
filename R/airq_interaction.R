#' Cached  \code{randomForestSRC::find.interaction} matrix for the air quality data. 
#' 
#' The \code{airq_interaction} data set is constructed from the 
#' \code{randomForestSRC::find.interaction} 
#' call on the random forest model (\code{randomForestSRC::rfsrc} in \code{\link{airq_rf}})
#' for the New York Air Quality Measurements. 
#'  
#' @details 
#' Many \code{randomForestSRC} functions are computationally expensive. We use cached objects to
#' improve the \code{ggRandomForests} diagnostic and example run times (see
#'  \code{\link{rebuild_cache_datasets}} to rebuild your own copy.)
#' 
#' We build a regression random forest (\code{\link{airq_rf}}) 
#' with the \code{airquality} data set, then run the \code{randomForestSRC::find.interaction}
#' function to determine pairwise variable interaction measures. 
#' 
#' The data were from New York, from May to September 1973. The data was obtained from the 
#' New York State Department of Conservation (ozone data) and the National Weather Service 
#' (meteorological data).
#' 
#' @seealso \code{airquality} 
#' \code{randomForestSRC::find.interaction} 
#' \code{randomForestSRC::rfsrc}
#' \code{\link{rebuild_cache_datasets}} 
#' \code{\link{airq_rf}} 
#' \code{\link{gg_interaction}} 
#' \code{\link{plot.gg_interaction}}
#' 
#' @examples
#' \dontrun{
#' ## The data was built with the following randomForestSRC commands
#' airq_rf <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute", ...)
#' airq_interaction <- find.interaction(airq_rf)
#' 
#' # ggRandomForests commands:
#' gg_dta <- gg_interaction(airq_interaction)
#' 
#' # Plot the interactions for Temp and Solar.R individually.
#' plot(gg_dta, xvar="Temp")
#' plot(gg_dta, xvar="Solar.R")
#' 
#' # Plot all of the interactions together.
#' plot(gg_dta, panel=TRUE)
#' }
#' 
#' @references 
#' Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. 
#' (1983) Graphical Methods for Data Analysis. Belmont, CA: Wadsworth.
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
#' @name airq_interaction
NULL
