#' Minimal depth variable selection object (randomForestSRC::var.select).
#' 
#' A cached object from \code{randomForestSRC::var.select} function for the New York 
#' Air Quality Measurements randomForestSRC regression forest \code{\link{airq_rf}}.
#'  
#' @details For ggRandomForest testing and the R CMD checks, we cache the 
#' computationally expensive parts of running a randomForest. 
#' 
#' We build a regression randomForest (\code{\link{airq_rf}}) with the 
#' \code{airquality} measurements data, then run the \code{randomForestSRC::var.select} 
#' function to determine minimal depth variable selection. 
#' 
#' This "data set" is a cache of the \code{var.select} function, which runs the
#' minimal depth variable selection method from the \code{\link{airq_rf}} random 
#' forest model. 
#' 
#' The data were from New York, from May to September 1973. The data was obtained from the New York State 
#' Department of Conservation (ozone data) and the National Weather Service 
#' (meteorological data).
#' 
#' 
#' @seealso \code{airquality} \code{var.select} \code{rfsrc} \code{\link{airq_rf}} 
#' \code{\link{gg_minimal_depth}} \code{\link{plot.gg_minimal_depth}}
#' 
#' @examples
#' \dontrun{
#' ## The data was built with the following command
#' airq_rf <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' airq_vs <- var.select(airq_rf)
#' 
#' gg_dta <- gg_minimal_depth(airq_vs)
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
