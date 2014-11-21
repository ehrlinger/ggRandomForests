#' Minimal depth variable selection object (randomForestSRC::var.select).
#' 
#' A cached object from \code{randomForestSRC::var.select} function for the 
#' randomForestSRC regression forest \code{\link{mtcars_rf}}.
#'  
#' @details For ggRandomForest testing and the R CMD checks, we cache the 
#' computationally expensive parts of running a randomForest. 
#' 
#' We build a regression randomForest (\code{\link{mtcars_rf}}) with the 
#' \code{mtcars} Motor Trend Car Road Tests data, then run the 
#' \code{randomForestSRC::var.select} 
#' function to determine minimal depth variable selection. 
#' 
#' This "data set" is a cache of the \code{var.select} function, which runs the
#' minimal depth variable selection method from the \code{\link{mtcars_rf}} random 
#' forest model. 
#' 
#' The data was extracted from the 1974 Motor Trend US magazine, and comprises 
#' fuel consumption and 10 aspects of automobile design and performance for 32 
#' automobiles (1973-74 models).
#' 
#' @seealso \code{mtcars} \code{var.select} \code{rfsrc} \code{\link{mtcars_rf}} 
#' \code{\link{gg_minimal_depth}} \code{\link{plot.gg_minimal_depth}}
#' 
#' @examples
#' \dontrun{
#' ## The data was built with the following command
#' mtcars_rf <- rfsrc(mpg ~ ., data = mtcars)
#' airq_vs <- var.select(mtcars_rf)
#' 
#' ggobj <- gg_minimal_depth(mtcars_vs)
#' plot(ggobj)
#' }
#' 
#' @references 
#' Henderson and Velleman (1981), Building multiple regression models 
#' interactively. Biometrics, 37, 391-411.
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
#' @name mtcars_vs
NULL
