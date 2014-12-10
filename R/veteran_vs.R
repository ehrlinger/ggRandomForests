#' Minimal depth variable selection object (randomForestSRC::var.select).
#' 
#' @description A cached object from \code{randomForestSRC::var.select} function 
#' for the Veteran's Administration Lung Cancer Trial survival forest \code{\link{veteran_rf}}.
#' 
#' 
#' @details For ggRandomForest testing and the R CMD checks, we cache the 
#' computationally expensive parts of running a randomForest. 
#' 
#' We build a regression randomForest (\code{\link{veteran_rf}}) with the 
#' \code{veteran} clinical trial data, then run the \code{randomForestSRC::var.select} 
#' function to determine minimal depth variable selection. 
#' 
#' This "data set" is a cache of the \code{randomForestSRC::var.select} function, which runs the
#' minimal depth variable selection method from the \code{\link{veteran_rf}} random 
#' forest model. 
#'
#' The Veteran's Administration Lung Cancer Trial  is a randomized trial of two treatment regimens 
#' for lung cancer. This is a standard survival analysis data set.
#' 
#' \itemize{
#' \item trt   treatment: 1=standard 2=test
#' \item celltype   cell-type: 1=squamous, 2=smallcell, 3=adeno, 4=large
#' \item time   survival time
#' \item status	 censoring status
#' \item karno	 Karnofsky performance score (100=good)
#' \item diagtime	 months from diagnosis to randomisation
#' \item age	 age in years
#' \item prior	 prior therapy 0=no, 1=yes
#' }
#' 
#' 
#' 
#' @seealso \code{veteran} \code{randomForestSRC::var.select} \code{randomForestSRC::rfsrc} 
#' \code{\link{veteran_rf}} \code{\link{gg_minimal_depth}} \code{\link{plot.gg_minimal_depth}}
#' 
#' @examples
#' \dontrun{
#' ## The data was built with the following commands
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' 
#' veteran_vs <- var.select(veteran_rf)
#' 
#' gg_dta <- gg_minimal_depth(veteran_vs)
#' plot(gg_dta)
#' }
#' 

#' 
#' @examples
#' \dontrun{
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' }
#' 
#' @seealso \code{var.select} \code{rfsrc} \code{veteran}
#' 
#' @references 
#' Kalbfleisch J. and Prentice R, (1980) The Statistical Analysis of Failure 
#' Time Data. New York: Wiley.
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
#' @format A var.select object for the veteran survival random forest
#' @name veteran_vs
NULL
