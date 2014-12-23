####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####    Assistant Staff
####    Dept of Quantitative Health Sciences
####    Learner Research Institute
####    Cleveland Clinic Foundation
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#'
#' Partial variable dependence plot, operates on a \code{\link{gg_partial}} object.
#' 
#' @description Generate a risk adjusted (partial) variable dependence plot. 
#' The function plots the \code{randomForestSRC::rfsrc} response variable (y-axis) against
#' the covariate of interest (specified when creating the
#'  \code{\link{gg_partial}} object).
#' 
#' @param x \code{\link{gg_partial}} object created from a \code{randomForestSRC::rfsrc} forest object
#' @param points plot points (boolean)
#' @param smooth use smooth curve (by type)
#' @param ... extra arguments
#' 
#' @return \code{ggplot} object
#' 
#' @export plot.gg_partial
#' 
#' @seealso \code{randomForestSRC::plot.variable} \code{\link{gg_partial}} 
#' \code{\link{plot.gg_partial_list}} \code{\link{gg_variable}} 
#' \code{\link{plot.gg_variable}} 
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, 
#' Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.4.
#' 
#' @importFrom ggplot2 ggplot aes labs geom_point geom_smooth 
#'
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' 
#' ## iris "Petal.Width" partial dependence plot
#' ##
#' # iris_rf <- rfsrc(Species ~., data = iris)
#' # iris_prtl <- plot.variable(iris_rf, xvar.names = "Petal.Width",
#' #                            partial=TRUE)
#' data(iris_prtl, package="ggRandomForests")
#' 
#' gg_dta <- gg_partial(iris_prtl)
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' 
#' ## airquality "Wind" partial dependence plot
#' ##
#' # airq_rf <- rfsrc(Ozone ~ ., data = airquality)
#' # airq_prtl <- plot.variable(airq_rf, xvar.names = "Wind",
#' #                            partial=TRUE, show.plot=FALSE)
#' data(airq_prtl, package="ggRandomForests")
#'
#' gg_dta <- gg_partial(airq_prtl)
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' ## survival "age" partial variable dependence plot
#' ##
#' # data(veteran, package = "randomForestSRC")
#' # veteran_rf <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' #
#' ## 30 day partial plot for age
#' # veteran_prtl <- plot.variable(veteran_rf, surv.type = "surv", 
#' #                               partial = TRUE, time=30, 
#' #                               xvar.names = "age", 
#' #                               show.plots=FALSE)
#' data(veteran_prtl, package="ggRandomForests")
#' 
#' gg_dta <- gg_partial(veteran_prtl)
#' plot(gg_dta)
#' }
### error rate plot
plot.gg_partial <- function(x, points=TRUE, smooth="loess", ...){
  
  
  gg_dta <- x 
  if(inherits(x, "plot.variable")){
    gg_dta <- gg_partial(x, ...)
  }else if(!inherits(x, "gg_partial")){
    stop("gg_partial expects an object from the rfsrc::plot.variable function")
  }
  
  # Get the colname of the independent variable
  hName <- colnames(gg_dta)[2]
  
  colnames(gg_dta)[2] <- "x"
  
  if(is.null(gg_dta$group)){
    gg_plt<- ggplot(gg_dta,aes_string(x="x", y="yhat"))
  }else{
    gg_plt<- ggplot(gg_dta,aes_string(x="x", y="yhat", shape="group", color="group"))
  }
  
  gg_plt <- gg_plt+
    labs(x=hName, y="predicted")
  if(!is.factor(gg_dta$x)){
    if(points)  
      gg_plt<- gg_plt+geom_point( ...)
    if(!is.null(smooth)){
      gg_plt<- gg_plt+geom_smooth(method=smooth, ...)
    }
  }else{
    gg_plt<- gg_plt+geom_boxplot(...)
  }
  
  return(gg_plt)
  
}
