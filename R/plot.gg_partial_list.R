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
#' Partial variable dependence plot, operates on a \code{gg_partial_list} object.
#' 
#' @description Generate a risk adjusted (partial) variable dependence plot. 
#' The function plots the \code{randomForestSRC::rfsrc} response variable (y-axis) against
#' the covariate of interest (specified when creating the
#'  \code{gg_partial_list} object).
#' 
#' @param x \code{gg_partial_list} object created from a \code{\link{gg_partial}} 
#' forest object
#' @param points plot points (boolean)
#' @param panel should the entire list be plotted together?
#' @param ... extra arguments
#' 
#' @return list of \code{ggplot} objects, or a single faceted \code{ggplot} object
#' 
#' @export plot.gg_partial_list
#' 
#' @seealso \code{randomForestSRC::plot.variable} \code{\link{gg_partial}} 
#' \code{\link{plot.gg_partial}} \code{\link{gg_variable}} 
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
#' ggrf_obj <- gg_partial(iris_prtl)
#' plot(ggrf_obj)
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
#' ggrf_obj <- gg_partial(airq_prtl)
#' plot(ggrf_obj)
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
#' ggrf_obj <- gg_partial(veteran_prtl)
#' plot(ggrf_obj)
#' }
#'
#' @importFrom ggplot2 ggplot aes labs geom_point geom_smooth facet_wrap
#' @importFrom parallel mclapply
#'
### error rate plot
plot.gg_partial_list <- function(x, points=TRUE, panel=FALSE, ...){
  gg_dta <- x 
  
  if(!inherits(gg_dta, "list")) stop("Functions expects a list object")
  
  lng <- length(gg_dta)
  
  # One figure, with facets?
  if(panel){
    
    # Go through each element of the list, and add the variable name column,
    # and rename the value column to "value"
    nms <- names(gg_dta)
    
    cls <- sapply(nms, function(nm){class(gg_dta[[nm]][,nm])})
    
    gg_dta <- mclapply(nms, function(nm){
      obj <- gg_dta[[nm]]
      colnames(obj)[which(colnames(obj)==nm)]  <- "value"
      obj$variable <- nm
      obj
    })
    
    gg_dta <- do.call(rbind, gg_dta)
    gg_dta$variable <- factor(gg_dta$variable,
                              levels=unique(gg_dta$variable))
    
    if(is.null(gg_dta$group)){
      gg_plt <- ggplot(gg_dta,
                       aes_string(x="value", y="yhat"))
      
    }else{
      gg_dta$group  <- factor(gg_dta$group,levels=unique(gg_dta$group))
      gg_plt <- ggplot(gg_dta,
                       aes_string(x="value", y="yhat", color="group", shape="group"))
    }
    
    if(sum(cls=="factor")==length(cls)){
      gg_plt <- gg_plt +
      geom_boxplot(...)
    }else{
      gg_plt <- gg_plt +
      geom_point(...)+
      geom_smooth(...)
    }
    return(gg_plt +
           facet_wrap(~variable,
                      scales="free_x")
    )
  }else{
    # OR a list of figures.
    gg_plt <- vector("list", length=lng)
    
    for(ind in 1:lng){
      gg_plt[[ind]] <- plot.gg_partial(gg_dta[[ind]], points, ...)
    }
    
    return(gg_plt)
  }
}
