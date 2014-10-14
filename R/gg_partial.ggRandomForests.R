####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
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
#' Partial variable dependence object 
#' 
#' @description The \code{randomForestSRC::plot.variable} function returns a 
#' list of either marginal variable dependance or partial variable dependence
#' data from a \code{randomForestSRC::rfsrc} object. 
#' The \code{gg_partial} function formulates the \code{randomForestSRC::plot.variable} output
#' for partial plots  (where partial=TRUE) into a data object for creation of 
#' partial dependence plots using the \code{\link{plot.gg_partial}} function. 
#' 
#' Partial variable dependence plots are the risk adjusted estimates of the specified response as a 
#' function of a single covariate, possibly subsetted on other covariates.
#' 
#' @param object the partial variable dependence data object from 
#'   \code{randomForestSRC::plot.variable} function
#' @param named optional column for merging multiple plots together
#' @param ... optional arguments
#'  
#' @return A \code{data.frame} or \code{list} of data.frames corresponding the variables 
#' contained within the \code{randomForestSRC::plot.variable} output. 
#' 
#' @seealso \code{\link{plot.gg_partial}} \code{randomForestSRC::plot.variable}
#' 
#' @aliases gg_partial
#' 
#' @export gg_partial.ggRandomForests gg_partial
#' @importFrom dplyr tbl_df
#' 
#' @examples
#' 
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
gg_partial.ggRandomForests <- function(object, 
                                      named,
                                      ...){
  if(!inherits(object,"plot.variable")){
    stop("gg_partial expects a plot.variable object, Run plot.variable with partial=TRUE")
  }
  if(!object$partial) invisible(gg_variable(object, ...))
  
  # How many variables
  n.var=length(object$pData)
  
  # Create a list of data
  pDat <- lapply(1:n.var, function(ind){
    data.frame(cbind(yhat=object$pData[[ind]]$yhat, 
                     x=object$pData[[ind]]$x.uniq))
  })
  
  # name the data, so labels come out correctly.
  for(ind in 1:n.var){
    pDat[[ind]] <- tbl_df(pDat[[ind]])
    colnames(pDat[[ind]])[-1] <- object$xvar.names[ind]
    if(!missing(named)) pDat[[ind]]$id=named
    class(pDat[[ind]]) <- c("gg_partial", class(pDat[[ind]]))
  }
  
  if(n.var ==1 ){
    # If there is only one, no need for a list
    invisible(pDat[[1]])
  }else{
    # otherwise, add a class label so we can handle it correctly. 
    class(pDat) <- c("gg_partial_list", class(pDat))
    invisible(pDat)
  }
  
}

gg_partial <- gg_partial.ggRandomForests
