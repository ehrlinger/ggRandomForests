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
#' @export gg_partial.ggRandomForests gg_partial
#' 
#' @importFrom parallel mclapply
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
  gg_dta <- mclapply(1:n.var, function(ind){
    
    if(length(object$pData[[ind]]$x.uniq) == length(object$pData[[ind]]$yhat)){
      data.frame(cbind(yhat=object$pData[[ind]]$yhat, 
                       x=object$pData[[ind]]$x.uniq))
    }else{
      
      x <- rep(as.character(object$pData[[ind]]$x.uniq),
               rep(object$n, object$pData[[ind]]$n.x))
      tmp <- data.frame(cbind(yhat=x, x=x))        
      tmp$x <- factor(tmp$x)
      tmp$yhat <- object$pData[[ind]]$yhat
      tmp
    }
  })
  
  names(gg_dta) <- object$xvar.names
  
  # name the data, so labels come out correctly.
  for(ind in 1:n.var){
    colnames(gg_dta[[ind]])[-1] <- object$xvar.names[ind]
    if(!missing(named)) gg_dta[[ind]]$id=named
    class(gg_dta[[ind]]) <- c("gg_partial", class(gg_dta[[ind]]))
  }
  
  if(n.var ==1 ){
    # If there is only one, no need for a list
    invisible(gg_dta[[1]])
  }else{
    # otherwise, add a class label so we can handle it correctly. 
    class(gg_dta) <- c("gg_partial_list", class(gg_dta))
    invisible(gg_dta)
  }
  
}

gg_partial <- gg_partial.ggRandomForests
