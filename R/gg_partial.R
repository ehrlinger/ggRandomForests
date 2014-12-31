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
#' for partial plots  (where \code{partial=TRUE}) into a data object for creation of 
#' partial dependence plots using the \code{\link{plot.gg_partial}} function. 
#' 
#' Partial variable dependence plots are the risk adjusted estimates of the specified 
#' response as a function of a single covariate, possibly subsetted on other covariates.
#' 
#' @param object the partial variable dependence data object from 
#'   \code{randomForestSRC::plot.variable} function
#' @param named optional column for merging multiple plots together
#' @param ... optional arguments
#'  
#' @return \code{gg_partial} object. A \code{data.frame} or \code{list} of 
#' \code{data.frames} corresponding the variables 
#' contained within the \code{randomForestSRC::plot.variable} output. 
#' 
#' @seealso \code{\link{plot.gg_partial}} \code{randomForestSRC::plot.variable}
#' 
#' @export gg_partial.rfsrc gg_partial
#' 
#' @importFrom parallel mclapply
#' 
#' @references 
#' Friedman, Jerome H. 2000. "Greedy Function Approximation: A Gradient Boosting 
#' Machine." Annals of Statistics 29: 1189-1232.
#' 
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification
#' ## ------------------------------------------------------------
#' 
#' ## iris "Petal.Width" partial dependence plot
#' ##
#' # rfsrc_iris <- rfsrc(Species ~., data = iris)
#' # partial_iris <- plot.variable(rfsrc_iris, xvar.names = "Petal.Width",
#' #                            partial=TRUE)
#' data(partial_iris, package="ggRandomForests")
#' 
#' gg_dta <- gg_partial(partial_iris)
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## regression
#' ## ------------------------------------------------------------
#' 
#' ## airquality "Wind" partial dependence plot
#' ##
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality)
#' # partial_airq <- plot.variable(rfsrc_airq, xvar.names = "Wind",
#' #                            partial=TRUE, show.plot=FALSE)
#' data(partial_airq, package="ggRandomForests")
#'
#' gg_dta <- gg_partial(partial_airq)
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' ## survival "age" partial variable dependence plot
#' ##
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time,status)~., veteran, nsplit = 10, ntree = 100)
#' #
#' ## 30 day partial plot for age
#' # partial_veteran <- plot.variable(rfsrc_veteran, surv.type = "surv", 
#' #                               partial = TRUE, time=30, 
#' #                               xvar.names = "age", 
#' #                               show.plots=FALSE)
#' data(partial_veteran, package="ggRandomForests")
#' 
#' gg_dta <- gg_partial(partial_veteran[[1]])
#' plot(gg_dta)
#' 
#' @aliases gg_partial gg_partial_list
#' @name gg_partial
#' @name gg_partial_list
#' 
gg_partial.rfsrc <- function(object, 
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

gg_partial <- gg_partial.rfsrc
