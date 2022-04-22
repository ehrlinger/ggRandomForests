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
#' The function plots the \code{\link[randomForestSRC]{rfsrc}} response variable (y-axis) against
#' the covariate of interest (specified when creating the
#'  \code{\link{gg_partial}} object).
#' 
#' @param x \code{\link{gg_partial}} object created from a \code{\link[randomForestSRC]{rfsrc}} forest object
#' @param points plot points (boolean) or a smooth line.
#' @param error "shade", "bars", "lines" or "none"
#' @param ... extra arguments passed to \code{ggplot2} functions.  
#' 
#' @return \code{ggplot} object
#' 
#' @seealso \code{\link[randomForestSRC]{plot.variable}} \code{\link{gg_partial}} 
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
#' ## -------- iris data
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
#' ## -------- air quality data
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
#' gg_dta.m <- gg_dta[["Month"]]
#' plot(gg_dta.m, notch=TRUE)
#' 
#' gg_dta[["Month"]] <- NULL
#' plot(gg_dta, panel=TRUE)
#' 
#' ## -------- Boston data
#' data(partial_Boston, package="ggRandomForests")
#'
#' gg_dta <- gg_partial(partial_Boston)
#' plot(gg_dta)
#' plot(gg_dta, panel=TRUE)
#' 
#' ## -------- mtcars data
#' data(partial_mtcars, package="ggRandomForests")
#'
#' gg_dta <- gg_partial(partial_mtcars)
#' 
#' plot(gg_dta)
#' 
#' gg_dta.cat <- gg_dta
#' gg_dta.cat[["disp"]] <- gg_dta.cat[["wt"]] <- gg_dta.cat[["hp"]] <- NULL
#' gg_dta.cat[["drat"]] <- gg_dta.cat[["carb"]] <- gg_dta.cat[["qsec"]] <- NULL
#'  
#' plot(gg_dta.cat, panel=TRUE)
#' 
#' gg_dta[["cyl"]] <- gg_dta[["vs"]] <- gg_dta[["am"]] <- NULL
#' gg_dta[["gear"]] <- NULL
#' plot(gg_dta, panel=TRUE)
#' 
#' ## ------------------------------------------------------------
#' ## survival examples
#' ## ------------------------------------------------------------
#' ## -------- veteran data
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
#' gg_dta.cat <- gg_dta
#' gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
#' plot(gg_dta, panel=TRUE)
#' 
#' gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <- gg_dta.cat[["age"]] <- NULL 
#' plot(gg_dta.cat, panel=TRUE, notch=TRUE)
#' 
#' gg_dta <- lapply(partial_veteran, gg_partial)
#' length(gg_dta)
#' gg_dta <- combine.gg_partial(gg_dta[[1]], gg_dta[[2]] )
#' 
#' plot(gg_dta[["karno"]])
#' plot(gg_dta[["celltype"]])
#' 
#' gg_dta.cat <- gg_dta
#' gg_dta[["celltype"]] <- gg_dta[["trt"]] <- gg_dta[["prior"]] <- NULL
#' plot(gg_dta, panel=TRUE)
#' 
#' gg_dta.cat[["karno"]] <- gg_dta.cat[["diagtime"]] <- gg_dta.cat[["age"]] <- NULL 
#' plot(gg_dta.cat, panel=TRUE, notch=TRUE)
#' 
#' ## -------- pbc data
#' }
#'  
#' @export
plot.gg_partial <- function(x, points=TRUE,
                            error=c("none", "shade","bars","lines"),
                            ...){
  
  gg_dta <- x 
  if(inherits(x, "plot.variable")){
    gg_dta <- gg_partial(x, ...)
  }else if(!inherits(x, "gg_partial")){
    stop("gg_partial expects an object from the rfsrc::plot.variable function")
  }
  
  error <- match.arg(error)
  arg_list <- list(...)
  
  if(!is.null(arg_list$se)) 
    if(arg_list$se != FALSE)
      error <- "none"
  
  # Get the colname of the independent variable
  h_name <- colnames(gg_dta)[2]
  
  colnames(gg_dta)[2] <- "x"
  
  if(is.null(gg_dta$group)){
    gg_plt <- ggplot(gg_dta,aes_string(x="x", y="yhat"))
  }else{
    gg_plt <- ggplot(gg_dta,aes_string(x="x", y="yhat", shape="group", color="group"))
  }
  if(!is.null(gg_dta$se)){
    conf.int <- .95
    if(!is.null(arg_list$conf.int)) conf.int <- arg_list$conf.int
    
    if(length(conf.int) == 1){
      if(conf.int > 1)conf.int <- conf.int / 100
      if(conf.int > .5){
        err <- qnorm(1 - conf.int / 2)
      }else{
        err <- qnorm(conf.int)
      }
    }else{
      # Two sided, 
      err <- qnorm(conf.int[1])
    }
    
    gg_dta$upper <- gg_dta$yhat + err * gg_dta$se
    gg_dta$lower <- gg_dta$yhat - err * gg_dta$se
    
    gg_plt <- switch(error,
                     # Shading the standard errors
                     shade = gg_plt + 
                       geom_ribbon(aes_string(x="x", ymax="upper", ymin="lower"),
                                   alpha=.3, data=gg_dta),
                     # Or showing error bars
                     bars = {
                       # Need to figure out how to remove some of these points when 
                       # requesting error bars, or this will get really messy.
                       #                     errFll <- fll
                       #                     if(!missing(errbars) )errFll <- errFll[errbars,]
                       gg_plt + 
                         geom_errorbar(aes_string(x="x", ymax="upper", ymin="lower"), 
                                       data=gg_dta)
                     },
                     lines= gg_plt + 
                       geom_smooth(aes_string(x="x", y="upper"), 
                                   linetype=2, data=gg_dta, se=FALSE) +
                       geom_smooth(aes_string(x="x", y="lower"), 
                                   linetype=2, data=gg_dta, se=FALSE), 
                     none=gg_plt)
  }
  gg_plt <- gg_plt +
    labs(x=h_name, y="predicted")
  if(!is.factor(gg_dta$x)){
    if(points){  
      gg_plt <- gg_plt + geom_point( ...)
    }else{
      gg_plt <- gg_plt + geom_smooth( ...)
    }
    
  }else{
    gg_plt <- gg_plt + geom_boxplot(...)
  }
  
  return(gg_plt)
}
