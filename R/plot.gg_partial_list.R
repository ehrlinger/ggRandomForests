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
#' The function plots the \code{\link[randomForestSRC]{rfsrc}} response variable (y-axis) against
#' the covariate of interest (specified when creating the
#'  \code{gg_partial_list} object).
#' 
#' @param x \code{gg_partial_list} object created from a \code{\link{gg_partial}} 
#' forest object
#' @param points plot points (boolean) or a smooth line.
#' @param panel should the entire list be plotted together?
#' @param ... extra arguments
#' 
#' @return list of \code{ggplot} objects, or a single faceted \code{ggplot} object
#' 
#' @seealso \code{\link[randomForestSRC]{plot.variable}} \code{\link{gg_partial}} 
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
#' @importFrom ggplot2 ggplot aes labs geom_point geom_smooth facet_wrap
#' @importFrom parallel mclapply
#'
#' @export
plot.gg_partial_list <- function(x, points=TRUE, panel=FALSE, ...){
  gg_dta <- x 
  
  if(!inherits(gg_dta, "list")) stop("Functions expects a list object")
  
  lng <- length(gg_dta)
  
  # One figure, with facets?
  if(panel){
    
    # Go through each element of the list, and add the variable name column,
    # and rename the value column to "value"
    nms <- names(gg_dta)
    
    cls <- sapply(nms, function(nm){
      class(gg_dta[[nm]][,nm])
    })
    
    gg_dta <- mclapply(nms, function(nm){
      obj <- gg_dta[[nm]]
      colnames(obj)[which(colnames(obj) == nm)]  <- "value"
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
    
    if(sum(cls == "factor") == length(cls)){
      gg_plt <- gg_plt +
        geom_boxplot(...)
    }else{
      if(points){
        gg_plt <- gg_plt +
          geom_point(...) 
      }else{
        gg_plt <- gg_plt +
          geom_smooth(...) 
      }
    }
    return(gg_plt +
             facet_wrap(~variable,
                        scales="free_x")
    )
  }else{
    # OR a list of figures.
    gg_plt <- vector("list", length=lng)
    
    for (ind in 1:lng){
      gg_plt[[ind]] <- plot.gg_partial(gg_dta[[ind]], points, ...)
    }
    
    return(gg_plt)
  }
}
