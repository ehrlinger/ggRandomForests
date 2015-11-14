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
#' Predicted response plot from a \code{\link{gg_rfsrc}} object.
#' 
#' Plot the predicted response from a \code{\link{gg_rfsrc}} object, the 
#' \code{\link[randomForestSRC]{rfsrc}} prediction, using the OOB prediction from the forest.
#'  
#' @param x \code{\link{gg_rfsrc}} object created from a \code{\link[randomForestSRC]{rfsrc}} object
#' @param ... arguments passed to \code{\link{gg_rfsrc}}.
#' 
#' @return \code{ggplot} object
#' 
#' @seealso \code{\link{gg_rfsrc}} \code{\link[randomForestSRC]{rfsrc}}
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for 
#' R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression 
#' and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_iris)
#' 
#' plot.gg_rfsrc(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_airq)
#' 
#' plot.gg_rfsrc(gg_dta)
#' 
#' ## -------- Boston data
#' data(rfsrc_Boston, package="ggRandomForests")
#' plot.gg_rfsrc(rfsrc_Boston) 
#' 
#' ## -------- mtcars data
#' data(rfsrc_mtcars, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(rfsrc_mtcars)
#' 
#' plot.gg_rfsrc(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' data(rfsrc_veteran, package = "ggRandomForests")
#' gg_dta <- gg_rfsrc(rfsrc_veteran)
#' plot(gg_dta)
#' 
#' gg_dta <- gg_rfsrc(rfsrc_veteran, conf.int=.95)
#' plot(gg_dta)
#' 
#' gg_dta <- gg_rfsrc(rfsrc_veteran, by="trt")
#' plot(gg_dta)
#' 
#' ## -------- pbc data
#' data(rfsrc_pbc, package = "ggRandomForests")
#' gg_dta <- gg_rfsrc(rfsrc_pbc)
#' plot(gg_dta)
#' 
#' gg_dta <- gg_rfsrc(rfsrc_pbc, conf.int=.95)
#' plot(gg_dta)
#' 
#' gg_dta <- gg_rfsrc(rfsrc_pbc, by="treatment")
#' plot(gg_dta)
#' 
#' 
#' }
#' @importFrom ggplot2 ggplot aes_string geom_step geom_ribbon labs geom_point geom_jitter geom_boxplot theme element_blank
#' @importFrom tidyr gather_
#' 
#' @export
plot.gg_rfsrc <- function(x,
                          ...){
  gg_dta <- x
  
  # Unpack argument list
  arg_set <- list(...)
  
  ## rfsrc places the class in position 1.
  if(inherits(gg_dta, "rfsrc")) gg_dta <- gg_rfsrc(gg_dta, ...)
  
  ## Classification forest?
  if(inherits(gg_dta, "class")){
    
    if(ncol(gg_dta) < 3){
      
      gg_plt <- ggplot(gg_dta) +
        geom_jitter(aes_string(x=1, y=colnames(gg_dta)[1],
                               color=colnames(gg_dta)[2],
                               shape=colnames(gg_dta)[2]), ...) +
        geom_boxplot(aes_string(x=1, y=colnames(gg_dta)[1]),
                     outlier.colour = "transparent", fill="transparent", notch = TRUE, ...) +
        theme(axis.ticks = element_blank(), axis.text.x = element_blank())
    }else{
      gathercols <- colnames(gg_dta)[-which(colnames(gg_dta) == "y")]
      gg_dta.mlt <- tidyr::gather_(gg_dta, "variable", "value", gathercols)
      
      gg_plt <- ggplot(gg_dta.mlt, aes_string(x="variable",y="value")) +
        geom_jitter(aes_string(color="y",shape="y"), alpha=.5)
    }
    gg_plt <- gg_plt + labs(y="Predicted (%)", x="")
    
    
  }else if(inherits(gg_dta, "surv")){
    
    # Check for conf.int calculations
    if("lower" %in% colnames(gg_dta)){
      if(is.null(arg_set$alpha)){
        alph <- .3
      }else{
        alph <- arg_set$alpha * .5
        arg_set$alpha <- NULL
      }
      
      if("group" %in% colnames(gg_dta)){
        gg_plt <- ggplot(gg_dta) +
          geom_ribbon(aes_string(x="time", ymin="lower", ymax="upper", fill="group"),
                      alpha=alph,...) +
          geom_step(aes_string(x="time", y="median", color="group"), ...)
      }else{
        gg_plt <- ggplot(gg_dta) +
          geom_ribbon(aes_string(x="time", ymin="lower", ymax="upper"),alpha=alph) +
          geom_step(aes_string(x="time", y="median"), ...)
      }
    }else{
      
      # Lines by observation
      gg_plt <- ggplot(gg_dta,
                       aes_string(x="variable", y="value", col="cens", 
                                  by="ptid")) +
        geom_step(...)
    }
    
    gg_plt <- gg_plt  +
      labs(x="time (years)", y="Survival (%)")
    
    
  }else if(inherits(gg_dta, "regr")){
    if("group" %in% colnames(gg_dta)){
      gg_plt <- ggplot(gg_dta, aes_string(x="group", y="yhat"))
    }else{
      gg_plt <- ggplot(gg_dta, aes_string(x=1, y="yhat"))
    }
    
    gg_plt <- gg_plt +
      geom_jitter(, ...) +
      geom_boxplot(outlier.colour = "transparent", fill="transparent", notch = TRUE, ...) +
      labs(y="Predicted Value", x=colnames(gg_dta)[2]) +
      theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  }else{
    stop(paste("Plotting for ", class(gg_dta)[2], " randomForestSRC is not yet implemented."))
  }
  return(gg_plt)
}
