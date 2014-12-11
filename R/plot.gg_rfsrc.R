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
#' \code{randomForestSRC::rfsrc} prediction, using the OOB prediction from the forest.
#'  
#' @param x \code{\link{gg_rfsrc}} object created from a \code{randomForestSRC::rfsrc} object
#' @param ... arguments passed to \code{\link{gg_rfsrc}}.
#' 
#' @return \code{ggplot} object
#' 
#' @export plot.gg_rfsrc
#' 
#' @seealso \code{\link{gg_rfsrc}} \code{randomForestSRC::rfsrc}
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
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' data(iris_rf, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(iris_rf)
#' 
#' plot.gg_rfsrc(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' # airq.obj <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' data(airq_rf, package="ggRandomForests")
#' gg_dta<- gg_rfsrc(airq_rf)
#' 
#' plot.gg_rfsrc(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' data(veteran_rf, package = "ggRandomForests")
#' gg_dta <- gg_rfsrc(veteran_rf)
#' plot(gg_dta)
#' 
#' }
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes_string geom_step geom_ribbon labs geom_point geom_smooth geom_jitter geom_boxplot theme element_blank
### error rate plot
plot.gg_rfsrc<- function(x, ...){
  gg_dta <- x
  
  # Unpack argument list
  arg_set <- list(...)

  # Initialize variables for gather statement... to silence R CMD CHECK
  #!TODO must be a better way to do this.?select
  variable <- value <- y <- ptid <- cens <- NA
  
  ## rfsrc places the class in position 1.
  if(class(gg_dta)[1] == "rfsrc") gg_dta<- gg_rfsrc(gg_dta, ...)
  
  ## Classification forest?
  if(inherits(gg_dta, "class")){
    if(dim(gg_dta)[2] < 3){
      
      gg_plt <- ggplot(gg_dta)+
        geom_jitter(aes_string(x=1, y=colnames(gg_dta)[1],
                               color=colnames(gg_dta)[2],
                               shape=colnames(gg_dta)[2]), ...)+
        geom_boxplot(aes_string(x=1, y=colnames(gg_dta)[1]),
                     outlier.colour = "transparent", fill="transparent", notch = TRUE, ...)
    }else{
      gg_dta.mlt <- gg_dta %>% gather(variable, value, -y)
      gg_plt <- ggplot(gg_dta.mlt, aes_string(x="variable",y="value"))+
        geom_jitter(aes_string(color="y",shape="y"), alpha=.5)
    }
    gg_plt <- gg_plt + labs(y="Predicted (%)", x="")
  }else if(inherits(gg_dta,"surv")){
    if(inherits(gg_dta,"survSE")){
      # Summarized survival plot for the group...
      gg_dta.t <-  select(gg_dta, time, median, mean)%>% gather(variable, value,-time)
      
      if(is.null(arg_set$alpha)){
        alph=.3
      }else{
        alph = arg_set$alpha*.5
        arg_set$alpha <- NULL
      }
      gg_plt <- ggplot(gg_dta.t)+
        geom_ribbon(aes_string(x="time", ymin="lower", ymax="upper"), 
                    data=gg_dta, alpha=alph, ...=arg_set)+
        geom_step(aes_string(x="time", y="value", color="variable"), ...)
      
    }else{
      gg_dta.mlt <- gg_dta %>% gather(variable, value, -ptid,-cens)
      gg_dta.mlt$variable <- as.numeric(as.character(gg_dta.mlt$variable))
      gg_dta.mlt$ptid <- factor(gg_dta.mlt$ptid)
      
      # Lines by observation
      gg_plt <- ggplot(gg_dta.mlt)+
        geom_step(aes_string(x="variable", y="value", col="cens", by="ptid"), 
                  ...)
    }
    
    gg_plt<-gg_plt  +
      labs(x="time (years)", y="OOB Survival (%)")
  }else if(inherits(gg_dta, "regr")){
    gg_plt <- ggplot(gg_dta)+
      geom_jitter(aes_string(x=1, y="yhat"), ...)+
      geom_boxplot(aes_string(x=1, y="yhat"),
                   outlier.colour = "transparent", fill="transparent", notch = TRUE, ...)+
      labs(y="Predicted Value", x=colnames(gg_dta)[2])+
      theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  }else{
    stop(paste("Plotting for ", class(gg_dta)[2], " randomForestSRC is not yet implemented."))
  }
  return(gg_plt)
}
