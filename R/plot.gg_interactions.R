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
#' plot.gg_variable
#' Plot a \code{\link{gg_variable}} object, 
#' 
#' @param x gg_variable object created from a randomForestSRC object
#' @param x_var variable (or list of variables) of interest.
#' @param ... arguments passed to the \code{\link{gg_variable}} function.
#' 
#' @return ggplot object
#' 
#' @export plot.gg_interaction
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for 
#' R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.4.
#' 
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth labs
### error rate plot
plot.gg_interaction <- function(x, x_var, ...){
  object <- x 
  if(is.matrix(object) & !inherits(object, "rfsrc")){
    # Check to make sure it's the right type of matrix...
    if(sum(colnames(object) != rownames(object)) > 0){
      stop("gg_interaction expects a rfsrc object, or a find.interaction object.")
    }
    class(object) <- c("gg_interaction", "rfsrc",class(object))
  }
  
  if(!inherits(object, "rfsrc")){
    stop("plot.gg_interaction expects either a rfsrc object, a gg_interaction object, or a find.interaction output.")
  } 
  if(!inherits(object, "gg_interaction")) 
    object <- gg_interaction(object, ...)
  
  intPlt.dta <- data.frame(cbind(dpth=object[which(rownames(object) %in% x_var),]))
  intPlt.dta$names <- cbind(rownames(object))
  
  if(length(x_var)==1)
    intPlt.dta <- filter(intPlt.dta, names != x_var)
  
  #intPlt.dta <- intPlt.dta[-which(intPlt.dta$names=="viable"),]
  intPlt.dta$names <- factor(intPlt.dta$names, levels=intPlt.dta$names)
  ggplot(intPlt.dta)+ geom_point(aes_string(x="names", y="dpth"))+
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle=90)) +
    labs(x="", y="Minimal Depth")
  
}
