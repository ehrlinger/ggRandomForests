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
#' plot.gg_minimal_vimp
#' Plot a \code{\link{gg_minimal_vimp}} object, the cumulative OOB error 
#' rates of the forest as a function of number of trees.
#' 
#' @param x gg_minimal_depth object created from a randomForestSRC object
#' @param modelsize should the figure be restricted to a subset of the points.
#' @param ... optional arguments (not used)
#' 
#' @export plot.gg_minimal_vimp
#' @importFrom ggplot2 ggplot aes_string geom_point labs geom_abline coord_flip
plot.gg_minimal_vimp <- function(x, modelsize, ...){
  object <- x
  
  # Test that object is the correct class object
  if(!inherits(object, "gg_minimal_vimp")){
    object <- gg_minimal_vimp(x, ...)
  }
  
  if(missing(modelsize)) modelsize <- dim(object)[1]
  if(modelsize > dim(object)[1]) modelsize <- dim(object)[1]
  if(length(unique(object$col)) > 1){
    object$col <- factor(object$col)
  }
  object$names <- factor(object$names, 
                         levels=object$names[order(as.numeric(object$depth))])
  
  object <- object[1:modelsize, ]
  
  # If we only have one class for coloring, just paint them black.
  if(length(unique(object$col)) > 1){
    gg_dta <- ggplot(object, aes_string(x="names", y="vimp", col="col"))+
      geom_point()+
      labs(x="Minimal Depth (Rank Order)", y="VIMP Rank", color="VIMP")+
      geom_abline(xintercept=0, slope=1, col="red", size=.5, linetype=2)+
      coord_flip()
  }else{
    gg_dta <- ggplot(object, aes_string(x="names", y="vimp"))+
      geom_point()+
      labs(x="Minimal Depth (Rank Order)", y="VIMP Rank")+
      geom_abline(xintercept=0, slope=1, col="red", size=.5, linetype=2)+
      coord_flip()
  }
  gg_dta
}