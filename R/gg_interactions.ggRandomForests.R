####**********************************************************************
####**********************************************************************
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
#' @title gg_interactions extract the variable interactions.
#' 
#' @param object a randomForestSRC object or the output from the
#' \code{find.interactions} function call
#' @param ... optional extra arguments passed to find.interactions
#' 
#' @export gg_interactions.ggRandomForests gg_interactions
#' @aliases gg_interactions
#' 
gg_interactions.ggRandomForests <- function(object, ...){
  if(is.matrix(object)){
    # Check to make sure it's the right type of matrix...
    if(sum(colnames(object) != rownames(object)) > 0){
      stop("gg_interactions expects a rfsrc object, or a find.interactions object.")
    }
    class(object) <- c("gg_interactions", "rfsrc",class(object))
  }
  
  # Want to also handle a plot.variable where partial!= TRUE
  if (!inherits(object, "rfsrc")) {
    stop("gg_interactions expects a randomForest or plot.variable object.")
  }
  
  # IF we called this with a partial plot obect, instead of marginal.
  if(!inherits(object, "interactions")){
    warning("Forest object means we assume max.subtree method for finding interactions.\nThis may take some time.")
    object_interact <- find.interactions(object,method="maxsubtree")
    class(object_interact) <- c("gg_interactions", "rfsrc",class(object_interact))
  }
  
  invisible(object_interact)
}

gg_interactions <- gg_interactions.ggRandomForests