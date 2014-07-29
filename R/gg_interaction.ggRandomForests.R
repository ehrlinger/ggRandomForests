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
#' Minimal Depth Variable Interaction data object.
#' 
#' Basically, this function adds attributes to the results of running
#' \code{find.interaction} on an \code{rfsrc} random forest. If passed  a 
#' \code{rfsrc} object, gg_interaction first runs the \code{find.interaction} 
#' function with all optional arguments.
#'
#' @param object a randomForestSRC object or the output from the
#' \code{find.interaction} function call
#' @param ... optional extra arguments passed to find.interaction
#' 
#' @seealso \code{find.interaction} \code{max.subtree} \code{var.select} \code{vimp}
#' 
#' @export gg_interaction gg_interaction.ggRandomForests 
#' @aliases gg_interaction
#' 
#' @importFrom randomForestSRC find.interaction
#' @importFrom dplyr tbl_df
#' 
#' @references
#' Ishwaran H. (2007). Variable importance in binary regression trees and 
#' forests, Electronic J. Statist., 1:519-537.
#' 
#' Ishwaran H., Kogalur U.B., Gorodeski E.Z, Minn A.J. and Lauer M.S. (2010).
#' High-dimensional variable selection for survival data. J. Amer. Statist. 
#' Assoc., 105:205-217.
#' 
#' Ishwaran H., Kogalur U.B., Chen X. and Minn A.J. (2011). Random survival 
#' forests for high-dimensional data. Statist. Anal. Data Mining, 4:115-132.
#' 
#' @examples
#' ## Examples from RFSRC package... 
#' ## ------------------------------------------------------------
#' ## find interactions, survival setting
#' ## ------------------------------------------------------------
#' ## data(pbc, package = "randomForestSRC") 
#' ## pbc.obj <- rfsrc(Surv(days,status) ~ ., pbc, nsplit = 10)
#' ## pbc_interaction <- find.interaction(pbc.obj, nvar = 8)
#' data(pbc_interaction, package="ggRandomForests")
#' gg_int <- gg_interaction(pbc_interaction)
#' 
#' plot(gg_int, x_var="bili")
#' plot(gg_int, x_var="copper")
#' 
#' 
#' ## ------------------------------------------------------------
#' ## find interactions, regression setting
#' ## ------------------------------------------------------------
#' ## airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' ##
#' ## TODO: VIMP interactions not handled yet....
#' ## find.interaction(airq.obj, method = "vimp", nrep = 3)
#' ## airq_interaction <- find.interaction(airq.obj)
#' data(airq_interaction, package="ggRandomForests")
#' gg_int <- gg_interaction(airq_interaction)
#' 
#' plot(gg_int, x_var="Temp")
#' plot(gg_int, x_var="Solar.R")
#' 
#' 
#' ## ------------------------------------------------------------
#' ## find interactions, classification setting
#' ## ------------------------------------------------------------
#' ## iris.obj <- rfsrc(Species ~., data = iris)
#' ## TODO: VIMP interactions not handled yet....
#' ## find.interaction(iris.obj, method = "vimp", nrep = 3)
#' ## iris_interaction <- find.interaction(iris.obj)
#' data(iris_interaction, package="ggRandomForests")
#' gg_int <- gg_interaction(iris_interaction)
#' 
#' plot(gg_int, x_var="Petal.Width")
#' plot(gg_int, x_var="Petal.Length")
#' 
#' 

gg_interaction.ggRandomForests <- function(object, ...){
  if(inherits(object, "matrix")){
    
    object <- tbl_df(data.frame(object))
    
    # Check to make sure it's the right type of matrix...
    if(sum(colnames(object) != rownames(object)) > 0){
      stop("gg_interaction expects a find.interaction object.")
    }
    
    
    class(object) <- c("gg_interaction",  class(object))

  }else if (inherits(object, "rfsrc")) {
    
    
    # If we called this with a rfsrc object, we need to run find.interaction.
    warning("Forest object means we assume max.subtree method for finding interactions.\nThis may take some time.")
    
    object_interact <- find.interaction(object,...)
    object <- tbl_df(data.frame(object_interact))
    class(object) <- c("gg_interaction", class(object_interact))
  }else{
    stop("gg_interaction expects a rfsrc or find.interaction object.")
  }

  invisible(object)
}

gg_interaction <- gg_interaction.ggRandomForests