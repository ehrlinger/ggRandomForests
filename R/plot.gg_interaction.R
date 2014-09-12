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
#' plot.gg_interaction
#' Plot a \code{\link{gg_interaction}} object, 
#' 
#' @param x gg_interaction object created from a randomForestSRC object
#' @param x_var variable (or list of variables) of interest.
#' @param ... arguments passed to the \code{\link{gg_interaction}} function.
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
#' 
#' @examples
#' \dontrun{
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
#' }
### error rate plot
plot.gg_interaction <- function(x, x_var, ...){
  object <- x 
  if(is.matrix(object)){
    # Check to make sure it's the right type of matrix...
    if(sum(colnames(object) != rownames(object)) > 0){
      stop("gg_interaction expects a rfsrc object, or a find.interaction object.")
    }
    class(object) <- c("gg_interaction", "rfsrc",class(object))
  }
  
  if(!inherits(object, "gg_interaction")) 
    object <- gg_interaction(object, ...)
  
  if(sum(x_var %in% rownames(object)) == 0){
    stop(paste("Invalid x_var (",x_var, ") specified, covariate not found.", sep=""))
  }
  
  if(length(x_var)> 1){
    intPlt.dta <- data.frame(cbind(names=rownames(object),t(object[which(rownames(object) %in% x_var),])))
    colnames(intPlt.dta) <- x_var
    intPlt.dta$rank <- 1:dim(intPlt.dta)[1]
    intPlt.dta <- melt(intPlt.dta, id.vars = "rank")
    
  }else{
    intPlt.dta <- data.frame(cbind(rank=1:dim(object)[1], object[which(rownames(object) %in% x_var),]))
    colnames(intPlt.dta)[2] <- "dpth" 
  }
  intPlt.dta$names <- rownames(intPlt.dta)
  #intPlt.dta <- intPlt.dta[-which(intPlt.dta$names=="viable"),]
  intPlt.dta$names <- factor(intPlt.dta$names, levels=intPlt.dta$names)
  ggplot(intPlt.dta)+ geom_point(aes_string(x="names", y="dpth"))+
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle=90)) +
    labs(x="", y="Minimal Depth")
  
}
