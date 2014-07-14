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
#' plot.gg_vimp
#' Plot a \code{\link{gg_vimp}} object, extracted variable importance of a 
#' rfsrc object
#' 
#' @param x gg_vimp object created from a randomForestSRC object
#' @param n.var restrict the plot to only nvar variable importance measures
#' @param ... optional arguments passed to gg_vimp if necessary
#' 
#' @return ggplot object
#' 
#' @export plot.gg_vimp
#' 
#' @seealso \code{\link{gg_vimp}}
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
#' @examples
#' \dontrun{
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggrf.obj<- gg_vimp(iris.obj)
#' 
#' plot.gg_vimp(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- gg_vimp(v.obj)
#' plot(ggrf.obj)
#'}
#' @importFrom ggplot2 ggplot geom_bar aes_string labs coord_flip
### error rate plot
plot.gg_vimp<- function(x, n.var, ...){
  object  <- x
  if(!inherits(object, "gg_vimp")) object<- gg_vimp(object, ...)
  if(missing(n.var)) n.var <- dim(object)[1]
  if(n.var > dim(object)[1]) n.var <- dim(object)[1]
  
  vimp.plt<-ggplot(object[1:n.var,])+
    geom_bar(aes_string(y="relVIMP", x="names", fill="positive"), 
             stat="identity", width=.5, color="black")+ 
    labs(x="", y="Relative Variable Importance") + 
    coord_flip()
  return(vimp.plt)
}
