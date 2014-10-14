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
#' Plot a \code{\link{gg_vimp}} object, extracted variable importance of a 
#' \code{randomForestSRC::rfsrc} object
#' 
#' @param x \code{\link{gg_vimp}} object created from a \code{randomForestSRC::rfsrc} object
#' @param n_var restrict the plot to only nvar variable importance measures
#' @param ... optional arguments passed to gg_vimp if necessary
#' 
#' @return \code{ggplot} object
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
#' #' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' data(iris_rf, package="ggRandomForests")
#' ggrf <- gg_vimp(iris_rf)
#' plot(ggrf)
#'  
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' 
#' # airq.obj <- rfsrc(Ozone ~ ., airquality)
#' data(airq_rf, package="ggRandomForests")
#' ggrf <- gg_vimp(airq_rf)
#' plot(ggrf)
#' 
#' ## ------------------------------------------------------------
#' ## survival example
#' ## ------------------------------------------------------------
#' data(veteran_rf, package="ggRandomForests")
#' ggrf <- gg_vimp(veteran_rf)
#' plot(ggrf)
#'}
#'
#' @importFrom ggplot2 ggplot geom_bar aes_string labs coord_flip
### error rate plot
plot.gg_vimp<- function(x, n_var, ...){
  object  <- x
  if(!inherits(object, "gg_vimp")) object<- gg_vimp(object, ...)
  if(missing(n_var)) n_var <- dim(object)[1]
  if(n_var > dim(object)[1]) n_var <- dim(object)[1]
  
  vimp.plt<-ggplot(object[1:n_var,])+
    geom_bar(aes_string(y="relVIMP", x="names", fill="positive"), 
             stat="identity", width=.5, color="black")+ 
    labs(x="", y="Relative Variable Importance") + 
    coord_flip()
  return(vimp.plt)
}
