#' ggError.ggRandomForests
#' Plot cumulative OOB error rates as a function of number of trees.
#' 
#' @param rfObj randomForestSRC object 
#' @param display show the graph (default: TRUE)
#' @param legend.position The legend location in c(x,y) world coordinates.
#' 
#' @return ggplot2 object
#' 
#' @export ggError.ggRandomForests
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ggError(iris.obj)
#' ggError(iris.obj, legend.position=c(.8,.8))
#' 
### error rate plot
ggError.ggRandomForests <- function(rfObj, display=TRUE, legend.position, ...) {
  ## Check that the input obect is of the correct type.
  if (inherits(rfObj, "rfsrc") == FALSE){
    stop("This function only works for Forests grown with the randomForestSRC package.")
  }
  if (is.null(rfObj$err.rate)) {
    stop("Performance values are not available for this forest.")
  }
  
  err <- rfObj$err.rate
  
  if(is.null(legend.position)) lgnd <- TRUE
  err <- as.data.frame(err)
  
  # If err is a single vector (has length but not dim), then there is only one response
  if(is.null(dim(err)&is.null(legend.position)) lgnd <- FALSE 
    
  err$indx <- 1:dim(err)[1]
  dta<-melt(err, id.vars = "indx")
  
  gDta=ggplot(dta, aes(x=indx,y=value, col=variable))+
    geom_line()+
    labs(x = "Number of Trees",
          y = "OOB Error Rate")

  if(!is.null(legend.position)) {
    gDta <- gDta + theme(legend.position=legend.position) 
  else if(is.null(legend.position) & lgnd) {
    gDta <- gDta
  }else {
    gDta <- gDta + theme(legend.position="none") 
  }
  if(display) show(gDta)
  
  invisible(gDta)
}

ggError <- ggError.ggRandomForests
