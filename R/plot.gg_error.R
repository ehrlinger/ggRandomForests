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
#' Plot a \code{\link{gg_error}} object
#' 
#' A plot of the cumulative OOB error rates of the random forest as a 
#' function of number of trees.
#' 
#' @param x gg_error object created from a \code{\link[randomForestSRC]{rfsrc}} object
#' @param ... extra arguments passed to \code{ggplot} functions
#' 
#' @return \code{ggplot} object
#' 
#' @details The gg_error plot is used to track the convergence of the 
#' randomForest. This figure is a reproduction of the error plot
#' from the \code{\link[randomForestSRC]{plot.rfsrc}} function.
#' 
#' @seealso \code{\link{gg_error}} \code{\link[randomForestSRC]{rfsrc}}
#'  \code{\link[randomForestSRC]{plot.rfsrc}}
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews, 
#' 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression 
#' and Classification (RF-SRC), R package version 1.4.
#' 
#' @examples
#' \dontrun{
#' ## Examples from RFSRC package... 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## ------------- iris data
#' ## You can build a randomForest
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' # ... or load a cached randomForestSRC object
#' data(rfsrc_iris, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_error(rfsrc_iris)
#' 
#' # Plot the gg_error object
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' ## ------------- airq data
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' # ... or load a cached randomForestSRC object
#' data(rfsrc_airq, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_error(rfsrc_airq)
#' 
#' # Plot the gg_error object
#' plot(gg_dta)
#' 
#' ## ------------- Boston data
#' data(rfsrc_Boston, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_error(rfsrc_Boston)
#' 
#' # Plot the gg_error object
#' plot(gg_dta)
#' 
#' ## ------------- mtcars data
#' data(rfsrc_mtcars, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_error(rfsrc_mtcars)
#' 
#' # Plot the gg_error object
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## ------------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' 
#' # Load a cached randomForestSRC object
#' data(rfsrc_veteran, package="ggRandomForests")
#' 
#' gg_dta <- gg_error(rfsrc_veteran)
#' plot(gg_dta)
#' 
#' ## ------------- pbc data
#' # Load a cached randomForestSRC object
#' data(rfsrc_pbc, package="ggRandomForests")
#' 
#' gg_dta <- gg_error(rfsrc_pbc)
#' plot(gg_dta)
#'}
#' @importFrom ggplot2 ggplot geom_line theme aes_string labs 
#' @importFrom tidyr gather_
#' @export
plot.gg_error <- function(x, ...){
  gg_dta <- x
    
  if(inherits(gg_dta, "rfsrc")) gg_dta <- gg_error(gg_dta)
  
  if(!inherits(gg_dta, "gg_error")) stop("Incorrect object type: Expects a gg_error object")
  
  if(dim(gg_dta)[2] > 2){
    gathercol <- colnames(gg_dta)[-which(colnames(gg_dta)=="ntree")]
    gg_dta <- gather_(gg_dta, "variable", "value", gathercol)
    gg_plt <- ggplot(gg_dta, aes_string(x="ntree", y="value", col="variable"))
  }else{
    # We expect the object to have the following columns
    gg_plt <- ggplot(gg_dta, aes_string(x="ntree", y="error"))
  }
  gg_plt <- gg_plt +
    geom_line() +
    labs(x = "Number of Trees",
         y = "OOB Error Rate", color="Outcome")
  
  if(length(unique(gg_dta$variable)) == 1){
    gg_plt <- gg_plt + theme(legend.position="none")
  }
  return(gg_plt)
}
