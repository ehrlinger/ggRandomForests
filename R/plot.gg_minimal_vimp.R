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
#' Plot a \code{\link{gg_minimal_vimp}} object for comparing the Minimal Depth and VIMP variable rankings.
#' 
#' @param x \code{\link{gg_minimal_depth}} object created from a \code{randomForestSRC::var.select} 
#' object
#' @param modelsize should the figure be restricted to a subset of the points.
#' @param lbls a vector of alternative variable names.
#' @param ... optional arguments (not used)
#' 
#' @return \code{ggplot} object
#' 
#' @export plot.gg_minimal_vimp
#' @importFrom ggplot2 ggplot aes_string geom_point labs geom_abline coord_flip scale_x_discrete
#'
#' @seealso \code{\link{gg_minimal_vimp}} \code{randomForestSRC::var.select}
#'  
#' @examples
#' \dontrun{
#' ## Examples from RFSRC package... 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## You can build a randomForest
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' # varsel_iris <- var.select(rfsrc_iris)
#' # ... or load a cached randomForestSRC object
#' data(varsel_iris, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_minimal_vimp(varsel_iris)
#' 
#' # Plot the gg_minimal_depth object
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' # rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' # varsel_airq <- var.select(rfsrc_airq)
#' # ... or load a cached randomForestSRC object
#' data(varsel_airq, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_vimp(varsel_airq)
#' 
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' 
#' ## -------- Boston data
#' data(varsel_Boston, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_vimp(varsel_Boston)
#' 
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' 
#' ## -------- mtcars data
#' data(varsel_mtcars, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_vimp(varsel_mtcars)
#' 
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' # varsel_veteran <- var.select(rfsrc_veteran)
#' # Load a cached randomForestSRC object
#' data(varsel_veteran, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_vimp(varsel_veteran)
#' plot(gg_dta)
#'   
#' ## -------- pbc data
#' data(varsel_pbc, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_vimp(varsel_pbc)
#' plot(gg_dta)
#' } 
plot.gg_minimal_vimp <- function(x, modelsize, lbls, ...){
  gg_dta <- x
  
  # Test that object is the correct class object
  if(!inherits(gg_dta, "gg_minimal_vimp")){
    gg_dta <- gg_minimal_vimp(x, ...)
  }
  
  if(missing(modelsize)) modelsize <- dim(gg_dta)[1]
  if(modelsize > dim(gg_dta)[1]) modelsize <- dim(gg_dta)[1]
  if(length(unique(gg_dta$col)) > 1){
    gg_dta$col <- factor(gg_dta$col)
  }
  gg_dta$names <- factor(gg_dta$names, 
                         levels=gg_dta$names[order(as.numeric(gg_dta$depth))])
  
  gg_dta <- gg_dta[1:modelsize, ]
  
  # If we only have one class for coloring, just paint them black.
  if(length(unique(gg_dta$col)) > 1){
    gg_plt <- ggplot(gg_dta, aes_string(x="names", y="vimp", col="col"))+
      labs(x="Minimal Depth (Rank Order)", y="VIMP Rank", color="VIMP")
    
  }else{
    gg_plt <- ggplot(gg_dta, aes_string(x="names", y="vimp"))+
      labs(x="Minimal Depth (Rank Order)", y="VIMP Rank")
  }
  if(!missing(lbls)){
    if(length(lbls) >= length(gg_dta$names)){
      st.lbls <- lbls[as.character(gg_dta$names)]
      names(st.lbls) <- as.character(gg_dta$names)
      st.lbls[which(is.na(st.lbls))] <- names(st.lbls[which(is.na(st.lbls))])
      
      gg_plt <- gg_plt +
        scale_x_discrete(labels=st.lbls)
    }
  }
  
  gg_plt + geom_point()+
    geom_abline(xintercept=0, slope=1, col="red", size=.5, linetype=2)+
    coord_flip()
  
}