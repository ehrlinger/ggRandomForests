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
#' @param relative should we plot vimp or relative vimp. Defaults to vimp.
#' @param lbls A vector of alternative variable names.
#' @param bars A vector of alternative variable colors.
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
#' gg_dta <- gg_vimp(iris_rf)
#' plot(gg_dta)
#'  
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' 
#' # airq.obj <- rfsrc(Ozone ~ ., airquality)
#' data(airq_rf, package="ggRandomForests")
#' gg_dta <- gg_vimp(airq_rf)
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## survival example
#' ## ------------------------------------------------------------
#' data(veteran_rf, package="ggRandomForests")
#' gg_dta <- gg_vimp(veteran_rf)
#' plot(gg_dta)
#'}
#'
#' @importFrom ggplot2 ggplot geom_bar aes_string labs coord_flip facet_grid scale_x_discrete
### error rate plot
plot.gg_vimp<- function(x, n_var, relative, lbls, bars, ...){
  gg_dta  <- x
  if(!inherits(gg_dta, "gg_vimp")) gg_dta<- gg_vimp(gg_dta, ...)
  if(missing(n_var)) n_var <- dim(gg_dta)[1]
  if(n_var > dim(gg_dta)[1]) n_var <- dim(gg_dta)[1]
  
  if(!missing(bars)){
    # We have an alternative coloring
    if(length(bars)==n_var){
      gg_dta$positive[1:n_var]  <- bars
    }  
  }
  
  gg_plt<-ggplot(gg_dta[1:n_var,])
  
  if(missing(relative) | is.null(gg_dta$rel_vimp)){
    if(length(unique(gg_dta$positive))>1){
      gg_plt<-gg_plt+
        geom_bar(aes_string(y="vimp", x="vars", fill="positive"), 
                 stat="identity", width=.5, color="black")
    }else{
      gg_plt<-gg_plt+
        geom_bar(aes_string(y="vimp", x="vars"), 
                 stat="identity", width=.5, color="black")
    }
    gg_plt<-gg_plt+labs(x="", y="Variable Importance")
    
  }else{
    if(length(unique(gg_dta$positive))>1){
      gg_plt<-gg_plt+
        geom_bar(aes_string(y="rel_vimp", x="vars", fill="positive"), 
                 stat="identity", width=.5, color="black")
    }else{
      gg_plt<-gg_plt+
        geom_bar(aes_string(y="rel_vimp", x="vars"), 
                 stat="identity", width=.5, color="black")
    }   
    gg_plt<-gg_plt+ 
      labs(x="", y="Relative Variable Importance") 
  }
  
  if(!missing(lbls) ){
    if(length(lbls) >= length(gg_dta$vars)){
      st.lbls <- lbls[as.character(gg_dta$vars)]
      names(st.lbls) <- as.character(gg_dta$vars)
      st.lbls[which(is.na(st.lbls))] <- names(st.lbls[which(is.na(st.lbls))])
      
      gg_plt <- gg_plt+
        scale_x_discrete(labels=st.lbls)
    }
  }
  if(is.null(gg_dta$set))
    gg_plt<-gg_plt+ 
    coord_flip()
  else  
    gg_plt<-gg_plt+ 
    coord_flip()+facet_grid(~set)
  
  return(gg_plt)
}
