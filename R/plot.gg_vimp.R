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
#' \code{\link[randomForestSRC]{rfsrc}} object
#' 
#' @param x \code{\link{gg_vimp}} object created from a 
#' \code{\link[randomForestSRC]{rfsrc}} object
#' @param relative should we plot vimp or relative vimp. Defaults to vimp.
#' @param lbls A vector of alternative variable labels. Item names should be 
#' the same as the variable names. 
#' @param ... optional arguments passed to gg_vimp if necessary
#' 
#' @return \code{ggplot} object
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
#' ## -------- iris data
#' # rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' data(rfsrc_iris, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_iris)
#' plot(gg_dta)
#'  
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' ## -------- air quality data 
#' # rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_airq)
#' plot(gg_dta)
#' 
#' ## -------- Boston data
#' data(rfsrc_Boston, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_Boston)
#' plot(gg_dta)
#' 
#' ## -------- mtcars data
#' data(rfsrc_mtcars, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_mtcars)
#' plot(gg_dta)
#' 
#' ## ------------------------------------------------------------
#' ## survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' data(rfsrc_veteran, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_veteran)
#' plot(gg_dta)
#'
#' ## -------- pbc data
#' data(rfsrc_pbc, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_pbc)
#' plot(gg_dta)
#' 
#'}
#'
#' @importFrom ggplot2 ggplot geom_bar aes_string labs coord_flip facet_grid scale_x_discrete
#' @export
plot.gg_vimp <- function(x, relative, lbls, ...){
  gg_dta  <- x
  if(!inherits(gg_dta, "gg_vimp")) gg_dta <- gg_vimp(gg_dta, ...)

    # Classification...
  arg_set <- as.list(substitute(list(...)))[-1L]
  
  nvar <- nrow(gg_dta)
  if(!is.null(arg_set$nvar)){
    if(is.numeric(arg_set$nvar) & arg_set$nvar > 1){
      if(arg_set$nvar < nrow(gg_dta)){
        nvar <- arg_set$nvar 
        gg_dta <- gg_dta[1:nvar,]
      }
  }}
  
  gg_plt <- ggplot(gg_dta)
  
  #  if(missing(relative) | is.null(gg_dta$rel_vimp)){
  if(length(unique(gg_dta$positive)) > 1){
    gg_plt<-gg_plt +
      geom_bar(aes_string(y="vimp", x="vars", fill="positive"), 
               stat="identity", width=.5, color="black")
  }else{
    gg_plt <- gg_plt +
      geom_bar(aes_string(y="vimp", x="vars"), 
               stat="identity", width=.5, color="black")
  }
  gg_plt <- gg_plt + labs(x="", y="Variable Importance")
  
  ## I export a rel_vimp from the gg_vimp any more,
  #   }else{
  #     if(length(unique(gg_dta$positive))>1){
  #       gg_plt<-gg_plt+
  #         geom_bar(aes_string(y="rel_vimp", x="vars", fill="positive"), 
  #                  stat="identity", width=.5, color="black")
  #     }else{
  #       gg_plt<-gg_plt+
  #         geom_bar(aes_string(y="rel_vimp", x="vars"), 
  #                  stat="identity", width=.5, color="black")
  #     }   
  #     gg_plt<-gg_plt+ 
  #       labs(x="", y="Relative Variable Importance") 
  #   }
  #  
  if(!missing(lbls) ){
    # Print a warning if the lbls is not a named vector.
    
    if(length(lbls) >= length(gg_dta$vars)){
      st.lbls <- lbls[as.character(gg_dta$vars)]
      names(st.lbls) <- as.character(gg_dta$vars)
      st.lbls[which(is.na(st.lbls))] <- names(st.lbls[which(is.na(st.lbls))])
      
      gg_plt <- gg_plt +
        scale_x_discrete(labels=st.lbls)
    }
  }
  if(is.null(gg_dta$set) | length(unique(gg_dta$set)) < 2){
    gg_plt <- gg_plt + 
      coord_flip()
  }else{  
    gg_plt <- gg_plt + 
      coord_flip() + facet_grid(~set)
  }
  
  return(gg_plt)
}
