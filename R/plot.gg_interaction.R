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
#' @param x gg_interaction object created from a \code{randomForestSRC::rfsrc} object
#' @param x_var variable (or list of variables) of interest.
#' @param lbls A vector of alternative variable names.
#' @param ... arguments passed to the \code{\link{gg_interaction}} function.
#' 
#' @return \code{ggplot} object
#' 
#' @export plot.gg_interaction
#' 
#' @seealso \code{\link{plot.gg_interaction}} \code{randomForestSRC::rfsrc} 
#' \code{randomForestSRC::find.interaction} 
#' \code{randomForestSRC::max.subtree} \code{randomForestSRC::var.select} 
#' \code{randomForestSRC::vimp}
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
#' @importFrom ggplot2 ggplot aes_string geom_point geom_smooth labs element_text
#' @importFrom tidyr gather
#' @importFrom dplyr filter
#' 
#' @examples
#' \dontrun{
#' #' ## Examples from randomForestSRC package... 
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
#' }
### error rate plot
plot.gg_interaction <- function(x, x_var, lbls, ...){
  
  object <- x 
  if(is.matrix(x)){
    # Check to make sure it's the right type of matrix...
    if(sum(colnames(x) != rownames(x)) > 0){
      stop("gg_interaction expects a rfsrc object, or a find.interaction object.")
    }
  }
  # Initialize variables for gather statement... to silence R CMD CHECK
  vars <- dpth <- NA
  
  if(!inherits(object, "gg_interaction")) 
    object <- gg_interaction(x, ...)
  
  if(sum(x_var %in% rownames(object)) == 0){
    stop(paste("Invalid x_var (",x_var, ") specified, covariate not found.", sep=""))
  }
  
  if(length(x_var) > 1){
    gg_dta <- data.frame(cbind(names=rownames(object),
                                   t(object[which(rownames(object) %in% x_var),])))
    #colnames(gg_dta) <- x_var
    gg_dta$rank <- 1:dim(gg_dta)[1]
    gg_dta <- gg_dta %>% 
      gather(vars, dpth, -rank, -names)
    
    gg_dta$dpth <- as.numeric(gg_dta$dpth)
    gg_dta$names <- factor(gg_dta$names,
                               levels=unique(gg_dta$names))
    gg_plt <- ggplot(gg_dta)+ 
      geom_point(aes_string(x="names", y="dpth", shape="vars"))+
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=90)) +
      labs(x="", y="Minimal Depth")
    
    if(!missing(lbls)){
      if(length(lbls) >= length(colnames(object))){
        st.lbls <- lbls[colnames(object)]
        names(st.lbls) <- colnames(object)
        st.lbls[which(is.na(st.lbls))] <- names(st.lbls[which(is.na(st.lbls))])
        
        gg_plt <- gg_plt +
          scale_x_discrete(labels=st.lbls)
      }
    }
    
    gg_plt + facet_wrap(~vars)
  }else{
    gg_dta <- data.frame(cbind(rank=1:dim(object)[1], 
                                   t(object[which(rownames(object) %in% x_var),])))
    colnames(gg_dta)[2] <- "dpth" 
    gg_dta$names <- rownames(gg_dta)
    
    gg_dta$dpth <- as.numeric(gg_dta$dpth)
    gg_dta$names <- factor(gg_dta$names,
                               levels=unique(gg_dta$names))
    gg_plt <- ggplot(gg_dta)+ 
      geom_point(aes_string(x="names", y="dpth"))+
      geom_point(aes_string(x="names", y="dpth"),
                 data=gg_dta[which(rownames(gg_dta)==x_var),],
                 shape=3, size=5,
                 color="red")+
      theme(text = element_text(size=10),
            axis.text.x = element_text(angle=90)) +
      labs(x="", y="Minimal Depth")
    
    
    if(!missing(lbls)){
      if(length(lbls) >= length(gg_dta$names)){
        st.lbls <- lbls[as.character(gg_dta$names)]
        names(st.lbls) <- as.character(gg_dta$names)
        st.lbls[which(is.na(st.lbls))] <- names(st.lbls[which(is.na(st.lbls))])
        
        gg_plt <- gg_plt +
          scale_x_discrete(labels=st.lbls)
      }
    }
    gg_plt
  }
}
