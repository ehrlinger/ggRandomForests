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
#' Plot a \code{\link{gg_minimal_depth}} object for random forest variable ranking.
#' 
#' @param x \code{\link{gg_minimal_depth}} object created from a 
#' \code{\link[randomForestSRC]{rfsrc}} object
#' @param selection should we restrict the plot to only include variables selected by the 
#' minimal depth criteria (boolean).
#' @param type select type of y axis labels c("named","rank")
#' @param lbls a vector of alternative variable names.
#' @param ... optional arguments passed to \code{\link{gg_minimal_depth}}
#' 
#' @return \code{ggplot} object
#' 
#' @seealso \code{\link[randomForestSRC]{var.select}} \code{\link{gg_minimal_depth}}
#' 
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#' 
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, 
#' Rnews, 7(2):25-31.
#' 
#' Ishwaran H. and Kogalur U.B. (2014). Random Forests for Survival, 
#' Regression and Classification (RF-SRC), R package version 1.5.
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
#' gg_dta<- gg_minimal_depth(varsel_iris)
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
#' gg_dta<- gg_minimal_depth(varsel_airq)
#' 
#' # Plot the gg_minimal_depth object
#' plot(gg_dta)
#' 
#' ## -------- Boston data
#' data(varsel_Boston, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' plot(gg_minimal_depth(varsel_Boston))
#' 
#' ## -------- mtcars data
#' data(varsel_mtcars, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' plot.gg_minimal_depth(varsel_mtcars)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## -------- veteran data
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' # varsel_veteran <- var.select(rfsrc_veteran)
#' # Load a cached randomForestSRC object
#' data(varsel_veteran, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_depth(varsel_veteran)
#' plot(gg_dta)
#' 
#' ## -------- pbc data
#' data(varsel_pbc, package="ggRandomForests")
#' 
#' gg_dta <- gg_minimal_depth(varsel_pbc)
#' plot(gg_dta)
#' 
#' }
#' 
#' @importFrom ggplot2 ggplot geom_line theme aes_string labs coord_cartesian geom_text annotate geom_hline coord_flip geom_vline scale_x_discrete
#' @export
plot.gg_minimal_depth <- function(x, selection=FALSE, 
                                  type=c("named","rank"),
                                  lbls,
                                  ...){
  gg_dta <- x
  if(!inherits(x, "gg_minimal_depth")){
    gg_dta <- gg_minimal_depth(x, ...)
  }
  type <- match.arg(type)
  arg_set <- as.list(substitute(list(...)))[-1L]
  
  nvar <- nrow(gg_dta$varselect)
  if(!is.null(arg_set$nvar)){
    if(is.numeric(arg_set$nvar) & arg_set$nvar > 1){
      nvar <- arg_set$nvar
      if(nvar < nrow(gg_dta$varselect))
        gg_dta$varselect <- gg_dta$varselect[1:nvar,]
    }
  }  
  
  xl <- c(0,ceiling(max(gg_dta$varselect$depth)) + 1)
  sel.th <- gg_dta$md.obj$threshold
  
  if(selection){
    modelsize <- gg_dta$modelsize
    
    # Labels for the top md vars.
    md.labs <- gg_dta$topvars
    
    ## Number the variables
    for(ind in 1:length(md.labs)){
      md.labs[ind] <- paste(ind, md.labs[ind], sep = ". ")
    }
    vsel <- gg_dta$varselect[1:modelsize,]
    vsel$rank <- 1:nrow(vsel)
    
    ## Reorder the minimal depth to place most "important" at top of figure
    vsel$names <- factor(vsel$names, 
                         levels=rev(levels(vsel$names )))
    gg_plt <- ggplot(vsel)
    gg_plt <- switch(type,
                     rank = gg_plt +
                       geom_point(aes_string(y="rank", x="depth", label="rank")) +
                       coord_cartesian(xlim=xl) + 
                       geom_text(aes_string(y="rank", x="depth" - .7, label="rank"), 
                                 size=3, hjust=0),
                     named = gg_plt +
                       geom_point(aes_string(y="depth", x="names")) +
                       coord_cartesian(ylim=xl)
    )
    
    
  } else { 
    vsel <- gg_dta$varselect
    vsel$rank <- 1:dim(vsel)[1]
    vsel$names <- factor(vsel$names, 
                         levels=rev(levels(vsel$names )))
    gg_plt <- ggplot(vsel)
    gg_plt <- switch(type,
                     rank = gg_plt +
                       geom_point(aes_string(y="rank", x="depth")) +
                       coord_cartesian(xlim=xl),
                     named = gg_plt +
                       geom_point(aes_string(y="depth", x="names")) +
                       coord_cartesian(ylim=xl)
    )}
  
  
  if(type == "named"){
    if(!missing(lbls)){
      if(length(lbls) >= length(vsel$names)){
        st.lbls <- lbls[as.character(vsel$names)]
        names(st.lbls) <- as.character(vsel$names)
        st.lbls[which(is.na(st.lbls))] <- names(st.lbls[which(is.na(st.lbls))])
        
        gg_plt <- gg_plt +
          scale_x_discrete(labels=st.lbls)
      }
    }
    
    gg_plt <- gg_plt +
      labs(y="Minimal Depth of a Variable", x="") 
    
    if(nvar > gg_dta$modelsize){
      gg_plt <- gg_plt +
        geom_hline(yintercept=sel.th, lty=2) 
      
    }
    gg_plt <- gg_plt +
      labs(y="Minimal Depth of a Variable", x="") +
      coord_flip() 
  }else{
    gg_plt <- gg_plt +
      labs(y="Rank", x="Minimal Depth of a Variable") 
    
    if(nvar > gg_dta$modelsize){
      gg_plt <- gg_plt +
        geom_vline(xintercept=sel.th, lty=2)
    }
  }
  return(gg_plt)
}
