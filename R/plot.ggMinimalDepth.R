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
#' plot.ggMinimalDepth
#' Plot a \link{\code{ggMinimalDepth}} object, the cumulative OOB error rates of the forest as a function of number of trees.
#' 
#' @param x ggMinimalDepth object created from a randomForestSRC object
#' 
#' @return ggplot object
#' 
#' @export plot.ggMinimalDepth
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
#' ggrf.obj<- ggMinimalDepth(iris.obj)
#' 
#' plot.ggMinimalDepth(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRCM")
#' v.obj <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#'
#' ggrf.obj <- ggMinimalDepth(v.obj)
#' plot(ggrf.obj)
#'
### error rate plot
plot.ggMinimalDepth <- function(object, selection=FALSE, list.vars = TRUE, type="rank",...){
  if(!inherits(object, "ggMinimalDepth")){
    object <-  ggMinimalDepth(object)
  }
  
  xl <-c(0,ceiling(max(object$varselect$depth))+1)
  sel.th = object$md.obj$threshold
  
  if(selection){
    modelSize<-object$modelsize
    
    # Labels for the top md vars.
    md.labs <- object$topvars
    
    ## Number the variables
    for(ind in 1:length(md.labs)){
      md.labs[ind] <- paste(ind, md.labs[ind], sep=". ")
    }
    vSel <- object$varselect[1:modelSize,]
    vSel$rank <- 1:dim(vSel)[1]
    gDta <- ggplot(vSel)
    gDta <- switch(type,
                   rank = gDta +
                     geom_point(aes(y=rank, x=depth, label=rank))+
                     coord_cartesian(x=xl) + 
                     geom_text(aes(y=rank, x=depth-.7, label=rank), size=3, hjust=0),
                   named  =gDta +
                     geom_point(aes(y=depth, x=names))+
                     coord_cartesian(y=xl)
    )
    
    if(list.vars){
      
      # We will need to modify this as we get more examples.
      x.pt <- ceiling(object$md.obj$threshold)
      
      # Populate the text box with min.depth ordered labels
      yPlace= object$modelsize -1
      
      for(ind in 1:length(md.labs)){
        gDta <- gDta +
          annotate(x=x.pt,y=yPlace,geom="text",label=md.labs[ind], hjust=0,size=3)
        yPlace <- yPlace - .95
      }
    }
    
  }else{ 
    vSel <- object$varselect
    vSel$rank <- 1:dim(vSel)[1]
    
    gDta <- ggplot(vSel)
    gDta <- switch(type,
                   rank = gDta +
                     geom_point(aes(y=rank, x=depth))+
                     coord_cartesian(x=xl),
                   named  =gDta +
                     geom_point(aes(y=depth, x=names))+
                     coord_cartesian(y=xl)
    )
    
  }
  
  if(type=="named"){
    gDta <- gDta+
      geom_hline(yintercept=sel.th, lty=2)+
      labs(y="Minimal Depth of a Variable", x="")+
      coord_flip()
  }else{
    gDta <- gDta+
      labs(y="Rank", x="Minimal Depth of a Variable")+
      geom_vline(xintercept=sel.th, lty=2)
  }
  return(gDta)
}
