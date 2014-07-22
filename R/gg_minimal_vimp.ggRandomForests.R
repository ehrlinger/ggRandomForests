####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
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
#' gg_minimal_vimp Compare ranks by minimal depth and vimp.
#'
#' @param object A var.select randomForestSRC object
#' @param event an optional vector of events. For shaping the points in 
#' the figure.
#' @param ... optional arguments passed to the var.select function 
#'  of randomForestSRC
#'  
#'  @seealso \code{\link{plot.gg_minimal_vimp}} \code{var.select.rfsrc}
#'  
#'  @export gg_minimal_vimp gg_minimal_vimp.ggRandomForests
#'  @aliases gg_minimal_vimp
#'  
gg_minimal_vimp.ggRandomForests <- function(object, event, ...){
  
  rnk.md <- rnk.vm <- tbl_df(data.frame(cbind(names=rownames(object$varselect))))
  rnk.md$depth <- rnk.vm$vimp <- 1:dim(rnk.md)[1]
  
  rnk.vm <- rnk.vm[order(object$varselect$vimp, decreasing=TRUE),]
  rnk.vm$vimp <- 1:dim(rnk.vm)[1]

  # Default color is by negative/positive vimp
  rnk.vm$col <- c("-", "+")[as.numeric(object$varselect$vimp[order(object$varselect$vimp, decreasing=TRUE)]>0)+1]
  
  rnk <- merge(rnk.vm, rnk.md,by="names")

  rnk <- tbl_df(rnk)
  class(rnk) <- c("gg_minimal_vimp", class(rnk))
  invisible(rnk)
  
  # Can I shape it by the class of the variable?
#   cls<- tbl_df(cls=cbind(sapply(rf.surv$xvar, class)))
#   cls$names<- rownames(cls)
#   rnk <- merge(rnk, cls,by="names")
#   rnk<- rnk[order(rnk$depth),]
#   ggplot(rnk[1:vSel$modelsize, ], aes(x=names, y=vimp, col=col, shape=cls))+
#     labs(x="Minimal Depth (Rank Order)", y="VIMP Rank", color="VIMP")+
#     geom_abline(xintercept=0, slope=1, col="red", size=.5, linetype=2)+
#     geom_point(size=2)+

}
gg_minimal_vimp <- gg_minimal_vimp.ggRandomForests