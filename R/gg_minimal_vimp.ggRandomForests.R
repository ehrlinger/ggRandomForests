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
#' Minimal depth vs VIMP camparison by variable ranks. 
#' 
#' @param object A randomForestSRC forest object, predict object or
#' the list from the var.select.rfsrc function.
#' @param event an optional vector of logical values (event indicator) for 
#' shaping the points in when plotting.
#' @param ... optional arguments passed to the var.select function 
#'  if operating on an rfsrc object. 
#'  
#'  @seealso \code{\link{plot.gg_minimal_vimp}} \code{var.select.rfsrc}
#'  
#'  @export gg_minimal_vimp gg_minimal_vimp.ggRandomForests
#'  @aliases gg_minimal_vimp
#'  
#' @examples
#' ## Examples from RFSRC package... 
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## You can build a randomForest
#' # iris_rf <- rfsrc(Species ~ ., data = iris)
#' # iris_vs <- var.select(iris_rf)
#' # ... or load a cached randomForestSRC object
#' data(iris_vs, package="ggRandomForests")
#' 
#' # Get a data.frame containing minimaldepth measures
#' ggrf.obj<- gg_minimal_vimp(iris_vs)
#' 
#' # Plot the gg_mkinimal_depth object
#' plot(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' # airq_rf <- rfsrc(Ozone ~ ., data = airquality, na.action = "na.impute")
#' # airq_vs <- var.select(airq_rf)
#' # ... or load a cached randomForestSRC object
#' data(airq_vs, package="ggRandomForests")
#' 
#' # Get a data.frame containing error rates
#' ggrf.obj<- gg_minimal_vimp(airq_vs)
#' 
#' # Plot the gg_error object
#' plot(ggrf.obj)
#' 
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' # data(veteran, package = "randomForestSRC")
#' # veteran_rf <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' # veteran_vs <- var.select(veteran_rf)
#' # Load a cached randomForestSRC object
#' data(veteran_vs, package="ggRandomForests")
#' 
#' ggrf.obj <- gg_minimal_vimp(veteran_vs)
#' plot(ggrf.obj)
#'   
gg_minimal_vimp.ggRandomForests <- function(object, event, ...){
  
  if (inherits(object, "rfsrc") == TRUE){
    vSel <- var.select(object, ...)
  }else if (!is.null(object$varselect)) {
    # Test for variable selection minimal depth object
    vSel <- object
  }else if(is.null(object$threshold)) {
    # Test for max.subtree minimal depth object, convert to vSel object
    
    stop("No support for max.subtree yet, use var.select instead")
  }else{
    stop("Function works only on rfsrc or var.select objects.")
  }
  
  rnk.md <- rnk.vm <- tbl_df(data.frame(cbind(names=rownames(vSel$varselect))))
  rnk.md$depth <- rnk.vm$vimp <- 1:dim(rnk.md)[1]
  
  # Rename the full vimp.all column to just "vimp"
  if(is.null(vSel$varselect$vimp))
    colnames(vSel$varselect)[which(colnames(vSel$varselect)=="vimp.all")] <- "vimp"
  
  rnk.vm <- rnk.vm[order(vSel$varselect$vimp, decreasing=TRUE),]
  rnk.vm$vimp <- 1:dim(rnk.vm)[1]

  # Default color is by negative/positive vimp
  rnk.vm$col <- c("-", "+")[as.numeric(vSel$varselect$vimp[order(vSel$varselect$vimp, 
                                                                 decreasing=TRUE)]>0)+1]
  
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