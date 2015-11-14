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
#' Variable Importance (VIMP) data object
#'
#' \code{gg_vimp} Extracts the variable importance (VIMP) information from a
#' a \code{\link[randomForestSRC]{rfsrc}} object.
#' 
#' @param object A \code{\link[randomForestSRC]{rfsrc}} object or output from 
#' \code{\link[randomForestSRC]{vimp}}
#' @param n_var select a number pf the highest VIMP variables to plot
#' @param ... arguments passed to the \code{\link[randomForestSRC]{vimp.rfsrc}} function if the 
#' \code{\link[randomForestSRC]{rfsrc}} object does not contain importance information.
#' 
#' @return \code{gg_vimp} object. A \code{data.frame} of VIMP measures, in rank order.
#' 
#' @seealso \code{\link{plot.gg_vimp}} \code{\link[randomForestSRC]{rfsrc}} \code{\link[randomForestSRC]{vimp}}
#' 
#' @references 
#' Ishwaran H. (2007). Variable importance in binary regression trees and forests, 
#' \emph{Electronic J. Statist.}, 1:519-537.
#' 
#' @importFrom tidyr gather_
#' @importFrom randomForestSRC vimp
#' 
#' @examples
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
#' \dontrun{
#' ## -------- air quality data 
#' # rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
#' data(rfsrc_airq, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_airq)
#' plot(gg_dta)
#' }
#' 
#' ## -------- Boston data
#' data(rfsrc_Boston, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_Boston)
#' plot(gg_dta)
#' 
#' \dontrun{
#' ## -------- mtcars data
#' data(rfsrc_mtcars, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_mtcars)
#' plot(gg_dta)
#' }
#' ## ------------------------------------------------------------
#' ## survival example
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- veteran data
#' data(rfsrc_veteran, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_veteran)
#' plot(gg_dta)
#' }
#' 
#' ## -------- pbc data
#' data(rfsrc_pbc, package="ggRandomForests")
#' gg_dta <- gg_vimp(rfsrc_pbc)
#' plot(gg_dta)
#' 
#' # Restrict to only the top 10.
#' gg_dta <- gg_vimp(rfsrc_pbc, n_var=10)
#' plot(gg_dta)

#' @aliases gg_vimp gg_vimp.rfsrc 

#' @export
gg_vimp.rfsrc <- function(object, n_var, ...){
  
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
  
  ### set importance to NA if it is NULL
  if (is.null(object$importance)){
    warning("rfsrc object does not contain VIMP information. Calculating...")
    gg_dta <- data.frame(sort(randomForestSRC::vimp(object)$importance, 
                              decreasing=TRUE))
  }else{
    gg_dta <- data.frame(object$importance)
   
  }
  if(ncol(gg_dta) == 1){
    colnames(gg_dta) <- "VIMP"
    gg_dta$vars <- rownames(gg_dta)
    gg_dta <- gg_dta[order(gg_dta$VIMP, decreasing=TRUE),]
  }
  if(missing(n_var)) n_var <- nrow(gg_dta)
  if(n_var > nrow(gg_dta)) n_var <- nrow(gg_dta)
  
  
  # Handle multiclass importance
  if(ncol(gg_dta) > 1){
    # Classification...
    arg_set <- list(...)
    
    if(!is.null(arg_set$which.outcome)){
      # test which.outcome specification
      if(!is.numeric(arg_set$which.outcome)){
        if(arg_set$which.outcome %in% colnames(gg_dta)){
          gg_v <- data.frame(vimp=sort(gg_dta[,arg_set$which.outcome], 
                                       decreasing=TRUE))
          gg_v$vars <- rownames(gg_dta)[order(gg_dta[,arg_set$which.outcome], 
                                              decreasing=TRUE)]
        }else{
          stop(paste("which.outcome naming is incorrect.", 
                     arg_set$which.outcome, 
                     "\nis not in", colnames(gg_dta)))
        }
      }else{
        if(arg_set$which.outcome < ncol(gg_dta)){
          gg_v <- data.frame(vimp=sort(gg_dta[,arg_set$which.outcome + 1], 
                                       decreasing=TRUE))
          gg_v$vars <- rownames(gg_dta)[order(gg_dta[,arg_set$which.outcome + 1], 
                                              decreasing=TRUE)]
        }else{
          stop(paste("which.outcome specified larger than the number of classes (+1).", 
                     arg_set$which.outcome, 
                     " >= ", ncol(gg_dta)))
        }
      }
      gg_dta <- gg_v
    }else{
      gg_dta$vars <- rownames(gg_dta)
    }
    #clnms <- colnames(gg_dta)[-which(colnames(gg_dta)=="vars")]
    gg_dta <- gg_dta[1:n_var,]
    gathercols <- colnames(gg_dta)[-which(colnames(gg_dta) == "vars")]
    gg_dta <- gather_(gg_dta, "set", "vimp", gathercols)
    gg_dta <- gg_dta[order(gg_dta$vimp, decreasing=TRUE),]
    gg_dta$vars <- factor(gg_dta$vars)
  }else{
    cnms <- colnames(gg_dta)
    gg_dta <- cbind(gg_dta, gg_dta / gg_dta[1,1])
    colnames(gg_dta) <- c(cnms, "rel_vimp")
    gg_dta$vars[which(is.na(gg_dta$vars))] <- 
      rownames(gg_dta)[which(is.na(gg_dta$vars))]
    
    gg_dta <- gg_dta[1:n_var,]
    
  }
  gg_dta$vars <- factor(gg_dta$vars, levels=rev(unique(gg_dta$vars)))
  gg_dta$positive <- TRUE
  gg_dta$positive[which(gg_dta$vimp <= 0)] <- FALSE
  
  class(gg_dta) <- c("gg_vimp", class(gg_dta))
  invisible(gg_dta)
}
#' @export
# gg_vimp <- function (object, ...) {
#   UseMethod("gg_vimp", object)
# }
gg_vimp <- gg_vimp.rfsrc 
