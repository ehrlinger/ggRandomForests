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
#'
#' ggVimp Extracts the variable importance (VIMP) information from a
#' a randomForestSRC object.
#' 
#' @param object A rfsrc object
#' @param ... arguments passed to the vimp.rfsrc function
#' 
#' @return a matrix of VIMP measures, in rank order.
#' 
#' @seealso \code{\link{plot.ggVimp}} \code{rfsrc} \code{vimp.rfsrc}
#' 
#' @references 
#' Ishwaran H. (2007). Variable importance in binary regression trees and forests, 
#' \emph{Electronic J. Statist.}, 1:519-537.
#' 
#' 
#' @examples
#' # notrun
#' ## ------------------------------------------------------------
#' ## classification example
#' ## showcase different vimp
#' ## ------------------------------------------------------------
#' 
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' 
#' # Breiman-Cutler permutation vimp
#' vimp(iris.obj)$importance
#' 
#' # Breiman-Cutler random daughter vimp
#' vimp(iris.obj, importance = "random")$importance
#' 
#' # Breiman-Cutler joint permutation vimp 
#' vimp(iris.obj, joint = TRUE)$importance
#' 
#' # Breiman-Cuter paired vimp
#' vimp(iris.obj, c("Petal.Length", "Petal.Width"), joint = TRUE)$importance
#' vimp(iris.obj, c("Sepal.Length", "Petal.Width"), joint = TRUE)$importance
#' 
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## compare Breiman-Cutler vimp to ensemble based vimp
#' ## ------------------------------------------------------------
#' 
#' airq.obj <- rfsrc(Ozone ~ ., airquality)
#' vimp.all <- cbind(
#'   ensemble = vimp(airq.obj, importance = "permute.ensemble")$importance,
#'   breimanCutler = vimp(airq.obj, importance = "permute")$importance)
#' print(vimp.all)
#' 
#' 
#' ## ------------------------------------------------------------
#' ## regression example
#' ## calculate VIMP on test data
#' ## ------------------------------------------------------------
#' 
#' set.seed(100080)
#' train <- sample(1:nrow(airquality), size = 80)
#' airq.obj <- rfsrc(Ozone~., airquality[train, ])
#' 
#' #training data vimp
#' airq.obj$importance
#' vimp(airq.obj)$importance
#' 
#' #test data vimp
#' vimp(airq.obj, newdata = airquality[-train, ])$importance
#' 
#' ## ------------------------------------------------------------
#' ## survival example
#' ## study how vimp depends on tree imputation
#' ## makes use of the subset option
#' ## ------------------------------------------------------------
#' 
#' data(pbc, package = "randomForestSRC")
#' 
#' # determine which records have missing values
#' which.na <- apply(pbc, 1, function(x){any(is.na(x))})
#' 
#' # impute the data using na.action = "na.impute"
#' pbc.obj <- rfsrc(Surv(days,status) ~ ., pbc, nsplit = 3,
#' na.action = "na.impute", nimpute = 1)
#' 
#' # compare vimp based on records with no missing values
#' # to those that have missing values
#' # note the option na.action="na.impute" in the vimp() call
#' vimp.not.na <- vimp(pbc.obj, subset = !which.na, na.action = "na.impute")$importance
#' vimp.na <- vimp(pbc.obj, subset = which.na, na.action = "na.impute")$importance
#' data.frame(vimp.not.na, vimp.na)
#' 
#' 
#' @export ggVimp.ggRandomForests
#' @export ggVimp
#' @aliases ggVimp
#' @importFrom dplyr tbl_df
#' 

ggVimp.ggRandomForests <- function(object, ...){
  
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
 
  ### set importance to NA if it is NULL
  if (is.null(object$importance)){
    warning("rfsrc object does not contain VIMP information. Calculating...")
    imp <- data.frame(sort(vimp(object, ...)$importance, decreasing=TRUE))
  }else{
    imp<-  data.frame(sort(object$importance, decreasing=TRUE))
  }
  
  imp<- cbind(imp, imp/imp[1,1])
  colnames(imp) <- c("VIMP", "relVIMP")
  imp$names <- rownames(imp)
  imp$names[which(is.na(imp$names))] <- rownames(imp)[which(is.na(imp$names))]
  
  imp$names <- factor(imp$names, levels=rev(imp$names))
  imp$positive <- TRUE
  imp$positive[which(imp$VIMP <=0)] <- FALSE
#   
#     if(missing(xvar.names)){
#       rfvimp <- as.data.frame(cbind(rfvimp[order(rfvimp, decreasing=TRUE)][1:n.var]))
#     }else{
#       rfvimp <- rfvimp[which(names(rfvimp) %in% var.names)]
#     }
  imp <- tbl_df(imp)
  class(imp) <- c("ggVimp", class(imp))
  invisible(imp)
}

ggVimp <-ggVimp.ggRandomForests
