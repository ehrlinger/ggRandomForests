#'
#' @title plot variable importance (VIMP) from a RF-SRC analysis
#' 
#' @description Generate a variable importance plot. Possibly for a subset of the variables used to grow the forest.
#' 
#' @param object randomForestSRC object
#' @param n.var number of variables to display. 
#' @param xvar.names Names of the x-variables to be used. If not specified all variables are used.
#' @param var.labels vector of labels for display instead of variable names
#' @param digits number of digits to display when printing VIMP values
#' @param sorted should the results be sorted in descending VIMP order
#' @param show should the graphic be displayed
#' 
#' @return a ggplot2 graphic of the VIMP plot.
#' 
#' @seealso \code{\link{vimp.rfsrc}}
#' 
#' @references Ishwaran H. (2007). Variable importance in binary regression trees and forests, \emph{Electronic J. Statist.}, 1:519-537.
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
#' @export plot.vimp.ggRandomForests
#' @export plot.vimp

plot.vimp.ggRandomForests <- function(object, n.var, xvar.names, var.labels=NULL, digits, sorted=TRUE, show=TRUE){
  
  if (sum(inherits(object, c("rfsrc", "grow"), TRUE) == c(1, 2)) != 2 &
        sum(inherits(object, c("rfsrc", "predict"), TRUE) == c(1, 2)) != 2) {
    stop("This function only works for objects of class `(rfsrc, grow)' or '(rfsrc, predict)'.")
  }
  if(missing(n.var)) n.var <- dim(object$xvar)[2]
  if(missing(digits)) digits<-4
  
  ### set importance to NA if it is NULL
  if (is.null(object$importance)){
    warning("rfsrc object does not contain VIMP information. Calculating...")
    rfvimp <-vimp(object)$importance
    if(missing(var.names)){
      rfvimp <- as.data.frame(cbind(rfvimp[order(rfvimp, decreasing=TRUE)][1:n.var]))
    }else{
      rfvimp <- rfvimp[which(names(rfvimp) %in% var.names)]
    }
  }else{
    if(missing(var.names)){
      rfvimp <- as.data.frame(cbind(object$importance[order(object$importance, decreasing=TRUE)][1:n.var]))
    }else{
      rfvimp <- object$importance[which(names(object$importance) %in% var.names)]
    }
  }
  rfvimp <- as.data.frame(cbind(rfvimp[order(rfvimp, decreasing=sorted)]))
  colnames(rfvimp) <-"VIMP"
  
  if(is.null(var.labels)){
    rfvimp$names <- rownames(rfvimp)
  }else{
    ## Manually enter the labels... I could get these from bdavr.labels, but then they would be really long.
    ##
    ## !!! This is a one-to-one list. If you rerun rfsrc ont his data, you could end up with a bad 
    ## variable label which may not be easily found.
    ##
    rfvimp$names <- var.labels
    n.var <- length(var.labels)
  }
  rfvimp$names <- factor(rfvimp$names, levels=rev(rfvimp$names))
  
  ## These plot functions will be packaged once we have them sort of finalized.
  vimp.plt<-ggplot(rfvimp)+
    geom_bar(aes(y=VIMP, x=names), stat="identity", width=.5, fill="blue")+ 
    labs(x="variable", y="vimp") + 
    coord_flip()
  
  
  rfvimp.out <- cbind(rfvimp[,1], rfvimp[,1]/rfvimp[1,1])
  colnames(rfvimp.out) <- c("Importance","Relative IMP")
  rownames(rfvimp.out) <- rfvimp$names
  print(rfvimp.out, digits=digits)
  
  if(show) show(vimp.plt)
  
  invisible(vimp.plt)
}

plot.vimp <- plot.vimp.ggRandomForests
