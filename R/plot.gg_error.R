####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Plot a \code{\link{gg_error}} object
#'
#' A plot of the cumulative OOB error rates of the random forest as a
#' function of number of trees.
#'
#' @param x gg_error object created from a \code{\link[randomForestSRC]{rfsrc}}
#' object
#' @param ... extra arguments passed to \code{ggplot} functions
#'
#' @return \code{ggplot} object
#'
#' @details The gg_error plot is used to track the convergence of the
#' randomForest. This figure is a reproduction of the error plot
#' from the \code{\link[randomForestSRC]{plot.rfsrc}} function.
#'
#' @seealso \code{\link{gg_error}} \code{\link[randomForestSRC]{rfsrc}}
#'  \code{\link[randomForestSRC]{plot.rfsrc}}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews,
#' 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. (2013). Random Forests for Survival, Regression
#' and Classification (RF-SRC), R package version 1.4.
#'
#' @examples
#' ## Examples from RFSRC package...
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## ------------- iris data
#' ## You can build a randomForest
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris, tree.err = TRUE)
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_error(rfsrc_iris)
#'
#' # Plot the gg_error object
#' plot(gg_dta)
#'
#' ## RandomForest example
#' rf_iris <- randomForest::randomForest(Species ~ .,
#'   data = iris,
#'   tree.err = TRUE,
#' )
#' gg_dta <- gg_error(rf_iris)
#' plot(gg_dta)
#'
#' gg_dta <- gg_error(rf_iris, training = TRUE)
#' plot(gg_dta)
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' ## ------------- airq data
#' rfsrc_airq <- rfsrc(Ozone ~ .,
#'   data = airquality,
#'   na.action = "na.impute", tree.err = TRUE,
#' )
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_error(rfsrc_airq)
#'
#' # Plot the gg_error object
#' plot(gg_dta)
#'
#'
#' ## ------------- Boston data
#' data(Boston, package = "MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' rfsrc_boston <- rfsrc(medv ~ .,
#'   data = Boston,
#'   forest = TRUE,
#'   importance = TRUE,
#'   tree.err = TRUE,
#'   save.memory = TRUE
#' )
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_error(rfsrc_boston)
#'
#' # Plot the gg_error object
#' plot(gg_dta)
#'
#' ## ------------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars, tree.err = TRUE)
#'
#' # Get a data.frame containing error rates
#' gg_dta<- gg_error(rfsrc_mtcars)
#'
#' # Plot the gg_error object
#' plot(gg_dta)
#'
#'
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' ## ------------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran,
#'                        tree.err = TRUE)
#'
#' gg_dta <- gg_error(rfsrc_veteran)
#' plot(gg_dta)
#'
#' ## ------------- pbc data
#' # Load a cached randomForestSRC object
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC",)
#' # For whatever reason, the age variable is in days... makes no sense to me
#' for (ind in seq_len(dim(pbc)[2])) {
#'  if (!is.factor(pbc[, ind])) {
#'    if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'      if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'    }
#'  } else {
#'    if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'      if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'      if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
#'        pbc[, ind] <- as.logical(pbc[, ind])
#'      }
#'    }
#'  }
#'  if (!is.logical(pbc[, ind]) &
#'      length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
#'    pbc[, ind] <- factor(pbc[, ind])
#'  }
#' }
#' #Convert age to years
#' pbc$age <- pbc$age / 364.24
#'
#' pbc$years <- pbc$days / 364.24
#' pbc <- pbc[, -which(colnames(pbc) == "days")]
#' pbc$treatment <- as.numeric(pbc$treatment)
#' pbc$treatment[which(pbc$treatment == 1)] <- "DPCA"
#' pbc$treatment[which(pbc$treatment == 2)] <- "placebo"
#' pbc$treatment <- factor(pbc$treatment)
#' dta_train <- pbc[-which(is.na(pbc$treatment)), ]
#' # Create a test set from the remaining patients
#' pbc_test <- pbc[which(is.na(pbc$treatment)), ]
#'
#' #========
#' # build the forest:
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(years, status) ~ .,
#'  dta_train,
#'  nsplit = 10,
#'  na.action = "na.impute",
#'  tree.err = TRUE,
#'  forest = TRUE,
#'  importance = TRUE,
#'  save.memory = TRUE
#' )
#'
#'
#' gg_dta <- gg_error(rfsrc_pbc)
#' plot(gg_dta)
#'
#' @importFrom ggplot2 ggplot geom_line theme labs
#' @importFrom tidyr gather
#' @export
plot.gg_error <- function(x, ...) {
  gg_dta <- x
  
  if (inherits(gg_dta, "rfsrc")) {
    gg_dta <- gg_error(gg_dta)
  }
  
  if (!inherits(gg_dta, "gg_error")) {
    stop("Incorrect object type: Expects a gg_error object")
  }
  point = FALSE
  if (nrow(na.omit(gg_dta)) < 2) { 
    point=TRUE
  }
  if (ncol(gg_dta) > 2) {
    gg_dta <- tidyr::gather(gg_dta, "variable", "value", -"ntree")
    gg_plt <-
      ggplot2::ggplot(na.omit(gg_dta),
                      ggplot2::aes(x = .data[["ntree"]], y = .data[["value"]], 
                                   col = .data[["variable"]]))
  } else {
    # We expect the object to have the following columns
    gg_plt <-
      ggplot2::ggplot(na.omit(gg_dta), ggplot2::aes(x = .data[["ntree"]], 
                                                    y = .data[["error"]]))
  }
  if (point) {
    gg_plt <- gg_plt +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Number of Trees", y = "OOB Error Rate", color = "Outcome")
  } else{
    gg_plt <- gg_plt +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Number of Trees", y = "OOB Error Rate", color = "Outcome")
  }
  if (length(unique(gg_dta$variable)) == 1) {
    gg_plt <- gg_plt + ggplot2::theme(legend.position = "none")
  }
  return(gg_plt)
}
