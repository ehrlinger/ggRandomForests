####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Minimal depth vs VIMP comparison by variable rankings.
#'
#' @param object A \code{\link[randomForestSRC]{rfsrc}} object,
#' \code{\link[randomForestSRC]{predict.rfsrc}} object or the list from the 
#' \code{\link[randomForestSRC]{var.select.rfsrc}} function.
#' 
#' @param ... optional arguments passed to the 
#' \code{\link[randomForestSRC]{var.select}} function if operating on an 
#' \code{\link[randomForestSRC]{rfsrc}} object.
#'
#' @return \code{gg_minimal_vimp} comparison object.
#'
#' @seealso \code{\link{plot.gg_minimal_vimp}} 
#' \code{\link[randomForestSRC]{var.select}}
#'
#' @aliases gg_minimal_vimp
#'
#' @importFrom randomForestSRC var.select
#' @examples
#' ## Examples from RFSRC package...
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' ## You can build a randomForest
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' varsel_iris <- randomForestSRC::var.select(rfsrc_iris)
#' 
#' # Get a data.frame containing minimaldepth measures
#' gg_dta<- gg_minimal_vimp(varsel_iris)
#'
#' # Plot the gg_minimal_depth object
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ ., data = airquality, 
#'                     na.action = "na.impute")
#' varsel_airq <- randomForestSRC::var.select(rfsrc_airq)
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_vimp(varsel_airq)
#'
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' }
#' \dontrun{
#' ## -------- Boston data
#' data(Boston, package = "MASS")
#' Boston$chas <- as.logical(Boston$chas)
#' rfsrc_boston <- rfsrc(medv ~ .,
#'    data = Boston,
#'    forest = TRUE,
#'    importance = TRUE,
#'    tree.err = TRUE,
#'    save.memory = TRUE)
#'    
#' varsel_boston <- var.select(rfsrc_boston)
#' 
#' # Get a data.frame containing error rates
#' gg_dta<- gg_minimal_vimp(varsel_boston)
#'
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' }
#' \dontrun{
#' ## -------- mtcars data
#'
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#' varsel_mtcars <- var.select(rfsrc_mtcars)
#' 
#' # Get a data.frame containing error rates
#' gg_dta <- gg_minimal_vimp(varsel_mtcars)
#'
#' # Plot the gg_minimal_vimp object
#' plot(gg_dta)
#' }
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' \dontrun{
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, 
#'                         ntree = 100)
#' varsel_veteran <- randomForestSRC::var.select(rfsrc_veteran)
#' 
#' gg_dta <- gg_minimal_vimp(varsel_veteran)
#' plot(gg_dta)
#'
#'
#' ## ------------------------------------------------------------
#' ## -------- pbc data
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC") 
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
#'  forest = TRUE,
#'  importance = TRUE,
#'  save.memory = TRUE
#' )
#' 
#' varsel_pbc <- var.select(rfsrc_pbc)
#'
#' 
#' gg_dta <- gg_minimal_vimp(varsel_pbc)
#' plot(gg_dta)
#' }
#' 
#' @aliases gg_minimal_vimp gg_minimal_vimp.randomForest 
#' gg_minimal_vimp.rfsrc
#' @export
gg_minimal_vimp <- function(object, ...) {
  UseMethod("gg_minimal_vimp", object)
}

#' @export
gg_minimal_vimp.randomForest <- function(object, ...) {
  stop("gg_minimal_vimp is not yet support for randomForest objects")
}

#' @export
gg_minimal_vimp.rfsrc <- function(object, ...) {
  if (inherits(object, "rfsrc") == TRUE) {
    vsel <- randomForestSRC::var.select(object, ...)
  } else if (!is.null(object$varselect)) {
    # Test for variable selection minimal depth object
    vsel <- object
  } else {
    stop("Function works only on rfsrc or var.select objects.")
  }
  
  rnk_md <-
    rnk_vm <- data.frame(cbind(names = rownames(vsel$varselect)))
  rnk_md$depth <- rnk_vm$vimp <- seq_len(dim(rnk_md)[1])
  
  # Rename the full vimp.all column to just "vimp"
  if (is.null(vsel$varselect$vimp))
    colnames(vsel$varselect)[which(colnames(vsel$varselect) ==
                                     "vimp.all")] <-
    "vimp"
  
  rnk_vm <- rnk_vm[order(vsel$varselect$vimp, decreasing = TRUE), ]
  rnk_vm$vimp <- seq_len(dim(rnk_vm)[1])
  
  # Default color is by negative/positive vimp
  rnk_vm$col <-
    c("-", "+")[as.numeric(vsel$varselect$vimp[order(vsel$varselect$vimp,
                                                     decreasing = TRUE)] > 0)
                + 1]
  
  gg_dta <- merge(rnk_vm, rnk_md, by = "names")
  
  class(gg_dta) <- c("gg_minimal_vimp", class(gg_dta))
  
  # So we can put a horizontal line at the MD selection point.
  attr(gg_dta, "modelsize") <- vsel$modelsize
  
  invisible(gg_dta)
}

#' @export
gg_minimal_vimp.default <- gg_minimal_vimp.rfsrc
