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
#' Minimal depth data object (\code{[randomForestSRC]{var.select}})
#'
#' @param object A \code{[randomForestSRC]{rfsrc}} object,
#' \code{[randomForestSRC]{predict}} object or the list from the
#' \code{[randomForestSRC]{var.select.rfsrc}} function.
#' @param ... optional arguments passed to the
#' \code{[randomForestSRC]{var.select}} function if operating on an
#' \code{[randomForestSRC]{rfsrc}} object.
#'
#' @description the \code{[randomForestSRC]{var.select}} function implements
#' random forest variable selection using tree minimal depth methodology. The
#' \code{gg_minimal_depth} function takes the output from
#' \code{[randomForestSRC]{var.select}} and creates a \code{data.frame}
#' formatted for the \code{\link{plot.gg_minimal_depth}} function.
#'
#' @return \code{gg_minimal_depth} object, A modified list of variables from
#' the \code{[randomForestSRC]{var.select}} function, ordered by minimal
#' depth rank.
#'
#' @aliases gg_minimal_depth gg_minimal_depth.rfsrc
#'
#' @seealso \code{[randomForestSRC]{var.select}}
#' @seealso \code{\link{plot.gg_minimal_depth}}
#'
#' @importFrom randomForestSRC var.select
#'
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
#' gg_dta <- gg_minimal_depth(varsel_iris)
#'
#' # Plot the gg_minimal_depth object
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## Regression example
#' ## ------------------------------------------------------------
#' 
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ .,
#'   data = airquality,
#'   na.action = "na.impute"
#' )
#' varsel_airq <- var.select(rfsrc_airq)
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_minimal_depth(varsel_airq)
#'
#' # Plot the gg_minimal_depth object
#' plot(gg_dta)
#' 
#'
#' ## -------- Boston data
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
#' varsel_boston <- var.select(rfsrc_boston)
#'
#' # Get a data.frame containing error rates
#' gg_dta <- gg_minimal_depth(varsel_boston)
#' print(gg_dta)
#' plot(gg_dta)
#' 
#' ## -------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#' varsel_mtcars <- var.select(rfsrc_mtcars)
#'
#' # Get a data.frame containing error rates
#' plot.gg_minimal_depth(varsel_mtcars)
#' 
#'
#' ## ------------------------------------------------------------
#' ## Survival example
#' ## ------------------------------------------------------------
#' 
#' ## -------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran, ntree = 100)
#' varsel_veteran <- randomForestSRC::var.select(rfsrc_veteran)
#'
#' gg_dta <- gg_minimal_depth(varsel_veteran)
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## -------- pbc data
#' # We need to create this dataset
#' data(pbc, package = "randomForestSRC", )
#' # For whatever reason, the age variable is in days... makes no sense to me
#' for (ind in seq_len(dim(pbc)[2])) {
#'   if (!is.factor(pbc[, ind])) {
#'     if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'       if (sum(range(pbc[, ind], na.rm = TRUE) == c(0, 1)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'     }
#'   } else {
#'     if (length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 2) {
#'       if (sum(sort(unique(pbc[, ind])) == c(0, 1)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'       if (sum(sort(unique(pbc[, ind])) == c(FALSE, TRUE)) == 2) {
#'         pbc[, ind] <- as.logical(pbc[, ind])
#'       }
#'     }
#'   }
#'   if (!is.logical(pbc[, ind]) &
#'     length(unique(pbc[which(!is.na(pbc[, ind])), ind])) <= 5) {
#'     pbc[, ind] <- factor(pbc[, ind])
#'   }
#' }
#' # Convert age to years
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
#' # ========
#' # build the forest:
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(years, status) ~ .,
#'   dta_train,
#'   nsplit = 10,
#'   na.action = "na.impute",
#'   forest = TRUE,
#'   importance = TRUE,
#'   save.memory = TRUE
#' )
#'
#' varsel_pbc <- var.select(rfsrc_pbc)
#'
#' gg_dta <- gg_minimal_depth(varsel_pbc)
#' plot(gg_dta)
#' 
#' @aliases gg_minimal_depth  gg_minimal_depth.randomForest
#' @aliases gg_minimal_depth.rfsrc
#'
#' @export
gg_minimal_depth <- function(object, ...) {
  UseMethod("gg_minimal_depth", object)
}

#' @export
gg_minimal_depth.randomForest <- function(object, ...) {
  stop("gg_minimal_depth is not yet support for randomForest objects")
}

#' @export
gg_minimal_depth.rfsrc <- function(object, ...) {
  if (inherits(object, "rfsrc") == TRUE) {
    vsel <- randomForestSRC::var.select(object, ...)
  } else if (!is.null(object$varselect)) {
    # Test for variable selection minimal depth object
    vsel <- object
  } else if (is.null(object$threshold)) {
    # Test for max.subtree minimal depth object, convert to vsel object
    stop("No support for max.subtree yet, use var.select instead")
  } else {
    stop("Function works only on rfsrc or var.select objects.")
  }

  # There seems to be a bug in the randomForestSRC::var.select
  # function that does not calculate the threshold correctly.
  vsel$varselect$names <- rownames(vsel$varselect)

  vsel$varselect$names <- factor(vsel$varselect$names,
    levels = unique(vsel$varselect$names)
  )

  class(vsel) <- c("gg_minimal_depth", class(vsel))
  invisible(vsel)
}


#' @export
gg_minimal_depth.default <- gg_minimal_depth.rfsrc
