####**********************************************************************
####**********************************************************************
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
#' Minimal Depth Variable Interaction data object
#' (\code{\link[randomForestSRC]{find.interaction}}).
#'
#' Converts the matrix returned from
#' \code{\link[randomForestSRC]{find.interaction}} to a \code{data.frame}
#' and add attributes for S3 identification. If passed  a
#' \code{\link[randomForestSRC]{rfsrc}} object, \code{gg_interaction}
#' first runs the \code{\link[randomForestSRC]{find.interaction}}
#' function with all optional arguments.
#'
#' @param object a \code{\link[randomForestSRC]{rfsrc}} object or the
#' output from the \code{\link[randomForestSRC]{find.interaction}}
#' function call.
#' @param ... optional extra arguments passed to
#' \code{\link[randomForestSRC]{find.interaction}}.
#'
#' @return \code{gg_interaction} object
#'
#' @seealso \code{\link[randomForestSRC]{rfsrc}}
#' \code{\link[randomForestSRC]{find.interaction}}
#' \code{\link[randomForestSRC]{max.subtree}}
#' \code{\link[randomForestSRC]{vimp}}
#' \code{\link{plot.gg_interaction}}
#'
#' @aliases gg_interaction
#'
#' @importFrom randomForestSRC find.interaction
#'
#' @references
#' Ishwaran H. (2007). Variable importance in binary regression trees and
#' forests, Electronic J. Statist., 1:519-537.
#'
#' Ishwaran H., Kogalur U.B., Gorodeski E.Z, Minn A.J. and Lauer M.S. (2010).
#' High-dimensional variable selection for survival data. J. Amer. Statist.
#' Assoc., 105:205-217.
#'
#' Ishwaran H., Kogalur U.B., Chen X. and Minn A.J. (2011). Random survival
#' forests for high-dimensional data. Statist. Anal. Data Mining, 4:115-132.
#'
#' @examples
#' ## Examples from randomForestSRC package...
#' ## ------------------------------------------------------------
#' ## find interactions, classification setting
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' iris.obj <- rfsrc(Species ~ ., data = iris)
#' ## TODO: VIMP interactions not handled yet....
#' ## randomForestSRC::find.interaction(iris.obj, method = "vimp", nrep = 3)
#'
#' interaction_iris <- randomForestSRC::find.interaction(iris.obj)
#' gg_dta <- gg_interaction(interaction_iris)
#'
#' plot(gg_dta, xvar = "Petal.Width")
#' plot(gg_dta, panel = TRUE)
#'
#' ## ------------------------------------------------------------
#' ## find interactions, regression setting
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#' ##
#' ## TODO: VIMP interactions not handled yet....
#' ## randomForestSRC::find.interaction(airq.obj, method = "vimp", nrep = 3)
#' interaction_airq <- randomForestSRC::find.interaction(airq.obj)
#'
#' gg_dta <- gg_interaction(interaction_airq)
#'
#' plot(gg_dta, xvar = "Temp")
#' plot(gg_dta, xvar = "Solar.R")
#'
#' plot(gg_dta, panel = TRUE)
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
#' interaction_boston <- find.interaction(rfsrc_boston)
#'
#' gg_dta <- gg_interaction(interaction_boston)
#'
#' plot(gg_dta, panel = TRUE)
#'
#' ## -------- mtcars data
#' rfsrc_mtcars <- rfsrc(mpg ~ ., data = mtcars)
#'
#' interaction_mtcars <- find.interaction(rfsrc_mtcars)
#'
#' gg_dta <- gg_interaction(interaction_mtcars)
#'
#' plot(gg_dta, panel = TRUE)
#'
#' ## ------------------------------------------------------------
#' ## find interactions, survival setting
#' ## ------------- veteran data
#' ## randomized trial of two treatment regimens for lung cancer
#' data(veteran, package = "randomForestSRC")
#' rfsrc_veteran <- rfsrc(Surv(time, status) ~ ., data = veteran)
#'
#' interaction_vet <- find.interaction(rfsrc_veteran)
#'
#' gg_dta <- gg_interaction(interaction_vet)
#'
#' plot(gg_dta, panel = True)
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
#' interaction_pbc <- find.interaction(rfsrc_pbc, nvar = 9)
#' gg_dta <- gg_interaction(interaction_pbc)
#'
#' plot(gg_dta, xvar = "bili")
#' plot(gg_dta, panel = TRUE)
#'
#' @aliases gg_interaction gg_interaction.randomForest gg_interaction.rfsrc
#'
#' @export
gg_interaction <- function(object, ...) {
  UseMethod("gg_interaction", object)
}
#' @export
gg_interaction.rfsrc <- function(object, ...) {
  if (inherits(object, "rfsrc")) {
    # If we called this with a rfsrc object, we need to run find.interaction.
    warning(
      paste(
        "Forest object means we assume max.subtree",
        "method for finding interactions.",
        "\nThis may take some time."
      )
    )

    object <- randomForestSRC::find.interaction(object, ...)
  }
  if (!inherits(object, "matrix")) {
    stop("gg_interaction expects a rfsrc or find.interaction object.")
  }

  # make the matrix a data.frame
  gg_dta <- data.frame(object)

  # Check to make sure it's the right type of structre...
  if (nrow(gg_dta) != ncol(gg_dta)) {
    stop("gg_interaction expects a find.interaction object.")
  }
  class(gg_dta) <- c("gg_interaction", class(gg_dta))

  invisible(gg_dta)
}
#
#' @export
gg_interaction.randomForest <- function(object, ...) {
  stop("gg_interaction is not yet support for randomForest objects")
}

#' @export
gg_interaction.default <- gg_interaction.rfsrc
