##=============================================================================
#' Partial dependence data from an rfsrc model
#'
#' Computes partial dependence for one or more predictors by calling
#' \code{\link[randomForestSRC]{partial.rfsrc}} internally, then splits the
#' results into separate data frames for continuous and categorical variables.
#' Unlike \code{\link{gg_partial}}, no separate \code{plot.variable} call is
#' required — supply the fitted \code{rfsrc} object directly.
#'
#' @param rf_model A fitted \code{\link[randomForestSRC]{rfsrc}} object.
#' @param xvar.names Character vector of predictor names for which partial
#'   dependence should be computed. Must be a subset of \code{rf_model$xvar.names}.
#' @param xvar2.name Optional single character name of a grouping variable in
#'   \code{newx}. When supplied, partial dependence is computed separately for
#'   each unique level of this variable and a \code{grp} column is appended.
#' @param newx Optional \code{data.frame} of predictor values to evaluate
#'   partial effects at. Defaults to the training data stored in
#'   \code{rf_model$xvar}. All column names must match \code{rf_model$xvar.names}.
#' @param cat_limit Variables with fewer than \code{cat_limit} unique values in
#'   \code{newx} are treated as categorical; all others are continuous.
#'   Defaults to 10.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{continuous}{A \code{data.frame} with columns \code{x} (numeric),
#'       \code{yhat}, \code{name} (variable name), and optionally \code{grp}
#'       (the level of \code{xvar2.name}) and \code{time} (survival forests
#'       only) for all continuous predictors.}
#'     \item{categorical}{A \code{data.frame} with the same columns but
#'       \code{x} kept as character, for low-cardinality predictors.}
#'   }
#'
#' @seealso \code{\link{gg_partial}}, \code{\link[randomForestSRC]{partial.rfsrc}},
#'   \code{\link[randomForestSRC]{get.partial.plot.data}}
#'
#' @examples
#' ## ------------------------------------------------------------
#' ##
#' ## regression
#' ##
#' ## ------------------------------------------------------------
#'
#' airq.obj <- rfsrc(Ozone ~ ., data = airquality)
#'
#' ## partial effect for wind
#' prt_dta <- gg_partial_rfsrc(airq.obj,
#'                        xvar.names = c("Wind"))
#'
#' @importFrom dplyr mutate filter select all_of
#' @export
gg_partial_rfsrc <- function(rf_model,
                             xvar.names = NULL,
                             xvar2.name = NULL,
                             newx = NULL,
                             cat_limit = 10) {
  # Check the rfsrc type
  # rf_model$family

  # we supply new data, make sure we use that and that it is a dataframe...
  if (is.null(newx)) {
    newx = rf_model$xvar
  }

  if (sum(colnames(newx) %in% rf_model$xvar.names) != ncol(newx)) {
    stop("newx must be a dataframe with the same columns used to train the rfsrc object")
  }

  if (!is.null(xvar.names)) {
    if (sum(xvar.names %in% colnames(newx)) != length(xvar.names)) {
      stop("xvar.names contains column names not found in the rfsrc object")
    }
  }

  if (is.null(xvar2.name)) {
    pdta <- lapply(xvar.names, function(xname) {
      xval <- unlist(newx |>
                       dplyr::select(dplyr::all_of(xname)))
      gr <- length(unique(xval)) < cat_limit
      partial.obj <- randomForestSRC::partial.rfsrc(
        object = rf_model,
        partial.xvar = xname,
        partial.values = xval
      )
      pout <- randomForestSRC::get.partial.plot.data(partial.obj, granule = gr)
      out_dta <- data.frame(x = pout$x, yhat = pout$yhat)
      out_dta$name <- xname
      out_dta$type <- c("continuous", "categorical")[gr + 1]
      if (!is.null(pout$partial.time)) {
        out_dta$time <- pout$partial.time
      }
      return(out_dta)
    })
  } else {
    xv2 <- unique(unlist(newx |>
                           dplyr::select(dplyr::all_of(xvar2.name))))
    pdta <- lapply(xv2, function(x2val) {
      p1dta <- lapply(xvar.names, function(xname) {
        xval <- unlist(newx |>
                         dplyr::select(dplyr::all_of(xname)))
        gr <- length(unique(xval)) < cat_limit
        partial.obj <- randomForestSRC::partial.rfsrc(
          object = rf_model,
          partial.xvar = xname,
          partial.values = xval,
          partial.xvar2 = xvar2.name,
          partial.values2 = x2val
        )
        pout <- randomForestSRC::get.partial.plot.data(partial.obj, granule = gr)
        out_dta <- data.frame(x = pout$x, yhat = pout$yhat)
        out_dta$name <- xname
        out_dta$type <- c("continuous", "categorical")[gr + 1]
        if (!is.null(pout$partial.time)) {
          out_dta$time <- pout$partial.time
        }
        return(out_dta)
      })
      p1dta <- do.call("rbind", p1dta)
      p1dta$grp <- x2val
      return(p1dta)
    })
  }
  pdta <- do.call("rbind", pdta)
  # Split into continuous / categorical and tidy up the type column
  cont_idx <- pdta$type == "continuous"
  continuous <- pdta[cont_idx, , drop = FALSE]
  continuous$x <- as.numeric(continuous$x)
  continuous$type <- NULL
  categorical <- pdta[!cont_idx, , drop = FALSE]
  categorical$type <- NULL
  list(continuous = continuous, categorical = categorical)
}