##=============================================================================
#' Partial dependence data from an rfsrc model
#'
#' Computes partial dependence for one or more predictors by calling
#' \code{\link[randomForestSRC]{partial.rfsrc}} internally, then splits the
#' results into separate data frames for continuous and categorical variables.
#' Unlike \code{\link{gg_partial}}, no separate \code{plot.variable} call is
#' required — supply the fitted \code{rfsrc} object directly.
#'
#' @section Survival forests and \code{partial.time}:
#' \code{\link[randomForestSRC]{partial.rfsrc}} requires that every value in
#' \code{partial.time} be an exact member of the model's \code{time.interest}
#' vector (the unique observed event times stored in the fitted object).
#' Passing arbitrary time values — even plausible ones such as \code{c(1, 3)}
#' for a study measured in years — causes a C-level prediction error inside
#' \code{partial.rfsrc}.
#'
#' \code{gg_partial_rfsrc} handles this automatically: every element of
#' \code{partial.time} is silently snapped to its nearest \code{time.interest}
#' value before the call is made.  To target a specific follow-up horizon,
#' find the closest grid point yourself and pass it explicitly:
#'
#' \preformatted{
#' ti  <- rf_model$time.interest
#' t1  <- ti[which.min(abs(ti - 1))]   # nearest to 1 year
#' pd  <- gg_partial_rfsrc(rf_model, xvar.names = "x", partial.time = t1)
#' }
#'
#' @section Logical predictor columns:
#' \code{\link[randomForestSRC]{partial.rfsrc}} does not handle
#' \code{logical} predictor columns correctly in survival forests
#' (randomForestSRC <= 3.5.1).  If your training data contains binary 0/1
#' columns, convert them to \code{\link{factor}} rather than \code{logical}
#' before fitting the model.
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
#' @param partial.time Numeric vector of desired time points for survival
#'   forests (ignored for regression/classification).  Values are automatically
#'   snapped to the nearest entry in \code{rf_model$time.interest} — see the
#'   \strong{Survival forests} section above.  When \code{NULL} (default),
#'   three quartile points of \code{time.interest} are used.
#' @param cat_limit Variables with fewer than \code{cat_limit} unique values in
#'   \code{newx} are treated as categorical; all others are continuous.
#'   Defaults to 10.
#' @param n_eval Number of evaluation points for continuous variables. Instead
#'   of passing all observed values (which can be slow, especially for survival
#'   forests), continuous predictors are evaluated on a quantile grid of this
#'   many points. Categorical variables always use all unique levels.
#'   Defaults to 25.
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
                             partial.time = NULL,
                             cat_limit = 10,
                             n_eval = 25) {
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

  ## For survival forests, partial.rfsrc() requires time points that exactly
  ## match values in the model's time.interest vector (unique observed event
  ## times).  Arbitrary values like c(1, 3) cause a C-level prediction error.
  ## We therefore always snap partial.time to the nearest time.interest values,
  ## and when the caller supplies NULL we default to three quartile points.
  is_surv <- !is.null(rf_model$family) && grepl("surv", rf_model$family)
  if (is_surv) {
    ti <- rf_model$time.interest
    if (is.null(partial.time)) {
      partial.time <- quantile(ti, probs = c(0.25, 0.5, 0.75), names = FALSE)
    }
    ## Snap every requested time to its nearest entry in time.interest
    partial.time <- sapply(partial.time,
                           function(t) ti[which.min(abs(ti - t))],
                           USE.NAMES = FALSE)
    partial.time <- unique(partial.time)
  }

  ## Helper: build evaluation grid for one variable.
  ## Categorical variables get all unique levels; continuous variables get a
  ## quantile grid of n_eval points (much faster and gives a smooth curve).
  make_eval_grid <- function(xname) {
    xval <- unlist(newx |> dplyr::select(dplyr::all_of(xname)))
    xval <- xval[!is.na(xval)]
    gr   <- length(unique(xval)) < cat_limit
    if (!gr && length(unique(xval)) > n_eval) {
      xval <- quantile_pts(xval, groups = n_eval)
    } else {
      xval <- sort(unique(xval))
    }
    list(xval = xval, categorical = gr)
  }

  ## Helper: call partial.rfsrc with the appropriate time argument.
  call_partial <- function(xname, xval, xvar2.name = NULL, x2val = NULL) {
    args <- list(
      object         = rf_model,
      partial.xvar   = xname,
      partial.values = xval
    )
    if (!is.null(xvar2.name)) {
      args$partial.xvar2   <- xvar2.name
      args$partial.values2 <- x2val
    }
    if (is_surv) {
      args$partial.time <- partial.time
    }
    do.call(randomForestSRC::partial.rfsrc, args)
  }

  if (is.null(xvar2.name)) {
    pdta <- lapply(xvar.names, function(xname) {
      eg   <- make_eval_grid(xname)
      xval <- eg$xval
      gr   <- eg$categorical
      partial.obj <- call_partial(xname, xval)
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
    xv2 <- xv2[!is.na(xv2)]
    pdta <- lapply(xv2, function(x2val) {
      p1dta <- lapply(xvar.names, function(xname) {
        eg   <- make_eval_grid(xname)
        xval <- eg$xval
        gr   <- eg$categorical
        partial.obj <- call_partial(xname, xval, xvar2.name, x2val)
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
