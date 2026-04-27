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
#'   \strong{Survival forests} section below.  When \code{NULL} (default),
#'   three quartile points of \code{time.interest} are used.
#' @param partial.type Character; type of predicted value for survival
#'   forests, passed through to \code{\link[randomForestSRC]{partial.rfsrc}}.
#'   One of \code{"surv"} (default), \code{"chf"}, or \code{"mort"}. Ignored
#'   for non-survival forests. \code{partial.rfsrc()} requires a non-\code{NULL}
#'   value for survival families; supplying it here avoids a cryptic
#'   \dQuote{argument is of length zero} error from the underlying C code.
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
                             partial.type = c("surv", "chf", "mort"),
                             cat_limit = 10,
                             n_eval = 25) {
  if (is.null(newx)) {
    newx <- rf_model$xvar
  }

  if (sum(colnames(newx) %in% rf_model$xvar.names) != ncol(newx)) {
    stop("newx must be a dataframe with the same columns used to train the rfsrc object")
  }

  if (!is.null(xvar.names)) {
    if (sum(xvar.names %in% colnames(newx)) != length(xvar.names)) {
      stop("xvar.names contains column names not found in the rfsrc object")
    }
  }

  v         <- validate_partial_args(n_eval, cat_limit)
  n_eval    <- v$n_eval
  cat_limit <- v$cat_limit

  is_surv <- !is.null(rf_model$family) && grepl("surv", rf_model$family)
  if (is_surv) {
    partial.time <- snap_partial_time(rf_model, partial.time)
    # partial.rfsrc() requires a non-NULL partial.type for survival forests;
    # NULL triggers a zero-length comparison inside the C code.
    partial.type <- match.arg(partial.type)
  } else {
    partial.type <- NULL
  }

  if (is.null(xvar2.name)) {
    pdta <- partial_no_group(xvar.names, newx, rf_model,
                             cat_limit, n_eval, is_surv, partial.time,
                             partial.type)
  } else {
    pdta <- partial_with_group(xvar.names, xvar2.name, newx, rf_model,
                               cat_limit, n_eval, is_surv, partial.time,
                               partial.type)
  }

  split_partial_result(do.call("rbind", pdta))
}

## ---- unexported helpers -------------------------------------------------------

## Check that x is a single non-NA numeric >= min_val; return as integer.
validate_scalar_int <- function(x, name, min_val = 2L) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && x >= min_val
  if (!ok) {
    stop(sprintf("'%s' must be a single integer >= %d.", name, min_val),
         call. = FALSE)
  }
  as.integer(x)
}

## Validate and coerce n_eval / cat_limit.
validate_partial_args <- function(n_eval, cat_limit) {
  list(
    n_eval    = validate_scalar_int(n_eval,    "n_eval",    2L),
    cat_limit = validate_scalar_int(cat_limit, "cat_limit", 2L)
  )
}

## Snap requested time points to the nearest values in time.interest.
snap_partial_time <- function(rf_model, partial.time) {
  ti <- rf_model$time.interest
  if (is.null(partial.time)) {
    partial.time <- quantile(ti, probs = c(0.25, 0.5, 0.75), names = FALSE)
  }
  snapped <- sapply(partial.time,
                    function(t) ti[which.min(abs(ti - t))],
                    USE.NAMES = FALSE)
  unique(snapped)
}

## Build the evaluation grid (xval vector + categorical flag) for one variable.
make_eval_grid <- function(xname, newx, cat_limit, n_eval) {
  # Use `[[` to preserve the column's class (factor, character, numeric, …).
  # unlist(dplyr::select(...)) would coerce factors to integer codes, breaking
  # both the cat_limit check and the partial.values passed to partial.rfsrc().
  xval <- newx[[xname]]
  xval <- xval[!is.na(xval)]
  if (length(xval) == 0L) {
    warning(sprintf(
      "Variable '%s' contains only NA values in 'newx'; skipping partial dependence.",
      xname
    ), call. = FALSE)
    return(NULL)
  }
  gr <- is.factor(xval) || is.character(xval) || length(unique(xval)) < cat_limit
  if (!gr && length(unique(xval)) > n_eval) {
    xval <- quantile_pts(xval, groups = n_eval)
  } else if (is.factor(xval)) {
    xval <- levels(droplevels(xval))   # preserve factor ordering; drop unused levels
  } else {
    xval <- sort(unique(xval))
  }
  list(xval = xval, categorical = gr)
}

## Thin wrapper around partial.rfsrc that builds the argument list.
call_partial_rfsrc <- function(rf_model, xname, xval,
                                is_surv, partial.time, partial.type,
                                xvar2.name = NULL, x2val = NULL) {
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
    args$partial.type <- partial.type
  }
  do.call(randomForestSRC::partial.rfsrc, args)
}

## Process a single predictor variable and return a tidy data.frame (or NULL).
partial_one_var <- function(xname, newx, rf_model,
                            cat_limit, n_eval, is_surv, partial.time,
                            partial.type,
                            xvar2.name = NULL, x2val = NULL) {
  eg <- make_eval_grid(xname, newx, cat_limit, n_eval)
  if (is.null(eg)) return(NULL)
  xval <- eg$xval
  gr   <- eg$categorical
  partial.obj <- call_partial_rfsrc(rf_model, xname, xval,
                                     is_surv, partial.time, partial.type,
                                     xvar2.name, x2val)
  pout    <- randomForestSRC::get.partial.plot.data(partial.obj, granule = gr)
  # Survival forests with >1 partial.time return yhat as an
  # [length(partial.values) x length(partial.time)] matrix; expand to long form
  # so each (x, time) pair is its own row. For non-survival or single-time
  # cases yhat is already a vector of length(partial.values).
  if (is.matrix(pout$yhat)) {
    pt <- if (!is.null(pout$partial.time)) pout$partial.time else seq_len(ncol(pout$yhat))
    out_dta <- data.frame(
      x    = rep(pout$x, times = length(pt)),
      yhat = as.numeric(pout$yhat),
      time = rep(pt, each = length(pout$x))
    )
  } else {
    out_dta <- data.frame(x = pout$x, yhat = pout$yhat)
    if (!is.null(pout$partial.time)) {
      out_dta$time <- pout$partial.time
    }
  }
  out_dta$name <- xname
  out_dta$type <- c("continuous", "categorical")[gr + 1L]
  out_dta
}

## Compute partial dependence across xvar.names (no grouping variable).
partial_no_group <- function(xvar.names, newx, rf_model,
                             cat_limit, n_eval, is_surv, partial.time,
                             partial.type) {
  pdta <- lapply(xvar.names, partial_one_var,
                 newx = newx, rf_model = rf_model,
                 cat_limit = cat_limit, n_eval = n_eval,
                 is_surv = is_surv, partial.time = partial.time,
                 partial.type = partial.type)
  Filter(Negate(is.null), pdta)
}

## Compute partial dependence across xvar.names for each level of xvar2.name.
partial_with_group <- function(xvar.names, xvar2.name, newx, rf_model,
                               cat_limit, n_eval, is_surv, partial.time,
                               partial.type) {
  xv2 <- unique(newx[[xvar2.name]])
  xv2 <- xv2[!is.na(xv2)]
  if (length(xv2) == 0L) {
    stop(sprintf(
      "Grouping variable '%s' contains only NA values in 'newx'; cannot compute surface partial dependence.",
      xvar2.name
    ), call. = FALSE)
  }
  pdta <- lapply(xv2, function(x2val) {
    p1dta <- lapply(xvar.names, partial_one_var,
                    newx = newx, rf_model = rf_model,
                    cat_limit = cat_limit, n_eval = n_eval,
                    is_surv = is_surv, partial.time = partial.time,
                    partial.type = partial.type,
                    xvar2.name = xvar2.name, x2val = x2val)
    p1dta <- Filter(Negate(is.null), p1dta)
    if (length(p1dta) == 0L) return(NULL)
    p1dta        <- do.call("rbind", p1dta)
    p1dta$grp    <- x2val
    p1dta
  })
  Filter(Negate(is.null), pdta)
}

## Split the combined data.frame into continuous / categorical and stamp class.
split_partial_result <- function(pdta) {
  cont_idx        <- pdta$type == "continuous"
  continuous      <- pdta[cont_idx, , drop = FALSE]
  continuous$x    <- as.numeric(continuous$x)
  continuous$type <- NULL
  categorical      <- pdta[!cont_idx, , drop = FALSE]
  categorical$type <- NULL
  result <- list(continuous = continuous, categorical = categorical)
  class(result) <- "gg_partial_rfsrc"
  result
}
