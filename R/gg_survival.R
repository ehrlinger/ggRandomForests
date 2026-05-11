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
#'
#' Nonparametric survival estimates.
#'
#' @details \code{gg_survival} is an S3 generic for generating nonparametric
#' survival estimates.  It dispatches on the class of its first argument:
#'
#' \describe{
#'   \item{\code{rfsrc}}{Extracts the response data from the fitted forest and
#'     delegates to \code{\link{kaplan}}.  Use the \code{by} argument to
#'     stratify on a predictor stored in the model's \code{xvar} slot.}
#'   \item{default}{Accepts raw survival data columns via the \code{interval},
#'     \code{censor}, \code{by}, and \code{data} arguments, delegating to
#'     either \code{\link{kaplan}} (default) or \code{\link{nelson}}.}
#' }
#'
#' @param object For the \code{rfsrc} method: a fitted
#'   \code{\link[randomForestSRC]{rfsrc}} survival forest.  For the default
#'   method: pass \code{NULL} (or omit) and supply \code{interval},
#'   \code{censor}, and \code{data} instead.
#' @param interval Character; name of the time-to-event column in \code{data}
#'   (default method only).
#' @param censor Character; name of the event-indicator column in \code{data}
#'   (1 = event, 0 = censored; default method only).
#' @param by Optional character; name of a grouping column for stratified
#'   estimates.  For the \code{rfsrc} method, \code{by} must be a column in
#'   \code{object$xvar}.
#' @param data A \code{data.frame} containing survival data (default method
#'   only).
#' @param type One of \code{"kaplan"} (Kaplan-Meier, default) or
#'   \code{"nelson"} (Nelson-Aalen cumulative hazard).  Default method only.
#' @param ... Additional arguments passed to \code{\link{kaplan}} or
#'   \code{\link{nelson}}.
#'
#' @return A \code{gg_survival} \code{data.frame} with columns \code{time},
#'   \code{surv}, \code{cum_haz}, \code{lower}, \code{upper}, \code{n.risk},
#'   and optionally \code{groups} when \code{by} is supplied.
#'
#' @seealso \code{\link{kaplan}} \code{\link{nelson}}
#' @seealso \code{\link{plot.gg_survival}}
#'
#' @examples
#' ## -------- pbc data (default method — raw data columns)
#' data(pbc, package = "randomForestSRC")
#' pbc$time <- pbc$days / 364.25
#'
#' gg_dta <- gg_survival(interval = "time", censor = "status", data = pbc)
#' plot(gg_dta, error = "none")
#'
#' # Stratified
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc, by = "treatment"
#' )
#' plot(gg_dta)
#'
#' @export
gg_survival <- function(object = NULL,
                        interval = NULL,
                        censor = NULL,
                        by = NULL,
                        data = NULL,
                        type = c("kaplan", "nelson"),
                        ...) {
  UseMethod("gg_survival")
}

#' @rdname gg_survival
#' @export
gg_survival.rfsrc <- function(object,
                              interval = NULL,
                              censor   = NULL,
                              by       = NULL,
                              data     = NULL,
                              type     = c("kaplan", "nelson"),
                              ...) {
  ## interval, censor, data, and type are accepted for consistency with the
  ## generic signature but are ignored: this method extracts everything it
  ## needs from the fitted forest's $yvar and $xvar slots.
  ##
  ## randomForestSRC stores the outcome as a two-column data frame: time (col 1)
  ## and status/censor (col 2) in object$yvar.
  yvar <- object$yvar
  if (is.null(yvar) || !is.data.frame(yvar) || ncol(yvar) < 2L) {
    stop(
      "gg_survival requires a survival forest; this rfsrc object ",
      "does not appear to be a survival forest.",
      call. = FALSE
    )
  }

  interval_col <- colnames(yvar)[1L]
  censor_col   <- colnames(yvar)[2L]
  surv_data    <- yvar

  if (!is.null(by)) {
    if (!by %in% colnames(object$xvar)) {
      stop(sprintf(
        "'by' column '%s' not found in the forest's xvar slot.", by
      ), call. = FALSE)
    }
    surv_data[[by]] <- object$xvar[[by]]
  }

  gg_dta <- kaplan(interval = interval_col, censor = censor_col, by = by,
                   data = surv_data, ...)
  .set_provenance(gg_dta, object)
}

#' @rdname gg_survival
#' @export
gg_survival.default <- function(object = NULL,
                                interval = NULL,
                                censor = NULL,
                                by = NULL,
                                data = NULL,
                                type = c("kaplan", "nelson"),
                                ...) {
  ## Allow data to be passed as the first positional argument for convenience.
  if (is.data.frame(object) && is.null(data)) {
    data <- object
  }

  type <- match.arg(type)

  gg_dta <- switch(type, # nolint: object_usage_linter
    kaplan = kaplan(
      interval = interval,
      censor   = censor,
      by       = by,
      data     = data,
      ...
    ),
    nelson = nelson(
      interval = interval,
      censor   = censor,
      by       = by,
      data     = data,
      ...
    )
  )

  return(gg_dta)
}
