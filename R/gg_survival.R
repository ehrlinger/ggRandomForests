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
#' @details \code{gg_survival} is a wrapper function for generating
#' nonparametric survival estimates using either \code{\link{nelson}}-Aalen
#' or \code{\link{kaplan}}-Meier estimates.
#'
#' @param data A \code{data.frame} containing the survival data.
#' @param interval Character; name of the time-to-event column in \code{data}.
#' @param censor Character; name of the event-indicator column in \code{data}
#'   (1 = event occurred, 0 = censored).
#' @param by Optional character; name of a grouping column in \code{data} for
#'   stratified estimates. Defaults to \code{NULL} (unstratified).
#' @param type One of \code{"kaplan"} (Kaplan-Meier, default) or
#'   \code{"nelson"} (Nelson-Aalen cumulative hazard).
#' @param ... Additional arguments passed to \code{\link{kaplan}} or
#'   \code{\link{nelson}} (e.g. \code{conf.int} to change the CI width).
#'
#' @return A \code{gg_survival} \code{data.frame} with columns \code{time},
#'   \code{surv} (or \code{cum_haz} for Nelson-Aalen), \code{lower},
#'   \code{upper} (confidence limits), and \code{n.risk}. A \code{strata}
#'   column is added when \code{by} is supplied.
#'
#' @seealso \code{\link{kaplan}} \code{\link{nelson}}
#' @seealso \code{\link{plot.gg_survival}}
#'
#' @examples
#' ## -------- pbc data
#' data(pbc, package = "randomForestSRC")
#' pbc$time <- pbc$days / 364.25
#'
#' # This is the same as kaplan
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc
#' )
#'
#' plot(gg_dta, error = "none")
#' plot(gg_dta)
#'
#' # Stratified on treatment variable.
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc, by = "treatment"
#' )
#'
#' plot(gg_dta, error = "none")
#' plot(gg_dta)
#'
#' # ...with smaller confidence limits.
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc, by = "treatment", conf.int = .68
#' )
#'
#' plot(gg_dta, error = "lines")
#' 
#' @export
gg_survival <- function(interval = NULL,
                        censor = NULL,
                        by = NULL,
                        data,
                        type = c("kaplan", "nelson"),
                        ...) {
  # Validate and normalise the estimator choice.  Kaplan-Meier is the default.
  type <- match.arg(type)

  # Delegate entirely to the selected estimator helper.  Both kaplan() and
  # nelson() return a gg_survival object that plot.gg_survival can render.
  gg_dta <- switch(type,
    kaplan = kaplan(
      interval = interval,
      censor = censor,
      by = by,
      data = data,
      ...
    ),
    nelson = nelson(
      interval = interval,
      censor = censor,
      by = by,
      data = data,
      ...
    )
  )

  return(gg_dta)
}
