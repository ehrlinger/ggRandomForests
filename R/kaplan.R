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
#' nonparametric Kaplan-Meier estimates
#'
#' @param data name of the training set \code{data.frame}
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param by stratifying variable in the training dataset, defaults to NULL
#' @param ... arguments passed to the \code{survfit} function
#'
#' @return \code{\link{gg_survival}} object
#'
#' @importFrom survival Surv survfit strata
#'
#' @seealso \code{\link{gg_survival}} \code{\link{nelson}}
#' \code{\link{plot.gg_survival}}
#'
#' @examples
#'
#' # These get run through the gg_survival examples.
#' data(pbc, package = "randomForestSRC")
#' pbc$time <- pbc$days / 364.25
#'
#' # This is the same as gg_survival
#' gg_dta <- kaplan(
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
#' @export
kaplan <- function(interval,
                   censor,
                   data,
                   by = NULL, ...) {
  # Build a Surv object from the named columns in the data frame.
  srv <- survival::Surv(time = data[[interval]], event = data[[censor]]) # nolint: object_usage_linter

  # Fit the Kaplan-Meier estimator; stratify on `by` when provided.
  if (is.null(by)) {
    srv_tab <- survival::survfit(srv ~ 1, ...)
  } else {
    strat <- .strata_factor(data[[by]])
    grp <- strat$grp # nolint: object_usage_linter
    srv_tab <- survival::survfit(srv ~ grp, ...)
  }

  # Cumulative hazard H(t) = -log(S(t)) via the Nelson-Aalen transform.
  cum_hazard <- -log(srv_tab$surv)

  # Collect per-time-point summary statistics into a flat data frame.
  tbl <- data.frame(
    cbind(
      time = srv_tab$time,
      n = srv_tab$n.risk,        # number at risk just before time t
      cens = srv_tab$n.censor,   # number censored at time t
      dead = srv_tab$n.event,    # number of events at time t
      surv = srv_tab$surv,       # KM survival estimate S(t)
      se = srv_tab$std.err,      # standard error of S(t)
      lower = srv_tab$lower,     # lower confidence bound
      upper = srv_tab$upper,     # upper confidence bound
      cum_haz = cum_hazard
    )
  )

  # When stratifying, stitch a "groups" label column onto the table.
  if (!is.null(by)) {
    tbl <- .label_strata(tbl, srv_tab, strat,
                         .fit_rows(srv, list(...)$subset))
  }

  # Keep only rows where at least one event occurred; censoring-only rows
  # do not contribute new KM estimates.
  gg_dta <- tbl[which(tbl[["dead"]] != 0), ]

  # Derived quantities computed from interval-based lagged differences. The
  # lags restart in every stratum: survfit() stacks the strata end to end, and
  # a lag taken across that boundary measures a group's first interval from
  # the last event time of the group before it.
  grp <- if (is.null(by)) rep(1L, nrow(gg_dta)) else gg_dta$groups
  lag_within <- function(val, start) {
    stats::ave(val, grp, FUN = function(v) c(start, v[-length(v)]))
  }
  lag_s <- lag_within(gg_dta$surv, 1)
  lag_t <- lag_within(gg_dta$time, 0)

  delta_t <- gg_dta$time - lag_t
  # Conditional hazard rate approximation: h(t) ≈ -log(S(t)/S(t-)) / Δt
  hzrd <- log(lag_s / gg_dta$surv) / delta_t

  # Probability density: f(t) ≈ (S(t-) - S(t)) / Δt
  dnsty <- (lag_s - gg_dta$surv) / delta_t
  mid_int <- (gg_dta$time + lag_t) / 2

  # Cumulative expected life in each interval (trapezoidal rule):
  # L(t_i) = L(t_{i-1}) + (S(t_{i-1}) + S(t_i)) / 2 * Δt_i
  life <- stats::ave((lag_s + gg_dta$surv) / 2 * delta_t, grp, FUN = cumsum)
  prp_life <- life / gg_dta$time
  gg_dta <- data.frame(
    cbind(
      gg_dta,
      hazard = hzrd,
      density = dnsty,
      mid_int = mid_int,
      life = life,
      proplife = prp_life
    )
  )

  class(gg_dta) <- c("gg_survival", class(gg_dta))
  invisible(gg_dta)
}
