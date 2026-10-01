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
#' nonparametric Nelson-Aalen estimates
#'
#' \code{cum_haz} is the Nelson-Aalen estimate of the cumulative hazard: at
#' each event time the number of events is divided by the number at risk, and
#' the ratios are summed. The \code{surv} column, its standard error and its
#' confidence limits are the Kaplan-Meier estimates, as \code{\link{kaplan}}
#' returns them, and \code{hazard}, \code{density}, \code{life} and
#' \code{proplife} are derived from that \code{surv}. The two functions
#' therefore differ only in \code{cum_haz}, where \code{kaplan} reports
#' \eqn{-\log S(t)}. The two agree closely while the risk set is large and
#' diverge in the tail; when the last observation is an event, \eqn{-\log S(t)}
#' is infinite there and the Nelson-Aalen sum is not.
#'
#' @param data name of the survival training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param by stratifying variable in the training dataset, defaults to NULL
#' @param weight optional numeric vector of event weights, one per row of
#'   \code{data} (default \code{NULL}, every event counts once). The weights
#'   apply to events only: each increment of \code{cum_haz} is the summed
#'   weight of the events at that time over the unweighted number at risk, so
#'   a censored observation's weight has no effect. Use it for
#'   severity-weighted events. The Kaplan-Meier columns are not weighted.
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
#' # These get run through the gg_survival examples.
#' data(pbc, package = "randomForestSRC")
#' pbc$time <- pbc$days / 364.25
#'
#' # This is the same as gg_survival
#' gg_dta <- nelson(
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
#' plot(gg_dta, error = "lines")
#' plot(gg_dta)
#'
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc, by = "treatment",
#'   type = "nelson"
#' )
#'
#' plot(gg_dta, error = "bars")
#' plot(gg_dta)
#'
#' @export
nelson <-
  function(interval,
           censor,
           data,
           by = NULL,
           weight = NULL,
           ...) {
    if (!is.null(weight)) {
      if (!is.numeric(weight) || length(weight) != nrow(data) ||
            anyNA(weight) || any(weight < 0)) {
        stop("nelson: 'weight' must be a non-negative numeric vector with ",
             "one value per row of 'data'.", call. = FALSE)
      }
    }

    # Build the Surv object and fit the (possibly stratified) estimator.
    srv <- # nolint: object_usage_linter
      survival::Surv(time = data[[interval]], event = data[[censor]])
    if (is.null(by)) {
      srv_tab <- survival::survfit(srv ~ 1, ...)
    } else {
      strat <- .strata_factor(data[[by]])
      grp <- strat$grp # nolint: object_usage_linter
      srv_tab <- survival::survfit(srv ~ grp, ...)
    }

    # Events at each time. With a weight, a second fit hands back the weighted
    # event totals on the same rows (same strata, same tie handling); only its
    # n.event is used, so the risk set below stays the unweighted count.
    events <- srv_tab$n.event
    if (!is.null(weight)) {
      if (is.null(by)) {
        wtd_tab <- survival::survfit(srv ~ 1, weights = weight, ...)
      } else {
        wtd_tab <- survival::survfit(srv ~ grp, weights = weight, ...)
      }
      if (length(wtd_tab$time) != length(srv_tab$time)) {
        stop("nelson: the weighted and unweighted fits returned different ",
             "event times.", call. = FALSE)
      }
      events <- wtd_tab$n.event
    }

    # Nelson-Aalen increment: events over the number at risk. It is summed
    # within each stratum once the rows are labelled.
    increment <- events / srv_tab$n.risk

    # Collect per-time-point statistics into a flat data frame.
    tbl <- data.frame(
      cbind(
        time = srv_tab$time,
        n = srv_tab$n.risk,        # number at risk just before t
        cens = srv_tab$n.censor,   # number censored at t
        dead = srv_tab$n.event,    # number of events at t
        surv = srv_tab$surv,       # KM survival estimate S(t)
        se = srv_tab$std.err,      # standard error of S(t)
        lower = srv_tab$lower,     # lower confidence bound
        upper = srv_tab$upper,     # upper confidence bound
        cum_haz = increment
      )
    )

    # Detect stratum boundaries and label each row with its group name.
    if (!is.null(by)) {
      tbl <- .label_strata(tbl, srv_tab, strat,
                           .fit_rows(srv, list(...)$subset))
    }

    # H(t) = sum over event times up to t, restarting in every stratum.
    grp <- if (is.null(by)) rep(1L, nrow(tbl)) else tbl$groups
    tbl$cum_haz <- stats::ave(tbl$cum_haz, grp, FUN = cumsum)

    # Retain only rows with at least one event.
    gg_dta <- tbl[which(tbl[["dead"]] != 0), ]

    # Derived interval-based quantities (same as in kaplan.R). The lags
    # restart in every stratum; see the note there.
    grp <- if (is.null(by)) rep(1L, nrow(gg_dta)) else gg_dta$groups
    lag_within <- function(val, start) {
      stats::ave(val, grp, FUN = function(v) c(start, v[-length(v)]))
    }
    lag_surv <- lag_within(gg_dta$surv, 1)
    lag_time <- lag_within(gg_dta$time, 0)

    delta_t <- gg_dta$time - lag_time
    # h(t) ≈ -log(S(t)/S(t-)) / Δt
    hzrd <- log(lag_surv / gg_dta$surv) / delta_t

    # f(t) ≈ (S(t-) - S(t)) / Δt
    dnsty <- (lag_surv - gg_dta$surv) / delta_t
    mid_int <- (gg_dta$time + lag_time) / 2

    # Cumulative expected life in each interval (trapezoidal rule):
    # L(t_i) = L(t_{i-1}) + (S(t_{i-1}) + S(t_i)) / 2 * Δt_i
    life <- stats::ave((lag_surv + gg_dta$surv) / 2 * delta_t, grp,
                       FUN = cumsum)
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
