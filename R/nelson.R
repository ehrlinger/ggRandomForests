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
#' @param data name of the survival training data.frame
#' @param interval name of the interval variable in the training dataset.
#' @param censor name of the censoring variable in the training dataset.
#' @param by stratifying variable in the training dataset, defaults to NULL
#' @param weight for each observation (default=NULL)
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
    # Incorporate observation weights: zero out weights for censored records
    # so they do not contribute to the weighted hazard computation.
    if (!is.null(weight)) {
      weight <- data[[censor]] * weight
    }

    # Build the Surv object and fit the (possibly stratified) estimator.
    srv <-
      survival::Surv(time = data[[interval]], event = data[[censor]])
    if (is.null(by)) {
      srv_tab <- survival::survfit(srv ~ 1, ...)
    } else {
      srv_tab <-
        survival::survfit(srv ~ survival::strata(data[[by]]), ...)
    }

    # Nelson-Aalen cumulative hazard: Λ(t) = Σ d_i / n_i  over t_i ≤ t
    # (The loop below computes the partial sums; then we overwrite with
    # -log(S(t)) which is equivalent and numerically identical for KM.)
    hazard <- srv_tab$n.event / srv_tab$n.risk
    cum_hazard <- vector()
    for (i in seq_len(length(hazard))) {
      cum_hazard[i] <- sum(hazard[1:i])
    }
    cum_hazard <- c(cum_hazard, cum_hazard[length(cum_hazard)])
    # Use -log(S(t)) for consistency with the Kaplan-Meier relation.
    cum_hazard <- -log(srv_tab$surv)

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
        cum_haz = cum_hazard
      )
    )

    # Detect stratum boundaries by finding time resets in the concatenated
    # survfit output, then label each row with its group name.
    if (!is.null(by)) {
      tm_splits <- which(c(FALSE, sapply(2:nrow(tbl), function(ind) {
        tbl$time[ind] < tbl$time[ind - 1]
      })))

      lbls <- unique(data[[by]])
      tbl$groups <- lbls[1]

      for (ind in 2:(length(tm_splits) + 1)) {
        tbl$groups[tm_splits[ind - 1]:nrow(tbl)] <- lbls[ind]
      }
    }

    # Retain only rows with at least one event.
    gg_dta <- tbl[which(tbl[["dead"]] != 0), ]

    # Derived interval-based quantities (same as in kaplan.R).
    lag_surv <- c(1, gg_dta$surv)[-(dim(gg_dta)[1] + 1)]
    lag_time <- c(0, gg_dta$time)[-(dim(gg_dta)[1] + 1)]

    delta_t <- gg_dta$time - lag_time
    # h(t) ≈ -log(S(t)/S(t-)) / Δt
    hzrd <- log(lag_surv / gg_dta$surv) / delta_t

    # f(t) ≈ (S(t-) - S(t)) / Δt
    dnsty <- (lag_surv - gg_dta$surv) / delta_t
    mid_int <- (gg_dta$time + lag_time) / 2
    lag_l <- 0

    life <- vector("numeric", length = dim(gg_dta)[1])
    for (ind in seq_len(dim(gg_dta)[1])) {
      life[ind] <-
        lag_l + delta_t[ind] * (3 * gg_dta[ind, "surv"] - lag_surv[ind]) / 2
      lag_l <- life[ind]
    }
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
