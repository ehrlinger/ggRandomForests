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
#' Brier score and CRPS for survival forests
#'
#' Extract the time-resolved Brier score and continuous ranked probability
#' score (CRPS) for a survival forest grown with \code{randomForestSRC}. The
#' Brier score is computed at each time on \code{object$time.interest}, both
#' overall and stratified by mortality-risk quartile. CRPS is the running
#' trapezoidal integral of the Brier score, normalised by elapsed time, and
#' is computed within each quartile and overall.
#'
#' @details Wraps \code{\link[randomForestSRC]{get.brier.survival}} and
#' rebuilds the quartile decomposition + running CRPS from the returned
#' \code{brier.matx} and \code{mort} components, mirroring the computation
#' in \code{randomForestSRC:::plot.survival}. The Brier score uses
#' inverse-probability-of-censoring weighting; the censoring distribution
#' is estimated either by Kaplan-Meier (\code{cens.model = "km"}, the
#' default) or by a separate censoring forest (\code{cens.model = "rfsrc"}).
#'
#' @param object A fitted \code{\link[randomForestSRC]{rfsrc}} survival
#'   forest (\code{object$family == "surv"}).
#' @param ... Optional arguments passed through to the methods. Supported
#'   for the \code{rfsrc} method:
#'   \describe{
#'     \item{\code{subset}}{Integer vector of training-set indices to
#'       restrict the evaluation to.}
#'     \item{\code{cens.model}}{Censoring-distribution estimator used in
#'       the IPCW weights: \code{"km"} (Kaplan-Meier, default) or
#'       \code{"rfsrc"} (random survival forest on the censoring
#'       indicator).}
#'   }
#'
#' @return A \code{gg_brier} \code{data.frame} with columns
#'   \describe{
#'     \item{time}{event time grid (\code{object$time.interest}).}
#'     \item{brier}{overall Brier score at each time.}
#'     \item{bs.q25, bs.q50, bs.q75, bs.q100}{Brier score within each
#'       mortality-risk quartile (lowest to highest risk).}
#'     \item{bs.lower, bs.upper}{15th and 85th percentile of per-subject
#'       Brier contributions at each time. Used by
#'       \code{plot.gg_brier(by_quartile = TRUE)} to draw an envelope
#'       around the overall curve.}
#'     \item{crps}{running CRPS (overall) at each time, normalised by
#'       elapsed time.}
#'     \item{crps.q25, crps.q50, crps.q75, crps.q100}{running CRPS within
#'       each mortality-risk quartile.}
#'     \item{crps.lower, crps.upper}{running CRPS of the 15th / 85th
#'       per-subject Brier percentile, normalised by elapsed time.}
#'   }
#'   The integrated CRPS (a single scalar matching
#'   \code{get.brier.survival()$crps}) is attached as
#'   \code{attr(., "crps_integrated")}.
#'
#' @seealso \code{\link{plot.gg_brier}},
#'   \code{\link[randomForestSRC]{get.brier.survival}},
#'   \code{\link{gg_error}}
#'
#' @references
#' Graf E., Schmoor C., Sauerbrei W., Schumacher M. (1999). Assessment and
#' comparison of prognostic classification schemes for survival data.
#' Statistics in Medicine, 18(17-18):2529-2545.
#'
#' Gerds T.A., Schumacher M. (2006). Consistent estimation of the expected
#' Brier score in general survival models with right-censored event times.
#' Biometrical Journal, 48(6):1029-1040.
#'
#' @examples
#' \dontrun{
#' data(pbc, package = "randomForestSRC")
#' rfsrc_pbc <- randomForestSRC::rfsrc(
#'   Surv(days, status) ~ ., data = pbc, nsplit = 10
#' )
#' gg_dta <- gg_brier(rfsrc_pbc)
#' plot(gg_dta)
#' plot(gg_dta, type = "crps")
#' plot(gg_dta, by_quartile = TRUE)   # overall line + 15-85% envelope
#'
#' # Multi-model comparison: stack gg_brier outputs and plot with ggplot2.
#' rf2 <- randomForestSRC::rfsrc(
#'   Surv(days, status) ~ ., data = pbc, nsplit = 10, mtry = 4
#' )
#' compare_dta <- dplyr::bind_rows(
#'   dplyr::mutate(gg_brier(rfsrc_pbc), model = "default"),
#'   dplyr::mutate(gg_brier(rf2),       model = "mtry=4")
#' )
#' ggplot2::ggplot(compare_dta,
#'   ggplot2::aes(x = time, y = brier, colour = model)) +
#'   ggplot2::geom_line()
#' }
#'
#' @importFrom stats quantile
#' @export
gg_brier <- function(object, ...) {
  UseMethod("gg_brier", object)
}

#' @export
gg_brier.rfsrc <- function(object,
                           subset = NULL,
                           cens.model = c("km", "rfsrc"),
                           ...) {
  if (!inherits(object, "rfsrc")) {
    stop("This function only works on rfsrc objects.")
  }
  if (is.null(object$family) || object$family != "surv") {
    stop("gg_brier only supports right-censored survival forests.")
  }

  cens.model <- match.arg(cens.model)

  brier_obj <- randomForestSRC::get.brier.survival(
    object,
    subset = subset,
    cens.model = cens.model
  )

  bs_df <- brier_obj$brier.score
  if (!all(c("time", "brier.score") %in% names(bs_df))) {
    stop("Unexpected output from get.brier.survival(); ",
         "ggRandomForests may need to be updated for this randomForestSRC version.")
  }

  brier_matx <- brier_obj$brier.matx
  mort       <- brier_obj$mort

  # Quartile breakpoints of per-subject mortality. The lowest bin uses an
  # open-left bound so the minimum-risk subject is captured.
  mort_breaks <- c(min(mort, na.rm = TRUE) - 1e-05,
                   stats::quantile(mort, (1:4) / 4, na.rm = TRUE))

  bs_quartile <- vapply(seq_len(4), function(k) {
    in_bin <- mort > mort_breaks[k] & mort <= mort_breaks[k + 1]
    colMeans(brier_matx[in_bin, , drop = FALSE], na.rm = TRUE)
  }, numeric(nrow(bs_df)))

  # Per-time 15th and 85th percentile of per-subject Brier contributions.
  # Provides a non-parametric envelope around the overall curve for the
  # default by_quartile = TRUE ribbon display.
  bs_envelope <- apply(brier_matx, 2, stats::quantile,
                       probs = c(0.15, 0.85), na.rm = TRUE)

  time_grid <- bs_df$time
  bs_all    <- bs_df$brier.score

  # Running, time-normalised CRPS via trapezoid rule over [time[1], time[j]].
  crps_running <- function(times, scores) {
    n <- length(times)
    out <- numeric(n)
    for (j in seq_len(n)) {
      if (j == 1) {
        out[j] <- scores[1]
      } else {
        span <- times[j] - times[1]
        if (span <= 0) {
          out[j] <- NA_real_
        } else {
          out[j] <- .trapz(times[seq_len(j)], scores[seq_len(j)]) / span
        }
      }
    }
    out
  }

  crps_all   <- crps_running(time_grid, bs_all)
  crps_q     <- vapply(seq_len(4), function(k) {
    crps_running(time_grid, bs_quartile[, k])
  }, numeric(length(time_grid)))
  crps_lower <- crps_running(time_grid, bs_envelope[1, ])
  crps_upper <- crps_running(time_grid, bs_envelope[2, ])

  gg_dta <- data.frame(
    time        = time_grid,
    brier       = bs_all,
    bs.q25      = bs_quartile[, 1],
    bs.q50      = bs_quartile[, 2],
    bs.q75      = bs_quartile[, 3],
    bs.q100     = bs_quartile[, 4],
    bs.lower    = bs_envelope[1, ],
    bs.upper    = bs_envelope[2, ],
    crps        = crps_all,
    crps.q25    = crps_q[, 1],
    crps.q50    = crps_q[, 2],
    crps.q75    = crps_q[, 3],
    crps.q100   = crps_q[, 4],
    crps.lower  = crps_lower,
    crps.upper  = crps_upper
  )

  attr(gg_dta, "crps_integrated") <- brier_obj$crps
  attr(gg_dta, "cens.model")      <- cens.model
  class(gg_dta) <- c("gg_brier", class(gg_dta))
  invisible(gg_dta)
}

# Internal trapezoidal integrator: sum_i (x[i+1]-x[i]) * (y[i]+y[i+1])/2.
.trapz <- function(x, y) {
  n <- length(x)
  if (n < 2) {
    return(0)
  }
  sum((x[-1] - x[-n]) * (y[-1] + y[-n])) / 2
}
