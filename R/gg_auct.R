##=============================================================================
#' Tidy time-varying AUC from a Random Hazard Forest
#'
#' Extracts the time-dependent AUC curve from [randomForestRHF::auct.rhf()]
#' into a tidy long data frame, one row per time point, with bootstrap
#' confidence bounds when available and the integrated AUC (iAUC) summary
#' attached as an attribute.
#'
#' @param object A fitted `rhf` object from \pkg{randomForestRHF}.
#' @param marker Risk marker for the AUC: `"chf"` (cumulative hazard, default)
#'   or `"haz"` (hazard). Ignored when `auct_fit` is supplied.
#' @param auct_fit Optional precomputed [randomForestRHF::auct.rhf()] result
#'   (class `"auct.rhf"`) for the same `object`. `NULL` (default) computes it.
#'   Supply it to reuse an expensive bootstrap run.
#' @param method Which time-dependent AUC definition to compute, passed to
#'   [randomForestRHF::auct.rhf()]. `"cumulative"` (default) ranks accumulated
#'   risk through a horizon; `"incident"` ranks local failures within the risk
#'   set at each time. See the note below before relying on the default.
#'   Ignored when `auct_fit` is supplied.
#' @param ... Further arguments passed to [randomForestRHF::auct.rhf()], for
#'   example `bootstrap.rep` to request confidence bounds, or `riskset` for the
#'   incident definition. Ignored when `auct_fit` is supplied.
#'
#' @return A `data.frame` of class `c("gg_auct", "data.frame")` with columns
#'   `time`, `auc`, `se`, `lower`, `upper`, `marker` (CI columns `NA` when no
#'   bootstrap), an `iauc` attribute (a list with `uno`, `std`, `uno.se`,
#'   `std.se`, `conf.level`), and a `provenance` attribute derived from
#'   `object` (source, family, ntree, n).
#'
#' @references
#' Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
#' arXiv:2608.21597. \doi{10.48550/arXiv.2608.21597}.
#'
#' Ishwaran H, Kogalur UB (2026). \emph{randomForestRHF: Random Hazard
#' Forests}. R package version 2.0.0.
#' \url{https://CRAN.R-project.org/package=randomForestRHF}.
#'
#' @note
#' Cumulative/dynamic AUC is unreliable under \pkg{randomForestRHF} 2.0.0, so
#' treat the `method = "cumulative"` default with care. That release holds the
#' in-sample cumulative hazard flat once a subject's supplied records end,
#' which `?randomForestRHF::rhf` documents. At a fixed grid point the marker
#' then reflects how long a subject was observed as well as how much risk they
#' carried, and the cumulative/dynamic definition compares subjects who have
#' already failed against subjects still under follow-up. The curve can fall
#' below the 0.5 chance line on data the forest fits well.
#'
#' The incident/dynamic definition does not inherit this, because it compares
#' subjects within a risk set at each time, before any of them has left
#' follow-up. It answers a different question rather than a better version of
#' the same one, so reach for `method = "incident"` where that question is the
#' one you are asking. The behavior is upstream, reported at
#' \url{https://github.com/kogalur/randomForestRHF/issues/1}; `gg_auct()`
#' passes the values through unchanged in every case.
#'
#' @seealso [plot.gg_auct()], [randomForestRHF::auct.rhf()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf("Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   plot(gg_auct(o, marker = "chf"))
#' }
#' }
#'
#' @export
gg_auct <- function(object, ...) {
  UseMethod("gg_auct", object)
}

#' @rdname gg_auct
#' @export
gg_auct.rhf <- function(object, marker = c("chf", "haz"), auct_fit = NULL,
                        method = c("cumulative", "incident"), ...) {
  marker <- match.arg(marker)
  method <- match.arg(method)

  if (is.null(auct_fit)) {
    if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
      stop("Install the 'randomForestRHF' package to use gg_auct(): ",
           "install.packages('randomForestRHF')", call. = FALSE)
    }
    auct_fit <- randomForestRHF::auct.rhf(object, marker = marker,
                                          method = method, ...)
  }
  if (!inherits(auct_fit, "auct.rhf")) {
    stop("auct_fit must be an 'auct.rhf' object from ",
         "randomForestRHF::auct.rhf().", call. = FALSE)
  }

  abt  <- auct_fit$AUC.by.time
  boot <- auct_fit$boot

  gg_dta <- data.frame(
    time   = abt$time,
    auc    = abt$AUC,
    se     = if (!is.null(boot)) boot$AUC.se    else NA_real_,
    lower  = if (!is.null(boot)) boot$AUC.lower else NA_real_,
    upper  = if (!is.null(boot)) boot$AUC.upper else NA_real_,
    marker = auct_fit$marker,
    stringsAsFactors = FALSE
  )

  attr(gg_dta, "iauc") <- list(
    uno        = auct_fit$iAUC.uno,
    std        = auct_fit$iAUC.std,
    uno.se     = if (!is.null(boot)) boot$iAUC.uno.se else NA_real_,
    std.se     = if (!is.null(boot)) boot$iAUC.std.se else NA_real_,
    conf.level = if (!is.null(boot)) boot$conf.level  else NA_real_
  )
  class(gg_dta) <- c("gg_auct", class(gg_dta))
  gg_dta <- .set_provenance(gg_dta, object)
  invisible(gg_dta)
}
