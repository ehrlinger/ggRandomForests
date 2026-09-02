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
#'   or `"haz"` (hazard). Not used when `auct_fit` is supplied, though the
#'   value is still validated.
#' @param auct_fit Optional precomputed [randomForestRHF::auct.rhf()] result
#'   (class `"auct.rhf"`) for the same `object`. `NULL` (default) computes it.
#'   Supply it to reuse an expensive bootstrap run.
#' @param method Which time-dependent AUC definition to compute, passed to
#'   [randomForestRHF::auct.rhf()]. `"cumulative"` (default) ranks accumulated
#'   risk through a horizon; `"incident"` ranks local failures within the risk
#'   set at each time. See the note below on choosing between them. Not
#'   used when `auct_fit` is supplied, though the value is still validated.
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
#' Forests}. R package version 2.0.3.
#' \url{https://CRAN.R-project.org/package=randomForestRHF}.
#'
#' @note
#' The two definitions answer different questions rather than better and worse
#' versions of the same one. Cumulative/dynamic AUC ranks accumulated risk
#' through a horizon, comparing subjects who have failed by that horizon
#' against subjects still event-free at it. Incident/dynamic AUC ranks local
#' failures within the risk set at each time. Pick the one that matches the
#' question you are asking, and read the two curves as separate estimands
#' rather than as a check on each other.
#'
#' Cumulative/dynamic AUC was unreliable under \pkg{randomForestRHF} 2.0.0,
#' which could push the curve below the 0.5 chance line on data the forest
#' fits well. That was an upstream problem, fixed in 2.0.3. R does not enforce
#' a `Suggests` version at run time, so `gg_auct()` checks the installed
#' version itself and errors rather than compute a cumulative/dynamic curve it
#' knows to be wrong. The check applies only when `gg_auct()` does the
#' computation: `method = "incident"` is unaffected by the upstream problem and
#' is never gated, and a supplied `auct_fit` is taken as given, since an
#' `auct.rhf` object records no version and may have been read from a file.
#' `gg_auct()` passes the values through unchanged in every case.
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

# Cumulative/dynamic AUC is wrong before randomForestRHF 2.0.3: that release
# corrected auct.rhf() to reconstruct common-time markers from the retained
# tree-level arrays. Before it, the in-sample cumulative hazard was held flat
# after follow-up, so the marker tracked observation length as well as risk and
# the curve could sit below the chance line on data the forest fits well.
#
# DESCRIPTION asks for >= 2.0.3 in Suggests, but R does not enforce a Suggests
# version at run time, so a session carrying an older build would otherwise get
# the inverted curve with no warning.
#
# Only the cumulative definition is gated. Incident/dynamic compares subjects
# within a risk set at t, before any has left follow-up, and is unaffected
# (measured identical at 0.531 across 2.0.0 and 2.0.3).
#
# `version` is a parameter rather than read here so the guard is testable
# without a downgraded install.
.stop_if_auct_cumulative_unsupported <- function(method, version) {
  if (identical(method, "cumulative") &&
        version < package_version("2.0.3")) {
    stop("gg_auct(method = \"cumulative\") needs randomForestRHF >= 2.0.3. ",
         "Version ", version, " is installed, and returns an inverted ",
         "cumulative/dynamic AUC. Update randomForestRHF, or use ",
         "method = \"incident\", which is unaffected.", call. = FALSE)
  }
  invisible(NULL)
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
    .stop_if_auct_cumulative_unsupported(
      method, utils::packageVersion("randomForestRHF")
    )
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
