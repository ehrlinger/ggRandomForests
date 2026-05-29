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
#' @param ... Not currently used.
#'
#' @return A `data.frame` of class `c("gg_auct", "data.frame")` with columns
#'   `time`, `auc`, `se`, `lower`, `upper`, `marker` (CI columns `NA` when no
#'   bootstrap), an `iauc` attribute (a list with `uno`, `std`, `uno.se`,
#'   `std.se`, `conf.level`), and a `provenance` attribute derived from
#'   `object` (source, family, ntree, n).
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
gg_auct.rhf <- function(object, marker = c("chf", "haz"), auct_fit = NULL, ...) {
  marker <- match.arg(marker)

  if (is.null(auct_fit)) {
    if (!requireNamespace("randomForestRHF", quietly = TRUE)) {
      stop("Install the 'randomForestRHF' package to use gg_auct(): ",
           "install.packages('randomForestRHF')", call. = FALSE)
    }
    auct_fit <- randomForestRHF::auct.rhf(object, marker = marker)
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
