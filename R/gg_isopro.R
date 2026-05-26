####**********************************************************************
####  gg_isopro: tidy extractor for varPro::isopro anomaly scores.
####
####  varPro::isopro returns a list with $howbad (per-observation anomaly
####  score in [0,1]) and $case.depth (average isolation depth, lower =
####  more anomalous). gg_isopro() reshapes these into a tidy data.frame
####  the plot/print/summary methods can consume.
####**********************************************************************

#' Tidy data from a varPro isolation-forest fit
#'
#' Pulls per-observation anomaly scores out of a `varPro::isopro` fit so
#' you can plot them, sort them, or write them to disk without having to
#' know the internal shape of the fit.
#'
#' @param object An `isopro` fit returned by `varPro::isopro()`.
#' @param ... Currently unused.
#'
#' @return A `data.frame` of class `c("gg_isopro", "data.frame")`, one row
#'   per observation. Columns:
#'   \describe{
#'     \item{obs}{Integer; observation index `1..n`.}
#'     \item{case.depth}{Numeric; mean isolation depth across the forest
#'       (lower = more anomalous).}
#'     \item{howbad}{Numeric in `[0, 1]`; anomaly score (higher = more
#'       anomalous). The plot method uses this as the primary axis.}
#'   }
#'   A `provenance` attribute records the source, `n`, and `ntree`.
#'
#' @details
#'   To compare methods (`"rnd"`, `"unsupv"`, `"auto"`), call `gg_isopro()`
#'   on each fit and `dplyr::bind_rows()` the results with a `method` label
#'   column. The plot method auto-detects `method` and colours the curves.
#'
#' @seealso [plot.gg_isopro()], [varPro::isopro()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   fit <- varPro::isopro(data = iris[, 1:4], method = "rnd",
#'                         sampsize = 32, ntree = 50)
#'   gg <- gg_isopro(fit)
#'   plot(gg)
#' }
#' }
#'
#' @export
gg_isopro <- function(object, ...) {
  UseMethod("gg_isopro", object)
}

#' @export
gg_isopro.isopro <- function(object, ...) {
  if (!inherits(object, "isopro")) {
    stop("gg_isopro expects a 'isopro' object from varPro::isopro().",
         call. = FALSE)
  }

  howbad <- as.numeric(object$howbad)
  depth  <- as.numeric(object$case.depth)
  n      <- length(howbad)

  gg_dta <- data.frame(
    obs        = seq_len(n),
    case.depth = depth,
    howbad     = howbad
  )

  class(gg_dta) <- c("gg_isopro", class(gg_dta))

  # isopro-specific provenance (the shared .gg_provenance helper only knows
  # about rfsrc / randomForest objects, so build the list inline).
  ntree <- tryCatch(
    as.integer(object$isoforest$ntree),
    error = function(e) NA_integer_
  )
  attr(gg_dta, "provenance") <- list(
    source = "varPro::isopro",
    n      = n,
    ntree  = if (length(ntree) == 1 && !is.na(ntree)) ntree else NA_integer_
  )

  invisible(gg_dta)
}
