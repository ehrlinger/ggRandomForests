####**********************************************************************
####  autoplot.gg_* S3 methods.
####
####  Thin wrappers that delegate to the corresponding plot.gg_*() method,
####  letting users call ggplot2::autoplot() in pipe-friendly workflows
####  and inside patchwork / cowplot compositions.
####
####  Each method accepts ... and passes it straight through to plot.gg_*
####  so all existing plot arguments remain available.
####**********************************************************************

#' @importFrom ggplot2 autoplot
NULL

#' \code{autoplot} methods for \pkg{ggRandomForests} data objects
#'
#' These methods let you use \code{ggplot2::autoplot()} on any \code{gg_*}
#' object returned by \pkg{ggRandomForests}.  They are thin wrappers around
#' the corresponding \code{plot.gg_*()} S3 methods, so all arguments
#' accepted by those methods are forwarded via \code{...}.
#'
#' @param object A \code{gg_*} data object (see Details).
#' @param ... Additional arguments forwarded to the underlying
#'   \code{plot.gg_*()} method.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' The following \code{gg_*} classes are supported:
#' \describe{
#'   \item{\code{gg_error}}{OOB error vs. number of trees}
#'   \item{\code{gg_vimp}}{Variable importance ranking}
#'   \item{\code{gg_rfsrc}}{Predicted vs. observed values}
#'   \item{\code{gg_variable}}{Marginal dependence}
#'   \item{\code{gg_partial}}{Partial dependence (via \code{plot.variable})}
#'   \item{\code{gg_partial_rfsrc}}{Partial dependence (via \code{partial.rfsrc})}
#'   \item{\code{gg_partialpro}}{Partial dependence (via \code{varPro})}
#'   \item{\code{gg_roc}}{ROC curve}
#'   \item{\code{gg_survival}}{Survival / cumulative hazard curves}
#'   \item{\code{gg_brier}}{Time-resolved Brier score and CRPS}
#' }
#'
#' @name autoplot.gg
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' set.seed(42)
#' rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
#'                               ntree = 50, importance = TRUE,
#'                               tree.err = TRUE)
#' autoplot(gg_error(rf))
#' autoplot(gg_vimp(rf))
#' }
NULL

#' @rdname autoplot.gg
#' @export
autoplot.gg_error <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_vimp <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_rfsrc <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_variable <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_partial <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_partial_rfsrc <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_partialpro <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_roc <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_survival <- function(object, ...) {
  plot(object, ...)
}

#' @rdname autoplot.gg
#' @export
autoplot.gg_brier <- function(object, ...) {
  plot(object, ...)
}
