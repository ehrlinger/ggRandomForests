####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Plot a \code{\link{gg_partial}} object
#'
#' Produces ggplot2 partial dependence curves from the named list returned by
#' \code{\link{gg_partial}}.  Continuous predictors are shown as line plots;
#' categorical predictors are shown as bar charts.  Both panels are faceted by
#' variable name so multiple predictors can be compared at a glance.
#'
#' @param x A \code{\link{gg_partial}} object (output of \code{\link{gg_partial}}).
#' @param ... Not currently used; reserved for future arguments.
#'
#' @return When only continuous or only categorical variables are present, a
#'   single \code{ggplot} object.  When both are present, a named list with
#'   elements \code{continuous} and \code{categorical}, each a \code{ggplot}.
#'
#' @seealso \code{\link{gg_partial}}, \code{\link{plot.gg_variable}}
#'
#' @examples
#' set.seed(42)
#' airq <- na.omit(airquality)
#' rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
#' pv <- randomForestSRC::plot.variable(rf, partial = TRUE, show.plots = FALSE)
#' pd <- gg_partial(pv)
#' plot(pd)
#'
#' @importFrom ggplot2 .data
#' @export
plot.gg_partial <- function(x, ...) {
  gg_dta <- x

  gg_cont <- NULL
  if (!is.null(gg_dta$continuous) && nrow(gg_dta$continuous) > 0) {
    cont <- gg_dta$continuous
    gg_cont <- ggplot2::ggplot(cont,
                               ggplot2::aes(x = .data$x, y = .data$yhat)) +
      ggplot2::geom_line()

    if ("model" %in% colnames(cont)) {
      gg_cont <- gg_cont +
        ggplot2::aes(color = .data$model, group = .data$model)
    }

    gg_cont <- gg_cont +
      ggplot2::facet_wrap(~name, scales = "free_x") +
      ggplot2::labs(x = NULL, y = "Partial Effect")
  }

  gg_cat <- NULL
  if (!is.null(gg_dta$categorical) && nrow(gg_dta$categorical) > 0) {
    cat_dta <- gg_dta$categorical
    gg_cat <- ggplot2::ggplot(cat_dta,
                              ggplot2::aes(x = .data$x, y = .data$yhat)) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::facet_wrap(~name, scales = "free_x") +
      ggplot2::labs(x = NULL, y = "Partial Effect")
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    list(continuous = gg_cont, categorical = gg_cat)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}

#' Plot a \code{\link{gg_partial_rfsrc}} object
#'
#' Produces ggplot2 partial dependence curves from the named list returned by
#' \code{\link{gg_partial_rfsrc}}.
#'
#' For standard (non-survival) forests: continuous predictors are line plots,
#' categorical predictors are bar charts, both faceted by variable name.
#'
#' For survival forests (when a \code{time} column is present): each predictor
#' value is a separate curve over time, faceted by variable name.
#'
#' For two-variable surface plots (when a \code{grp} column is present):
#' each group level is a separate line, faceted by primary predictor name.
#'
#' @param x A \code{\link{gg_partial_rfsrc}} object.
#' @param ... Not currently used.
#'
#' @return A single \code{ggplot} object, or a named list with elements
#'   \code{continuous} and \code{categorical} when both types are present.
#'
#' @seealso \code{\link{gg_partial_rfsrc}}, \code{\link{plot.gg_partial}}
#'
#' @importFrom ggplot2 .data
#' @export
plot.gg_partial_rfsrc <- function(x, ...) {
  gg_dta <- x

  gg_cont <- NULL
  if (!is.null(gg_dta$continuous) && nrow(gg_dta$continuous) > 0) {
    cont <- gg_dta$continuous

    if (!is.null(cont$time)) {
      ## Survival forest: predictor value is the grouping variable; x-axis is time
      gg_cont <- ggplot2::ggplot(
        cont,
        ggplot2::aes(
          x     = .data$time,
          y     = .data$yhat,
          color = factor(.data$x),
          group = factor(.data$x)
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~name, scales = "free") +
        ggplot2::labs(x = "Time", y = "Partial Effect", color = "Predictor value")

    } else if (!is.null(cont$grp)) {
      ## Two-variable surface: group is xvar2; x-axis is the primary predictor
      gg_cont <- ggplot2::ggplot(
        cont,
        ggplot2::aes(
          x     = .data$x,
          y     = .data$yhat,
          color = factor(.data$grp),
          group = factor(.data$grp)
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~name, scales = "free_x") +
        ggplot2::labs(x = NULL, y = "Partial Effect", color = "Group")

    } else {
      ## Standard: one curve per variable
      gg_cont <- ggplot2::ggplot(cont,
                                 ggplot2::aes(x = .data$x, y = .data$yhat)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~name, scales = "free_x") +
        ggplot2::labs(x = NULL, y = "Partial Effect")
    }
  }

  gg_cat <- NULL
  if (!is.null(gg_dta$categorical) && nrow(gg_dta$categorical) > 0) {
    cat_dta <- gg_dta$categorical
    gg_cat <- ggplot2::ggplot(
      cat_dta,
      ggplot2::aes(x = factor(.data$x), y = .data$yhat)
    ) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::facet_wrap(~name, scales = "free_x") +
      ggplot2::labs(x = NULL, y = "Partial Effect")
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    list(continuous = gg_cont, categorical = gg_cat)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}

#' Plot a \code{\link{gg_partialpro}} object
#'
#' Produces ggplot2 partial dependence curves from the named list returned by
#' \code{\link{gg_partialpro}}, which wraps \code{varpro::partialpro} output.
#'
#' Each variable produces up to three effect curves: parametric, nonparametric,
#' and causal.  The \code{type} argument controls which are shown.
#'
#' @param x A \code{\link{gg_partialpro}} object.
#' @param type Character vector; one or more of \code{"parametric"},
#'   \code{"nonparametric"}, \code{"causal"}.  Defaults to all three.
#' @param ... Not currently used.
#'
#' @return A single \code{ggplot} or a named list with \code{continuous} and
#'   \code{categorical} elements when both types of predictors are present.
#'
#' @seealso \code{\link{gg_partialpro}}
#'
#' @importFrom ggplot2 .data
#' @export
plot.gg_partialpro <- function(x,
                               type = c("parametric", "nonparametric", "causal"),
                               ...) {
  gg_dta <- x
  type <- match.arg(type, several.ok = TRUE)

  gg_cont <- NULL
  if (!is.null(gg_dta$continuous) && nrow(gg_dta$continuous) > 0) {
    cont <- gg_dta$continuous
    ## Pivot to long form so all requested effect types appear as one colour/line
    cont_long <- tidyr::pivot_longer(
      cont,
      cols      = tidyr::all_of(type),
      names_to  = "effect_type",
      values_to = "yhat"
    )
    gg_cont <- ggplot2::ggplot(
      cont_long,
      ggplot2::aes(
        x        = .data$variable,
        y        = .data$yhat,
        color    = .data$effect_type,
        linetype = .data$effect_type
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~name, scales = "free_x") +
      ggplot2::labs(x = NULL, y = "Partial Effect",
                    color = "Effect type", linetype = "Effect type")
  }

  gg_cat <- NULL
  if (!is.null(gg_dta$categorical) && nrow(gg_dta$categorical) > 0) {
    cat_dta <- gg_dta$categorical
    cat_long <- tidyr::pivot_longer(
      cat_dta,
      cols      = tidyr::all_of(type),
      names_to  = "effect_type",
      values_to = "yhat"
    )
    gg_cat <- ggplot2::ggplot(
      cat_long,
      ggplot2::aes(
        x    = factor(.data$variable),
        y    = .data$yhat,
        fill = .data$effect_type
      )
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~name, scales = "free_x") +
      ggplot2::labs(x = NULL, y = "Partial Effect", fill = "Effect type")
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    list(continuous = gg_cont, categorical = gg_cat)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}
