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

# Map partial.type ("surv" / "chf" / "mort") to a human y-axis label.
# Falls back to "Predicted Survival" when the attribute is absent (e.g. an
# object built before this attribute was introduced).
partial_surv_y_label <- function(partial.type) {
  if (is.null(partial.type)) return("Predicted Survival")
  switch(partial.type,
         surv = "Predicted Survival",
         chf  = "Predicted CHF",
         mort = "Predicted Mortality",
         "Predicted Survival")
}

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
#' For survival forests (when a \code{time} column is present): each evaluation
#' time point is a separate curve over the predictor's value, faceted by
#' variable name. The y-axis label adapts to the \code{partial.type} stored on
#' the object (\dQuote{Predicted Survival}, \dQuote{Predicted CHF}, or
#' \dQuote{Predicted Mortality}).
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
      ## Survival forest: predictor value on x-axis, one curve per time point.
      ## Group/colour by the *full-precision* time so distinct horizons that
      ## happen to round to the same value are not silently merged. The legend
      ## is relabelled with rounded values for readability.
      time_levels <- sort(unique(cont$time))
      cont$.time_factor <- factor(cont$time, levels = time_levels)
      legend_labels <- format(round(time_levels, 2), trim = TRUE)
      y_lab <- partial_surv_y_label(attr(gg_dta, "partial.type"))
      gg_cont <- ggplot2::ggplot(
        cont,
        ggplot2::aes(
          x     = .data$x,
          y     = .data$yhat,
          color = .data$.time_factor,
          group = .data$.time_factor
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~name, scales = "free_x") +
        ggplot2::scale_color_discrete(labels = legend_labels) +
        ggplot2::labs(x = NULL, y = y_lab, color = "Time")

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
