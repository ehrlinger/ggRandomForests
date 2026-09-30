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
#' Turns a \code{\link{gg_partial}} object into a ggplot2 figure.  Each curve
#' is a partial dependence trace, the forest's average prediction as one
#' predictor is swept across its range while the rest are marginalized over the
#' training data.  Continuous predictors appear as line plots.  Categorical
#' predictors appear as box plots, one box per level, drawn from the
#' per-observation predictions that \code{plot.variable()} returns for them, so
#' the box shows how the prediction varies across the training data at that
#' level.  Both panels are faceted by variable name
#' so you can compare the shape and scale of each variable's effect at a
#' glance.
#'
#' When a \code{model} label was attached in \code{gg_partial()}, lines are
#' colored by model, which is handy for overlaying results from two forests (e.g.,
#' one tuned, one default) in the same figure.
#'
#' @param x A \code{\link{gg_partial}} object (output of \code{\link{gg_partial}}).
#' @param labels Optional variable labels for the facet strips.  One of: a named
#'   character vector (\code{c(bpd_last = "BP Diastole")}); a labelled data frame,
#'   whose \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame.  Variables with no label keep their raw
#'   name.  Defaults to \code{NULL} (raw names).
#' @param ... Not currently used; reserved for future arguments.
#'
#' @return A \code{ggplot} (or \code{patchwork}) object.  When only one
#'   variable type is present a single \code{ggplot} is returned.  When both
#'   continuous and categorical variables are present the two panels are
#'   combined vertically via \code{patchwork::wrap_plots()}, which also
#'   satisfies \code{inherits(p, "ggplot")}.
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
#' @importFrom patchwork wrap_plots
#' @export
plot.gg_partial <- function(x, labels = NULL, ...) {
  gg_dta <- x

  ## plot.variable() records what the partial yhat actually is ("mortality",
  ## "predicted survival (time=...)", "probability setosa", expression(hat(y))).
  ## Prefer it over the generic label so mortality is never mistaken for a
  ## probability.
  y_lab <- attr(gg_dta, "ylabel")
  if (is.null(y_lab)) {
    y_lab <- "Partial Effect"
  }

  ## Labels are a presentation concern: resolved here and applied to the facet
  ## strips, never written back into x.  The returned object keeps raw variable
  ## names, because changing them would be a breaking change downstream.
  strip_labeller <- .forest_strip_labeller(labels)

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
      ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
      ggplot2::labs(x = NULL, y = y_lab)
  }

  gg_cat <- NULL
  if (!is.null(gg_dta$categorical) && nrow(gg_dta$categorical) > 0) {
    ## The categorical frame carries one prediction per observation per level,
    ## so the panel shows their spread.  A bar with stat = "identity" stacks
    ## them, and the axis then reads as their sum.
    cat_dta <- gg_dta$categorical
    gg_cat <- ggplot2::ggplot(
      cat_dta,
      ggplot2::aes(x = factor(.data$x), y = .data$yhat)
    ) +
      ggplot2::geom_boxplot()

    if ("model" %in% colnames(cat_dta)) {
      ## factor(): a numeric model label would otherwise be a continuous fill,
      ## which does not split the boxes.
      gg_cat <- gg_cat +
        ggplot2::aes(fill = factor(.data$model)) +
        ggplot2::labs(fill = "model")
    }

    gg_cat <- gg_cat +
      ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
      ggplot2::labs(x = NULL, y = y_lab)
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    wrap_plots(gg_cont, gg_cat, ncol = 1)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}

#' Plot a \code{\link{gg_partial_rfsrc}} object
#'
#' Renders the partial dependence curves from \code{\link{gg_partial_rfsrc}}
#' as a ggplot2 figure.  The layout adapts automatically to what the object
#' contains.
#'
#' For a standard regression or classification forest, continuous predictors
#' are drawn as line plots and categorical predictors as box plots, both
#' faceted by variable name, the same arrangement as
#' \code{\link{plot.gg_partial}}.  The categorical data hold one prediction
#' per training observation per level, not their average, so each box shows
#' the spread of the prediction at that level and its middle line the median.
#' For a survival forest the boxes are filled by time horizon, and with
#' \code{xvar2.name} by the level of the second variable.  When a survival
#' forest has both, the fill is the time horizon and each level of the second
#' variable gets its own panel.
#'
#' For a survival forest, each call to \code{partial.rfsrc} returns a predicted
#' quantity (survival probability, cumulative hazard function, or mortality) at
#' one or more chosen time horizons.  When a \code{time} column is present in
#' the data, each horizon becomes a separate colored curve over the predictor's
#' value, still faceted by variable.  The y-axis label (\dQuote{Predicted
#' Survival}, \dQuote{Predicted CHF}, or \dQuote{Predicted Mortality}) tracks
#' the \code{partial.type} attribute set by \code{gg_partial_rfsrc()}.
#'
#' For a two-variable interaction surface (when \code{xvar2.name} was supplied
#' to \code{gg_partial_rfsrc}), the secondary variable's levels become
#' separate colored lines, faceted by the primary predictor.
#'
#' @param x A \code{\link{gg_partial_rfsrc}} object.
#' @param labels Optional variable labels for the facet strips.  One of: a named
#'   character vector (\code{c(bpd_last = "BP Diastole")}); a labelled data frame,
#'   whose \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame.  Variables with no label keep their raw
#'   name.  Defaults to \code{NULL} (raw names).
#' @param ... Not currently used.
#'
#' @return A \code{ggplot} (or \code{patchwork}) object.  When both continuous
#'   and categorical variables are present the two panels are combined
#'   vertically via \code{patchwork::wrap_plots()}.
#'
#' @seealso \code{\link{gg_partial_rfsrc}}, \code{\link{plot.gg_partial}}
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## Regression forest -- one continuous curve per variable
#' ## ------------------------------------------------------------
#' set.seed(42)
#' airq <- na.omit(airquality)
#' rfsrc_airq <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
#'
#' pd <- gg_partial_rfsrc(rfsrc_airq, xvar.names = c("Wind", "Temp"),
#'                        n_eval = 10)
#' plot(pd)
#'
#' \donttest{
#' ## ------------------------------------------------------------
#' ## Survival forest -- one curve per requested time horizon,
#' ## faceted by variable. Y-axis label tracks `partial.type`.
#' ## ------------------------------------------------------------
#' # randomForestSRC's formula parser requires the unqualified Surv() symbol;
#' # it Depends on `survival`, so Surv is on the search path once
#' # randomForestSRC is loaded.
#' data(veteran, package = "randomForestSRC")
#' set.seed(42)
#' rfsrc_v <- randomForestSRC::rfsrc(Surv(time, status) ~ .,
#'                                   data = veteran, ntree = 50)
#' ti  <- rfsrc_v$time.interest
#' t30 <- ti[which.min(abs(ti - 30))]
#' t90 <- ti[which.min(abs(ti - 90))]
#'
#' # Default partial.type = "surv" -> y-axis "Predicted Survival"
#' pd_s <- gg_partial_rfsrc(rfsrc_v, xvar.names = "age",
#'                          partial.time = c(t30, t90), n_eval = 8)
#' plot(pd_s)
#'
#' # partial.type = "chf" -> y-axis "Predicted CHF"
#' pd_c <- gg_partial_rfsrc(rfsrc_v, xvar.names = "age",
#'                          partial.time = c(t30, t90),
#'                          partial.type = "chf", n_eval = 8)
#' plot(pd_c)
#' }
#'
#' @importFrom ggplot2 .data
#' @importFrom patchwork wrap_plots
#' @export
plot.gg_partial_rfsrc <- function(x, labels = NULL, ...) {
  gg_dta <- x

  ## Labels are a presentation concern: resolved here and applied to the facet
  ## strips, never written back into x.  The returned object keeps raw variable
  ## names, because changing them would be a breaking change downstream.
  strip_labeller <- .forest_strip_labeller(labels)

  gg_cont <- NULL
  if (!is.null(gg_dta$continuous) && nrow(gg_dta$continuous) > 0) {
    cont <- gg_dta$continuous

    if (!is.null(cont$time)) {
      ## Survival forest: predictor value on x-axis, one curve per time point.
      ## Group/color by the *full-precision* time so distinct horizons that
      ## happen to round to the same value are not silently merged. The legend
      ## is relabeled with rounded values for readability.
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
        ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
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
        ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
        ggplot2::labs(x = NULL, y = "Partial Effect", color = "Group")

    } else {
      ## Standard: one curve per variable
      gg_cont <- ggplot2::ggplot(cont,
                                 ggplot2::aes(x = .data$x, y = .data$yhat)) +
        ggplot2::geom_line() +
        ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
        ggplot2::labs(x = NULL, y = "Partial Effect")
    }
  }

  gg_cat <- NULL
  if (!is.null(gg_dta$categorical) && nrow(gg_dta$categorical) > 0) {
    ## The categorical frame carries one prediction per observation per level,
    ## so the panel shows their spread.  A bar with stat = "identity" stacks
    ## them (and every time horizon with them), and the axis then reads as
    ## their sum.
    cat_dta <- gg_dta$categorical
    cat_y_lab <- "Partial Effect"
    by_time <- !is.null(cat_dta$time)
    if (by_time) {
      time_levels <- sort(unique(cat_dta$time))
      cat_dta$.time_factor <- factor(cat_dta$time, levels = time_levels)
    }
    gg_cat <- ggplot2::ggplot(
      cat_dta,
      ggplot2::aes(x = factor(.data$x), y = .data$yhat)
    ) +
      ggplot2::geom_boxplot()

    if (by_time) {
      ## Survival forest: one box per level per time point, as the continuous
      ## panel draws one curve per time point.
      cat_y_lab <- partial_surv_y_label(attr(gg_dta, "partial.type"))
      gg_cat <- gg_cat +
        ggplot2::aes(fill = .data$.time_factor) +
        ggplot2::scale_fill_discrete(
          labels = format(round(time_levels, 2), trim = TRUE)
        ) +
        ggplot2::labs(fill = "Time")
    } else if (!is.null(cat_dta$grp)) {
      gg_cat <- gg_cat +
        ggplot2::aes(fill = factor(.data$grp)) +
        ggplot2::labs(fill = "Group")
    }

    if (by_time && !is.null(cat_dta$grp)) {
      ## Survival forest with xvar2.name: the fill is taken by the time point,
      ## so each level of the second variable gets its own panel.  Without
      ## this the boxes would pool every level of it.
      gg_cat <- gg_cat +
        ggplot2::facet_wrap(
          ~name + grp, scales = "free_x",
          labeller = ggplot2::labeller(
            name = strip_labeller,
            grp  = function(val) paste("Group", val)
          )
        )
    } else {
      gg_cat <- gg_cat +
        ggplot2::facet_wrap(~name, scales = "free_x",
                            labeller = strip_labeller)
    }
    gg_cat <- gg_cat + ggplot2::labs(x = NULL, y = cat_y_lab)
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    wrap_plots(gg_cont, gg_cat, ncol = 1)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}

#' @rdname plot.gg_partial_varpro
#' @name plot.gg_partial_varpro
#' @export
plot.gg_partialpro <- function(x, type = c("parametric", "nonparametric",
                                            "causal"), labels = NULL, ...) {
  ## Deprecated class shim: re-dispatch to plot.gg_partial_varpro.
  class(x) <- c("gg_partial_varpro", setdiff(class(x), "gg_partialpro"))
  plot.gg_partial_varpro(x, type = type, labels = labels, ...)
}
