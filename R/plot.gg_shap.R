#' Plot a \code{\link{gg_shap}} object
#'
#' Routes to one of three SHAP views. \code{type = "beeswarm"} (default) draws
#' the signature SHAP summary; \code{"importance"} draws a mean-absolute-SHAP
#' bar chart; \code{"dependence"} draws SHAP value against a single feature's
#' value.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param type One of \code{"beeswarm"}, \code{"importance"}, or
#'   \code{"dependence"}.
#' @param xvar For \code{type = "dependence"}, the variable to plot. When
#'   \code{NULL}, the top-ranked variable is used.
#' @param labels Optional variable labels, forwarded to the mode function. One
#'   of: a named character vector (`c(Temp = "Temperature")`); a labelled data
#'   frame, whose \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame. Variables with no label keep their raw
#'   name. In \code{"dependence"} mode the label is substituted into the axis
#'   titles; \code{xvar} still matches on raw names.
#' @param ... Passed to the underlying builder.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{gg_shap}} \code{\link{shap_importance}}
#'
#' @examples
#' \donttest{
#' if (requireNamespace("kernelshap", quietly = TRUE)) {
#'   rf <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
#'                                ntree = 50)
#'   gg_dta <- gg_shap(rf, bg_n = 20)
#'   plot(gg_dta, type = "importance")
#' }
#' }
#'
#' @export
plot.gg_shap <- function(x, type = c("beeswarm", "importance", "dependence"),
                         xvar = NULL, labels = NULL, ...) {
  type <- match.arg(type)
  switch(type,
         beeswarm   = shap_beeswarm(x, labels = labels, ...),
         importance = shap_importance(x, labels = labels, ...),
         dependence = shap_dependence(x, xvar = xvar, labels = labels, ...))
}

#' SHAP global importance bar chart
#'
#' Bar chart of mean absolute SHAP value per variable -- the SHAP analog of
#' \code{\link{plot.gg_vimp}}.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param labels Optional variable labels. One of: a named character vector
#'   (`c(Temp = "Temperature")`); a labelled data frame, whose
#'   \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame. Variables with no label keep their raw
#'   name. Defaults to \code{NULL} (raw names).
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gg_shap}} \code{\link{plot.gg_shap}}
#' @export
shap_importance <- function(x, labels = NULL, ...) {
  imp <- dplyr::summarise(dplyr::group_by(x, .data$vars),
                          mean_abs = mean(abs(.data$shap)), .groups = "drop")
  gg_plt <- ggplot2::ggplot(imp) +
    ggplot2::geom_bar(
      ggplot2::aes(x = .data$vars, y = .data$mean_abs),
      stat = "identity", width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "mean(|SHAP|)")

  # coord_flip(), so the variable axis is a flipped x, as in plot.gg_vimp().
  lab_lookup <- .forest_labels(labels)
  if (!is.null(lab_lookup)) {
    gg_plt <- gg_plt + ggplot2::scale_x_discrete(
      labels = function(v) .apply_forest_labels(v, lab_lookup)
    )
  }
  gg_plt
}

#' SHAP beeswarm summary plot
#'
#' The signature SHAP summary: one jittered point per (observation, variable),
#' positioned by SHAP value and colored by the feature value, min-max scaled
#' to \code{[0, 1]} within each variable so every variable's own range maps
#' to the full color gradient. Categorical features have no numeric value and
#' render as a neutral gray (no numeric value to scale).
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param labels Optional variable labels. One of: a named character vector
#'   (`c(Temp = "Temperature")`); a labelled data frame, whose
#'   \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame. Variables with no label keep their raw
#'   name. Defaults to \code{NULL} (raw names).
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gg_shap}} \code{\link{plot.gg_shap}}
#' @export
shap_beeswarm <- function(x, labels = NULL, ...) {
  x <- dplyr::mutate(dplyr::group_by(x, .data$vars),
                     value_scaled = {
                       finite_vals <- .data$value[is.finite(.data$value)]
                       if (length(finite_vals) == 0) {
                         rep(NA_real_, dplyr::n())
                       } else {
                         rng <- range(finite_vals)
                         if (rng[1] == rng[2]) {
                           rep(0.5, dplyr::n())
                         } else {
                           scaled <- (.data$value - rng[1]) / (rng[2] - rng[1])
                           pmin(pmax(scaled, 0), 1)
                         }
                       }
                     })
  x <- dplyr::ungroup(x)

  gg_plt <- ggplot2::ggplot(x, ggplot2::aes(x = .data$shap, y = .data$vars)) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, color = "gray60") +
    ggplot2::geom_jitter(ggplot2::aes(color = .data$value_scaled),
                         height = 0.2, width = 0, alpha = 0.6) +
    ggplot2::scale_color_viridis_c(name = "Feature value",
                                    breaks = c(0, 1),
                                    labels = c("Low", "High")) +
    ggplot2::labs(x = "SHAP value (impact on prediction)", y = "")

  # No coord_flip() here: vars are mapped to y directly, so the labelled scale
  # is scale_y_discrete().
  lab_lookup <- .forest_labels(labels)
  if (!is.null(lab_lookup)) {
    gg_plt <- gg_plt + ggplot2::scale_y_discrete(
      labels = function(v) .apply_forest_labels(v, lab_lookup)
    )
  }
  gg_plt
}

#' SHAP dependence plot
#'
#' SHAP value against the value of a single feature — the SHAP analog of a
#' partial-dependence plot. Numeric features use a continuous x-axis; factor
#' or character features fall back to their labels on a discrete axis.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param xvar The variable to plot. When \code{NULL}, the top-ranked variable
#'   (largest mean absolute SHAP) is used.
#' @param labels Optional variable labels. One of: a named character vector
#'   (`c(Temp = "Temperature")`); a labelled data frame, whose
#'   \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame. Variables with no label keep their raw
#'   name. Defaults to \code{NULL} (raw names).
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gg_shap}} \code{\link{plot.gg_shap}}
#' @export
shap_dependence <- function(x, xvar = NULL, labels = NULL, ...) {
  # vars levels are reversed (most important last); top variable is the last
  # level.
  if (is.null(xvar)) {
    xvar <- utils::tail(levels(x$vars), 1)
  }
  if (!xvar %in% levels(x$vars)) {
    stop("shap_dependence: '", xvar, "' is not a variable in this gg_shap ",
         "object.", call. = FALSE)
  }

  sub <- x[as.character(x$vars) == xvar, , drop = FALSE]
  is_numeric_feature <- any(!is.na(sub$value))

  # There is no discrete variable scale in this mode: the variable name appears
  # in the axis titles.  Resolve xvar against the raw names first (above), then
  # substitute the display label, so 'labels' stays display-only.
  xvar_display <- .apply_forest_labels(xvar, .forest_labels(labels))

  gg_plt <- ggplot2::ggplot(sub) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "gray60") +
    ggplot2::labs(x = xvar_display,
                  y = paste("SHAP value for", xvar_display))

  if (is_numeric_feature) {
    gg_plt + ggplot2::geom_point(
      ggplot2::aes(x = .data$value, y = .data$shap), alpha = 0.6)
  } else {
    gg_plt + ggplot2::geom_boxplot(
      ggplot2::aes(x = .data$value_label, y = .data$shap))
  }
}
