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
                         xvar = NULL, ...) {
  type <- match.arg(type)
  switch(type,
         beeswarm   = shap_beeswarm(x, ...),
         importance = shap_importance(x, ...),
         dependence = shap_dependence(x, xvar = xvar, ...))
}

#' SHAP global importance bar chart
#'
#' Bar chart of mean absolute SHAP value per variable -- the SHAP analog of
#' \code{\link{plot.gg_vimp}}.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gg_shap}} \code{\link{plot.gg_shap}}
#' @export
shap_importance <- function(x, ...) {
  imp <- dplyr::summarise(dplyr::group_by(x, .data$vars),
                          mean_abs = mean(abs(.data$shap)), .groups = "drop")
  ggplot2::ggplot(imp) +
    ggplot2::geom_bar(
      ggplot2::aes(x = .data$vars, y = .data$mean_abs),
      stat = "identity", width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "", y = "mean(|SHAP|)")
}

#' SHAP beeswarm summary plot
#'
#' The signature SHAP summary: one jittered point per (observation, variable),
#' positioned by SHAP value and colored by the (scaled) feature value.
#' Categorical features have no numeric value and render uncolored.
#'
#' @param x A \code{\link{gg_shap}} object.
#' @param ... Unused.
#'
#' @return A \code{ggplot} object.
#' @seealso \code{\link{gg_shap}} \code{\link{plot.gg_shap}}
#' @export
shap_beeswarm <- function(x, ...) {
  ggplot2::ggplot(x, ggplot2::aes(x = .data$shap, y = .data$vars)) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = "grey60") +
    ggplot2::geom_jitter(ggplot2::aes(colour = .data$value),
                         height = 0.2, width = 0, alpha = 0.6) +
    ggplot2::scale_colour_viridis_c(name = "Feature value") +
    ggplot2::labs(x = "SHAP value (impact on prediction)", y = "")
}
