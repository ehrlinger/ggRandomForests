####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************

#' Plot a \code{\link{gg_ale_rfsrc}} object
#'
#' Renders first-order Accumulated Local Effects as a ggplot2 figure.
#' Continuous predictors are drawn as line plots and categorical predictors
#' as bar charts, both faceted by variable name -- the same arrangement as
#' \code{\link{plot.gg_partial_rfsrc}}, so the two are directly comparable
#' side by side.
#'
#' @param x A \code{\link{gg_ale_rfsrc}} object.
#' @param labels Optional variable labels for the facet strips. One of: a
#'   named character vector (\code{c(bpd_last = "BP Diastole")}); a labelled
#'   data frame, whose \code{attr(col, "label")} values are read; or a
#'   two-column \code{key}/\code{label} data frame. Variables with no label
#'   keep their raw name. Defaults to \code{NULL} (raw names).
#' @param ... Not currently used.
#'
#' @return A \code{ggplot} (or \code{patchwork}) object. When both
#'   continuous and categorical variables are present the two panels are
#'   combined vertically via \code{patchwork::wrap_plots()}.
#'
#' @seealso \code{\link{gg_ale_rfsrc}}, \code{\link{plot.gg_partial_rfsrc}}
#'
#' @examples
#' airq.obj <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
#'                                     ntree = 50)
#' ale_dta <- gg_ale_rfsrc(airq.obj, xvar.names = c("Wind", "Temp"),
#'                          n_eval = 10)
#' plot(ale_dta)
#'
#' @importFrom ggplot2 .data
#' @importFrom patchwork wrap_plots
#' @export
plot.gg_ale_rfsrc <- function(x, labels = NULL, ...) {
  gg_dta <- x
  strip_labeller <- .forest_strip_labeller(labels)

  gg_cont <- NULL
  if (!is.null(gg_dta$continuous) && nrow(gg_dta$continuous) > 0) {
    cont <- gg_dta$continuous
    gg_cont <- ggplot2::ggplot(cont,
                               ggplot2::aes(x = .data$x, y = .data$yhat)) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
      ggplot2::labs(x = NULL, y = "Accumulated Local Effect")
  }

  gg_cat <- NULL
  if (!is.null(gg_dta$categorical) && nrow(gg_dta$categorical) > 0) {
    cat_dta <- gg_dta$categorical
    gg_cat <- ggplot2::ggplot(
      cat_dta,
      ggplot2::aes(x = .data$x, y = .data$yhat)
    ) +
      ggplot2::geom_bar(stat = "identity", width = 0.5) +
      ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
      ggplot2::labs(x = NULL, y = "Accumulated Local Effect")
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    wrap_plots(gg_cont, gg_cat, ncol = 1)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}

#' Plot a \code{\link{gg_ale_rfsrc}} interaction object
#'
#' Renders the second-order (interaction) ALE surface from
#' \code{\link{gg_ale_rfsrc}} (when called with \code{xvar2.name}) as a
#' ggplot2 heatmap. Values near zero mean the two predictors act
#' additively over that region of their joint range; large positive or
#' negative values mark where their combined effect departs from the sum
#' of their individual main effects.
#'
#' @param x A \code{gg_ale_interaction} object.
#' @param ... Not currently used.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{gg_ale_rfsrc}}, \code{\link{plot.gg_ale_rfsrc}}
#'
#' @examples
#' \donttest{
#' airq.obj <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
#'                                     ntree = 50)
#' ale_int <- gg_ale_rfsrc(airq.obj, xvar.names = "Wind",
#'                          xvar2.name = "Temp", n_eval = 12)
#' plot(ale_int)
#' }
#'
#' @importFrom ggplot2 .data
#' @export
plot.gg_ale_interaction <- function(x, ...) {
  name1 <- x$name1[1]
  name2 <- x$name2[1]

  ## geom_tile(), not geom_raster(): the ALE grid is quantile-based, so its
  ## cells are unevenly spaced by construction. geom_raster() assumes a regular
  ## grid and silently shifts the cells onto one, which moves the interaction
  ## away from the predictor values it belongs to.
  ggplot2::ggplot(x, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$ale)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white",
                                  high = "#B2182B", midpoint = 0) +
    ggplot2::labs(x = name1, y = name2, fill = "Interaction\nALE")
}
