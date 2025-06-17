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
#'
#' Plot a \code{\link{gg_survival}}  object.
#'
#' @param x \code{\link{gg_survival}} or a survival \code{\link{gg_rfsrc}}
#' object created from a \code{\link[randomForestSRC]{rfsrc}} object
#'
#' @param error "shade", "bars", "lines" or "none"
#' @param type "surv", "cum_haz", "hazard", "density", "mid_int", "life", "proplife"
#' @param label Modify the legend label when gg_survival has stratified samples
#' @param ... not used
#'
#' @return \code{ggplot} object
#'
#' @examples
#' ## -------- pbc data
#' data(pbc, package = "randomForestSRC")
#' pbc$time <- pbc$days / 364.25
#'
#' # This is the same as kaplan
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc
#' )
#'
#' plot(gg_dta, error = "none")
#' plot(gg_dta)
#'
#' # Stratified on treatment variable.
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc, by = "treatment"
#' )
#'
#' plot(gg_dta, error = "none")
#' plot(gg_dta)
#' plot(gg_dta, label = "treatment")
#'
#' # ...with smaller confidence limits.
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc, by = "treatment", conf.int = .68
#' )
#'
#' plot(gg_dta, error = "lines")
#' plot(gg_dta, label = "treatment", error = "lines")
#'
#' # ...with smaller confidence limits.
#' gg_dta <- gg_survival(
#'   interval = "time", censor = "status",
#'   data = pbc, by = "sex", conf.int = .68
#' )
#'
#' plot(gg_dta, error = "lines")
#' plot(gg_dta, label = "sex", error = "lines")
#'
#' @export
### Survival plots
plot.gg_survival <- function(x,
                             type = c("surv",
                                      "cum_haz",
                                      "hazard",
                                      "density",
                                      "mid_int",
                                      "life",
                                      "proplife"),
                             error = c("shade", "bars", "lines", "none"),
                             label = NULL,
                             ...) {
  gg_dta <- x
  if (inherits(gg_dta, "rfsrc")) {
    gg_dta <- gg_survival(gg_dta)
  }
  
  error <- match.arg(error)
  type <- match.arg(type)
  
  # Now order matters, so we want to place the forest predictions on the bottom
  # Create the figure skeleton,
  if (is.null(gg_dta$groups)) {
    gg_plt <- ggplot2::ggplot(gg_dta) +
      ggplot2::geom_step(ggplot2::aes(x = .data$time, y = .data[[type]]), ...)
  } else {
    gg_dta$groups <- factor(gg_dta$groups)
    gg_plt <- ggplot2::ggplot(gg_dta) +
      ggplot2::geom_step(ggplot2::aes(
        x = .data$time,
        y = .data[[type]],
        color = .data$groups
      ),
      ...)
    if (!is.null(label)) {
      gg_plt <- gg_plt +
        ggplot2::labs(color = label, fill = label)
    }
  }
  # Do we want to show confidence limits?
  if (type == "surv") {
    if (is.null(gg_dta$groups)) {
      gg_plt <- switch(
        error,
        # Shading the standard errors
        shade = gg_plt +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = .data$time,
              ymax = .data$upper,
              ymin = .data$lower
            ),
            alpha = .3
          ),
        # Or showing error bars
        bars = {
          # Need to figure out how to remove some of these points when
          # requesting error bars, or this will get really messy.
          gg_plt +
            ggplot2::geom_errorbar(ggplot2::aes(
              x = .data$time,
              ymax = .data$upper,
              ymin = .data$lower
            ))
        },
        lines = gg_plt +
          ggplot2::geom_step(ggplot2::aes(
            x = .data$time, y = .data$upper
          ), linetype = 2) +
          ggplot2::geom_step(ggplot2::aes(
            x = .data$time, y = .data$lower
          ), linetype = 2),
        none = gg_plt
      )
    } else {
      gg_plt <- switch(
        error,
        # Shading the standard errors
        shade = gg_plt +
          ggplot2::geom_ribbon(
            ggplot2::aes(
              x = .data$time,
              ymax = .data$upper,
              ymin = .data$lower,
              fill = .data$groups,
              color = .data$groups
            ),
            alpha = .3
          ),
        # Or showing error bars
        bars = {
          # Need to figure out how to remove some of these points when
          # requesting error bars, or this will get really messy.
          gg_plt +
            ggplot2::geom_errorbar(
              ggplot2::aes(
                x = .data$time,
                ymax = .data$upper,
                ymin = .data$lower,
                color = .data$groups
              )
            )
        },
        lines = gg_plt +
          ggplot2::geom_step(
            ggplot2::aes(
              x = .data$time,
              y = .data$upper,
              color = .data$groups
            ),
            linetype = 2
          ) +
          ggplot2::geom_step(
            ggplot2::aes(
              x = .data$time,
              y = .data$lower,
              color = .data$groups
            ),
            linetype = 2
          ),
        none = gg_plt
      )
    }
  }
  return(gg_plt)
}
