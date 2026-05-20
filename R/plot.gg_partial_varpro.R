##=============================================================================
#' Plot a \code{\link{gg_partial_varpro}} object
#'
#' Produces ggplot2 partial dependence curves from the named list returned
#' by \code{\link{gg_partial_varpro}}.  Continuous predictors are shown as
#' overlaid line curves (one per effect type); categorical predictors as
#' side-by-side boxplots.  For survival path-C objects (produced when
#' \code{scale \%in\% c("surv","chf")} is passed to the extractor) the plot
#' is delegated to \code{\link{plot.gg_partial_rfsrc}}.
#'
#' @param x A \code{\link{gg_partial_varpro}} object.
#' @param type Character vector; one or more of \code{"parametric"},
#'   \code{"nonparametric"}, \code{"causal"}.  Defaults to all three.
#'   Ignored for path-C objects.
#' @param ... Not currently used for path-A objects; forwarded to
#'   \code{plot.gg_partial_rfsrc} for path-C objects.
#'
#' @details
#' **Ensemble mortality (scale = "mortality"):** When the provenance scale
#' is \code{"mortality"}, the y-axis label reads
#' \emph{"Ensemble mortality (expected events)"} to make clear that this
#' is an \strong{unbounded relative-risk score}, not a survival probability
#' or \eqn{1 - S(t)} (Ishwaran, Kogalur, Blackstone & Lauer, 2008
#' <doi:10.1214/08-AOAS169>).
#'
#' @return A \code{ggplot} (or \code{patchwork}) object.
#'
#' @references
#' Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS (2008).
#' Random survival forests. \emph{The Annals of Applied Statistics},
#' \bold{2}(3), 841--860. \doi{10.1214/08-AOAS169}.
#'
#' @seealso \code{\link{gg_partial_varpro}}
#'
#' @examples
#' set.seed(42)
#' n_obs <- 30; n_pts <- 15
#' mock_data <- list(
#'   age = list(
#'     xvirtual    = seq(30, 80, length.out = n_pts),
#'     xorg        = sample(seq(30, 80, by = 5), n_obs, replace = TRUE),
#'     yhat.par    = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
#'     yhat.nonpar = matrix(rnorm(n_obs * n_pts), nrow = n_obs),
#'     yhat.causal = matrix(rnorm(n_obs * n_pts), nrow = n_obs)
#'   ),
#'   sex = list(
#'     xvirtual    = c(0, 1),
#'     xorg        = sample(c(0, 1), n_obs, replace = TRUE),
#'     yhat.par    = matrix(rnorm(n_obs * 2), nrow = n_obs),
#'     yhat.nonpar = matrix(rnorm(n_obs * 2), nrow = n_obs),
#'     yhat.causal = matrix(rnorm(n_obs * 2), nrow = n_obs)
#'   )
#' )
#' pp <- gg_partial_varpro(mock_data)
#' plot(pp)
#' plot(pp, type = "parametric")
#'
#' @importFrom ggplot2 .data
#' @importFrom patchwork wrap_plots
#' @export
plot.gg_partial_varpro <- function(x,
                                    type = c("parametric", "nonparametric",
                                             "causal"),
                                    ...) {
  ## C-path: delegate to plot.gg_partial_rfsrc via NextMethod().
  prov <- attr(x, "provenance")
  if (!is.null(prov) && identical(prov$path, "C")) {
    return(NextMethod())
  }

  ## A-path rendering.
  type   <- match.arg(type, several.ok = TRUE)
  ylabel <- .partial_varpro_ylabel(prov)

  gg_cont <- NULL
  if (!is.null(x$continuous) && nrow(x$continuous) > 0) {
    cont_long <- tidyr::pivot_longer(
      x$continuous,
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
      ggplot2::labs(x = NULL, y = ylabel,
                    color = "Effect type", linetype = "Effect type")
  }

  gg_cat <- NULL
  if (!is.null(x$categorical) && nrow(x$categorical) > 0) {
    cat_long <- tidyr::pivot_longer(
      x$categorical,
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
      ggplot2::labs(x = NULL, y = ylabel, fill = "Effect type")
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    patchwork::wrap_plots(gg_cont, gg_cat, ncol = 1)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}

## ---------------------------------------------------------------------------
## Internal: build honest y-axis label from provenance.
#' @keywords internal
.partial_varpro_ylabel <- function(prov) {
  if (is.null(prov)) return("Partial Effect")
  scale <- prov$scale %||% "generic"
  switch(scale,
    mortality = "Ensemble mortality (expected events)",
    rmst      = {
      tau <- prov$rmst_tau
      if (!is.null(tau) && !is.na(tau)) {
        sprintf("RMST (τ = %g)", tau)
      } else {
        "RMST"
      }
    },
    surv      = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) {
        sprintf("Survival probability at t = %g", t)
      } else {
        "Survival probability"
      }
    },
    chf       = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) {
        sprintf("Cumulative hazard at t = %g", t)
      } else {
        "Cumulative hazard"
      }
    },
    "Partial Effect"   # generic / auto-regr / auto-class / unknown
  )
}
