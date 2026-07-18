##=============================================================================
#' Plot a \code{\link{gg_partial_varpro}} object
#'
#' Draws the partial dependence curves from the list that
#' \code{\link{gg_partial_varpro}} returns.  Continuous predictors get
#' overlaid line curves, one per effect type; categorical predictors get
#' side-by-side boxplots.  Survival path-C objects (the ones you get when
#' \code{scale \%in\% c("surv","chf")} was passed to the extractor) are
#' handed off to \code{\link{plot.gg_partial_rfsrc}} for drawing.
#'
#' @section Reading the partial dependence:
#' For a continuous variable the x-axis is the variable's grid of values
#' and the y-axis is the partial prediction; each of the three effect
#' types (\code{parametric}, \code{nonparametric}, \code{causal}) is
#' drawn as its own line. The shape of the line is the story: a clear
#' slope says the model uses the variable, a flat line says it
#' essentially does not, and a U-shape or a threshold says the effect
#' is nonlinear in a way a single coefficient would miss. For a
#' categorical variable the picture is a boxplot per level; here the
#' eye is looking at level-to-level shifts in the center of each box.
#'
#' Where the three effect types track each other, the parametric story
#' is a fair summary of what the forest is doing. Where they fan
#' apart (typically the parametric curve smoother than the
#' nonparametric, or the causal curve flatter than either) the
#' variable is one to inspect more carefully before reading a single
#' effect off the plot.
#'
#' @section What this tells you:
#' Use these curves to describe how the model uses each variable, not
#' to claim how the world works. They are a window into the fitted
#' relationship; they do not by themselves establish that intervening
#' on the variable would move the outcome. For survival path-C
#' (\code{scale = "chf"}), the y-axis is on the cumulative-hazard scale.
#'
#' @section Reading a probability curve (scale = "prob"):
#' The y-axis is \eqn{P(Y = \mathrm{target})}, the model's predicted probability
#' of the target class as the focal variable varies (others held at their
#' UVT-plausible average).  \code{"odds"} and \code{"logodds"} are the same
#' curve on the odds and log-odds scales.  The \code{causal} curve is a
#' contrast (below) and is \emph{not} shown on \code{"prob"}/\code{"odds"};
#' use \code{"logodds"} to see it.
#'
#' @section Reading a survival-probability curve (scale = "surv"):
#' The y-axis is \eqn{S(\tau \mid x)}, the predicted probability of surviving
#' past \eqn{\tau}, bounded in \eqn{[0, 1]} and read in the model's time units.
#' Higher is better (more survival).  \eqn{\tau} defaults to the median
#' follow-up time when not supplied.
#'
#' @section What the causal curve is, and when to use it:
#' \code{causal} is the \strong{baseline-subtracted local effect} -- varPro's
#' virtual- ("digital-") twins estimator (Ishwaran & Blackstone, 2025).  It
#' shows how the prediction shifts as the focal variable moves away from the
#' reference grid point, with the other covariates held at on-manifold
#' (UVT-plausible) values; it is a \strong{contrast} (it starts at 0), not a
#' level.  Use it when you want the local effect (change-from-baseline) rather
#' than the absolute predicted level, and as a cross-check on the parametric
#' and nonparametric curves.  It is varpro's local estimator \emph{within the
#' fitted model}, \strong{not a structural causal claim} about the
#' data-generating process.  Because it is a contrast it cannot share a
#' probability/odds axis with the absolute curves, so it is shown only on the
#' additive scales (\code{"logodds"}, \code{"mortality"}, \code{"rmst"}).
#'
#' @section Reading an RMST curve (scale = "rmst"):
#' The y-axis is restricted mean survival time at horizon \eqn{\tau},
#' \eqn{\mathrm{RMST}(\tau)=\int_0^\tau S(t)\,dt}: the \strong{expected
#' event-free time during the first \eqn{\tau} time-units}, the area under the
#' survival curve out to \eqn{\tau}. Read it in the \strong{model's own time
#' units}, where it is bounded by \eqn{0 \le \mathrm{RMST}(\tau) \le \tau}.
#'
#' Two things follow. First, \eqn{\tau} must be given in the fit's time units;
#' a \eqn{\tau} past the largest event time just truncates to the full
#' restricted mean and stops changing. Second, higher is better here -- more
#' time event-free -- which is the opposite of the ensemble-mortality scale.
#'
#' A continuous variable's curve sloping \emph{up} means higher values of that
#' covariate buy you \emph{more} restricted-mean event-free time within \eqn{\tau}
#' (with the other covariates held at their UVT-plausible average); a flat curve
#' means the covariate does not move it. Unlike ensemble mortality, RMST reads
#' on a directly clinical scale, "so many event-free time-units within
#' \eqn{\tau}", which is usually the one you want to report.
#'
#' @param x A \code{\link{gg_partial_varpro}} object.
#' @param type Character vector; one or more of \code{"parametric"},
#'   \code{"nonparametric"}, \code{"causal"}.  Defaults to all three.
#'   Ignored for path-C objects.
#' @param ... Unused for path-A objects; forwarded to
#'   \code{plot.gg_partial_rfsrc} for path-C objects.
#'
#' @details
#' **Ensemble mortality (scale = "mortality"):** when the provenance scale
#' is \code{"mortality"}, the y-axis is labeled
#' \emph{"Ensemble mortality (expected events)"}.  The wording is
#' deliberate: this is an \strong{unbounded relative-risk score}, not a
#' survival probability and not \eqn{1 - S(t)} (Ishwaran, Kogalur,
#' Blackstone & Lauer, 2008 <doi:10.1214/08-AOAS169>).
#'
#' @return A \code{ggplot} (or \code{patchwork}) object.
#'
#' @references
#' Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS (2008).
#' Random survival forests. \emph{The Annals of Applied Statistics},
#' \bold{2}(3), 841--860. \doi{10.1214/08-AOAS169}.
#'
#' Ishwaran H, Blackstone EH (2025).
#' Harnessing the power of virtual (digital) twins: Graphical causal tools for
#' understanding patient and hospital differences.
#' \emph{Computational and Structural Biotechnology Journal}, \bold{28}, 312.
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
#' @importFrom ggplot2 .data ggplot aes geom_line geom_boxplot facet_wrap labs
#' @importFrom tidyr pivot_longer all_of
#' @importFrom patchwork wrap_plots
#' @name plot.gg_partial_varpro
#' @export
plot.gg_partial_varpro <- function(x,
                                    type = c("parametric", "nonparametric",
                                             "causal"),
                                    ...) {
  type_user <- !missing(type)   # was 'causal' asked for, or is it the default?

  ## C-path: delegate to plot.gg_partial_rfsrc via NextMethod().
  prov <- attr(x, "provenance")
  if (!is.null(prov) && identical(prov$path, "C")) {
    return(NextMethod())
  }

  ## A-path rendering.
  type   <- match.arg(type, several.ok = TRUE)
  ylabel <- .partial_varpro_ylabel(prov)

  ## On bounded scales (prob/odds/surv) the causal contrast is not shown.
  type <- .partial_varpro_plot_type(type, type_user, prov)

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

## Drop the `causal` contrast on bounded scales (prob/odds/surv) -- it cannot
## share the level axis. Warn only when the user explicitly asked for it; fall
## back to the level curves if causal was the only requested type.
#' @keywords internal
.partial_varpro_plot_type <- function(type, type_user, prov) {
  if (is.null(prov) || !.is_bounded_scale(prov$scale %||% "generic"))
    return(type)
  if (type_user && "causal" %in% type) {
    warning("plot.gg_partial_varpro: 'causal' is not shown on the ",
            prov$scale, " scale (it is a contrast, not a level). ",
            "Use scale = 'logodds' (classification) or 'mortality'/'rmst' ",
            "(survival) to see it.", call. = FALSE)
  }
  type <- setdiff(type, "causal")
  if (length(type) == 0L) c("parametric", "nonparametric") else type
}

## ---------------------------------------------------------------------------
## Internal: build honest y-axis label from provenance.
#' @keywords internal
.partial_varpro_ylabel <- function(prov) {
  if (is.null(prov)) return("Partial Effect")
  scale <- prov$scale %||% "generic"
  tgt <- prov$target
  has_tgt <- !is.null(tgt) && !is.na(tgt)
  switch(scale,
    prob      = if (has_tgt) sprintf("P(Y = %s)", tgt) else "Probability",
    odds      = if (has_tgt) sprintf("Odds(Y = %s)", tgt) else "Odds",
    logodds   = if (has_tgt) sprintf("Log-odds(Y = %s)", tgt) else "Log-odds",
    mortality = "Ensemble mortality (expected events)",
    rmst      = {
      tau <- prov$rmst_tau
      if (!is.null(tau) && !is.na(tau)) sprintf("RMST (\u03c4 = %g)", tau)
      else "RMST"
    },
    surv      = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) sprintf("Survival probability at t = %g", t)
      else "Survival probability"
    },
    chf       = {
      t <- prov$rmst_tau
      if (!is.null(t) && !is.na(t)) sprintf("Cumulative hazard at t = %g", t)
      else "Cumulative hazard"
    },
    "Partial Effect"   # generic / regr / unknown
  )
}
