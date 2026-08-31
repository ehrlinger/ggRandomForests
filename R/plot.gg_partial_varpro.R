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
#' @param labels Optional variable labels for the facet strips.  One of: a named
#'   character vector (\code{c(bpd_last = "BP Diastole")}); a labelled data frame,
#'   whose \code{attr(col, "label")} values are read; or a two-column
#'   \code{key}/\code{label} data frame.  Variables with no label keep their raw
#'   name.  Defaults to \code{NULL} (raw names).
#' @param ... Unused for path-A objects; forwarded to
#'   \code{plot.gg_partial_rfsrc} for path-C objects.
#' @param which Character; which frame to draw.  \code{"both"} (default) keeps
#'   the historical behaviour, returning a \pkg{patchwork} stack when the object
#'   carries both continuous and categorical variables.  \code{"continuous"} or
#'   \code{"categorical"} returns a bare \code{ggplot} for that frame alone.
#'   Use it when you want to add scales or themes with \code{+}: on a patchwork
#'   \code{+} reaches only the last panel, so \code{which} is how you get one
#'   plot to modify.  Implied by \code{panels}.
#' @param panels Optional data frame giving per-panel scales, one row per panel.
#'   Supplying it selects the continuous frame and switches rendering from
#'   \code{facet_wrap()} to \pkg{patchwork}, which is the only way to vary the
#'   x scale between panels.  Only \code{name} is required; every other column
#'   is optional and an absent one leaves that decision to \pkg{ggplot2}.
#'   \describe{
#'     \item{name}{variable, matched against \code{x$continuous$name}.  A name
#'       the frame does not carry is an error rather than a silent drop.}
#'     \item{xlab}{panel x axis title.  Falls back to the \code{labels} value
#'       for that variable, then to \code{name}.}
#'     \item{xmin, xmax}{clipped range, applied with \code{coord_cartesian()}
#'       so points outside are hidden rather than dropped from the smooth.
#'       Supplying both also removes the axis padding.}
#'     \item{xby}{tick spacing, expanded to \code{seq(xmin, xmax, xby)}.}
#'     \item{span}{per-panel \code{geom_smooth()} span.}
#'   }
#'   Panels are drawn in row order, so the frame pins panel order explicitly.
#'   A variable absent from \code{panels} is not drawn; selecting a subset is
#'   the usual reason to supply it.
#'
#'   The y axis is shared across panels, matching the facet route, and is
#'   computed over the selected variables only -- so dropping a variable
#'   rescales the figure. Only the x scale varies between panels; four partial
#'   dependence curves on four different y ranges would not compare.
#' @param points Logical; add the grid-point values as points.  Default
#'   \code{FALSE}.
#' @param smooth Logical; draw a \code{geom_smooth()} loess instead of the
#'   line.  Default \code{FALSE}.  Note that \code{parametric} is already
#'   partialpro's local-polynomial fit, so smoothing it is a smooth of a smooth;
#'   this is here for the raw-looking figure some journals ask for, not as a
#'   better estimate.
#' @param palette Character; the effect-type colour or fill scale.  Defaults to
#'   \code{"black"}: this package's figures are made for manuscripts, and
#'   \code{linetype} is mapped to the effect type as well, so the three
#'   estimators stay legible as solid, dotted and dashed with no colour at all.
#'   \code{"mono"} is a synonym, and \code{"grey"} / \code{"gray"} give a
#'   flat grey.  Any ColorBrewer palette name (e.g. \code{"Set1"}) routes to
#'   the brewer scale instead, which is worth reaching for when you are
#'   comparing two or three estimators on screen and colour separates them
#'   faster than a dash pattern.  \code{NULL} keeps \pkg{ggplot2}'s own
#'   scale.
#' @param ncol Integer; columns in the \code{panels} layout.  \code{NULL}
#'   (default) lets \pkg{patchwork} choose.
#' @param point_size,point_alpha Numeric; size and alpha of the points drawn
#'   when \code{points = TRUE}.  Defaults \code{1.1} and \code{0.55}.  Print
#'   figures usually want a smaller point than a screen figure.
#' @param linewidth Numeric; width of the line or smooth.  Default \code{0.5},
#'   \pkg{ggplot2}'s own.
#' @param complement Logical; plot \eqn{1 - p} instead of \eqn{p}, and prefix
#'   the y axis label with \code{"1 - "}.  Use it when the fit targets the
#'   class you do \emph{not} want on the axis -- a model of weaning failure
#'   read as the probability of weaning success, say -- so you do not have to
#'   recompute \code{\link[varPro]{partialpro}} against the other target.
#'   Requires a probability scale (\code{scale = "prob"} or \code{"surv"});
#'   on the additive, multiplicative and unbounded scales \eqn{1 - x} has no
#'   referent and this is an error rather than a silent no-op.  Default
#'   \code{FALSE}.
#' @param ylim Numeric length-2; the shared y range for every panel.
#'   \code{NULL} (default) takes the range of the plotted values, which is what
#'   the facet route has always done.  Supply it to pin a scale that means
#'   something independent of the data -- \code{c(0, 1)} on a probability
#'   scale, say, so a flat curve reads as flat rather than filling the panel.
#'   It cannot be set from outside: on the \code{panels} route a
#'   \code{coord_cartesian()} added with \code{&} replaces the per-panel
#'   coordinate system and takes the per-panel x ranges down with it, and
#'   \code{scale_y_continuous(limits = )} is overridden by that coordinate
#'   system.
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
#' pp <- gg_partial_varpro(mock_data, scale = "logodds")
#' plot(pp)
#' plot(pp, type = "parametric")
#'
#' ## The continuous frame alone, so `+` reaches the plot you meant.
#' plot(pp, which = "continuous") + ggplot2::labs(title = "Continuous only")
#'
#' ## Per-panel scales.  Only 'name' is required; the rest tune one axis each.
#' spec <- data.frame(name = "age", xlab = "Age (years)",
#'                    xmin = 30, xmax = 80, xby = 10, span = 0.6)
#' plot(pp, type = "parametric", panels = spec, points = TRUE, smooth = TRUE)
#'
#' ## Colour earns its place when several estimators share a panel: two curves
#' ## separate faster by hue than by dash pattern.  Any ColorBrewer name works.
#' plot(pp, type = c("parametric", "nonparametric"), palette = "Set1")
#'
#' ## The default is monochrome, for print.
#' plot(pp, type = c("parametric", "nonparametric"))
#'
#' ## A patchwork takes `&`, not `+`, to reach every panel.
#' plot(pp, type = "parametric", panels = spec) & ggplot2::theme_minimal()
#'
#' ## complement = TRUE reads a failure model as its success probability.
#' pp_prob <- gg_partial_varpro(mock_data, scale = "prob")
#' plot(pp_prob, type = "parametric", complement = TRUE)
#'
#' @importFrom ggplot2 .data ggplot aes geom_line geom_boxplot facet_wrap labs
#' @importFrom ggplot2 geom_point geom_smooth scale_x_continuous waiver
#' @importFrom ggplot2 coord_cartesian scale_color_brewer scale_fill_brewer
#' @importFrom ggplot2 scale_color_manual scale_fill_manual
#' @importFrom tidyr pivot_longer all_of
#' @importFrom patchwork wrap_plots
#' @name plot.gg_partial_varpro
#' @export
plot.gg_partial_varpro <- function(x, # nolint: cyclocomp_linter
                                    type = c("parametric", "nonparametric",
                                             "causal"),
                                    labels = NULL,
                                    ...,
                                    ## AFTER the dots deliberately.  R
                                    ## partial-matches argument names only
                                    ## BEFORE ..., so a caller writing
                                    ## 'pan = ' or 'point = ' binds exactly
                                    ## or falls into ... rather than silently
                                    ## capturing a neighbouring formal.
                                    which   = c("both", "continuous",
                                                "categorical"),
                                    panels  = NULL,
                                    points  = FALSE,
                                    smooth  = FALSE,
                                    palette = "black",
                                    ncol    = NULL,
                                    point_size  = 1.1,
                                    point_alpha = 0.55,
                                    linewidth   = 0.5,
                                    complement  = FALSE,
                                    ylim        = NULL) {
  type_user <- !missing(type)   # was 'causal' asked for, or is it the default?

  ## C-path: delegate to plot.gg_partial_rfsrc via NextMethod().
  prov <- attr(x, "provenance")
  if (!is.null(prov) && identical(prov$path, "C")) {
    return(NextMethod())
  }

  ## A-path rendering.  '...' means nothing here, so name what was dropped
  ## rather than discarding it in silence (path C returned above, and does
  ## use its dots).
  .warn_partial_varpro_dots(list(...))

  type   <- match.arg(type, several.ok = TRUE)
  which  <- match.arg(which)
  .check_complement(complement, prov)
  ylabel <- .partial_varpro_ylabel(prov)
  if (isTRUE(complement)) ylabel <- paste("1 -", ylabel)

  ## 'panels' is continuous-only vocabulary (xmin/xby/span describe a numeric
  ## axis), so supplying it selects the continuous frame.
  if (!is.null(panels)) {
    which <- "continuous"
  }

  ## Labels are a presentation concern: resolved here and applied to the facet
  ## strips, never written back into x.  The returned object keeps raw variable
  ## names, because changing them would be a breaking change downstream.
  strip_labeller <- .forest_strip_labeller(labels)

  ## On bounded scales (prob/odds/surv) the causal contrast is not shown.
  type <- .partial_varpro_plot_type(type, type_user, prov)

  want_cont <- which %in% c("both", "continuous")
  want_cat  <- which %in% c("both", "categorical")

  gg_cont <- NULL
  if (want_cont && !is.null(x$continuous) && nrow(x$continuous) > 0) {
    cont_long <- tidyr::pivot_longer(
      x$continuous,
      cols      = tidyr::all_of(type),
      names_to  = "effect_type",
      values_to = "yhat"
    )
    if (isTRUE(complement)) cont_long$yhat <- 1 - cont_long$yhat

    if (is.null(panels)) {
      gg_cont <- .partial_varpro_faceted(cont_long, ylabel, strip_labeller,
                                         points, smooth, palette,
                                         point_size, point_alpha, linewidth,
                                         ylim)
    } else {
      spec    <- .check_partial_panels(panels, x$continuous)
      lookup  <- .forest_labels(labels)
      ## Shared y across panels.  facet_wrap(scales = "free_x") frees only x, so
      ## the facet route has always given a common y; patchwork panels would
      ## each compute their own, and four partial dependence curves on four
      ## different y ranges do not compare.  Computed over the SELECTED
      ## variables only, so dropping a variable rescales the figure.
      if (is.null(ylim)) {
        sel  <- cont_long[as.character(cont_long$name) %in% spec$name, ,
                          drop = FALSE]
        ylim <- range(sel$yhat, na.rm = TRUE, finite = TRUE)
        if (!all(is.finite(ylim))) ylim <- NULL
      }
      built   <- lapply(seq_len(nrow(spec)), function(i) {
        .partial_varpro_panel(spec[i, , drop = FALSE], cont_long, ylabel,
                              lookup, points, smooth, palette,
                              point_size, point_alpha, linewidth, ylim)
      })
      return(patchwork::wrap_plots(built, ncol = ncol) +
               patchwork::plot_layout(axis_titles = "collect"))
    }
  }

  gg_cat <- NULL
  if (want_cat && !is.null(x$categorical) && nrow(x$categorical) > 0) {
    cat_long <- tidyr::pivot_longer(
      x$categorical,
      cols      = tidyr::all_of(type),
      names_to  = "effect_type",
      values_to = "yhat"
    )
    if (isTRUE(complement)) cat_long$yhat <- 1 - cat_long$yhat
    gg_cat <- ggplot2::ggplot(
      cat_long,
      ggplot2::aes(
        x    = factor(.data$variable),
        y    = .data$yhat,
        fill = .data$effect_type
      )
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
      ggplot2::labs(x = NULL, y = ylabel, fill = "Effect type")
    gg_cat <- gg_cat + .partial_varpro_fill_scale(palette)
  }

  if (!is.null(gg_cont) && !is.null(gg_cat)) {
    patchwork::wrap_plots(gg_cont, gg_cat, ncol = 1)
  } else if (!is.null(gg_cont)) {
    gg_cont
  } else {
    gg_cat
  }
}

## The default continuous rendering: one facet per variable, free x RANGE but a
## single shared x SCALE.  Unchanged from 4.0.0-rc3 when points/smooth/palette
## are left at their defaults.
#' @keywords internal
.partial_varpro_faceted <- function(cont_long, ylabel, strip_labeller,
                                    points, smooth, palette,
                                    point_size, point_alpha, linewidth,
                                    ylim = NULL) {
  p <- ggplot2::ggplot(
    cont_long,
    ggplot2::aes(
      x        = .data$variable,
      y        = .data$yhat,
      color    = .data$effect_type,
      linetype = .data$effect_type
    )
  ) +
    .partial_varpro_marks(points, smooth, span = NA_real_,
                          point_size, point_alpha, linewidth) +
    ggplot2::facet_wrap(~name, scales = "free_x", labeller = strip_labeller) +
    ggplot2::labs(x = NULL, y = ylabel,
                  color = "Effect type", linetype = "Effect type")
  p <- p + .partial_varpro_colour_scale(palette)
  if (!is.null(ylim)) {
    p <- p + ggplot2::coord_cartesian(ylim = ylim)
  }
  p
}

## Mark grammar, shared by both routes.  'smooth' REPLACES the line rather than
## adding to it: a loess over partialpro's already-local-polynomial 'parametric'
## column would otherwise be a smooth of a smooth.
#' @keywords internal
.partial_varpro_marks <- function(points, smooth, span,
                                  point_size, point_alpha, linewidth) {
  out <- list()
  if (isTRUE(points)) {
    out <- c(out, list(ggplot2::geom_point(alpha = point_alpha,
                                           size  = point_size)))
  }
  if (isTRUE(smooth)) {
    args <- list(se = FALSE, linewidth = linewidth)
    if (!is.na(span)) args$span <- span
    out <- c(out, list(do.call(ggplot2::geom_smooth, args)))
  } else {
    out <- c(out, list(ggplot2::geom_line(linewidth = linewidth)))
  }
  out
}

## Validate the per-panel spec frame and fill its optional columns with NA so
## the builder can test them uniformly.  A name the continuous frame does not
## carry is an error: silently dropping it is the defect class this package
## spent 2026-08-29 removing.
#' @keywords internal
.check_partial_panels <- function(panels, cont) {
  if (!is.data.frame(panels)) {
    stop("'panels' must be a data frame with a 'name' column.", call. = FALSE)
  }
  if (!"name" %in% names(panels)) {
    stop("'panels' must have a 'name' column naming the variables to draw.",
         call. = FALSE)
  }
  if (nrow(panels) == 0L) {
    stop("'panels' has no rows; supply one row per panel.", call. = FALSE)
  }
  panels <- as.data.frame(panels, stringsAsFactors = FALSE)
  panels$name <- as.character(panels$name)

  absent <- setdiff(panels$name, as.character(cont$name))
  if (length(absent) > 0L) {
    stop("'panels' names ", length(absent),
         " variable(s) absent from the continuous frame: ",
         paste(absent, collapse = ", "), ".", call. = FALSE)
  }

  for (nm in c("xmin", "xmax", "xby", "span")) {
    if (is.null(panels[[nm]])) panels[[nm]] <- NA_real_
    panels[[nm]] <- .panels_numeric_col(panels[[nm]], nm)
  }
  if (is.null(panels[["xlab"]])) panels[["xlab"]] <- NA_character_
  panels[["xlab"]] <- as.character(panels[["xlab"]])
  panels
}

## Build one panel of the patchwork route.  Every scale column is optional; an
## absent one leaves that decision to ggplot2, which is what the facet does.
#' @keywords internal
.partial_varpro_panel <- function(spec, cont_long, ylabel, lookup,
                                  points, smooth, palette,
                                  point_size, point_alpha, linewidth,
                                  ylim = NULL) {
  dta <- cont_long[as.character(cont_long$name) == spec$name, , drop = FALSE]

  xlab <- if (!is.na(spec$xlab)) {
    spec$xlab
  } else if (!is.null(lookup) && spec$name %in% names(lookup)) {
    lookup[[spec$name]]
  } else {
    spec$name
  }

  p <- ggplot2::ggplot(
    dta,
    ggplot2::aes(
      x        = .data$variable,
      y        = .data$yhat,
      color    = .data$effect_type,
      linetype = .data$effect_type
    )
  ) +
    .partial_varpro_marks(points, smooth, span = spec$span,
                          point_size, point_alpha, linewidth) +
    ggplot2::labs(x = xlab, y = ylabel,
                  color = "Effect type", linetype = "Effect type")

  ## An explicit range means the caller is taking control of the axis, so the
  ## padding goes too -- that is what makes the panels butt against the axes.
  xlim <- NULL
  if (!is.na(spec$xmin) && !is.na(spec$xmax)) {
    brks <- if (!is.na(spec$xby)) {
      seq(spec$xmin, spec$xmax, by = spec$xby)
    } else {
      ggplot2::waiver()
    }
    p <- p + ggplot2::scale_x_continuous(breaks = brks, expand = c(0, 0))
    xlim <- c(spec$xmin, spec$xmax)
  }
  ## coord_cartesian() rather than scale limits: it clips the VIEW, so a point
  ## outside the range still informs the smooth instead of being dropped.
  p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)

  p + .partial_varpro_colour_scale(palette)
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

## Path A ignores '...'.  Accepting an argument and discarding it without a word
## is the defect class this package removed from plot.gg_variable() on
## 2026-08-31 (see R/plot.gg_variable.R): the caller cannot tell a typo, a
## retired name, or an argument from a newer version from one that worked.
#' @keywords internal
.warn_partial_varpro_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(invisible(NULL))
  }
  nms <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))
  shown <- ifelse(nzchar(nms), nms, "<unnamed>")
  warning("plot.gg_partial_varpro: ", length(dots),
          " argument(s) in '...' are not used by this method and were ",
          "ignored: ", paste(shown, collapse = ", "),
          ". This method's own arguments sit after '...' and so match by ",
          "exact name; check the spelling, or check that the installed ",
          "version has the argument you meant.", call. = FALSE)
  invisible(NULL)
}

## 1 - p is only meaningful where p is a probability.  On the additive
## (logodds), multiplicative (odds) and unbounded (mortality, rmst, chf) scales
## it is arithmetic without a referent, so this errors rather than quietly
## producing a plot nobody should read.
#' @keywords internal
.check_complement <- function(complement, prov) {
  if (!isTRUE(complement)) {
    return(invisible(NULL))
  }
  scale <- if (is.null(prov)) "generic" else (prov$scale %||% "generic")
  if (!scale %in% c("prob", "surv")) {
    stop("plot.gg_partial_varpro: 'complement' needs a probability scale, ",
         "but this object is on the '", scale, "' scale. Re-extract with ",
         "gg_partial_varpro(scale = \"prob\") for a classification fit, or ",
         "scale = \"surv\" for a survival fit.", call. = FALSE)
  }
  invisible(NULL)
}

## Resolve the 'palette' argument to a scale.  A ColorBrewer name goes to the
## brewer scale; the monochrome keywords resolve to a flat manual scale, since
## print journals routinely ask for a figure with no colour in it and there is
## no brewer ramp that means "all black".  linetype is already mapped to
## effect_type, so a monochrome figure still separates the estimators.
#' @keywords internal
.partial_varpro_mono <- function(palette) {
  if (is.null(palette) || length(palette) != 1L || is.na(palette)) return(NULL)
  switch(tolower(as.character(palette)),
    black = "black", mono = "black",
    grey = "grey30", gray = "grey30",
    NULL)
}

#' @keywords internal
.partial_varpro_colour_scale <- function(palette) {
  if (is.null(palette)) {
    return(NULL)
  }
  mono <- .partial_varpro_mono(palette)
  if (!is.null(mono)) {
    ## Three, because there are at most three effect types.
    return(ggplot2::scale_color_manual(values = rep(mono, 3L),
                                       guide = "none"))
  }
  ggplot2::scale_color_brewer(palette = palette)
}

#' @keywords internal
.partial_varpro_fill_scale <- function(palette) {
  if (is.null(palette)) {
    return(NULL)
  }
  mono <- .partial_varpro_mono(palette)
  if (!is.null(mono)) {
    return(ggplot2::scale_fill_manual(values = rep(mono, 3L)))
  }
  ggplot2::scale_fill_brewer(palette = palette)
}

## Coerce one scale column to numeric.  as.numeric() on a factor returns the
## integer CODES, not the labels, so a factor xmin of c(0, 20) silently becomes
## c(1, 2) and the panel is drawn on an axis nobody asked for.  Go through
## as.character() first, and make junk an error rather than a silent NA.
#' @keywords internal
.panels_numeric_col <- function(x, nm) {
  if (is.factor(x)) x <- as.character(x)
  out <- suppressWarnings(as.numeric(x))
  bad <- is.na(out) & !is.na(x)
  if (any(bad)) {
    stop("'panels' column '", nm, "' must be numeric, but could not coerce: ",
         paste(unique(as.character(x)[bad]), collapse = ", "), ".",
         call. = FALSE)
  }
  out
}
