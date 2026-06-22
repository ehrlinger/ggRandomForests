##=============================================================================
#' Partial dependence data from a varPro model
#'
#' \code{varPro::partialpro} returns one list, with continuous and
#' categorical predictors mixed together. This function splits that list into
#' two tidy data frames, one for each kind, and resolves the y-axis label the
#' plot method will use.
#'
#' @section What partialpro is doing:
#' A partial dependence curve answers the question, "if I hold a single
#' variable at a grid of values and average out everything else, how does
#' the model's prediction move?" That is the same question \code{rfsrc}
#' partial dependence answers. What \code{varPro::partialpro} adds is two
#' wrinkles that are worth understanding before you read the curves.
#'
#' First, \code{partialpro} filters the partial grid through an isolation
#' forest (Unlimited Virtual Twins, or UVT) so that unlikely combinations
#' of the focal variable with the rest of the data are downweighted. The
#' \code{rfsrc} version, by contrast, averages over the full marginal grid
#' regardless of plausibility. So when a covariate is highly correlated
#' with others, the two methods can disagree, and \code{partialpro}'s
#' curve is the one restricted to the data manifold.
#'
#' Second, \code{partialpro} fits a local polynomial model to the
#' predicted values rather than just plotting their mean. That gives
#' three parallel curves per variable, stored as \code{yhat.par},
#' \code{yhat.nonpar}, and \code{yhat.causal}, which the plot method
#' overlays so you can see whether a smooth parametric story and the
#' raw forest predictions are telling you the same thing.
#'
#' Interpretation of the y-axis depends on the outcome (per
#' \code{varPro::partialpro}): response scale for regression, log-odds of
#' the target class for classification, and either ensemble mortality
#' (default) or RMST (if the original \code{varpro} call set
#' \code{rmst}) for survival.
#'
#' @section What's in the output:
#' We split \code{partialpro}'s mixed list into two tidy data frames so
#' the plot method does not have to. A variable with more than
#' \code{cat_limit} distinct grid points goes into \code{$continuous},
#' one row per grid point with the column means of \code{yhat.par},
#' \code{yhat.nonpar}, and \code{yhat.causal} stored as
#' \code{parametric}, \code{nonparametric}, and \code{causal}. A
#' variable at or below \code{cat_limit} goes into \code{$categorical},
#' one row per observation per category level, carrying the same three
#' columns unaveraged so the plot method can draw boxplots. Path C
#' (\code{scale \%in\% c("surv","chf")}) takes a different route: we
#' hand the underlying \code{rfsrc} forest to \code{gg_partial_rfsrc} so
#' you get a survival-probability or cumulative-hazard curve on the
#' usual rfsrc scale instead.
#'
#' @section What you use this for:
#' \itemize{
#'   \item read the marginal shape of a relationship the varpro model
#'     found important: monotone, threshold, U-shape, flat;
#'   \item compare the three partialpro estimators on the same variable
#'     and flag the ones where parametric and nonparametric disagree,
#'     those are the candidates for closer inspection;
#'   \item report a survival partial dependence on the probability or
#'     cumulative-hazard scale (\code{scale = "surv"} or \code{"chf"})
#'     rather than the unbounded mortality scale.
#' }
#' A varpro partial dependence curve is a description of the model, not
#' a causal effect. The \code{causal} column is varpro's local
#' estimator, not a structural causal claim about the data-generating
#' process.
#'
#' @param part_dta Partial plot data from \code{varPro::partialpro}.  Each
#'   element must contain \code{xvirtual}, \code{xorg}, \code{yhat.par},
#'   \code{yhat.nonpar}, and \code{yhat.causal}.  Supply at least one of
#'   \code{part_dta} or \code{object}.
#' @param object A fitted \code{varpro} object, the forest the partial data
#'   came from.  When supplied it provides the provenance metadata, and when
#'   \code{part_dta} is \code{NULL} it is passed to
#'   \code{varPro::partialpro(object)} for you.  Required when
#'   \code{scale \%in\% c("surv","chf")}.
#' @param scale Character; sets the y-axis label and, for survival forests,
#'   the output type.  One of \code{"auto"} (default), \code{"mortality"},
#'   \code{"rmst"}, \code{"surv"}, or \code{"chf"}.
#' @param time Numeric; the evaluation time point.  Required when
#'   \code{scale = "rmst"} (the RMST horizon \eqn{\tau}), where it now
#'   \emph{drives} the partial computation through an RMST(\eqn{\tau})
#'   learner (see \strong{Details}), not just the axis label.  Optional when
#'   \code{scale \%in\% c("surv","chf")}: if supplied it is snapped to the
#'   nearest value in \code{object\$rf\$time.interest} and used for both
#'   computation and axis labeling; if \code{NULL}, three quartile time
#'   points from \code{time.interest} are used (see
#'   \code{\link{gg_partial_rfsrc}}).
#' @param nvars Integer; how many variables (list elements) to process.
#'   Defaults to every variable in \code{part_dta}.
#' @param cat_limit Integer; a variable with
#'   \code{length(xvirtual) <= cat_limit} is treated as categorical.
#'   Default \code{10}.
#' @param model Character; a label tacked onto every row, handy when you are
#'   combining results from several models in one figure.
#'
#' @details
#' **Scale detection:** with \code{scale = "auto"} and an \code{object} in
#' hand, the scale resolves to \code{"mortality"} for a survival forest and
#' \code{"generic"} for a regression or classification forest.  The RMST
#' horizon \eqn{\tau} is \emph{not} stored in the \code{varpro} object
#' (varPro 3.1.0), so RMST output requires you to pass
#' \code{scale = "rmst", time = tau} explicitly.
#'
#' **RMST partial dependence (scale = "rmst"):** \code{varPro::partialpro}
#' has no time argument, so its default survival learner returns ensemble
#' mortality at every horizon -- passing a horizon through \code{...} is
#' silently dropped, and multi-horizon plots built that way differ only by
#' Monte-Carlo noise, not by \eqn{\tau}.  To get a genuine RMST(\eqn{\tau})
#' curve, \code{scale = "rmst"} supplies \code{partialpro} a \code{learner}
#' that returns \eqn{\mathrm{RMST}(\tau)=\int_0^\tau S(t)\,dt} from the
#' survival forest, so the curve actually depends on \eqn{\tau}.  This path
#' \strong{recomputes} from \code{object}, so it needs \code{object} (a
#' survival fit) with \code{part_dta = NULL}; a precomputed \code{part_dta}
#' can only be relabeled, and \code{gg_partial_varpro} warns when you try.
#' A \eqn{\tau} beyond the model's largest event time is truncated there
#' (with a warning), since \eqn{S(t)} cannot be extrapolated.
#'
#' **Ensemble mortality (scale = "mortality"):** here the y-axis is
#' \emph{ensemble mortality}, the expected number of events a subject would
#' see if they were exposed to the study-average cumulative hazard.  It is
#' the same quantity as the \code{rfsrc} \code{predicted} value for survival
#' forests (Ishwaran, Kogalur, Blackstone & Lauer, 2008
#' <doi:10.1214/08-AOAS169>).  This is an \strong{unbounded relative-risk
#' score}, \emph{not} a survival probability and not \eqn{1 - S(t)}; don't
#' read it as one.  For a bounded, time-anchored survival summary, use
#' \code{scale = "rmst", time = tau} (restricted mean survival time, in the
#' time units of the outcome) or \code{scale = "surv"} / \code{"chf"}.
#'
#' @return A named list of class \code{"gg_partial_varpro"} with elements:
#' \describe{
#'   \item{continuous}{data.frame with columns \code{variable},
#'     \code{parametric}, \code{nonparametric}, \code{causal}, \code{name}
#'     (and optionally \code{model}).}
#'   \item{categorical}{data.frame with the same columns, one row per
#'     observation per category level.}
#' }
#' A \code{"provenance"} attribute carries \code{source}, \code{family},
#' \code{ntree}, \code{n}, \code{scale}, \code{rmst_tau},
#' \code{xvar.names}, and \code{path}.
#'
#' @references
#' Ishwaran H, Kogalur UB, Blackstone EH, Lauer MS (2008).
#' Random survival forests. \emph{The Annals of Applied Statistics},
#' \bold{2}(3), 841--860. \doi{10.1214/08-AOAS169}.
#'
#' @seealso \code{\link{plot.gg_partial_varpro}},
#'   \code{\link{gg_varpro}}, \code{\link{gg_vimp}},
#'   \code{\link{gg_partialpro}} (deprecated),
#'   \code{\link{gg_partial_rfsrc}}, \code{\link{varpro_feature_names}}
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
#' result <- gg_partial_varpro(mock_data)
#' head(result$continuous)
#' head(result$categorical)
#'
#' @importFrom varPro partialpro
#' @export
gg_partial_varpro <- function(part_dta  = NULL,
                               object    = NULL,
                               scale     = c("auto", "rmst", "mortality",
                                             "surv", "chf"),
                               time      = NULL,
                               nvars     = NULL,
                               cat_limit = 10,
                               model     = NULL) {
  scale <- match.arg(scale)

  ## ---- Input validation --------------------------------------------------
  .validate_varpro_inputs(part_dta, object, scale, time)

  ## ---- C-path: route through gg_partial_rfsrc ----------------------------
  if (!is.null(object) && scale %in% c("surv", "chf")) {
    return(.gg_partial_varpro_cpath(object, scale, time, model))
  }

  ## ---- RMST guardrails (A-path) ------------------------------------------
  .warn_varpro_rmst(part_dta, object, scale, time)

  ## ---- A-path: partialpro-based partial dependence -----------------------
  ## scale = "rmst" drives the partialpro computation with an RMST(tau)
  ## learner so the curve genuinely depends on tau, rather than relabeling
  ## the default ensemble-mortality curve.  This requires recomputing from
  ## 'object'; a precomputed 'part_dta' can only be relabeled (warned above).
  if (is.null(part_dta)) {
    part_dta <- if (scale == "rmst") {
      varPro::partialpro(object, learner = .rmst_learner(object, time))
    } else {
      varPro::partialpro(object)
    }
  }
  if (is.null(nvars)) {
    nvars <- length(part_dta)
  }

  prov <- .varpro_provenance(object, scale, time, path = "A")

  dfs <- .build_varpro_dfs(part_dta, nvars, cat_limit)
  continuous  <- dfs$continuous
  categorical <- dfs$categorical

  if (!is.null(model)) {
    continuous$model  <- model
    categorical$model <- model
  }

  result <- list(continuous = continuous, categorical = categorical)
  class(result) <- c("gg_partial_varpro", "list")
  attr(result, "provenance") <- prov
  result
}

## ---------------------------------------------------------------------------
## Internal helpers

#' @keywords internal
.validate_varpro_inputs <- function(part_dta, object, scale, time) {
  if (is.null(part_dta) && is.null(object)) {
    stop("at least one of 'part_dta' or 'object' must be supplied",
         call. = FALSE)
  }
  if (scale %in% c("surv", "chf") && is.null(object)) {
    stop("scale = '", scale, "' requires 'object' (the varpro fit)",
         call. = FALSE)
  }
  if (scale == "rmst" && is.null(time)) {
    stop("scale = 'rmst' requires 'time' (the RMST horizon tau)",
         call. = FALSE)
  }
  ## RMST is only meaningful for a survival fit; when we will recompute from
  ## 'object' (part_dta = NULL), the fit must be survival.
  if (scale == "rmst" && is.null(part_dta) && !is.null(object) &&
      !identical(object$family, "surv")) {
    stop("scale = 'rmst' requires a survival varpro fit ",
         "(object$family == \"surv\")", call. = FALSE)
  }
  invisible(NULL)
}

## Surface the two RMST traps: a precomputed part_dta can't be driven by tau
## (curve is mortality with an RMST label only), and a tau past the model's
## event-time range is truncated.  Also flag a 'time' a scale ignores.
#' @keywords internal
.warn_varpro_rmst <- function(part_dta, object, scale, time) {
  if (scale == "rmst") {
    if (!is.null(part_dta)) {
      warning("gg_partial_varpro: scale = 'rmst' cannot drive the partial ",
              "computation from a precomputed 'part_dta'; the curves are ",
              "ensemble mortality carrying an RMST(tau) label only. Supply ",
              "'object' with part_dta = NULL for a genuine RMST(tau) curve.",
              call. = FALSE)
    } else if (!is.null(object)) {
      ti <- object$rf$time.interest
      if (!is.null(ti) && (time > max(ti) || time < min(ti))) {
        warning(sprintf(
          paste0("gg_partial_varpro: RMST horizon tau = %g is outside the ",
                 "model's event-time range [%g, %g]; RMST is truncated at ",
                 "the nearest observed event time."),
          time, min(ti), max(ti)), call. = FALSE)
      }
    }
  } else if (!is.null(time)) {
    ## 'time' only feeds rmst/surv/chf; warn when the resolved scale ignores it.
    fam      <- if (!is.null(object)) object$family else NA_character_
    resolved <- .resolve_varpro_scale(scale, fam)
    if (!resolved %in% c("rmst", "surv", "chf")) {
      warning("gg_partial_varpro: 'time' is ignored for scale = '", scale,
              "' (resolved to '", resolved, "').", call. = FALSE)
    }
  }
  invisible(NULL)
}

## RMST(tau) learner for varPro::partialpro: maps feature rows to
## RMST(tau) = integral_0^tau S(t) dt from the survival forest in object$rf.
## Called with no argument it returns OOB predictions on the training data,
## matching partialpro's default-learner contract.
#' @keywords internal
.rmst_learner <- function(object, tau) {
  rf    <- object$rf
  times <- rf$time.interest
  function(newx) {
    if (missing(newx)) {
      pr   <- randomForestSRC::predict.rfsrc(rf, perf.type = "none")
      surv <- pr$survival.oob
      if (is.null(surv)) surv <- pr$survival
    } else {
      surv <- randomForestSRC::predict.rfsrc(rf, newx,
                                             perf.type = "none")$survival
    }
    .rmst_from_survival(surv, times, tau)
  }
}

## Restricted mean survival time from a survival matrix:
## RMST(tau) = integral_0^tau S(t) dt.  'surv' is n x J with column k =
## S(times[k]) (randomForestSRC layout); the curve is constant on each
## [times[k-1], times[k]) interval, S = 1 on [0, times[1]).  Beyond
## max(times) S cannot be extrapolated, so RMST is truncated there.
#' @keywords internal
.rmst_from_survival <- function(surv, times, tau) {
  if (is.null(dim(surv))) surv <- matrix(surv, nrow = 1L)
  n_times <- length(times)
  t_left  <- c(0, times)                                  # length J + 1
  upper   <- pmin(t_left[-1], tau)                        # length J
  lower   <- pmin(t_left[-(n_times + 1L)], tau)           # length J
  width   <- pmax(upper - lower, 0)                       # length J
  ## Survival level on interval k is S at its left endpoint: 1, then S(t_{k-1}).
  level   <- cbind(1, surv[, -n_times, drop = FALSE])     # n x J
  as.numeric(level %*% width)
}

#' @keywords internal
.varpro_provenance <- function(object, scale, time, path = "A") {
  prov_family <- if (!is.null(object)) object$family     else NA_character_
  prov_xvars  <- if (!is.null(object)) object$xvar.names else NA_character_
  prov_n      <- if (!is.null(object)) nrow(object$x)    else NA_integer_
  prov_ntree  <- if (!is.null(object)) object$max.tree   else NA_integer_
  scale_used  <- .resolve_varpro_scale(scale, prov_family)
  list(
    source     = "varPro",
    family     = prov_family,
    ntree      = prov_ntree,
    n          = prov_n,
    scale      = scale_used,
    rmst_tau   = time,
    xvar.names = prov_xvars,
    path       = path
  )
}

#' @keywords internal
.build_varpro_dfs <- function(part_dta, nvars, cat_limit) {
  cont_list <- list()
  cat_list  <- list()
  for (feature in seq(nvars)) {
    feat      <- part_dta[[feature]]
    feat_name <- names(part_dta)[[feature]]
    if (length(feat$xvirtual) > cat_limit) {
      plt.df <- dplyr::bind_cols(
        variable      = feat$xvirtual,
        parametric    = colMeans(feat$yhat.par,    na.rm = TRUE),
        nonparametric = colMeans(feat$yhat.nonpar, na.rm = TRUE),
        causal        = colMeans(feat$yhat.causal, na.rm = TRUE)
      )
      plt.df$name <- feat_name
      cont_list[[feature]] <- plt.df
    } else {
      cat_list[[feature]] <- .process_cat_var(feat, feat_name)
    }
  }
  list(
    continuous  = dplyr::bind_rows(cont_list),
    categorical = dplyr::bind_rows(cat_list)
  )
}

#' @keywords internal
.resolve_varpro_scale <- function(scale, family) {
  if (scale != "auto") return(scale)
  if (is.na(family) || is.null(family)) return("generic")
  if (family == "surv")  return("mortality")
  "generic"   # regr, class, or unknown
}

#' @keywords internal
.process_cat_var <- function(feat, feat_name) {
  n_cats   <- length(unique(feat$xorg))
  cat_feat <- list()
  for (ind in seq(n_cats)) {
    cat_feat[[ind]] <- dplyr::bind_cols(
      parametric    = feat$yhat.par[, ind],
      nonparametric = feat$yhat.nonpar[, ind],
      causal        = feat$yhat.causal[, ind]
    )
    cat_feat[[ind]]$variable <- unique(feat$xorg)[ind]
    plt.df <- if (ind == 1L) cat_feat[[ind]] else
      dplyr::bind_rows(plt.df, cat_feat[[ind]])
  }
  plt.df$name <- feat_name
  plt.df
}

#' @keywords internal
.gg_partial_varpro_cpath <- function(object, scale, time, model) {
  rf           <- object$rf
  partial_type <- if (scale == "surv") "surv" else "chf"

  partial_time <- NULL
  if (!is.null(time)) {
    ti           <- rf$time.interest
    partial_time <- ti[which.min(abs(ti - time))]
  }

  pd <- gg_partial_rfsrc(rf,
                          xvar.names   = object$xvar.names,
                          partial.time = partial_time,
                          partial.type = partial_type)

  ## Prepend our class so S3 dispatch routes here first; NextMethod() falls
  ## through to plot.gg_partial_rfsrc for rendering.
  class(pd) <- c("gg_partial_varpro", class(pd))

  if (!is.null(model)) {
    if (is.data.frame(pd$continuous))  pd$continuous$model  <- model
    if (is.data.frame(pd$categorical)) pd$categorical$model <- model
  }

  attr(pd, "provenance") <- list(
    source     = "varPro",
    family     = object$family,
    ntree      = object$max.tree,
    n          = nrow(object$x),
    scale      = scale,
    rmst_tau   = time,
    xvar.names = object$xvar.names,
    path       = "C"
  )
  pd
}
