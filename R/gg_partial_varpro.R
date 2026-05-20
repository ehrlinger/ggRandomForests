##=============================================================================
#' Partial dependence data from a varPro model
#'
#' Splits the list returned by \code{varpro::partialpro} into separate
#' data frames for continuous and categorical predictors, with provenance-
#' aware y-axis labeling for downstream plot methods.
#'
#' @param part_dta Partial plot data from \code{varpro::partialpro}.  Each
#'   element must contain \code{xvirtual}, \code{xorg}, \code{yhat.par},
#'   \code{yhat.nonpar}, and \code{yhat.causal}.  At least one of
#'   \code{part_dta} or \code{object} must be supplied.
#' @param object A fitted \code{varpro} object (the originating forest).
#'   When supplied, used for provenance metadata and — when \code{part_dta}
#'   is \code{NULL} — \code{varpro::partialpro(object)} is called
#'   internally.  Required when \code{scale \%in\% c("surv","chf")}.
#' @param scale Character; controls y-axis labeling and (for survival)
#'   the output type.  One of \code{"auto"} (default), \code{"mortality"},
#'   \code{"rmst"}, \code{"surv"}, or \code{"chf"}.
#' @param time Numeric; required when \code{scale = "rmst"} (the RMST
#'   horizon \eqn{\tau}), and when \code{scale \%in\% c("surv","chf")} to
#'   label the evaluation time point.
#' @param nvars Integer; number of variables (list elements) to process.
#'   Defaults to all variables in \code{part_dta}.
#' @param cat_limit Integer; variables with
#'   \code{length(xvirtual) <= cat_limit} are treated as categorical.
#'   Default \code{10}.
#' @param model Character; label appended to all rows (useful when
#'   combining results from multiple models in a single figure).
#'
#' @details
#' **Scale detection:** \code{scale = "auto"} with a supplied \code{object}
#' resolves to \code{"mortality"} for survival forests and \code{"generic"}
#' for regression/classification forests.  The RMST horizon \eqn{\tau} is
#' \emph{not} stored in the \code{varpro} object (varPro 3.1.0); pass
#' \code{scale = "rmst", time = \tau} explicitly for RMST-labeled output.
#'
#' **Ensemble mortality (scale = "mortality"):** The y-axis represents
#' \emph{ensemble mortality}: the expected number of events if the subject
#' experienced the study-average cumulative hazard, equivalent to the
#' \code{rfsrc} \code{predicted} value for survival forests (Ishwaran,
#' Kogalur, Blackstone & Lauer, 2008 <doi:10.1214/08-AOAS169>).  This is
#' an \strong{unbounded relative-risk score}---\emph{not} a survival
#' probability or \eqn{1 - S(t)}---and must not be interpreted as one.
#' For probability-scale output refit with
#' \code{varpro(\ldots, rmst = \tau)} and use \code{scale = "rmst"}.
#'
#' @return A named list of class \code{"gg_partial_varpro"} with elements:
#' \describe{
#'   \item{continuous}{data.frame with columns \code{variable},
#'     \code{parametric}, \code{nonparametric}, \code{causal}, \code{name}
#'     (and optionally \code{model}).}
#'   \item{categorical}{data.frame with the same columns but one row per
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

  ## ---- C-path: route through gg_partial_rfsrc ----------------------------
  if (!is.null(object) && scale %in% c("surv", "chf")) {
    return(.gg_partial_varpro_cpath(object, scale, time, model))
  }

  ## ---- A-path: partialpro-based partial dependence -----------------------
  if (is.null(part_dta)) {
    part_dta <- varPro::partialpro(object)
  }

  ## Provenance fields from object (NA when no object supplied).
  prov_family <- if (!is.null(object)) object$family   else NA_character_
  prov_xvars  <- if (!is.null(object)) object$xvar.names else NA_character_
  prov_n      <- if (!is.null(object)) nrow(object$x)  else NA_integer_
  prov_ntree  <- if (!is.null(object)) object$max.tree  else NA_integer_

  scale_used  <- .resolve_varpro_scale(scale, prov_family)

  if (is.null(nvars)) {
    nvars <- length(part_dta)
  }

  cont_list <- list()
  cat_list  <- list()

  for (feature in seq(nvars)) {
    if (length(part_dta[[feature]]$xvirtual) > cat_limit) {
      ## -- Continuous: one row per xvirtual grid point ---------------------
      plt.df <- dplyr::bind_cols(
        variable      = part_dta[[feature]]$xvirtual,
        parametric    = colMeans(part_dta[[feature]]$yhat.par,    na.rm = TRUE),
        nonparametric = colMeans(part_dta[[feature]]$yhat.nonpar, na.rm = TRUE),
        causal        = colMeans(part_dta[[feature]]$yhat.causal,  na.rm = TRUE)
      )
      plt.df$name <- names(part_dta)[[feature]]
      cont_list[[feature]] <- plt.df

    } else {
      ## -- Categorical: stack per-observation rows per category level ------
      n_cats   <- length(unique(part_dta[[feature]]$xorg))
      cat_feat <- list()
      for (ind in seq(n_cats)) {
        cat_feat[[ind]] <- dplyr::bind_cols(
          parametric    = part_dta[[feature]]$yhat.par[, ind],
          nonparametric = part_dta[[feature]]$yhat.nonpar[, ind],
          causal        = part_dta[[feature]]$yhat.causal[, ind]
        )
        cat_feat[[ind]]$variable <- unique(part_dta[[feature]]$xorg)[ind]
        plt.df <- if (ind == 1L) cat_feat[[ind]] else
          dplyr::bind_rows(plt.df, cat_feat[[ind]])
      }
      plt.df$name <- names(part_dta)[[feature]]
      cat_list[[feature]] <- plt.df
    }
  }

  continuous  <- dplyr::bind_rows(cont_list)
  categorical <- dplyr::bind_rows(cat_list)

  if (!is.null(model)) {
    continuous$model <- model
    categorical$model <- model
  }

  result <- list(continuous = continuous, categorical = categorical)
  class(result) <- c("gg_partial_varpro", "list")

  attr(result, "provenance") <- list(
    source     = "varPro",
    family     = prov_family,
    ntree      = prov_ntree,
    n          = prov_n,
    scale      = scale_used,
    rmst_tau   = time,
    xvar.names = prov_xvars,
    path       = "A"
  )
  result
}

## ---------------------------------------------------------------------------
## Internal helpers

#' @keywords internal
.resolve_varpro_scale <- function(scale, family) {
  if (scale != "auto") return(scale)
  if (is.na(family) || is.null(family)) return("generic")
  if (family == "surv")  return("mortality")
  "generic"   # regr, class, or unknown
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
