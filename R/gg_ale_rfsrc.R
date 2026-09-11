##=============================================================================
#' Accumulated Local Effects (ALE) data from an rfsrc model
#'
#' A partial dependence curve (\code{\link{gg_partial_rfsrc}}) marginalizes
#' the forest's prediction by averaging over the joint distribution of the
#' other predictors, a computation that is misleading when predictors are
#' correlated, because it evaluates the forest at combinations of predictor
#' values that never occur together in the data. Accumulated Local Effects
#' (Apley and Zhu, 2020) avoid this by only ever perturbing a predictor
#' within small local neighborhoods of its own observed values, then
#' accumulating those local effects into a global curve.
#'
#' For a continuous predictor, \code{gg_ale_rfsrc} bins the observed values
#' into \code{n_eval} quantile-based intervals. Within each bin, every
#' observation's predictor value is replaced first with the bin's lower edge
#' and then with its upper edge (all other predictors held at that
#' observation's own values), and the average change in prediction is the
#' bin's local effect. These local effects are accumulated (cumulatively
#' summed) across bins and centered to have a weighted mean of zero, giving a
#' curve that is directly comparable to \code{\link{gg_partial_rfsrc}}'s
#' output but immune to extrapolation into implausible predictor
#' combinations.
#'
#' For a categorical predictor, the model's own factor level order is used
#' as the "grid": the local effect for the step from level \eqn{k} to level
#' \eqn{k+1} is estimated only from observations actually at level
#' \eqn{k+1}, comparing their prediction at that level against the
#' counterfactual of level \eqn{k}. Relevel the predictor before fitting the
#' forest to control this ordering.
#'
#' Supplying \code{xvar2.name} switches to second-order (interaction) ALE
#' between \code{xvar.names} and \code{xvar2.name}, isolating the part of
#' their joint effect that is not explained by either variable's own main
#' effect. It is the ALE analogue of an interaction term, computed on a
#' 2-D grid of bins using the same local-perturbation idea, with the main
#' effects removed via a row/column/grand weighted-mean decomposition (the
#' same device used to isolate an interaction term in a two-way ANOVA). A
#' purely additive forest returns an all-zero surface.
#'
#' @param rf_model A fitted \code{\link[randomForestSRC]{rfsrc}} object.
#'   Regression and classification forests only; survival is not yet
#'   implemented (see \code{\link{gg_shap}}, which has the same limitation).
#' @param xvar.names Character vector of predictor names to compute ALE for.
#'   When \code{xvar2.name} is supplied, this must name exactly one
#'   predictor.
#' @param xvar2.name Optional single character name of a second predictor.
#'   When supplied, second-order (interaction) ALE is computed for the pair
#'   \code{xvar.names} x \code{xvar2.name} instead of first-order ALE.
#'   Both predictors must be continuous in this version (see \code{cat_limit}).
#' @param newx Optional \code{data.frame} of predictor values to evaluate ALE
#'   at. Defaults to the training data stored in \code{rf_model$xvar}. All
#'   column names must match \code{rf_model$xvar.names}.
#' @param cat_limit Variables with fewer than \code{cat_limit} unique values
#'   in \code{newx} are treated as categorical; all others are continuous.
#'   Defaults to 10.
#' @param n_eval Number of quantile bins used for a continuous predictor's
#'   ALE grid (first-order) or per axis (second-order). Defaults to 25.
#' @param which.class For classification forests, the class (integer column
#'   index into the predicted-probability matrix) whose ALE is computed.
#'   Defaults to 1.
#'
#' @return For first-order ALE (\code{xvar2.name = NULL}), a named list with
#'   two elements, classed \code{"gg_ale_rfsrc"}:
#'   \describe{
#'     \item{continuous}{A \code{data.frame} with columns \code{x} (the bin
#'       edges, numeric), \code{yhat} (the centered ALE value at each edge),
#'       and \code{name} (variable name), for all continuous predictors.}
#'     \item{categorical}{The same columns but \code{x} kept as a
#'       \code{factor} in the model's level order, for low-cardinality
#'       predictors.}
#'   }
#'   For second-order ALE (\code{xvar2.name} supplied), a \code{data.frame}
#'   classed \code{"gg_ale_interaction"} with columns \code{x} (grid values
#'   of \code{xvar.names}), \code{y} (grid values of \code{xvar2.name}),
#'   \code{ale} (the interaction surface value), \code{name1}, and
#'   \code{name2}.
#'
#' @references Apley, D. W. and Zhu, J. (2020). Visualizing the effects of
#'   predictor variables in black box supervised learning models. Journal of
#'   the Royal Statistical Society Series B, 82(4), 1059-1086.
#'
#' @seealso \code{\link{gg_partial_rfsrc}}, \code{\link{plot.gg_ale_rfsrc}}
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## regression, first-order ALE
#' ## ------------------------------------------------------------
#' airq.obj <- randomForestSRC::rfsrc(Ozone ~ ., data = na.omit(airquality),
#'                                     ntree = 100)
#' ale_dta <- gg_ale_rfsrc(airq.obj, xvar.names = c("Wind", "Temp"))
#' plot(ale_dta)
#'
#' \donttest{
#' ## ------------------------------------------------------------
#' ## second-order (interaction) ALE between two continuous predictors
#' ## ------------------------------------------------------------
#' ale_int <- gg_ale_rfsrc(airq.obj, xvar.names = "Wind",
#'                          xvar2.name = "Temp", n_eval = 15)
#' plot(ale_int)
#' }
#'
#' @importFrom stats quantile predict
#' @export
gg_ale_rfsrc <- function(rf_model,
                         xvar.names,
                         xvar2.name = NULL,
                         newx = NULL,
                         cat_limit = 10,
                         n_eval = 25,
                         which.class = 1) {
  if (!inherits(rf_model, "rfsrc")) {
    stop("gg_ale_rfsrc: expected an 'rfsrc' object; ",
         "got an object of class ", paste(class(rf_model), collapse = "/"), ".",
         call. = FALSE)
  }
  if (!rf_model$family %in% c("regr", "class")) {
    stop("gg_ale_rfsrc: only regression and classification forests are ",
         "supported in this version; got family '", rf_model$family, "'. ",
         "Survival support is not yet implemented.", call. = FALSE)
  }
  ## length 0 is caught here with the same message as missing/NULL. Left to
  ## fall through, character(0) reaches do.call("rbind", list()) and fails with
  ## base R's "argument is of length zero", which names neither the argument
  ## nor the function.
  if (missing(xvar.names) || is.null(xvar.names) ||
        length(xvar.names) == 0L) {
    stop("gg_ale_rfsrc: 'xvar.names' is required.", call. = FALSE)
  }

  newx <- .ale_validate_newx(rf_model, newx, xvar.names)

  ## validate_partial_args() is defined in gg_partial_rfsrc.R and shared here.
  v         <- validate_partial_args(n_eval, cat_limit)
  n_eval    <- v$n_eval
  cat_limit <- v$cat_limit

  is_class <- rf_model$family == "class"
  if (is_class) {
    ## .gg_shap_validate_which_class() is defined in gg_shap.R and shared here.
    which.class <- .gg_shap_validate_which_class(which.class,
                                                 ncol(rf_model$predicted))
  }

  ## Same prediction convention as gg_shap.rfsrc()'s pred_fun: $predicted is
  ## already a probability matrix for classification forests, no
  ## type = "prob" needed.
  pred_fun <- function(newdata) {
    pr <- predict(rf_model, newdata)$predicted
    if (is_class) as.numeric(pr[, which.class]) else as.numeric(pr)
  }

  if (!is.null(xvar2.name)) {
    if (length(xvar.names) != 1L) {
      stop("gg_ale_rfsrc: when 'xvar2.name' is supplied, 'xvar.names' must ",
           "name exactly one predictor (the pair is xvar.names x xvar2.name).",
           call. = FALSE)
    }
    if (!xvar2.name %in% colnames(newx)) {
      stop("xvar2.name contains a column name not found in the rfsrc object")
    }
    result <- .ale_interaction(pred_fun, newx, xvar.names, xvar2.name,
                               cat_limit, n_eval)
    result <- .set_provenance(result, rf_model)
    return(result)
  }

  ## The documented grid for a factor is the MODEL's level order. newx may be a
  ## subset, or carry a relevelled copy, so read the ordering from the fitted
  ## forest rather than from whatever was passed in.
  model_levels <- lapply(rf_model$xvar, function(col) {
    if (is.factor(col)) levels(col) else NULL
  })

  pdta <- lapply(xvar.names, .ale_one_var,
                newx = newx, pred_fun = pred_fun,
                cat_limit = cat_limit, n_eval = n_eval,
                model_levels = model_levels)
  pdta   <- Filter(Negate(is.null), pdta)
  result <- .ale_split_result(do.call("rbind", pdta))
  result <- .set_provenance(result, rf_model)
  result
}

## Resolve and validate the frame ALE is evaluated on. Split out of
## gg_ale_rfsrc() to keep that function under the cyclomatic complexity the
## repo lints for; it is all one concern, so it reads as one function.
.ale_validate_newx <- function(rf_model, newx, xvar.names) {
  if (is.null(newx)) {
    newx <- rf_model$xvar
  }
  if (!is.data.frame(newx)) {
    stop("gg_ale_rfsrc: 'newx' must be a data.frame; got an object of class ",
         paste(class(newx), collapse = "/"), ".", call. = FALSE)
  }
  ## predict.rfsrc() needs every training predictor, not just the ones being
  ## profiled: the other columns are held at their observed values. Checking
  ## only that the supplied names are known would let a subset through, to fail
  ## later inside predict() with a message that names no column.
  missing_cols <- setdiff(rf_model$xvar.names, colnames(newx))
  if (length(missing_cols) > 0L) {
    stop("gg_ale_rfsrc: 'newx' is missing ", length(missing_cols),
         " predictor(s) the forest was trained on: ",
         paste(missing_cols, collapse = ", "), ".", call. = FALSE)
  }
  extra_cols <- setdiff(colnames(newx), rf_model$xvar.names)
  if (length(extra_cols) > 0L) {
    stop("gg_ale_rfsrc: 'newx' carries column(s) the forest was not trained ",
         "on: ", paste(extra_cols, collapse = ", "), ".", call. = FALSE)
  }
  if (sum(xvar.names %in% colnames(newx)) != length(xvar.names)) {
    stop("xvar.names contains column names not found in the rfsrc object")
  }
  newx
}

## ---------------------------------------------------------------------------
## Internal: shared bin construction, accumulation, and per-variable ALE.
## ---------------------------------------------------------------------------

## Same cat_limit convention used throughout the package (see
## make_eval_grid() in gg_partial_rfsrc.R), NA handling included: NA is
## missingness, not a level, and counting it as one lets a predictor with
## cat_limit - 1 genuine values plus some NA read as continuous. The two
## routes then disagree about the same column, which is worse than either
## answer on its own. make_eval_grid() drops NA before the count for this
## reason (gg_partial_rfsrc.R:221) and so does this.
.ale_is_categorical <- function(xval, cat_limit) {
  if (is.factor(xval) || is.character(xval)) {
    return(TRUE)
  }
  length(unique(xval[!is.na(xval)])) < cat_limit
}

## Quantile-based bin edges for a continuous predictor (n_bin + 1 edges, n_bin bins).
## The lower edge is nudged down a hair so the minimum observed value is
## captured by findInterval()'s rightmost/leftmost-closed bin.
.ale_bin_edges <- function(xval, n_eval) {
  probs <- seq(0, 1, length.out = n_eval + 1L)
  edges <- unique(stats::quantile(xval, probs = probs, type = 1, names = FALSE))
  if (length(edges) < 2L) {
    stop("gg_ale_rfsrc: predictor has too few distinct values to bin.",
         call. = FALSE)
  }
  edges[1] <- edges[1] - 1e-8 * max(1, abs(edges[1]))
  edges
}

## Which of n_bin bins each value falls into, given n_bin + 1 edges.
.ale_bin_index <- function(xval, edges) {
  findInterval(xval, edges, rightmost.closed = TRUE, all.inside = TRUE)
}

## Accumulate per-bin local effects into a centered ALE curve (Apley and Zhu
## 2020). `delta` is the average local effect in each of n_bin bins; `nk` is the
## observation count in each bin. Returns n_bin + 1 centered values, one per grid
## point (bin edges for a continuous variable, levels for a categorical one).
.ale_accumulate <- function(delta, nk) {
  n  <- sum(nk)
  fJ <- c(0, cumsum(delta))
  avg <- if (n > 0) {
    sum((nk / n) * (fJ[-1] + fJ[-length(fJ)]) / 2)
  } else {
    0
  }
  fJ - avg
}

## First-order ALE for one continuous predictor.
.ale_continuous <- function(xname, newx, pred_fun, n_eval) {
  xval <- newx[[xname]]
  keep <- !is.na(xval)
  dd_all <- newx[keep, , drop = FALSE]
  xval   <- xval[keep]

  edges <- .ale_bin_edges(xval, n_eval)
  n_bin     <- length(edges) - 1L
  bin   <- .ale_bin_index(xval, edges)

  delta <- numeric(n_bin)
  nk    <- numeric(n_bin)
  for (k in seq_len(n_bin)) {
    idx <- which(bin == k)
    nk[k] <- length(idx)
    if (nk[k] == 0L) next
    dd_lo <- dd_all[idx, , drop = FALSE]
    dd_hi <- dd_lo
    dd_lo[[xname]] <- edges[k]
    dd_hi[[xname]] <- edges[k + 1L]
    delta[k] <- mean(pred_fun(dd_hi) - pred_fun(dd_lo))
  }

  ale <- .ale_accumulate(delta, nk)
  data.frame(x = edges, yhat = ale, name = xname, type = "continuous")
}

## Accumulate per-level effects into a centered categorical ALE curve. The
## continuous form in .ale_accumulate() cannot be reused: it averages
## neighbouring grid points trapezoidally and weights by BIN counts, of which
## there are m - 1 for m levels, so the first level's population never enters
## the centering. Levels are points, not intervals -- there is nothing between
## two of them to integrate over -- so the centering constant is the plain
## frequency-weighted mean over all m levels, which is what makes the expected
## ALE over the observed data zero.
.ale_accumulate_categorical <- function(delta, n_level) {
  fj <- c(0, cumsum(delta))
  n <- sum(n_level)
  if (n == 0) {
    return(fj)
  }
  fj - sum((n_level / n) * fj)
}

## Impose one grid value on a predictor column, keeping the column's type.
## Substituting a factor into a numeric column changes that column's type, and
## the forest then scores a variable it was not fit on -- silently, because
## predict() still returns numbers. This matters because .ale_is_categorical()
## treats ANY predictor with fewer than cat_limit unique values as categorical,
## so a numeric 0/1 indicator reaches this path.
.ale_impose_level <- function(col, value) {
  if (is.factor(col)) {
    factor(as.character(value), levels = levels(col))
  } else {
    value
  }
}

## First-order ALE for one categorical predictor. Levels are ordered as in the
## fitted model, not as they happen to appear in newx. Level 1 has no
## predecessor and contributes no local-effect step of its own -- it is the
## zero anchor the accumulation starts from, exactly as a continuous variable's
## first bin edge is -- but its observations do count toward the centering.
.ale_categorical <- function(xname, newx, pred_fun, model_levels = NULL) {
  xval <- newx[[xname]]
  keep <- !is.na(xval)
  dd_all <- newx[keep, , drop = FALSE]
  xval   <- xval[keep]

  if (is.factor(xval)) {
    ## droplevels(xval) would follow newx's ordering; the documented grid is
    ## the model's. Restrict to levels actually present, keeping that order.
    lev <- if (is.null(model_levels)) levels(xval) else model_levels
    flabels <- lev[lev %in% as.character(xval)]
  } else if (is.character(xval)) {
    flabels <- sort(unique(xval))
  } else {
    ## Sort numerically. Sorting the labels instead would order "10" before
    ## "2", which reverses part of the grid and therefore part of the curve.
    flabels <- as.character(sort(unique(xval)))
  }
  fvalues <- if (is.numeric(xval)) as.numeric(flabels) else flabels

  m <- length(flabels)
  if (m < 2L) {
    stop("gg_ale_rfsrc: categorical predictor '", xname,
         "' has fewer than 2 observed levels.", call. = FALSE)
  }
  code <- match(as.character(xval), flabels)

  ## Population of every level, the first included: it takes no step, but it
  ## weighs on where the curve is centered.
  n_level <- tabulate(code, nbins = m)

  n_bin <- m - 1L
  delta <- numeric(n_bin)
  for (k in seq_len(n_bin)) {
    ## Bin k's members are observations at the UPPER level of the step, the
    ## same convention as the continuous case (bin k = values up through
    ## edge_k).
    idx <- which(code == k + 1L)
    if (length(idx) == 0L) next
    dd_lo <- dd_all[idx, , drop = FALSE]
    dd_hi <- dd_lo
    dd_lo[[xname]] <- .ale_impose_level(dd_all[[xname]], fvalues[k])
    dd_hi[[xname]] <- .ale_impose_level(dd_all[[xname]], fvalues[k + 1L])
    delta[k] <- mean(pred_fun(dd_hi) - pred_fun(dd_lo))
  }

  ale <- .ale_accumulate_categorical(delta, n_level)
  data.frame(x = factor(flabels, levels = flabels), yhat = ale, name = xname,
             type = "categorical")
}

## Dispatch one predictor to the continuous or categorical ALE builder.
.ale_one_var <- function(xname, newx, pred_fun, cat_limit, n_eval,
                         model_levels = NULL) {
  xval <- newx[[xname]]
  if (.ale_is_categorical(xval, cat_limit)) {
    .ale_categorical(xname, newx, pred_fun, model_levels[[xname]])
  } else {
    .ale_continuous(xname, newx, pred_fun, n_eval)
  }
}

## Split the combined data.frame into continuous / categorical and stamp
## class, mirroring split_partial_result() in gg_partial_rfsrc.R.
.ale_split_result <- function(pdta) {
  cont_idx        <- pdta$type == "continuous"
  continuous      <- pdta[cont_idx, , drop = FALSE]
  continuous$x    <- as.numeric(continuous$x)
  continuous$type <- NULL
  categorical      <- pdta[!cont_idx, , drop = FALSE]
  categorical$type <- NULL
  if (nrow(categorical) > 0L) {
    categorical$x <- factor(categorical$x, levels = unique(categorical$x))
  }
  result <- list(continuous = continuous, categorical = categorical)
  class(result) <- "gg_ale_rfsrc"
  result
}

## ---------------------------------------------------------------------------
## Internal: second-order (interaction) ALE, continuous x continuous only.
## ---------------------------------------------------------------------------

## Isolates the pure interaction effect between two continuous predictors.
## The joint second-difference surface is double-accumulated (analogous to
## .ale_accumulate(), but over a 2-D grid), then the row-weighted,
## column-weighted, and grand-weighted means are subtracted -- the same
## two-way decomposition used to isolate an interaction term in ANOVA. A
## purely additive f(x1, x2) = g(x1) + h(x2) returns an all-zero surface;
## see tests/testthat/test_gg_ale_rfsrc.R for the numerical check.
.ale_interaction <- function(pred_fun, newx, xname1, xname2, cat_limit, n_eval) {
  x1 <- newx[[xname1]]
  x2 <- newx[[xname2]]
  if (.ale_is_categorical(x1, cat_limit) || .ale_is_categorical(x2, cat_limit)) {
    stop("gg_ale_rfsrc: interaction ALE (xvar2.name) supports two continuous ",
         "predictors only in this version; '", xname1, "' or '", xname2,
         "' is categorical (see cat_limit).", call. = FALSE)
  }
  keep   <- !is.na(x1) & !is.na(x2)
  dd_all <- newx[keep, , drop = FALSE]
  x1 <- x1[keep]
  x2 <- x2[keep]

  e1 <- .ale_bin_edges(x1, n_eval)
  e2 <- .ale_bin_edges(x2, n_eval)
  n_bin1 <- length(e1) - 1L
  n_bin2 <- length(e2) - 1L
  b1 <- .ale_bin_index(x1, e1)
  b2 <- .ale_bin_index(x2, e2)

  delta <- matrix(0, n_bin1, n_bin2)
  cnt   <- matrix(0, n_bin1, n_bin2)
  for (k in seq_len(n_bin1)) {
    for (l in seq_len(n_bin2)) {
      idx <- which(b1 == k & b2 == l)
      cnt[k, l] <- length(idx)
      if (cnt[k, l] == 0L) next
      dd    <- dd_all[idx, , drop = FALSE]
      dd_hh <- dd
      dd_hh[[xname1]] <- e1[k + 1L]
      dd_hh[[xname2]] <- e2[l + 1L]
      dd_hl <- dd
      dd_hl[[xname1]] <- e1[k + 1L]
      dd_hl[[xname2]] <- e2[l]
      dd_lh <- dd
      dd_lh[[xname1]] <- e1[k]
      dd_lh[[xname2]] <- e2[l + 1L]
      dd_ll <- dd
      dd_ll[[xname1]] <- e1[k]
      dd_ll[[xname2]] <- e2[l]
      delta[k, l] <- mean(pred_fun(dd_hh) - pred_fun(dd_hl) -
                            pred_fun(dd_lh) + pred_fun(dd_ll))
    }
  }

  ## Double cumulative sum, down columns then across rows. Written as loops
  ## rather than nested apply(): apply() drops the dimension when an axis has a
  ## single bin, and the outer call then fails with "dim(X) must have a
  ## positive length". A one-bin axis is not exotic -- quantile edges collapse
  ## on tied values, so a predictor with most of its mass at one value reaches
  ## it for any modest n_eval.
  g <- delta
  if (n_bin1 > 1L) {
    for (k in 2:n_bin1) g[k, ] <- g[k, ] + g[k - 1L, ]
  }
  if (n_bin2 > 1L) {
    for (l in 2:n_bin2) g[, l] <- g[, l] + g[, l - 1L]
  }
  g <- rbind(0, g)
  g <- cbind(0, g)

  w <- matrix(0, n_bin1 + 1L, n_bin2 + 1L)
  w[-1, -1] <- cnt
  n_row <- rowSums(w)
  n_col <- colSums(w)
  n_tot <- sum(w)
  row_mean   <- ifelse(n_row > 0, rowSums(w * g) / n_row, 0)
  col_mean   <- ifelse(n_col > 0, colSums(w * g) / n_col, 0)
  grand_mean <- sum(w * g) / n_tot

  interaction <- sweep(g, 1, row_mean, "-")
  interaction <- sweep(interaction, 2, col_mean, "-") + grand_mean

  out <- expand.grid(x = e1, y = e2)
  out$ale   <- as.vector(interaction)
  out$name1 <- xname1
  out$name2 <- xname2
  class(out) <- c("gg_ale_interaction", class(out))
  out
}
