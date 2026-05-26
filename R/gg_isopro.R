####**********************************************************************
####  gg_isopro: tidy extractor for varPro::isopro anomaly scores.
####
####  varPro::isopro returns a list with $howbad (per-observation anomaly
####  score in [0,1]) and $case.depth (average isolation depth, lower =
####  more anomalous). gg_isopro() reshapes these into a tidy data.frame
####  the plot/print/summary methods can consume.
####**********************************************************************

#' Tidy data from a varPro isolation-forest fit
#'
#' Pulls per-observation anomaly scores out of a `varPro::isopro` fit so
#' you can plot them, sort them, or write them to disk without having to
#' know the internal shape of the fit.
#'
#' @section What isopro is doing:
#' An isolation forest (Liu, Ting and Zhou 2008) is a random forest grown
#' on very small subsamples of the data and asked to split until each
#' observation lands in its own terminal node. The intuition is geometric:
#' a typical observation sits in the dense middle of the feature cloud and
#' takes many splits to isolate, while an unusual observation sits out
#' near an edge and gets cut off after only a few. So **the depth at which
#' an observation is isolated is a proxy for how typical it is** — shallow
#' depth means anomalous, deep depth means ordinary. Average a single
#' observation's depth across many trees and the noise washes out, leaving
#' a stable per-observation rank.
#'
#' `varPro::isopro` supports three flavours of isolation forest, which
#' differ in how the splits are chosen:
#' \describe{
#'   \item{`"rnd"`}{The original Liu/Ting/Zhou method: each tree node
#'     picks a variable at random and a split point uniformly at random
#'     in the variable's range. Fast, no model, surprisingly effective.}
#'   \item{`"unsupv"`}{Unsupervised splitting from `randomForestSRC`:
#'     splits are chosen to separate the data along the directions of
#'     highest variance. More structured than `"rnd"`; sometimes more
#'     accurate, especially when the anomalies follow a coherent
#'     direction.}
#'   \item{`"auto"`}{An auto-encoder formulation that grows a multivariate
#'     forest predicting each feature from the others. Most expressive,
#'     slowest, best suited to low-dimensional data.}
#' }
#' No method is universally best. The varPro authors recommend trying at
#' least two and comparing the score distributions; the plot method here
#' colours per-method curves automatically when you stack the results.
#'
#' @section What's in the output:
#' The fit gives back two parallel per-observation vectors: `case.depth`
#' is the raw mean isolation depth (units of "splits", lower = more
#' anomalous) and `howbad` is the same information transformed onto a
#' `[0, 1]` scale via the empirical CDF of `case.depth` (higher = more
#' anomalous). Both columns are kept so you can plot in either space and
#' have the raw depth on hand for diagnostics; `howbad` is the canonical
#' score and is what the plot method uses by default.
#'
#' @section What you use this for:
#' This is screening, not inference. Reach for it when you want to:
#' \itemize{
#'   \item flag observations that may be data-entry errors, out-of-range
#'     measurements, or distinct subpopulations before fitting a primary
#'     model;
#'   \item check whether a held-out cohort sits inside the training
#'     distribution before scoring with a model trained elsewhere;
#'   \item give the analyst a ranked list of "look at these first" cases
#'     for a manual review.
#' }
#' The score is a *rank*, not a probability of being an outlier — two
#' observations with `howbad = 0.92` are both unusual, not "92\% likely
#' to be anomalous". Pick a cutoff by looking at where the elbow rises;
#' `plot.gg_isopro` can annotate either a score (`threshold`) or a
#' top-percent (`top_n_pct`) for you.
#'
#' @param object An `isopro` fit returned by `varPro::isopro()`.
#' @param ... Currently unused.
#'
#' @return A `data.frame` of class `c("gg_isopro", "data.frame")`, one row
#'   per observation. Columns:
#'   \describe{
#'     \item{obs}{Integer; observation index `1..n`, in the same order as
#'       the rows of the data passed to `isopro()`.}
#'     \item{case.depth}{Numeric; mean isolation depth across the forest.
#'       Lower means the observation was isolated quickly — more
#'       anomalous.}
#'     \item{howbad}{Numeric in `[0, 1]`; the `case.depth` values pushed
#'       through their own empirical CDF and flipped so higher means more
#'       anomalous. This is the score the plot method draws by default.}
#'   }
#'   A `provenance` attribute records `source = "varPro::isopro"`, the
#'   observation count `n`, and the number of trees `ntree`.
#'
#' @section Comparing methods:
#' To compare methods (`"rnd"`, `"unsupv"`, `"auto"`), call `gg_isopro()`
#' on each fit and `dplyr::bind_rows()` the results with a `method` label
#' column. The plot method auto-detects `method` and colours the curves.
#'
#' @references
#' Liu, F. T., Ting, K. M., and Zhou, Z. H. (2008). Isolation Forest.
#' \emph{Eighth IEEE International Conference on Data Mining}, 413-422.
#'
#' Ishwaran, H., Mantero, A., and Lu, M. (2025). varPro: Model-Independent
#' Variable Selection via the Rule-Based Variable Priority Framework.
#' \emph{R package version 3.x}.
#'
#' @seealso [plot.gg_isopro()], [varPro::isopro()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   fit <- varPro::isopro(data = iris[, 1:4], method = "rnd",
#'                         sampsize = 32, ntree = 50)
#'   gg <- gg_isopro(fit)
#'   plot(gg)
#' }
#' }
#'
#' @export
gg_isopro <- function(object, ...) {
  UseMethod("gg_isopro", object)
}

#' @export
gg_isopro.isopro <- function(object, ...) {
  if (!inherits(object, "isopro")) {
    stop("gg_isopro expects a 'isopro' object from varPro::isopro().",
         call. = FALSE)
  }

  howbad <- as.numeric(object$howbad)
  depth  <- as.numeric(object$case.depth)
  n      <- length(howbad)

  gg_dta <- data.frame(
    obs        = seq_len(n),
    case.depth = depth,
    howbad     = howbad
  )

  class(gg_dta) <- c("gg_isopro", class(gg_dta))

  # isopro-specific provenance (the shared .gg_provenance helper only knows
  # about rfsrc / randomForest objects, so build the list inline).
  ntree <- tryCatch(
    as.integer(object$isoforest$ntree),
    error = function(e) NA_integer_
  )
  attr(gg_dta, "provenance") <- list(
    source = "varPro::isopro",
    n      = n,
    ntree  = if (length(ntree) == 1 && !is.na(ntree)) ntree else NA_integer_
  )

  invisible(gg_dta)
}
