##=============================================================================
#' Variable importance data from a varPro model
#'
#' Pulls the per-tree importance scores out of a fitted \code{varpro} object
#' and summarises them into a data structure the plot method can draw as a
#' boxplot.  The box hinges are the 15th and 85th percentiles and the
#' whiskers run to the 5th and 95th -- not the usual Tukey 1.5 IQR whiskers.
#' For a classification forest you can also keep the class-conditional
#' importances.
#'
#' @section What varpro is doing:
#' Permutation importance asks "what happens to OOB accuracy when I scramble
#' this variable?" That works, but it leans on artificial data — the
#' permuted column — and the answer can be unstable when variables are
#' correlated. The varpro framework (Lu and Ishwaran, 2024) replaces
#' permutation with \emph{release rules}. The forest is grown with guided
#' splitting; from a subset of trees varpro samples a collection of
#' decision-rule branches; for each variable it then compares the
#' response inside the rule's region to the response after the rule's
#' constraint on that variable is "released". The size of that change,
#' aggregated over many rules and trees, is the variable's importance.
#' No synthetic covariates, no permutation — the contrast is between two
#' real subsets of the data.
#'
#' Because varpro builds importance from rules sampled over trees, every
#' tree contributes its own importance value for each variable. Those are
#' the per-tree scores we summarise here. With \code{local.std = TRUE}
#' (the default) the per-tree values are standardised by their column
#' standard deviation so the column mean equals the aggregate z-score
#' returned by \code{varPro::importance()}; that z-score is the canonical
#' "is this variable in or out?" statistic, and \code{cutoff = 0.79} is
#' varpro's default selection threshold.
#'
#' For a classification forest, varpro also returns a class-conditional
#' z table: the same importance computed restricting attention to rules
#' relevant to each class. \code{conditional = TRUE} keeps that table so
#' the plot method can show which variables matter for which class
#' rather than only in aggregate.
#'
#' @section What's in the output:
#' \code{$imp} is the one-row-per-variable summary: aggregate z from
#' \code{varPro::importance()}, plus a \code{selected} flag for
#' \code{z > cutoff}. \code{$stats} holds the box quantiles
#' (5/15/50/85/95 percentiles, plus the raw mean) computed from the
#' per-tree matrix; these are what the boxplot draws. \code{$imp.tree}
#' is the per-tree matrix itself, kept only when \code{faithful = TRUE}
#' so the plot method can scatter individual tree values over the box.
#' \code{$conditional} is the tidy class x variable z table, present
#' only when \code{conditional = TRUE} and the family is
#' classification.
#'
#' @section What you use this for:
#' \itemize{
#'   \item rank candidate variables by importance and pick a working set
#'     above varpro's z cutoff;
#'   \item see, via the boxplot's spread and the per-tree points
#'     (\code{faithful = TRUE}), how stable each variable's importance
#'     is across trees — a high median with a wide box is a different
#'     story from a high median with a tight box;
#'   \item for a classification forest, ask which variables drive which
#'     class (\code{conditional = TRUE}) rather than just which
#'     variables drive the model overall.
#' }
#' The z-score is a standardised ranking statistic, not a p-value or a
#' probability. Two variables with the same z are "similarly important
#' by this method", not "equally likely to be true signal". For a
#' data-driven cutoff rather than the 0.79 default, see
#' \code{varPro::cv.varpro}.
#'
#' @references
#' Lu, M. and Ishwaran, H. (2024). Model-independent variable selection
#' via the rule-based variable priority framework. \emph{arXiv preprint}
#' \href{https://arxiv.org/abs/2409.09003}{arXiv:2409.09003}.
#'
#' @param object A fitted \code{varpro} object (required).
#' @param local.std Logical; default \code{TRUE}.  When \code{TRUE} the
#'   per-tree importances are put on the z-scale before the box statistics
#'   are computed.  Set it \code{FALSE} to keep the raw importance scale,
#'   which is what \code{type = "raw"} in \code{plot.gg_varpro} needs.
#' @param cutoff Numeric; the z-score above which a variable is treated as
#'   selected.  Default \code{0.79}.  A variable with aggregate z above the
#'   cutoff is flagged \code{selected = TRUE} in \code{$imp}.
#' @param faithful Logical; default \code{FALSE}.  When \code{TRUE},
#'   \code{$imp.tree} is kept so \code{plot.gg_varpro} can scatter the
#'   per-tree points over the box.
#' @param conditional Logical; default \code{FALSE}.  When \code{TRUE}, and
#'   only for a classification forest, the \code{$conditional.z} matrix is
#'   extracted and stored as \code{$conditional}.
#' @param nvar Integer; keep only the top \code{nvar} variables, ranked by
#'   median per-tree z, after the cutoff filter has been applied.
#'   \code{NULL} keeps all of them.
#' @param ... Additional arguments passed to \code{varPro::importance()}.
#'
#' @return A named list of class \code{"gg_varpro"} with elements:
#' \describe{
#'   \item{\code{$imp}}{Summary data frame: \code{variable} (factor whose
#'     levels run least- to most-important by median per-tree z, so the
#'     most-important variable sits at the top of the plot after
#'     \code{coord_flip}), \code{z} (aggregate
#'     z-score from \code{importance()}), \code{selected} (logical,
#'     \code{z > cutoff}).}
#'   \item{\code{$imp.tree}}{\code{NULL} when \code{faithful = FALSE};
#'     otherwise an ntree x p matrix of per-tree importance values.}
#'   \item{\code{$stats}}{Per-variable summary: \code{variable},
#'     \code{median}, \code{q05}, \code{q15}, \code{q85}, \code{q95}
#'     (on z-scale when \code{local.std = TRUE}, raw when \code{FALSE}),
#'     plus \code{mean} (raw importance mean, always stored).}
#'   \item{\code{$conditional}}{\code{NULL} when \code{conditional = FALSE};
#'     otherwise a data frame with columns \code{variable}, \code{class},
#'     \code{z} (one row per variable x class combination).}
#' }
#' A \code{"provenance"} attribute carries \code{family}, \code{local.std},
#' \code{cutoff}, \code{faithful}, \code{conditional}, \code{xvar.names},
#' and \code{n}.
#'
#' @seealso \code{\link{plot.gg_varpro}}, \code{\link{gg_vimp}}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#' gg <- gg_varpro(vp)
#' print(gg)
#' plot(gg)
#' }
#'
#' @importFrom varPro importance
#' @export
gg_varpro <- function(object,
                      local.std   = TRUE,
                      cutoff      = 0.79,
                      faithful    = FALSE,
                      conditional = FALSE,
                      nvar        = NULL,
                      ...) {

  ## ---- Validation + coercion ------------------------------------------------
  local.std <- .validate_varpro_imp_inputs(object, local.std, faithful, conditional)

  ## ---- Call importance (local.std=FALSE gives mean & std columns) -----------
  imp_out <- varPro::importance(object, local.std = FALSE, ...)

  ## ---- Build per-tree importance matrix from object$results -----------------
  imp_tree_mat <- .build_varpro_imp_tree(object)

  ## ---- Build tidy data structures ------------------------------------------
  dfs <- .build_varpro_imp_dfs(imp_out, imp_tree_mat, object$family,
                                cutoff, nvar, faithful, local.std, conditional)

  ## ---- Assemble result ------------------------------------------------------
  result <- structure(
    list(
      imp         = dfs$imp,
      imp.tree    = dfs$imp_tree,
      stats       = dfs$stats,
      conditional = dfs$conditional
    ),
    class = c("gg_varpro", "list")
  )

  attr(result, "provenance") <- list(
    family      = object$family,
    local.std   = local.std,  # resolved value (may differ from arg when faithful=TRUE)
    cutoff      = cutoff,
    faithful    = faithful,
    conditional = conditional,
    xvar.names  = object$xvar.names,
    n           = nrow(object$x)
  )

  result
}

## ---- Internal helpers -------------------------------------------------------

#' @keywords internal
.validate_varpro_imp_inputs <- function(object, local.std, faithful, conditional) {
  if (missing(object) || is.null(object)) {
    stop("'object' must be a fitted varpro object.", call. = FALSE)
  }
  if (!inherits(object, "varpro")) {
    stop("'object' must be a varpro fit (class \"varpro\").", call. = FALSE)
  }
  if (conditional && !identical(object$family, "class")) {
    stop("conditional=TRUE requires a classification forest ",
         "(object$family == \"class\").", call. = FALSE)
  }
  invisible(local.std)
}

#' Build per-tree importance matrix from varpro object results
#'
#' Reconstructs the ntree x p per-tree importance matrix from
#' \code{object$results}, replicating the aggregation performed internally
#' by \code{varPro:::.importance.varpro.workhorse}.
#' @keywords internal
.build_varpro_imp_tree <- function(object) {
  dta        <- object$results
  xvar.names <- object$xvar.names

  dta$variable <- factor(xvar.names[dta$variable])
  xvarused.names <- levels(dta$variable)
  p          <- length(xvarused.names)
  wt.vec     <- rep(1, p)
  names(wt.vec) <- xvarused.names

  trees       <- sort(unique(dta$tree))
  ntree.used  <- length(trees)
  tree.idx    <- match(dta$tree, trees)
  var.idx     <- match(as.character(dta$variable), xvarused.names)
  grp         <- tree.idx + (var.idx - 1L) * ntree.used

  w          <- dta$n.oob
  numer      <- dta$imp * w
  numer[is.na(numer)] <- 0
  numer.sum  <- rowsum(numer, grp, reorder = FALSE)
  denom.sum  <- rowsum(w,     grp, reorder = FALSE)
  ratio      <- as.numeric(numer.sum[, 1L] / denom.sum[, 1L])
  ratio[!is.finite(ratio)] <- 0

  imp_tree <- matrix(0, nrow = ntree.used, ncol = p,
    dimnames = list(as.character(trees), xvarused.names))
  ## g.rows are the grp keys (1..ntree.used*p); decode back to (tree, var) indices.
  ## reorder=FALSE preserves insertion order but rownames() always give the
  ## original grp key values, so the modular decode is stable regardless of order.
  g.rows   <- as.integer(rownames(numer.sum))
  imp_tree[cbind(((g.rows - 1L) %% ntree.used) + 1L,
                 ((g.rows - 1L) %/% ntree.used) + 1L)] <- ratio
  sweep(imp_tree, 2L, wt.vec, `*`)
}

#' Compute per-variable box statistics from a per-tree importance matrix
#'
#' When \code{local.std = TRUE} the columns of \code{mat} are standardised
#' to unit variance before computing quantiles so that the display scale
#' matches the aggregate z-score.  The \code{mean} column always stores raw
#' (unstandardised) column means.
#' @keywords internal
.varpro_imp_stats <- function(mat, local.std = TRUE) {
  if (local.std) {
    sd_j <- apply(mat, 2L, stats::sd, na.rm = TRUE)
    sd_j[sd_j < .Machine$double.eps] <- 1   # guard zero-variance columns
    # z_ij = imp_ij / sd_j so that mean(z_ij) == aggregate z_j from importance()
    display_mat <- sweep(mat, 2L, sd_j, FUN = "/")
  } else {
    display_mat <- mat
  }
  data.frame(
    variable = colnames(mat),
    median   = apply(display_mat, 2L, stats::quantile, probs = 0.50,
                     na.rm = TRUE),
    q05      = apply(display_mat, 2L, stats::quantile, probs = 0.05,
                     na.rm = TRUE),
    q15      = apply(display_mat, 2L, stats::quantile, probs = 0.15,
                     na.rm = TRUE),
    q85      = apply(display_mat, 2L, stats::quantile, probs = 0.85,
                     na.rm = TRUE),
    q95      = apply(display_mat, 2L, stats::quantile, probs = 0.95,
                     na.rm = TRUE),
    mean     = colMeans(mat, na.rm = TRUE),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Build the tidy importance data structures from importance() output
#' @keywords internal
.build_varpro_imp_dfs <- function(imp_out, imp_tree_mat, family, cutoff, nvar,
                                   faithful, local.std, conditional) {
  is_class <- identical(family, "class")

  ## Unpack importance() data frame — different shapes per family
  if (is_class && is.list(imp_out) && !is.data.frame(imp_out)) {
    imp_df_raw <- imp_out$unconditional
    cond_mat   <- if (conditional) imp_out$conditional.z else NULL
  } else {
    imp_df_raw <- imp_out
    cond_mat   <- NULL
  }

  ## Replace NaN z with 0 for sorting/selection
  z_vec <- imp_df_raw$z
  z_vec[!is.finite(z_vec)] <- 0

  ## $imp: one row per variable
  imp_df <- data.frame(
    variable = rownames(imp_df_raw),
    z        = as.numeric(z_vec),
    selected = as.numeric(z_vec) > cutoff,
    stringsAsFactors = FALSE
  )

  ## Keep only columns present in both imp_df and imp_tree_mat
  keep_vars  <- imp_df$variable[imp_df$variable %in% colnames(imp_tree_mat)]
  imp_tree_k <- imp_tree_mat[, keep_vars, drop = FALSE]

  ## $stats: box quantiles per variable (full set before nvar truncation)
  stats_df <- .varpro_imp_stats(imp_tree_k, local.std = local.std)

  ## Apply nvar truncation by median z (spec: "top-nvar by median z")
  if (!is.null(nvar)) {
    top_vars <- stats_df$variable[order(-stats_df$median)][
      seq_len(min(nvar, nrow(stats_df)))]
    imp_df   <- imp_df[imp_df$variable %in% top_vars, , drop = FALSE]
    stats_df <- stats_df[stats_df$variable %in% top_vars, , drop = FALSE]
  }

  ## Factor levels run least-important-first (reversed descending median z)
  ## so the most-important variable lands at the TOP after coord_flip in the
  ## plot method, matching the gg_vimp convention. `var_order` itself stays
  ## most-important-first for row ordering and top-nvar selection.
  var_order         <- stats_df$variable[order(-stats_df$median)]
  lvl               <- rev(as.character(var_order))
  imp_df$variable   <- factor(imp_df$variable,   levels = lvl)
  stats_df$variable <- factor(stats_df$variable, levels = lvl)
  ## Reorder rows to match factor levels (avoids row-order / level mismatch)
  imp_df   <- imp_df[order(imp_df$variable), , drop = FALSE]
  stats_df <- stats_df[order(stats_df$variable), , drop = FALSE]
  rownames(imp_df)   <- NULL
  rownames(stats_df) <- NULL

  ## $conditional: tidy class-conditional z-scores
  cond_df <- NULL
  if (!is.null(cond_mat)) {
    cond_df <- data.frame(
      variable = rep(rownames(cond_mat), ncol(cond_mat)),
      class    = rep(colnames(cond_mat), each = nrow(cond_mat)),
      z        = as.vector(cond_mat),
      stringsAsFactors = FALSE
    )
    cond_df <- cond_df[cond_df$variable %in% keep_vars, , drop = FALSE]
    cond_df$variable <- factor(cond_df$variable,
                               levels = levels(imp_df$variable))
  }

  list(
    imp         = imp_df,
    imp_tree    = if (faithful) imp_tree_k else NULL,
    stats       = stats_df,
    conditional = cond_df
  )
}
