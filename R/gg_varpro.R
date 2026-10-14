##=============================================================================
#' Variable importance data from a varPro model
#'
#' Pulls the per-tree importance scores out of a fitted \code{varpro} object
#' and summarizes them into a data structure the plot method can draw as a
#' boxplot.  The box hinges are the 15th and 85th percentiles and the
#' whiskers run to the 5th and 95th, not the usual Tukey 1.5 IQR whiskers.
#' For a classification forest you can also keep the class-conditional
#' importances.
#'
#' @section What varpro is doing:
#' Permutation importance asks "what happens to OOB accuracy when I scramble
#' this variable?" That works, but it leans on artificial data (the
#' permuted column) and the answer can be unstable when variables are
#' correlated. The varpro framework (Lu and Ishwaran, 2024) replaces
#' permutation with \emph{release rules}. The forest is grown with guided
#' splitting; from a subset of trees varpro samples a collection of
#' decision-rule branches; for each variable it then compares the
#' response inside the rule's region to the response after the rule's
#' constraint on that variable is "released". The size of that change,
#' aggregated over many rules and trees, is the variable's importance.
#' No synthetic covariates, no permutation: the contrast is between two
#' real subsets of the data.
#'
#' Because varpro builds importance from rules sampled over trees, every
#' tree contributes its own importance value for each variable. Those are
#' the per-tree scores we summarize here, and the aggregate z comes from
#' the same rules on the same trees, so the boxes and z always agree.
#' \code{varPro::importance()} takes a winsorised mean over trees (trimmed
#' by \code{trim}), counting a tree with no usable release rule for the
#' variable as 0. With \code{local.std = TRUE} (the default, as in varPro)
#' the release rules are re-sampled and scored by their local importance,
#' the standard deviation is fixed at 1, and z is that winsorised mean, so
#' the per-tree values already sit on z's scale. With \code{local.std =
#' FALSE} the fit-time rules are used and z is the winsorised mean divided
#' by the winsorised standard deviation. \code{cutoff = 0.79} is varpro's
#' default selection threshold, but the scale of z depends on how many
#' variables share each tree, so the same cutoff does not mean the same
#' thing across settings.
#'
#' A variable scored in fewer than about \code{trim} of the trees (a tree
#' scores it when one of its release rules for the variable returns an
#' importance value) is \emph{untested}: its z is 0 (or undefined with
#' \code{local.std = FALSE}) because every non-zero tree is winsorised
#' away, not because it was tested and found unimportant. \code{coverage}
#' and \code{tested} in the output say which variables those are, and the
#' plot draws them as open boxes. \code{\link{varpro_coverage}} counts
#' coverage the same way and adds a per-variable roll-up and a noise-based
#' cutoff.
#'
#' For a classification forest, varpro also returns a class-conditional
#' z table: the same importance computed restricting attention to rules
#' relevant to each class. \code{conditional = TRUE} keeps that table so
#' the plot method can show which variables matter for which class
#' rather than only in aggregate.
#'
#' @section What's in the output:
#' \code{$imp} is the one-row-per-variable summary: aggregate z from
#' varPro's importance aggregation, a \code{selected} flag for
#' \code{z > cutoff}, and the variable's \code{coverage} and
#' \code{tested} flag. \code{$stats} holds the box quantiles
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
#'     is across trees: a high median with a wide box is a different
#'     story from a high median with a tight box;
#'   \item for a classification forest, ask which variables drive which
#'     class (\code{conditional = TRUE}) rather than just which
#'     variables drive the model overall.
#' }
#' The z-score is a standardized ranking statistic, not a p-value or a
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
#' @param local.std Logical; default \code{TRUE}.  Passed to varPro's
#'   importance aggregation, with the same meaning as in
#'   \code{varPro::importance()}: \code{TRUE} re-samples the release rules,
#'   scores them by local importance and reports z as their winsorised mean,
#'   so the boxes are on z's scale.  \code{FALSE} uses the fit-time rules and
#'   standardises z by the winsorised standard deviation; the boxes are then
#'   on the raw importance scale, which is what \code{type = "raw"} in
#'   \code{plot.gg_varpro} needs.  With \code{TRUE} the result depends on
#'   the random number stream, so call \code{set.seed()} first for a
#'   reproducible z.
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
#' @param trim Winsorising fraction used for z, as in
#'   \code{varPro::importance()}; default \code{0.1}.  A variable is
#'   \code{tested} when its coverage is at least \code{trim}.
#' @param max.tree,max.rules.tree Number of trees, and release rules per
#'   tree, to score.  Default to the values stored in the fit.  Supplying
#'   either one re-samples the rules even with \code{local.std = FALSE}, as
#'   \code{varPro::importance()} does.  The boxes, z and coverage are always
#'   computed from the same rules.
#' @param ... Not used.  Earlier versions passed it to
#'   \code{varPro::importance()}; \code{trim}, \code{max.tree} and
#'   \code{max.rules.tree} are now arguments, and anything else is an error.
#'
#' @return A named list of class \code{"gg_varpro"} with elements:
#' \describe{
#'   \item{\code{$imp}}{Summary data frame: \code{variable} (factor whose
#'     levels run least- to most-important by median per-tree z, so the
#'     most-important variable sits at the top of the plot after
#'     \code{coord_flip}), \code{z} (aggregate
#'     z-score, \code{NA} when it is undefined), \code{selected} (logical,
#'     \code{z > cutoff}), \code{coverage} (share of the scored trees in
#'     which a release rule for the variable returned an importance value)
#'     and \code{tested}
#'     (\code{coverage >= trim}).}
#'   \item{\code{$imp.tree}}{\code{NULL} when \code{faithful = FALSE};
#'     otherwise an ntree x p matrix of per-tree importance values.}
#'   \item{\code{$stats}}{Per-variable summary: \code{variable},
#'     \code{median}, \code{q05}, \code{q15}, \code{q85}, \code{q95}
#'     of the per-tree values (on z's scale when \code{local.std = TRUE},
#'     raw when \code{FALSE}), \code{mean} (their plain mean), and
#'     \code{coverage} and \code{tested} as in \code{$imp}.}
#'   \item{\code{$conditional}}{\code{NULL} when \code{conditional = FALSE};
#'     otherwise a data frame with columns \code{variable}, \code{class},
#'     \code{z} (one row per variable x class combination). \code{class} is
#'     a factor of the outcome's own level names, also for a binary outcome,
#'     which varPro recodes to 0/1 internally.}
#' }
#' A \code{"provenance"} attribute carries \code{family}, \code{local.std},
#' \code{cutoff}, \code{faithful}, \code{conditional}, \code{xvar.names},
#' \code{n}, \code{trim}, \code{max.tree} and \code{max.rules.tree}.
#'
#' @seealso \code{\link{plot.gg_varpro}}, \code{\link{varpro_coverage}},
#'   \code{\link{gg_vimp}}
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
#' @importFrom varPro importance.varpro.workhorse
#' @export
gg_varpro <- function(object,
                      local.std      = TRUE,
                      cutoff         = 0.79,
                      faithful       = FALSE,
                      conditional    = FALSE,
                      nvar           = NULL,
                      trim           = 0.1,
                      max.tree       = object$max.tree,
                      max.rules.tree = object$max.rules.tree,
                      ...) {

  ## ---- Validation + coercion ------------------------------------------------
  local.std <- .validate_varpro_imp_inputs(object, local.std, faithful, conditional,
                                           trim, ...)

  ## ---- Score one set of rules, as varPro::importance() would (#340) ---------
  ## z, the per-tree matrix and coverage all come from `scored$results`, so the
  ## boxes and z describe the same trees.
  scored <- object
  if (local.std || !missing(max.tree) || !missing(max.rules.tree)) {
    if (is.null(object$rf)) {
      stop("gg_varpro(): this varpro fit has no forest ($rf), so its release ",
           "rules cannot be re-sampled, which local.std = TRUE, max.tree and ",
           "max.rules.tree all need. Use local.std = FALSE to score the ",
           "fit-time rules.", call. = FALSE)
    }
    scored <- .varpro_score_rules(object, max.tree, max.rules.tree,
                                  local.std = local.std)$scored
  }
  imp_out <- varPro::importance.varpro.workhorse(
    scored, cutoff = cutoff, trim = trim, plot.it = FALSE, conf = TRUE,
    sort = TRUE, ylab = "", local.std = local.std)

  imp_tree_mat <- .build_varpro_imp_tree(scored)
  coverage     <- .varpro_imp_coverage(scored, rownames(imp_tree_mat))

  ## ---- Build tidy data structures ------------------------------------------
  dfs <- .build_varpro_imp_dfs(imp_out, imp_tree_mat, object$family,
                                cutoff, nvar, faithful, conditional,
                                coverage = coverage, trim = trim)

  ## varPro recodes a binary outcome to 0/1 and names the conditional columns
  ## after those codes; report the original class names, as gg_ivarpro() does.
  ## Only binary outcomes are recoded, so only they are mapped: a multiclass
  ## outcome whose own labels are "0", "1", ... must be left alone.
  if (!is.null(dfs$conditional)) {
    lv    <- .ivarpro_class_levels(object)
    codes <- as.character(seq_along(lv) - 1L)
    cls   <- dfs$conditional$class
    if (length(lv) == 2L && all(cls %in% codes)) cls <- lv[match(cls, codes)]
    dfs$conditional$class <- factor(cls, levels = intersect(lv, cls))
  }

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
    n           = nrow(object$x),
    trim        = trim,
    max.tree       = scored$max.tree,
    max.rules.tree = scored$max.rules.tree
  )

  result
}

## ---- Internal helpers -------------------------------------------------------

#' @keywords internal
.validate_varpro_imp_inputs <- function(object, local.std, faithful, conditional,
                                        trim = 0.1, ...) {
  if (...length() > 0L) {
    stop("gg_varpro(): unused argument(s): ",
         paste(names(list(...)), collapse = ", "),
         ". Only trim, max.tree and max.rules.tree are passed to varPro.",
         call. = FALSE)
  }
  if (!is.numeric(trim) || length(trim) != 1L || trim < 0 || trim >= 0.5) {
    stop("'trim' must be a single number in [0, 0.5).", call. = FALSE)
  }
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

#' Share of scored trees with a usable release rule, per variable
#'
#' A rule is usable when varPro scored it (non-missing \code{imp}).  The
#' denominator is the trees in \code{scored$results}, the same trees the
#' per-tree matrix and z are built from.
#' @keywords internal
.varpro_imp_coverage <- function(scored, trees) {
  dta  <- scored$results
  used <- !is.na(dta$imp)
  vars <- scored$xvar.names[dta$variable[used]]
  n    <- tapply(dta$tree[used], vars, function(t) length(unique(t)))
  stats::setNames(as.numeric(n) / length(trees), names(n))
}

#' Compute per-variable box statistics from a per-tree importance matrix
#'
#' Quantiles of the per-tree values as they are.  With varPro's
#' \code{local.std = TRUE} those values are already on z's scale (z is their
#' winsorised mean); with \code{FALSE} they are raw importances.
#' @keywords internal
.varpro_imp_stats <- function(mat) {
  data.frame(
    variable = colnames(mat),
    median   = apply(mat, 2L, stats::quantile, probs = 0.50,
                     na.rm = TRUE),
    q05      = apply(mat, 2L, stats::quantile, probs = 0.05,
                     na.rm = TRUE),
    q15      = apply(mat, 2L, stats::quantile, probs = 0.15,
                     na.rm = TRUE),
    q85      = apply(mat, 2L, stats::quantile, probs = 0.85,
                     na.rm = TRUE),
    q95      = apply(mat, 2L, stats::quantile, probs = 0.95,
                     na.rm = TRUE),
    mean     = colMeans(mat, na.rm = TRUE),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Build the tidy importance data structures from importance() output
#' @keywords internal
.build_varpro_imp_dfs <- function(imp_out, imp_tree_mat, family, cutoff, nvar,
                                   faithful, conditional, coverage = NULL,
                                   trim = 0.1) {
  is_class <- identical(family, "class")

  ## Unpack importance() data frame: different shapes per family
  if (is_class && is.list(imp_out) && !is.data.frame(imp_out)) {
    imp_df_raw <- imp_out$unconditional
    cond_mat   <- if (conditional) imp_out$conditional.z else NULL
  } else {
    imp_df_raw <- imp_out
    cond_mat   <- NULL
  }

  vars_raw <- rownames(imp_df_raw)
  z_vec    <- imp_df_raw$z

  ## Guard (issue #118): varPro::importance() occasionally returns a
  ## degenerate importance table -- 0 rows, or a missing/empty `z` column
  ## while p variables are named -- for some fits (observed on survival fits
  ## where the release-rule step selects no variables). Detect that here and
  ## fail with a clear, specific message instead of the cryptic
  ## "arguments imply differing number of rows: p, 0" raised when the columns
  ## below are recycled. Scoped to the degenerate case only: working fits
  ## (survival included) have length(z_vec) == length(vars_raw) > 0 and are
  ## unaffected -- this is NOT a blanket survival-family block (cf. #116).
  if (length(vars_raw) == 0L || length(z_vec) != length(vars_raw)) {
    stop("gg_varpro(): varPro::importance() returned no usable importance ",
         "for this ", family, " fit -- the release-rule step selected no ",
         "variables, so the importance table is empty and there is nothing ",
         "to plot. This is most often seen on survival fits; refitting with ",
         "more trees (a larger 'ntree') usually resolves it.",
         call. = FALSE)
  }

  ## An undefined z (0/0 for an untested variable when local.std = FALSE) is
  ## reported as NA, not 0: "untested" is not "tested and unimportant" (#340).
  z_vec <- as.numeric(z_vec)
  z_vec[!is.finite(z_vec)] <- NA_real_

  ## $imp: one row per variable
  imp_df <- data.frame(
    variable = vars_raw,
    z        = z_vec,
    selected = !is.na(z_vec) & z_vec > cutoff,
    stringsAsFactors = FALSE
  )
  cov_j           <- unname(coverage[vars_raw])
  imp_df$coverage <- if (is.null(coverage)) NA_real_ else ifelse(is.na(cov_j), 0, cov_j)
  imp_df$tested   <- imp_df$coverage >= trim

  ## Keep only columns present in both imp_df and imp_tree_mat
  keep_vars  <- imp_df$variable[imp_df$variable %in% colnames(imp_tree_mat)]
  imp_tree_k <- imp_tree_mat[, keep_vars, drop = FALSE]

  ## $stats: box quantiles per variable (full set before nvar truncation)
  stats_df <- .varpro_imp_stats(imp_tree_k)
  stats_df <- cbind(stats_df,
                    imp_df[match(stats_df$variable, imp_df$variable),
                           c("coverage", "tested")])

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
  ## Rows stay most-important-first (descending median z); the reversed
  ## factor levels above only drive the plot's vertical order, not row order.
  imp_df   <- imp_df[order(imp_df$variable,   decreasing = TRUE), , drop = FALSE]
  stats_df <- stats_df[order(stats_df$variable, decreasing = TRUE), , drop = FALSE]
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
