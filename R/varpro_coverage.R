##=============================================================================
#' How well a varpro fit tested each variable
#'
#' Reports, for every term varpro could test, how many of the forest's trees
#' hold a usable release rule for it, next to its importance z.  Use it to
#' tell a variable that was tested and found unimportant from one that was
#' barely tested at all.
#'
#' @section Why coverage matters:
#' \code{varPro::importance()} scores a term by a trimmed (winsorised) mean of
#' its importance in each tree, and a tree without a usable release rule for
#' the term counts as zero.  A term tested in fewer than about \code{trim} of
#' the trees therefore has every non-zero value trimmed away and comes back
#' with z exactly 0: it is \emph{untested}, not unimportant.  A term varpro
#' does not return at all was never tested, either because it was cut before
#' rule-building (split weights and \code{nvar}) or because no rule had cases
#' on both sides of its release.  Neither is evidence of no effect.
#'
#' A rule is usable when its out-of-bag region and its released complement
#' both hold cases.  Coverage is the share of scored trees with at least one
#' usable rule for the term.
#'
#' @section Noise reference:
#' varpro's default cutoff (z > 0.79) does not carry from one fit to another
#' with the default \code{local.std = TRUE}: z is then an unstandardised mean,
#' and it shrinks as more terms share each tree.  If the data include
#' permuted copies of real columns (noise by construction), name them with a
#' common prefix and pass a pattern as \code{noise}.  Each real row is then
#' compared with the largest z among \emph{tested} noise rows of the same kind
#' (categorical or continuous); an untested noise row has z = 0 by
#' construction and sets no bar.  A kind with no tested noise falls back to
#' the largest tested-noise z overall.  ggRandomForests does not add the
#' noise columns; do that before fitting.
#'
#' @param object A fitted \code{varpro} object.
#' @param by \code{"term"} (one row per term in the fit's data, factor
#'   levels separately) or \code{"variable"} (one-hot levels rolled up to
#'   their original variable: tested in a tree when any level is, z the
#'   largest level z).
#' @param trim Coverage below which a term counts as untested.  Defaults to
#'   0.1, \code{varPro::importance()}'s winsorisation trim, the share below
#'   which z is exactly 0.
#' @param max.tree Number of trees to score.  Defaults to every tree, so that
#'   the coverage counts and z describe the same trees.
#' @param max.rules.tree Maximum release rules sampled per tree; defaults to
#'   the value stored in \code{object}.  Sampling only happens in trees with
#'   more branches than this.
#' @param noise \code{NULL}, or a regular expression matching the names of
#'   noise variables (for example \code{"^noise"}).
#' @param kind_values A numeric variable with at most this many distinct
#'   values counts as categorical for the noise comparison.
#' @param seed Seed passed to \code{varPro::varpro.strength()}.  For the z
#'   column to be reproducible as well, call \code{set.seed()} first.
#'
#' @return A data frame of class \code{c("varpro_coverage", "data.frame")},
#'   one row per term (or variable), most-covered first, with columns
#'   \code{term} (term rows only), \code{variable}, \code{n_terms} (variable
#'   rows only), \code{eligible} (varpro built rules for it; \code{FALSE}
#'   when the split weights or \code{nvar} cut it first), \code{n_trees} (trees with a usable rule), \code{coverage}
#'   (\code{n_trees} over trees scored), \code{n_rules} (usable rules),
#'   \code{z} (from \code{varPro::importance()}; \code{NA} when never
#'   returned) and \code{tested} (\code{coverage >= trim}).  With
#'   \code{noise} set it adds \code{noise} (logical), \code{kind},
#'   \code{noise_cut} and \code{selected} (a tested real row whose z beats
#'   \code{noise_cut}).  Attributes \code{by}, \code{trees}, \code{trim} and
#'   \code{max.rules.tree} record the settings.
#'
#' @seealso \code{\link{gg_varpro}}, \code{\link{varpro_feature_names}}
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(20261014)
#'   d <- mtcars
#'   d$noise_wt <- sample(d$wt)        # permuted copy: noise by construction
#'   vp <- varPro::varpro(mpg ~ ., data = d, ntree = 50)
#'   set.seed(20261014)
#'   varpro_coverage(vp, noise = "^noise")
#' }
#' }
#'
#' @importFrom varPro importance varpro.strength
#' @export
varpro_coverage <- function(object,
                            by             = c("term", "variable"),
                            trim           = 0.1,
                            max.tree       = object$rf$ntree,
                            max.rules.tree = object$max.rules.tree,
                            noise          = NULL,
                            kind_values    = 5L,
                            seed           = NULL) {
  if (!inherits(object, "varpro")) {
    stop("'object' must be a varpro fit (class \"varpro\").", call. = FALSE)
  }
  by <- match.arg(by)
  if (!is.numeric(trim) || length(trim) != 1L || trim < 0 || trim >= 0.5) {
    stop("'trim' must be a single number in [0, 0.5).", call. = FALSE)
  }
  rule_terms <- object$xvar.names
  terms <- colnames(object$x)
  if (is.null(terms)) terms <- rule_terms

  ## Usable release rules per term: varpro.strength() on the rule forest,
  ## exactly as importance() builds them.
  rf <- object$rf
  rf$x <- object$x
  rf$xvar.names <- rule_terms
  sa <- varPro::varpro.strength(rf, max.rules.tree = max.rules.tree,
                                max.tree = max.tree, seed = seed)$strengthArray
  sa <- sa[sa$oobCT > 0 & sa$compCT > 0, , drop = FALSE]
  trees <- min(max.tree, rf$ntree)
  rule_term <- rule_terms[sa$xReleaseID]

  imp <- varPro::importance(object, plot.it = FALSE, max.tree = max.tree,
                            max.rules.tree = max.rules.tree, trim = trim)
  imp <- .varpro_coverage_imp(imp, object$family)
  z_term <- imp$z[match(terms, rownames(imp))]

  ## Term -> original variable, by the package's own suffix rule.
  org <- object$xvar.org.names
  ds <- stats::setNames(data.frame(matrix(nrow = 0L, ncol = length(org))), org)
  term_var <- vapply(terms, function(t) varpro_feature_names(t, ds)[1L], character(1))

  count_by <- function(group, levels, what) {
    g <- factor(group, levels = levels)
    n <- if (identical(what, "trees")) {
      tapply(sa$treeID, g, function(t) length(unique(t)))
    } else {
      tapply(sa$treeID, g, length)
    }
    n[is.na(n)] <- 0L
    as.integer(n)
  }

  if (by == "term") {
    out <- data.frame(term = terms, variable = unname(term_var),
                      eligible = terms %in% rule_terms,
                      n_trees = count_by(rule_term, terms, "trees"),
                      n_rules = count_by(rule_term, terms, "rules"),
                      z = z_term, stringsAsFactors = FALSE)
  } else {
    vars <- unique(unname(term_var))
    z_var <- vapply(vars, function(v) {
      z <- z_term[term_var == v]
      if (all(is.na(z))) NA_real_ else max(z, na.rm = TRUE)
    }, numeric(1))
    rule_var <- unname(term_var[rule_term])
    out <- data.frame(variable = vars,
                      n_terms = as.integer(table(factor(term_var, levels = vars))),
                      eligible = vapply(vars, function(v) any(terms[term_var == v] %in% rule_terms),
                                        logical(1), USE.NAMES = FALSE),
                      n_trees = count_by(rule_var, vars, "trees"),
                      n_rules = count_by(rule_var, vars, "rules"),
                      z = unname(z_var), stringsAsFactors = FALSE)
  }
  out$coverage <- out$n_trees / trees
  out$tested <- out$coverage >= trim
  out <- out[, c(intersect(c("term", "variable", "n_terms", "eligible", "n_trees", "coverage", "n_rules"),
                           names(out)), "z", "tested")]

  if (!is.null(noise)) {
    out$noise <- grepl(noise, out$variable)
    if (!any(out$noise)) {
      warning("no variable name matches 'noise' = \"", noise, "\".", call. = FALSE)
    } else if (!any(out$noise & out$tested)) {
      warning("no noise variable was tested (coverage >= trim), so the noise ",
              "check is empty: noise_cut is 0 and every tested row is selected.",
              call. = FALSE)
    }
    out$kind <- .varpro_coverage_kind(object, out$variable, kind_values)
    out$noise_cut <- .varpro_coverage_cut(out)
    out$selected <- !out$noise & out$tested & !is.na(out$z) & out$z > out$noise_cut
  }

  out <- out[order(-out$coverage), , drop = FALSE]
  rownames(out) <- NULL
  structure(out, class = c("varpro_coverage", "data.frame"), by = by, trees = trees,
            trim = trim, max.rules.tree = max.rules.tree)
}

## ---- Internal helpers -------------------------------------------------------

#' Unconditional importance table from importance(), whatever the family
#' @keywords internal
.varpro_coverage_imp <- function(imp, family) {
  if (identical(family, "class") && is.list(imp) && !is.data.frame(imp)) {
    imp <- imp$unconditional
  }
  if (identical(family, "regr+") && is.list(imp) && !is.data.frame(imp)) {
    stop("varpro_coverage() does not support multivariate regression fits.", call. = FALSE)
  }
  imp
}

#' Categorical or continuous, per original variable
#' @keywords internal
.varpro_coverage_kind <- function(object, vars, kind_values) {
  factors <- names(attr(object$x, "levels", exact = TRUE))
  vapply(vars, function(v) {
    if (v %in% factors || !v %in% colnames(object$x)) return("categorical")
    n <- length(unique(stats::na.omit(object$x[[v]])))
    if (n <= kind_values) "categorical" else "continuous"
  }, character(1), USE.NAMES = FALSE)
}

#' Largest tested-noise z of the same kind; falls back to all tested noise
#' @keywords internal
.varpro_coverage_cut <- function(out) {
  tn <- out$noise & out$tested & !is.na(out$z)
  all_cut <- max(c(0, out$z[tn]))
  vapply(out$kind, function(k) {
    z <- out$z[tn & out$kind == k]
    if (length(z)) max(z) else all_cut
  }, numeric(1), USE.NAMES = FALSE)
}
