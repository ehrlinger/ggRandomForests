##=============================================================================
#' How well a varpro fit tested each variable
#'
#' Reports, for every term varpro could test, how many of the forest's trees
#' scored it with a release rule, next to its importance z.  Use it to
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
#' both hold cases; it scores the term when its local importance also comes
#' back as a value.  Coverage is the share of scored trees with at least one
#' rule that scored the term, out of the trees z is averaged over (those
#' holding any usable rule).  It counts exactly what \code{\link{gg_varpro}}
#' counts, so on the same rules the two agree on \code{coverage} and
#' \code{tested}.
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
#' @section The first-pass filter:
#' Before it builds any rules, \code{varPro::varpro()} screens the terms with
#' split weights.  With its default \code{split.weight = TRUE} it weights
#' every term (by lasso, variable importance or tree usage), drops any term
#' whose weight is 0, and keeps only the \code{nvar} heaviest (30 by
#' default).  Every other term is ineligible: no rule is ever built for it, so
#' it is untested however strong its effect.  On wide data most terms can be
#' cut here.  Fit with \code{split.weight = FALSE} to make every term eligible;
#' \code{nvar} then has no effect.  varPro's hidden
#' \code{split.weight.tolerance} only floors the weights of the terms that
#' survive and does not change which terms are cut.
#'
#' @param object A fitted \code{varpro} object.
#' @param by \code{"term"} (one row per term in the fit's data, factor
#'   levels separately) or \code{"variable"} (one-hot levels rolled up to
#'   their original variable: tested in a tree when any level is, z the
#'   largest level z).
#' @param trim Coverage below which a term counts as untested, and the
#'   winsorisation trim used for z.  Defaults to 0.1, as in
#'   \code{varPro::importance()}: the share of trees below which z is 0.
#' @param max.tree Number of trees to score.  Defaults to every tree, rather
#'   than varPro's sample of 150, so that coverage rests on all the evidence.
#'   Coverage and z always come from the same extracted rules.
#' @param max.rules.tree Maximum release rules sampled per tree; defaults to
#'   the value stored in \code{object}.  Sampling only happens in trees with
#'   more branches than this.
#' @param noise \code{NULL}, or a regular expression matching the names of
#'   noise variables (for example \code{"^noise"}).
#' @param kind_values A numeric variable with at most this many distinct
#'   values counts as categorical for the noise comparison.
#' @param seed Seed passed to \code{varPro::varpro.strength()}, which samples
#'   the release rules; with a seed (or a \code{set.seed()} call first) the
#'   table is reproducible.
#' @param max_untested \code{NULL} (the default: report only), or the largest
#'   share of real variables, from 0 to 1, that may be untested.  Rows are
#'   counted as \code{by} lists them, noise rows are left out, and ineligible
#'   rows count as untested.  When the share is larger the call stops with an
#'   error, so that a screen that tested too little cannot be read as one
#'   that found no effect.
#'
#' @return A data frame of class \code{c("varpro_coverage", "data.frame")},
#'   one row per term (or variable), most-covered first, with columns
#'   \code{term} (term rows only), \code{variable}, \code{n_terms} (variable
#'   rows only), \code{eligible} (varpro built rules for it; \code{FALSE}
#'   when the split weights or \code{nvar} cut it first), \code{n_trees}
#'   (trees with a rule that scored it), \code{coverage} (\code{n_trees}
#'   over the trees z is averaged over), \code{n_rules} (rules that scored
#'   it),
#'   \code{z} (computed as \code{varPro::importance()} computes it, with
#'   its default \code{local.std = TRUE}, from the same rules; \code{NA} when never
#'   returned) and \code{tested} (\code{coverage >= trim}).  With
#'   \code{noise} set it adds \code{noise} (logical), \code{kind},
#'   \code{noise_cut} and \code{selected} (a tested real row whose z beats
#'   \code{noise_cut}).  Attributes \code{by}, \code{trees} (the coverage
#'   denominator), \code{trim} and \code{max.rules.tree} record the
#'   settings.
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
#' @importFrom varPro varpro.strength get.varpro.strengthArray local.importance
#' @importFrom varPro importance.varpro.workhorse
#' @export
varpro_coverage <- function(object,
                            by             = c("term", "variable"),
                            trim           = 0.1,
                            max.tree       = object$rf$ntree,
                            max.rules.tree = object$max.rules.tree,
                            noise          = NULL,
                            kind_values    = 5L,
                            seed           = NULL,
                            max_untested   = NULL) {
  if (!inherits(object, "varpro")) {
    stop("'object' must be a varpro fit (class \"varpro\").", call. = FALSE)
  }
  by <- match.arg(by)
  if (!is.numeric(trim) || length(trim) != 1L || trim < 0 || trim >= 0.5) {
    stop("'trim' must be a single number in [0, 0.5).", call. = FALSE)
  }
  .varpro_coverage_check_max(max_untested)
  rule_terms <- object$xvar.names
  terms <- colnames(object$x)
  if (is.null(terms)) terms <- rule_terms

  ## One extraction of release rules feeds both coverage and z, so the two
  ## always describe the same sampled rules, and coverage counts what z
  ## averages: scored rules, over the trees z is averaged over.
  ext <- .varpro_coverage_extract(object, max.tree, max.rules.tree, trim, seed)
  sa <- ext$rules
  trees <- ext$trees
  rule_term <- rule_terms[sa$xReleaseID]

  imp <- .varpro_coverage_imp(ext$importance, object$family)
  z_term <- imp$z[match(terms, rownames(imp))]

  ## Term -> original variable, from varPro's stored hot-encoding map.
  term_var <- .varpro_coverage_map(object, terms)

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

  if (!is.null(max_untested)) .varpro_coverage_guard(out, max_untested, trim)

  out <- out[order(-out$coverage), , drop = FALSE]
  rownames(out) <- NULL
  structure(out, class = c("varpro_coverage", "data.frame"), by = by, trees = trees,
            trim = trim, max.rules.tree = max.rules.tree)
}

## ---- Internal helpers -------------------------------------------------------

#' One release-rule extraction, scored as importance() scores it
#'
#' Mirrors the default (\code{local.std = TRUE}) path of
#' \code{varPro::importance()}: rules from \code{varpro.strength()} with
#' memberships, local importance for each usable rule (out-of-bag and
#' complement cases both present), then varPro's own aggregation.  Returns
#' the usable rules and the importance table built from exactly those rules.
#' @keywords internal
.varpro_coverage_extract <- function(object, max.tree, max.rules.tree, trim, seed) {
  ext <- .varpro_score_rules(object, max.tree, max.rules.tree, seed, local.std = TRUE)
  importance <- varPro::importance.varpro.workhorse(ext$scored, cutoff = 0.79, trim = trim,
                                                    plot.it = FALSE, conf = TRUE, sort = TRUE,
                                                    ylab = "", local.std = TRUE)
  ## Usable rules whose local importance came back NA add nothing to z, so
  ## they do not count as testing the term (they still count as rows of the
  ## scored table, so their trees stay in the denominator, as they do in z).
  results <- ext$scored$results
  scored  <- if (nrow(ext$rules) == nrow(results)) !is.na(results$imp) else
    logical(nrow(ext$rules))
  list(rules = ext$rules[scored, , drop = FALSE], importance = importance,
       trees = length(unique(results$tree)))
}

#' Re-extract release rules and score them as importance() does
#'
#' The rule-sampling half of \code{varPro::importance()}.  With
#' \code{local.std = TRUE} each usable rule (out-of-bag and complement cases
#' both present) gets its local importance and only those rules are kept; with
#' \code{FALSE} the strength array's own importance is used, unfiltered.
#' Returns the usable rules and a copy of \code{object} whose \code{$results}
#' holds the scored rules, ready for \code{importance.varpro.workhorse()}.
#' @keywords internal
.varpro_score_rules <- function(object, max.tree, max.rules.tree, seed = NULL,
                                local.std = TRUE) {
  rf <- object$rf
  rf$x <- object$x
  rf$xvar.names <- object$xvar.names
  oo <- varPro::varpro.strength(rf, max.rules.tree = max.rules.tree, max.tree = max.tree,
                                membership = TRUE, seed = seed)
  sa <- oo$strengthArray
  keep <- which(sa$oobCT > 0 & sa$compCT > 0)

  results <- varPro::get.varpro.strengthArray(sa, object$family, object$y)
  if (local.std) {
    imp_cols <- grepl("imp", colnames(results))
    results[, imp_cols] <- NA
    y <- object$y
    attr(y, "y.org") <- object$y.org
    attr(y, "family") <- object$family
    if (length(keep) > 0L) {
      imp <- lapply(keep, function(i) {
        varPro::local.importance(y, oo$oobMembership[[i]], oo$compMembership[[i]])
      })
      results[keep, imp_cols] <- if (sum(imp_cols) == 1L) unlist(imp) else do.call(rbind, imp)
      results <- results[keep, , drop = FALSE]
    }
  }
  scored <- object
  scored$results <- results
  scored$max.rules.tree <- max.rules.tree
  scored$max.tree <- max.tree
  list(rules = sa[keep, , drop = FALSE], scored = scored)
}

#' Original variable of each encoded term, from varPro's stored map
#'
#' varPro stores the term-to-variable map as the \code{"xvar.map"} attribute
#' of \code{object$x}.  Suffix stripping cannot be trusted: a factor
#' \code{g} with a level encoded \code{ga.1} and a numeric \code{ga} collide.
#' @keywords internal
.varpro_coverage_map <- function(object, terms) {
  map <- attr(object$x, "xvar.map", exact = TRUE)
  if (!is.null(map)) {
    if (anyNA(match(terms, names(map)))) {
      stop("varPro's stored encoding map does not cover every term of this fit.", call. = FALSE)
    }
    return(stats::setNames(unname(map[terms]), terms))
  }
  if (!isTRUE(attr(object$x, "hotencode", exact = TRUE))) {
    return(stats::setNames(terms, terms))
  }
  stop("this varpro fit has one-hot encoded factors but no stored encoding map ",
       "(attribute 'xvar.map'); refit it with varPro >= 3.3.0.", call. = FALSE)
}

#' Validate max_untested before the rules are extracted
#' @keywords internal
.varpro_coverage_check_max <- function(max_untested) {
  if (is.null(max_untested)) return(invisible(NULL))
  if (!is.numeric(max_untested) || length(max_untested) != 1L || is.na(max_untested) ||
        max_untested < 0 || max_untested > 1) {
    stop("'max_untested' must be NULL or a single number in [0, 1].", call. = FALSE)
  }
  invisible(NULL)
}

#' Stop when too many real variables went untested
#'
#' \code{out} is the coverage table before sorting; it has \code{tested},
#' and \code{noise} only when a noise pattern was given.  Returns
#' \code{invisible(NULL)} when the screen passes.
#' @keywords internal
.varpro_coverage_guard <- function(out, max_untested, trim) {
  real <- if ("noise" %in% names(out)) !out$noise else rep(TRUE, nrow(out))
  ## Nothing real to judge (the noise pattern matched every row) is not a pass.
  if (!any(real)) {
    stop("'max_untested' has no real variables to judge: 'noise' matches every row.",
         call. = FALSE)
  }
  ## A row no tree scored (every ineligible row among them) is untested even
  ## when trim = 0 makes `tested` TRUE for it.
  untested <- !out$tested | out$n_trees == 0L
  n_untested <- sum(untested[real])
  if (n_untested / sum(real) > max_untested) {
    stop(n_untested, " of ", sum(real), " real variables are untested (coverage < trim = ",
         trim, ", or no tree scored them), more than max_untested = ", max_untested, " allows. Untested is not ",
         "unimportant: refit with split.weight = FALSE so every variable is eligible; see ",
         "the \"first-pass filter\" section of ?varpro_coverage.", call. = FALSE)
  }
  invisible(NULL)
}

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
