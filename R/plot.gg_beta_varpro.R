##=============================================================================
#' Plot a `gg_beta_varpro` object
#'
#' Horizontal bar chart of the mean absolute coefficient
#' \eqn{\mathrm{mean}(|\hat{\beta}|)}{mean(|beta hat|)} per variable, sorted
#' descending so
#' the eye lands on the top variable first. Bars filled blue when above the
#' selection cutoff, gray otherwise. Dashed red line marks the cutoff.
#'
#' @section Reading the chart:
#' Each bar is the average magnitude of a per-rule lasso coefficient for
#' that variable. **The numeric scale carries the predictor's units.**
#' If "age" is in years and "creatinine" is in mg/dL, a longer bar for
#' age does not mean age is "more important" in any unit-free sense.
#' Comparisons across data sets or across variables with very different
#' units require keeping the units context in mind. Within one data set,
#' bars are comparable up to that unit caveat.
#'
#' Variables above the cutoff are colored blue and flagged `selected`;
#' variables below are gray. Lasso shrinkage can drive a rule's
#' \eqn{\hat{\beta}}{beta hat} to
#' exactly zero; those rules are kept in the average, so a variable
#' with many shrunk-to-zero rules will sit lower in the ranking than
#' one whose released coefficients are consistently non-zero.
#'
#' For a classification fit, variables are sorted by
#' `mean(|sum-of-class-beta|)` descending and that ordering is shared
#' across every facet, so rows line up between classes for visual
#' comparison. Each facet has its own cutoff line.
#'
#' @section What this tells you:
#' Use the bar chart as a selection ranking, not as an effect-size axis.
#' Pair it with [gg_varpro()] to see where split-strength importance and
#' local lasso-beta importance agree or disagree; disagreement is often the
#' interesting signal.
#'
#' @param x A `gg_beta_varpro` object from [gg_beta_varpro()].
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_beta_varpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#'   plot(gg_beta_varpro(v))
#' }
#' }
#'
#' @name plot.gg_beta_varpro
#' @importFrom ggplot2 ggplot aes geom_col geom_hline coord_flip
#' @importFrom ggplot2 scale_fill_manual labs theme_minimal
#' @export
plot.gg_beta_varpro <- function(x, ...) {
  if (nrow(x) == 0L) {
    stop("plot.gg_beta_varpro: nothing to plot (gg_beta_varpro has 0 rows).",
         call. = FALSE)
  }
  prov          <- attr(x, "provenance")
  n_rules_total <- if (!is.null(prov)) prov$n_rules_total %||% NA_integer_ else NA_integer_
  cv_txt        <- .gg_beta_cv_txt(prov)
  has_class     <- "class" %in% names(x)
  cutoff_vec    <- .gg_beta_cutoff_vec(prov, x, has_class)
  caption_txt   <- if (has_class) {
    .gg_beta_caption_class(x, n_rules_total, cv_txt)
  } else {
    sprintf("Mean |beta| over %s rules. Lasso: %s. Cutoff: %.4g.",
            if (is.na(n_rules_total)) "NA" else format(n_rules_total),
            cv_txt, cutoff_vec[[1]])
  }

  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x    = .data[["variable"]],
      y    = .data[["beta_mean"]],
      fill = factor(.data[["selected"]])
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
      guide  = "none"
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Mean |beta| (per-rule lasso)",
      caption = caption_txt
    ) +
    ggplot2::theme_minimal()

  if (has_class && length(unique(x$class)) > 1L) {
    # Per-class cutoff lines via data join
    hline_df <- data.frame(
      class  = factor(names(cutoff_vec), levels = levels(x$class)),
      cutoff = unname(cutoff_vec),
      stringsAsFactors = FALSE
    )
    p <- p +
      ggplot2::facet_wrap(~ class, nrow = 1L) +
      ggplot2::geom_hline(
        data        = hline_df,
        ggplot2::aes(yintercept = .data[["cutoff"]]),
        linetype    = "dashed",
        color       = "#e74c3c",
        linewidth   = 0.7,
        inherit.aes = FALSE
      )
  } else {
    # Single panel (regression or single-class) -- one horizontal line
    cutoff_scalar <- if (has_class) cutoff_vec[[as.character(x$class[1])]] else cutoff_vec[[1]]
    p <- p + ggplot2::geom_hline(
      yintercept = cutoff_scalar,
      linetype   = "dashed",
      color      = "#e74c3c",
      linewidth  = 0.7
    )
  }
  p
}

# ---- Internal helpers ------------------------------------------------------

#' @noRd
.gg_beta_cv_txt <- function(prov) {
  if (!is.null(prov) && isTRUE(prov$use.cv)) return("cv")
  if (!is.null(prov) && length(prov$use.cv) == 1L && is.na(prov$use.cv)) {
    return("unknown (precomputed)")
  }
  "fixed"
}

#' @noRd
.gg_beta_cutoff_vec <- function(prov, x, has_class) {
  if (!is.null(prov) && !is.null(prov$cutoff)) return(prov$cutoff)
  if (has_class) {
    return(stats::setNames(vapply(split(x$beta_mean, x$class), mean, numeric(1)),
                           levels(x$class)))
  }
  stats::setNames(mean(x$beta_mean), "regr")
}

#' @noRd
.gg_beta_caption_class <- function(x, n_rules_total, cv_txt) {
  n_panels <- length(unique(x$class))
  tail <- if (n_panels == 1L) {
    sprintf("Class: %s.", as.character(x$class[1]))
  } else {
    sprintf("%d classes (faceted).", n_panels)
  }
  sprintf("Mean |beta| over %s rules. Lasso: %s. %s",
          if (is.na(n_rules_total)) "NA" else format(n_rules_total),
          cv_txt, tail)
}
