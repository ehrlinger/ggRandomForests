##=============================================================================
#' Plot a `gg_ivarpro` object
#'
#' Branches on the presence of `which_obs` provenance and the `class`
#' column. Distribution view: jittered points showing per-observation
#' local importances per variable. Per-observation view: horizontal bar
#' chart of one observation's local importances across variables.
#' Classification: faceted by class unless `which_class` collapses to
#' a single class.
#'
#' @section Reading the chart:
#' Each point in the distribution view is one observation's local
#' importance for that variable. Variables are sorted by descending
#' `mean(|local_imp|)`. The cutoff line picks the variables whose local
#' importance is, on average, large enough to flag. For a classification
#' fit, every facet shares the same row order so you can read across.
#'
#' For a classification fit, variables are sorted by descending
#' `mean(|local_imp|)` across all (obs, class) rows and that ordering
#' is shared across every facet, so rows line up between classes for
#' visual comparison. Each facet has its own cutoff line.
#'
#' The per-observation view (`which_obs`) is a horizontal bar chart of
#' one observation's local importances; bars below the cutoff are gray,
#' above are blue. The visual resembles a SHAP waterfall, but the values
#' are release-rule contributions: scaled per-rule contrasts on observed
#' data, not Shapley values and not permutation-based.
#'
#' @param x A `gg_ivarpro` object from [gg_ivarpro()].
#' @param ... Not currently used.
#'
#' @return A `ggplot` object.
#'
#' @seealso [gg_ivarpro()].
#'
#' @examples
#' \donttest{
#' if (requireNamespace("varPro", quietly = TRUE) &&
#'     requireNamespace("MASS", quietly = TRUE)) {
#'   set.seed(1)
#'   v <- varPro::varpro(medv ~ ., data = MASS::Boston, ntree = 50)
#'   plot(gg_ivarpro(v))
#' }
#' }
#'
#' @name plot.gg_ivarpro
#' @importFrom ggplot2 ggplot aes geom_col geom_jitter geom_hline coord_flip
#' @importFrom ggplot2 facet_wrap scale_fill_manual scale_color_manual labs theme_minimal
#' @export
plot.gg_ivarpro <- function(x, ...) {
  if (nrow(x) == 0L) {
    stop("plot.gg_ivarpro: nothing to plot (gg_ivarpro has 0 rows).",
         call. = FALSE)
  }
  prov           <- attr(x, "provenance")
  has_class      <- "class" %in% names(x)
  which_obs_set  <- !is.null(prov) && !is.null(prov$which_obs)

  cutoff_vec <- if (!is.null(prov) && !is.null(prov$cutoff)) {
    prov$cutoff
  } else if (has_class) {
    stats::setNames(vapply(split(abs(x$local_imp), x$class), mean, numeric(1)),
                    levels(x$class))
  } else {
    stats::setNames(mean(abs(x$local_imp)), "regr")
  }

  base <- ggplot2::ggplot(
    x,
    ggplot2::aes(x = .data[["variable"]],
                 y = .data[["local_imp"]])
  )

  if (which_obs_set) {
    p <- base +
      ggplot2::geom_col(
        ggplot2::aes(fill = factor(.data[["selected"]]))
      ) +
      ggplot2::scale_fill_manual(
        values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
        guide  = "none"
      )
  } else {
    p <- base +
      ggplot2::geom_jitter(
        ggplot2::aes(color = factor(.data[["selected"]])),
        width = 0.2, height = 0, alpha = 0.5
      ) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
        guide  = "none"
      )
  }

  p <- p + ggplot2::coord_flip()

  if (has_class && length(unique(x$class)) > 1L) {
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
    cutoff_scalar <- if (has_class) cutoff_vec[[as.character(x$class[1])]] else cutoff_vec[[1]]
    p <- p + ggplot2::geom_hline(
      yintercept = cutoff_scalar,
      linetype   = "dashed",
      color      = "#e74c3c",
      linewidth  = 0.7
    )
  }

  caption_txt <- sprintf(
    "Local importance over %d obs x %d variables.%s",
    prov$n_obs %||% NA_integer_,
    prov$n_var %||% NA_integer_,
    if (which_obs_set) sprintf(" obs = %d.", prov$which_obs) else ""
  )

  p + ggplot2::labs(
    x = NULL,
    y = "Local importance",
    caption = caption_txt
  ) + ggplot2::theme_minimal()
}
