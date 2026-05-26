##=============================================================================
#' Plot a \code{gg_varpro} variable importance object
#'
#' Draws a horizontal boxplot of the per-tree importance z-scores, or of the
#' raw importances if you asked for those.  Set \code{faithful = TRUE} at
#' extract time and the per-tree points are scattered over the box; for a
#' classification forest, \code{conditional = TRUE} splits the plot into one
#' facet per class.
#'
#' @section Reading the boxplot:
#' Variables are sorted top to bottom by descending median per-tree
#' importance, so the eye lands on the most important variable first.
#' For each variable the box spans the 15th to 85th percentile of the
#' per-tree scores, the centre line is the median, and the whiskers run
#' out to the 5th and 95th percentile — not the usual Tukey 1.5 IQR
#' whiskers. The dashed vertical line is the selection \code{cutoff}
#' (default \code{0.79}). On the default z-score axis
#' (\code{local.std = TRUE}) that line is a z; on the raw-importance
#' axis (\code{local.std = FALSE}, \code{type = "raw"}) it is the same
#' numeric value but in raw-importance units. Boxes whose aggregate
#' value sits above the line are coloured blue and flagged
#' \code{selected = TRUE}, the rest are grey. A
#' selected variable with a tight, high box is a variable the forest
#' agrees on across trees. A selected variable with a wide box that
#' straddles the cutoff is one to look at twice before relying on it.
#'
#' With \code{faithful = TRUE} the box is drawn faint and the per-tree
#' values are jittered over it as semi-transparent points, on the same
#' scale as the box (z when \code{local.std = TRUE}, raw otherwise). A
#' white-outlined dot marks the mean. Use this view when you want to
#' see how individual trees voted rather than just the summary.
#'
#' For a classification forest with \code{conditional = TRUE} the plot
#' splits into one facet per class. Variables keep the unconditional
#' sort order, so the rows line up across facets and you can read
#' across to see which class a variable is informative for.
#'
#' @section What this tells you:
#' Take the variables above the cutoff as your candidate set. Use the
#' width of the box and the per-tree overlay to gauge confidence — a
#' narrow box well above the cutoff is a confident pick, a wide box
#' that crosses it is a coin flip you should not lean on. For
#' classification, conditional importance tells you which variables
#' drive which class; a variable that is unconditionally important but
#' only important for one class out of several is still useful, just
#' useful for a narrower question.
#'
#' @param x A \code{gg_varpro} object from \code{\link{gg_varpro}}.
#' @param type Character; the display scale.  Leave it off and it is read
#'   from \code{provenance$local.std}: \code{"z"} when \code{local.std =
#'   TRUE} (the default), \code{"raw"} when \code{local.std = FALSE}.
#'   Asking for a scale that the extract step did not prepare raises an
#'   error.
#' @param ... Not currently used.
#'
#' @details
#' **Boxplot geometry:** the hinges are the 15th and 85th percentiles of the
#' per-tree z-distribution, and the whiskers run to the 5th and 95th.  This
#' is \strong{not} a Tukey boxplot, and the plot carries a caption that says
#' so.
#'
#' **\code{faithful = TRUE}:** the per-tree values are jittered over the box
#' as semi-transparent points, on the same scale as the box itself (z when
#' \code{local.std = TRUE}, raw when \code{local.std = FALSE}).  The box is
#' drawn faint to let the points show through, and a white-outlined dot
#' marks the mean.
#'
#' **\code{conditional = TRUE}:** the class-conditional importances
#' (\code{$conditional}) are shown as a faceted bar chart
#' (\code{facet_wrap(~class, nrow = 1)}).  Variables keep the sort order set
#' by the unconditional median z in \code{$stats}, so the facets line up.
#'
#' @return A \code{ggplot} object.
#'
#' @seealso \code{\link{gg_varpro}}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' vp <- varPro::varpro(mpg ~ ., data = mtcars, ntree = 50)
#' plot(gg_varpro(vp))
#' plot(gg_varpro(vp, faithful = TRUE))
#' }
#'
#' @name plot.gg_varpro
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_point geom_col
#' @importFrom ggplot2 geom_vline geom_hline coord_flip facet_wrap
#' @importFrom ggplot2 scale_fill_manual labs theme_minimal
#' @importFrom tidyr pivot_longer
#' @export
plot.gg_varpro <- function(x, type, ...) {
  prov <- attr(x, "provenance")

  ## ---- Auto-detect type from provenance when not supplied ------------------
  if (missing(type)) {
    type <- if (isTRUE(prov$local.std)) "z" else "raw"
  } else {
    type <- match.arg(type, c("z", "raw"))
  }

  ## ---- Validate type vs. extract-time local.std ----------------------------
  if (identical(type, "raw") && isTRUE(prov$local.std)) {
    stop("type = 'raw' requires local.std = FALSE at gg_varpro() extract time.",
         call. = FALSE)
  }
  if (identical(type, "z") && !isTRUE(prov$local.std)) {
    stop("type = 'z' requires local.std = TRUE at gg_varpro() extract time.",
         call. = FALSE)
  }

  ## ---- Conditional view ---------------------------------------------------
  if (!is.null(x$conditional)) {
    if (!missing(type) && !identical(type, "z")) {
      message("Conditional plot ignores type = '", type,
              "'; showing class-conditional z scores.")
    }
    return(.plot_varpro_conditional(x, prov))
  }

  ## ---- Default / faithful view --------------------------------------------
  .plot_varpro_main(x, type, prov)
}

## ---- Internal renderers ----------------------------------------------------

#' @keywords internal
.plot_varpro_main <- function(x, type, prov) {
  stats_df <- x$stats
  # Merge selected flag
  sel_df <- unique(x$imp[, c("variable", "selected")])
  stats_df <- merge(stats_df, sel_df, by = "variable", all.x = TRUE)

  faithful  <- isTRUE(prov$faithful)
  cutoff    <- prov$cutoff %||% 0.79
  fill_vals <- c("TRUE" = "#4e8fcd", "FALSE" = "#888888")
  cap_text  <- paste0(
    "Hinges: 15th/85th percentiles; whiskers: 5th/95th. ",
    if (faithful) "Points show per-tree importance. " else "",
    "Not a Tukey boxplot."
  )

  p <- ggplot2::ggplot(stats_df,
         ggplot2::aes(x      = .data[["variable"]],
                      ymin   = .data[["q05"]],
                      lower  = .data[["q15"]],
                      middle = .data[["median"]],
                      upper  = .data[["q85"]],
                      ymax   = .data[["q95"]],
                      fill   = factor(.data[["selected"]]))) +
    ggplot2::geom_boxplot(stat = "identity",
                          alpha = if (faithful) 0.4 else 0.85) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = fill_vals, guide = "none") +
    ggplot2::geom_hline(yintercept = cutoff,
                        linetype = "dashed", color = "#e74c3c", linewidth = 0.7) +
    ggplot2::labs(x = NULL,
                  y = .varpro_imp_ylabel(type, prov),
                  caption = cap_text) +
    ggplot2::theme_minimal()

  ## ---- faithful overlay ---------------------------------------------------
  if (faithful && !is.null(x$imp.tree)) {
    long_df <- as.data.frame(x$imp.tree)
    long_df$tree <- seq_len(nrow(x$imp.tree))
    long_df <- tidyr::pivot_longer(long_df, !"tree",
                                   names_to = "variable", values_to = "imp_raw")
    long_df$variable <- factor(long_df$variable, levels = levels(x$imp$variable))

    ## Overlay y-values match the box scale (type-aware) --------------------
    if (identical(type, "z")) {
      ## z_ij = imp_ij / sd_j  (same formula as .varpro_imp_stats)
      sd_j <- tapply(long_df$imp_raw, long_df$variable,
                     stats::sd, na.rm = TRUE)
      sd_j[sd_j < .Machine$double.eps] <- 1
      long_df$overlay  <- long_df$imp_raw / sd_j[long_df$variable]
      mean_df          <- stats_df
      mean_df$overlay_mean <- mean_df$mean / sd_j[as.character(mean_df$variable)]
    } else {
      ## raw scale: imp_raw as-is; mean column is already raw
      long_df$overlay      <- long_df$imp_raw
      mean_df              <- stats_df
      mean_df$overlay_mean <- mean_df$mean
    }

    p <- p +
      ggplot2::geom_jitter(data = long_df,
                           ggplot2::aes(x = .data[["variable"]],
                                        y = .data[["overlay"]]),
                           inherit.aes = FALSE,
                           width = 0.15, height = 0,  # width jitters in categorical band
                           alpha = 0.4, size = 1.2,
                           color = "#4e8fcd") +
      ggplot2::geom_point(data = mean_df,
                          ggplot2::aes(x = .data[["variable"]],
                                       y = .data[["overlay_mean"]]),
                          inherit.aes = FALSE,
                          shape = 21, fill = "white", color = "#4e8fcd",
                          size = 2.5)
  }

  p
}

#' @keywords internal
.plot_varpro_conditional <- function(x, prov) {
  ## Class-conditional z-scores as faceted bar chart.
  cond_df   <- x$conditional
  ## Replace NA/NaN z with 0 to suppress geom_col remove_missing warnings
  cond_df$z[!is.finite(cond_df$z)] <- 0
  cond_df$variable <- factor(cond_df$variable, levels = levels(x$imp$variable))
  cutoff    <- prov$cutoff %||% 0.79

  ggplot2::ggplot(cond_df,
                  ggplot2::aes(x    = .data[["variable"]],
                               y    = .data[["z"]],
                               fill = .data[["z"]] > cutoff)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ class, nrow = 1L) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = "#4e8fcd", "FALSE" = "#888888"),
      guide  = "none"
    ) +
    ggplot2::geom_hline(yintercept = cutoff,
                        linetype = "dashed", color = "#e74c3c",
                        linewidth = 0.7) +
    ggplot2::labs(x = NULL,
                  y = "Variable importance (z)",
                  caption = paste0("Dashed line at z = ", cutoff,
                                   ". Conditional class importance.")) +
    ggplot2::theme_minimal()
}

#' @keywords internal
.varpro_imp_ylabel <- function(type, prov) {
  if (identical(type, "raw")) {
    "Variable importance"
  } else {
    "Variable importance (z)"
  }
}
