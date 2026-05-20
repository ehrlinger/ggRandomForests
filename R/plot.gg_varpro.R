##=============================================================================
#' Plot a \code{gg_varpro} variable importance object
#'
#' Renders a horizontal boxplot of per-tree importance z-scores (or raw
#' importances) with optional per-tree jitter overlay (\code{faithful=TRUE})
#' or class-conditional facets (\code{conditional=TRUE}).
#'
#' @param x A \code{gg_varpro} object from \code{\link{gg_varpro}}.
#' @param type Character; controls the display scale.  When omitted,
#'   auto-detected from \code{provenance$local.std}: \code{"z"} if
#'   \code{local.std = TRUE} (the default), \code{"raw"} if
#'   \code{local.std = FALSE}.  Explicitly supplying a value that conflicts
#'   with the extract-time setting raises an error.
#' @param ... Not currently used.
#'
#' @details
#' **Honest boxplot geometry:** Hinges are the 15th and 85th percentiles of
#' the per-tree z-distribution; whiskers extend to the 5th and 95th
#' percentiles.  This is \strong{not} a Tukey boxplot.  A mandatory plot
#' caption states this explicitly.
#'
#' **\code{faithful = TRUE}:** Per-tree values are overlaid as jittered
#' semi-transparent points on the same scale as the boxplot (z-scale when
#' \code{local.std = TRUE}, raw-scale when \code{local.std = FALSE}); the
#' box is drawn at reduced opacity; a white-outlined filled dot marks the
#' mean.
#'
#' **\code{conditional = TRUE}:** The conditional class-importance scores
#' (\code{$conditional}) are shown as a faceted bar chart
#' (\code{facet_wrap(~class, nrow = 1)}); variable sort order follows the
#' unconditional median z from \code{$stats}.
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
