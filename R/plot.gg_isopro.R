####**********************************************************************
####  plot.gg_isopro: ranked-elbow + density plot for isopro anomaly scores.
####**********************************************************************

#' Plot a varPro isolation-forest anomaly score
#'
#' Renders a `gg_isopro` object as a ranked elbow (observations sorted by
#' anomaly score), a density of scores, or both side-by-side. Optionally
#' annotates a threshold either in score-space (`threshold`) or in
#' quantile-space (`top_n_pct`).
#'
#' @section Reading the elbow:
#' The elbow plot is the canonical anomaly-detection picture. The x-axis
#' is observation rank — observations sorted from most ordinary to most
#' anomalous — and the y-axis is the `howbad` score. For a clean
#' population the curve sits flat near zero across the bulk of the data
#' and then bends sharply upward in the right tail; that bend is where
#' the anomalous observations live. The point of the plot is not to read
#' off a single score, it is to *see where the curve breaks*. Pick a
#' cutoff there. Pass it back in as `threshold` (for a score) or
#' `top_n_pct` (for "the top 5\%") and the plot draws a dashed reference
#' line so you can record the choice you made.
#'
#' @section Reading the density:
#' The density panel is the same scores viewed as a distribution. A
#' single tight mode near zero with a long thin right tail is the
#' picture you hope for — bulk of the data ordinary, a few clear
#' anomalies. A bimodal density says you may have two populations rather
#' than one clean cluster plus outliers, and the cutoff question becomes
#' harder. Either way, this panel is a sanity check on what the elbow
#' suggests.
#'
#' @section Comparing methods:
#' When the input object carries a `method` column (because you bound
#' several `gg_isopro()` calls together), both panels colour by method
#' automatically. The point of comparing `"rnd"`, `"unsupv"`, and
#' `"auto"` is not to pick a winner from the figure alone — it is to see
#' whether the methods agree on which observations are anomalous. Curves
#' that overlap in the right tail and elbow at roughly the same rank are
#' telling you the same story three ways. Curves that diverge are
#' telling you the score is method-sensitive, which is itself useful
#' information.
#'
#' @param x A `gg_isopro` object from [gg_isopro()].
#' @param panel One of `"both"` (default — a `patchwork` of elbow + density),
#'   `"elbow"`, or `"density"` (each returns a single `ggplot`).
#' @param threshold Numeric in `[0, 1]`, or `NULL` (default). If set, draws
#'   a reference line at that `howbad` value on the elbow and density.
#' @param top_n_pct Numeric in `(0, 100)`, or `NULL` (default). If set,
#'   resolves to the matching `howbad` quantile and draws the same
#'   reference line. If both `threshold` and `top_n_pct` are supplied,
#'   `threshold` wins with a `message()`.
#' @param ... Currently unused.
#'
#' @return A `ggplot` (single panel) or a `patchwork` (panel = "both").
#'
#' @seealso [gg_isopro()], [varPro::isopro()]
#' @export
plot.gg_isopro <- function(x,
                           panel     = c("both", "elbow", "density"),
                           threshold = NULL,
                           top_n_pct = NULL,
                           ...) {
  panel <- match.arg(panel)

  # Resolve threshold (score wins if both supplied).
  thr <- .resolve_isopro_threshold(x$howbad, threshold, top_n_pct)

  # Multi-method detection: a `method` column means the user bound several
  # fits together with bind_rows; colour/group by method.
  has_method <- "method" %in% names(x)

  elbow   <- .gg_isopro_elbow(x, thr, has_method)
  density <- .gg_isopro_density(x, thr, has_method)

  if (panel == "elbow")    return(elbow)
  if (panel == "density")  return(density)

  # panel == "both"
  elbow + density
}

# Returns the resolved threshold (numeric scalar) or NA_real_ if neither
# arg was supplied. Emits a message when both args are set.
.resolve_isopro_threshold <- function(howbad, threshold, top_n_pct) {
  if (!is.null(threshold) && !is.null(top_n_pct)) {
    message("Both `threshold` and `top_n_pct` supplied; using `threshold`.")
    return(as.numeric(threshold))
  }
  if (!is.null(threshold)) return(as.numeric(threshold))
  if (!is.null(top_n_pct)) {
    q <- 1 - (as.numeric(top_n_pct) / 100)
    return(as.numeric(stats::quantile(howbad, probs = q, na.rm = TRUE)))
  }
  NA_real_
}

# Rank-ordered elbow panel.
.gg_isopro_elbow <- function(x, thr, has_method) {
  if (has_method) {
    x$rank <- stats::ave(x$howbad, x$method, FUN = function(v) rank(v, ties.method = "first"))
  } else {
    x$rank <- rank(x$howbad, ties.method = "first")
  }
  x <- x[order(x$rank), , drop = FALSE]

  aes_line <- if (has_method) {
    ggplot2::aes(x = .data$rank, y = .data$howbad, colour = .data$method)
  } else {
    ggplot2::aes(x = .data$rank, y = .data$howbad)
  }

  p <- ggplot2::ggplot(x) +
    ggplot2::geom_line(aes_line) +
    ggplot2::labs(x = "Observation rank (by howbad)",
                  y = "howbad (anomaly score)")

  if (!is.na(thr)) {
    p <- p +
      ggplot2::geom_hline(yintercept = thr, linetype = "dashed",
                          colour = "red", linewidth = 0.4)
  }
  p
}

# Score-density panel.
.gg_isopro_density <- function(x, thr, has_method) {
  aes_dens <- if (has_method) {
    ggplot2::aes(x = .data$howbad, fill = .data$method)
  } else {
    ggplot2::aes(x = .data$howbad)
  }

  p <- ggplot2::ggplot(x) +
    ggplot2::geom_density(aes_dens, alpha = if (has_method) 0.4 else 1) +
    ggplot2::labs(x = "howbad (anomaly score)", y = "Density")

  if (!is.na(thr)) {
    p <- p +
      ggplot2::geom_vline(xintercept = thr, linetype = "dashed",
                          colour = "red", linewidth = 0.4)
  }
  p
}
