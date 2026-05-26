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
