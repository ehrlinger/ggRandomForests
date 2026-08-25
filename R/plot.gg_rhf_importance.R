##=============================================================================
#' Plot Random Hazard Forest variable priority over time
#'
#' Draws a point matrix from a [gg_rhf_importance()] object. Each row is a
#' variable, each column is a time window, and point size and color carry the
#' time-localized RHF variable-priority score.
#'
#' @param x A `gg_rhf_importance` object from [gg_rhf_importance()].
#' @param vars Optional nonempty character vector of variables to display.
#'   Unknown names are an error. When supplied, this takes precedence over
#'   `top_n_union`.
#' @param top_n_union `NULL` or one positive integer. When `vars` is `NULL`,
#'   each time window contributes this many leading variables and the plot
#'   displays their union. `NULL` displays every variable.
#' @param transform Display transformation: `"none"` (default) or `"log10"`,
#'   which uses `log10(priority + 1)`. The returned extractor object is never
#'   changed.
#' @param size_cap,color_cap One numeric value in `(0, 1]`. Point size and
#'   color are capped at these quantiles of the finite display values. A value
#'   of `1` applies no cap.
#' @param display_note Logical; if `TRUE`, an applied size or color cap is
#'   reported in the caption.
#' @param ... Additional arguments passed to [ggplot2::geom_point()].
#'
#' @details
#' Variables retain the global `q90` ordering prepared by
#' [gg_rhf_importance()], with the highest-ranked variable at the top. Time
#' windows remain chronological. This follows the variable-priority matrix in
#' Ishwaran et al. (2026) while returning a ggplot object you can extend.
#'
#' The transformation and caps affect display values only. The `priority`
#' column in `x` remains on the upstream scale. A zero priority is drawn at the
#' minimum point size; missing priorities are not drawn. If variable filtering
#' leaves no finite values, the method stops rather than returning an empty
#' plot.
#'
#' @return A `ggplot` object.
#'
#' @references
#' Ishwaran H, Hsich EM, Kogalur UB, Lee DKK (2026). Random Hazard Forests.
#' arXiv:2608.21597. \doi{10.48550/arXiv.2608.21597}.
#'
#' @seealso [gg_rhf_importance()], [randomForestRHF::dotmatrix.importance.rhf()]
#'
#' @examples
#' \donttest{
#' if (requireNamespace("randomForestRHF", quietly = TRUE)) {
#'   data(pbc, package = "randomForestSRC")
#'   d <- randomForestRHF::convert.counting(
#'     survival::Surv(days, status) ~ ., na.omit(pbc))
#'   o <- randomForestRHF::rhf(
#'     "Surv(id, start, stop, event) ~ .", d, ntree = 30)
#'   priority_fit <- randomForestRHF::importance.rhf(o)
#'   priority <- gg_rhf_importance(o, importance_fit = priority_fit)
#'
#'   plot(priority, top_n_union = 10)
#' }
#' }
#'
#' @name plot.gg_rhf_importance
#' @export
plot.gg_rhf_importance <- function(x, vars = NULL, top_n_union = 15L,
                                   transform = c("none", "log10"),
                                   size_cap = 0.99, color_cap = 0.99,
                                   display_note = TRUE, ...) {
  if (!inherits(x, "gg_rhf_importance")) {
    stop("plot.gg_rhf_importance() requires a 'gg_rhf_importance' object.",
         call. = FALSE)
  }
  transform <- match.arg(transform)
  d <- .rhf_priority_plot_data(x, vars, top_n_union)
  d <- d[is.finite(d$priority), , drop = FALSE]
  if (!nrow(d)) {
    stop("No finite RHF priority values to plot.", call. = FALSE)
  }

  d$display_priority <- if (transform == "log10") {
    log10(d$priority + 1)
  } else {
    d$priority
  }
  size <- .rhf_priority_cap(d$display_priority, size_cap, "size_cap")
  color <- .rhf_priority_cap(d$display_priority, color_cap, "color_cap")
  d$size_display <- size$value
  d$color_display <- color$value

  ordered_windows <- unique(d[order(d$time_index),
                              c("time_index", "time_window")])
  d$time_window <- factor(d$time_window,
                          levels = ordered_windows$time_window)
  note <- .rhf_priority_display_note(
    size$applied, color$applied, size_cap, color_cap, display_note
  )
  point_args <- list(...)
  if (is.null(point_args$alpha)) {
    point_args$alpha <- 0.9
  }

  ggplot2::ggplot(d, ggplot2::aes(
    x = .data[["time_window"]],
    y = .data[["variable"]],
    size = .data[["size_display"]],
    color = .data[["color_display"]]
  )) +
    do.call(ggplot2::geom_point, point_args) +
    ggplot2::scale_size_continuous(range = c(1.5, 7)) +
    ggplot2::scale_color_gradient(low = "grey85", high = "steelblue4") +
    ggplot2::labs(
      x = "Time window",
      y = NULL,
      size = "RHF variable priority",
      color = "RHF variable priority",
      caption = note
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}

.rhf_priority_plot_data <- function(x, vars, top_n_union) {
  available <- levels(x$variable)
  if (!is.null(vars)) {
    keep <- .validate_rhf_priority_vars(vars, available)
  } else if (is.null(top_n_union)) {
    keep <- available
  } else {
    top_n_union <- .validate_rhf_priority_top_n(top_n_union)
    finite <- x[is.finite(x$priority), , drop = FALSE]
    by_window <- split(finite, finite$time_index)
    keep <- unique(unlist(lapply(by_window, function(d) {
      d <- d[order(-d$priority, as.character(d$variable)), , drop = FALSE]
      utils::head(as.character(d$variable), top_n_union)
    }), use.names = FALSE))
  }
  x[as.character(x$variable) %in% keep, , drop = FALSE]
}

.validate_rhf_priority_vars <- function(vars, available) {
  if (!is.character(vars) || !length(vars) || anyNA(vars)) {
    stop("'vars' must be a nonempty character vector.", call. = FALSE)
  }
  unknown <- setdiff(vars, available)
  if (length(unknown)) {
    stop("Unknown RHF priority variables: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  unique(vars)
}

.validate_rhf_priority_top_n <- function(top_n_union) {
  if (!is.numeric(top_n_union) || length(top_n_union) != 1L ||
      !is.finite(top_n_union) || top_n_union < 1L ||
      top_n_union != as.integer(top_n_union)) {
    stop("'top_n_union' must be NULL or one positive integer.",
         call. = FALSE)
  }
  as.integer(top_n_union)
}

.rhf_priority_cap <- function(x, prob, arg) {
  if (!is.numeric(prob) || length(prob) != 1L || !is.finite(prob) ||
      prob <= 0 || prob > 1) {
    stop("'", arg, "' must be one numeric value in (0, 1].", call. = FALSE)
  }
  cap <- unname(stats::quantile(x[is.finite(x)], prob, names = FALSE))
  list(value = pmin(x, cap), applied = any(x > cap, na.rm = TRUE))
}

.rhf_priority_display_note <- function(size_applied, color_applied,
                                       size_cap, color_cap, display_note) {
  if (!isTRUE(display_note)) {
    return(NULL)
  }
  bits <- c(
    if (size_applied) sprintf("size capped at q%.0f", 100 * size_cap),
    if (color_applied) sprintf("color capped at q%.0f", 100 * color_cap)
  )
  if (length(bits)) {
    paste("Display only:", paste(bits, collapse = "; "))
  } else {
    NULL
  }
}
