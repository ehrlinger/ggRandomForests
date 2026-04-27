####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####  ----------------------------------------------------------------
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
#' Plot a \code{\link{gg_vimp}} object, extracted variable importance of a
#' \code{\link[randomForestSRC]{rfsrc}} object
#'
#' @param x \code{\link{gg_vimp}} object created from a
#' \code{\link[randomForestSRC]{rfsrc}} object
#' @param relative should we plot vimp or relative vimp. Defaults to vimp.
#' @param lbls A vector of alternative variable labels. Item names should be
#' the same as the variable names.
#' @param ... optional arguments passed to gg_vimp if necessary
#'
#' @return \code{ggplot} object
#'
#' @seealso \code{\link{gg_vimp}}
#'
#' @references
#' Breiman L. (2001). Random forests, Machine Learning, 45:5-32.
#'
#' Ishwaran H. and Kogalur U.B. (2007). Random survival forests for
#' R, Rnews, 7(2):25-31.
#'
#' Ishwaran H. and Kogalur U.B. randomForestSRC: Random Forests for Survival,
#' Regression and Classification. R package version >= 3.4.0.
#' \url{https://cran.r-project.org/package=randomForestSRC}
#'
#' @examples
#' ## ------------------------------------------------------------
#' ## classification example
#' ## ------------------------------------------------------------
#' ## -------- iris data
#' rfsrc_iris <- rfsrc(Species ~ ., data = iris)
#' gg_dta <- gg_vimp(rfsrc_iris)
#' plot(gg_dta)
#'
#' ## ------------------------------------------------------------
#' ## regression example
#' ## ------------------------------------------------------------
#' ## -------- air quality data
#' rfsrc_airq <- rfsrc(Ozone ~ ., airquality)
#' gg_dta <- gg_vimp(rfsrc_airq)
#' plot(gg_dta)
#'
#'
#' @export
plot.gg_vimp <- function(x, relative, lbls, ...) {
  gg_dta <- x

  # Accept raw rfsrc / randomForest objects and compute VIMP on the fly
  if (!inherits(gg_dta, "gg_vimp")) {
    gg_dta <- gg_vimp(gg_dta, ...)
  }

  # Capture extra args so we can inspect nvar.
  arg_set <- list(...)

  # Optionally restrict to the top-nvar most important variables (gg_vimp
  # already sorts by descending VIMP, so we just trim the tail).
  nvar <- nrow(gg_dta)
  if (!is.null(arg_set$nvar)) {
    if (is.numeric(arg_set$nvar) && arg_set$nvar > 1) {
      if (arg_set$nvar < nrow(gg_dta)) {
        nvar <- arg_set$nvar
        gg_dta <- gg_dta[seq_len(nvar), ]
      }
    }
  }

  gg_plt <- ggplot2::ggplot(gg_dta)

  # Use "vimp" as the bar-height column when it exists; fall back to the
  # first column name for objects that store a renamed importance measure.
  msr <- "vimp"
  if (!msr %in% colnames(gg_dta)) {
    msr <- colnames(gg_dta)[1]
  }

  # When there are both positive and negative VIMP values, colour the bars
  # differently so the user can immediately see which variables are below zero
  # (i.e. hurt predictive accuracy on average). Both `fill` and `color` are
  # mapped to the same column; we give them the same legend title ("VIMP > 0")
  # so ggplot collapses them into a single legend rather than rendering one
  # legend titled "VIMP > 0" (fill) and a second titled "positive" (color).
  legend_title <- "VIMP > 0"
  if (length(unique(gg_dta$positive)) > 1) {
    gg_plt <- gg_plt +
      ggplot2::geom_bar(
        ggplot2::aes(
          y = .data[[msr]],
          x = .data$vars,
          fill = .data$positive,
          color = .data$positive
        ),
        stat = "identity",
        width = .5,
      )
  } else {
    # All values have the same sign — colour only the bar border to avoid a
    # redundant fill legend.
    gg_plt <- gg_plt +
      ggplot2::geom_bar(
        ggplot2::aes(y = .data[[msr]], x = .data$vars, color = .data$positive),
        stat = "identity",
        width = .5,
      )
  }
  # Set both legends' titles to the same string so ggplot merges them.
  # Users can override with their own labs() call after the fact.
  gg_plt <- gg_plt +
    ggplot2::labs(x = "", y = msr,
                  fill = legend_title, color = legend_title)

  if (!missing(lbls)) {
    # Map internal variable names to human-readable labels.  lbls should be a
    # named character vector; any unmatched variables keep their original name.
    if (length(lbls) >= length(gg_dta$vars)) {
      st_lbls <- lbls[as.character(gg_dta$vars)]
      names(st_lbls) <- as.character(gg_dta$vars)
      # Fall back to the raw variable name when no label was supplied
      st_lbls[which(is.na(st_lbls))] <-
        names(st_lbls[which(is.na(st_lbls))])

      gg_plt <- gg_plt +
        ggplot2::scale_x_discrete(labels = st_lbls)
    }
  }

  # Flip coordinates so variable names appear on the y-axis (horizontal bars
  # are easier to read when there are many variables).  If gg_vimp contains
  # a "set" column (comparison VIMP across two forests), facet by set.
  if (is.null(gg_dta$set) || length(unique(gg_dta$set)) < 2) {
    gg_plt <- gg_plt +
      ggplot2::coord_flip()
  } else {
    gg_plt <- gg_plt +
      ggplot2::coord_flip() +
      ggplot2::facet_grid(~set)
  }

  return(gg_plt)
}
