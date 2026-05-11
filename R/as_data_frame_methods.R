####**********************************************************************
####  as.data.frame() S3 methods for the list-based gg_partial family.
####
####  gg_partial, gg_partial_rfsrc, and gg_partialpro all return a list
####  with $continuous and $categorical data frames.  as.data.frame() on
####  such an object would normally return a one-column data.frame of the
####  list elements — not useful.  These methods instead collapse the two
####  sub-frames into a single tidy data.frame with a "type" column
####  ("continuous" / "categorical") added, using dplyr::bind_rows() so
####  columns present in only one sub-frame are filled with NA rather than
####  causing an error.
####
####  The provenance attribute is forwarded to the result so downstream
####  code can still inspect the source forest.
####**********************************************************************

#' Flatten a \code{gg_partial} list into a tidy data frame
#'
#' Collapses the \code{$continuous} and \code{$categorical} sub-frames into a
#' single \code{data.frame} with a \code{type} column
#' (\code{"continuous"} / \code{"categorical"}).  Useful for piping into
#' \code{dplyr} verbs or combining multiple models with
#' \code{dplyr::bind_rows()}.
#'
#' @param x A \code{gg_partial} object.
#' @param ... Ignored.
#'
#' @return A plain \code{data.frame}.  The \code{provenance} attribute is
#'   forwarded.
#'
#' @seealso \code{\link{gg_partial}}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' airq <- na.omit(airquality)
#' rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
#' pv <- randomForestSRC::plot.variable(rf, partial = TRUE,
#'                                       show.plots = FALSE)
#' gp <- gg_partial(pv)
#' df <- as.data.frame(gp)
#' table(df$type)
#' }
#'
#' @export
as.data.frame.gg_partial <- function(x, ...) {
  .partial_flatten(x)
}

#' Flatten a \code{gg_partial_rfsrc} list into a tidy data frame
#'
#' @inherit as.data.frame.gg_partial description return
#'
#' @param x A \code{gg_partial_rfsrc} object.
#' @param ... Ignored.
#'
#' @seealso \code{\link{gg_partial_rfsrc}}
#'
#' @examples
#' \donttest{
#' set.seed(42)
#' airq <- na.omit(airquality)
#' rf <- randomForestSRC::rfsrc(Ozone ~ ., data = airq, ntree = 50)
#' gp <- gg_partial_rfsrc(rf, xvar.names = c("Temp", "Wind"))
#' df <- as.data.frame(gp)
#' table(df$type)
#' }
#'
#' @export
as.data.frame.gg_partial_rfsrc <- function(x, ...) {
  .partial_flatten(x)
}

#' Flatten a \code{gg_partialpro} list into a tidy data frame
#'
#' @inherit as.data.frame.gg_partial description return
#'
#' @param x A \code{gg_partialpro} object.
#' @param ... Ignored.
#'
#' @seealso \code{\link{gg_partialpro}}
#'
#' @examples
#' \donttest{
#' ## gg_partialpro requires the varPro package — see ?gg_partialpro
#' }
#'
#' @export
as.data.frame.gg_partialpro <- function(x, ...) {
  .partial_flatten(x)
}

## Internal helper shared by all three as.data.frame methods.
.partial_flatten <- function(x) {
  cont <- x$continuous
  cat  <- x$categorical

  if (is.data.frame(cont) && nrow(cont) > 0L) {
    # Coerce x to character so bind_rows can combine with categorical (factor)
    # x from the other frame without a vctrs type error.
    if ("x" %in% names(cont)) cont$x <- as.character(cont$x)
    cont$type <- "continuous"
  } else {
    cont <- NULL
  }
  if (is.data.frame(cat) && nrow(cat) > 0L) {
    if ("x" %in% names(cat)) cat$x <- as.character(cat$x)
    cat$type <- "categorical"
  } else {
    cat <- NULL
  }

  out <- dplyr::bind_rows(cont, cat)

  # Forward provenance so downstream code can still inspect the source.
  prov <- attr(x, "provenance")
  if (!is.null(prov)) attr(out, "provenance") <- prov

  out
}
