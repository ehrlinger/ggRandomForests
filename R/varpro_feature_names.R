
##=============================================================================
#' Recover original variable names from varpro one-hot encoded feature names
#'
#' \code{varpro} one-hot encodes factor variables, appending a numeric suffix
#' for each level (e.g., \code{sex} becomes \code{sex0} and \code{sex1}).
#' This function strips those suffixes iteratively until every name in
#' \code{varpro_names} can be matched back to a column in \code{dataset}.
#'
#' @param varpro_names character vector of names as output by varpro (may
#'   include one-hot encoded suffixed names such as \code{"sex0"}, \code{"sex1"})
#' @param dataset the original data frame passed to varpro, used to look up
#'   valid column names
#'
#' @return character vector of unique original variable names (no suffixes)
#'
#' @seealso \code{\link{gg_partialpro}}
#'
#' @importFrom stringr str_sub
#'
#' @examples
#' ## ------------------------------------------------------------------
#' ## Simple case: one continuous variable + one binary factor
#' ## ------------------------------------------------------------------
#' ds <- data.frame(age = c(25, 30, 45), sex = c("M", "F", "M"))
#'
#' # varpro one-hot encodes 'sex' into 'sex0' and 'sex1'
#' varpro_names <- c("age", "sex0", "sex1")
#' varpro_feature_names(varpro_names, ds)
#' # Returns: c("age", "sex")
#'
#' ## ------------------------------------------------------------------
#' ## Multi-level factor: three-level 'group' variable
#' ## ------------------------------------------------------------------
#' ds2 <- data.frame(score = 1:6,
#'                   group = factor(rep(c("A", "B", "C"), 2)))
#'
#' # varpro appends 0/1/2 for each level
#' vn2 <- c("score", "group0", "group1", "group2")
#' varpro_feature_names(vn2, ds2)
#' # Returns: c("score", "group")
#'
#' ## ------------------------------------------------------------------
#' ## Already-clean names pass through unchanged
#' ## ------------------------------------------------------------------
#' ds3 <- data.frame(x = 1:5, y = 1:5)
#' varpro_feature_names(c("x", "y"), ds3)
#' # Returns: c("x", "y")
#'
#' @export
varpro_feature_names <- function(varpro_names, dataset) {
  # Names that already match a column in dataset — keep as-is
  inc_set <- varpro_names[which(varpro_names %in% colnames(dataset))]

  # Names that do not yet match any column — need suffix stripping
  one_set <- varpro_names[which(!varpro_names %in% colnames(dataset))]

  ## Iteratively strip the last character of each unmatched name until every
  ## name either resolves to a column or the set is exhausted.
  while (length(one_set) > 0) {
    # Drop the trailing character from each remaining unmatched name
    orig <- unlist(lapply(one_set, stringr::str_sub, 1, -2))

    # Accept names that now match a dataset column
    inc_set <- union(inc_set, orig[which(orig %in% colnames(dataset))])

    # Continue stripping names that still don't match
    one_set <- orig[which(!orig %in% colnames(dataset))]
  }
  return(inc_set)
}
