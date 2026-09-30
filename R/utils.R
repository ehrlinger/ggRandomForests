####**********************************************************************
####**********************************************************************
####
####  ----------------------------------------------------------------
####  Written by:
####    John Ehrlinger, Ph.D.
####
####    email:  john.ehrlinger@gmail.com
####    URL:    https://github.com/ehrlinger/ggRandomForests
####  ----------------------------------------------------------------
####
####**********************************************************************
####**********************************************************************
## Internal utility functions shared across the package.
## None of these are exported to end-users.

# --------------------------------------------------------------------------- #
# Internal: lead / lag shift for numeric vectors.
#
# `x`        numeric vector of values.
# `shift_by` integer length 1 giving the number of positions to lead
#            (positive) or lag (negative) by; can also be a vector to
#            return a matrix of shifts.
#
# Removes the dplyr::lead dependency.  Adapted from
# http://ctszkin.com/2012/03/11/generating-a-laglead-variables/
#
# @noRd
shift <- function(x, shift_by = 1) {
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))

  if (length(shift_by) > 1) {
    return(sapply(shift_by, shift, x = x))
  }

  abs_shift_by <- abs(shift_by)
  if (shift_by > 0) {
    out <- c(tail(x, -abs_shift_by), rep(NA, abs_shift_by))
  } else if (shift_by < 0) {
    out <- c(rep(NA, abs_shift_by), head(x, -abs_shift_by))
  } else {
    out <- x
  }
  out
}

# --------------------------------------------------------------------------- #
# Internal helper: label a survfit tbl with stratum group names.
#
# survfit() concatenates strata end-to-end, in the sorted order of the `by`
# values (level order, for a factor), and records how many rows each one owns
# in $strata. The boundaries are read from those counts. Inferring a boundary
# from a drop in the time column misses a stratum whose times all follow the
# previous one's, and taking the labels from the row order of the data swaps
# them whenever that order is not the sorted one.
#
# @param tbl     data.frame produced from survfit output, one row per time
# @param srv_tab the stratified survfit object tbl was built from
# @param by_col  the grouping column, restricted to the rows survfit() used
#
# @return tbl with an additional $groups column containing the group label
#   for each row, in the type of by_col (levels, for a factor).
.label_strata <- function(tbl, srv_tab, by_col) {
  by_col <- by_col[!is.na(by_col)]
  lbls <- if (is.factor(by_col)) {
    levels(droplevels(by_col))
  } else {
    sort(unique(by_col))
  }

  # A single stratum: survfit() returns no $strata.
  counts <- srv_tab$strata
  if (is.null(counts)) counts <- nrow(tbl)

  if (length(lbls) != length(counts)) {
    stop("the 'by' column has ", length(lbls), " groups but the fit has ",
         length(counts), " strata.", call. = FALSE)
  }

  tbl$groups <- rep(lbls, times = counts)
  tbl
}
