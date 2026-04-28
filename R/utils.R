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
# survfit() concatenates strata end-to-end in ascending-time order. Stratum
# boundaries are detected by finding rows where the time column resets
# (i.e. time[i] < time[i-1]).
#
# @param tbl     data.frame produced from survfit output (must have $time col)
# @param data    original data.frame passed to kaplan()/nelson()
# @param by      character; name of the grouping column in data
#
# @return tbl with an additional $groups column containing the group label
#   for each row.
.label_strata <- function(tbl, data, by) {
  # Use levels() for factors to respect the existing ordering; fall back to
  # unique() (in order of first appearance) for character/numeric vectors.
  by_col <- data[[by]]
  lbls <- if (is.factor(by_col)) levels(by_col) else unique(by_col)

  # Single stratum or fewer than 2 rows: label everything with first group
  if (nrow(tbl) < 2L) {
    tbl$groups <- lbls[1L]
    return(tbl)
  }

  # Detect stratum boundaries where the time column resets
  tm_splits <- which(c(FALSE, sapply(seq(2L, nrow(tbl)), function(ind) {
    tbl$time[ind] < tbl$time[ind - 1L]
  })))

  tbl$groups <- lbls[1L]
  if (length(tm_splits) > 0L) {
    for (ind in seq_along(tm_splits)) {
      tbl$groups[tm_splits[ind]:nrow(tbl)] <- lbls[ind + 1L]
    }
  }
  tbl
}
