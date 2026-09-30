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
# Internal helpers: stratify a survfit() call on `by` and label its rows.
#
# kaplan() and nelson() fit on the factor .strata_factor() returns, bound to
# the name `grp`, so survfit() names each stratum "grp=<level>" and the labels
# can be read back from the fit. They cannot be taken from the data: an option
# passed through `...` (subset, start.time) can drop a whole group from the
# fit, and survfit() reports only the groups it kept.

# The `by` values as a factor whose levels are the values themselves, sorted
# (a factor keeps its own level order). vals holds them in their own type.
.strata_factor <- function(by_col) {
  vals <- if (is.factor(by_col)) levels(by_col) else sort(unique(by_col))
  vals <- vals[!is.na(vals)]
  list(grp = factor(by_col, levels = vals), vals = vals)
}

# The rows a survfit() call on `srv` used: complete, and inside any `subset`
# passed through `...`. The subscript is applied to the row numbers, so it
# means what it means to survfit(): a logical vector, positive indices, or
# negative ones that exclude rows.
.fit_rows <- function(srv, subset = NULL) {
  kept <- !is.na(srv)
  if (is.logical(subset)) {
    kept <- kept & subset %in% TRUE
  } else if (!is.null(subset)) {
    kept <- kept & seq_along(kept) %in% seq_along(kept)[subset]
  }
  kept
}

# @param tbl     data.frame produced from survfit output, one row per time
# @param srv_tab the survfit object tbl was built from, fitted on `grp`
# @param strat   the list .strata_factor() returned
# @param kept    logical, the rows of the data that the fit used; read only
#   when `subset` left a single stratum, which survfit() does not name (one
#   left by start.time keeps its name)
#
# @return tbl with an additional $groups column containing the group label
#   for each row, in the type of the `by` column (levels, for a factor).
.label_strata <- function(tbl, srv_tab, strat, kept) {
  counts <- srv_tab$strata
  if (is.null(counts)) {
    present <- unique(as.character(strat$grp[kept & !is.na(strat$grp)]))
    if (length(present) != 1L) {
      stop("the fit kept a single stratum, and it cannot be matched to one ",
           "'by' group. Subset 'data' instead of passing the filter to ",
           "survfit().", call. = FALSE)
    }
    keys <- present
    counts <- nrow(tbl)
  } else {
    keys <- sub("^grp=", "", names(counts))
  }
  idx <- match(keys, as.character(strat$vals))
  if (anyNA(idx)) {
    stop("could not match the fitted strata to the 'by' groups.",
         call. = FALSE)
  }
  tbl$groups <- rep(strat$vals[idx], times = counts)
  tbl
}
