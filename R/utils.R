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

# --------------------------------------------------------------------------- #
# Variable-label resolution, shared by every plot method that draws variable
# names.  Base R only: 'ggRandomForests' is on CRAN and must not take a
# dependency on 'labelled' or on an internal package for a cosmetic feature.
#
# Three input shapes are accepted, because attr(x, "label") is a haven/SAS-era
# carrier that does not reliably survive a parquet round-trip.  The named-vector
# and key/label arms are format-agnostic and are the durable ones.
#' @keywords internal
.forest_labels <- function(labels) {
  if (is.null(labels)) {
    return(NULL)
  }

  ## Shape 3 first: a two-column key/label lookup (the shape
  ## hvtiRutilities::label_map() returns).  Checked before attribute reading so
  ## a lookup table is never mistaken for a labelled data frame.
  if (is.data.frame(labels) && all(c("key", "label") %in% names(labels))) {
    out <- as.character(labels[["label"]])
    names(out) <- as.character(labels[["key"]])
    return(.forest_labels_check(out))
  }

  ## Shape 1: a labelled data frame -- read attr(col, "label") per column.
  if (is.data.frame(labels)) {
    out <- vapply(labels, function(col) {
      lb <- attr(col, "label")
      if (is.null(lb) || !nzchar(as.character(lb)[1L])) {
        NA_character_
      } else {
        as.character(lb)[1L]
      }
    }, character(1L))
    return(.forest_labels_check(out[!is.na(out)]))
  }

  ## Shape 2: a named character vector.
  if (is.character(labels) && !is.null(names(labels))) {
    return(.forest_labels_check(labels))
  }

  stop("'labels' must be a named character vector, a labelled data frame, ",
       "or a two-column key/label data frame.", call. = FALSE)
}

## Warn once when a lookup resolves nothing at all.  The usual cause is that
## 'label' attributes were dropped in transit (a parquet round-trip written by a
## non-R stage does this silently), which otherwise presents as a figure that is
## simply unlabelled with no explanation.
#' @keywords internal
.forest_labels_check <- function(x) {
  if (length(x) == 0L) {
    warning("No variable labels were found. If the data came through a ",
            "parquet round-trip, 'label' attributes may have been dropped; ",
            "pass a named character vector instead.", call. = FALSE)
  }
  x
}

## Map raw variable names onto display labels, falling back per variable.  A
## variable with no label keeps its raw name: never blank, never an error.
#' @keywords internal
.apply_forest_labels <- function(vars, lookup) {
  vars <- as.character(vars)
  if (is.null(lookup) || length(lookup) == 0L) {
    return(vars)
  }
  out <- unname(lookup[vars])
  out[is.na(out)] <- vars[is.na(out)]
  out
}

## Facet-strip labeller built straight from the user's 'labels' argument, so the
## resolve-then-wrap pair is written once rather than at every faceted call site.
#' @keywords internal
.forest_strip_labeller <- function(labels) {
  lookup <- .forest_labels(labels)
  ggplot2::as_labeller(function(v) .apply_forest_labels(v, lookup))
}

## ---------------------------------------------------------------------------
## Rank the variables in a partialpro list by varPro importance.
##
## get.topvars() returns a SHORT ranked vector -- far shorter than the fit
## reaches -- so most names arrive unranked.  Those keep their incoming order and
## are appended after the ranked block; nothing is ever dropped.
#' @keywords internal
.varpro_importance_order <- function(part_dta, object) {
  nms <- names(part_dta)
  if (is.null(object) || is.null(nms)) {
    return(nms)
  }

  ranked <- tryCatch(as.character(varPro::get.topvars(object)),
                     error = function(e) character(0L))
  if (length(ranked) == 0L) {
    return(nms)
  }

  rank_key <- .varpro_rank_of(nms, ranked)
  ## seq_along() as the tiebreaker keeps incoming order stable among names that
  ## share a rank (in practice, all the unranked ones at Inf).
  nms[order(rank_key, seq_along(nms))]
}

## Position of each name in the ranked vector.  Exact match wins; failing that a
## one-hot level (name followed by digits, e.g. sex0/sex1) is accepted and the
## best -- lowest -- position across levels is taken.  Requiring digits keeps
## 'age' from being captured by 'age_group'.
#' @keywords internal
.varpro_rank_of <- function(nms, ranked) {
  vapply(nms, function(nm) {
    hit <- which(ranked == nm)
    if (length(hit) == 0L) {
      pat <- paste0("^", .escape_regex(nm), "[0-9]+$")
      hit <- grep(pat, ranked)
    }
    if (length(hit) == 0L) Inf else min(hit)
  }, numeric(1L))
}

#' @keywords internal
.escape_regex <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}
