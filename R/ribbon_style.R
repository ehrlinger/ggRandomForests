####**********************************************************************
####  Internal: shared styling constants and helpers for ribbon overlays.
####
####  Every plot.gg_* method that draws a ribbon (CI band on KM/NA curves,
####  bootstrap CI band on gg_rfsrc survival curves, per-subject envelope
####  on gg_brier) uses these constants so the family renders uniformly.
####  This is styling only — the underlying probability/coverage of each
####  ribbon is computed elsewhere and is intentionally heterogeneous
####  (95% inferential CIs vs. 15-85 percentile descriptive envelopes).
####**********************************************************************

# Canonical ribbon transparency. Low enough that overlapping bands stay
# readable when several models are layered, high enough to read on white.
.gg_ribbon_alpha <- 0.2

# Default fill for single-series ribbons (no group aesthetic). Group-coloured
# ribbons override via aes(fill = group) and ignore this constant.
.gg_ribbon_fill <- "steelblue"

# Split a captured list(...) into ribbon and step components.
#
# When the user passes alpha = x to a plot.gg_* method, that alpha is
# meant for the main line/step geom.  The ribbon should use a softer
# value (half of the user alpha, or the package default if none supplied).
# This helper centralises that logic and prevents the duplicate-formal
# error that arises when alpha is both in ... and set explicitly on geom_ribbon.
#
# Returns a list with:
#   $ribbon_alpha  — numeric alpha to pass explicitly to geom_ribbon()
#   $step_dots     — original dots (forwarded to geom_step / geom_line)
#   $ribbon_dots   — dots with alpha removed (ribbon alpha is explicit)
.gg_split_alpha <- function(dots) {
  user_alpha <- dots[["alpha"]]
  list(
    ribbon_alpha = if (is.null(user_alpha)) .gg_ribbon_alpha
                   else user_alpha * 0.5,
    step_dots    = dots,
    ribbon_dots  = dots[setdiff(names(dots), "alpha")]
  )
}
