####**********************************************************************
####  Internal: shared styling constants for ribbon overlays.
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
