This is ggRandomForests package submission v2.7.1
-------------------------------------------------------------------------
This is a bug-fix release. Key changes:

* Fix `gg_partial_rfsrc()` for survival forests: `partial.rfsrc()` is now
  called with `partial.type = "surv"` (default; also accepts `"chf"` /
  `"mort"`). Without this, a zero-length comparison inside the underlying
  C code aborted the call and left the survival-vignette partial-dep chunks
  empty.
* Fix `gg_partial_rfsrc()` for multiple `partial.time` values: yhat is
  reshaped from the matrix returned by `get.partial.plot.data()` into
  long form so each `(x, time)` pair is one row.
* Improve `plot.gg_partial_rfsrc()` survival layout: predictor on the
  x-axis with one curve per time point coloured by `Time`, faceted by
  variable name.
* New regression test file `test_plot_layer_data.R` uses
  `ggplot2::layer_data()` to verify each `plot.gg_*()` method renders
  non-empty layers across all forest families, catching empty-figure
  regressions without visual inspection.

## R CMD check results
0 errors | 0 warnings | 0 notes

## Test environments
* local R installation (R 4.5, macOS)
* GitHub Actions: ubuntu-latest (R devel)
* GitHub Actions: ubuntu-latest (R release)
* GitHub Actions: ubuntu-latest (R oldrel-1)
* GitHub Actions: windows-latest (R release)
* GitHub Actions: macos-latest (R release)
