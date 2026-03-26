This is ggRandomForests package submission v2.7.0
-------------------------------------------------------------------------
This is a bug-fix and code-quality release. Key changes:

* Fix critical visual bug: `aes()` calls throughout `plot.gg_rfsrc` and
  `plot.gg_roc` used bare string literals instead of `.data[[col]]`,
  causing aesthetics to map to constant strings rather than data columns.
* Fix `bootstrap_survival` CI-band indexing and `gg_rfsrc.randomForest`
  incorrect use of non-existent `object$xvar` field.
* Fix `seq_len(nvar)` vs `1:nvar` silent bug in `gg_vimp` and `plot.gg_vimp`.
* Full test suite migration to testthat 3.x API.
* Improved GitHub Actions CI (lintr enforcement, warnings-as-errors).

## R CMD check results
0 errors | 0 warnings | 0 notes

## Test environments
* local R installation (R 4.4, macOS)
* GitHub Actions: ubuntu-latest (R devel)
* GitHub Actions: ubuntu-latest (R release)
* GitHub Actions: ubuntu-latest (R oldrel-1)
* GitHub Actions: windows-latest (R release)
* GitHub Actions: macos-latest (R release)
