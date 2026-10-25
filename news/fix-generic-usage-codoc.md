* The help pages for `calc_roc()`, `gg_rfsrc()` and `gg_roc()` now show the
  generic call in their usage section, not only the `rfsrc` method. R-devel's
  `R CMD check` reports exported functions with no usage line as "Exported
  functions without usage information", and these three were the only ones.
  No argument or return value changed.
