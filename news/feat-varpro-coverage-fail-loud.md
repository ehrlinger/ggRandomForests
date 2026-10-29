* `varpro_coverage()` gains `max_untested`. Set it, and the call stops with an
  error when more than that share of the real variables went untested, so a
  screen that tested too little cannot pass for one that found no effect. A
  factor counts as tested only when a single level was, not when its levels
  together reach `trim`. The default, `NULL`, only reports, as before. A new
  help section, "The first-pass filter", explains how `varpro()`'s default
  `split.weight = TRUE` and `nvar = 30` cut variables before any rule is
  built, and that `split.weight = FALSE`, without `split.weight.custom`, keeps
  every variable eligible.
