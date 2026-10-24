* `?gg_partial_varpro` no longer says that `scale = "surv"` is computed
  through `gg_partial_rfsrc()`. Only `scale = "chf"` takes that route;
  `"surv"` uses the `partialpro()` S(tau) learner, and its `time` is the
  horizon tau, defaulting to the median follow-up time.
