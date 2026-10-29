* The varPro vignette no longer calls `nvar` a red herring. On a guided fit
  (`split.weight = TRUE`, the default) varPro keeps only the `nvar` heaviest
  predictors, 30 by default, and builds no rule for the rest, so `nvar` caps
  which predictors compete, not just how many are reported. The vignette also
  says that `sparse` does nothing once `split.weight = FALSE`, and that
  unguided fits try every predictor at every split unless `mtry` is set.
