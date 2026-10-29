* The varPro vignette no longer calls `nvar` a red herring. On a guided fit
  (`split.weight = TRUE`, the default) varPro keeps only the `nvar` heaviest
  encoded columns, 30 by default, and builds no rule for the rest, so `nvar`
  caps which predictors compete, not just how many are reported. A factor with
  more than two levels takes one column per level, so it can fill several of
  those places. The vignette also says that `sparse` does nothing once
  `split.weight = FALSE`, that custom `split.weight.custom` weights override
  that switch, that unguided fits try every predictor at every split unless
  `mtry` is set, and that an eligible predictor is not necessarily a tested
  one (`varpro_coverage()` tells them apart).
