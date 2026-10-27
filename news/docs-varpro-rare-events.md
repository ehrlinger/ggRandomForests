* The "varPro on small or sparse data" vignette has a new section, "What if
  the events are rare?". A simulation with known signal, at 60 and 150 events,
  shows that offering every variable (`split.weight = FALSE`) needs a smaller
  `mtry` before binary predictors get tested, that more trees and rules pay
  off only when there are events to use, that a real rare effect can score
  near zero at 60 events while counting as tested, and that a noise variable
  correlated with a signal scores like one. It ends with the settings we start
  from on sparse survival data. The summaries are precomputed by
  `vignettes/precompute_varpro_rare.R`.
