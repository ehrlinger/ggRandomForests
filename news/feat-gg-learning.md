* New `gg_learning()` and `plot.gg_learning()` draw learning curves from saved
  `randomForestSRC::subsample()` results: OOB error, and the stability of the
  top-k VIMP ranking (Jaccard index and Kendall's tau across replicates),
  against the number of events (#402). `gg_error()` says whether there are
  enough trees; `gg_learning()` says whether there is enough data, for
  prediction and, separately, for the variable ranking. In simulation the
  second needed several times the events of the first. Like `gg_tune_rhf()`,
  it only tidies: run `subsample(performance = TRUE)` once per fraction and
  pass the list. Fractions with fewer than `min_events` events, and those
  above 0.5, are flagged and drawn open, because both distort the curve.
