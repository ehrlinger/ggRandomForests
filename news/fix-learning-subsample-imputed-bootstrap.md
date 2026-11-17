* `learning_subsample()` leaves out the whole-cohort double bootstrap, with a
  warning, when the forest imputed missing values (`na.action =
  "na.impute"` on data with any missing value). `randomForestSRC` 3.10.0
  crashes R double-bootstrapping such a forest, in every family; the
  subsamples are unaffected, and `gg_learning()` draws the curve without the
  whole-cohort point.
