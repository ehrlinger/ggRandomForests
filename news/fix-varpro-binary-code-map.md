* `gg_varpro(conditional = TRUE)` on a binary classification forest now reads
  which class each of varPro's 0/1 codes stands for from the data. varPro
  codes by frequency, not level order: the minority class is `1`, and on a
  tie the first level is. Mapping by position put each class's z-scores under
  the other class's name whenever the first level was the minority or the
  classes were tied, as on a balanced fit. When the codes cannot be paired
  with the original labels, they are kept as `0` and `1` rather than guessed.
