* `plot.gg_learning()` draws the whole-cohort double bootstrap as a square,
  labeled "whole cohort (expected)": it draws with replacement, so its event
  count is the number expected in each draw, not an exact one. The square is
  open when that count is below `min_events`. `gg_learning()` now warns when
  `top_k` covers every predictor and gives `jaccard` as `NA` there, since a
  top set of all the predictors is the same in every replicate and would
  score a perfect 1 however the ranking moved.
