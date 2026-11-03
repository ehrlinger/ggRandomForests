* Two plot methods take their arguments in a new order, which matters only to
  a call that passes them by position. In `plot.gg_variable()` the third
  argument is now `panel`, where in 3.5.5 it was `time` and `panel` was fifth;
  `oob` and `points` move up two places with it. In
  `plot.gg_partial_varpro()` the third argument is now `labels`, where in 3.5.5
  it fell into `...`. Calls that name their arguments, as the examples do, are
  unchanged.
