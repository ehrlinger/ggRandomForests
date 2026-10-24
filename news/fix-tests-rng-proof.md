* Two tests no longer depend on the `randomForestSRC` random-number stream.
  The live `gg_sdependent()` test computes its beta fit once and gives it to
  both sides, and the `plot.gg_varpro()` test for a class with no defined z
  sets that z by hand. Both failed against the `randomForestSRC` development
  release, whose new seed generator grows a different forest from the same
  seed.
