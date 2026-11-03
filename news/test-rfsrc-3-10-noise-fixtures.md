* Two noise-bar tests no longer depend on which variables a seeded forest
  happens to score. `randomForestSRC` 3.10.0 grows a different forest from the
  same seed, which failed them on every platform.
