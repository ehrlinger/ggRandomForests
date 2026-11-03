* `plot.gg_rhf()` no longer draws a straight line across an internal gap in a
  case's start-stop path. Each run of defined values is now drawn as its own
  line, so a gap shows as a gap.

* `?gg_rhf` and `?plot.gg_rhf` no longer say the cumulative hazard stays flat
  through an internal gap. `randomForestRHF` documents it that way, but 2.1.0
  returns `chf` as `NA` from a case's first gap to the end of its follow-up,
  even where the hazard is defined. The tests now fit a forest whose cases have
  several start-stop rows with gaps between them, and check `gg_rhf()`,
  `plot.gg_rhf()`, `gg_auct()` and `gg_rhf_importance()` on it.
