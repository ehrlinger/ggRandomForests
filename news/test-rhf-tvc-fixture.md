* `?gg_rhf` and `?plot.gg_rhf` no longer say the cumulative hazard stays flat
  through an internal gap in a case's start-stop path. `randomForestRHF`
  documents it that way, but 2.1.0 returns `chf` as `NA` inside a gap, the
  same as the hazard. The tests now fit a forest whose cases have several
  start-stop rows with gaps between them, and check that `gg_rhf()`,
  `gg_auct()` and `gg_rhf_importance()` handle it.
