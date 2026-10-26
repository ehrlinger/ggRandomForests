* `gg_ivarpro()` mirrors a binary profile into the second class only when it
  explains the forest's own probability of the first level. A `y.external`
  target named after a class is any numeric response, not a class
  probability: it is kept as a single profile, filed under its target's name
  when that is a response level and under the last level otherwise, and the
  binary default view shows it. The one case that cannot be told apart is a
  precomputed `y.external` fit named after the first level and run with
  `save.data = FALSE`; it is mirrored.

* `gg_partial_varpro()` keeps each categorical variable's level order in
  `$categorical$variable` when `part_dta` carries factor-valued `xorg`, so the
  plot no longer lists the categories alphabetically. The column is a factor
  whose levels run through each variable's levels in turn, and it is ordered
  when every categorical variable is an ordered factor.
