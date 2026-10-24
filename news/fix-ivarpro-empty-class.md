* `gg_ivarpro()` no longer fails with "replacement has 1 row, data has 0"
  when every local-importance value for a class is nonfinite. The empty
  per-class frame now takes its `class` column without error: a fit where
  every class is empty returns an empty `gg_ivarpro`, and one where only some
  are drops those classes. Reported by Udaya Kogalur.
