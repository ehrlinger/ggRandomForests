* `gg_varpro()` gains `noise` and `kind_values`. Name the permuted noise
  columns you added before fitting (for example `noise = "^noise"`), and a
  variable is selected when it is tested and its z beats the best tested noise
  of its own kind, categorical or continuous, the rule `varpro_coverage()`
  uses. The bar comes from the same rules as the boxes and is judged before
  `nvar` truncation. `plot()` then draws the noise bar in place of the 0.79
  line: one line when there is a single bar, a dashed tick on each row when the
  two kinds differ. Noise boxes are orange. `print()` and `summary()` say how
  the selection was made. Without `noise`, nothing changes. The `cutoff` help
  now says that varPro uses 0.79 only to highlight its plot.
