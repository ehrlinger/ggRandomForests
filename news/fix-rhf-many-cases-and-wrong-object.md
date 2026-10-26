* `plot.gg_rhf()` gives each case its own colour and legend key only while
  there are at most 10 cases. With more, which includes the default of every
  case on a real cohort, the curves are drawn as one translucent bundle with no
  legend. Before this change a 2,386-patient fit drew a legend that filled the
  page with record ids.

* `gg_auct()` and `gg_rhf_importance()` called on the `auct.rhf` or
  `importance.rhf` result, rather than on the forest, now say to pass that
  result as `auct_fit =` or `importance_fit =`. They used to fail with "no
  applicable method".
