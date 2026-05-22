# Voice-Audit Classification Table

This table records the documentation voice classification for every documentation
surface in ggRandomForests, as part of the documentation voice-audit (Task 2). The
package's prose is a mix of John Ehrlinger's original JSS-era writing (2014-2016) and
AI-written prose added during 2026-05 development. Classification is based on the
first-commit date of each R source file and the commit history of vignettes and
top-level docs.

- **original** — written by John Ehrlinger, 2014-2016 JSS-era; out of scope for rewrite.
- **AI** — file first committed in 2026-05; AI-written throughout.
- **mixed** — original file with substantive 2026-05 AI edits.

Original-voice rows are listed explicitly (Rewrite? = no) so the out-of-scope
decision is auditable. Rewrite happens in a later task — this table changes nothing.

| Surface | Register | Verdict | Rewrite? |
|---|---|---|---|
| R/autoplot_methods.R (roxygen) | both | AI | yes |
| R/gg_brier.R (roxygen) | both | AI | yes |
| R/plot.gg_brier.R (roxygen) | both | AI | yes |
| R/gg_partial_varpro.R (roxygen) | both | AI | yes |
| R/plot.gg_partial_varpro.R (roxygen) | both | AI | yes |
| R/gg_varpro.R (roxygen) | both | AI | yes |
| R/plot.gg_varpro.R (roxygen) | both | AI | yes |
| R/gg_udependent.R (roxygen) | both | AI | yes |
| R/plot.gg_udependent.R (roxygen) | both | AI | yes |
| R/print_methods.R (roxygen) | both | AI | yes |
| R/print_helpers.R (roxygen) | both | AI | yes |
| R/summary_methods.R (roxygen) | both | AI | yes |
| R/ribbon_style.R (roxygen) | both | AI | yes |
| R/utils.R (roxygen) | both | AI | yes |
| R/gg_roc.R (roxygen) | both | mixed | yes |
| R/plot.gg_roc.R (roxygen) | both | mixed | yes |
| R/calc_roc.R (roxygen) | both | mixed | yes |
| R/gg_variable.R (roxygen) | both | mixed | yes |
| R/plot.gg_variable.R (roxygen) | both | mixed | yes |
| R/gg_error.R, R/plot.gg_error.R (roxygen) | both | original | no |
| R/gg_partial.R, R/plot.gg_partial.R (roxygen) | both | original | no |
| R/gg_rfsrc.R, R/plot.gg_rfsrc.R (roxygen) | both | original | no |
| R/gg_survival.R, R/plot.gg_survival.R (roxygen) | both | original | no |
| R/gg_vimp.R, R/plot.gg_vimp.R (roxygen) | both | original | no |
| R/calc_roc.R helper (kaplan.R, nelson.R) (roxygen) | both | original | no |
| R/quantile_pts.R (roxygen) | both | original | no |
| R/help.R, R/ggrandomforests.news.R, R/zzz.R (roxygen) | both | original | no |
| R/gg_partial_rfsrc.R, R/gg_partialpro.R (roxygen) | both | original | no |
| R/surv_partial.rfsrc.R, R/varpro_feature_names.R (roxygen) | both | original | no |
| vignettes/ggRandomForests.qmd | narrative | original | no |
| vignettes/ggRandomForests-regression.qmd | narrative | original | no |
| vignettes/ggRandomForests-survival.qmd | both | mixed | yes |
| README.md | both | mixed | yes |
| NEWS.md | terse | mixed | yes |
