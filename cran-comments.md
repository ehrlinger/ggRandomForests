This is the ggRandomForests package submission v1.1.3
--------------------------------------------------------------------------------
* Add package vignette "ggRandomForests: Visually Exploring a Random Forest for Regression"
* Add `gg_partial_coplot`, `quantile_cuts` and `surface_matrix` functions
* export the `calc_roc` and `calc_auc` functions.
* replace tidyr function dependency with reshape2 (melt instead of gather) due to lazy eval issues.
* reduce dplyr dependencies (remove select and %>% usage for base equivalents, I still use tbl_df for printing)
* Further development of package vignette "Survival with Random Forests" 
* Refactor cached example datasets for better documentation, estimates and examples.
* Improved help files.
* Updated DESCRIPTION file to remove redundant parts.
* Misc Bug Fixes.
