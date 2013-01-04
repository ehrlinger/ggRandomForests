ggrfsrc Package
========================================================
grfsrc is intended to replace the plotting functions of the randomForestSRC (RF-SRC) package with equivalent graphics using the ggplot2 package. The advantage of using ggplot2 is to allow manipulation of the figures  for reporting and publication purposes.

We include a series of plot functions for working  with rfsrc objects:
* ggplot.rfsrc Plot out-of-bag (OOB) error rates and variable importance (VIMP) from a RF-SRC analysis. This is the default plot method for the package.
* ggplot.variable (*ggplot.variable.rfsrc*) plots marginal (or partial) effects of variables on an rfsrc response. 
* ggplot.survival (*ggplot.survival.rfsrc*) Plot various RF-SRC survival estimates.
* ggplot.competing.risk (*ggplot.competing.risk.rfsrc*)Plot the ensemble cumulative incidence function (CIF) and cause-specific cumulative hazard function (CSCHF) from a competing risk analysis.

## References
Ishwaran H. and Kogalur U.B. (2012). Random Forests for Survival, Regression and Classification (RF-SRC), 
R package version 1.0.2.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R. R News 7(2), 25--31.

Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random survival forests. Ann. Appl. Statist. 2(3), 841--860.

Friedman J.H. (2001). Greedy function approximation: a gradient boosting machine, Ann. of Statist., 5:1189-1232.

Ishwaran H., Gerds, T.A. Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M. (2012). Random survival forests for competing risks.

H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.