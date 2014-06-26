ggRandomForests Package
========================================================

Main Goal: Simplify graphical representation of randomForests.

ggRandomForests is designed to be used with the randomForestSRC (RF-SRC) package. The package has two design goals:

* Extract data.frame objects for analytics for the randomForest[SRC] model.
* Encapsulate the generation of graphic elements from these data frames.

We have chosen to use the ggplot2 package for graphics, as it allows the user to modify the output in an intuitive manner.

We include a series of functions for working  with rfsrc objects:
* ggRFsrc Prediction from the randomForest[SRC] 
* ggError Track RF convergence as OOB stability
* ggROC Receiver Operator Charactertics
* ggVimp Variable importance
* ggMinimalDepth Minimal Depth variable selection
* ggVariable variable dependence 
* ggPartial partial variable dependence

We use the S3 object model to provide plot methods each of these objects.

## References
Ishwaran H. and Kogalur U.B. (2014). Random Forests for Survival, Regression and Classification (RF-SRC),
R package version 1.4.0.14.

Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R. R News 7(2), 25--31.

Ishwaran H., Kogalur U.B., Blackstone E.H. and Lauer M.S. (2008). Random survival forests. Ann. Appl. Statist. 2(3), 841--860.

Friedman J.H. (2001). Greedy function approximation: a gradient boosting machine, Ann. of Statist., 5:1189-1232.

Ishwaran H., Gerds, T.A. Kogalur U.B., Moore R.D., Gange S.J. and Lau B.M. (2012). Random survival forests for competing risks.

H. Wickham. ggplot2: elegant graphics for data analysis. Springer New York, 2009.