# v2.8.0 Phase 0: randomForestSRC/randomForest are Imports, not attached.
# Survival-formula tests use a bare Surv(...) in rfsrc() formulas, which
# resolve in the test environment only if `survival` is attached. Attach
# the exact dependency surface the tests assume, here, once.
library(survival)
library(randomForestSRC)
library(randomForest)
