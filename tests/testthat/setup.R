# v2.8.0 Phase 0: randomForestSRC/randomForest are Imports, not attached.
# Survival-formula tests use a bare Surv(...) in rfsrc() formulas, which
# resolve in the test environment only if `survival` is attached. Attach
# the exact dependency surface the tests assume, here, once.
library(survival)
library(randomForestSRC)
library(randomForest)

# Grow every forest single-threaded, as the vignettes already do. Under
# OpenMP (Linux builds link libgomp; CRAN's macOS binary does not)
# randomForestSRC ignores set.seed(): measured 2026-10-07 on the
# test_snapshots.R fixtures, four threads moved Boston VIMP by about 2.0 and
# swapped two variables' ranks between runs, while one thread on Linux matched
# the macOS fits exactly. The vdiffr baselines are drawn single-threaded.
options(rf.cores = 1L)
