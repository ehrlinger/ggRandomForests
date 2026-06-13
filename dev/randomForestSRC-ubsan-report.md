# UBSAN: 0-length-array pointer arithmetic in `rfsrcGrow` (entry.c:184)

**Package:** randomForestSRC (observed in 3.6.2, latent in earlier versions
incl. 3.3.5)
**Reporter:** John Ehrlinger (ggRandomForests maintainer)
**Severity:** Undefined behaviour flagged by gcc-UBSAN; numerically benign in
practice, but CRAN treats it as an "Additional issue."

## What CRAN reports

gcc-UBSAN, surfaced via the ggRandomForests test suite (which grows varPro
forests):

```
entry.c:184:55: runtime error: pointer index expression with base
0x000000000001 overflowed to 0xfffffffffffffff9
    #0 rfsrcGrow .../randomForestSRC/src/entry.c:184
```

## Root cause

`rfsrcGrow()` builds 1-offset working pointers from incoming R weight vectors:

```c
// src/entry.c (3.6.2), lines 183-185
RF_xWeightStat = REAL(xWeightStat);  RF_xWeightStat--;   // 183
RF_yWeight     = REAL(yWeight);      RF_yWeight--;        // 184  <-- UBSAN
RF_xWeight     = REAL(xWeight);      RF_xWeight--;        // 185
```

When the corresponding R vector has **length 0**, `REAL()` returns R's
empty-vector sentinel address `0x1`. The unconditional `ptr--` then forms
`0x1 - sizeof(double) = 0x1 - 8 = 0xfffffffffffffff9`. *Forming* this
out-of-bounds pointer is undefined behaviour (UBSAN, column 55 = the `--`),
even though the package never dereferences `RF_yWeight[i]` when the weight is
empty — so the result is numerically correct and the bug is otherwise silent.

`yWeight` (R-side `yvar.wt`) is length 0 whenever the y-weight vector is
empty. The R glue produces exactly that for the **unsupervised** family:

```r
# R/rfsrc.R
if (family == "unspv") {
  yvar.wt <- NULL          # -> as.double(NULL) -> numeric(0) -> length-0 yWeight
} else {
  yvar.wt <- get.weight(yvar.wt, length(yvar.types))
}
```

So every unsupervised forest already forms this pointer; varPro reaches it
because its rule-grow machinery fits forests through that path.

### Minimal reproduction (no varPro needed)

```r
library(randomForestSRC)
d <- data.frame(y = rnorm(60), x1 = rnorm(60), x2 = rnorm(60))
rfsrc(Unsupervised() ~ ., d, ntree = 10)   # yvar.wt is numeric(0) -> entry.c:184
```

Confirmed by tracing `randomForestSRC:::get.weight`: the unsupervised path
returns a length-0 `yvar.wt` (`n=0, outlen=0`); regression returns length 1.

Note the sibling pointers `RF_subjWeight`, `RF_eventType`, etc. are guarded
with `if (VECTOR_ELT(...) != R_NilValue)`, but these three are not — and
`as.double(numeric(0))` is never `R_NilValue`, so nothing catches the empty
case.

## Suggested fix

Only decrement when the vector is non-empty (mirrors the existing
NULL-guarded idiom; the empty pointer is never indexed downstream):

```c
RF_xWeightStat = REAL(xWeightStat); if (LENGTH(xWeightStat) > 0) RF_xWeightStat--;
RF_yWeight     = REAL(yWeight);     if (LENGTH(yWeight)     > 0) RF_yWeight--;
RF_xWeight     = REAL(xWeight);     if (LENGTH(xWeight)     > 0) RF_xWeight--;
```

(In this repo's amalgamated `src/randomForestSRC.c` the same three lines are
at ~36965-36967.) The identical guard should be applied anywhere else
`rfsrcGrow`/`rfsrcPredict` does `REAL(w); w--` / `INTEGER(w); w--` on a vector
that can arrive length 0.

## Downstream impact

ggRandomForests 3.1.1 works around this by not exercising the path under
`R CMD check` (varPro tests `skip_on_cran()`; the varpro vignette loads
precomputed fits). A guard in randomForestSRC would remove the UB at the
source for all callers.
