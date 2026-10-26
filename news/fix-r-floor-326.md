* `Depends` now reads `R (>= 4.3.0)`, down from `R (>= 4.4.0)`. The old floor
  was the internal hvtiR family's policy, not anything ggRandomForests needs.
  The new one is the floor of `randomForestSRC` and `varPro`, the strictest of
  the `Imports`; the package code itself needs nothing past R 4.1. A new
  `R CMD check` leg on R 4.3 tests the floor on every push to `main`
  (#326).
