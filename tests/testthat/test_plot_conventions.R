# Cross-function convention: every gg_* importance plot must put the
# most-important variable at the TOP of the horizontal layout. After
# coord_flip(), the topmost variable is the LAST factor level of the
# variable axis. This test pins that convention across the family so a
# future wrapper can't silently reintroduce a bottom-heavy ordering
# (the bug fixed in the v3.0.0 importance-plot-ordering work).

test_that("importance plots agree: most-important variable is the last factor level", {
  skip_if_not_installed("varPro")
  skip_if_not_installed("randomForestSRC")

  # ---- gg_vimp (rfsrc regression) ----------------------------------------
  set.seed(20260528L)
  rf <- randomForestSRC::rfsrc(mpg ~ ., data = mtcars, ntree = 100,
                               importance = TRUE)
  gv <- gg_vimp(rf)
  top_vimp <- as.character(gv$vars[which.max(gv$vimp)])
  expect_equal(tail(levels(gv$vars), 1), top_vimp,
               label = "gg_vimp top factor level")

  # ---- gg_varpro (varpro regression) -------------------------------------
  v  <- .varpro_mtcars()
  gvp <- gg_varpro(v)
  top_varpro <- as.character(
    gvp$stats$variable[which.max(gvp$stats$median)]
  )
  expect_equal(tail(levels(gvp$imp$variable), 1), top_varpro,
               label = "gg_varpro top factor level")

  # ---- gg_beta_varpro (varpro regression) --------------------------------
  b  <- .beta_fit_mtcars()
  gb <- gg_beta_varpro(v, beta_fit = b)
  top_beta <- as.character(gb$variable[which.max(gb$beta_mean)])
  expect_equal(tail(levels(gb$variable), 1), top_beta,
               label = "gg_beta_varpro top factor level")

  # ---- gg_ivarpro (varpro regression) ------------------------------------
  vb <- .varpro_boston()
  iv <- .ivarpro_boston()
  gi <- gg_ivarpro(vb, ivarpro_fit = iv)
  agg_i <- tapply(abs(gi$local_imp), gi$variable, mean, na.rm = TRUE)
  top_ivar <- names(which.max(agg_i))
  expect_equal(tail(levels(gi$variable), 1), top_ivar,
               label = "gg_ivarpro top factor level")
})
