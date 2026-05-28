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

# Companion contract: reversing the factor levels for the plot must NOT
# reorder the public data frames. Row order stays most-important-FIRST so
# callers inspecting $imp / $stats / the long frames (and summary()) see a
# stable, descending-importance order. Pins the regression caught in PR #101
# review (rows had followed the reversed factor into least-important-first).

test_that("importance frames keep rows most-important-first despite reversed levels", {
  skip_if_not_installed("varPro")
  skip_if_not_installed("randomForestSRC")

  # ---- gg_varpro: $imp and $stats rows descend by median z ----------------
  v   <- .varpro_mtcars()
  gvp <- gg_varpro(v)
  top_varpro <- as.character(gvp$stats$variable[which.max(gvp$stats$median)])
  expect_equal(as.character(gvp$imp$variable[1]), top_varpro,
               label = "gg_varpro $imp first row")
  expect_equal(as.character(gvp$stats$variable[1]), top_varpro,
               label = "gg_varpro $stats first row")
  expect_true(all(diff(gvp$stats$median) <= 0),
              label = "gg_varpro $stats median is non-increasing")

  # ---- gg_beta_varpro: rows descend by beta_mean --------------------------
  b  <- .beta_fit_mtcars()
  gb <- gg_beta_varpro(v, beta_fit = b)
  top_beta <- as.character(gb$variable[which.max(gb$beta_mean)])
  expect_equal(as.character(gb$variable[1]), top_beta,
               label = "gg_beta_varpro first row")
  expect_true(all(diff(gb$beta_mean) <= 0),
              label = "gg_beta_varpro beta_mean is non-increasing")

  # ---- gg_ivarpro: first long-frame row is the most-important variable ----
  vb <- .varpro_boston()
  iv <- .ivarpro_boston()
  gi <- gg_ivarpro(vb, ivarpro_fit = iv)
  agg_i <- tapply(abs(gi$local_imp), gi$variable, mean, na.rm = TRUE)
  top_ivar <- names(which.max(agg_i))
  expect_equal(as.character(gi$variable[1]), top_ivar,
               label = "gg_ivarpro long-frame first row")
})
