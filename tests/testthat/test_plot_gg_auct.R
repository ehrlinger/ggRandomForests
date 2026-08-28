test_that("plot.gg_auct builds an AUC(t) ggplot with a 0.5 reference", {
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_noboot())
  p  <- plot(gg)
  expect_s3_class(p, "ggplot")
  expect_gt(nrow(ggplot2::layer_data(p)), 0)
  expect_equal(p$labels$y, "AUC(t)")
})

test_that("plot.gg_auct adds a ribbon when bootstrap CI is present (no warning)", {
  gg_b <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_boot())
  expect_no_warning(p_b <- plot(gg_b))          # finite-CI-only ribbon -> no 'removed rows' warning
  geoms_b <- vapply(p_b$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% geoms_b)

  gg_n <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_noboot())
  p_n  <- plot(gg_n)
  geoms_n <- vapply(p_n$layers, function(l) class(l$geom)[1], character(1))
  expect_false("GeomRibbon" %in% geoms_n)       # no CI -> no ribbon
})

test_that("plot.gg_auct rejects non-gg_auct input", {
  expect_error(plot.gg_auct(mtcars), "gg_auct")
})

test_that("plot.gg_auct drops NA AUC rows instead of warning", {
  # Under randomForestRHF >= 2.0.0 the final grid time can carry an NA AUC,
  # because the censoring-weight denominator is undefined once the control set
  # is nearly exhausted. Drop it rather than let geom_line() warn.
  gg <- gg_auct(.rhf_pbc(), auct_fit = .auct_pbc_noboot())
  p  <- plot(gg)
  expect_no_warning(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)))
  ld <- ggplot2::layer_data(p)
  expect_gt(nrow(ld), 0)
  expect_false(anyNA(ld$y))
})
