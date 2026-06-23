# Issue #118: gg_varpro() on some survival fits failed with the cryptic
# "arguments imply differing number of rows: <p>, 0" when varPro::importance()
# returns a degenerate importance table (0 rows, or p rows with no usable `z`
# column). The guard in .build_varpro_imp_dfs() must turn that into a clear,
# specific message — scoped to the degenerate case, NOT a blanket survival
# block (cf. the reverted #116).
#
# These exercise the internal directly with a constructed importance table, so
# they run on CRAN with no varPro grow.

.dummy_tree <- function() matrix(0, 1, 1, dimnames = list("t1", "x"))

test_that("0-row importance table fails with the #118 message, not a cryptic one", {
  imp0 <- data.frame(mean = numeric(0), std = numeric(0), z = numeric(0))
  err <- tryCatch(
    ggRandomForests:::.build_varpro_imp_dfs(
      imp0, .dummy_tree(), "surv", 0.79, NULL, FALSE, TRUE, FALSE),
    error = function(e) conditionMessage(e))
  expect_match(err, "no usable importance")
  expect_match(err, "surv")
  expect_false(grepl("differing number of rows", err))
})

test_that("importance with p rows but no z column fails clearly (the real #118 shape)", {
  # p named variables, columns present in working fits EXCEPT `z`
  imp_noz <- data.frame(mean = c(1, 2, 3), std = c(.1, .2, .3),
                        row.names = c("age", "bili", "albumin"))
  err <- tryCatch(
    ggRandomForests:::.build_varpro_imp_dfs(
      imp_noz, .dummy_tree(), "surv", 0.79, NULL, FALSE, TRUE, FALSE),
    error = function(e) conditionMessage(e))
  expect_match(err, "no usable importance")
  expect_false(grepl("differing number of rows", err))
})

test_that("a well-formed importance table is NOT blocked (working path preserved)", {
  # 3 variables with a real z column + a matching per-tree matrix
  imp_ok <- data.frame(mean = c(1, 2, 3), std = c(1, 1, 1), z = c(0.5, 1.5, 2.5),
                       row.names = c("age", "bili", "albumin"))
  tree <- matrix(rnorm(9), nrow = 3,
                 dimnames = list(c("t1", "t2", "t3"),
                                 c("age", "bili", "albumin")))
  dfs <- ggRandomForests:::.build_varpro_imp_dfs(
    imp_ok, tree, "surv", 0.79, NULL, FALSE, FALSE, FALSE)
  expect_named(dfs, c("imp", "imp_tree", "stats", "conditional"))
  expect_equal(nrow(dfs$imp), 3L)
  expect_setequal(as.character(dfs$imp$variable), c("age", "bili", "albumin"))
})
