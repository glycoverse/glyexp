test_that("split works on samples", {
  res <- split(toy_experiment, group, where = "sample_info")
  expect_equal(length(res), 2)
  expect_equal(names(res), c("A", "B"))
  expect_equal(res$A$sample_info$sample, c("S1", "S2", "S3"))
  expect_equal(res$B$sample_info$sample, c("S4", "S5", "S6"))
  expect_equal(res$A$expr_mat, toy_experiment$expr_mat[, 1:3])
  expect_equal(res$B$expr_mat, toy_experiment$expr_mat[, 4:6])
})

test_that("split works on variables", {
  res <- split(toy_experiment, glycan_composition, where = "var_info")
  expect_equal(length(res), 2)
  expect_equal(names(res), c("H5N2", "N3N2"))
  expect_equal(res$H5N2$var_info$variable, c("V1", "V2"))
  expect_equal(res$N3N2$var_info$variable, c("V3", "V4"))
  expect_equal(res$H5N2$expr_mat, toy_experiment$expr_mat[1:2, ])
  expect_equal(res$N3N2$expr_mat, toy_experiment$expr_mat[3:4, ])
})

test_that("split works with non-existing column", {
  expect_error(
    split(toy_experiment, non_existing_column, where = "sample_info"),
    "Column non_existing_column not found in sample_info"
  )
})

test_that("split works with one element", {
  exp <- toy_experiment %>%
    mutate_obs(group = "A")
  res <- split(exp, group, where = "sample_info")
  expect_equal(length(res), 1)
  expect_equal(res$A, exp)
})