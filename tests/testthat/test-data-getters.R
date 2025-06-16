test_that("getter functions work", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  exp <- new_experiment(expr_mat, sample_info, var_info, list())

  expect_equal(get_expr_mat(exp), expr_mat)
  expect_equal(get_sample_info(exp), sample_info)
  expect_equal(get_var_info(exp), var_info)
})


test_that("motifying data returned by getters doesn't affect exp", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  exp <- new_experiment(expr_mat, sample_info, var_info, list())

  expr_mat2 <- get_expr_mat(exp)
  sample_info2 <- get_sample_info(exp)
  var_info2 <- get_var_info(exp)

  expr_mat2[1, 1] <- 100
  sample_info2[1, 1] <- "S100"
  var_info2[1, 1] <- "V100"

  expect_equal(get_expr_mat(exp), expr_mat)
  expect_equal(get_sample_info(exp), sample_info)
  expect_equal(get_var_info(exp), var_info)
})
