test_that("constructor of experiment works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info, list(foo = "bar"))

  expect_equal(exp$name, "my_exp")
  expect_equal(exp$expr_mat, expr_mat)
  expect_equal(exp$sample_info, sample_info)
  expect_equal(exp$var_info, var_info)
  expect_equal(exp$meta_data$foo, "bar")
})


test_that("different samples in `sample_info` and `expr_mat` raises an error", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info, list())

  msg <- "samples inn `sample_info` and `expr_mat` are different"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("different variables in `var_info` and `expr_mat` raises an error", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2"))
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info, list())

  msg <- "variables in `var_info` and `expr_mat` are different"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("missing 'sample' column in `sample_info` raises an error", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  sample_info <- sample_info[, -1]
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info, list())

  msg <- "missing 'sample' column in `sample_info`"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("missing 'variable' column in `var_info` raises an error", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  var_info <- var_info[, -1]
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info, list())

  msg <- "missing 'variable' column in `var_info`"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("validate duplicated 'sample' column", {
  sample_info <- create_sample_info(c("S1", "S2", "S2"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  expr_mat <- create_expr_mat(c("S1", "S2", "S2"), c("V1", "V2", "V3"))
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info, list())

  msg <- "duplicated 'sample' column in `sample_info`"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("validate duplicated 'variable' column", {
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V2"))
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V2"))
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info, list())

  msg <- "duplicated 'variable' column in `var_info`"
  expect_error(validate_experiment(exp), regexp = msg)
})
