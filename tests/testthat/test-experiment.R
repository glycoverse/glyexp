test_that("constructor of experiment works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info)

  expect_equal(exp$name, "my_exp")
  expect_equal(exp$expr_mat, expr_mat)
  expect_equal(exp$sample_info, sample_info)
  expect_equal(exp$var_info, var_info)
})


test_that("different samples in `sample_info` and `expr_mat` raises an error", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info)

  msg <- "samples inn `sample_info` and `expr_mat` are different"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("different variables in `var_info` and `expr_mat` raises an error", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2"))
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info)

  msg <- "variables in `var_info` and `expr_mat` are different"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("missing 'sample' column in `sample_info` raises an error", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  sample_info <- sample_info[, -1]
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info)

  msg <- "missing 'sample' column in `sample_info`"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("missing 'variable' column in `var_info` raises an error", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))
  var_info <- var_info[, -1]
  exp <- new_experiment("my_exp", expr_mat, sample_info, var_info)

  msg <- "missing 'variable' column in `var_info`"
  expect_error(validate_experiment(exp), regexp = msg)
})


test_that("helper for creating experiment works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  exp <- experiment("my_exp", expr_mat, sample_info, var_info)

  expect_equal(exp$name, "my_exp")
  expect_equal(exp$expr_mat, expr_mat)
  expect_equal(exp$sample_info, sample_info)
  expect_equal(exp$var_info, var_info)
})


test_that("data.frames as input data works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  sample_info_df <- tibble::column_to_rownames(sample_info, "sample")
  var_info <- create_var_info(c("V1", "V2", "V3"))
  var_info_df <- tibble::column_to_rownames(var_info, "variable")

  exp <- experiment("my_exp", expr_mat, sample_info_df, var_info_df)

  expect_identical(exp$sample_info, sample_info)
  expect_identical(exp$var_info, var_info)
})


test_that("creating experiment with wrong order of samples and variables works", {
  expr_mat <- create_expr_mat(c("S3", "S1", "S2"), c("V2", "V3", "V1"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  exp <- experiment("my_exp", expr_mat, sample_info, var_info)

  expect_identical(colnames(exp$expr_mat), c("S1", "S2", "S3"))
  expect_identical(rownames(exp$expr_mat), c("V1", "V2", "V3"))
  expect_identical(exp$sample_info, sample_info)
  expect_identical(exp$var_info, var_info)
})


test_that("creating experiment with missing samples in `sample_info`", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info), error = TRUE)
})


test_that("creating experiment with missing variables in `var_info`", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2"))

  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info), error = TRUE)
})


test_that("creating experiment with extra samples in `sample_info`", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3", "S4"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info), error = TRUE)
})


test_that("creating experiment with extra variables in `var_info`", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3", "V4"))

  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info), error = TRUE)
})
