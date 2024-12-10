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


test_that("samples are not unique", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S2"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S2"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info), error = TRUE)
})


test_that("variables are not unique", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V2"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V2"))

  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info), error = TRUE)
})


test_that("both samples and variables are not unique", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S2"), c("V1", "V2", "V2"))
  sample_info <- create_sample_info(c("S1", "S2", "S2"))
  var_info <- create_var_info(c("V1", "V2", "V2"))

  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info), error = TRUE)
})


test_that("only one sample still works", {
  # `experiment` failed when there was only one sample
  # in commit b900650f9d because subseting `expr_mat` with a scalar
  # will automatically convert the matrix into a vector.
  expr_mat <- create_expr_mat(c("S1"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info), error = FALSE)
})


test_that("no variable works", {
  expr_mat <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, paste0("S", 1:3)))
  sample_info <- tibble::tibble(sample = paste0("S", 1:3))
  var_info <- tibble::tibble(variable = character(0))
  expect_snapshot(experiment("my_exp", expr_mat, sample_info, var_info))
})
