test_that("selecting sample info works", {
  exp <- create_test_exp_2()

  exp2 <- select_obs(exp, col1)

  expect_identical(colnames(exp2$sample_info), c("sample", "col1"))
})


test_that("selecting variable info works", {
  exp <- create_test_exp_2()

  exp2 <- select_var(exp, col1)

  expect_identical(colnames(exp2$var_info), c("variable", "col1"))
})

test_that("select verbs support SummarizedExperiment", {
  se <- create_test_se(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  SummarizedExperiment::colData(se)$sample <- c("A", "B", "C")
  SummarizedExperiment::rowData(se)$variable <- c("X", "Y", "Z")

  result <- se |>
    select_obs(group, sample) |>
    select_var(type, variable)

  expect_identical(
    colnames(SummarizedExperiment::colData(result)),
    c("group", "sample")
  )
  expect_identical(
    colnames(SummarizedExperiment::rowData(result)),
    c("type", "variable")
  )
  expect_identical(colnames(result), c("S1", "S2", "S3"))
  expect_identical(rownames(result), c("V1", "V2", "V3"))

  expect_error(select_obs(se, .sample), "should not explicitly select")
  expect_error(select_var(se, .variable), "should not explicitly select")
})


test_that("selecting 'sample' column raises an error", {
  exp <- create_test_exp_2()

  expect_snapshot(select_obs(exp, sample, col1), error = TRUE)
})


test_that("selecting 'sample' column with dynamic selection silently keeps 'sample'", {
  exp <- create_test_exp_2()

  exp2 <- select_obs(exp, -tidyselect::starts_with("sample"))

  expect_identical(colnames(exp2$sample_info), c("sample", "col1", "col2"))
})


test_that("selecting 'variable' column raises an error", {
  exp <- create_test_exp_2()

  expect_snapshot(select_var(exp, variable, col1), error = TRUE)
})


test_that("selecting non-existing columns raises an error", {
  exp <- create_test_exp_2()

  expect_snapshot(select_obs(exp, bad_col), error = TRUE)
  expect_snapshot(select_var(exp, bad_col), error = TRUE)
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"

  exp2 <- select_var(exp)

  expect_equal(exp2$something, "haha")
})
