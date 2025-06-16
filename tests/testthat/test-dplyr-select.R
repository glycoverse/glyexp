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
