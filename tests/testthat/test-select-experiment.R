create_mutate_test_exp <- function() {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- tibble::tibble(
    sample = c("S1", "S2", "S3"),
    col1 = c(1, 2, 3),
    col2 = c("A", "B", "C")
  )
  var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    col1 = c(1, 2, 3),
    col2 = c("A", "B", "C")
  )
  new_experiment("my_exp", expr_mat, sample_info, var_info)
}


test_that("selecting sample info works", {
  exp <- create_mutate_test_exp()

  exp2 <- select_samples(exp, col1)

  expect_identical(colnames(exp2$sample_info), c("sample", "col1"))
})


test_that("selecting variable info works", {
  exp <- create_mutate_test_exp()

  exp2 <- select_variables(exp, col1)

  expect_identical(colnames(exp2$var_info), c("variable", "col1"))
})


test_that("selecting 'sample' column raises an error", {
  exp <- create_mutate_test_exp()

  expect_snapshot(select_samples(exp, sample, col1), error = TRUE)
})


test_that("selecting 'variable' column raises an error", {
  exp <- create_mutate_test_exp()

  expect_snapshot(select_variables(exp, variable, col1), error = TRUE)
})


test_that("selecting non-existing columns raises an error", {
  exp <- create_mutate_test_exp()

  expect_snapshot(select_samples(exp, bad_col), error = TRUE)
  expect_snapshot(select_variables(exp, bad_col), error = TRUE)
})
