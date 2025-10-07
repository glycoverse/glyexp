test_that("as_tibble works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  tb <- tibble::as_tibble(exp)

  expected_tb <- tibble::tibble(
    sample = rep(c("S1", "S2", "S3"), each = 3),
    group = factor(rep("A", 9)),
    variable = rep(c("V1", "V2", "V3"), 3),
    type = rep("B", 9),
    value = c(1:9)
  )
  expect_equal(dplyr::arrange(tb, value), expected_tb)
})


test_that("`sample_cols` and `variable_cols` works", {
  exp <- create_test_exp_2()

  tb <- tibble::as_tibble(exp, sample_cols = col1, var_cols = col2)

  expect_identical(colnames(tb), c("sample", "col1", "variable", "col2", "value"))
})
