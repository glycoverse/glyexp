test_that("renaming info tibbles works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp2 <- rename_samples(exp, new_group = group)
  exp2 <- rename_variables(exp2, new_type = type)

  expect_identical(colnames(exp2$sample_info), c("sample", "new_group"))
  expect_identical(colnames(exp2$var_info), c("variable", "new_type"))
})


test_that("trying to rename non-existing columns throws an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(rename_samples(exp, new_group = group2), error = TRUE)
  expect_snapshot(rename_variables(exp, new_type = type2), error = TRUE)
})


test_that("trying to rename 'sample' or 'variable' columns throws an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(rename_samples(exp, new_sample = sample), error = TRUE)
  expect_snapshot(rename_variables(exp, new_variable = variable), error = TRUE)
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"

  exp2 <- rename_variables(exp)

  expect_equal(exp2$something, "haha")
})
