test_that("renaming info tibbles works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp2 <- rename_obs(exp, new_group = group)
  exp2 <- rename_var(exp2, new_type = type)

  expect_identical(colnames(exp2$sample_info), c("sample", "new_group"))
  expect_identical(colnames(exp2$var_info), c("variable", "new_type"))
})

test_that("rename verbs support SummarizedExperiment", {
  se <- create_test_se(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  SummarizedExperiment::colData(se)$sample <- c("A", "B", "C")
  SummarizedExperiment::rowData(se)$variable <- c("X", "Y", "Z")

  result <- se |>
    rename_obs(condition = group, sample_id = sample) |>
    rename_var(class = type, variable_id = variable)

  expect_identical(
    colnames(SummarizedExperiment::colData(result)),
    c("condition", "sample_id")
  )
  expect_identical(
    colnames(SummarizedExperiment::rowData(result)),
    c("class", "variable_id")
  )
  expect_identical(colnames(result), c("S1", "S2", "S3"))
  expect_identical(rownames(result), c("V1", "V2", "V3"))

  expect_error(rename_obs(se, id = .sample), "could not rename the.*\\.sample")
  expect_error(
    rename_var(se, id = .variable),
    "could not rename the.*\\.variable"
  )
})


test_that("trying to rename non-existing columns throws an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(rename_obs(exp, new_group = group2), error = TRUE)
  expect_snapshot(rename_var(exp, new_type = type2), error = TRUE)
})


test_that("trying to rename 'sample' or 'variable' columns throws an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(rename_obs(exp, new_sample = sample), error = TRUE)
  expect_snapshot(rename_var(exp, new_variable = variable), error = TRUE)
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"

  exp2 <- rename_var(exp)

  expect_equal(exp2$something, "haha")
})
