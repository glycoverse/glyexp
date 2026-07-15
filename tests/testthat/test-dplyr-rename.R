test_that("renaming info tibbles works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp2 <- rename_col(exp, new_group = group)
  exp2 <- rename_row(exp2, new_type = type)

  expect_identical(colnames(exp2$sample_info), c("sample", "new_group"))
  expect_identical(colnames(exp2$var_info), c("variable", "new_type"))
})

test_that("rename verbs support SummarizedExperiment", {
  se <- create_test_se(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  SummarizedExperiment::colData(se)$sample <- c("A", "B", "C")
  SummarizedExperiment::rowData(se)$variable <- c("X", "Y", "Z")

  result <- se |>
    rename_col(condition = group, sample_id = sample) |>
    rename_row(class = type, variable_id = variable)

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

  expect_error(rename_col(se, id = .sample), "could not rename the.*\\.sample")
  expect_error(
    rename_row(se, id = .variable),
    "could not rename the.*\\.variable"
  )
  expect_error(
    rename_col(se, .sample = sample),
    "reserved for dimension names"
  )
  expect_error(
    rename_row(se, .variable = variable),
    "reserved for dimension names"
  )
})

test_that("rename verbs require names for virtual identifiers", {
  se <- create_unnamed_test_se()

  result <- se |>
    rename_col(condition = group) |>
    rename_row(class = type)
  expect_null(colnames(result))
  expect_null(rownames(result))

  expect_snapshot(rename_col(se, id = .sample), error = TRUE)
  expect_snapshot(rename_row(se, id = .variable), error = TRUE)
  expect_snapshot(rename_col(se, .sample = group), error = TRUE)
  expect_snapshot(rename_row(se, .variable = type), error = TRUE)
})


test_that("trying to rename non-existing columns throws an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(rename_col(exp, new_group = group2), error = TRUE)
  expect_snapshot(rename_row(exp, new_type = type2), error = TRUE)
})


test_that("trying to rename 'sample' or 'variable' columns throws an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(rename_col(exp, new_sample = sample), error = TRUE)
  expect_snapshot(rename_row(exp, new_variable = variable), error = TRUE)
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"

  exp2 <- rename_row(exp)

  expect_equal(exp2$something, "haha")
})
