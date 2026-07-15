test_that("selecting sample info works", {
  exp <- create_test_exp_2()

  exp2 <- select_col(exp, col1)

  expect_identical(colnames(exp2$sample_info), c("sample", "col1"))
})


test_that("selecting variable info works", {
  exp <- create_test_exp_2()

  exp2 <- select_row(exp, col1)

  expect_identical(colnames(exp2$var_info), c("variable", "col1"))
})

test_that("select verbs support SummarizedExperiment", {
  se <- create_test_se(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  SummarizedExperiment::colData(se)$sample <- c("A", "B", "C")
  SummarizedExperiment::rowData(se)$variable <- c("X", "Y", "Z")

  result <- se |>
    select_col(group, sample) |>
    select_row(type, variable)

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

  expect_error(select_col(se, .sample), "should not explicitly select")
  expect_error(select_row(se, .variable), "should not explicitly select")
  expect_error(
    select_col(se, .sample = sample),
    "reserved for dimension names"
  )
  expect_error(
    select_row(se, .variable = variable),
    "reserved for dimension names"
  )
})

test_that("select verbs require names for virtual identifiers", {
  se <- create_unnamed_test_se()

  result <- se |>
    select_col(group) |>
    select_row(type)
  expect_null(colnames(result))
  expect_null(rownames(result))

  expect_snapshot(select_col(se, .sample), error = TRUE)
  expect_snapshot(select_row(se, .variable), error = TRUE)
  expect_snapshot(select_col(se, dplyr::all_of(".sample")), error = TRUE)
  expect_snapshot(select_col(se, .sample = group), error = TRUE)
  expect_snapshot(select_row(se, .variable = type), error = TRUE)
})


test_that("selecting 'sample' column raises an error", {
  exp <- create_test_exp_2()

  expect_snapshot(select_col(exp, sample, col1), error = TRUE)
})


test_that("selecting 'sample' column with dynamic selection silently keeps 'sample'", {
  exp <- create_test_exp_2()

  exp2 <- select_col(exp, -tidyselect::starts_with("sample"))

  expect_identical(colnames(exp2$sample_info), c("sample", "col1", "col2"))
})


test_that("selecting 'variable' column raises an error", {
  exp <- create_test_exp_2()

  expect_snapshot(select_row(exp, variable, col1), error = TRUE)
})


test_that("selecting non-existing columns raises an error", {
  exp <- create_test_exp_2()

  expect_snapshot(select_col(exp, bad_col), error = TRUE)
  expect_snapshot(select_row(exp, bad_col), error = TRUE)
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"

  exp2 <- select_row(exp)

  expect_equal(exp2$something, "haha")
})
