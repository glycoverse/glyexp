test_that("mutating sample info works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_col(exp, new_col = 1:3)

  expect_equal(new_exp$sample_info$new_col, 1:3)
})

test_that("mutate verbs support SummarizedExperiment", {
  se <- create_test_se(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  SummarizedExperiment::colData(se)$sample <- c("A", "B", "C")
  SummarizedExperiment::rowData(se)$variable <- c("X", "Y", "Z")

  result <- se |>
    mutate_col(
      .sample = paste0(.sample, "_new"),
      sample = paste0(sample, "_metadata"),
      batch = 1:3
    ) |>
    mutate_row(
      .variable = paste0(.variable, "_new"),
      variable = paste0(variable, "_metadata"),
      score = 3:1
    )

  expect_identical(colnames(result), paste0(c("S1", "S2", "S3"), "_new"))
  expect_identical(rownames(result), paste0(c("V1", "V2", "V3"), "_new"))
  expect_identical(SummarizedExperiment::colData(result)$batch, 1:3)
  expect_identical(SummarizedExperiment::rowData(result)$score, 3:1)
  expect_identical(
    SummarizedExperiment::colData(result)$sample,
    paste0(c("A", "B", "C"), "_metadata")
  )
  expect_identical(
    SummarizedExperiment::rowData(result)$variable,
    paste0(c("X", "Y", "Z"), "_metadata")
  )
  expect_identical(S4Vectors::metadata(result)$marker, "preserved")
})

test_that("SummarizedExperiment virtual identifier names are reserved", {
  obs_se <- create_test_se(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  SummarizedExperiment::colData(obs_se)$.sample <- c("A", "B", "C")
  expect_error(mutate_col(obs_se, value = 1), "reserved for dimension names")

  var_se <- create_test_se(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  SummarizedExperiment::rowData(var_se)$.variable <- c("X", "Y", "Z")
  expect_error(mutate_row(var_se, value = 1), "reserved for dimension names")
})


test_that("mutating variable info works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_row(exp, new_col = 1:3)

  expect_equal(new_exp$var_info$new_col, 1:3)
})


test_that("mutating using non-existing columns raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(mutate_col(exp, new_col = bad_col), error = TRUE)
  expect_snapshot(mutate_row(exp, new_col = bad_col), error = TRUE)
})


test_that("trying to mutate 'sample' with duplicated values raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(mutate_col(exp, sample = 1), error = TRUE)
})


test_that("trying to mutate 'variable' with duplicated values raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(mutate_row(exp, variable = 1), error = TRUE)
})


test_that("mutating 'sample' updates 'expr_mat' correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_col(exp, sample = c("A", "B", "C"))

  expect_equal(colnames(new_exp$expr_mat), c("A", "B", "C"))
})


test_that("mutating 'variable' updates 'expr_mat' correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_row(exp, variable = c("A", "B", "C"))

  expect_equal(rownames(new_exp$expr_mat), c("A", "B", "C"))
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"

  exp2 <- mutate_row(exp)

  expect_equal(exp2$something, "haha")
})
