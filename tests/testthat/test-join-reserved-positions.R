test_that("SE joins reject collisions with their internal position key", {
  mat <- matrix(1:6, nrow = 2, dimnames = list(c("V1", "V2"), c("S1", "S2", "S3")))
  exp <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = mat),
    rowData = S4Vectors::DataFrame(key = c("a", "b")),
    colData = S4Vectors::DataFrame(key = c("a", "b", "c"))
  )
  y <- tibble::tibble(key = c("a", "b", "c"), annotation = 1:3)
  y[[tidy_position_column()]] <- c(3L, 2L, 1L)

  joins <- list(
    left_join_row, inner_join_row, semi_join_row, anti_join_row,
    left_join_col, inner_join_col, semi_join_col, anti_join_col
  )
  for (join in joins) {
    expect_error(join(exp, y, by = "key"), "reserved")
    expect_error(join(exp, y), "reserved")
    expect_identical(SummarizedExperiment::assay(exp), mat)
  }
})

test_that("ordinary SE join annotations retain assay positions", {
  mat <- matrix(1:4, nrow = 2, dimnames = list(c("same", "same"), c("S1", "S2")))
  exp <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = mat),
    rowData = S4Vectors::DataFrame(key = c("a", "b"))
  )
  result <- left_join_row(exp, tibble::tibble(key = c("b", "a"), annotation = c(20, 10)), by = "key")
  expect_identical(SummarizedExperiment::assay(result), mat)
  expect_identical(SummarizedExperiment::rowData(result)$annotation, c(10, 20))
})
