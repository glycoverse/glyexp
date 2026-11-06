test_that("unique suffixes avoid pre-existing and generated identifiers", {
  expect_identical(.ensure_unique(c("a", "a", "a-1"), "-{N}"), c("a-2", "a-3", "a-1"))
  expect_identical(
    .ensure_unique(c("a-1", "a", "a", "a-2"), "-{N}"),
    c("a-1", "a-3", "a-4", "a-2")
  )
  expect_identical(
    .ensure_unique(c("a", "a", "a_v1"), "_v{N}"),
    c("a_v2", "a_v3", "a_v1")
  )
  values <- c("a", "a", "a-1", "a-1", "a-1-1")
  result <- .ensure_unique(values, "-{N}")
  expect_identical(anyDuplicated(result), 0L)
  expect_identical(result[length(result)], "a-1-1")
  expect_identical(.ensure_unique(c("a", "b"), "-{N}"), c("a", "b"))
})

test_that("standardize_variable keeps identifiers unique in SE and legacy containers", {
  ids <- paste0("V", 1:3)
  samples <- c("S1", "S2")
  mat <- matrix(seq_len(6), nrow = 3, dimnames = list(ids, samples))
  se <- GlycomicSE(
    mat,
    rowData = S4Vectors::DataFrame(
      glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2)), 3),
      label = c("a", "a", "a-1"),
      row.names = ids
    ),
    colData = S4Vectors::DataFrame(group = c("A", "B"), row.names = samples),
    metadata = list(glycan_type = "N")
  )
  legacy <- suppressWarnings(from_se(se, exp_type = "glycomics"))
  result_se <- standardize_variable(se, format = "{label}")
  result_legacy <- standardize_variable(legacy, format = "{label}")
  expected <- c("a-2", "a-3", "a-1")

  expect_identical(rownames(result_se), expected)
  expect_identical(rownames(result_legacy$expr_mat), expected)
  expect_identical(as.character(result_legacy$var_info$variable), expected)
  expect_equal(unname(SummarizedExperiment::assay(result_se)), unname(mat))
  expect_equal(unname(result_legacy$expr_mat), unname(mat))
  expect_identical(rownames(se), ids)
  expect_identical(rownames(legacy$expr_mat), ids)
})
