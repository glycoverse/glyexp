test_that("arranging samples works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$sample_info$group <- c("B", "A", "C")
  
  arranged_exp <- arrange_samples(exp, group)
  
  expect_equal(arranged_exp$sample_info$sample, c("S2", "S1", "S3"))
  expect_equal(arranged_exp$sample_info$group, c("A", "B", "C"))
  expect_equal(colnames(arranged_exp$expr_mat), c("S2", "S1", "S3"))
})


test_that("arranging variables works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$var_info$type <- c("Y", "X", "Z")
  
  arranged_exp <- arrange_variables(exp, type)
  
  expect_equal(arranged_exp$var_info$variable, c("V2", "V1", "V3"))
  expect_equal(arranged_exp$var_info$type, c("X", "Y", "Z"))
  expect_equal(rownames(arranged_exp$expr_mat), c("V2", "V1", "V3"))
})


test_that("arranging by multiple columns works", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4"), c("V1", "V2"))
  exp$sample_info$group <- c("A", "B", "A", "B")
  exp$sample_info$score <- c(2, 1, 1, 2)
  
  arranged_exp <- arrange_samples(exp, group, score)
  
  expected_order <- c("S3", "S1", "S2", "S4")
  expect_equal(arranged_exp$sample_info$sample, expected_order)
  expect_equal(colnames(arranged_exp$expr_mat), expected_order)
})


test_that("arranging with non-existing columns raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  expect_snapshot(arrange_samples(exp, bad_col), error = TRUE)
  expect_snapshot(arrange_variables(exp, bad_col), error = TRUE)
})


test_that("arranging preserves expression matrix values correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$sample_info$group <- c("C", "A", "B")
  
  # Original matrix values
  original_s1_col <- exp$expr_mat[, "S1"]
  original_s2_col <- exp$expr_mat[, "S2"]
  original_s3_col <- exp$expr_mat[, "S3"]
  
  arranged_exp <- arrange_samples(exp, group)
  
  # After arranging by group (A, B, C), order should be S2, S3, S1
  expect_equal(arranged_exp$expr_mat[, "S2"], original_s2_col)
  expect_equal(arranged_exp$expr_mat[, "S3"], original_s3_col)
  expect_equal(arranged_exp$expr_mat[, "S1"], original_s1_col)
})


test_that("arranging variables preserves expression matrix values correctly", {
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2", "V3"))
  exp$var_info$type <- c("Z", "X", "Y")
  
  # Original matrix values
  original_v1_row <- exp$expr_mat["V1", ]
  original_v2_row <- exp$expr_mat["V2", ]
  original_v3_row <- exp$expr_mat["V3", ]
  
  arranged_exp <- arrange_variables(exp, type)
  
  # After arranging by type (X, Y, Z), order should be V2, V3, V1
  expect_equal(arranged_exp$expr_mat["V2", ], original_v2_row)
  expect_equal(arranged_exp$expr_mat["V3", ], original_v3_row)
  expect_equal(arranged_exp$expr_mat["V1", ], original_v1_row)
})


test_that("empty arrange works (no change)", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Arrange without any arguments should return the same object
  arranged_exp <- arrange_samples(exp)
  expect_equal(arranged_exp$sample_info, exp$sample_info)
  expect_equal(arranged_exp$expr_mat, exp$expr_mat)
  
  arranged_exp <- arrange_variables(exp)
  expect_equal(arranged_exp$var_info, exp$var_info)
  expect_equal(arranged_exp$expr_mat, exp$expr_mat)
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"
  exp$sample_info$group <- c("B", "A", "C")
  
  exp2 <- arrange_samples(exp, group)
  
  expect_equal(exp2$something, "haha")
}) 