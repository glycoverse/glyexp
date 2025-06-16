test_that("slice_obs works", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4", "S5"), c("V1", "V2", "V3"))
  
  sliced_exp <- slice_obs(exp, 1, 3, 5)
  
  expect_equal(sliced_exp$sample_info$sample, c("S1", "S3", "S5"))
  expect_equal(colnames(sliced_exp$expr_mat), c("S1", "S3", "S5"))
  expect_equal(nrow(sliced_exp$sample_info), 3)
})


test_that("slice_var works", {
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2", "V3", "V4", "V5"))
  
  sliced_exp <- slice_var(exp, 2, 4)
  
  expect_equal(sliced_exp$var_info$variable, c("V2", "V4"))
  expect_equal(rownames(sliced_exp$expr_mat), c("V2", "V4"))
  expect_equal(nrow(sliced_exp$var_info), 2)
})


test_that("slice_head_obs works", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4", "S5"), c("V1", "V2"))
  
  sliced_exp <- slice_head_obs(exp, n = 3)
  
  expect_equal(sliced_exp$sample_info$sample, c("S1", "S2", "S3"))
  expect_equal(colnames(sliced_exp$expr_mat), c("S1", "S2", "S3"))
  expect_equal(nrow(sliced_exp$sample_info), 3)
})


test_that("slice_head_var works", {
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2", "V3", "V4"))
  
  sliced_exp <- slice_head_var(exp, n = 2)
  
  expect_equal(sliced_exp$var_info$variable, c("V1", "V2"))
  expect_equal(rownames(sliced_exp$expr_mat), c("V1", "V2"))
  expect_equal(nrow(sliced_exp$var_info), 2)
})


test_that("slice_tail_obs works", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4", "S5"), c("V1", "V2"))
  
  sliced_exp <- slice_tail_obs(exp, n = 2)
  
  expect_equal(sliced_exp$sample_info$sample, c("S4", "S5"))
  expect_equal(colnames(sliced_exp$expr_mat), c("S4", "S5"))
  expect_equal(nrow(sliced_exp$sample_info), 2)
})


test_that("slice_tail_var works", {
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2", "V3", "V4"))
  
  sliced_exp <- slice_tail_var(exp, n = 2)
  
  expect_equal(sliced_exp$var_info$variable, c("V3", "V4"))
  expect_equal(rownames(sliced_exp$expr_mat), c("V3", "V4"))
  expect_equal(nrow(sliced_exp$var_info), 2)
})


test_that("slice_sample_obs works", {
  set.seed(123)
  exp <- create_test_exp(c("S1", "S2", "S3", "S4", "S5"), c("V1", "V2"))
  
  sliced_exp <- slice_sample_obs(exp, n = 3)
  
  expect_equal(nrow(sliced_exp$sample_info), 3)
  expect_equal(ncol(sliced_exp$expr_mat), 3)
  expect_true(all(sliced_exp$sample_info$sample %in% c("S1", "S2", "S3", "S4", "S5")))
  expect_equal(colnames(sliced_exp$expr_mat), sliced_exp$sample_info$sample)
})


test_that("slice_sample_var works", {
  set.seed(123)
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2", "V3", "V4", "V5"))
  
  sliced_exp <- slice_sample_var(exp, n = 3)
  
  expect_equal(nrow(sliced_exp$var_info), 3)
  expect_equal(nrow(sliced_exp$expr_mat), 3)
  expect_true(all(sliced_exp$var_info$variable %in% c("V1", "V2", "V3", "V4", "V5")))
  expect_equal(rownames(sliced_exp$expr_mat), sliced_exp$var_info$variable)
})


test_that("slice_max_obs works", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4", "S5"), c("V1", "V2"))
  exp$sample_info$score <- c(10, 30, 20, 50, 40)
  
  sliced_exp <- slice_max_obs(exp, order_by = score, n = 2)
  
  expect_equal(sliced_exp$sample_info$sample, c("S4", "S5"))
  expect_equal(sliced_exp$sample_info$score, c(50, 40))
  expect_equal(colnames(sliced_exp$expr_mat), c("S4", "S5"))
})


test_that("slice_max_var works", {
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2", "V3", "V4", "V5"))
  exp$var_info$value <- c(5, 15, 10, 25, 20)
  
  sliced_exp <- slice_max_var(exp, order_by = value, n = 2)
  
  expect_equal(sliced_exp$var_info$variable, c("V4", "V5"))
  expect_equal(sliced_exp$var_info$value, c(25, 20))
  expect_equal(rownames(sliced_exp$expr_mat), c("V4", "V5"))
})


test_that("slice_min_obs works", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4", "S5"), c("V1", "V2"))
  exp$sample_info$score <- c(10, 30, 20, 50, 40)
  
  sliced_exp <- slice_min_obs(exp, order_by = score, n = 2)
  
  expect_equal(sliced_exp$sample_info$sample, c("S1", "S3"))
  expect_equal(sliced_exp$sample_info$score, c(10, 20))
  expect_equal(colnames(sliced_exp$expr_mat), c("S1", "S3"))
})


test_that("slice_min_var works", {
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2", "V3", "V4", "V5"))
  exp$var_info$value <- c(5, 15, 10, 25, 20)
  
  sliced_exp <- slice_min_var(exp, order_by = value, n = 2)
  
  expect_equal(sliced_exp$var_info$variable, c("V1", "V3"))
  expect_equal(sliced_exp$var_info$value, c(5, 10))
  expect_equal(rownames(sliced_exp$expr_mat), c("V1", "V3"))
})


test_that("slice functions preserve expression matrix values correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4"), c("V1", "V2", "V3"))
  
  # Original matrix values
  original_s2_col <- exp$expr_mat[, "S2"]
  original_s4_col <- exp$expr_mat[, "S4"]
  
  sliced_exp <- slice_obs(exp, 2, 4)
  
  # After slicing, values should be preserved
  expect_equal(sliced_exp$expr_mat[, "S2"], original_s2_col)
  expect_equal(sliced_exp$expr_mat[, "S4"], original_s4_col)
})


test_that("slice functions preserve other experiment components", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4"), c("V1", "V2"))
  exp$something <- "preserved"
  
  sliced_exp <- slice_obs(exp, 1, 3)
  
  expect_equal(sliced_exp$something, "preserved")
})


test_that("slice with proportion works", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4", "S5"), c("V1", "V2"))
  
  sliced_exp <- slice_head_obs(exp, prop = 0.6)
  
  expect_equal(nrow(sliced_exp$sample_info), 3)  # 60% of 5 = 3
  expect_equal(sliced_exp$sample_info$sample, c("S1", "S2", "S3"))
})


test_that("slice with weight_by works", {
  set.seed(123)
  exp <- create_test_exp(c("S1", "S2", "S3", "S4"), c("V1", "V2"))
  exp$sample_info$weight <- c(1, 2, 3, 4)
  
  sliced_exp <- slice_sample_obs(exp, n = 2, weight_by = weight)
  
  expect_equal(nrow(sliced_exp$sample_info), 2)
  expect_true(all(sliced_exp$sample_info$sample %in% c("S1", "S2", "S3", "S4")))
})


test_that("slice with ties works", {
  exp <- create_test_exp(c("S1", "S2", "S3", "S4"), c("V1", "V2"))
  exp$sample_info$score <- c(10, 20, 20, 30)
  
  # With ties = TRUE (default), should get both S2 and S3
  sliced_exp <- slice_max_obs(exp, order_by = score, n = 2, with_ties = TRUE)
  expect_equal(nrow(sliced_exp$sample_info), 3)  # S4, S2, S3
  
  # With ties = FALSE, should get exactly 2
  sliced_exp <- slice_max_obs(exp, order_by = score, n = 2, with_ties = FALSE)
  expect_equal(nrow(sliced_exp$sample_info), 2)
})


test_that("slice with non-existing columns raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  expect_snapshot(slice_max_obs(exp, order_by = bad_col, n = 1), error = TRUE)
  expect_snapshot(slice_min_var(exp, order_by = bad_col, n = 1), error = TRUE)
})


test_that("slice with out-of-bounds indices returns empty result", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2"))
  
  # Out-of-bounds indices should return empty results (consistent with dplyr behavior)
  sliced_exp <- slice_obs(exp, 10)
  expect_equal(nrow(sliced_exp$sample_info), 0)
  expect_equal(ncol(sliced_exp$expr_mat), 0)
  
  sliced_exp <- slice_var(exp, 5)
  expect_equal(nrow(sliced_exp$var_info), 0)
  expect_equal(nrow(sliced_exp$expr_mat), 0)
})


test_that("slice_sample with replace works", {
  set.seed(123)
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2"))
  
  # Sample with replacement should allow more samples than original
  sliced_exp <- slice_sample_obs(exp, n = 5, replace = TRUE)
  
  expect_equal(nrow(sliced_exp$sample_info), 5)
  expect_equal(ncol(sliced_exp$expr_mat), 5)
})


test_that("empty slice operations work", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # slice with n = 0 should return empty result
  sliced_exp <- slice_head_obs(exp, n = 0)
  expect_equal(nrow(sliced_exp$sample_info), 0)
  expect_equal(ncol(sliced_exp$expr_mat), 0)
  
  sliced_exp <- slice_tail_var(exp, n = 0)
  expect_equal(nrow(sliced_exp$var_info), 0)
  expect_equal(nrow(sliced_exp$expr_mat), 0)
}) 