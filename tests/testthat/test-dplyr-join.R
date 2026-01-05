# Test joining data to sample or variable information

test_that("left_join_obs works correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create additional sample info
  extra_info <- tibble::tibble(
    sample = c("S1", "S2", "S3"), 
    age = c(25, 30, 35),
    treatment = c("A", "B", "A")
  )
  
  result <- left_join_obs(exp, extra_info, by = "sample")
  
  # Check that new columns were added
  expect_true("age" %in% colnames(result$sample_info))
  expect_true("treatment" %in% colnames(result$sample_info))
  expect_equal(result$sample_info$age, c(25, 30, 35))
  
  # Check that expr_mat is unchanged (all samples match)
  expect_equal(result$expr_mat, exp$expr_mat)
})


test_that("left_join_obs handles missing values correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create additional sample info with only partial matches
  extra_info <- tibble::tibble(
    sample = c("S1", "S3"), 
    age = c(25, 35)
  )
  
  result <- left_join_obs(exp, extra_info, by = "sample")
  
  # Check that S2 has NA for age
  expect_equal(result$sample_info$age, c(25, NA, 35))
  
  # Check that all samples are still present
  expect_equal(result$sample_info$sample, c("S1", "S2", "S3"))
  
  # Check that expr_mat is unchanged
  expect_equal(result$expr_mat, exp$expr_mat)
})


test_that("inner_join_obs removes non-matching samples", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create additional sample info with only partial matches
  extra_info <- tibble::tibble(
    sample = c("S1", "S3"), 
    age = c(25, 35)
  )
  
  result <- inner_join_obs(exp, extra_info, by = "sample")
  
  # Check that only S1 and S3 remain
  expect_equal(result$sample_info$sample, c("S1", "S3"))
  expect_equal(result$sample_info$age, c(25, 35))
  
  # Check that expr_mat was updated correctly
  expected_expr_mat <- matrix(c(1, 2, 3, 7, 8, 9), nrow = 3)
  rownames(expected_expr_mat) <- c("V1", "V2", "V3")
  colnames(expected_expr_mat) <- c("S1", "S3")
  expect_equal(result$expr_mat, expected_expr_mat)
})


test_that("semi_join_obs keeps only matching samples without adding columns", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create additional sample info with only partial matches
  extra_info <- tibble::tibble(
    sample = c("S1", "S3"), 
    age = c(25, 35)
  )
  
  result <- semi_join_obs(exp, extra_info, by = "sample")
  
  # Check that only S1 and S3 remain
  expect_equal(result$sample_info$sample, c("S1", "S3"))
  
  # Check that age column was NOT added
  expect_false("age" %in% colnames(result$sample_info))
  
  # Check that expr_mat was updated correctly
  expected_expr_mat <- matrix(c(1, 2, 3, 7, 8, 9), nrow = 3)
  rownames(expected_expr_mat) <- c("V1", "V2", "V3")
  colnames(expected_expr_mat) <- c("S1", "S3")
  expect_equal(result$expr_mat, expected_expr_mat)
})


test_that("anti_join_obs keeps only non-matching samples", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create additional sample info with only partial matches
  extra_info <- tibble::tibble(
    sample = c("S1", "S3"), 
    age = c(25, 35)
  )
  
  result <- anti_join_obs(exp, extra_info, by = "sample")
  
  # Check that only S2 remains
  expect_equal(result$sample_info$sample, "S2")
  
  # Check that expr_mat was updated correctly (only S2 column)
  expected_expr_mat <- matrix(c(4, 5, 6), nrow = 3)
  rownames(expected_expr_mat) <- c("V1", "V2", "V3")
  colnames(expected_expr_mat) <- "S2"
  expect_equal(result$expr_mat, expected_expr_mat)
})


test_that("left_join_var works correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create additional variable info
  extra_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"), 
    protein = c("P1", "P2", "P3"),
    pathway = c("A", "B", "A")
  )
  
  result <- left_join_var(exp, extra_info, by = "variable")
  
  # Check that new columns were added
  expect_true("protein" %in% colnames(result$var_info))
  expect_true("pathway" %in% colnames(result$var_info))
  expect_equal(result$var_info$protein, c("P1", "P2", "P3"))
  
  # Check that expr_mat is unchanged (all variables match)
  expect_equal(result$expr_mat, exp$expr_mat)
})


test_that("inner_join_var removes non-matching variables", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create additional variable info with only partial matches
  extra_info <- tibble::tibble(
    variable = c("V1", "V3"), 
    protein = c("P1", "P3")
  )
  
  result <- inner_join_var(exp, extra_info, by = "variable")
  
  # Check that only V1 and V3 remain
  expect_equal(result$var_info$variable, c("V1", "V3"))
  expect_equal(result$var_info$protein, c("P1", "P3"))
  
  # Check that expr_mat was updated correctly
  expected_expr_mat <- matrix(c(1, 3, 4, 6, 7, 9), nrow = 2)
  rownames(expected_expr_mat) <- c("V1", "V3")
  colnames(expected_expr_mat) <- c("S1", "S2", "S3")
  expect_equal(result$expr_mat, expected_expr_mat)
})


test_that("relationship parameter is locked to many-to-one", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  extra_info <- tibble::tibble(
    sample = c("S1", "S2", "S3"), 
    age = c(25, 30, 35)
  )
  
  # Test that explicitly setting relationship throws an error
  expect_snapshot(
    left_join_obs(exp, extra_info, by = "sample", relationship = "one-to-one"),
    error = TRUE
  )
  
  expect_snapshot(
    inner_join_var(exp, extra_info, by = "variable", relationship = "one-to-many"),
    error = TRUE
  )
})


test_that("join detects many-to-many relationships", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create data with duplicate keys that would violate many-to-one
  extra_info <- tibble::tibble(
    sample = c("S1", "S1", "S2", "S3"), 
    age = c(25, 26, 30, 35)
  )
  
  expect_snapshot(
    left_join_obs(exp, extra_info, by = "sample"),
    error = TRUE
  )
})


test_that("join with no matching observations returns empty experiment", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create data with no matching keys
  extra_info <- tibble::tibble(
    sample = c("S4", "S5", "S6"), 
    age = c(25, 30, 35)
  )
  
  result_inner <- inner_join_obs(exp, extra_info, by = "sample")
  expect_equal(nrow(result_inner$sample_info), 0)
  expect_equal(ncol(result_inner$expr_mat), 0)
  expect_equal(rownames(result_inner$expr_mat), result_inner$var_info$variable)

  result_semi <- semi_join_obs(exp, extra_info, by = "sample")
  expect_equal(nrow(result_semi$sample_info), 0)
  expect_equal(ncol(result_semi$expr_mat), 0)
  expect_equal(rownames(result_semi$expr_mat), result_semi$var_info$variable)
})

test_that("join with no matching variables returns empty experiment", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  extra_info <- tibble::tibble(
    variable = c("V4", "V5", "V6"),
    protein = c("P4", "P5", "P6")
  )

  result_inner <- inner_join_var(exp, extra_info, by = "variable")
  expect_equal(nrow(result_inner$var_info), 0)
  expect_equal(nrow(result_inner$expr_mat), 0)
  expect_equal(colnames(result_inner$expr_mat), result_inner$sample_info$sample)

  result_semi <- semi_join_var(exp, extra_info, by = "variable")
  expect_equal(nrow(result_semi$var_info), 0)
  expect_equal(nrow(result_semi$expr_mat), 0)
  expect_equal(colnames(result_semi$expr_mat), result_semi$sample_info$sample)
})


test_that("join with missing columns throws informative error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create data without the expected join column
  extra_info <- tibble::tibble(
    bad_column = c("S1", "S2", "S3"), 
    age = c(25, 30, 35)
  )
  
  expect_snapshot(
    left_join_obs(exp, extra_info, by = "sample"),
    error = TRUE
  )
})


test_that("join by different column names works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  # Create data with different column name for samples
  extra_info <- tibble::tibble(
    sample_id = c("S1", "S2", "S3"), 
    age = c(25, 30, 35)
  )
  
  result <- left_join_obs(exp, extra_info, by = c("sample" = "sample_id"))
  
  # Check that new column was added
  expect_true("age" %in% colnames(result$sample_info))
  expect_equal(result$sample_info$age, c(25, 30, 35))
})


test_that("other metadata is preserved during joins", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$meta_data$something <- "important"
  exp$custom_field <- "preserve_me"
  
  extra_info <- tibble::tibble(
    sample = c("S1", "S3"), 
    age = c(25, 35)
  )
  
  result <- inner_join_obs(exp, extra_info, by = "sample")
  
  # Check that metadata was preserved
  expect_equal(result$meta_data$something, "important")
  expect_equal(result$custom_field, "preserve_me")
})


test_that("join works with join_by syntax", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  
  extra_info <- tibble::tibble(
    sample_id = c("S1", "S2", "S3"), 
    age = c(25, 30, 35)
  )
  
  result <- left_join_obs(exp, extra_info, by = dplyr::join_by(sample == sample_id))
  
  # Check that new column was added
  expect_true("age" %in% colnames(result$sample_info))
  expect_equal(result$sample_info$age, c(25, 30, 35))
})
