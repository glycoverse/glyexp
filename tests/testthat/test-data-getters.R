test_that("getter functions work", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  exp <- new_experiment(expr_mat, sample_info, var_info, list())

  expect_equal(get_expr_mat(exp), expr_mat)
  expect_equal(get_sample_info(exp), sample_info)
  expect_equal(get_var_info(exp), var_info)
})


test_that("motifying data returned by getters doesn't affect exp", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_var_info(c("V1", "V2", "V3"))

  exp <- new_experiment(expr_mat, sample_info, var_info, list())

  expr_mat2 <- get_expr_mat(exp)
  sample_info2 <- get_sample_info(exp)
  var_info2 <- get_var_info(exp)

  expr_mat2[1, 1] <- 100
  sample_info2[1, 1] <- "S100"
  var_info2[1, 1] <- "V100"

  expect_equal(get_expr_mat(exp), expr_mat)
  expect_equal(get_sample_info(exp), sample_info)
  expect_equal(get_var_info(exp), var_info)
})


test_that("getting meta data works", {
  exp <- toy_experiment()
  exp$meta_data <- list(structure_type = "pglyco", method = "LC-MS")
  
  # Test getting existing field
  expect_equal(get_meta_data(exp, "structure_type"), "pglyco")
  expect_equal(get_meta_data(exp, "method"), "LC-MS")
  
  # Test getting non-existing field returns NULL
  expect_null(get_meta_data(exp, "non_existing_field"))
})


test_that("get_exp_type works", {
  exp <- toy_experiment()
  
  # Test when exp_type is set
  exp$meta_data$exp_type <- "proteomics"
  expect_equal(get_exp_type(exp), "proteomics")
  
  # Test when exp_type is not set
  exp$meta_data$exp_type <- NULL
  expect_null(get_exp_type(exp))
  
  # Test with different exp_type values
  exp$meta_data$exp_type <- "glycomics"
  expect_equal(get_exp_type(exp), "glycomics")
})


test_that("get_glycan_type works", {
  exp <- toy_experiment()
  
  # Test when glycan_type is set
  exp$meta_data$glycan_type <- "N-linked"
  expect_equal(get_glycan_type(exp), "N-linked")
  
  # Test when glycan_type is not set
  exp$meta_data$glycan_type <- NULL
  expect_null(get_glycan_type(exp))
  
  # Test with different glycan_type values
  exp$meta_data$glycan_type <- "O-linked"
  expect_equal(get_glycan_type(exp), "O-linked")
})
