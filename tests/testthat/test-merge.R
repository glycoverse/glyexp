expect_equal_exp <- function(result, expected) {
  expect_equal(result$expr_mat, expected$expr_mat)
  expect_equal(result$var_info, expected$var_info)
  expect_equal(result$sample_info, expected$sample_info)
  expect_equal(result$meta_data, expected$meta_data)
}

create_exp_pair <- function(
  var_info_1,
  var_info_2,
  sample_info_1 = NULL,
  sample_info_2 = NULL,
  expr_mat_1 = NULL,
  expr_mat_2 = NULL
) {
  default_sample_info_1 <- tibble::tibble(sample = c("S1", "S2", "S3"), group = factor(c("A", "A", "A")))
  default_sample_info_2 <- tibble::tibble(sample = c("S4", "S5", "S6"), group = factor(c("B", "B", "B")))

  if (is.null(sample_info_1)) sample_info_1 <- default_sample_info_1
  if (is.null(sample_info_2)) sample_info_2 <- default_sample_info_2

  if (is.null(expr_mat_1)) expr_mat_1 <- create_expr_mat(sample_info_1$sample, var_info_1$variable)
  if (is.null(expr_mat_2)) expr_mat_2 <- create_expr_mat(sample_info_2$sample, var_info_2$variable)

  exp1 <- experiment(expr_mat_1, sample_info_1, var_info_1, "others", "N", check_col_types = FALSE)
  exp2 <- experiment(expr_mat_2, sample_info_2, var_info_2, "others", "N", check_col_types = FALSE)

  list(exp1 = exp1, exp2 = exp2)
}

test_that("merge handles fully overlapping variables", {
  # Create test experiments
  var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  exps <- create_exp_pair(var_info, var_info)

  # Perform merge
  exp <- merge(exps$exp1, exps$exp2)

  # Create expected experiment
  expected_expr_mat <- matrix(
    c(
      1, 4, 7, 1, 4, 7,
      2, 5, 8, 2, 5, 8,
      3, 6, 9, 3, 6, 9
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("V1", "V2", "V3"), c("S1", "S2", "S3", "S4", "S5", "S6"))
  )
  expected_var_info <- var_info
  expected_sample_info <- tibble::tibble(
    sample = paste0("S", 1:6),
    group = factor(c(rep("A", 3), rep("B", 3)))
  )
  expected <- experiment(expected_expr_mat, expected_sample_info, expected_var_info, "others", "N", check_col_types = FALSE)

  # Compare results
  expect_equal_exp(exp, expected)
})

test_that("merge handles fully overlapping variables with different orders", {
  # Create test experiments
  var_info_1 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  var_info_2 <- tibble::tibble(
    variable = c("V3", "V2", "V1"),
    glycan = c("G3", "G2", "G1")
  )
  exps <- create_exp_pair(var_info_1, var_info_2)

  # Perform merge
  exp <- merge(exps$exp1, exps$exp2)

  # Create expected experiment
  expected_expr_mat <- matrix(
    c(
      1, 4, 7, 3, 6, 9,
      2, 5, 8, 2, 5, 8,
      3, 6, 9, 1, 4, 7
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("V1", "V2", "V3"), c("S1", "S2", "S3", "S4", "S5", "S6"))
  )
  expected_var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  expected_sample_info <- tibble::tibble(
    sample = paste0("S", 1:6),
    group = factor(c(rep("A", 3), rep("B", 3)))
  )
  expected <- experiment(expected_expr_mat, expected_sample_info, expected_var_info, "others", "N", check_col_types = FALSE)

  # Compare results
  expect_equal_exp(exp, expected)
})

test_that("merge handles partially overlapping variables", {
  # Create test experiments
  var_info_1 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  var_info_2 <- tibble::tibble(
    variable = c("V1", "V2", "V4"),
    glycan = c("G1", "G2", "G4")
  )
  exps <- create_exp_pair(var_info_1, var_info_2)

  # Perform merge
  exp <- merge(exps$exp1, exps$exp2)

  # Create expected experiment
  expected_var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3", "V4"),
    glycan = c("G1", "G2", "G3", "G4")
  )
  expected_sample_info <- tibble::tibble(
    sample = paste0("S", 1:6),
    group = factor(c(rep("A", 3), rep("B", 3)))
  )
  expected_expr_mat <- matrix(
    c(
      1, 4, 7, 1, 4, 7,    # G1
      2, 5, 8, 2, 5, 8,    # G2
      3, 6, 9, NA, NA, NA, # G3
      NA, NA, NA, 3, 6, 9  # G4
    ),
    nrow = 4, byrow = TRUE,
    dimnames = list(c("V1", "V2", "V3", "V4"), c("S1", "S2", "S3", "S4", "S5", "S6"))
  )
  expected <- experiment(expected_expr_mat, expected_sample_info, expected_var_info, "others", "N", check_col_types = FALSE)

  # Compare results
  expect_equal_exp(exp, expected)
})

test_that("merge handles no overlapping variables", {
  # Create test experiments
  var_info_1 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  var_info_2 <- tibble::tibble(
    variable = c("V4", "V5", "V6"),
    glycan = c("G4", "G5", "G6")
  )
  exps <- create_exp_pair(var_info_1, var_info_2)

  # Perform merge
  exp <- merge(exps$exp1, exps$exp2)

  # Create expected experiment
  expected_var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3", "V4", "V5", "V6"),
    glycan = c("G1", "G2", "G3", "G4", "G5", "G6")
  )
  expected_sample_info <- tibble::tibble(
    sample = paste0("S", 1:6),
    group = factor(c(rep("A", 3), rep("B", 3)))
  )
  expected_expr_mat <- matrix(
    c(
      1, 4, 7, NA, NA, NA, # G1
      2, 5, 8, NA, NA, NA, # G2
      3, 6, 9, NA, NA, NA, # G3
      NA, NA, NA, 1, 4, 7, # G4
      NA, NA, NA, 2, 5, 8, # G5
      NA, NA, NA, 3, 6, 9  # G6
    ),
    nrow = 6, byrow = TRUE,
    dimnames = list(c("V1", "V2", "V3", "V4", "V5", "V6"), c("S1", "S2", "S3", "S4", "S5", "S6"))
  )
  expected <- experiment(expected_expr_mat, expected_sample_info, expected_var_info, "others", "N", check_col_types = FALSE)

  # Compare results
  expect_equal_exp(exp, expected)
})

test_that("merge raises an error when the variable information is not unique", {
  var_info_1 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G2") # not unique
  )
  var_info_2 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  exps <- create_exp_pair(var_info_1, var_info_2)
  expect_error(merge(exps$exp1, exps$exp2), "Variable information is not unique")
})

test_that("merge raises an error for overlapping samples", {
  var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  sample_info_1 <- tibble::tibble(sample = c("S1", "S2", "S3"), group = factor("A"))
  sample_info_2 <- tibble::tibble(sample = c("S1", "S4", "S5"), group = factor("B"))
  exps <- create_exp_pair(var_info, var_info, sample_info_1, sample_info_2)
  expect_error(merge(exps$exp1, exps$exp2), 'Overlapping samples: "S1"')
})

test_that("merge raises an error for mismatched variable information columns", {
  var_info_1 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3"),
    extra_col = c("A", "B", "C")
  )
  var_info_2 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  sample_info_1 <- create_sample_info(c("S1", "S2", "S3"))
  sample_info_2 <- create_sample_info(c("S4", "S5", "S6"))
  exps <- create_exp_pair(var_info_1, var_info_2, sample_info_1, sample_info_2)
  expect_error(merge(exps$exp1, exps$exp2), "Column names in variable information do not match")
})

test_that("merge raises an error for mismatched sample information columns", {
  var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  sample_info_1 <- create_sample_info(c("S1", "S2", "S3"))
  sample_info_2 <- create_sample_info(c("S4", "S5", "S6"))
  sample_info_1$extra_col <- 1:3
  exps <- create_exp_pair(var_info, var_info, sample_info_1, sample_info_2)
  expect_error(merge(exps$exp1, exps$exp2), "Column names in sample information do not match")
})

test_that("merge raises an error for mismatched column types in variable information", {
  var_info_1 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3"),
    extra_col = c("A", "B", "C")
  )
  var_info_2 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3"),
    extra_col = c(1, 2, 3)
  )
  sample_info_1 <- create_sample_info(c("S1", "S2", "S3"))
  sample_info_2 <- create_sample_info(c("S4", "S5", "S6"))
  exps <- create_exp_pair(var_info_1, var_info_2, sample_info_1, sample_info_2)
  expect_error(merge(exps$exp1, exps$exp2), "Column types in variable information do not match")
})

test_that("merge raises an error for mismatched column types in sample information", {
  var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  sample_info_1 <- create_sample_info(c("S1", "S2", "S3"))
  sample_info_2 <- create_sample_info(c("S4", "S5", "S6"))
  sample_info_1$extra_col <- 1:3
  sample_info_2$extra_col <- as.character(1:3)
  exps <- create_exp_pair(var_info, var_info, sample_info_1, sample_info_2)
  expect_error(merge(exps$exp1, exps$exp2), "Column types in sample information do not match")
})

test_that("merge handles empty experiments", {
  # Create empty experiments
  var_info_1 <- tibble::tibble(variable = character(0), glycan = character(0))
  var_info_2 <- tibble::tibble(variable = character(0), glycan = character(0))
  sample_info_1 <- tibble::tibble(sample = character(0), group = factor(character(0)))
  sample_info_2 <- tibble::tibble(sample = character(0), group = factor(character(0)))
  expr_mat_1 <- matrix(numeric(0), nrow = 0, ncol = 0)
  expr_mat_2 <- matrix(numeric(0), nrow = 0, ncol = 0)
  dimnames(expr_mat_1) <- list(character(0), character(0))
  dimnames(expr_mat_2) <- list(character(0), character(0))
  
  exp1 <- experiment(expr_mat_1, sample_info_1, var_info_1, "others", "N", check_col_types = FALSE)
  exp2 <- experiment(expr_mat_2, sample_info_2, var_info_2, "others", "N", check_col_types = FALSE)
  
  result <- merge(exp1, exp2)
  
  expect_equal(nrow(result$expr_mat), 0)
  expect_equal(ncol(result$expr_mat), 0)
  expect_equal(nrow(result$var_info), 0)
  expect_equal(nrow(result$sample_info), 0)
})

test_that("merge handles one empty and one non-empty experiment", {
  # Create one empty and one non-empty experiment
  var_info_1 <- tibble::tibble(variable = character(0), glycan = character(0))
  var_info_2 <- tibble::tibble(variable = c("V1", "V2"), glycan = c("G1", "G2"))
  sample_info_1 <- tibble::tibble(sample = character(0), group = factor(character(0)))
  sample_info_2 <- tibble::tibble(sample = c("S1", "S2"), group = factor(c("A", "B")))
  expr_mat_1 <- matrix(numeric(0), nrow = 0, ncol = 0)
  dimnames(expr_mat_1) <- list(character(0), character(0))
  expr_mat_2 <- create_expr_mat(c("S1", "S2"), c("V1", "V2"))
  
  exp1 <- experiment(expr_mat_1, sample_info_1, var_info_1, "others", "N", check_col_types = FALSE)
  exp2 <- experiment(expr_mat_2, sample_info_2, var_info_2, "others", "N", check_col_types = FALSE)
  
  result <- merge(exp1, exp2)
  
  expect_equal(dim(result$expr_mat), c(2, 2))
  expect_equal(rownames(result$expr_mat), c("V1", "V2"))
  expect_equal(colnames(result$expr_mat), c("S1", "S2"))
  expect_equal(nrow(result$var_info), 2)
  expect_equal(nrow(result$sample_info), 2)
})

test_that("merge handles single variable experiments", {
  # Create experiments with single variables
  var_info_1 <- tibble::tibble(variable = "V1", glycan = "G1")
  var_info_2 <- tibble::tibble(variable = "V1", glycan = "G1")
  sample_info_1 <- tibble::tibble(sample = "S1", group = factor("A"))
  sample_info_2 <- tibble::tibble(sample = "S2", group = factor("B"))
  expr_mat_1 <- matrix(5, nrow = 1, ncol = 1, dimnames = list("V1", "S1"))
  expr_mat_2 <- matrix(10, nrow = 1, ncol = 1, dimnames = list("V1", "S2"))
  
  exp1 <- experiment(expr_mat_1, sample_info_1, var_info_1, "others", "N", check_col_types = FALSE)
  exp2 <- experiment(expr_mat_2, sample_info_2, var_info_2, "others", "N", check_col_types = FALSE)
  
  result <- merge(exp1, exp2)
  
  expect_equal(dim(result$expr_mat), c(1, 2))
  expect_equal(as.vector(result$expr_mat), c(5, 10))
  expect_equal(rownames(result$expr_mat), "V1")
  expect_equal(colnames(result$expr_mat), c("S1", "S2"))
})

test_that("merge handles complex variable information with multiple columns", {
  # Create experiments with complex variable info
  var_info_1 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3"),
    protein = c("P1", "P1", "P2"),
    site = c(1, 2, 1)
  )
  var_info_2 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G4", "G3"),
    protein = c("P1", "P2", "P2"),
    site = c(1, 1, 1)
  )
  exps <- create_exp_pair(var_info_1, var_info_2)

  result <- merge(exps$exp1, exps$exp2)

  # Should have 4 unique combinations: (G1,P1,1), (G2,P1,2), (G3,P2,1), (G4,P2,1)
  expect_equal(nrow(result$var_info), 4)
  expect_equal(result$var_info$glycan, c("G1", "G2", "G3", "G4"))
  expect_equal(result$var_info$protein, c("P1", "P1", "P2", "P2"))
  expect_equal(result$var_info$site, c(1, 2, 1, 1))
})

test_that("merge preserves metadata from first experiment", {
  # Create experiments with different metadata
  var_info <- tibble::tibble(variable = c("V1", "V2"), glycan = c("G1", "G2"))
  exps <- create_exp_pair(var_info, var_info)
  
  # Modify metadata of second experiment
  exps$exp2$meta_data$exp_type <- "glycoproteomics"
  exps$exp2$meta_data$glycan_type <- "O"
  exps$exp2$meta_data$extra_info <- "test"
  
  result <- merge(exps$exp1, exps$exp2)
  
  # Should preserve metadata from first experiment
  expect_equal(result$meta_data$exp_type, "others")
  expect_equal(result$meta_data$glycan_type, "N")
  expect_false("extra_info" %in% names(result$meta_data))
})

test_that("merge raises error for non-experiment objects", {
  var_info <- tibble::tibble(variable = c("V1", "V2"), glycan = c("G1", "G2"))
  exps <- create_exp_pair(var_info, var_info)
  
  expect_error(merge(exps$exp1, "not_an_experiment"), "Must inherit from class")
})

test_that("merge handles variable info with only 'variable' column", {
  # Test case where variable info has no identity columns
  var_info_1 <- tibble::tibble(variable = c("V1", "V2", "V3"))
  var_info_2 <- tibble::tibble(variable = c("V1", "V2", "V3"))
  exps <- create_exp_pair(var_info_1, var_info_2)
  
  result <- merge(exps$exp1, exps$exp2)
  
  # All variables should be treated as unique
  expect_equal(nrow(result$var_info), 6)
  expect_equal(result$var_info$variable, paste0("V", 1:6))
})

test_that("merge raises error when second experiment has non-unique variable info", {
  var_info_1 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G2", "G3")
  )
  var_info_2 <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan = c("G1", "G1", "G3") # non-unique
  )
  exps <- create_exp_pair(var_info_1, var_info_2)
  expect_error(merge(exps$exp1, exps$exp2), "Variable information is not unique")
})

test_that("merge handles multiple overlapping samples", {
  var_info <- tibble::tibble(variable = c("V1", "V2"), glycan = c("G1", "G2"))
  sample_info_1 <- tibble::tibble(sample = c("S1", "S2", "S3"), group = factor("A"))
  sample_info_2 <- tibble::tibble(sample = c("S2", "S3", "S4"), group = factor("B"))
  exps <- create_exp_pair(var_info, var_info, sample_info_1, sample_info_2)
  expect_error(merge(exps$exp1, exps$exp2), 'Overlapping samples: "S2" and "S3"')
})
