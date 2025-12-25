test_that("summarize_experiment works for compositions", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_composition"], 2)
  
  # Missing column
  exp$var_info$glycan_composition <- NULL
  res <- summarize_experiment(exp)
  expect_false("total_composition" %in% res$item)
})

test_that("summarize_experiment works for structures", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_structure"], 2) # Struct1, Struct2
  
  # Missing column
  exp$var_info$glycan_structure <- NULL
  res <- summarize_experiment(exp)
  expect_false("total_structure" %in% res$item)
})

test_that("summarize_experiment works for peptides", {
  exp <- create_test_exp_2()
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP1")
  
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_peptide"], 2) # PEP1, PEP2
  
  # Missing column
  exp$var_info$peptide <- NULL
  res <- summarize_experiment(exp)
  expect_false("total_peptide" %in% res$item)
})

test_that("summarize_experiment works for glycopeptides with default parameters", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP3")
  exp$var_info$peptide_site <- c("N123", "N456", "N789")
  
  # Default should count by composition since no glycan_structure column
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_glycopeptide"], 3)
  
  # Add duplicate glycopeptide
  exp$var_info$peptide_site[3] <- "N456"
  exp$var_info$peptide[3] <- "PEP2"
  exp$var_info$glycan_composition[3] <- "H5N2"
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_glycopeptide"], 2)
})

test_that("summarize_experiment works for glycopeptides with count_struct", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP3")
  exp$var_info$peptide_site <- c("N123", "N456", "N789")
  exp$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  
  # Count by structure (default when structure exists)
  res_auto <- summarize_experiment(exp)
  expect_equal(res_auto$n[res_auto$item == "total_glycopeptide"], 3)
  
  # Explicitly count by structure
  res_true <- summarize_experiment(exp, count_struct = TRUE)
  expect_equal(res_true$n[res_true$item == "total_glycopeptide"], 3)
  
  # Explicitly count by composition
  res_false <- summarize_experiment(exp, count_struct = FALSE)
  # Even if structure exists, we ignore it and use composition
  # Here compositions are H5N2, H5N2, N3N2.
  # Rows are unique: (H5N2, PEP1, N123), (H5N2, PEP2, N456), (N3N2, PEP3, N789) -> 3 unique
  expect_equal(res_false$n[res_false$item == "total_glycopeptide"], 3)
  
  # Modify so structure matters
  # Row 1: Struct1, H5N2, PEP1, N123
  # Row 2: Struct2, H5N2, PEP1, N123  <- Same composition/peptide/site different structure
  exp$var_info <- exp$var_info[1:2,]
  exp$var_info$glycan_composition <- c("H5N2", "H5N2")
  exp$var_info$glycan_structure <- c("Struct1", "Struct2")
  exp$var_info$peptide <- c("PEP1", "PEP1")
  exp$var_info$peptide_site <- c("N123", "N123")
  
  res_struct <- summarize_experiment(exp, count_struct = TRUE)
  expect_equal(res_struct$n[res_struct$item == "total_glycopeptide"], 2) # Struct1 vs Struct2
  
  res_comp <- summarize_experiment(exp, count_struct = FALSE)
  expect_equal(res_comp$n[res_comp$item == "total_glycopeptide"], 1) # Same H5N2/PEP1/N123
})

test_that("summarize_experiment works for glycoforms", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO3")
  exp$var_info$protein_site <- c("N123", "N456", "N789")
  
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_glycoform"], 3)
})

test_that("summarize_experiment works for proteins", {
  exp <- create_test_exp_2()
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO1")
  
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_protein"], 2)
})

test_that("summarize_experiment works for glycosites", {
  exp <- create_test_exp_2()
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO1")
  exp$var_info$protein_site <- c("N123", "N456", "N123")
  
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_glycosite"], 2)
})


test_that("summarize_experiment handles edge cases", {
  # Single row
  exp <- create_test_exp_2()
  exp$expr_mat <- exp$expr_mat[1, , drop = FALSE]
  exp$var_info <- exp$var_info[1, ]
  exp$var_info$glycan_composition <- "H5N2"
  exp$var_info$peptide <- "PEP1"
  exp$var_info$protein <- "PRO1"
  
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_composition"], 1)
  expect_equal(res$n[res$item == "total_peptide"], 1)
  expect_equal(res$n[res$item == "total_protein"], 1)
  
  # Zero rows
  exp <- create_test_exp_2()
  exp$expr_mat <- exp$expr_mat[integer(0), , drop = FALSE]
  exp$var_info <- exp$var_info[integer(0), ]
  exp$var_info$glycan_composition <- character(0)
  
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_composition"], 0)
  
  # NA values
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", NA, "N3N2")
  res <- summarize_experiment(exp)
  expect_equal(res$n[res$item == "total_composition"], 3)
})

test_that("summarize_experiment validates input", {
  expect_error(summarize_experiment("not_an_experiment"))
  
  exp <- create_test_exp_2()
  expect_error(summarize_experiment(exp, count_struct = 123))
})

test_that("summarize_experiment calculates per_sample stats correctly", {
  exp <- create_test_exp_2()
  # 2 samples: S1, S2
  # 3 variables: V1, V2, V3
  exp$expr_mat <- matrix(c(10, NA, 20, 30, 40, NA), nrow = 3, ncol = 2)
  colnames(exp$expr_mat) <- c("S1", "S2")
  rownames(exp$expr_mat) <- c("V1", "V2", "V3")
  exp$sample_info <- tibble::tibble(sample = c("S1", "S2"))
  exp$var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    glycan_composition = c("A", "B", "A"),
    protein = c("P1", "P1", "P2"),
    protein_site = c(1, 1, 2)
  )
  
  res <- summarize_experiment(exp)
  
  # Check item names exist
  expect_true("composition_per_sample" %in% res$item)
  expect_true("protein_per_sample" %in% res$item)
  
  # Calculate expected values:
  # V1: A, P1, 1
  # V2: B, P1, 1
  # V3: A, P2, 2
  
  # Sample S1: V1 (10), V3 (20) detected (V2 is NA)
  #   - Compositions: A (V1), A (V3) -> 1 unique (A)
  #   - Proteins: P1 (V1), P2 (V3) -> 2 unique (P1, P2)
  
  # Sample S2: V1 (30), V2 (40) detected (V3 is NA)
  #   - Compositions: A (V1), B (V2) -> 2 unique (A, B)
  #   - Proteins: P1 (V1), P1 (V2) -> 1 unique (P1)
  
  # Average Composition: (1 + 2) / 2 = 1.5
  expect_equal(res$n[res$item == "composition_per_sample"], 1.5)
  
  # Average Protein: (2 + 1) / 2 = 1.5
  expect_equal(res$n[res$item == "protein_per_sample"], 1.5)
})
