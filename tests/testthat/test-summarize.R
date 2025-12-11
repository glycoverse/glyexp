test_that("count_compositions works", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  
  result <- count_compositions(exp)
  expect_equal(result, 2)  # H5N2 and N3N2
  
  # Test with non-experiment object
  expect_error(count_compositions("not_an_experiment"))
})

test_that("count_compositions fails when glycan_composition column is missing", {
  exp <- create_test_exp_2()
  # glycan_composition column doesn't exist by default
  
  expect_error(count_compositions(exp), "glycan_composition")
})

test_that("count_structures works", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  
  result <- count_structures(exp)
  expect_equal(result, 2)  # Struct1, Struct2
})

test_that("count_structures fails when glycan_structure column is missing", {
  exp <- create_test_exp_2()
  # glycan_structure column doesn't exist by default
  
  expect_error(count_structures(exp), "glycan_structure")
})

test_that("count_peptides works", {
  exp <- create_test_exp_2()
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP1")
  
  result <- count_peptides(exp)
  expect_equal(result, 2)  # PEP1, PEP2
})

test_that("count_peptides fails when peptide column is missing", {
  exp <- create_test_exp_2()
  # peptide column doesn't exist by default
  
  expect_error(count_peptides(exp), "peptide")
})

test_that("count_glycopeptides works with default parameters", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP3")
  exp$var_info$peptide_site <- c("N123", "N456", "N789")
  
  # Default should count by composition since no glycan_structure column
  result <- count_glycopeptides(exp)
  expect_equal(result, 3)  # Each row is unique combination
  
  # Add duplicate glycopeptide (same glycan_composition + peptide + peptide_site)
  exp$var_info$peptide_site[3] <- "N456"     # Same as row 2
  exp$var_info$peptide[3] <- "PEP2"          # Same as row 2  
  exp$var_info$glycan_composition[3] <- "H5N2"  # Same as row 2
  result <- count_glycopeptides(exp)
  expect_equal(result, 2)
})

test_that("count_glycopeptides works with count_struct = TRUE", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP3")
  exp$var_info$peptide_site <- c("N123", "N456", "N789")
  exp$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  
  # Count by structure
  result <- count_glycopeptides(exp, count_struct = TRUE)
  expect_equal(result, 3)
})

test_that("count_glycopeptides works with count_struct = FALSE", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP3")
  exp$var_info$peptide_site <- c("N123", "N456", "N789")
  exp$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  
  # Count by composition (still counts unique glycan_composition + peptide + peptide_site combinations)
  result <- count_glycopeptides(exp, count_struct = FALSE)
  expect_equal(result, 3)  # Each row is unique
})

test_that("count_glycopeptides fails when required columns are missing", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP3")
  
  # Missing peptide_site
  expect_error(count_glycopeptides(exp), "peptide_site")
  
  # Missing peptide
  exp$var_info$peptide_site <- c("N123", "N456", "N789")
  exp$var_info$peptide <- NULL
  expect_error(count_glycopeptides(exp), "peptide")
})

test_that("count_glycoforms works with protein column", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO3")
  exp$var_info$protein_site <- c("N123", "N456", "N789")
  
  result <- count_glycoforms(exp)
  expect_equal(result, 3)  # Based on glycan_composition + protein + protein_site
})

test_that("count_glycoforms works with proteins column", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$proteins <- c("PRO1", "PRO2", "PRO3")  # Use plural form
  exp$var_info$protein_sites <- c("N123", "N456", "N789")  # Use plural form
  
  result <- count_glycoforms(exp)
  expect_equal(result, 3)
})

test_that("count_glycoforms works with count_struct parameter", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO3")
  exp$var_info$protein_site <- c("N123", "N456", "N789")
  exp$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  
  # Count by structure
  result1 <- count_glycoforms(exp, count_struct = TRUE)
  expect_equal(result1, 3)
  
  # Count by composition (still counts unique glycan_composition + protein + protein_site combinations)
  result2 <- count_glycoforms(exp, count_struct = FALSE)
  expect_equal(result2, 3)  # Each row is unique
})

test_that("count_glycoforms fails when required columns are missing", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO3")
  
  # Missing protein_site/protein_sites
  expect_error(count_glycoforms(exp), "protein_site")
  
  # Missing protein/proteins
  exp$var_info$protein_site <- c("N123", "N456", "N789")
  exp$var_info$protein <- NULL
  expect_error(count_glycoforms(exp), "protein")
})

test_that("count_proteins works with protein column", {
  exp <- create_test_exp_2()
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO1")
  
  result <- count_proteins(exp)
  expect_equal(result, 2)  # PRO1, PRO2
})

test_that("count_proteins works with proteins column", {
  exp <- create_test_exp_2()
  exp$var_info$proteins <- c("PRO1", "PRO2", "PRO1")
  
  result <- count_proteins(exp)
  expect_equal(result, 2)  # PRO1, PRO2
})

test_that("count_proteins fails when protein columns are missing", {
  exp <- create_test_exp_2()
  # No protein columns by default
  
  expect_error(count_proteins(exp), "protein")
})

test_that("count_glycosites works with single columns", {
  exp <- create_test_exp_2()
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO1")
  exp$var_info$protein_site <- c("N123", "N456", "N123")
  
  result <- count_glycosites(exp)
  expect_equal(result, 2)  # (PRO1,N123) and (PRO2,N456)
})

test_that("count_glycosites works with plural columns", {
  exp <- create_test_exp_2()
  exp$var_info$proteins <- c("PRO1", "PRO2", "PRO1")
  exp$var_info$protein_sites <- c("N123", "N456", "N123")
  
  result <- count_glycosites(exp)
  expect_equal(result, 2)  # (PRO1,N123) and (PRO2,N456)
})

test_that("count_glycosites fails when required columns are missing", {
  exp <- create_test_exp_2()
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO1")
  
  # Missing protein_site/protein_sites
  expect_error(count_glycosites(exp), "protein_site")
  
  # Missing protein/proteins
  exp$var_info$protein_site <- c("N123", "N456", "N789")
  exp$var_info$protein <- NULL
  expect_error(count_glycosites(exp), "protein")
})

test_that("count functions work with edge cases", {
  # Test with single row
  exp <- create_test_exp_2()
  exp$expr_mat <- exp$expr_mat[1, , drop = FALSE]
  exp$var_info <- exp$var_info[1, ]
  exp$var_info$glycan_composition <- "H5N2"
  exp$var_info$peptide <- "PEP1"
  exp$var_info$protein <- "PRO1"
  
  expect_equal(count_compositions(exp), 1)
  expect_equal(count_peptides(exp), 1)
  expect_equal(count_proteins(exp), 1)
  
  # Test with all identical values
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- rep("H5N2", 3)
  exp$var_info$peptide <- rep("PEP1", 3)
  exp$var_info$protein <- rep("PRO1", 3)
  
  expect_equal(count_compositions(exp), 1)
  expect_equal(count_peptides(exp), 1)
  expect_equal(count_proteins(exp), 1)
})

test_that("all count functions validate input class", {
  not_exp <- list(var_info = data.frame(glycan_composition = "H5N2"))
  
  expect_error(count_compositions(not_exp))
  expect_error(count_structures(not_exp))
  expect_error(count_peptides(not_exp))
  expect_error(count_glycopeptides(not_exp))
  expect_error(count_glycoforms(not_exp))
  expect_error(count_proteins(not_exp))
  expect_error(count_glycosites(not_exp))
})

test_that("count functions handle empty data", {
  # Test with zero rows
  exp <- create_test_exp_2()
  exp$expr_mat <- exp$expr_mat[integer(0), , drop = FALSE]
  exp$var_info <- exp$var_info[integer(0), ]
  exp$var_info$glycan_composition <- character(0)
  exp$var_info$peptide <- character(0)
  exp$var_info$protein <- character(0)
  
  expect_equal(count_compositions(exp), 0)
  expect_equal(count_peptides(exp), 0)
  expect_equal(count_proteins(exp), 0)
})

test_that("count functions handle NA values", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", NA, "N3N2")
  exp$var_info$peptide <- c("PEP1", NA, "PEP1")
  exp$var_info$protein <- c("PRO1", "PRO2", NA)
  
  # NA is treated as a distinct value by dplyr::n_distinct
  expect_equal(count_compositions(exp), 3)  # H5N2, NA, N3N2
  expect_equal(count_peptides(exp), 2)      # PEP1, NA  
  expect_equal(count_proteins(exp), 3)      # PRO1, PRO2, NA
})

test_that("count_glycopeptides auto-inference works correctly", {
  # Test with glycan_structure present - should default to count_struct=TRUE
  exp1 <- create_test_exp_2()
  exp1$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp1$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  exp1$var_info$peptide <- c("PEP1", "PEP1", "PEP1")
  exp1$var_info$peptide_site <- c("N123", "N123", "N123")
  
  result1_auto <- count_glycopeptides(exp1)  # Should use structure
  result1_true <- count_glycopeptides(exp1, count_struct = TRUE)
  expect_equal(result1_auto, result1_true)
  expect_equal(result1_auto, 2)  # Struct1, Struct2
  
  # Test without glycan_structure - should default to count_struct=FALSE
  exp2 <- create_test_exp_2()
  exp2$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp2$var_info$peptide <- c("PEP1", "PEP1", "PEP1")
  exp2$var_info$peptide_site <- c("N123", "N123", "N123")
  
  result2_auto <- count_glycopeptides(exp2)  # Should use composition
  result2_false <- count_glycopeptides(exp2, count_struct = FALSE)
  expect_equal(result2_auto, result2_false)
  expect_equal(result2_auto, 2)  # H5N2, N3N2
})

test_that("count_glycoforms auto-inference works correctly", {
  # Test with glycan_structure present
  exp1 <- create_test_exp_2()
  exp1$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp1$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  exp1$var_info$protein <- c("PRO1", "PRO1", "PRO1")
  exp1$var_info$protein_site <- c("N123", "N123", "N123")
  
  result1_auto <- count_glycoforms(exp1)
  result1_true <- count_glycoforms(exp1, count_struct = TRUE)
  expect_equal(result1_auto, result1_true)
  
  # Test without glycan_structure
  exp2 <- create_test_exp_2()
  exp2$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp2$var_info$protein <- c("PRO1", "PRO1", "PRO1")
  exp2$var_info$protein_site <- c("N123", "N123", "N123")
  
  result2_auto <- count_glycoforms(exp2)
  result2_false <- count_glycoforms(exp2, count_struct = FALSE)
  expect_equal(result2_auto, result2_false)
})

test_that("summarize_experiment returns tibble of counts", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP1")
  exp$var_info$peptide_site <- c("N123", "N456", "N123")
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO1")
  exp$var_info$protein_site <- c("N123", "N456", "N123")

  result <- summarize_experiment(exp)

  expect_s3_class(result, "tbl_df")
  expect_equal(
    result$item,
    c("composition", "structure", "peptide", "glycopeptide", "glycoform", "protein", "glycosite")
  )
  expect_equal(result$n, rep(2, length(result$item)))
})

test_that("summarize_experiment respects count_struct flag", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "H5N2")
  exp$var_info$glycan_structure <- c("Struct1", "Struct2", "Struct1")
  exp$var_info$peptide <- rep("PEP1", 3)
  exp$var_info$peptide_site <- rep("N123", 3)
  exp$var_info$protein <- rep("PRO1", 3)
  exp$var_info$protein_site <- rep("N123", 3)

  result_struct <- summarize_experiment(exp, count_struct = TRUE)
  result_comp <- summarize_experiment(exp, count_struct = FALSE)

  expect_equal(
    result_struct$n[result_struct$item == "glycopeptide"],
    2
  )
  expect_equal(
    result_comp$n[result_comp$item == "glycopeptide"],
    1
  )
  expect_equal(
    result_struct$n[result_struct$item == "glycoform"],
    2
  )
  expect_equal(
    result_comp$n[result_comp$item == "glycoform"],
    1
  )
  expect_equal(result_struct$item, result_comp$item)
})

test_that("summarize_experiment fails when required columns are missing", {
  exp <- create_test_exp_2()
  expect_error(summarize_experiment(exp), "glycan_composition")
})

test_that("count_struct parameter validation works", {
  exp <- create_test_exp_2()
  exp$var_info$glycan_composition <- c("H5N2", "H5N2", "N3N2")
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP3")
  exp$var_info$peptide_site <- c("N123", "N456", "N789")
  
  # Invalid count_struct parameter
  expect_error(count_glycopeptides(exp, count_struct = "invalid"))
  expect_error(count_glycopeptides(exp, count_struct = 1))
  expect_error(count_glycoforms(exp, count_struct = "invalid"))
  expect_error(count_glycoforms(exp, count_struct = 1))
})

test_that("count_glycopeptides fails when glycan columns are missing", {
  exp <- create_test_exp_2()
  exp$var_info$peptide <- c("PEP1", "PEP2", "PEP3")
  exp$var_info$peptide_site <- c("N123", "N456", "N789")
  
  expect_error(count_glycopeptides(exp, count_struct = FALSE))
  expect_error(count_glycopeptides(exp, count_struct = TRUE))
})

test_that("count_glycoforms fails when glycan columns are missing", {
  exp <- create_test_exp_2()
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO3")
  exp$var_info$protein_site <- c("N123", "N456", "N789")
  
  # count_glycoforms actually validates glycan column existence
  expect_error(count_glycoforms(exp, count_struct = FALSE), "glycan_composition")
  expect_error(count_glycoforms(exp, count_struct = TRUE), "glycan_structure")
})

test_that("plural column resolution works correctly", {
  # Test .resolve_plural_col indirectly through functions that use it
  exp <- create_test_exp_2()
  
  # Both singular and plural present - should prefer singular
  exp$var_info$protein <- c("PRO1", "PRO2", "PRO3")
  exp$var_info$proteins <- c("PRO_PLURAL1", "PRO_PLURAL2", "PRO_PLURAL3")
  exp$var_info$protein_site <- c("N123", "N456", "N789")
  exp$var_info$protein_sites <- c("N_PLURAL123", "N_PLURAL456", "N_PLURAL789")
  
  result_proteins <- count_proteins(exp)
  expect_equal(result_proteins, 3)  # Should use singular form
  
  result_glycosites <- count_glycosites(exp)
  expect_equal(result_glycosites, 3)  # Should use singular forms
})

test_that("error messages are informative", {
  exp <- create_test_exp_2()
  
  # Test multiple missing columns
  expect_error(count_glycopeptides(exp), "peptide.*peptide_site")
  
  # Test specific column names in error messages  
  expect_error(count_compositions(exp), "glycan_composition")
  expect_error(count_structures(exp), "glycan_structure")
  expect_error(count_peptides(exp), "peptide")
})
