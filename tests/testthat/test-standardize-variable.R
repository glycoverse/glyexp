# Copyright (c) 2024 Glyexp authors
#
# This file is part of glyexp.

test_that("standardize_variable exists", {
  expect_true(is.function(standardize_variable))
})

test_that("standardize_variable returns experiment invisibly", {
  exp <- toy_experiment
  exp$meta_data$exp_type <- "glycomics"
  exp$var_info$glycan_composition <- glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))

  res <- standardize_variable(exp)
  expect_invisible(res)
})

test_that("standardize_variable works for glycomics", {
  # Create a glycomics experiment with meaningless variable IDs
  expr_mat <- matrix(1:6, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2", "S3")
  sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycomics", glycan_type = "N")

  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("Hex(5)HexNAc(2)", "Hex(5)HexNAc(2)"))
  expect_equal(rownames(res$expr_mat), c("Hex(5)HexNAc(2)", "Hex(5)HexNAc(2)"))
})

test_that("standardize_variable makes IDs unique with suffix", {
  # Same composition for both -> should get suffix
  expr_mat <- matrix(1:6, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2", "S3")
  sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycomics", glycan_type = "N")

  res <- standardize_variable(exp)

  expect_equal(sort(res$var_info$variable), c("Hex(5)HexNAc(2)-1", "Hex(5)HexNAc(2)-2"))
})

test_that("standardize_variable works for glycoproteomics", {
  expr_mat <- matrix(1:6, nrow = 2)
  rownames(expr_mat) <- c("GP1", "GP2")
  colnames(expr_mat) <- c("S1", "S2", "S3")
  sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
  var_info <- tibble::tibble(
    variable = c("GP1", "GP2"),
    protein = c("P12345", "P12345"),
    protein_site = c(32L, 45L),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycoproteomics", glycan_type = "N")

  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("P12345-32-Hex(5)HexNAc(2)", "P12345-45-Hex(5)HexNAc(2)"))
})

test_that("standardize_variable works for glycoproteomics without protein_site", {
  # Create a glycoproteomics experiment with protein_site = NA
  # This tests the case where site information is not available
  expr_mat <- matrix(1:6, nrow = 2)
  rownames(expr_mat) <- c("GP1", "GP2")
  colnames(expr_mat) <- c("S1", "S2", "S3")
  sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
  var_info <- tibble::tibble(
    variable = c("GP1", "GP2"),
    protein = c("P12345", "P67890"),
    protein_site = c(NA_integer_, NA_integer_),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycoproteomics", glycan_type = "N")

  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("P12345-Hex(5)HexNAc(2)", "P67890-Hex(5)HexNAc(2)"))
})
