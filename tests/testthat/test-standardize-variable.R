test_that("standardize_variable works for glycomics", {
  # Create a glycomics experiment with different glycan compositions
  expr_mat <- matrix(1:6, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2", "S3")
  sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycomics", glycan_type = "N")

  # Use custom format with protein column to ensure unique IDs
  exp$var_info$protein <- c("ProtA", "ProtB")
  res <- standardize_variable(exp, format = "{protein}-{glycan_composition}")

  expect_equal(res$var_info$variable, c("ProtA-Hex(5)HexNAc(2)", "ProtB-Hex(5)HexNAc(2)"))
  expect_equal(rownames(res$expr_mat), c("ProtA-Hex(5)HexNAc(2)", "ProtB-Hex(5)HexNAc(2)"))
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

test_that("standardize_variable works for traitomics with motif", {
  expr_mat <- matrix(1:4, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2")
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    motif = c("Lewis A", "Lewis B")
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "traitomics", glycan_type = "N")

  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("Lewis A", "Lewis B"))
})

test_that("standardize_variable works for traitomics with trait", {
  expr_mat <- matrix(1:4, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2")
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    trait = c("high_mannose", "complex")
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "traitomics", glycan_type = "N")

  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("high_mannose", "complex"))
})

test_that("standardize_variable works for traitproteomics", {
  expr_mat <- matrix(1:4, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2")
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    protein = c("P12345", "P12345"),
    protein_site = c(32L, 45L),
    motif = c("Lewis A", "Lewis B")
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "traitproteomics", glycan_type = "N")

  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("P12345-32-Lewis A", "P12345-45-Lewis B"))
})

test_that("standardize_variable works with custom format", {
  expr_mat <- matrix(1:4, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2")
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    protein = c("P12345", "P67890"),
    gene = c("GENE_A", "GENE_B"),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycomics", glycan_type = "N")

  res <- standardize_variable(exp, format = "{gene}-{glycan_composition}")

  expect_equal(res$var_info$variable, c("GENE_A-Hex(5)HexNAc(2)", "GENE_B-Hex(5)HexNAc(2)"))
})

test_that("standardize_variable works with custom unique_suffix", {
  expr_mat <- matrix(1:4, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2")
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycomics", glycan_type = "N")

  res <- standardize_variable(exp, unique_suffix = "_v{N}")

  expect_equal(sort(res$var_info$variable), c("Hex(5)HexNAc(2)_v1", "Hex(5)HexNAc(2)_v2"))
})

# Tests for .get_default_format
test_that(".get_default_format returns correct format for glycomics", {
  var_info <- tibble::tibble(
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5))
  )
  format <- .get_default_format("glycomics", var_info)
  expect_equal(format, "{glycan_composition}")
})

test_that(".get_default_format returns correct format for glycoproteomics with protein_site", {
  var_info <- tibble::tibble(
    protein = c("P12345"),
    protein_site = c(32L),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5))
  )
  format <- .get_default_format("glycoproteomics", var_info)
  expect_equal(format, "{protein}-{protein_site}-{glycan_composition}")
})

test_that(".get_default_format returns correct format for glycoproteomics without protein_site", {
  var_info <- tibble::tibble(
    protein = c("P12345"),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5))
  )
  format <- .get_default_format("glycoproteomics", var_info)
  expect_equal(format, "{protein}-{glycan_composition}")
})

test_that(".get_default_format returns correct format for traitproteomics with motif", {
  var_info <- tibble::tibble(
    protein = c("P12345"),
    protein_site = c(32L),
    motif = c("Lewis A")
  )
  format <- .get_default_format("traitproteomics", var_info)
  expect_equal(format, "{protein}-{protein_site}-{motif}")
})

test_that("standardize_variable uses protein_site directly for glycoproteomics", {
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

  # Should use protein_site directly
  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("P12345-32-Hex(5)HexNAc(2)", "P12345-45-Hex(5)HexNAc(2)"))
})

test_that("standardize_variable works for traitproteomics with protein_site", {
  expr_mat <- matrix(1:4, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2")
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    protein = c("P12345", "P12345"),
    protein_site = c(32L, 45L),
    motif = c("Lewis A", "Lewis B")
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "traitproteomics", glycan_type = "N")

  # Should use protein_site directly
  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("P12345-32-Lewis A", "P12345-45-Lewis B"))
})
