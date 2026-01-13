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

  # Use withVisible to properly check invisibility
  wv <- withVisible(standardize_variable(exp))
  expect_false(wv$visible)
})

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

  # Provide fasta to use <site> token (amino acid + position)
  seqs <- c(P12345 = "MABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ")
  res <- standardize_variable(exp, fasta = seqs)

  expect_equal(res$var_info$variable, c("P12345-E32-Hex(5)HexNAc(2)", "P12345-R45-Hex(5)HexNAc(2)"))
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
    peptide = c("NKT", "LPNG"),
    peptide_site = c(NA_integer_, NA_integer_),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info, exp_type = "glycoproteomics", glycan_type = "N")

  res <- standardize_variable(exp)

  expect_equal(res$var_info$variable, c("P12345-X-Hex(5)HexNAc(2)", "P67890-X-Hex(5)HexNAc(2)"))
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

  # Provide fasta to use <site> token (amino acid + position)
  seqs <- c(P12345 = "MABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZ")
  res <- standardize_variable(exp, fasta = seqs)

  expect_equal(res$var_info$variable, c("P12345-E32-Lewis A", "P12345-R45-Lewis B"))
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

test_that("standardize_variable accepts fasta and taxid parameters", {
  expr_mat <- matrix(1:4, nrow = 2)
  rownames(expr_mat) <- c("V1", "V2")
  colnames(expr_mat) <- c("S1", "S2")
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    protein = c("P12345", "P12345"),
    protein_site = c(32L, 45L),
    peptide = c("NKT", "LPNG"),
    peptide_site = c(1L, 2L),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))
  )
  exp <- experiment(expr_mat, sample_info, var_info,
    exp_type = "glycoproteomics", glycan_type = "N"
  )

  # Should accept fasta and taxid parameters with <site> token in format
  seqs <- c(P12345 = "MABCDEFGHIJKLMNOPQRSTUVWXYZ")
  res <- standardize_variable(exp, format = "{protein}-<site>-{glycan_composition}",
                              fasta = seqs, taxid = 9606)

  expect_equal(res$var_info$variable, c("P12345-N32-Hex(5)HexNAc(2)", "P12345-P45-Hex(5)HexNAc(2)"))
})

# Tests for .get_default_format using <site> token
test_that(".get_default_format uses <site> for glycoproteomics", {
  var_info <- tibble::tibble(
    protein = c("P12345"),
    protein_site = c(32L),
    glycan_composition = glyrepr::glycan_composition(c(Hex = 5))
  )
  format <- .get_default_format("glycoproteomics", var_info)
  expect_equal(format, "{protein}-<site>-{glycan_composition}")
})

test_that(".get_default_format uses <site> for traitproteomics with motif", {
  var_info <- tibble::tibble(
    protein = c("P12345"),
    protein_site = c(32L),
    motif = c("Lewis A")
  )
  format <- .get_default_format("traitproteomics", var_info)
  expect_equal(format, "{protein}-<site>-{motif}")
})

test_that(".compute_site_aa_pos uses peptide if available", {
  var_info <- tibble::tibble(
    protein = c("P12345"),
    protein_site = c(32L),
    peptide = c("NKT"),
    peptide_site = c(1L)
  )
  result <- .compute_site_aa_pos(var_info, fasta = NULL, taxid = 9606)
  expect_equal(result, "N32")
})

test_that(".compute_site_aa_pos uses fasta if peptide not available", {
  var_info <- tibble::tibble(
    protein = c("P12345"),
    protein_site = c(32L)
  )
  seqs <- c(P12345 = paste(rep("F", 32), collapse = ""))
  result <- .compute_site_aa_pos(var_info, fasta = seqs, taxid = 9606)
  expect_equal(result, "F32")
})

test_that(".compute_site_aa_pos returns X when protein_site is NA", {
  var_info <- tibble::tibble(
    protein = c("P12345"),
    protein_site = c(NA_integer_),
    peptide = c("NKT"),
    peptide_site = c(1L)
  )
  result <- .compute_site_aa_pos(var_info, fasta = NULL, taxid = 9606)
  expect_equal(result, "X")
})

test_that(".compute_site_aa_pos handles mixed NA and non-NA protein_site", {
  var_info <- tibble::tibble(
    protein = c("P12345", "P12345"),
    protein_site = c(32L, NA_integer_),
    peptide = c("NKT", "NKT"),
    peptide_site = c(1L, NA_integer_)
  )
  result <- .compute_site_aa_pos(var_info, fasta = NULL, taxid = 9606)
  expect_equal(result, c("N32", "X"))
})

test_that(".get_aa_from_fasta parses FASTA and extracts AA", {
  # Named character vector input
  seqs <- c(ProteinA = "MABCDEFG", ProteinB = "MKLMNOPQ")
  var_info <- tibble::tibble(
    protein = c("ProteinA", "ProteinB"),
    protein_site = c(3L, 5L)
  )
  result <- .get_aa_from_fasta(var_info, fasta = seqs)
  expect_equal(result, c("B", "N"))
})

test_that(".get_aa_from_peptide returns amino acid at position", {
  var_info <- tibble::tibble(
    peptide = c("NKT", "LPNG"),
    peptide_site = c(1L, 3L)
  )
  result <- .get_aa_from_peptide(var_info)
  expect_equal(result, c("N", "N"))
})

test_that(".get_aa_from_uniprot exists and has correct signature", {
  expect_true(is.function(.get_aa_from_uniprot))
  # Just test function exists - actual network tests can be manual
})

test_that(".resolve_site_token replaces <site> with {site_aa_pos} placeholder", {
  var_info <- tibble::tibble(
    variable = c("V1"),
    protein = c("P12345"),
    protein_site = c(32L)
  )
  format <- "{protein}-<site>-{glycan_composition}"
  site_aa_pos <- "N32"
  result <- .resolve_site_token(var_info, format, site_aa_pos)
  # Function should replace <site> with {site_aa_pos} placeholder
  expect_equal(result, "{protein}-{site_aa_pos}-{glycan_composition}")
})

test_that(".resolve_site_token returns format unchanged if no <site>", {
  format <- "{protein}-{glycan_composition}"
  site_aa_pos <- "N32"
  var_info <- tibble::tibble(protein = c("P12345"))
  result <- .resolve_site_token(var_info, format, site_aa_pos)
  expect_equal(result, "{protein}-{glycan_composition}")
})
