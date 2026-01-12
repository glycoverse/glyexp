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
