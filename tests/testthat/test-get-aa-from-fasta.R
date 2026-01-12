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
