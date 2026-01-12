test_that(".get_aa_from_peptide returns amino acid at position", {
  var_info <- tibble::tibble(
    peptide = c("NKT", "LPNG"),
    peptide_site = c(1L, 3L)
  )
  result <- .get_aa_from_peptide(var_info)
  expect_equal(result, c("N", "N"))
})
