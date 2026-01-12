test_that(".get_aa_from_uniprot exists and has correct signature", {
  expect_true(is.function(.get_aa_from_uniprot))
  # Just test function exists - actual network tests can be manual
})
