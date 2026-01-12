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
