# Test as_pseudo_glycome function

# Create a simple glycoproteomics experiment for testing
create_test_gp_exp <- function() {
  # 4 glycopeptides, 3 samples
  # GP1: ProteinA-site1-H5N4 (sample values: 1, 2, 3)
  # GP2: ProteinA-site2-H5N4 (sample values: 4, 5, 6) - same glycan as GP1
  # GP3: ProteinB-site1-H6N5 (sample values: 7, 8, 9)
  # GP4: ProteinB-site2-H6N5 (sample values: 10, 11, 12) - same glycan as GP3
  expr_mat <- matrix(
    c(1, 4, 7, 10,
      2, 5, 8, 11,
      3, 6, 9, 12),
    nrow = 4, ncol = 3,
    dimnames = list(
      c("GP1", "GP2", "GP3", "GP4"),
      c("S1", "S2", "S3")
    )
  )

  sample_info <- tibble::tibble(
    sample = c("S1", "S2", "S3"),
    group = factor(c("A", "A", "B"))
  )

  var_info <- tibble::tibble(
    variable = c("GP1", "GP2", "GP3", "GP4"),
    protein = c("P1", "P1", "P2", "P2"),
    protein_site = c(1L, 2L, 1L, 2L),
    glycan_composition = glyrepr::as_glycan_composition(c(
      "H5N4", "H5N4", "H6N5", "H6N5"
    ))
  )

  glyexp::experiment(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
}

test_that("as_pseudo_glycome aggregates by glycan_composition", {
  gp_exp <- create_test_gp_exp()

  result <- as_pseudo_glycome(gp_exp)

  # Should be a valid experiment
  expect_true(glyexp::is_experiment(result))

  # Should be glycomics type
  expect_equal(glyexp::get_exp_type(result), "glycomics")

  # Should preserve glycan_type
  expect_equal(result$meta_data$glycan_type, "N")

  # Should have 2 variables (H5N4 and H6N5)
  expect_equal(nrow(result$expr_mat), 2)

  # Check aggregated values: GP1+GP2 = 1+4, 2+5, 3+6 = 5, 7, 9
  #                         GP3+GP4 = 7+10, 8+11, 9+12 = 17, 19, 21
  expect_equal(as.vector(result$expr_mat[1, ]), c(5, 7, 9))
  expect_equal(as.vector(result$expr_mat[2, ]), c(17, 19, 21))

  # Should have glycan_composition column
  expect_true("glycan_composition" %in% colnames(result$var_info))

  # Should not have protein columns
  expect_false("protein" %in% colnames(result$var_info))
  expect_false("protein_site" %in% colnames(result$var_info))
})
