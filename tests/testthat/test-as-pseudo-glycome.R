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


# Create a glycoproteomics experiment with glycan_structure
create_test_gp_exp_with_structure <- function() {
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

  # Two different structures with same composition
  strucs <- glyrepr::as_glycan_structure(c(
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-",
    "Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-",
    "GlcNAc(b1-2)Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-",
    "GlcNAc(b1-2)Man(a1-3)[Man(a1-6)]Man(b1-4)GlcNAc(b1-4)GlcNAc(b1-"
  ))

  var_info <- tibble::tibble(
    variable = c("GP1", "GP2", "GP3", "GP4"),
    protein = c("P1", "P1", "P2", "P2"),
    protein_site = c(1L, 2L, 1L, 2L),
    glycan_composition = glyrepr::as_glycan_composition(c("H5N2", "H5N2", "H6N3", "H6N3")),
    glycan_structure = strucs
  )

  glyexp::experiment(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
}

test_that("as_pseudo_glycome aggregates by glycan_structure when available", {
  gp_exp <- create_test_gp_exp_with_structure()

  result <- as_pseudo_glycome(gp_exp)

  # Should have 2 variables (two unique structures)
  expect_equal(nrow(result$expr_mat), 2)

  # Should have both columns
  expect_true("glycan_structure" %in% colnames(result$var_info))
  expect_true("glycan_composition" %in% colnames(result$var_info))

  # Check aggregated values match composition-based expectation
  expect_equal(as.vector(result$expr_mat[1, ]), c(5, 7, 9))
  expect_equal(as.vector(result$expr_mat[2, ]), c(17, 19, 21))
})

test_that("as_pseudo_glycome validates input type", {
  expect_error(
    as_pseudo_glycome("not an experiment"),
    "must be an experiment"
  )

  expect_error(
    as_pseudo_glycome(list(not = "an experiment")),
    "must be an experiment"
  )
})

test_that("as_pseudo_glycome requires glycoproteomics experiment", {
  # Create a glycomics experiment
  expr_mat <- matrix(1:6, nrow = 2)
  colnames(expr_mat) <- c("S1", "S2", "S3")
  rownames(expr_mat) <- c("G1", "G2")

  sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
  var_info <- tibble::tibble(
    variable = c("G1", "G2"),
    glycan_composition = glyrepr::as_glycan_composition(c("H5N4", "H6N5"))
  )

  glycome_exp <- glyexp::experiment(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "glycomics",
    glycan_type = "N"
  )

  expect_error(
    as_pseudo_glycome(glycome_exp),
    "glycoproteomics"
  )
})

test_that("as_pseudo_glycome handles empty experiment", {
  expr_mat <- matrix(nrow = 0, ncol = 3)
  colnames(expr_mat) <- c("S1", "S2", "S3")

  sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
  var_info <- tibble::tibble(
    variable = character(),
    protein = character(),
    protein_site = integer(),
    glycan_composition = glyrepr::as_glycan_composition(character())
  )

  empty_exp <- glyexp::experiment(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )

  result <- as_pseudo_glycome(empty_exp)

  expect_true(glyexp::is_experiment(result))
  expect_equal(nrow(result$expr_mat), 0)
  expect_equal(glyexp::get_exp_type(result), "glycomics")
})

test_that("as_pseudo_glycome handles NA values correctly", {
  expr_mat <- matrix(
    c(1, NA, 3, 4,
      2, 5, NA, 7,
      3, 6, 9, 10),
    nrow = 4, ncol = 3,
    dimnames = list(
      c("GP1", "GP2", "GP3", "GP4"),
      c("S1", "S2", "S3")
    )
  )

  sample_info <- tibble::tibble(sample = c("S1", "S2", "S3"))
  var_info <- tibble::tibble(
    variable = c("GP1", "GP2", "GP3", "GP4"),
    protein = c("P1", "P1", "P2", "P2"),
    protein_site = c(1L, 2L, 1L, 2L),
    glycan_composition = glyrepr::as_glycan_composition(c("H5N4", "H5N4", "H6N5", "H6N5"))
  )

  gp_exp <- glyexp::experiment(
    expr_mat = expr_mat,
    sample_info = sample_info,
    var_info = var_info,
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )

  result <- as_pseudo_glycome(gp_exp)

  # GP1 + GP2: (1+0, 2+5, 3+6) = (1, 7, 9) - NA treated as 0
  # GP3 + GP4: (3+4, 0+7, 9+10) = (7, 7, 19)
  expect_equal(as.vector(result$expr_mat[1, ]), c(1, 7, 9))
  expect_equal(as.vector(result$expr_mat[2, ]), c(7, 7, 19))
})