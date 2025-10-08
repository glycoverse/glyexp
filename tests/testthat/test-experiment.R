create_valid_glycomics_var_info <- function(variables) {
  tibble::tibble(
    variable = variables,
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), length(variables))
  )
}

create_valid_glycoproteomics_var_info <- function(variables) {
  tibble::tibble(
    variable = variables,
    protein = rep("P1", length(variables)),
    protein_site = rep(1L, length(variables)),
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), length(variables))
  )
}

test_that("constructor of experiment works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  exp <- new_experiment(expr_mat, sample_info, var_info, list(foo = "bar"))

  # name attribute removed
  expect_equal(exp$expr_mat, expr_mat)
  expect_equal(exp$sample_info, sample_info)
  expect_equal(exp$var_info, var_info)
  expect_equal(exp$meta_data$foo, "bar")
})


test_that("helper for creating experiment works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  exp <- experiment(expr_mat, sample_info, var_info, "glycomics", "N")

  expect_equal(exp$expr_mat, expr_mat)
  expect_equal(exp$sample_info, sample_info)
  expect_equal(exp$var_info, var_info)
})


test_that("data.frames as input data works", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  sample_info_df <- tibble::column_to_rownames(sample_info, "sample")
  var_info <- create_valid_glycoproteomics_var_info(c("V1", "V2", "V3"))
  var_info_df <- tibble::column_to_rownames(var_info, "variable")

  exp <- experiment(expr_mat, sample_info_df, var_info_df, "glycoproteomics", "O")

  expect_identical(exp$sample_info, sample_info)
  expect_identical(exp$var_info, var_info)
})


test_that("creating experiment with wrong order of samples and variables works", {
  expr_mat <- create_expr_mat(c("S3", "S1", "S2"), c("V2", "V3", "V1"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  exp <- experiment(expr_mat, sample_info, var_info, "glycomics", "N")

  expect_identical(colnames(exp$expr_mat), c("S1", "S2", "S3"))
  expect_identical(rownames(exp$expr_mat), c("V1", "V2", "V3"))
  expect_identical(exp$sample_info, sample_info)
  expect_identical(exp$var_info, var_info)
})


test_that("creating experiment with missing samples in `sample_info`", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycomics", "N"), error = TRUE)
})


test_that("creating experiment with missing variables in `var_info`", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2"))

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycoproteomics", "O"), error = TRUE)
})


test_that("creating experiment with extra samples in `sample_info`", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3", "S4"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycomics", "N"), error = TRUE)
})


test_that("creating experiment with extra variables in `var_info`", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3", "V4"))

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycoproteomics", "O"), error = TRUE)
})


test_that("samples are not unique", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S2"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S2"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycomics", "N"), error = TRUE)
})


test_that("variables are not unique", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V2"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V2"))

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycoproteomics", "O"), error = TRUE)
})


test_that("both samples and variables are not unique", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S2"), c("V1", "V2", "V2"))
  sample_info <- create_sample_info(c("S1", "S2", "S2"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V2"))

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycomics", "N"), error = TRUE)
})


test_that("only one sample still works", {
  # `experiment` failed when there was only one sample
  # in commit b900650f9d because subseting `expr_mat` with a scalar
  # will automatically convert the matrix into a vector.
  expr_mat <- create_expr_mat(c("S1"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycomics", "N"), error = FALSE)
})


test_that("no variable works", {
  expr_mat <- matrix(nrow = 0, ncol = 3, dimnames = list(NULL, paste0("S", 1:3)))
  sample_info <- tibble::tibble(sample = paste0("S", 1:3))
  var_info <- tibble::tibble(variable = character(0), glycan_composition = glyrepr::glycan_composition())
  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycomics", "N"))
})


test_that("experiment correctly stores exp_type and glycan_type in meta_data", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycoproteomics_var_info(c("V1", "V2", "V3"))

  exp1 <- experiment(expr_mat, sample_info, var_info, "glycomics", "N")
  expect_equal(exp1$meta_data$exp_type, "glycomics")
  expect_equal(exp1$meta_data$glycan_type, "N")

  exp2 <- experiment(expr_mat, sample_info, var_info, "glycoproteomics", "O")
  expect_equal(exp2$meta_data$exp_type, "glycoproteomics")
  expect_equal(exp2$meta_data$glycan_type, "O")
})


test_that("experiment validates exp_type parameter", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  expect_snapshot(
    experiment(expr_mat, sample_info, var_info, "invalid_type", "N"),
    error = TRUE
  )
})


test_that("experiment validates glycan_type parameter", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  expect_snapshot(
    experiment(expr_mat, sample_info, var_info, "glycomics", "invalid_type"),
    error = TRUE
  )
})


test_that("experiment accepts additional meta_data through ...", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))

  exp <- experiment(
    expr_mat, 
    sample_info, 
    var_info, 
    "glycomics", 
    "N", 
    instrument = "Orbitrap",
    batch = "20230101"
  )

  expect_equal(exp$meta_data$exp_type, "glycomics")
  expect_equal(exp$meta_data$glycan_type, "N")
  expect_equal(exp$meta_data$instrument, "Orbitrap")
  expect_equal(exp$meta_data$batch, "20230101")
})


test_that("is_experiment() works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  expect_true(is_experiment(exp))
  expect_false(is_experiment(data.frame()))
})


test_that("experiment checks required columns in var_info", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycomics_var_info(c("V1", "V2", "V3"))
  var_info$glycan_composition <- NULL
  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycomics", "N"), error = TRUE)
})


test_that("experiment checks column types", {
  expr_mat <- create_expr_mat(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  sample_info <- create_sample_info(c("S1", "S2", "S3"))
  var_info <- create_valid_glycoproteomics_var_info(c("V1", "V2", "V3"))

  # Make some columns invalid
  var_info$protein_site <- 1.2
  var_info$glycan_composition <- "H5N2"

  expect_snapshot(experiment(expr_mat, sample_info, var_info, "glycoproteomics", "N"))
})


test_that("experiment coerces common column types safely", {
  expr_mat <- create_expr_mat(c("S1", "S2"), c("V1", "V2"))
  sample_info <- tibble::tibble(
    sample = c("S1", "S2"),
    group = c("A", "B"),
    batch = c("B1", "B2")
  )
  var_info <- tibble::tibble(
    variable = c("V1", "V2"),
    protein = c(101, 102),
    protein_site = as.numeric(c(1, 2)),
    peptide_site = c(3.5, 4),
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2)
  )

  expect_snapshot(exp <- experiment(expr_mat, sample_info, var_info, "glycoproteomics", "N"))

  expect_s3_class(exp$sample_info$group, "factor")
  expect_s3_class(exp$sample_info$batch, "factor")
  expect_type(exp$var_info$protein, "character")
  expect_true(is.integer(exp$var_info$protein_site))
  expect_type(exp$var_info$peptide_site, "double")
})

test_that("glycan_type can be NULL if exp_type is others", {
  expr_mat <- create_expr_mat(c("S1", "S2"), c("V1", "V2"))
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  var_info <- tibble::tibble(variable = c("V1", "V2"))
  exp <- experiment(expr_mat, sample_info, var_info, "others", NULL)
  expect_equal(exp$meta_data$glycan_type, NULL)
})

test_that("experiment can be created with just an expression matrix", {
  expr_mat <- create_expr_mat(c("S1", "S2"), c("V1", "V2"))
  exp <- experiment(expr_mat)
  expect_equal(exp$meta_data$exp_type, "others")
  expect_equal(exp$meta_data$glycan_type, NULL)
  expect_equal(exp$sample_info, tibble::tibble(sample = c("S1", "S2")))
  expect_equal(exp$var_info, tibble::tibble(variable = c("V1", "V2")))
})

test_that("var_info cannot be NULL if exp_type is not others", {
  expr_mat <- create_expr_mat(c("S1", "S2"), c("V1", "V2"))
  expect_snapshot(experiment(expr_mat, exp_type = "glycomics", glycan_type = "N"), error = TRUE)
  expect_snapshot(experiment(expr_mat, exp_type = "glycoproteomics", glycan_type = "O"), error = TRUE)
})