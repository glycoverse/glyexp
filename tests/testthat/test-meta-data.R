test_that("getting meta data works", {
  exp <- toy_experiment
  exp$meta_data <- list(structure_type = "pglyco", method = "LC-MS")

  # Test getting existing field
  expect_equal(get_meta_data(exp, "structure_type"), "pglyco")
  expect_equal(get_meta_data(exp, "method"), "LC-MS")

  # Test getting all fields
  expect_equal(
    get_meta_data(exp),
    list(structure_type = "pglyco", method = "LC-MS")
  )

  # Test getting non-existing field returns NULL
  expect_null(get_meta_data(exp, "non_existing_field"))
})


test_that("get_exp_type works", {
  exp <- toy_experiment

  # Test when exp_type is set
  exp$meta_data$exp_type <- "proteomics"
  expect_equal(get_exp_type(exp), "proteomics")

  # Test when exp_type is not set
  exp$meta_data$exp_type <- NULL
  expect_null(get_exp_type(exp))

  # Test with different exp_type values
  exp$meta_data$exp_type <- "glycomics"
  expect_equal(get_exp_type(exp), "glycomics")
})


test_that("get_glycan_type works", {
  exp <- toy_experiment

  # Test when glycan_type is set
  exp$meta_data$glycan_type <- "N-linked"
  expect_equal(get_glycan_type(exp), "N-linked")

  # Test when glycan_type is not set
  exp$meta_data$glycan_type <- NULL
  expect_null(get_glycan_type(exp))

  # Test with different glycan_type values
  exp$meta_data$glycan_type <- "O-linked"
  expect_equal(get_glycan_type(exp), "O-linked")
})


test_that("setting meta data works", {
  exp <- toy_experiment

  # Test setting a new field
  result <- set_meta_data(exp, "structure_type", "pglyco")
  expect_equal(get_meta_data(result, "structure_type"), "pglyco")

  # Test setting multiple fields
  result <- set_meta_data(exp, "method", "LC-MS")
  result <- set_meta_data(result, "instrument", "Q-Exactive")
  expect_equal(get_meta_data(result, "method"), "LC-MS")
  expect_equal(get_meta_data(result, "instrument"), "Q-Exactive")

  # Test that original experiment is not modified
  expect_null(get_meta_data(exp, "structure_type"))
  expect_equal(get_exp_type(exp), "others")
})

test_that("set_meta_data checks meta data", {
  exp <- toy_experiment
  expect_snapshot(set_meta_data(exp, "exp_type", "invalid_type"), error = TRUE)
  expect_snapshot(
    set_meta_data(exp, "glycan_type", "invalid_type"),
    error = TRUE
  )
})

test_that("set_exp_type works", {
  exp <- toy_experiment

  # Test setting exp_type to glycomics (only requires glycan_composition)
  result <- set_exp_type(exp, "glycomics")
  expect_equal(get_exp_type(result), "glycomics")

  # Test setting exp_type to traitomics (no required columns)
  result <- set_exp_type(exp, "traitomics")
  expect_equal(get_exp_type(result), "traitomics")

  # Test setting exp_type to an invalid value
  expect_error(set_exp_type(exp, "invalid_type"))
})


test_that("set_exp_type validates required columns for glycoproteomics", {
  # toy_experiment is missing protein_site, required for glycoproteomics
  exp <- toy_experiment
  expect_error(
    set_exp_type(exp, "glycoproteomics"),
    "protein_site"
  )

  # real_experiment has all required columns for glycoproteomics
  exp2 <- real_experiment
  result <- set_exp_type(exp2, "glycoproteomics")
  expect_equal(get_exp_type(result), "glycoproteomics")
})


test_that("set_exp_type validates required columns for traitproteomics", {
  # toy_experiment is missing protein_site, required for traitproteomics
  exp <- toy_experiment
  expect_error(
    set_exp_type(exp, "traitproteomics"),
    "protein_site"
  )
})


test_that("set_glycan_type works", {
  exp <- toy_experiment

  # Test setting glycan_type
  result <- set_glycan_type(exp, "O-GalNAc")
  expect_equal(get_glycan_type(result), "O-GalNAc")

  # Test setting glycan_type to NULL
  result <- set_glycan_type(exp, NULL)
  expect_null(get_glycan_type(result))

  # Test with different glycan_type values
  result <- set_glycan_type(exp, "N")
  expect_equal(get_glycan_type(result), "N")

  # Test setting glycan_type to an invalid value
  expect_error(set_glycan_type(exp, "invalid_type"))
})
