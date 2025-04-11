test_that("add_comp_descriptions adds correct columns", {
  # Create a test experiment
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2"))
  exp$var_info$glycan_composition <- c("H5N4F1S2", "H4N3A1")

  # Add descriptions
  exp_with_desc <- add_comp_descriptions(exp)

  # Check if all expected columns are added
  expect_true(all(c("n_hex", "n_hexnac", "n_fuc", "n_neuac", "n_neugc", "n_sia") %in% 
                  colnames(exp_with_desc$var_info)))

  # Check values for first composition (H5N4F1S2)
  expect_equal(exp_with_desc$var_info$n_hex[1], 5)
  expect_equal(exp_with_desc$var_info$n_hexnac[1], 4)
  expect_equal(exp_with_desc$var_info$n_fuc[1], 1)
  expect_equal(exp_with_desc$var_info$n_neuac[1], 0)
  expect_equal(exp_with_desc$var_info$n_neugc[1], 0)
  expect_equal(exp_with_desc$var_info$n_sia[1], 2)

  # Check values for second composition (H4N3A1)
  expect_equal(exp_with_desc$var_info$n_hex[2], 4)
  expect_equal(exp_with_desc$var_info$n_hexnac[2], 3)
  expect_equal(exp_with_desc$var_info$n_fuc[2], 0)
  expect_equal(exp_with_desc$var_info$n_neuac[2], 1)
  expect_equal(exp_with_desc$var_info$n_neugc[2], 0)
  expect_equal(exp_with_desc$var_info$n_sia[2], 1)
})


test_that("add_comp_descriptions handles A and G correctly", {
  # Test with both A and G in composition
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2"))
  exp$var_info$glycan_composition <- c("H5N4A1G1", "H4N3A1")

  exp_with_desc <- add_comp_descriptions(exp)

  # Check if A and G are correctly counted
  expect_equal(exp_with_desc$var_info$n_neuac[1], 1)  # A
  expect_equal(exp_with_desc$var_info$n_neugc[1], 1)  # G
  expect_equal(exp_with_desc$var_info$n_sia[1], 2)    # A + G
  expect_equal(exp_with_desc$var_info$n_sia[2], 1)    # Only A
})


test_that("add_comp_descriptions throws error for invalid input", {
  # Test with missing glycan_composition column
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2"))
  exp$var_info$glycan_composition <- NULL

  expect_error(add_comp_descriptions(exp), 
               "Column glycan_composition not found in var_info")

  # Test with non-experiment object
  expect_error(add_comp_descriptions("not_an_experiment"),
               "Assertion on 'exp' failed")
})
