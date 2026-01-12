# Copyright (c) 2024 Glyexp authors
#
# This file is part of glyexp.

test_that("standardize_variable exists", {
  expect_true(is.function(standardize_variable))
})

test_that("standardize_variable returns experiment invisibly", {
  exp <- toy_experiment
  exp$meta_data$exp_type <- "glycomics"
  exp$var_info$glycan_composition <- glyrepr::glycan_composition(c(Hex = 5, HexNAc = 2))

  res <- standardize_variable(exp)
  expect_invisible(res)
})
