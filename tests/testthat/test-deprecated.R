test_that("retained modern and tidy APIs are not deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")

  glycoproteomic_se <- as_glycoproteomic_se(real_experiment)

  expect_no_warning(mutate_obs(real_experiment, condition = .data$group))
  expect_no_warning(mutate_obs(glycoproteomic_se, review_flag = 1))
  expect_no_warning(standardize_variable(glycoproteomic_se))
  expect_no_warning(summarize_experiment(glycoproteomic_se))
})

test_that("standardize_variable is not deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")

  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2"))
  exp$var_info$glycan_composition <- glyrepr::glycan_composition(
    c(Hex = 5, HexNAc = 2)
  )

  expect_no_warning(standardize_variable(exp))
})
