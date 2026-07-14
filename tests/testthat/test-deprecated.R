test_that("retained modern and tidy APIs are not deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")

  glycoproteomic_se <- as_glycoproteomic_se(real_experiment)

  expect_no_warning(mutate_col(real_experiment, condition = .data$group))
  expect_no_warning(mutate_col(glycoproteomic_se, review_flag = 1))
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

test_that("deprecated dplyr aliases warn and preserve behavior", {
  withr::local_options(lifecycle_verbosity = "warning")

  expect_snapshot({
    old <- filter_obs(real_experiment, group == "H")
  })
  expect_identical(old, filter_col(real_experiment, group == "H"))
})

test_that("all deprecated dplyr aliases remain exported", {
  aliases <- c(
    "arrange_obs",
    "arrange_var",
    "filter_obs",
    "filter_var",
    "mutate_obs",
    "mutate_var",
    "rename_obs",
    "rename_var",
    "select_obs",
    "select_var",
    "slice_obs",
    "slice_var",
    "slice_head_obs",
    "slice_head_var",
    "slice_tail_obs",
    "slice_tail_var",
    "slice_sample_obs",
    "slice_sample_var",
    "slice_max_obs",
    "slice_max_var",
    "slice_min_obs",
    "slice_min_var",
    "left_join_obs",
    "left_join_var",
    "inner_join_obs",
    "inner_join_var",
    "semi_join_obs",
    "semi_join_var",
    "anti_join_obs",
    "anti_join_var"
  )

  expect_true(all(aliases %in% getNamespaceExports("glyexp")))
})
