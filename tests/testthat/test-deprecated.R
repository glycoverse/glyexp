expect_default_container_warning <- function(object) {
  expect_warning(
    object,
    regexp = "GlycomicSE.*GlycoproteomicSE"
  )
}

test_that("legacy experiment APIs recommend the default SE containers", {
  withr::local_options(lifecycle_verbosity = "warning")

  exp <- toy_experiment
  se <- suppressWarnings(as_se(exp))

  expect_snapshot(
    invisible(experiment(matrix(1, 1, 1, dimnames = list("V1", "S1"))))
  )

  expect_default_container_warning(is_experiment(exp))
  expect_default_container_warning(as_se(exp))
  expect_default_container_warning(from_se(se, exp_type = "others"))

  expect_default_container_warning(get_expr_mat(exp))
  expect_default_container_warning(get_sample_info(exp))
  expect_default_container_warning(get_var_info(exp))
  expect_default_container_warning(get_meta_data(exp))
  expect_default_container_warning(get_exp_type(exp))
  expect_default_container_warning(get_glycan_type(exp))
  expect_default_container_warning(set_meta_data(exp, "note", "value"))
  expect_default_container_warning(set_exp_type(exp, "others"))
  expect_default_container_warning(set_glycan_type(exp, "O"))

  expect_default_container_warning(samples(exp))
  expect_default_container_warning(variables(exp))
  expect_default_container_warning(n_samples(exp))
  expect_default_container_warning(n_variables(exp))

  expect_default_container_warning(dim(exp))
  expect_default_container_warning(dimnames(exp))
  expect_default_container_warning(exp[,])
  expect_default_container_warning(tibble::as_tibble(exp))
  expect_default_container_warning(split(exp, group, where = "sample_info"))
  expect_default_container_warning(suppressMessages(print(exp)))

  exp$var_info$peptide <- paste0("P", seq_len(nrow(exp$var_info)))
  exp2 <- exp
  exp2$sample_info$sample <- paste0("X", exp2$sample_info$sample)
  colnames(exp2$expr_mat) <- exp2$sample_info$sample
  expect_default_container_warning(merge(exp, exp2))
})

test_that("read-only legacy methods warn before retaining their errors", {
  withr::local_options(lifecycle_verbosity = "warning")
  exp <- toy_experiment

  expect_snapshot(dim(exp) <- c(2, 2), error = TRUE)
  expect_snapshot(exp[1, 1] <- 0, error = TRUE)
})

test_that("standardize_variable is not deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")

  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2"))
  exp$var_info$glycan_composition <- glyrepr::glycan_composition(
    c(Hex = 5, HexNAc = 2)
  )

  expect_no_warning(standardize_variable(exp))
})

test_that("retained modern and tidy APIs are not deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")

  expect_no_warning(as_glycoproteomic_se(real_experiment))
  expect_no_warning(mutate_obs(toy_experiment, condition = .data$group))
  expect_no_warning(summarize_experiment(real_experiment))
})
