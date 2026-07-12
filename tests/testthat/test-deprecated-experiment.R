legacy_api_names <- c(
  "experiment",
  "is_experiment",
  "as_se",
  "from_se",
  "get_expr_mat",
  "get_sample_info",
  "get_var_info",
  "get_meta_data",
  "get_exp_type",
  "get_glycan_type",
  "set_meta_data",
  "set_exp_type",
  "set_glycan_type",
  "samples",
  "variables",
  "n_samples",
  "n_variables",
  "arrange_obs",
  "arrange_var",
  "filter_obs",
  "filter_var",
  "left_join_obs",
  "inner_join_obs",
  "semi_join_obs",
  "anti_join_obs",
  "left_join_var",
  "inner_join_var",
  "semi_join_var",
  "anti_join_var",
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
  "standardize_variable"
)

test_that("every exported legacy experiment helper warns", {
  namespace <- asNamespace("glyexp")
  warned_names <- names(Filter(
    function(x) {
      is.function(x) && any(grepl("\\.deprecate_experiment", deparse(body(x))))
    },
    mget(legacy_api_names, envir = namespace, inherits = FALSE)
  ))

  expect_setequal(warned_names, legacy_api_names)
})

test_that("legacy experiment entry points are soft-deprecated", {
  old_options <- options(glyexp.expect_deprecation = TRUE)
  on.exit(options(old_options), add = TRUE)
  exp <- create_test_exp(c("S1", "S2"), c("V1", "V2"))

  lifecycle::expect_deprecated(experiment(exp$expr_mat))
  lifecycle::expect_deprecated(is_experiment(exp))
  lifecycle::expect_deprecated(as_se(exp))
  lifecycle::expect_deprecated(get_expr_mat(exp))
  lifecycle::expect_deprecated(get_meta_data(exp))
  lifecycle::expect_deprecated(samples(exp))
  lifecycle::expect_deprecated(filter_obs(exp, group == "A"))
  lifecycle::expect_deprecated(mutate_var(exp, extra = 1:2))
  lifecycle::expect_deprecated(dim(exp))
  lifecycle::expect_deprecated(exp[,])
})

test_that("native containers do not warn", {
  se <- GlycomicSE(
    matrix(1:4, nrow = 2),
    rowData = S4Vectors::DataFrame(
      glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2)
    ),
    metadata = list(glycan_type = "N")
  )

  expect_silent(summarize_experiment(se))
})
