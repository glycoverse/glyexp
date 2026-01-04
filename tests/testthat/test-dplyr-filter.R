# Expression Matrix for testing:
#    S1 S2 S3
# V1  1  4  7
# V2  2  5  8
# V3  3  6  9


test_that("filtering works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp2 <- filter_obs(exp, sample %in% c("S1", "S3"))
  exp2 <- filter_var(exp2, variable %in% c("V1", "V2"))

  # check expr_mat
  expected_expr_mat <- matrix(c(1, 2, 7, 8), nrow = 2)
  colnames(expected_expr_mat) <- c("S1", "S3")
  rownames(expected_expr_mat) <- c("V1", "V2")
  expect_equal(exp2$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp2$sample_info, create_sample_info(c("S1", "S3")))
  # check var_info
  expect_equal(exp2$var_info, create_var_info(c("V1", "V2")))
})


test_that("filtering to no samples/variables results in an empty experiment", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_no_obs <- filter_obs(exp, sample == "bad")
  expect_equal(ncol(exp_no_obs$expr_mat), 0)
  expect_equal(nrow(exp_no_obs$sample_info), 0)

  exp_no_var <- filter_var(exp, variable == "bad")

  expect_equal(nrow(exp_no_var$expr_mat), 0)
  expect_equal(nrow(exp_no_var$var_info), 0)
})


test_that("filtering using non-existing columns raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(filter_obs(exp, bad_column == 1), error = TRUE)
  expect_snapshot(filter_var(exp, bad_column == 1), error = TRUE)
})


test_that("filtering with one sample left", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp2 <- filter_obs(exp, sample == "S1")

  expect_equal(colnames(exp2$expr_mat), "S1")
})


test_that("filtering with one variable left", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp2 <- filter_var(exp, variable == "V1")

  expect_equal(rownames(exp2$expr_mat), "V1")
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"

  exp2 <- filter_var(exp)

  expect_equal(exp2$something, "haha")
})


test_that("drop levels only for referenced columns in filter_obs", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$sample_info <- tibble::tibble(
    sample = c("S1", "S2", "S3"),
    group = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    batch = factor(c("X", "Y", "Z"), levels = c("X", "Y", "Z"))
  )

  exp2 <- filter_obs(exp, group %in% c("A", "B"), .drop_levels = TRUE)

  expect_equal(levels(exp2$sample_info$group), c("A", "B"))
  expect_equal(levels(exp2$sample_info$batch), c("X", "Y", "Z"))
})


test_that("drop levels for if_any selection in filter_obs", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$sample_info <- tibble::tibble(
    sample = c("S1", "S2", "S3"),
    group = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    batch = factor(c("X", "Y", "Z"), levels = c("X", "Y", "Z"))
  )

  exp2 <- filter_obs(
    exp,
    dplyr::if_any(c(group, batch), ~ .x %in% c("A", "X")),
    .drop_levels = TRUE
  )

  expect_equal(levels(exp2$sample_info$group), "A")
  expect_equal(levels(exp2$sample_info$batch), "X")
})


test_that("drop levels only for referenced columns in filter_var", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$var_info <- tibble::tibble(
    variable = c("V1", "V2", "V3"),
    type = factor(c("T1", "T2", "T3"), levels = c("T1", "T2", "T3")),
    class = factor(c("C1", "C2", "C3"), levels = c("C1", "C2", "C3"))
  )

  exp2 <- filter_var(exp, type %in% c("T1", "T2"), .drop_levels = TRUE)

  expect_equal(levels(exp2$var_info$type), c("T1", "T2"))
  expect_equal(levels(exp2$var_info$class), c("C1", "C2", "C3"))
})
