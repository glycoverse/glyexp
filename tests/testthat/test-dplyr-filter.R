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


test_that("filtering to no samples/variables raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(filter_obs(exp, group == "bad"), error = TRUE)
  expect_snapshot(filter_var(exp, type == "bad"), error = TRUE)
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
