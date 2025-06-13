# Expression Matrix for testing:
#    S1 S2 S3
# V1  1  4  7
# V2  2  5  8
# V3  3  6  9


test_that("subsetting i>1 j>1", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[1:2, 2:3]

  # check expr_mat
  expected_expr_mat <- matrix(c(4, 5, 7, 8), nrow = 2)
  colnames(expected_expr_mat) <- c("S2", "S3")
  rownames(expected_expr_mat) <- c("V1", "V2")
  expect_equal(exp_sub$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp_sub$sample_info, create_sample_info(c("S2", "S3")))
  # check var_info
  expect_equal(exp_sub$var_info, create_var_info(c("V1", "V2")))
})


test_that("subsetting i>1 j=1", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[1:2, 1]

  # check expr_mat
  expected_expr_mat <- matrix(c(1, 2), nrow = 2)
  colnames(expected_expr_mat) <- c("S1")
  rownames(expected_expr_mat) <- c("V1", "V2")
  expect_equal(exp_sub$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp_sub$sample_info, create_sample_info(c("S1")))
  # check var_info
  expect_equal(exp_sub$var_info, create_var_info(c("V1", "V2")))
})


test_that("subsetting i=1 j>1", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[1, 2:3]

  # check expr_mat
  expected_expr_mat <- matrix(c(4, 7), nrow = 1)
  colnames(expected_expr_mat) <- c("S2", "S3")
  rownames(expected_expr_mat) <- c("V1")
  expect_equal(exp_sub$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp_sub$sample_info, create_sample_info(c("S2", "S3")))
  # check var_info
  expect_equal(exp_sub$var_info, create_var_info(c("V1")))
})


test_that("subsetting i=1 j=1", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[1, 1]

  # check expr_mat
  expected_expr_mat <- matrix(1, nrow = 1)
  colnames(expected_expr_mat) <- c("S1")
  rownames(expected_expr_mat) <- c("V1")
  expect_equal(exp_sub$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp_sub$sample_info, create_sample_info(c("S1")))
  # check var_info
  expect_equal(exp_sub$var_info, create_var_info(c("V1")))
})


test_that("subsetting i>1 j=m", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[1:2, ]

  # check expr_mat
  expected_expr_mat <- matrix(c(1, 4, 7, 2, 5, 8), nrow = 2, byrow = TRUE)
  colnames(expected_expr_mat) <- c("S1", "S2", "S3")
  rownames(expected_expr_mat) <- c("V1", "V2")
  expect_equal(exp_sub$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp_sub$sample_info, create_sample_info(c("S1", "S2", "S3")))
  # check var_info
  expect_equal(exp_sub$var_info, create_var_info(c("V1", "V2")))
})


test_that("subsetting i=m j>1", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[, 2:3]

  # check expr_mat
  expected_expr_mat <- matrix(c(4, 7, 5, 8, 6, 9), nrow = 3, byrow = TRUE)
  colnames(expected_expr_mat) <- c("S2", "S3")
  rownames(expected_expr_mat) <- c("V1", "V2", "V3")
  expect_equal(exp_sub$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp_sub$sample_info, create_sample_info(c("S2", "S3")))
  # check var_info
  expect_equal(exp_sub$var_info, create_var_info(c("V1", "V2", "V3")))
})


test_that("subsetting i=m j=m", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[, ]

  expect_identical(exp_sub, exp)
})


# The `name` argument has been removed; skipping the corresponding test.


test_that("subsetting i>1 j>1 using chars", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[c("V1", "V2"), c("S2", "S3")]

  # check expr_mat
  expected_expr_mat <- matrix(c(4, 5, 7, 8), nrow = 2)
  colnames(expected_expr_mat) <- c("S2", "S3")
  rownames(expected_expr_mat) <- c("V1", "V2")
  expect_equal(exp_sub$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp_sub$sample_info, create_sample_info(c("S2", "S3")))
  # check var_info
  expect_equal(exp_sub$var_info, create_var_info(c("V1", "V2")))
})


test_that("subsetting mixed using nums and chars", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[c("V1", "V2"), 2:3]

  # check expr_mat
  expected_expr_mat <- matrix(c(4, 5, 7, 8), nrow = 2)
  colnames(expected_expr_mat) <- c("S2", "S3")
  rownames(expected_expr_mat) <- c("V1", "V2")
  expect_equal(exp_sub$expr_mat, expected_expr_mat)
  # check sample_info
  expect_equal(exp_sub$sample_info, create_sample_info(c("S2", "S3")))
  # check var_info
  expect_equal(exp_sub$var_info, create_var_info(c("V1", "V2")))
})


test_that("subsetting keeps orders consistent", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  exp_sub <- exp[c(1, 3, 2), c(2, 3, 1)]

  # check order of expr_mat
  expect_identical(colnames(exp_sub$expr_mat), c("S2", "S3", "S1"))
  expect_identical(rownames(exp_sub$expr_mat), c("V1", "V3", "V2"))
  # check order of sample_info
  expect_identical(exp_sub$sample_info$sample, c("S2", "S3", "S1"))
  # check order of var_info
  expect_identical(exp_sub$var_info$variable, c("V1", "V3", "V2"))
})


test_that("subsetting with j omitted raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(exp[1], error = TRUE)
})


test_that("[<- does not work", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(exp[1, 1] <- 100, error = TRUE)
})
