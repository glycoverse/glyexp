test_that("mutating sample info works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_obs(exp, new_col = 1:3)

  expect_equal(new_exp$sample_info$new_col, 1:3)
})


test_that("mutating variable info works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_var(exp, new_col = 1:3)

  expect_equal(new_exp$var_info$new_col, 1:3)
})


test_that("mutating using non-existing columns raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(mutate_obs(exp, new_col = bad_col), error = TRUE)
  expect_snapshot(mutate_var(exp, new_col = bad_col), error = TRUE)
})


test_that("trying to mutate 'sample' with duplicated values raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(mutate_obs(exp, sample = 1), error = TRUE)
})


test_that("trying to mutate 'variable' with duplicated values raises an error", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_snapshot(mutate_var(exp, variable = 1), error = TRUE)
})


test_that("mutating 'sample' updates 'expr_mat' correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_obs(exp, sample = c("A", "B", "C"))

  expect_equal(colnames(new_exp$expr_mat), c("A", "B", "C"))
})


test_that("mutating 'variable' updates 'expr_mat' correctly", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  new_exp <- mutate_var(exp, variable = c("A", "B", "C"))

  expect_equal(rownames(new_exp$expr_mat), c("A", "B", "C"))
})


test_that("other items in list are preserved", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  exp$something <- "haha"

  exp2 <- mutate_var(exp)

  expect_equal(exp2$something, "haha")
})
