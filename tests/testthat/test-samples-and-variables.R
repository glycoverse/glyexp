test_that("getting samples and variables works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_equal(samples(exp), c("S1", "S2", "S3"))
  expect_equal(variables(exp), c("V1", "V2", "V3"))
})


test_that("getting number of samples and variables works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_equal(n_samples(exp), 3)
  expect_equal(n_variables(exp), 3)
})
