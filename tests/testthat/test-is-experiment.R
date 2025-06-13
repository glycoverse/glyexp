test_that("is_experiment() works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))
  expect_true(is_experiment(exp))
  expect_false(is_experiment(data.frame()))
})
