test_that("is_experiment() works", {
  exp <- create_test_exp(c(1, 2, 3), c(4, 5, 6))
  expect_true(is_experiment(exp))
  expect_false(is_experiment(data.frame()))
})
