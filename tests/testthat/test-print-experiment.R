test_that("print works", {
  exp <- create_test_exp(c(1, 2, 3), c(4, 5, 6))

  expect_snapshot(print(exp))
})
