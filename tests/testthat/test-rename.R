test_that("renaming works", {
  exp <- create_test_exp(c(1, 2, 3), c(4, 5, 6))

  new_exp <- rename(exp, "new_name")

  expect_equal(new_exp$name, "new_name")
  expect_equal(exp$name, "test_exp")
})
