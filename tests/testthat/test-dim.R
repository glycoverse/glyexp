test_that("dim works", {
  exp <- create_test_exp(c(1, 2, 3), c(4, 5, 6, 7))

  expect_equal(dim(exp), c(4, 3))
})


test_that("dim<- does not work", {
  exp <- create_test_exp(c(1, 2, 3), c(4, 5, 6, 7))

  expect_snapshot(dim(exp) <- c(3, 3), error = TRUE)
})
