test_that("dim works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3", "V4"))

  expect_equal(dim(exp), c(4, 3))
})


test_that("dim<- does not work", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3", "V4"))

  expect_snapshot(dim(exp) <- c(3, 3), error = TRUE)
})
