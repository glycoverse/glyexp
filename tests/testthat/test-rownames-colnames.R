test_that("`rownames()` and `colnames()` works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_identical(rownames(exp), c("V1", "V2", "V3"))
  expect_identical(colnames(exp), c("S1", "S2", "S3"))
})


test_that("`dimnames()` works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  expect_identical(dimnames(exp), list(c("V1", "V2", "V3"), c("S1", "S2", "S3")))
})
