test_that("creating a toy experiment works", {
  exp <- toy_experiment

  expect_equal(class(exp), "glyexp_experiment")
})
