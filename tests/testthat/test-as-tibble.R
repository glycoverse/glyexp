test_that("as_tibble works", {
  exp <- create_test_exp(c("S1", "S2", "S3"), c("V1", "V2", "V3"))

  tb <- tibble::as_tibble(exp)

  expected_tb <- tibble::tibble(
    sample = rep(c("S1", "S2", "S3"), each = 3),
    group = factor(rep("A", 9)),
    variable = rep(c("V1", "V2", "V3"), 3),
    type = rep("B", 9),
    value = c(1:9)
  )
  expect_equal(dplyr::arrange(tb, value), expected_tb)
})


test_that("`sample_cols` and `variable_cols` works", {
  exp <- create_test_exp_2()

  tb <- tibble::as_tibble(exp, sample_cols = col1, var_cols = col2)

  expect_identical(
    colnames(tb),
    c("sample", "col1", "variable", "col2", "value")
  )
})

for (class_name in c("GlycomicSE", "GlycoproteomicSE")) {
  test_that(paste("as_tibble supports", class_name), {
    abundance <- matrix(
      c(1, NA, 3, 4),
      2,
      dimnames = list(c("G2", "G1"), c("S2", "S1"))
    )
    rows <- S4Vectors::DataFrame(
      protein = c("P2", "P1"),
      protein_site = c(20L, 10L),
      glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2),
      tags = I(list(c("a", "b"), "c"))
    )
    x <- get(class_name)(
      abundance,
      rowData = rows,
      colData = S4Vectors::DataFrame(group = factor(c("case", "control"))),
      metadata = list(glycan_type = "N")
    )
    tb <- tibble::as_tibble(x)
    expect_identical(tb$sample, c("S2", "S1", "S2", "S1"))
    expect_identical(tb$variable, c("G2", "G2", "G1", "G1"))
    expect_identical(tb$value, c(1, 3, NA_real_, 4))
    expect_identical(tb$group, factor(c("case", "control", "case", "control")))
    expect_identical(
      tb$glycan_composition,
      rows$glycan_composition[c(1, 1, 2, 2)]
    )
    expect_equal(tb$tags, rows$tags[c(1, 1, 2, 2)])
    expect_named(
      tibble::as_tibble(x, sample_cols = NULL, var_cols = NULL),
      c("sample", "variable", "value")
    )
    expect_named(
      tibble::as_tibble(x, var_cols = tidyselect::starts_with("protein")),
      c("sample", "group", "variable", "protein", "protein_site", "value")
    )
    expect_identical(nrow(tibble::as_tibble(x[integer(), ])), 0L)
    expect_identical(nrow(tibble::as_tibble(x[, integer()])), 0L)
    colnames(x) <- c("same", "same")
    expect_identical(nrow(tibble::as_tibble(x)), 4L)
    colnames(x) <- NULL
    rownames(x) <- NULL
    expect_identical(tibble::as_tibble(x)$sample, rep(NA_character_, 4))
    expect_identical(tibble::as_tibble(x)$variable, rep(NA_character_, 4))
    SummarizedExperiment::colData(x)$value <- c("a", "b")
    SummarizedExperiment::rowData(x)$value <- c("c", "d")
    collision <- tibble::as_tibble(x)
    expect_identical(collision$value, c(1, 3, NA_real_, 4))
    expect_identical(collision$value.1, c("a", "b", "a", "b"))
    expect_identical(collision$value.2, c("c", "c", "d", "d"))
  })
}
