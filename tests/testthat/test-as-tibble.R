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
    expect_identical(tb$.sample, c("S2", "S1", "S2", "S1"))
    expect_identical(tb$.variable, c("G2", "G2", "G1", "G1"))
    expect_identical(tb$value, c(1, 3, NA_real_, 4))
    expect_identical(tb$group, factor(c("case", "control", "case", "control")))
    expect_identical(
      tb$glycan_composition,
      rows$glycan_composition[c(1, 1, 2, 2)]
    )
    expect_equal(tb$tags, rows$tags[c(1, 1, 2, 2)])
    expect_named(
      tibble::as_tibble(x, sample_cols = NULL, var_cols = NULL),
      c(".sample", ".variable", "value")
    )
    expect_named(
      tibble::as_tibble(x, var_cols = tidyselect::starts_with("protein")),
      c(".sample", "group", ".variable", "protein", "protein_site", "value")
    )
    expect_identical(nrow(tibble::as_tibble(x[integer(), ])), 0L)
    expect_identical(nrow(tibble::as_tibble(x[, integer()])), 0L)
    expect_error(colnames(x) <- c("same", "same"), "unique")
    expect_identical(nrow(tibble::as_tibble(x)), 4L)
    expect_error(colnames(x) <- NULL, "one name per position")
    expect_error(rownames(x) <- NULL, "one name per position")
    SummarizedExperiment::colData(x)$value <- c("a", "b")
    SummarizedExperiment::rowData(x)$value <- c("c", "d")
    collision <- tibble::as_tibble(x)
    expect_identical(collision$value, c(1, 3, NA_real_, 4))
    expect_identical(collision$value.1, c("a", "b", "a", "b"))
    expect_identical(collision$value.2, c("c", "c", "d", "d"))
  })
}

for (class_name in c("GlycomicSE", "GlycoproteomicSE")) {
  test_that(paste("as_tibble protects internal names for", class_name), {
    abundance <- matrix(
      c(1, NA, 3, 4, 5, 6),
      nrow = 2,
      dimnames = list(c("G2", "G1"), c("S3", "S1", "S2"))
    )
    rows <- S4Vectors::DataFrame(
      protein = c("P2", "P1"),
      protein_site = c(20L, 10L),
      glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2)
    )
    x <- get(class_name)(
      abundance,
      rowData = rows,
      colData = S4Vectors::DataFrame(
        group = factor(c("case", "control", "case"))
      ),
      metadata = list(glycan_type = "N")
    )
    annotation_names <- c(
      "sample_ids",
      "variable_ids",
      "sample_index",
      "variable_index",
      "x",
      "samples",
      "variables",
      "annotation_names",
      "columns",
      "sample_cols",
      "var_cols",
      ".sample",
      ".variable",
      "value",
      "sample",
      "variable"
    )
    for (name in annotation_names) {
      SummarizedExperiment::colData(x)[[name]] <- c("s-a", "s-b", "s-c")
      SummarizedExperiment::rowData(x)[[name]] <- c("v-a", "v-b")
    }
    before <- x
    sample_index <- rep(seq_len(ncol(x)), times = nrow(x))
    variable_index <- rep(seq_len(nrow(x)), each = ncol(x))
    expected_values <- as.vector(t(abundance))
    expected_names <- make.unique(c(
      ".sample",
      ".variable",
      "value",
      names(SummarizedExperiment::colData(x)),
      names(SummarizedExperiment::rowData(x))
    ))[-seq_len(3)]
    tb <- tibble::as_tibble(x)
    expect_identical(tb$.sample, colnames(x)[sample_index])
    expect_identical(tb$.variable, rownames(x)[variable_index])
    expect_identical(tb$value, expected_values)
    sample_names <- head(expected_names, ncol(SummarizedExperiment::colData(x)))
    variable_names <- tail(
      expected_names,
      ncol(SummarizedExperiment::rowData(x))
    )
    for (i in seq_along(annotation_names)) {
      name <- annotation_names[[i]]
      sample_position <- match(name, names(SummarizedExperiment::colData(x)))
      variable_position <- match(name, names(SummarizedExperiment::rowData(x)))
      expect_identical(
        tb[[sample_names[[sample_position]]]],
        c("s-a", "s-b", "s-c")[sample_index]
      )
      expect_identical(
        tb[[variable_names[[variable_position]]]],
        c("v-a", "v-b")[variable_index]
      )
    }
    selected <- tibble::as_tibble(x, sample_cols = variable_ids, var_cols = x)
    expect_named(
      selected,
      c(".sample", "variable_ids", ".variable", "x", "value")
    )
    expect_identical(selected$.variable, rownames(x)[variable_index])
    expect_identical(selected$value, expected_values)
    expect_identical(nrow(tibble::as_tibble(x[integer(), ])), 0L)
    expect_identical(nrow(tibble::as_tibble(x[, integer()])), 0L)
    expect_identical(x, before)
  })
}
