for (class_name in c("GlycomicSE", "GlycoproteomicSE")) {
  test_that(paste("metadata setters update and validate", class_name), {
    exp <- get(class_name)(
      matrix(1),
      rowData = S4Vectors::DataFrame(
        glycan_composition = glyrepr::glycan_composition(c(Hex = 1)),
        protein = "P1",
        protein_site = 1L
      ),
      metadata = list(glycan_type = "N", source = "test")
    )
    withr::local_options(lifecycle_verbosity = "error")
    for (scale in c("raw", "log", "log2", "log10", "logx")) {
      result <- set_scale(exp, scale)
      expect_identical(get_scale(result), scale)
      expect_identical(class(result), class(exp))
      expect_identical(
        SummarizedExperiment::assay(result),
        SummarizedExperiment::assay(exp)
      )
      expect_identical(S4Vectors::metadata(result)$source, "test")
      expect_identical(get_glycan_type(result), "N")
    }
    for (type in c(.valid_glycan_types(), "others")) {
      result <- set_glycan_type(exp, type)
      expect_identical(get_glycan_type(result), type)
      expect_identical(class(result), class(exp))
      expect_identical(
        SummarizedExperiment::rowData(result),
        SummarizedExperiment::rowData(exp)
      )
      expect_identical(
        SummarizedExperiment::assay(result),
        SummarizedExperiment::assay(exp)
      )
      expect_identical(S4Vectors::metadata(result)$source, "test")
      expect_identical(get_scale(result), "raw")
    }
    expect_identical(get_scale(exp), "raw")
    expect_identical(get_glycan_type(exp), "N")
    for (bad in list(
      NULL,
      NA_character_,
      character(),
      c("N", "O"),
      "invalid",
      1
    )) {
      expect_error(set_glycan_type(exp, bad))
      expect_error(set_scale(exp, bad))
    }
    logged <- set_scale(exp, "log2")
    SummarizedExperiment::assay(logged)[] <- -1
    expect_error(set_scale(logged, "raw"), "non-negative")
    expect_identical(get_scale(logged), "log2")
    expect_identical(get_scale(set_scale(logged, "logx")), "logx")
    S4Vectors::metadata(exp)$scale <- NULL
    expect_identical(get_scale(set_scale(exp, "raw")), "raw")
  })
}

test_that("metadata setters reject unsupported inputs", {
  for (exp in list(
    NULL,
    list(),
    SummarizedExperiment::SummarizedExperiment()
  )) {
    expect_error(set_scale(exp, "raw"), "GlycomicSE")
    expect_error(set_glycan_type(exp, "N"), "GlycomicSE")
  }
  expect_error(set_scale(create_test_exp("S1", "V1"), "raw"), "GlycomicSE")
})

test_that("set_glycan_type preserves deprecated legacy support", {
  exp <- create_test_exp("S1", "V1")
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    result <- set_glycan_type(exp, "O-GalNAc"),
    class = "lifecycle_warning_deprecated"
  )
  expect_identical(result$meta_data$glycan_type, "O-GalNAc")
  expect_identical(class(result), class(exp))
})
