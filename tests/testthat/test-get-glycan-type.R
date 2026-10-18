for (class_name in c("GlycomicSE", "GlycoproteomicSE")) {
  test_that(paste("get_glycan_type reads metadata from", class_name), {
    withr::local_options(lifecycle_verbosity = "error")
    for (glycan_type in c(.valid_glycan_types(), "others")) {
      exp <- get(class_name)(
        matrix(1),
        rowData = S4Vectors::DataFrame(
          glycan_composition = glyrepr::glycan_composition(c(Hex = 1)),
          protein = "P1",
          protein_site = 1L
        ),
        metadata = list(glycan_type = glycan_type)
      )

      expect_identical(get_glycan_type(exp), glycan_type)
    }

    S4Vectors::metadata(exp) <- list(glycan_type_other = "N")
    expect_null(get_glycan_type(exp))
  })
}

test_that("get_glycan_type preserves legacy experiment support", {
  exp <- create_test_exp("S1", "V1", glycan_type = "O-GalNAc")
  expect_identical(get_glycan_type(exp = exp), "O-GalNAc")

  exp$meta_data$glycan_type <- NULL
  expect_null(get_glycan_type(exp))
})

test_that("get_glycan_type warns for legacy experiments", {
  exp <- create_test_exp("S1", "V1")
  withr::local_options(lifecycle_verbosity = "warning")

  expect_snapshot(get_glycan_type(exp))
})

test_that("get_glycan_type rejects unsupported inputs", {
  expect_snapshot(error = TRUE, get_glycan_type(NULL))
  expect_snapshot(
    error = TRUE,
    get_glycan_type(SummarizedExperiment::SummarizedExperiment())
  )
})
