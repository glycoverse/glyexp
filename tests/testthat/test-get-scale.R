for (class_name in c("GlycomicSE", "GlycoproteomicSE")) {
  test_that(paste("get_scale reads metadata from", class_name), {
    create <- function(meta) {
      get(class_name)(
        matrix(1),
        rowData = S4Vectors::DataFrame(
          glycan_composition = glyrepr::glycan_composition(c(Hex = 1)),
          protein = "P1",
          protein_site = 1L
        ),
        metadata = meta
      )
    }

    expect_identical(get_scale(create(list(glycan_type = "N"))), "raw")
    for (scale in c("raw", "log", "log2", "log10", "logx")) {
      exp <- create(list(glycan_type = "N", scale = scale))
      expect_identical(get_scale(exp = exp), scale)
    }

    S4Vectors::metadata(exp) <- list(glycan_type = "N", scale_other = "log2")
    expect_null(get_scale(exp))
  })
}

test_that("get_scale rejects unsupported inputs", {
  expect_error(get_scale(NULL), "GlycomicSE")
  expect_error(
    get_scale(SummarizedExperiment::SummarizedExperiment()),
    "GlycomicSE"
  )
  expect_error(get_scale(create_test_exp("S1", "V1")), "GlycomicSE")
})
