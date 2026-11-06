test_that("identification summaries are independent of duplicated SE identifiers", {
  mat <- matrix(c(1, NA, 2, NA, 4, NA, 1, 4, NA), nrow = 3)
  info <- S4Vectors::DataFrame(
    glycan_composition = glyrepr::as_glycan_composition(c("H3N2", "H4N2", "H5N2"))
  )
  for (feature_names in list(NULL, rep("same", 3), paste0("V", 1:3))) {
    for (sample_names in list(NULL, rep("same", 3), paste0("S", 1:3))) {
      dimnames(mat) <- list(feature_names, sample_names)
      exp <- GlycomicSE(mat, rowData = info, metadata = list(glycan_type = "N"))
      before <- SummarizedExperiment::assay(exp)
      result <- summarize_experiment(exp)
      expect_equal(result$n[result$item == "total_composition"], 3)
      expect_equal(result$n[result$item == "composition_per_sample"], 5 / 3)
      expect_identical(SummarizedExperiment::assay(exp), before)
    }
  }
})

test_that("position-based summaries retain zero counts for entirely missing samples", {
  mat <- matrix(c(1, 2, NA, NA), nrow = 2)
  dimnames(mat) <- list(c("same", "same"), c("same", "same"))
  exp <- GlycoproteomicSE(
    mat,
    rowData = S4Vectors::DataFrame(
      protein = c("P1", "P2"),
      protein_site = c(1L, 2L),
      glycan_composition = glyrepr::as_glycan_composition(c("H3N2", "H4N2"))
    ),
    metadata = list(glycan_type = "N")
  )
  result <- summarize_experiment(exp)
  for (item in c("composition", "protein", "glycoform", "glycosite")) {
    expect_equal(result$n[result$item == paste0("total_", item)], 2)
    expect_equal(result$n[result$item == paste0(item, "_per_sample")], 1)
  }
})
