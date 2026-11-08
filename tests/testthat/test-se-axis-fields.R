for (class_name in c("GlycomicSE", "GlycoproteomicSE")) {
  test_that(
    paste(class_name, "keeps axis IDs separate from ordinary metadata"),
    {
      x <- get(class_name)(
        matrix(1:4, 2, dimnames = list(c("V2", "V1"), c("S2", "S1"))),
        rowData = S4Vectors::DataFrame(
          variable = c("annotation-a", "annotation-b"),
          protein = c("P1", "P2"),
          protein_site = c(1L, 2L),
          glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2)
        ),
        colData = S4Vectors::DataFrame(sample = c("sample-a", "sample-b")),
        metadata = list(glycan_type = "N")
      )
      original <- x
      tb <- tibble::as_tibble(x)
      expect_identical(tb$.variable, c("V2", "V2", "V1", "V1"))
      expect_identical(tb$.sample, c("S2", "S1", "S2", "S1"))
      expect_identical(
        tb$variable,
        rep(c("annotation-a", "annotation-b"), each = 2)
      )
      expect_identical(tb$sample, rep(c("sample-a", "sample-b"), 2))
      result <- standardize_variable(x, "{.variable}-{variable}")
      expect_identical(
        rownames(result),
        c("V2-annotation-a", "V1-annotation-b")
      )
      expect_identical(
        SummarizedExperiment::rowData(result)$variable,
        SummarizedExperiment::rowData(x)$variable
      )
      expect_identical(
        rownames(SummarizedExperiment::assay(result, withDimnames = FALSE)),
        rownames(result)
      )
      inputs <- .summarize_experiment_data(x)
      expect_identical(inputs$var_info$.variable, rownames(x))
      expect_identical(
        inputs$var_info$variable,
        c("annotation-a", "annotation-b")
      )
      expect_named(summarize_experiment(x), c("item", "n"))
      expect_identical(x, original)

      SummarizedExperiment::rowData(x)$.variable <- c("custom-a", "custom-b")
      SummarizedExperiment::colData(x)$.sample <- c("custom-s1", "custom-s2")
      collision <- tibble::as_tibble(x)
      expect_identical(collision$.variable, tb$.variable)
      expect_identical(collision$.sample, tb$.sample)
      expect_identical(
        collision$.variable.1,
        rep(c("custom-a", "custom-b"), each = 2)
      )
      expect_identical(collision$.sample.1, rep(c("custom-s1", "custom-s2"), 2))
      expect_error(standardize_variable(x, "{.variable}"), "reserved")
      expect_error(summarize_experiment(x), "reserved")
    }
  )
}
