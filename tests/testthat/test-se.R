test_that("as_se converts experiment to SummarizedExperiment correctly", {
  exp <- toy_experiment
  se <- as_se(exp)

  # Check class
  expect_s4_class(se, "SummarizedExperiment")

  # Check dimensions
  expect_equal(dim(se), dim(exp))

  # Check that assay data is preserved
  expect_equal(SummarizedExperiment::assay(se, 1), exp$expr_mat)

  # Check that sample info is preserved in colData
  expected_coldata <- as.data.frame(exp$sample_info)
  rownames(expected_coldata) <- expected_coldata$sample
  expected_coldata$sample <- NULL
  expect_equal(
    tibble::as_tibble(SummarizedExperiment::colData(se)),
    tibble::as_tibble(expected_coldata)
  )

  # Check that variable info is preserved in rowData
  expected_rowdata <- as.data.frame(exp$var_info)
  rownames(expected_rowdata) <- expected_rowdata$variable
  expected_rowdata$variable <- NULL
  expect_equal(
    tibble::as_tibble(SummarizedExperiment::rowData(se)),
    tibble::as_tibble(expected_rowdata)
  )

  # Check that metadata is preserved
  expect_equal(S4Vectors::metadata(se), exp$meta_data)

  # Check default assay name
  expect_equal(names(SummarizedExperiment::assays(se)), "counts")
})

test_that("as_se works with custom assay name", {
  exp <- toy_experiment
  se <- as_se(exp, assay_name = "intensity")

  expect_equal(names(SummarizedExperiment::assays(se)), "intensity")
})

test_that("as_se validates input", {
  # Test with non-experiment object
  expect_error(as_se("not_an_experiment"), class = "simpleError")

  # Test with invalid assay name
  expect_error(as_se(toy_experiment, assay_name = 123), class = "simpleError")
})

test_that("from_se converts SummarizedExperiment to experiment correctly", {
  exp <- toy_experiment
  se <- as_se(exp)
  exp_back <- from_se(
    se,
    exp_type = exp$meta_data$exp_type,
    glycan_type = exp$meta_data$glycan_type
  )

  # Check class
  expect_equal(class(exp_back), "glyexp_experiment")

  # Check dimensions
  expect_equal(dim(exp_back), dim(exp))

  # Check that expression matrix is preserved
  expect_equal(exp_back$expr_mat, exp$expr_mat)

  # Check that sample info is preserved
  expect_equal(exp_back$sample_info, exp$sample_info)

  # Check that variable info is preserved
  expect_equal(exp_back$var_info, exp$var_info)

  # Check metadata
  expect_equal(exp_back$meta_data$exp_type, exp$meta_data$exp_type)
  expect_equal(exp_back$meta_data$glycan_type, exp$meta_data$glycan_type)
})

test_that("from_se works with metadata defaults", {
  # Create SE with metadata containing exp_type and glycan_type
  exp <- toy_experiment
  se <- as_se(exp)

  # Convert back without specifying exp_type and glycan_type
  exp_back <- from_se(se)

  expect_equal(exp_back$meta_data$exp_type, exp$meta_data$exp_type)
  expect_equal(exp_back$meta_data$glycan_type, exp$meta_data$glycan_type)
})

test_that("from_se preserves all metadata", {
  exp <- toy_experiment
  exp$meta_data$instrument <- "Orbitrap"
  exp$meta_data$analysis <- list(method = "LC-MS", version = 1)
  se <- as_se(exp)

  exp_back <- from_se(se)

  expect_equal(exp_back$meta_data, exp$meta_data)
})

test_that("from_se accepts trait experiment types from metadata", {
  expr_mat <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(c("V1", "V2"), c("S1", "S2"))
  )
  sample_info <- tibble::tibble(sample = c("S1", "S2"))
  cases <- list(
    traitomics = tibble::tibble(
      variable = c("V1", "V2"),
      trait = c("motif_a", "motif_b")
    ),
    traitproteomics = tibble::tibble(
      variable = c("V1", "V2"),
      protein = c("P1", "P2"),
      protein_site = c(10L, 20L),
      trait = c("motif_a", "motif_b")
    )
  )

  purrr::iwalk(cases, function(var_info, exp_type) {
    exp <- experiment(
      expr_mat,
      sample_info,
      var_info,
      exp_type = exp_type,
      glycan_type = "N"
    )
    exp_back <- from_se(as_se(exp))

    expect_equal(exp_back$meta_data$exp_type, exp_type)
    expect_equal(exp_back$var_info, exp$var_info)
  })
})

test_that("from_se works with empty metadata", {
  # Create a simple SummarizedExperiment without metadata
  expr_mat <- matrix(runif(12), nrow = 3, ncol = 4)
  colnames(expr_mat) <- paste0("S", 1:4)
  rownames(expr_mat) <- paste0("V", 1:3)

  coldata <- data.frame(
    group = factor(c("A", "A", "B", "B")),
    row.names = colnames(expr_mat)
  )

  rowdata <- data.frame(
    protein = paste0("P", 1:3),
    row.names = rownames(expr_mat)
  )

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = expr_mat),
    colData = coldata,
    rowData = rowdata
  )

  # Convert to experiment with explicit parameters
  exp <- from_se(se, exp_type = "others", glycan_type = "O-GalNAc")

  expect_equal(class(exp), "glyexp_experiment")
  expect_equal(exp$meta_data$exp_type, "others")
  expect_equal(exp$meta_data$glycan_type, "O-GalNAc")
})

test_that("from_se requires exp_type when metadata does not provide it", {
  se <- as_se(toy_experiment)
  S4Vectors::metadata(se) <- list(glycan_type = "N")

  expect_error(
    from_se(se),
    "Provide it through the `exp_type` argument"
  )
})

test_that("from_se requires glycan_type when metadata does not provide it", {
  se <- as_se(toy_experiment)
  S4Vectors::metadata(se) <- list(exp_type = "glycomics")

  expect_error(
    from_se(se),
    "Provide it through the `glycan_type` argument"
  )
})

test_that("from_se allows missing glycan_type for others experiments", {
  expr_mat <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(c("V1", "V2"), c("S1", "S2"))
  )
  exp <- experiment(expr_mat)
  se <- as_se(exp)

  exp_back <- from_se(se)

  expect_equal(exp_back$meta_data$exp_type, "others")
  expect_null(exp_back$meta_data$glycan_type)
})

test_that("from_se works with custom assay name", {
  exp <- toy_experiment
  se <- as_se(exp, assay_name = "intensity")
  exp_back <- from_se(
    se,
    assay_name = "intensity",
    exp_type = exp$meta_data$exp_type,
    glycan_type = exp$meta_data$glycan_type
  )

  expect_equal(exp_back$expr_mat, exp$expr_mat)
})

test_that("from_se validates input", {
  # Test with non-SummarizedExperiment object
  expect_error(from_se("not_a_se"), class = "simpleError")

  # Test with invalid exp_type
  expect_error(
    from_se(as_se(toy_experiment), exp_type = "invalid"),
    class = "simpleError"
  )

  # Test with invalid glycan_type
  expect_error(
    from_se(as_se(toy_experiment), glycan_type = "invalid"),
    class = "simpleError"
  )
})

test_that("round-trip conversion preserves data integrity", {
  exp <- toy_experiment

  # Forward and backward conversion
  se <- as_se(exp)
  exp_back <- from_se(
    se,
    exp_type = exp$meta_data$exp_type,
    glycan_type = exp$meta_data$glycan_type
  )

  # Check that all components are identical
  expect_equal(exp_back$expr_mat, exp$expr_mat)
  expect_equal(exp_back$sample_info, exp$sample_info)
  expect_equal(exp_back$var_info, exp$var_info)
  expect_equal(exp_back$meta_data$exp_type, exp$meta_data$exp_type)
  expect_equal(exp_back$meta_data$glycan_type, exp$meta_data$glycan_type)
})
