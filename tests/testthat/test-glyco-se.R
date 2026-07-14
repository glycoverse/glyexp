glycomic_abundance <- function() {
  matrix(
    c(1, 2, 3, 4),
    nrow = 2,
    dimnames = list(c("G1", "G2"), c("S1", "S2"))
  )
}

glycomic_row_data <- function() {
  S4Vectors::DataFrame(
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2),
    row.names = c("G1", "G2")
  )
}

glycoproteomic_row_data <- function() {
  S4Vectors::DataFrame(
    protein = c("P1", "P2"),
    protein_site = c(10L, 20L),
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2),
    row.names = c("G1", "G2")
  )
}

test_that("GlycomicSE show method summarizes the container", {
  se <- GlycomicSE(
    glycomic_abundance(),
    colData = S4Vectors::DataFrame(
      group = c("control", "case"),
      row.names = c("S1", "S2")
    ),
    rowData = glycomic_row_data(),
    metadata = list(glycan_type = "N")
  )

  expect_snapshot(show(se))
})

test_that("GlycoproteomicSE show method summarizes the container", {
  se <- GlycoproteomicSE(
    glycomic_abundance(),
    rowData = glycoproteomic_row_data(),
    metadata = list(glycan_type = "N")
  )

  expect_snapshot(show(se))
})

test_that("GlycomicSE creates a valid SummarizedExperiment subclass", {
  abundance <- glycomic_abundance()
  col_data <- S4Vectors::DataFrame(
    group = c("control", "case"),
    row.names = c("S1", "S2")
  )
  row_data <- glycomic_row_data()

  se <- GlycomicSE(
    abundance,
    colData = col_data,
    rowData = row_data,
    metadata = list(glycan_type = "N")
  )

  expect_s4_class(se, "GlycomicSE")
  expect_true(methods::is(se, "SummarizedExperiment"))
  expect_true(validObject(se))
  expect_equal(names(SummarizedExperiment::assays(se)), "abundance")
  expect_identical(SummarizedExperiment::assay(se, "abundance"), abundance)
  expect_equal(SummarizedExperiment::colData(se), col_data)
  expect_equal(SummarizedExperiment::rowData(se), row_data)
  expect_equal(S4Vectors::metadata(se), list(glycan_type = "N"))
})

test_that("GlycomicSE requires exactly one non-negative abundance assay", {
  abundance <- glycomic_abundance()
  row_data <- glycomic_row_data()

  expect_error(
    GlycomicSE(
      -abundance,
      rowData = row_data,
      metadata = list(glycan_type = "N")
    ),
    "non-negative"
  )

  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = abundance, normalized = abundance),
    rowData = row_data,
    metadata = list(glycan_type = "N")
  )

  expect_error(.GlycomicSE(se), "only one assay")
})

test_that("GlycomicSE validates glycan_type metadata", {
  abundance <- glycomic_abundance()
  row_data <- glycomic_row_data()
  valid_glycan_types <- c(
    "N",
    "O",
    "O-GalNAc",
    "O-Man",
    "O-Fuc",
    "O-GlcNAc",
    "HMO",
    "GSL",
    "GAG",
    "GPI"
  )

  purrr::walk(valid_glycan_types, \(glycan_type) {
    se <- GlycomicSE(
      abundance,
      rowData = row_data,
      metadata = list(glycan_type = glycan_type)
    )
    expect_equal(S4Vectors::metadata(se)$glycan_type, glycan_type)
  })

  expect_error(
    GlycomicSE(abundance, rowData = row_data),
    "glycan_type"
  )
  expect_error(
    GlycomicSE(
      abundance,
      rowData = row_data,
      metadata = list(glycan_type = "invalid")
    ),
    "unknown glycan type"
  )
})

test_that("GlycomicSE validates glycomics rowData", {
  abundance <- glycomic_abundance()
  metadata <- list(glycan_type = "N")

  expect_error(
    GlycomicSE(
      abundance,
      rowData = S4Vectors::DataFrame(label = c("G1", "G2")),
      metadata = metadata
    ),
    "glycan_composition"
  )
  expect_error(
    GlycomicSE(
      abundance,
      rowData = S4Vectors::DataFrame(
        glycan_composition = c("Hex(1)", "Hex(1)")
      ),
      metadata = metadata
    ),
    "glyrepr::glycan_composition"
  )
})

test_that("GlycomicSE validates optional glycan structure rowData column", {
  abundance <- glycomic_abundance()
  metadata <- list(glycan_type = "N")
  row_data <- glycomic_row_data()

  row_data$glycan_structure <- c("Man", "Man")
  expect_error(
    GlycomicSE(abundance, rowData = row_data, metadata = metadata),
    "glyrepr::glycan_structure"
  )
})

test_that("as_glycomic_se coerces supported inputs", {
  abundance <- glycomic_abundance()
  row_data <- glycomic_row_data()
  metadata <- list(glycan_type = "N")
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = abundance),
    rowData = row_data,
    metadata = metadata
  )

  glycomic_se <- as_glycomic_se(se)
  expect_s4_class(glycomic_se, "GlycomicSE")
  expect_identical(SummarizedExperiment::assay(glycomic_se), abundance)
  expect_equal(SummarizedExperiment::rowData(glycomic_se), row_data)
  expect_equal(S4Vectors::metadata(glycomic_se), metadata)

  expect_identical(as_glycomic_se(glycomic_se), glycomic_se)

  exp <- experiment(
    abundance,
    var_info = tibble::tibble(
      variable = rownames(abundance),
      glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2)
    ),
    exp_type = "glycomics",
    glycan_type = "N"
  )
  expect_s4_class(as_glycomic_se(exp), "GlycomicSE")
})

test_that("as_glycomic_se validates input schema", {
  abundance <- glycomic_abundance()
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = abundance),
    rowData = S4Vectors::DataFrame(label = c("G1", "G2")),
    metadata = list(glycan_type = "N")
  )

  expect_error(as_glycomic_se(se), "glycan_composition")
  expect_error(
    as_glycomic_se("not a SummarizedExperiment"),
    class = "simpleError"
  )
})

test_that("is_glycomic_se identifies GlycomicSE objects", {
  se <- GlycomicSE(
    glycomic_abundance(),
    rowData = glycomic_row_data(),
    metadata = list(glycan_type = "N")
  )

  expect_true(is_glycomic_se(se))
  expect_false(is_glycomic_se("not a SummarizedExperiment"))
})

test_that("GlycoproteomicSE creates a valid SummarizedExperiment subclass", {
  abundance <- glycomic_abundance()
  row_data <- glycoproteomic_row_data()

  se <- GlycoproteomicSE(
    abundance,
    rowData = row_data,
    metadata = list(glycan_type = "N")
  )

  expect_s4_class(se, "GlycoproteomicSE")
  expect_true(methods::is(se, "SummarizedExperiment"))
  expect_true(validObject(se))
  expect_equal(names(SummarizedExperiment::assays(se)), "abundance")
  expect_identical(SummarizedExperiment::assay(se, "abundance"), abundance)
  expect_equal(SummarizedExperiment::rowData(se), row_data)
  expect_equal(S4Vectors::metadata(se), list(glycan_type = "N"))
})

test_that("GlycoproteomicSE requires glycosite rowData columns", {
  abundance <- glycomic_abundance()

  expect_error(
    GlycoproteomicSE(
      abundance,
      rowData = glycomic_row_data(),
      metadata = list(glycan_type = "N")
    ),
    "protein"
  )
})

test_that("GlycoproteomicSE validates glycan and glycosite rowData columns", {
  abundance <- glycomic_abundance()
  metadata <- list(glycan_type = "N")

  row_data <- glycoproteomic_row_data()
  row_data$protein <- c(1, 2)
  expect_error(
    GlycoproteomicSE(abundance, rowData = row_data, metadata = metadata),
    "@rowData\\$protein"
  )

  row_data <- glycoproteomic_row_data()
  row_data$protein_site <- c("10", "20")
  expect_error(
    GlycoproteomicSE(abundance, rowData = row_data, metadata = metadata),
    "@rowData\\$protein_site"
  )

  row_data <- glycoproteomic_row_data()
  row_data$glycan_composition <- c("Hex(1)", "Hex(1)")
  expect_error(
    GlycoproteomicSE(abundance, rowData = row_data, metadata = metadata),
    "glyrepr::glycan_composition"
  )
})

test_that("as_glycoproteomic_se coerces supported inputs", {
  abundance <- glycomic_abundance()
  row_data <- glycoproteomic_row_data()
  metadata <- list(glycan_type = "N")
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = abundance),
    rowData = row_data,
    metadata = metadata
  )

  glycoproteomic_se <- as_glycoproteomic_se(se)
  expect_s4_class(glycoproteomic_se, "GlycoproteomicSE")
  expect_identical(SummarizedExperiment::assay(glycoproteomic_se), abundance)
  expect_equal(SummarizedExperiment::rowData(glycoproteomic_se), row_data)
  expect_equal(S4Vectors::metadata(glycoproteomic_se), metadata)

  expect_identical(
    as_glycoproteomic_se(glycoproteomic_se),
    glycoproteomic_se
  )

  exp <- experiment(
    abundance,
    var_info = tibble::tibble(
      variable = rownames(abundance),
      protein = c("P1", "P2"),
      protein_site = c(10L, 20L),
      glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 2)
    ),
    exp_type = "glycoproteomics",
    glycan_type = "N"
  )
  expect_s4_class(as_glycoproteomic_se(exp), "GlycoproteomicSE")
})

test_that("as_glycoproteomic_se validates input schema", {
  abundance <- glycomic_abundance()
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = abundance),
    rowData = glycomic_row_data(),
    metadata = list(glycan_type = "N")
  )

  expect_error(as_glycoproteomic_se(se), "protein")
  expect_error(
    as_glycoproteomic_se("not a SummarizedExperiment"),
    class = "simpleError"
  )
})

test_that("is_glycoproteomic_se identifies GlycoproteomicSE objects", {
  se <- GlycoproteomicSE(
    glycomic_abundance(),
    rowData = glycoproteomic_row_data(),
    metadata = list(glycan_type = "N")
  )

  expect_true(is_glycoproteomic_se(se))
  expect_false(is_glycoproteomic_se("not a SummarizedExperiment"))
})
