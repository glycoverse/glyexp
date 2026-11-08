name_fixture <- function(proteomic = FALSE) {
  rd <- S4Vectors::DataFrame(
    glycan_composition = rep(glyrepr::glycan_composition(c(Hex = 1)), 3),
    feature = c("a", "b", "c"),
    row.names = c("g1", "g2", "g3")
  )
  if (proteomic) {
    rd$protein <- c("p1", "p2", "p3")
    rd$protein_site <- 1:3
  }
  list(
    abundance = matrix(1:6, 3, 2, dimnames = list(rownames(rd), c("s1", "s2"))),
    rowData = rd,
    colData = S4Vectors::DataFrame(
      group = c("A", "B"),
      row.names = c("s1", "s2")
    ),
    metadata = list(glycan_type = "N")
  )
}

expect_names_synced <- function(x, rows, cols) {
  expect_identical(rownames(x), rows)
  expect_identical(colnames(x), cols)
  expect_identical(rownames(SummarizedExperiment::rowData(x)), rows)
  expect_identical(rownames(SummarizedExperiment::colData(x)), cols)
  a <- SummarizedExperiment::assay(x, withDimnames = FALSE)
  expect_equal(rownames(a), if (length(rows)) rows else NULL)
  expect_equal(colnames(a), if (length(cols)) cols else NULL)
  expect_true(validObject(x))
}

test_that("four name sources are resolved independently for both axes and classes", {
  for (proteomic in c(FALSE, TRUE)) {
    ctor <- if (proteomic) GlycoproteomicSE else GlycomicSE
    for (row_case in 1:4) {
      for (col_case in 1:4) {
        args <- name_fixture(proteomic)
        if (row_case == 1) {
          args$rowData <- args$rowData[c(3, 1, 2), , drop = FALSE]
        }
        if (col_case == 1) {
          args$colData <- args$colData[2:1, , drop = FALSE]
        }
        if (row_case %in% c(2, 4)) {
          rownames(args$abundance) <- NULL
        }
        if (row_case %in% c(3, 4)) {
          rownames(args$rowData) <- NULL
        }
        if (col_case %in% c(2, 4)) {
          colnames(args$abundance) <- NULL
        }
        if (col_case %in% c(3, 4)) {
          rownames(args$colData) <- NULL
        }
        x <- do.call(ctor, args)
        rows <- if (row_case == 4) paste0("V", 1:3) else paste0("g", 1:3)
        cols <- if (col_case == 4) paste0("S", 1:2) else paste0("s", 1:2)
        expect_names_synced(x, rows, cols)
        expect_equal(unname(SummarizedExperiment::assay(x)), matrix(1:6, 3, 2))
        expect_identical(
          SummarizedExperiment::rowData(x)$feature,
          c("a", "b", "c")
        )
        expect_identical(SummarizedExperiment::colData(x)$group, c("A", "B"))
      }
    }
  }
})

test_that("provided invalid or unmatched names cannot bypass validation", {
  for (proteomic in c(FALSE, TRUE)) {
    ctor <- if (proteomic) GlycoproteomicSE else GlycomicSE
    for (axis in 1:2) {
      for (source in c("matrix", "annotation")) {
        for (kind in c("duplicate", "empty", "missing", "mismatch")) {
          args <- name_fixture(proteomic)
          size <- dim(args$abundance)[axis]
          value <- dimnames(args$abundance)[[axis]]
          value[1] <- switch(
            kind,
            duplicate = value[2],
            empty = "",
            missing = NA_character_,
            mismatch = "other"
          )
          if (source == "matrix") {
            dimnames(args$abundance)[[axis]] <- value
          } else {
            if (kind == "missing") {
              next
            } # DataFrame already rejects NA rownames.
            rownames(args[[c("rowData", "colData")[axis]]]) <- value
          }
          expect_error(do.call(ctor, c(args, list(checkDimnames = FALSE))))
        }
      }
    }
  }
})

test_that("name replacements synchronize without moving data or annotations", {
  for (proteomic in c(FALSE, TRUE)) {
    ctor <- if (proteomic) GlycoproteomicSE else GlycomicSE
    x <- do.call(ctor, name_fixture(proteomic))
    before <- SummarizedExperiment::assay(x)
    rownames(x) <- c("z", "y", "x")
    colnames(x) <- c("second", "first")
    expect_names_synced(x, c("z", "y", "x"), c("second", "first"))
    expect_identical(unname(SummarizedExperiment::assay(x)), unname(before))
    expect_identical(SummarizedExperiment::rowData(x)$feature, c("a", "b", "c"))
    expect_identical(SummarizedExperiment::colData(x)$group, c("A", "B"))
    dimnames(x) <- list(c("a", "b", "c"), c("d", "e"))
    expect_names_synced(x, c("a", "b", "c"), c("d", "e"))
    for (bad in list(
      NULL,
      c("a", "a", "c"),
      c("a", "", "c"),
      c("a", NA, "c"),
      "short"
    )) {
      expect_error(rownames(x) <- bad)
    }
    for (bad in list(NULL, c("a", "a"), c("a", ""), c("a", NA), "short")) {
      expect_error(colnames(x) <- bad)
    }
    expect_error(dimnames(x) <- NULL)
    expect_error(dimnames(x) <- list(c("a", "b", "c"), NULL))
    expect_names_synced(x, c("a", "b", "c"), c("d", "e"))
  }
})

test_that("conversions normalize older SE objects using authoritative names", {
  for (proteomic in c(FALSE, TRUE)) {
    args <- name_fixture(proteomic)
    convert <- if (proteomic) as_glycoproteomic_se else as_glycomic_se
    dimnames(args$abundance) <- NULL
    rownames(args$rowData) <- NULL
    rownames(args$colData) <- NULL
    x <- SummarizedExperiment::SummarizedExperiment(
      assays = list(abundance = args$abundance),
      rowData = args$rowData,
      colData = args$colData,
      metadata = args$metadata
    )
    y <- convert(x)
    expect_names_synced(y, paste0("V", 1:3), paste0("S", 1:2))
    expect_null(rownames(x))
    # Plain SE name replacement changes top-level names but not raw assay names.
    dimnames(x) <- list(paste0("g", 1:3), paste0("s", 1:2))
    y <- convert(x)
    expect_names_synced(y, paste0("g", 1:3), paste0("s", 1:2))
    expect_identical(convert(y), y)
    rownames(x) <- c("g", "g", "h")
    expect_error(convert(x), "unique")
  }
})

test_that("validity detects invalid top-level and raw assay names", {
  x <- do.call(GlycomicSE, name_fixture())
  a <- SummarizedExperiment::assay(x, withDimnames = FALSE)
  rownames(a) <- c("wrong", "g2", "g3")
  expect_error(
    SummarizedExperiment::assay(x, withDimnames = FALSE) <- a,
    "must match"
  )
  # Simulate a persisted pre-contract object via the public parent method.
  setter <- methods::getMethod(
    "assays<-",
    c("SummarizedExperiment", "SimpleList")
  )
  x <- setter(
    x,
    withDimnames = FALSE,
    value = S4Vectors::SimpleList(abundance = a)
  )
  expect_error(validObject(x), "underlying assay names")
  repaired <- as_glycomic_se(x)
  expect_names_synced(repaired, paste0("g", 1:3), paste0("s", 1:2))
})

test_that("automatic data.frame rownames and omitted colData inherit names", {
  args <- name_fixture()
  args$colData <- data.frame(group = c("A", "B"))
  x <- do.call(GlycomicSE, args)
  expect_names_synced(x, paste0("g", 1:3), paste0("s", 1:2))
  args$colData <- NULL
  x <- do.call(GlycomicSE, args)
  expect_names_synced(x, paste0("g", 1:3), paste0("s", 1:2))
  expect_equal(ncol(SummarizedExperiment::colData(x)), 0)
  args$colData <- S4Vectors::DataFrame(sample = c("other", "other"))
  args$rowData$variable <- rep(NA_character_, 3)
  x <- do.call(GlycomicSE, args)
  expect_identical(SummarizedExperiment::colData(x)$sample, c("other", "other"))
  expect_true(all(is.na(SummarizedExperiment::rowData(x)$variable)))
})

test_that("zero-length axes and dimensional mismatches are handled explicitly", {
  args <- name_fixture()
  args$abundance <- args$abundance[integer(), , drop = FALSE]
  args$rowData <- args$rowData[integer(), , drop = FALSE]
  x <- suppressWarnings(do.call(GlycomicSE, args))
  suppressWarnings(expect_names_synced(x, character(), paste0("s", 1:2)))
  suppressWarnings(rownames(x) <- character())
  suppressWarnings(expect_true(validObject(x)))
  args <- name_fixture()
  args$colData <- args$colData[1, , drop = FALSE]
  expect_error(do.call(GlycomicSE, args), "must have 2 rows")
})

test_that("inherited mutation entry points preserve the name contract", {
  for (proteomic in c(FALSE, TRUE)) {
    ctor <- if (proteomic) GlycoproteomicSE else GlycomicSE
    x <- do.call(ctor, name_fixture(proteomic))
    names(x) <- c("a", "b", "c")
    expect_names_synced(x, c("a", "b", "c"), c("s1", "s2"))
    expect_error(names(x) <- c("a", "a", "c"), "unique")
    expect_error(names(x) <- NULL, "one name")
    cd <- SummarizedExperiment::colData(x)
    rownames(cd) <- c("first", "second")
    SummarizedExperiment::colData(x) <- cd
    expect_names_synced(x, c("a", "b", "c"), c("first", "second"))
    rownames(cd) <- c("same", "same")
    expect_error(SummarizedExperiment::colData(x) <- cd, "unique")
    rownames(cd) <- NULL
    expect_error(SummarizedExperiment::colData(x) <- cd, "one name")
    a <- SummarizedExperiment::assay(x)
    dimnames(a) <- NULL
    SummarizedExperiment::assay(x, withDimnames = FALSE) <- a
    expect_names_synced(x, c("a", "b", "c"), c("first", "second"))
    SummarizedExperiment::rowData(x)$feature <- c("one", "two", "three")
    expect_names_synced(x, c("a", "b", "c"), c("first", "second"))
    expect_error(x[c(1, 1), ], "unique")
    expect_error(x[, c(1, 1)], "unique")
    expect_names_synced(x[c(3, 1), 2:1], c("c", "a"), c("second", "first"))
    y <- x
    rownames(y) <- c("d", "e", "f")
    expect_names_synced(rbind(x, y), letters[1:6], c("first", "second"))
    expect_error(rbind(x, x), "unique")
    y <- x
    colnames(y) <- c("third", "fourth")
    expect_names_synced(
      cbind(x, y),
      letters[1:3],
      c("first", "second", "third", "fourth")
    )
    expect_error(cbind(x, x), "unique")
    # Replacement by the same identities remains valid.
    x[1, ] <- x[1, ]
    expect_names_synced(x, letters[1:3], c("first", "second"))
  }
})

test_that("plain SE objects retain their upstream naming semantics", {
  x <- SummarizedExperiment::SummarizedExperiment(assays = list(matrix(1:6, 3)))
  expect_null(rownames(x))
  colnames(x) <- c("same", "same")
  expect_identical(colnames(x), c("same", "same"))
  expect_true(validObject(x))
})

test_that("existing glyco-omics objects with missing scale migrate to raw", {
  for (proteomic in c(FALSE, TRUE)) {
    ctor <- if (proteomic) GlycoproteomicSE else GlycomicSE
    convert <- if (proteomic) as_glycoproteomic_se else as_glycomic_se
    x <- do.call(ctor, name_fixture(proteomic))
    S4Vectors::metadata(x)$scale <- NULL
    y <- convert(x)
    expect_identical(get_scale(y), "raw")
    expect_names_synced(y, rownames(x), colnames(x))
  }
})

test_that("assay replacement preserves the single-assay class contract", {
  x <- do.call(GlycomicSE, name_fixture())
  a <- SummarizedExperiment::assay(x)
  expect_error(
    SummarizedExperiment::assays(x) <- list(abundance = a, copy = a),
    "only one assay"
  )
})
