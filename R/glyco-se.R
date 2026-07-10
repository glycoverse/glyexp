#' @export
.GlycomicSE <- setClass("GlycomicSE", contains = "SummarizedExperiment")

#' @export
.GlycoproteomicSE <- setClass(
  "GlycoproteomicSE",
  contains = "SummarizedExperiment"
)

#' Create a GlycomicSE object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `GlycomicSE()` creates a single-assay
#' [SummarizedExperiment::SummarizedExperiment()] subclass for glycomics data.
#' It is a thin wrapper around `SummarizedExperiment()` with additional
#' Glycoverse validation:
#'
#' 1. Exactly one assay is allowed. Extra assays are rejected to avoid
#'    ambiguous glycomics measurements.
#' 2. The assay must contain only non-negative values. Raw glycomics abundance
#'    data are non-negative; log transformation should be handled by downstream
#'    Glycoverse packages.
#' 3. `rowData` must contain a `glycan_composition` column, and that column must
#'    be a `glyrepr::glycan_composition()` vector. A `glycan_structure` column is
#'    optional, but if present it must be a `glyrepr::glycan_structure()` vector.
#' 4. `metadata` must contain a `glycan_type` field.
#'
#' This container is experimental and is not recognized by all Glycoverse
#' packages yet. It is intended to become a recommended entry point as package
#' contracts migrate toward `SummarizedExperiment`-based containers.
#'
#' @param abundance A numeric abundance matrix with glycans as rows and samples
#'   as columns.
#' @param ... Arguments passed to [SummarizedExperiment::SummarizedExperiment()].
#'   - `rowData`: A `S4Vectors::DataFrame()` with at least the following columns:
#'     - `glycan_composition`: required, a `glyrepr::glycan_composition()` vector
#'     - `glycan_structure`: optional, a `glyrepr::glycan_structure()` vector
#'   - `colData`: A `S4Vectors::DataFrame()`.
#'   - `metadata`: A list. It must include a `glycan_type` field with one of
#'     `"N"`, `"O"`, `"O-GalNAc"`, `"O-Man"`, `"O-Fuc"`, `"O-GlcNAc"`,
#'     `"O-Glc"`, `"HMO"`, `"GSL"`, `"GAG"`, or `"GPI"`.
#'
#' @returns A `GlycomicSE` object.
#' @aliases GlycomicSE-class
#' @section S4 class:
#' `GlycomicSE` is an S4 class that extends
#' [SummarizedExperiment::SummarizedExperiment()].
#' @seealso [GlycoproteomicSE()]
#'
#' @export
GlycomicSE <- function(abundance, ...) {
  se <- SummarizedExperiment::SummarizedExperiment(
    list(abundance = abundance),
    ...
  )
  .GlycomicSE(se)
}

#' Coerce to GlycomicSE
#'
#' @description
#' `as_glycomic_se()` converts supported objects to a `GlycomicSE` object.
#' Existing `GlycomicSE` objects are returned unchanged. [experiment()] objects
#' are first converted with [as_se()], and `SummarizedExperiment` objects are
#' reclassified after `GlycomicSE` validity checks pass.
#'
#' `is_glycomic_se()` checks whether an object inherits from `GlycomicSE`.
#'
#' @param x An object to coerce or check.
#'
#' @returns
#' `as_glycomic_se()` returns a `GlycomicSE` object.
#' `is_glycomic_se()` returns a logical value.
#' @export
as_glycomic_se <- function(x) {
  .require_se()

  if (is_glycomic_se(x)) {
    return(x)
  }
  if (is_experiment(x)) {
    return(.GlycomicSE(as_se(x)))
  }

  checkmate::assert_class(x, "SummarizedExperiment")
  .GlycomicSE(x)
}

#' @rdname as_glycomic_se
#' @export
is_glycomic_se <- function(x) {
  methods::is(x, "GlycomicSE")
}

#' Show a GlycomicSE object
#'
#' @param object A `GlycomicSE` object.
#' @returns Invisibly returns `object`.
#' @keywords internal
#' @export
methods::setMethod("show", "GlycomicSE", function(object) {
  .show_glyco_se(object, "GlycomicSE")
})

#' Create a GlycoproteomicSE object
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `GlycoproteomicSE()` creates a single-assay
#' [SummarizedExperiment::SummarizedExperiment()] subclass for
#' glycoproteomics data. It is a thin wrapper around `SummarizedExperiment()`
#' with additional Glycoverse validation:
#'
#' 1. Exactly one assay is allowed. Extra assays are rejected to avoid
#'    ambiguous glycoproteomics measurements.
#' 2. The assay must contain only non-negative values. Raw glycoproteomics
#'    abundance data are non-negative; log transformation should be handled by
#'    downstream Glycoverse packages.
#' 3. `rowData` must contain `protein`, `protein_site`, and
#'    `glycan_composition` columns. The `protein` column must be character,
#'    `protein_site` must be integer-like, and `glycan_composition` must be a
#'    `glyrepr::glycan_composition()` vector. A `glycan_structure` column is
#'    optional, but if present it must be a `glyrepr::glycan_structure()` vector.
#' 4. `metadata` must contain a `glycan_type` field.
#'
#' This container is experimental and is not recognized by all Glycoverse
#' packages yet. It is intended to become a recommended entry point as package
#' contracts migrate toward `SummarizedExperiment`-based containers.
#'
#' @param abundance A numeric abundance matrix with glycopeptides or glycoforms
#'   as rows and samples as columns.
#' @param ... Arguments passed to [SummarizedExperiment::SummarizedExperiment()].
#'   - `rowData`: A `S4Vectors::DataFrame()` with at least the following columns:
#'     - `protein`: required, a character vector
#'     - `protein_site`: required, an integer-like vector
#'     - `glycan_composition`: required, a `glyrepr::glycan_composition()` vector
#'     - `glycan_structure`: optional, a `glyrepr::glycan_structure()` vector
#'   - `colData`: A `S4Vectors::DataFrame()`.
#'   - `metadata`: A list. It must include a `glycan_type` field with one of
#'     `"N"`, `"O"`, `"O-GalNAc"`, `"O-Man"`, `"O-Fuc"`, `"O-GlcNAc"`,
#'     `"O-Glc"`, `"HMO"`, `"GSL"`, `"GAG"`, or `"GPI"`.
#' @returns A `GlycoproteomicSE` object.
#' @aliases GlycoproteomicSE-class
#' @section S4 class:
#' `GlycoproteomicSE` is an S4 class that extends
#' [SummarizedExperiment::SummarizedExperiment()].
#' @seealso [GlycomicSE()]
#' @export
GlycoproteomicSE <- function(abundance, ...) {
  se <- SummarizedExperiment::SummarizedExperiment(
    list(abundance = abundance),
    ...
  )
  .GlycoproteomicSE(se)
}

#' Coerce to GlycoproteomicSE
#'
#' @description
#' `as_glycoproteomic_se()` converts supported objects to a
#' `GlycoproteomicSE` object. Existing `GlycoproteomicSE` objects are returned
#' unchanged. [experiment()] objects are first converted with [as_se()], and
#' `SummarizedExperiment` objects are reclassified after `GlycoproteomicSE`
#' validity checks pass.
#'
#' `is_glycoproteomic_se()` checks whether an object inherits from
#' `GlycoproteomicSE`.
#'
#' @param x An object to coerce or check.
#'
#' @returns
#' `as_glycoproteomic_se()` returns a `GlycoproteomicSE` object.
#' `is_glycoproteomic_se()` returns a logical value.
#'
#' @export
as_glycoproteomic_se <- function(x) {
  .require_se()

  if (is_glycoproteomic_se(x)) {
    return(x)
  }
  if (is_experiment(x)) {
    return(.GlycoproteomicSE(as_se(x)))
  }

  checkmate::assert_class(x, "SummarizedExperiment")
  .GlycoproteomicSE(x)
}

#' @rdname as_glycoproteomic_se
#' @export
is_glycoproteomic_se <- function(x) {
  methods::is(x, "GlycoproteomicSE")
}

#' Show a GlycoproteomicSE object
#'
#' @param object A `GlycoproteomicSE` object.
#' @returns Invisibly returns `object`.
#' @keywords internal
#' @export
methods::setMethod("show", "GlycoproteomicSE", function(object) {
  .show_glyco_se(object, "GlycoproteomicSE")
})

#' Show a glyco SummarizedExperiment subclass
#'
#' @param object A `GlycomicSE` or `GlycoproteomicSE` object.
#' @param class_name The class name to display.
#' @returns Invisibly returns `object`.
#' @noRd
.show_glyco_se <- function(object, class_name) {
  meta <- S4Vectors::metadata(object)
  row_data_msg <- paste0(
    "Row data fields: ",
    format_fields_with_types(SummarizedExperiment::rowData(object))
  )
  col_data_msg <- paste0(
    "Column data fields: ",
    format_fields_with_types(SummarizedExperiment::colData(object))
  )
  metadata_msg <- paste0(
    "Metadata fields: ",
    .format_metadata_fields_with_types(meta)
  )

  cli::cli_h1(class_name)
  cli::cli_alert_info(
    "Abundance assay: {.val {ncol(object)}} samples, {.val {nrow(object)}} variables"
  )
  cli::cli_alert_info("Glycan type: {meta$glycan_type}")
  cli::cli_alert_info(row_data_msg)
  cli::cli_alert_info(col_data_msg)
  cli::cli_alert_info(metadata_msg)

  invisible(object)
}

#' Format metadata field names with type abbreviations
#'
#' @param meta The object metadata list.
#' @returns A string containing formatted metadata field names and types.
#' @noRd
.format_metadata_fields_with_types <- function(meta) {
  field_cols <- names(meta)
  if (is.null(field_cols)) {
    return("none")
  }

  field_cols <- field_cols[field_cols != ""]
  if (length(field_cols) == 0) {
    return("none")
  }

  formatted_fields <- purrr::map_chr(field_cols, function(col_name) {
    col_type <- get_col_type(meta[[col_name]])
    paste0("{.field ", col_name, "} {.cls ", col_type, "}")
  })

  paste(formatted_fields, collapse = ", ")
}

S4Vectors::setValidity2("GlycomicSE", function(object) {
  msg <- c(
    .validate_assays(SummarizedExperiment::assays(object)),
    .validate_metadata(S4Vectors::metadata(object)),
    .validate_glycomic_row_data(SummarizedExperiment::rowData(object))
  )
  if (is.null(msg)) TRUE else msg
})

S4Vectors::setValidity2("GlycoproteomicSE", function(object) {
  msg <- c(
    .validate_assays(SummarizedExperiment::assays(object)),
    .validate_metadata(S4Vectors::metadata(object)),
    .validate_glycoproteomic_row_data(SummarizedExperiment::rowData(object))
  )
  if (is.null(msg)) TRUE else msg
})

#' Validate assays
#'
#' @param assays The `assays`.
#' @returns `NULL` if no problems, otherwise a string for the reason.
#' @noRd
.validate_assays <- function(assays) {
  msg <- NULL
  if (length(assays) != 1) {
    msg <- c(msg, "only one assay is allowed")
  }
  if (min(assays[[1]], na.rm = TRUE) < 0) {
    msg <- c(msg, "`abundance` must be non-negative")
  }
  msg
}

#' Validate metadata
#'
#' @param meta The `metadata`.
#' @returns `NULL` if no problems, otherwise a string for the reason.
#' @noRd
.validate_metadata <- function(meta) {
  .validate_glycan_type(meta$glycan_type)
}

#' Validate `glycan_type` metadata
#'
#' @param glycan_type The `glycan_type` metadata.
#' @returns `NULL` if no problems, otherwise a string for the reason.
#' @noRd
.validate_glycan_type <- function(glycan_type) {
  if (is.null(glycan_type)) {
    return("metadata `glycan_type` must be provided")
  }
  choices <- .valid_glycan_types()
  if (glycan_type %in% choices) {
    NULL
  } else {
    "unknown glycan type"
  }
}

#' Validate rowData of GlycomicSE
#' @param row_data The `rowData`.
#' @returns `NULL` if no problems, otherwise a string for the reason.
#' @noRd
.validate_glycomic_row_data <- function(row_data) {
  c(
    .validate_row_data_required_columns(
      row_data,
      required = "glycan_composition"
    ),
    .validate_glycan_columns(row_data)
  )
}

#' Validate rowData of GlycoproteomicSE
#' @param row_data The `rowData`.
#' @returns `NULL` if no problems, otherwise a string for the reason.
#' @noRd
.validate_glycoproteomic_row_data <- function(row_data) {
  c(
    .validate_row_data_required_columns(
      row_data,
      required = c("protein", "protein_site", "glycan_composition")
    ),
    .validate_glycan_columns(row_data),
    .validate_glycosite_columns(row_data)
  )
}

#' Validate if all required columns exist in rowData
#' @param row_data The `rowData`.
#' @param required The required columns in `rowData`.
#' @returns `NULL` if no problems, otherwise a string for the reason.
#' @noRd
.validate_row_data_required_columns <- function(row_data, required) {
  if (!all(required %in% colnames(row_data))) {
    missing <- required[!required %in% colnames(row_data)]
    col_str <- glue::glue_collapse(
      paste0("'", required, "'"),
      sep = "' ",
      last = ", and "
    )
    glue::glue("`@rowData` must contain these columns: {col_str}")
  } else {
    NULL
  }
}

#' Validate glycosite rowData columns
#'
#' @param row_data The `rowData`.
#' @returns `NULL` if no problems, otherwise a string for the reason.
#' @noRd
.validate_glycosite_columns <- function(row_data) {
  msg <- NULL
  if (!is.character(row_data$protein)) {
    msg <- c(msg, "`@rowData$protein` must be character")
  }
  if (!rlang::is_integerish(row_data$protein_site)) {
    msg <- c(msg, "`@rowData$protein_site` must be integer")
  }
  msg
}

#' Validate glycan rowData columns
#'
#' @param row_data The `rowData`.
#' @returns `NULL` if no problems, otherwise a string for the reason.
#' @noRd
.validate_glycan_columns <- function(row_data) {
  msg <- NULL
  if (
    "glycan_composition" %in%
      colnames(row_data) &&
      !glyrepr::is_glycan_composition(row_data$glycan_composition)
  ) {
    msg <- c(
      msg,
      "`@rowData$glycan_composition` must be `glyrepr::glycan_composition()`"
    )
  }
  if ("glycan_structure" %in% colnames(row_data)) {
    if (!glyrepr::is_glycan_structure(row_data$glycan_structure)) {
      msg <- c(
        msg,
        "`@rowData$glycan_structure` must be `glyrepr::glycan_structure()`"
      )
    }
  }
  msg
}
