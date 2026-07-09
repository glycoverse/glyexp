#' @export
.GlycomicSE <- setClass("GlycomicSE", contains = "SummarizedExperiment")

#' @export
.GlycoproteomicSE <- setClass(
  "GlycoproteomicSE",
  contains = "SummarizedExperiment"
)

#' Create a GlycomicSE object
#'
#' @param abundance A numeric abundance matrix with glycans as rows and samples
#'   as columns.
#' @param ... Arguments passed to [SummarizedExperiment::SummarizedExperiment()],
#'   such as `rowData`, `colData`, and `metadata`.
#' @returns A `GlycomicSE` object.
#' @export
GlycomicSE <- function(abundance, ...) {
  se <- SummarizedExperiment::SummarizedExperiment(
    list(abundance = abundance),
    ...
  )
  .GlycomicSE(se)
}

#' Create a GlycoproteomicSE object
#'
#' @param abundance A numeric abundance matrix with glycopeptides or glycoforms
#'   as rows and samples as columns.
#' @param ... Arguments passed to [SummarizedExperiment::SummarizedExperiment()],
#'   such as `rowData`, `colData`, and `metadata`.
#' @returns A `GlycoproteomicSE` object.
#' @export
GlycoproteomicSE <- function(abundance, ...) {
  se <- SummarizedExperiment::SummarizedExperiment(
    list(abundance = abundance),
    ...
  )
  .GlycoproteomicSE(se)
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
  if (length(assays) > 1) {
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
  choices <- c("N", "O", "O-GalNAc", "O-GlcNAc", "O-Man", "O-Fuc", "O-Glc")
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
